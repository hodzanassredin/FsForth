
module Forth 

open System
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text
open System.IO

module Memory = 

    [<Literal>]
    let baseSize = 4//sizeof<Int32>
    [<Measure>] type bytes
    [<Measure>] type ints
    //conversion factor
    let bytesPerInt = 4<bytes/ints>  
    let toBytes (ints:int<ints>) : int<bytes>= ints * bytesPerInt
    let toInts (bytes:int<bytes>) : int<ints>= bytes / bytesPerInt

    let pad address = (address + (baseSize-1)) &&& ~~~(baseSize-1)//same as address + (baseSize - (string_length % baseSize)) % baseSize

    type MemoryConfig = {
        INITIAL_DATA_SEGMENT_SIZE : int<bytes>
        DATA_STACK_SIZE : int<bytes>
        RETURN_STACK_SIZE : int<bytes>
        BUFFER_SIZE : int<bytes>
    }

    let defaultConfig = {
        INITIAL_DATA_SEGMENT_SIZE = 65536<bytes>
        DATA_STACK_SIZE = 8192<bytes>
        RETURN_STACK_SIZE  = 8192<bytes>
        BUFFER_SIZE = 4096<bytes>
    }
    type Span = private {
        address : int<bytes>
        size : int<bytes>
    }

    let top span = span.address + span.size

    type Cursor = private {
        span : Span
        isInverse : bool
        mutable current : int<bytes>
    }
    let reset cursor = cursor.current <- if cursor.isInverse then cursor.span.address + cursor.span.size else cursor.span.address
    let used cursor = if cursor.isInverse then top cursor.span - cursor.current else cursor.current - cursor.span.address
    let isFull cursor = used cursor = cursor.span.size
    let createCursor address size isInverse = 
        let c ={ 
            span = { address = address; size = size }
            isInverse = isInverse
            current = 0<bytes>
        }
        reset c
        c


    let validate (cursor:Cursor) = ()

        //if cursor.current < cursor.span.address || cursor.current > (top cursor.span)
        //then failwith "broken span bounds"

    let change op noop (v:Cursor) count = 
        let prev = v.current
        let op = if v.isInverse then op else noop
        v.current <- op v.current count
        validate v
        prev

    let inc = change (-) (+)
    let dec = change (+) (-) 

    let incInts span ints = inc span (toBytes ints)
    let decInts span ints = dec span (toBytes ints)

    type Memory = private {
        memory : byte[]
    }

    

    let get (memory:Memory)  (address:int<bytes>) = memory.memory.[int address]
    let set (memory:Memory)  (address:int<bytes>) v = memory.memory.[int address] <- v 

    let getInt (memory:Memory) (address:int<bytes>) = BitConverter.ToInt32(memory.memory, address |> int)

    let setInt (memory:Memory) (address:int<bytes>) (v:int) = BitConverter.GetBytes(v).CopyTo(memory.memory, address |> int);

    let copyFromBytes (memory:Memory) (address:int<bytes>) (arr:byte array) = arr.CopyTo(memory.memory, int address)
    let writeStdout (memory:Memory) cursor = 
        use out = System.Console.OpenStandardOutput()
        out.Write(memory.memory, int cursor.span.address, int cursor.current)
        out.Flush()

    let readStdin (memory:Memory) span = 
        use inp = System.Console.OpenStandardInput()
        inp.Read(memory.memory, int span.address, int span.size) * 1<bytes>

    let write (memory:Memory) cursor b = 
        set memory cursor.current b
        inc cursor 1<bytes> |> ignore

    let writeInt (memory:Memory) cursor i = 
        setInt memory cursor.current i
        incInts cursor 1<ints> |> ignore

    type InputBuffer(memory : Memory, cursor : Cursor)=
        
        let mutable bufftop = cursor.span.address 
        do 
            if cursor.isInverse then failwith "not possible to use inverse memory as buffer"
        member x.GetLast n = 
            let count = used cursor
            let count = if n > count then count else n
            { address = cursor.current - count; size = count}

        member x.get () = 
            if cursor.current < bufftop
            then let v = get memory cursor.current
                 inc cursor 1<bytes> |> ignore
                 v
            else let count = readStdin memory cursor.span 
                 if count<= 0<bytes> then failwith "exit"//todo
                 else bufftop <- cursor.span.address + count 
                      reset cursor
                      x.get ()

    type OutputBuffer(memory : Memory, cursor : Cursor)=

        member x.flush() =
            writeStdout memory cursor
            reset cursor

        member x.set c = 
            if isFull cursor then x.flush()
            write memory cursor c

    type Stack(memory : Memory, cursor : Cursor)=
        do 
            if not cursor.isInverse then failwith "not possible to use non inverse memory as stack"
        member x.reset () = reset cursor
        member x.top 
            with get () = cursor.current
            and set (value) = cursor.current <- value
        member x.S0 = top cursor.span
        member x.push v = 
            assert (cursor.current > cursor.span.address)
            decInts cursor 1<ints> |> ignore
            setInt memory cursor.current v
    
        member x.pop () = 
            assert (cursor.current < top cursor.span)
            let v = get memory cursor.current
            incInts cursor 1<ints> |> ignore
            v

        member x.peek (offset:int<ints>) = getInt memory (cursor.current + (toBytes offset))
    
        member x.apply f = x.peek 0<ints> |> f |> setInt memory cursor.current 

    type MemoryLayout = {
        memory : Memory
        allocated : Cursor
        data : Cursor
        data_stack : Stack
        return_stack : Stack
        input_buffer : InputBuffer
        out_buffer : OutputBuffer
        word_buffer : Cursor
    }

    let create (size:int<bytes>) config  = 
        let memory = { memory =Array.create (int size) 0uy }
        let allocated = createCursor 0<bytes> size false 
        let allocate bytes isInverse init = 
            let address = inc allocated bytes
            let c = createCursor address bytes isInverse
            init (memory,c)

        {
            memory = memory
            allocated = allocated
            data = allocate config.INITIAL_DATA_SEGMENT_SIZE false snd
            data_stack = allocate config.DATA_STACK_SIZE true Stack
            return_stack = allocate config.RETURN_STACK_SIZE true Stack
            input_buffer = allocate config.BUFFER_SIZE false InputBuffer
            out_buffer= allocate config.BUFFER_SIZE false OutputBuffer
            word_buffer = allocate 32<bytes> false snd
        }


type FnPointer = Int32

type ForthVM(memory : Memory) =
    //memory for main forth variables
    member val QUIT = Variable(memory, 0)
    member val STATE = Variable(memory, 0)//Is the interpreter executing code (0) or compiling a word (non-zero)?
    member val LATEST = Variable(memory, 0)//Points to the latest (most recently defined) word in the dictionary.
    member val HERE = Variable(memory, 0)//Points to the next free byte of memory.  When compiling, compiled words go here.
    //member val S0 = Variable(memory, 0)//Stores the address of the top of the parameter stack.
    //member val R0 = Variable(memory, 0)//Stores the address of the top of the parameter stack.
    //member val BASE = Variable(memory, 10)//The current base for printing and reading numbers.
   
    member val  errmsg = reserveString memory <| Encoding.ASCII.GetBytes "PARSE ERROR: "
    member val  errmsgnl = reserveString memory <| Encoding.ASCII.GetBytes "\n"
    [<DefaultValue>] val mutable IP : Int32//istruction pointer in bytes
    [<DefaultValue>] val mutable W : Int32//work
    member x.memory = memory

type Fn = ForthVM -> FnPointer

type CodeMemory()=
    //native funcs addressable storage
    let nativeFuncs : Fn[] = Array.zeroCreate (1<<<8) 
    let mutable nextFnPointer = 0
    let addFn f = 
        nativeFuncs.[nextFnPointer] <- f
        nextFnPointer <- nextFnPointer + 1
        nextFnPointer - 1

    let next (vm:ForthVM) = 
        vm.W <- vm.IP
        inc &vm.IP baseSize
        vm.memory.get vm.W
    let next = addFn next
    //alternative names: docolon, enter
    let nest (vm:ForthVM) = 
        vm.RSP.push(vm.IP)
        vm.IP <- vm.W
        inc &vm.IP baseSize
        next
    let nest = addFn nest
    //alternative names: exit
    let unnest (vm:ForthVM) =
        vm.IP <- vm.RSP.pop()
        next

    let unnest = addFn unnest

    member x.NEXT = next
    member x.NEST = nest
    member x.UNNEST = unnest
    member x.addFunction = addFn

    member x.get idx = nativeFuncs.[idx]



[<Flags>]
type Flags =
    | NONE = 0
    | IMMEDIATE = 0x80
    | HIDDEN = 0x20

let LENMASK = 0x1f
type ReadMode = SKIP|WORD|COMMENT
type Forth(memory : Int32[]) =
    let memory = Memory(memory)
    let code = CodeMemory()
    let vm = ForthVM(memory)
    
    do 
        vm.HERE.set vm.memory.MemoryTop

    let writeByte (v:byte) = 
        let address = vm.HERE.get()
        vm.memory.setByte address <| int v
        vm.HERE.set <| address + 1

    let write (v:Int32) = 
        let address = vm.HERE.get()
        vm.memory.set address v
        vm.HERE.set <| address + baseSize
        
    let writeString (str:String) = Encoding.ASCII.GetBytes str |> Array.iter writeByte 

    let align () = vm.HERE.get() |> pad |> vm.HERE.set

    let rec run (fnIdx:Int32) =  
        let f = code.get fnIdx
        let nextFn = f vm
        run nextFn

    member x.create (name: string) (flags:Flags)=
        let nameSize = byte name.Length
        assert (nameSize <= 31uy)
        let flags = byte flags

        let curlink = vm.LATEST.get()
        vm.LATEST.set <| vm.HERE.get()//set link
        write(curlink)
        nameSize + flags |> int |> write
        writeString name
        align()

    member x.defwordRetCodeword (name: string) (flags:Flags) (program:Int32[]) = 
        x.create name flags 
        let codewordAddr = vm.memory.MemoryTop
        write code.NEST //codeword
        for d in program do
            write d 
        codewordAddr

    member x.defword (name: string) (flags:Flags) (program:Int32[]) = 
        x.defwordRetCodeword name flags program |>ignore

    //add native word
    member x.defcodeRetAddr (name: string) (flags:Flags) (f:Fn) = 
        let fAddr = code.addFunction f
        x.create name flags 
        write fAddr 
        fAddr
    member x.defcode (name: string) (flags:Flags) (f:Fn) = x.defcodeRetAddr name flags f |> ignore

    //add variable
    member x.defvarRetAddr (name: string) (flags:Flags) (varAddress:Int32) (initial:Int32 option) = 
        match initial with  
            | Some(initial) -> vm.memory.set varAddress initial
            | _ -> ()
        x.defcodeRetAddr name flags (fun vm -> 
            vm.SP.push varAddress
            code.NEXT
        )
    member x.defvar (name: string) (flags:Flags) (varAddress:Int32) (initial:Int32 option) = x.defvarRetAddr name flags varAddress initial |> ignore
    member x.defconstRetAddr (name: string) (flags:Flags) (value:Int32) = 
        x.defcodeRetAddr name flags (fun vm -> 
            vm.SP.push value
            code.NEXT
        )

    member x.defconst (name: string) (flags:Flags) (value:Int32) = 
        x.defconstRetAddr name flags value |> ignore

    member x.coldStart () = 
        vm.IP <- vm.QUIT.address
        run code.NEXT
        
    member x.init () = 
        let EXIT = code.UNNEST
        let DOCOL = code.NEST
        x.defcode "DROP" Flags.NONE (fun vm -> 
            vm.SP.pop() |> ignore
            code.NEXT
        ) 

        x.defcode "SWAP" Flags.NONE (fun vm -> 
            let x = vm.SP.pop()
            let y = vm.SP.pop()
            vm.SP.push(x)
            vm.SP.push(y)
            code.NEXT
        ) 
        x.defcode "DUP" Flags.NONE (fun vm -> 
            vm.SP.push(vm.SP.peek 0)// duplicate top of stack
            code.NEXT
        ) 
        x.defcode "OVER" Flags.NONE (fun vm -> 
            vm.SP.push(vm.SP.peek 1)//get the second element of stack and push it on top
            code.NEXT
        ) 
        //( a b c -- b c a )
        x.defcode "ROT" Flags.NONE (fun vm -> 
            let eax = vm.SP.pop()
            let ebx = vm.SP.pop()
            let ecx = vm.SP.pop()
            vm.SP.push ebx
            vm.SP.push eax
            vm.SP.push ecx
            code.NEXT 
        ) 
        //( a b c -- c a b ) rot rot 
        x.defcode "-ROT" Flags.NONE (fun vm -> 
            let eax = vm.SP.pop()
            let ebx = vm.SP.pop()
            let ecx = vm.SP.pop()
            vm.SP.push eax
            vm.SP.push ecx
            vm.SP.push ebx
            code.NEXT 
        ) 
        //( a b -- ) drop drop ;
        x.defcode "2DROP" Flags.NONE (fun vm ->  // drop top two elements of stack
            vm.SP.pop () |> ignore
            vm.SP.pop () |> ignore
            code.NEXT 
        ) 
        //( a b -- a b a b ) over over ;
        x.defcode "2DUP" Flags.NONE (fun vm ->  // duplicate top two elements of stack
            let b = vm.SP.peek 0
            let a = vm.SP.peek 1
            vm.SP.push a
            vm.SP.push b
            code.NEXT 
        ) 
        //( d1 d2 — d2 d1 )
        x.defcode "2SWAP" Flags.NONE (fun vm ->  // swap top two pairs of elements of stack
            let eax = vm.SP.pop ()
            let ebx = vm.SP.pop ()
            let ecx = vm.SP.pop ()
            let edx = vm.SP.pop ()
            vm.SP.push ebx
            vm.SP.push eax
            vm.SP.push edx
            vm.SP.push ecx
            code.NEXT 
        ) 

        let apply (vm:ForthVM) = vm.SP.apply
        let apply2 (vm:ForthVM) (f: Int32->Int32->Int32)  = vm.SP.apply (f <| vm.SP.pop())
        let boolToBase b = if b then -1 else 0
    
        let applyBool vm f  = apply vm (fun a -> f a |> boolToBase) 
        let applyBool2 vm f  = apply2 vm (fun a b -> f a b |> boolToBase)

        let def applier name f= x.defcode name Flags.NONE (fun vm -> 
            applier vm f 
            code.NEXT  
        ) 

        //( a -- a a | 0 ) dup if dup then 
        //Duplicate x if it is non-zero. 
        x.defcode "?DUP" Flags.NONE (fun vm -> // duplicate top of stack if non-zero
            let eax = vm.SP.peek 0
            if eax <> 0 then vm.SP.push eax
            code.NEXT 
        ) 
        def apply "1+" <| (+) 1
        def apply "1-" <| (-) 1
        def apply "4+" <| (+) 4
        def apply "4-" <| (-) 4
        def apply2 "+" <| (+)
        def apply2 "-" <| (-)
        def apply2 "*" <| (*)
    
        //( n1 n2 -- n3 n4 ) Divide n1 by n2, giving the single-cell remainder n3 and the single-cell quotient n4
        x.defcode "/MOD" Flags.NONE (fun vm -> 
            let divisor = vm.SP.pop()
            let dividend = vm.SP.pop()
            let quotient, remainder = Math.DivRem(dividend, divisor)
            vm.SP.push remainder// push remainder
            vm.SP.push quotient// push quotient
            code.NEXT 
        ) 

        ////Lots of comparison operations like =, <, >, etc..
        ////ANS FORTH says that the comparison words should return all (binary) 1's for
        ////TRUE and all 0's for FALSE.  

        //n1 n2 – f  
        def applyBool2 "=" (=)
        def applyBool2 "<>" (<>)
        def applyBool2 "<" (<)
        def applyBool2 ">" (>)
        def applyBool2 "<=" (<=)
        def applyBool2 ">=" (>=)
        def applyBool2 ">" (>)
    
        def applyBool "0=" <| (=) 0
        def applyBool "0<>" <| (<>) 0
        def applyBool "0<" <| (<) 0
        def applyBool "0>" <| (>) 0
        def applyBool "0<=" <| (<=) 0
        def applyBool "0>=" <| (>=) 0
    
        def apply2 "AND" (&&&) 
        def apply2 "OR" (|||) 
        def apply2 "XOR" (^^^) 
        def apply "INVERT" (~~~) 
    
        let LIT = x.defcodeRetAddr "LIT" Flags.NONE (fun vm ->
            vm.W <- vm.IP
            inc &vm.IP baseSize
            vm.memory.get(vm.W) |> vm.SP.push
            code.NEXT
        )
        x.defcode "!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.SP.pop()
            vm.memory.set address data
            code.NEXT 
        )
        let FETCH = x.defcodeRetAddr "@" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.memory.get address 
            vm.SP.push data
            code.NEXT 
        )
        x.defcode "+!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let amount = vm.SP.pop()
            vm.memory.set address (vm.memory.get address + amount)
            code.NEXT 
        )
        x.defcode "-!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let amount = vm.SP.pop()
            vm.memory.set address (vm.memory.get address - amount)
            code.NEXT 
        )

        //! and @ (STORE and FETCH) store 32-bit words.  It's also useful to be able to read and write bytes
        //so we also define standard words C@ and C!.
        //Byte-oriented operations only work on architectures which permit them (i386 is one of those).

        x.defcode "C!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.SP.pop()
            vm.memory.setByte address data
            code.NEXT 
        )
        x.defcode "C@" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.memory.getByte address 
            vm.SP.push data
            code.NEXT 
        )

        // C@C! is a useful byte copy primitive. 
        x.defcode "C@C!" Flags.NONE (fun vm -> 
            let destination = vm.SP.pop()
            let source = vm.SP.pop()
            vm.memory.setByte destination <| vm.memory.getByte source
            vm.SP.push (source + 1)// increment source address
            vm.SP.push (destination + 1)// increment destination address
            code.NEXT
        )

        // and CMOVE is a block copy operation. 
        x.defcode "CMOVE" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let destination = vm.SP.pop()
            let source = vm.SP.pop()
            for i in 0..length do
                vm.memory.setByte (destination + i) <| vm.memory.getByte (source + i)
            code.NEXT 
        )

        x.defvar "STATE" Flags.NONE vm.STATE.address Option.None
        x.defvar "HERE" Flags.NONE vm.HERE.address Option.None
        let LATEST = x.defvarRetAddr "LATEST" Flags.NONE vm.LATEST.address Option.None //name_SYSCALL0 // SYSCALL0 must be last in built-in dictionary
        x.defvar "S0" Flags.NONE vm.S0.address Option.None
        x.defvar "BASE" Flags.NONE vm.BASE.address Option.None
        
        x.defconst "VERSION" Flags.NONE 1
        let RZ = x.defconstRetAddr "R0" Flags.NONE vm.RSP.S0
        x.defconst "DOCOL" Flags.NONE DOCOL
        x.defconst "F_IMMED" Flags.NONE <| int Flags.IMMEDIATE
        x.defconst "F_HIDDEN" Flags.NONE <| int Flags.HIDDEN
        x.defconst "F_LENMASK" Flags.NONE LENMASK
        
        //x.defconst "SYS_EXIT",8,,SYS_EXIT,__NR_exit
        //x.defconst "SYS_OPEN",8,,SYS_OPEN,__NR_open
        //x.defconst "SYS_CLOSE",9,,SYS_CLOSE,__NR_close
        //x.defconst "SYS_READ",8,,SYS_READ,__NR_read
        //x.defconst "SYS_WRITE",9,,SYS_WRITE,__NR_write
        //x.defconst "SYS_CREAT",9,,SYS_CREAT,__NR_creat
        //x.defconst "SYS_BRK",7,,SYS_BRK,__NR_brk
        
        //x.defconst "O_RDONLY",8,,__O_RDONLY,0
        //x.defconst "O_WRONLY",8,,__O_WRONLY,1
        //x.defconst "O_RDWR",6,,__O_RDWR,2
        //x.defconst "O_CREAT",7,,__O_CREAT,0100
        //x.defconst "O_EXCL",6,,__O_EXCL,0200
        //x.defconst "O_TRUNC",7,,__O_TRUNC,01000
        //x.defconst "O_APPEND",8,,__O_APPEND,02000
        //x.defconst "O_NONBLOCK",10,,__O_NONBLOCK,04000

        //RETURN STACK ----------------------------------------------------------------------
        x.defcode ">R" Flags.NONE (fun vm -> 
            vm.SP.pop() |> vm.RSP.push
            code.NEXT 
        )
        x.defcode "R>" Flags.NONE (fun vm -> 
            vm.RSP.pop() |> vm.SP.push
            code.NEXT 
        )
        //get return stack pointer
        x.defcode "RSP@" Flags.NONE (fun vm -> 
            vm.SP.push vm.RSP.top
            code.NEXT 
        )
        //set return stack pointer
        let RSPSTORE = x.defcodeRetAddr "RSP!" Flags.NONE (fun vm -> 
            vm.RSP.top <- vm.SP.pop()
            code.NEXT 
        )
        x.defcode "RDROP" Flags.NONE (fun vm -> 
            vm.RSP.pop()|> ignore
            code.NEXT 
        )
        //PARAMETER (DATA) STACK ----------------------------------------------------------------------
        //get data stack pointer
        x.defcode "DSP@" Flags.NONE (fun vm -> 
            vm.SP.push vm.SP.top
            code.NEXT 
        )
        //set data stack pointer
        x.defcode "DSP!" Flags.NONE (fun vm -> 
            vm.SP.top <- vm.SP.pop()
            code.NEXT 
        )

        x.defcode "DSP!" Flags.NONE (fun vm -> 
            vm.SP.top <- vm.SP.pop()
            code.NEXT 
        )
        //io
        x.defcode "KEY" Flags.NONE (fun vm -> 
            vm.SP.push <| vm.Input.get()
            code.NEXT 
        )
        x.defcode "EMIT" Flags.NONE (fun vm -> 
            vm.SP.pop() |> vm.Output.set
            code.NEXT 
        )
        
        let rec readWord mode =
            let c = vm.Input.get() 
            match (char c,mode) with
                | ('\n', ReadMode.COMMENT) -> readWord ReadMode.SKIP
                | (_, ReadMode.COMMENT) -> readWord ReadMode.COMMENT
                | (c, ReadMode.SKIP) when c = ' ' || c = '\t' -> readWord ReadMode.SKIP
                | (c, ReadMode.WORD) when c = ' ' || c = '\t' -> vm.WordBuffer.str
                | ('/', ReadMode.SKIP) -> readWord ReadMode.COMMENT
                | (_, _) -> vm.WordBuffer.write c
                            readWord ReadMode.WORD

        let _WORD () = readWord ReadMode.SKIP

        let WORD = x.defcodeRetAddr "WORD" Flags.NONE (fun vm -> 
            let str = _WORD()
            vm.SP.push str.address
            vm.SP.push str.length
            code.NEXT 
        )

        let _NUMBER str = 
            let radix = vm.BASE.get()
            let mutable n = 0
            let mutable idx = 0

            let next() = let c = vm.memory.getByte str.address 
                         idx <- idx + 1
                         c

            let mutable c = next()
            let isNegative = '-' = char c
            if isNegative then c <- next()
            let mutable cnt = true
            while cnt do
                n <- n * radix
                let d = next() - int '0'
                if d > radix then cnt <- false
                else n <- n + d * radix
                cnt <- idx < str.length
            
            let count = if str.length = 0 then 0 else str.length - idx

            if isNegative then -n, count else n, count

        x.defcode "NUMBER" Flags.NONE (fun vm -> 
            let len, addr = vm.SP.pop(), vm.SP.pop()
            let number, numberOfUnparsedChars = _NUMBER {address = addr ; length = len}
            vm.SP.push number
            vm.SP.push numberOfUnparsedChars // 0 if no error
            code.NEXT 
        )

        let rec eqStrings (vm:ForthVM) address address2 length = 
            if length = 0 
            then true
            else if vm.memory.getByte address = vm.memory.getByte address2
                 then eqStrings vm (address+1) (address2+1) (length - 1)
                 else false

        let _FIND str = 
            let mutable wordAddr = vm.LATEST.get()
            let mutable cnt = true
            while cnt do
                if wordAddr = 0 
                then cnt <- false
                else let flagsLen = vm.memory.getByte (wordAddr + baseSize)
                     let flagsLen = (int Flags.HIDDEN ||| LENMASK) &&& flagsLen
                     if flagsLen = str.length && eqStrings vm str.address (wordAddr + baseSize + 1) str.length
                     then cnt <- false
                     else wordAddr <- vm.memory.get wordAddr
            wordAddr
                     

        let FIND = x.defcodeRetAddr "FIND" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            let addrOfDictEntry = _FIND {address = address; length = length }
            vm.SP.push addrOfDictEntry
            code.NEXT 
        )
        let _TCFA (vm:ForthVM) linkAddress = 
            let flagsLenAddress = linkAddress + baseSize
            let length = vm.memory.getByte flagsLenAddress &&& LENMASK
            let cfaAddress = flagsLenAddress + 1 + length |> pad
            cfaAddress

        x.defcode ">CFA" Flags.NONE (fun vm -> 
            let worAddress = vm.SP.pop()
            let cfaAddress = _TCFA vm worAddress
            vm.SP.push cfaAddress
            code.NEXT 
        )
        x.defcode ">DFA" Flags.NONE (fun vm -> 
            let worAddress = vm.SP.pop()
            let cfaAddress = _TCFA vm worAddress
            vm.SP.push (cfaAddress + baseSize)
            code.NEXT 
        )
        let readString (vm:ForthVM) address length = 
            let arr = Array.create length ' '
            for i in 0..(length - 1) do
                arr.[i] <- char (vm.memory.getByte (address + i))
            new String(arr)

        let CREATE = x.defcodeRetAddr "CREATE" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            let name = readString vm address length
            x.create name Flags.NONE
            code.NEXT 
        )
        let _COMMA (vm:ForthVM) v =
            let address = vm.HERE.get()
            vm.memory.set address v
            vm.HERE.set (address + baseSize)

        let COMMA = x.defcodeRetAddr "," Flags.NONE (fun vm -> 
            let v = vm.SP.pop()
            _COMMA vm v
            code.NEXT 
        )

        let LBRAC = x.defcodeRetAddr "[" Flags.IMMEDIATE (fun vm -> 
            vm.STATE.set 0
            code.NEXT 
        )
        let RBRAC = x.defcodeRetAddr "]" Flags.IMMEDIATE (fun vm -> 
            vm.STATE.set 1
            code.NEXT 
        )

        let toggleLastWordFlag (flag:Flags) (vm:ForthVM)  = 
            let flagsAddress = vm.LATEST.get() + baseSize
            let flagsLen = vm.memory.get flagsAddress//todo or get byte
            let flagsLen = flagsLen ^^^ int flag //toggle immediate
            vm.memory.set flagsAddress flagsLen
            code.NEXT 

        let IMMEDIATE = x.defcodeRetAddr "IMMEDIATE" Flags.IMMEDIATE (toggleLastWordFlag Flags.IMMEDIATE)

        let HIDDEN = x.defcodeRetAddr "HIDDEN" Flags.IMMEDIATE (toggleLastWordFlag Flags.HIDDEN)

        x.defword ":" Flags.NONE 
            [|
                WORD;// Get the name of the new word
                CREATE;// CREATE the dictionary entry / header
                LIT; DOCOL; COMMA;// Append DOCOL  (the codeword).
                LATEST; FETCH; HIDDEN // Make the word hidden (see below for definition).
                RBRAC;// Go into compile mode.
                EXIT// Return from the function.
            |]
        
        x.defword ";" Flags.IMMEDIATE 
            [|
                LIT; EXIT; COMMA;// Append EXIT (so the word will return).
                LATEST; FETCH; HIDDEN; // Toggle hidden flag -- unhide the word (see below for definition).
                LBRAC;// Go back to IMMEDIATE mode.
                EXIT;// Return from the function.
            |]

        x.defword "HIDE" Flags.NONE [|WORD;FIND;HIDDEN;EXIT|]

        let TICK = x.defcodeRetAddr "'" Flags.NONE (fun vm ->
            vm.SP.push <| vm.memory.get vm.IP//todo check
            inc &vm.IP baseSize
            code.NEXT 
        )

        let BRANCH = x.defcodeRetAddr "BRANCH" Flags.NONE (fun vm ->
            vm.IP <- vm.IP + vm.memory.get vm.IP // add the offset to the instruction pointer
            code.NEXT 
        )
        x.defcode "0BRANCH" Flags.NONE (fun vm ->
            if vm.SP.pop () = 0 // top of stack is zero?
            then vm.IP <- vm.IP + vm.memory.get vm.IP // add the offset to the instruction pointer
            else inc &vm.IP baseSize// otherwise we need to skip the offset
            code.NEXT 
        )

        x.defcode "LITSTRING" Flags.NONE (fun vm ->
            let length = vm.memory.get vm.IP
            inc &vm.IP baseSize
            vm.SP.push vm.IP     // push the address of the start of the string
            vm.SP.push length    // push length on the stack
            vm.IP <- vm.IP + length |> pad  // skip past the string and round up to next BaseSize byte boundary
            code.NEXT 
        )
        x.defcode "TELL" Flags.NONE (fun vm ->
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            vm.memory.writeStdout {address = address; length = length}
            code.NEXT 
        )


        let INTERPRET = x.defcodeRetAddr "INTERPRET" Flags.NONE (fun vm ->
            let word = _WORD() // Returns %ecx = length, %edi = pointer to word.
            let pointer = _FIND word//pointer to header or 0 if not found.
            if pointer <> 0 //found word
            then
                let nameFlags = vm.memory.get pointer
                let codeword = _TCFA vm pointer
                let isImmediate = (nameFlags &&& int Flags.IMMEDIATE) <> 0
                if isImmediate
                then codeword// If IMMED, jump straight to executing.
                else if vm.STATE.get() = 0// Are we compiling or executing?
                     then codeword// Jump if executing.
                     else // Compiling - just append the word to the current dictionary definition.
                          _COMMA vm codeword
                          code.NEXT
            else // Not in the dictionary (not a word) so assume it's a literal number.
                 let number, numberOfUnparsedChars = _NUMBER word
                 if numberOfUnparsedChars > 0
                 then vm.memory.writeStdout vm.errmsg
                      vm.memory.writeStdout <| vm.Input.GetLast 40
                      vm.memory.writeStdout vm.errmsgnl
                      code.NEXT
                 else vm.SP.push number
                      _COMMA vm LIT
                      code.NEXT
        )
        
        let QUIT = x.defwordRetCodeword "QUIT" Flags.NONE [|
            RZ;RSPSTORE;// R0 RSP!, clear the return stack
            INTERPRET;// interpret the next word
            BRANCH;-(baseSize * 2)// and loop (indefinitely)
        |]
        vm.QUIT.set QUIT//cold start

        x.defcode "CHAR" Flags.NONE (fun vm ->
            let str = _WORD ()
            let c = vm.memory.get str.address
            vm.SP.push c
            code.NEXT
        )
        x.defcode "EXECUTE" Flags.NONE (fun vm ->
            let addr = vm.SP.pop()// Get xt into %eax and jump to it. After xt runs its NEXT will continue executing the current word.
            addr
        )

        ()