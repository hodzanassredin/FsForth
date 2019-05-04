
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

    let pad address = (address + (baseSize-1)) &&& ~~~(baseSize-1)//same as address + (baseSize - (string_length % baseSize)) % baseSize

    type MemoryConfig = {
        INITIAL_DATA_SEGMENT_SIZE : int
        DATA_STACK_SIZE : int
        RETURN_STACK_SIZE : int
        BUFFER_SIZE : int
    }

    let defaultConfig = {
        INITIAL_DATA_SEGMENT_SIZE = 65536
        DATA_STACK_SIZE = 8192
        RETURN_STACK_SIZE  = 8192
        BUFFER_SIZE = 4096
    }
    type Span = {
        address : int
        size : int
    }

    let top span = span.address + span.size

    type Cursor = {
        span : Span
        isInverse : bool
        mutable current : int
    }
    let reset cursor = cursor.current <- if cursor.isInverse then cursor.span.address + cursor.span.size else cursor.span.address
    let used cursor = if cursor.isInverse then top cursor.span - cursor.current else cursor.current - cursor.span.address
    let isFull cursor = used cursor = cursor.span.size
    let createCursor address size isInverse = 
        let c ={ 
            span = { address = address; size = size }
            isInverse = isInverse
            current = 0
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

    type Memory = byte[]
    let createMemory (size:int) = Array.create size 0uy 

    let getInt (memory:Memory) (address:int) = BitConverter.ToInt32(memory, address |> int)

    let setInt (memory:Memory) (address:int) (v:int) = BitConverter.GetBytes(v).CopyTo(memory, address |> int);

    let copyFromBytes (memory:Memory) (address:int) (arr:byte array) = arr.CopyTo(memory, int address)
    let copyToBytes (memory:Memory) (address:int) (arr:byte array) = Array.Copy(memory, address, arr, 0, arr.Length)

    let write (memory:Memory) cursor b = 
        memory.[cursor.current] <- b
        inc cursor 1 |> ignore

    let writeInt (memory:Memory) cursor i = 
        setInt memory cursor.current i
        inc cursor baseSize |> ignore

    type StringBuffer(memory : Memory, cursor : Cursor)=
        member x.getStrAndReset () = 
            let res = {address = cursor.span.address; size = used cursor }
            reset cursor
            res

        member x.write c = write memory cursor c

    type InputBuffer(memory : Memory, cursor : Cursor)=
        let readStdin (memory:Memory) span = 
            
            use inp = System.Console.OpenStandardInput()
            let count = inp.Read(memory, int span.address, int span.size) 
            count
        let mutable bufftop = cursor.span.address 
        do 
            if cursor.isInverse then failwith "not possible to use inverse memory as buffer"
        member x.GetLast n = 
            let count = used cursor
            let count = if n > count then count else n
            { address = cursor.current - count; size = count}

        member x.get () = 
            if cursor.current < bufftop
            then let v = memory.[cursor.current]
                 inc cursor 1 |> ignore
                 v
            else let count = readStdin memory cursor.span 
                 if count<= 0 then failwith "exit"//todo
                 else bufftop <- cursor.span.address + count 
                      reset cursor
                      x.get ()

    type OutputBuffer(memory : Memory, cursor : Cursor)=
        do 
            if cursor.isInverse then failwith "not possible to use inverse memory as buffer"
        member x.flush() =
            use out = System.Console.OpenStandardOutput()
            out.Write(memory, int cursor.span.address, used cursor)
            out.Flush()
            reset cursor

        member x.setString span = 
            for i in span.address..(top span) do
                x.set memory.[i]

        member x.set c = 
            write memory cursor c
            if isFull cursor || c = byte '\n' then x.flush()

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
            dec cursor baseSize |> ignore
            setInt memory cursor.current v
    
        member x.pop () = 
            assert (cursor.current < top cursor.span)
            let v = getInt memory cursor.current
            inc cursor baseSize |> ignore
            v

        member x.peek (offset:int) = getInt memory (cursor.current + (baseSize * offset))
    
        member x.apply f = x.peek 0 |> f |> setInt memory cursor.current 

    type Variable(memory:Memory, cursor : Cursor, init: int)=
        do  
            setInt memory cursor.span.address init
        member x.address = cursor.span.address
        member x.value 
            with get () = getInt memory cursor.span.address
            and set v = setInt memory cursor.span.address v

module ForthVM =
    open Memory

    type FnPointer = Int32
    type PredefinedWords = {
        NEXT : FnPointer
        DOCOL : FnPointer //NEST
        EXIT : FnPointer//UNNEST
    }
    type Fn = ForthVM -> FnPointer
    
    and CodeMemory()=
        //native funcs addressable storage
        let nativeFuncs : Fn[] = Array.zeroCreate (1<<<8) 
        let mutable nextFnPointer = 0
        let addFn f = 
            nativeFuncs.[nextFnPointer] <- f
            nextFnPointer <- nextFnPointer + 1
            nextFnPointer - 1
    
        let next (vm:ForthVM) = 
            vm.W <- getInt vm.memory vm.IP//current codeword
            vm.IP <- vm.IP + baseSize
            getInt vm.memory vm.W//native fn address
        let next = addFn next
        //alternative names: docolon, enter
        let docol (vm:ForthVM) = 
            vm.RSP.push(int vm.IP)
            vm.IP <- vm.W
            vm.IP <- vm.IP + baseSize
            next
        let docol = addFn docol
        //alternative names: exit
        let exit (vm:ForthVM) =
            vm.IP <- vm.RSP.pop() 
            next
    
        let exit = addFn exit

        let constFn (vm:ForthVM) =
            let value = getInt vm.memory (vm.IP + baseSize)
            vm.SP.push value 
            next

        member x.CONST = addFn constFn
    
        member x.DirectPredefinedWords = {
            NEXT = next
            DOCOL = docol
            EXIT = exit
        }

        member x.addFunction = addFn
    
        member x.get idx = nativeFuncs.[idx]

    and ForthVM = {
        memory : Memory
        allocated : Cursor
        data : Cursor
        SP : Stack
        RSP : Stack
        input_buffer : InputBuffer
        out_buffer : OutputBuffer
        word_buffer : StringBuffer
        STATE : Variable
        LATEST : Variable
        HERE : Variable
        BASE : Variable
        errmsg : Span
        errmsgnl : Span
        mutable IP : int //istruction pointer in bytes
        mutable W : int //work

        //indirect fn calls
        NEXT : Variable
        DOCOL : Variable
        EXIT : Variable
        QUIT : Variable//cold start
    }

    let create size config  = 
        let memory = createMemory size
        let allocated = createCursor 0 size false 
        let allocate bytes isInverse init = 
            let address = inc allocated bytes
            let c = createCursor address bytes isInverse
            init (memory,c)

        let var init (memory, cursor) = Variable(memory, cursor, init)

        let reserveString (str:string) = 
            let arr = Encoding.ASCII.GetBytes str
            let init (m,c) = copyFromBytes m c.span.address arr
                             c.span
            allocate arr.Length false init
        {
            memory = memory
            allocated = allocated
            data = allocate config.INITIAL_DATA_SEGMENT_SIZE false snd
            SP = allocate config.DATA_STACK_SIZE true Stack
            RSP = allocate config.RETURN_STACK_SIZE true Stack
            input_buffer = allocate config.BUFFER_SIZE false InputBuffer
            out_buffer= allocate config.BUFFER_SIZE false OutputBuffer
            word_buffer = allocate 32 false StringBuffer
            STATE = allocate baseSize false <| var 0
            LATEST = allocate baseSize false <| var 0
            HERE = allocate baseSize false <| var 0
            BASE = allocate baseSize false <| var 10
            errmsg = reserveString "PARSE ERROR: "
            errmsgnl = reserveString Environment.NewLine
            IP = 0
            W = 0
            NEXT = allocate baseSize false <| var 0
            DOCOL = allocate baseSize false <| var 0
            EXIT = allocate baseSize false <| var 0
            QUIT = allocate baseSize false <| var 0
        }

module Words = 
    open ForthVM
    [<Flags>]
    type Flags =
        | NONE = 0
        | IMMEDIATE = 0x80
        | HIDDEN = 0x20

    let LENMASK = 0x1f
    type ReadMode = SKIP|WORD|COMMENT//word parser modes

    type Writer(vm:ForthVM, code:CodeMemory) =
        let writeByte (v:byte) = 
            let address = vm.HERE.value
            vm.memory.[address] <- v
            vm.HERE.value <- address + 1

        let write (v:Int32) = 
            let address = vm.HERE.value
            Memory.setInt vm.memory address v
            vm.HERE.value <- address + Memory.baseSize
        
        let writeString (str:String) = Encoding.ASCII.GetBytes str |> Array.iter writeByte 

        let align () = vm.HERE.value <- Memory.pad vm.HERE.value  
        
        member x.setIndirectPredefinedWords () =
            vm.NEXT.value <- code.DirectPredefinedWords.NEXT
            vm.DOCOL.value <- code.DirectPredefinedWords.DOCOL
            vm.EXIT.value <- code.DirectPredefinedWords.EXIT
            {
                NEXT = vm.NEXT.address
                DOCOL = vm.DOCOL.address
                EXIT = vm.EXIT.address
            }
         member x.setQUIT quit = vm.QUIT.value <- quit
        member x.create (name: string) (flags:Flags) =
            let nameSize = byte name.Length
            assert (nameSize <= 31uy)
            let flags = byte flags

            let curlink = vm.LATEST.value
            vm.LATEST.value <- vm.HERE.value//set link
            write(curlink)
            nameSize + flags |> writeByte
            writeString name
            align()

        member x.writeCodewordPayloadRetCodeword (name: string) (flags:Flags) (codeword:FnPointer) (payload:Int32[]) = 
            x.create name flags 
            let codewordAddr = vm.HERE.value
            write codeword
            for d in payload do
                write d 
            codewordAddr

        member x.defwordRetCodeword (name: string) (flags:Flags) (program:Int32[]) = 
            x.writeCodewordPayloadRetCodeword name flags code.DirectPredefinedWords.DOCOL program

        member x.defword (name: string) (flags:Flags) (program:Int32[]) = 
            x.defwordRetCodeword name flags program |>ignore

        //add native word
        member x.defcodeRetCodeword (name: string) (flags:Flags) (f:Fn) = 
            let fAddr = code.addFunction f
            x.writeCodewordPayloadRetCodeword name flags fAddr Array.empty

        member x.defcode (name: string) (flags:Flags) (f:Fn) = x.defcodeRetCodeword name flags f |> ignore

        member x.defconstRetCodeword (name: string) (flags:Flags) (value:ForthVM ->Int32) = 
            let value = value vm
            x.writeCodewordPayloadRetCodeword name flags code.CONST [|value|]

        member x.defconst (name: string) (flags:Flags) (value:ForthVM ->Int32) = 
            x.defconstRetCodeword name flags value |> ignore
        //add variable
        member x.defvarRetCodeword (name: string) (flags:Flags) (varAddress:ForthVM -> Int32) (initial:Int32 option) = 
            let varAddress = varAddress vm
            match initial with  
                | Some(initial) -> Memory.setInt vm.memory varAddress initial
                | _ -> ()
            x.defconstRetCodeword name flags (fun vm -> varAddress)
        member x.defvar (name: string) (flags:Flags) (varAddress:ForthVM -> Int32) (initial:Int32 option) = x.defvarRetCodeword name flags varAddress initial |> ignore
        
        
    let init (x: Writer) (words:PredefinedWords) = 
        let codewords = x.setIndirectPredefinedWords ()
        x.defcode "DROP" Flags.NONE (fun vm -> 
            vm.SP.pop() |> ignore
            words.NEXT
        ) 

        x.defcode "SWAP" Flags.NONE (fun vm -> 
            let x = vm.SP.pop()
            let y = vm.SP.pop()
            vm.SP.push(x)
            vm.SP.push(y)
            words.NEXT
        ) 
        x.defcode "DUP" Flags.NONE (fun vm -> 
            vm.SP.push(vm.SP.peek 0)// duplicate top of stack
            words.NEXT
        ) 
        x.defcode "OVER" Flags.NONE (fun vm -> 
            vm.SP.push(vm.SP.peek 1)//get the second element of stack and push it on top
            words.NEXT
        ) 
        //( a b c -- b c a )
        x.defcode "ROT" Flags.NONE (fun vm -> 
            let eax = vm.SP.pop()
            let ebx = vm.SP.pop()
            let ecx = vm.SP.pop()
            vm.SP.push ebx
            vm.SP.push eax
            vm.SP.push ecx
            words.NEXT 
        ) 
        //( a b c -- c a b ) rot rot 
        x.defcode "-ROT" Flags.NONE (fun vm -> 
            let eax = vm.SP.pop()
            let ebx = vm.SP.pop()
            let ecx = vm.SP.pop()
            vm.SP.push eax
            vm.SP.push ecx
            vm.SP.push ebx
            words.NEXT 
        ) 
        //( a b -- ) drop drop ;
        x.defcode "2DROP" Flags.NONE (fun vm ->  // drop top two elements of stack
            vm.SP.pop () |> ignore
            vm.SP.pop () |> ignore
            words.NEXT 
        ) 
        //( a b -- a b a b ) over over ;
        x.defcode "2DUP" Flags.NONE (fun vm ->  // duplicate top two elements of stack
            let b = vm.SP.peek 0
            let a = vm.SP.peek 1
            vm.SP.push a
            vm.SP.push b
            words.NEXT 
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
            words.NEXT 
        ) 

        let apply (vm:ForthVM) = vm.SP.apply
        let apply2 (vm:ForthVM) (f: Int32->Int32->Int32)  = vm.SP.apply (f <| vm.SP.pop())
        let boolToBase b = if b then -1 else 0
    
        let applyBool vm f  = apply vm (fun a -> f a |> boolToBase) 
        let applyBool2 vm f  = apply2 vm (fun a b -> f a b |> boolToBase)

        let def applier name f= x.defcode name Flags.NONE (fun vm -> 
            applier vm f 
            words.NEXT  
        ) 

        //( a -- a a | 0 ) dup if dup then 
        //Duplicate x if it is non-zero. 
        x.defcode "?DUP" Flags.NONE (fun vm -> // duplicate top of stack if non-zero
            let eax = vm.SP.peek 0
            if eax <> 0 then vm.SP.push eax
            words.NEXT 
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
            words.NEXT 
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
    
        let LIT = x.defcodeRetCodeword "LIT" Flags.NONE (fun vm ->
            vm.W <- vm.IP
            vm.IP <- vm.IP + Memory.baseSize
            Memory.getInt vm.memory vm.W |> vm.SP.push
            words.NEXT
        )
        x.defcode "!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.SP.pop()
            Memory.setInt vm.memory address data
            words.NEXT 
        )
        let FETCH = x.defcodeRetCodeword "@" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = Memory.getInt vm.memory address 
            vm.SP.push data
            words.NEXT 
        )
        x.defcode "+!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let amount = vm.SP.pop()
            Memory.setInt vm.memory address (Memory.getInt vm.memory address + amount)
            words.NEXT 
        )
        x.defcode "-!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let amount = vm.SP.pop()
            Memory.setInt vm.memory address (Memory.getInt vm.memory address - amount)
            words.NEXT 
        )

        //! and @ (STORE and FETCH) store 32-bit words.  It's also useful to be able to read and write bytes
        //so we also define standard words C@ and C!.
        //Byte-oriented operations only work on architectures which permit them (i386 is one of those).

        x.defcode "C!" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = vm.SP.pop()
            vm.memory.[address] <- byte data
            words.NEXT 
        )
        x.defcode "C@" Flags.NONE (fun vm -> 
            let address = vm.SP.pop()
            let data = int vm.memory.[address]
            vm.SP.push <| data
            words.NEXT 
        )

        // C@C! is a useful byte copy primitive. 
        x.defcode "C@C!" Flags.NONE (fun vm -> 
            let destination = vm.SP.pop()
            let source = vm.SP.pop()
            vm.memory.[destination] <- vm.memory.[source]
            vm.SP.push (source + 1)// increment source address
            vm.SP.push (destination + 1)// increment destination address
            words.NEXT
        )

        // and CMOVE is a block copy operation. 
        x.defcode "CMOVE" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let destination = vm.SP.pop()
            let source = vm.SP.pop()
            for i in 0..length do
                vm.memory.[destination + i] <- vm.memory.[source + i]
            words.NEXT 
        )

        x.defvar "STATE" Flags.NONE (fun vm -> vm.STATE.address) Option.None
        x.defvar "HERE" Flags.NONE (fun vm -> vm.HERE.address) Option.None
        let LATEST = x.defvarRetCodeword "LATEST" Flags.NONE (fun vm -> vm.LATEST.address) Option.None //name_SYSCALL0 // SYSCALL0 must be last in built-in dictionary
        x.defvar "BASE" Flags.NONE (fun vm -> vm.BASE.address) Option.None
        
        x.defconst "S0" Flags.NONE (fun vm -> vm.SP.S0)
        x.defconst "VERSION" Flags.NONE (fun vm -> 1)
        let RZ = x.defconstRetCodeword "R0" Flags.NONE (fun vm -> vm.RSP.S0)
        x.defconst "DOCOL" Flags.NONE (fun vm -> words.DOCOL)//probaly words -> codewords
        x.defconst "F_IMMED" Flags.NONE (fun vm ->  int Flags.IMMEDIATE)
        x.defconst "F_HIDDEN" Flags.NONE (fun vm ->  int Flags.HIDDEN)
        x.defconst "F_LENMASK" Flags.NONE (fun vm -> LENMASK)
        
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
            words.NEXT 
        )
        x.defcode "R>" Flags.NONE (fun vm -> 
            vm.RSP.pop() |> vm.SP.push
            words.NEXT 
        )
        //get return stack pointer
        x.defcode "RSP@" Flags.NONE (fun vm -> 
            vm.SP.push vm.RSP.top
            words.NEXT 
        )
        //set return stack pointer
        let RSPSTORE = x.defcodeRetCodeword "RSP!" Flags.NONE (fun vm -> 
            vm.RSP.top <- vm.SP.pop()
            words.NEXT 
        )
        x.defcode "RDROP" Flags.NONE (fun vm -> 
            vm.RSP.pop()|> ignore
            words.NEXT 
        )
        //PARAMETER (DATA) STACK ----------------------------------------------------------------------
        //get data stack pointer
        x.defcode "DSP@" Flags.NONE (fun vm -> 
            vm.SP.push vm.SP.top
            words.NEXT 
        )
        //set data stack pointer
        x.defcode "DSP!" Flags.NONE (fun vm -> 
            vm.SP.top <- vm.SP.pop()
            words.NEXT 
        )

        x.defcode "DSP!" Flags.NONE (fun vm -> 
            vm.SP.top <- vm.SP.pop()
            words.NEXT 
        )
        //io
        x.defcode "KEY" Flags.NONE (fun vm -> 
            vm.input_buffer.get() |> int |> vm.SP.push
            words.NEXT 
        )
        x.defcode "EMIT" Flags.NONE (fun vm -> 
            vm.SP.pop() |> byte |> vm.out_buffer.set
            words.NEXT 
        )
         
        let rec readWord vm mode : Memory.Span =
            let c = vm.input_buffer.get() 
            match (char c,mode) with
                | ('\n', ReadMode.COMMENT) -> readWord vm ReadMode.SKIP
                | (_, ReadMode.COMMENT) -> readWord vm ReadMode.COMMENT
                | (c, ReadMode.SKIP) when c = ' ' || c = '\t' || c = '\r' || c = '\n' -> readWord vm ReadMode.SKIP 
                | (c, ReadMode.WORD) when c = ' ' || c = '\t' || c = '\r' || c = '\n' -> vm.word_buffer.getStrAndReset()
                | ('/', ReadMode.SKIP) -> readWord vm ReadMode.COMMENT
                | (_, _) -> vm.word_buffer.write c
                            readWord vm ReadMode.WORD

        let _WORD vm = readWord vm ReadMode.SKIP

        let WORD = x.defcodeRetCodeword "WORD" Flags.NONE (fun vm -> 
            let str = _WORD vm
            vm.SP.push str.address
            vm.SP.push str.size
            words.NEXT 
        )

        let _NUMBER vm (str:Memory.Span) = 
            let radix = vm.BASE.value
            let mutable n = 0
            let mutable idx = 0

            let current() = int vm.memory.[str.address + idx]

            let mutable c = current ()
            let isNegative = '-' = char c
            if isNegative then idx <- 1
            let mutable cnt = true
            while cnt && idx < str.size do
                n <- n * radix
                let d = current() - (int '0')
                if d > radix || d < 0 then cnt <- false
                else n <- n + d
                     idx <- idx + 1
            
            let count = if str.size = 0 then 0 else str.size - idx

            if isNegative then -n, count else n, count

        x.defcode "NUMBER" Flags.NONE (fun vm -> 
            let len, addr = vm.SP.pop(), vm.SP.pop()
            let number, numberOfUnparsedChars = _NUMBER vm {address = addr ; size = len}
            vm.SP.push number
            vm.SP.push numberOfUnparsedChars // 0 if no error
            words.NEXT 
        )

        let rec eqStrings (vm:ForthVM) address address2 length = 
            if length = 0 
            then true
            else if vm.memory.[address] = vm.memory.[address2]
                    then eqStrings vm (address+1) (address2+1) (length - 1)
                    else false

        let _FIND vm (str:Memory.Span) = 
            let mutable wordAddr = vm.LATEST.value
            let mutable cnt = true
            while cnt do
                if wordAddr = 0 
                then cnt <- false
                else let flagsLen = Memory.getInt vm.memory (wordAddr + Memory.baseSize)
                     let flagsLen = (int Flags.HIDDEN ||| LENMASK) &&& flagsLen
                     if flagsLen = str.size && eqStrings vm str.address (wordAddr + Memory.baseSize + 1) str.size
                     then cnt <- false
                     else wordAddr <- Memory.getInt vm.memory wordAddr
            wordAddr
                     

        let FIND = x.defcodeRetCodeword "FIND" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            let addrOfDictEntry = _FIND vm {address = address; size = length }
            vm.SP.push addrOfDictEntry
            words.NEXT 
        )
        let _TCFA (vm:ForthVM) linkAddress = 
            let flagsLenAddress = linkAddress + Memory.baseSize
            let length = Memory.getInt vm.memory flagsLenAddress &&& LENMASK
            let cfaAddress = flagsLenAddress + 1 + length |> Memory.pad
            cfaAddress

        x.defcode ">CFA" Flags.NONE (fun vm -> 
            let worAddress = vm.SP.pop()
            let cfaAddress = _TCFA vm worAddress
            vm.SP.push cfaAddress
            words.NEXT 
        )
        x.defcode ">DFA" Flags.NONE (fun vm -> 
            let worAddress = vm.SP.pop()
            let cfaAddress = _TCFA vm worAddress
            vm.SP.push (cfaAddress + Memory.baseSize)
            words.NEXT 
        )
        let readString (vm:ForthVM) address length = 
            let arr = Array.create length ' '
            for i in 0..(length - 1) do
                arr.[i] <- char (vm.memory.[address + i])
            new String(arr)

        let CREATE = x.defcodeRetCodeword "CREATE" Flags.NONE (fun vm -> 
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            let name = readString vm address length
            x.create name Flags.NONE
            words.NEXT 
        )
        let _COMMA (vm:ForthVM) v =
            let address = vm.HERE.value
            Memory.setInt vm.memory address v
            vm.HERE.value <-address + Memory.baseSize

        let COMMA = x.defcodeRetCodeword "," Flags.NONE (fun vm -> 
            let v = vm.SP.pop()
            _COMMA vm v
            words.NEXT 
        )

        let LBRAC = x.defcodeRetCodeword "[" Flags.IMMEDIATE (fun vm -> 
            vm.STATE.value <- 0
            words.NEXT 
        )
        let RBRAC = x.defcodeRetCodeword "]" Flags.IMMEDIATE (fun vm -> 
            vm.STATE.value <- 1
            words.NEXT 
        )

        let toggleLastWordFlag (flag:Flags) (vm:ForthVM)  = 
            let flagsAddress = vm.LATEST.value + Memory.baseSize
            let flagsLen = Memory.getInt vm.memory flagsAddress//todo or get byte
            let flagsLen = flagsLen ^^^ int flag //toggle immediate
            Memory.setInt vm.memory flagsAddress flagsLen
            words.NEXT 

        let IMMEDIATE = x.defcodeRetCodeword "IMMEDIATE" Flags.IMMEDIATE (toggleLastWordFlag Flags.IMMEDIATE)

        let HIDDEN = x.defcodeRetCodeword "HIDDEN" Flags.IMMEDIATE (toggleLastWordFlag Flags.HIDDEN)

        x.defword ":" Flags.NONE 
            [|
                WORD;// Get the name of the new word
                CREATE;// CREATE the dictionary entry / header
                LIT; codewords.DOCOL; COMMA;// Append DOCOL  (the codeword).
                LATEST; FETCH; HIDDEN // Make the word hidden (see below for definition).
                RBRAC;// Go into compile mode.
                codewords.EXIT// Return from the function.
            |]
        
        x.defword ";" Flags.IMMEDIATE 
            [|
                LIT; codewords.EXIT; COMMA;// Append EXIT (so the word will return).
                LATEST; FETCH; HIDDEN; // Toggle hidden flag -- unhide the word (see below for definition).
                LBRAC;// Go back to IMMEDIATE mode.
                codewords.EXIT;// Return from the function.
            |]
        x.defword "HIDE" Flags.NONE [|WORD;FIND;HIDDEN;codewords.EXIT|]

        let TICK = x.defcodeRetCodeword "'" Flags.NONE (fun vm ->
            vm.SP.push <| Memory.getInt vm.memory vm.IP//todo check
            vm.IP <- vm.IP + Memory.baseSize
            words.NEXT 
        )

        let BRANCH = x.defcodeRetCodeword "BRANCH" Flags.NONE (fun vm ->
            vm.IP <- vm.IP + Memory.getInt vm.memory vm.IP // add the offset to the instruction pointer
            words.NEXT 
        )
        x.defcode "0BRANCH" Flags.NONE (fun vm ->
            if vm.SP.pop () = 0 // top of stack is zero?
            then vm.IP <- vm.IP + Memory.getInt vm.memory vm.IP // add the offset to the instruction pointer
            else vm.IP <- vm.IP + Memory.baseSize// otherwise we need to skip the offset
            words.NEXT 
        )

        x.defcode "LITSTRING" Flags.NONE (fun vm ->
            let length = Memory.getInt vm.memory vm.IP
            vm.IP <- vm.IP + Memory.baseSize
            vm.SP.push vm.IP     // push the address of the start of the string
            vm.SP.push length    // push length on the stack
            vm.IP <- vm.IP + length |> Memory.pad  // skip past the string and round up to next BaseSize byte boundary
            words.NEXT 
        )
        x.defcode "TELL" Flags.NONE (fun vm ->
            let length = vm.SP.pop()
            let address = vm.SP.pop()
            vm.out_buffer.setString {address = address; size = length}
            words.NEXT 
        )


        let INTERPRET = x.defcodeRetCodeword "INTERPRET" Flags.NONE (fun vm ->
            let word = _WORD vm // Returns %ecx = length, %edi = pointer to word.
            let pointer = _FIND vm word//pointer to header or 0 if not found.
            if pointer <> 0 //found word
            then
                let nameFlags = Memory.getInt vm.memory pointer
                let codeword = _TCFA vm pointer
                let isImmediate = (nameFlags &&& int Flags.IMMEDIATE) <> 0
                if isImmediate
                then codeword// If IMMED, jump straight to executing.
                else if vm.STATE.value = 0// Are we compiling or executing?
                        then codeword// Jump if executing.
                        else // Compiling - just append the word to the current dictionary definition.
                            _COMMA vm codeword
                            words.NEXT
            else // Not in the dictionary (not a word) so assume it's a literal number.
                    let number, numberOfUnparsedChars = _NUMBER vm word
                    if numberOfUnparsedChars > 0
                    then vm.out_buffer.setString vm.errmsg
                         vm.out_buffer.setString <| vm.input_buffer.GetLast 40
                         vm.out_buffer.setString vm.errmsgnl
                         words.NEXT
                    else if vm.STATE.value = 0// Are we compiling or executing?vm.SP.push number
                        then vm.SP.push number// Jump if executing.
                             words.NEXT
                        else // Compiling - just append the word to the current dictionary definition.
                            _COMMA vm LIT
                            _COMMA vm number
                            words.NEXT
        )
        
        let QUIT = x.defwordRetCodeword "QUIT" Flags.NONE
                    [|
                        RZ;RSPSTORE;// R0 RSP!, clear the return stack
                        INTERPRET;// interpret the next word
                        BRANCH;-(Memory.baseSize * 2)// and loop (indefinitely)
                    |]
        x.defcode "CHAR" Flags.NONE (fun vm ->
            let str = _WORD vm
            let c = Memory.getInt vm.memory str.address
            vm.SP.push c
            words.NEXT
        )
        x.defcode "EXECUTE" Flags.NONE (fun vm ->
            let addr = vm.SP.pop()// Get xt into %eax and jump to it. After xt runs its NEXT will continue executing the current word.
            addr
        )
        x.setQUIT QUIT //cold start

