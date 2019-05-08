module Forth 

open System
open System.Text
open Hardware

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
    
        let nextF (vm:ForthVM) = 
            vm.W <- getInt vm.memory vm.IP//current codeword
            vm.IP <- vm.IP + baseSize
            getInt vm.memory vm.W//native fn address
        let next = addFn nextF
        //alternative names: docolon, enter
        let docol (vm:ForthVM) = 
            vm.RSP.push vm.IP
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
            let value = getInt vm.memory (vm.W + baseSize)
            vm.SP.push value 
            next

        member x.CONST = addFn constFn
    
        member x.DirectPredefinedWords = {
            NEXT = next
            DOCOL = docol
            EXIT = exit
        }

        member x.addFunction = addFn
    
        member x.get idx = 
            if next = idx then nextF else nativeFuncs.[idx]

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
        mutable IP : int //istruction pointer in bytes esi
        mutable W : int //work eax

        //indirect fn calls
        NEXT : Variable
        DOCOL : Variable
        EXIT : Variable
        QUIT : Variable//cold start
    }

    let create size config  = 
        let memory = createMemory size
        let allocated = createCursor 0 size 
        let allocate bytes init = 
            let address = inc allocated bytes
            let c = createCursor address bytes
            init (memory,c)

        let var init (memory, cursor) = Variable(memory, cursor, init)

        let reserveString (str:string) = 
            let arr = Encoding.ASCII.GetBytes str
            let init (m,c) = copyFromBytes m c.span.address arr
                             c.span
            allocate arr.Length init
        {
            memory = memory
            allocated = allocated
            data = allocate config.INITIAL_DATA_SEGMENT_SIZE snd
            SP = allocate config.DATA_STACK_SIZE Stack
            RSP = allocate config.RETURN_STACK_SIZE Stack
            input_buffer = allocate config.BUFFER_SIZE InputBuffer
            out_buffer= allocate config.BUFFER_SIZE OutputBuffer
            word_buffer = allocate 32 StringBuffer
            STATE = allocate baseSize  <| var 0
            LATEST = allocate baseSize  <| var 0
            HERE = allocate baseSize  <| var 1//0 is not defined word
            BASE = allocate baseSize  <| var 10
            errmsg = reserveString "PARSE ERROR: "
            errmsgnl = reserveString Environment.NewLine
            IP = 0
            W = 0
            NEXT = allocate baseSize  <| var 0
            DOCOL = allocate baseSize  <| var 0
            EXIT = allocate baseSize  <| var 0
            QUIT = allocate baseSize  <| var 0
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
            //x.writeCodewordPayloadRetCodeword name flags code.CONST [|value|]
            x.defcodeRetCodeword name flags (fun vm -> 
                vm.SP.push value
                code.DirectPredefinedWords.NEXT
            )

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

        x.defcode "EXIT" Flags.NONE (fun vm -> 
            words.EXIT
        ) 

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
        let boolToBase b = if b then 1 else 0
    
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

        let flip f a b = f b a 

        def apply "1+" <| (+) 1
        def apply "1-" <| flip (-) 1
        def apply "4+" <| (+) 4
        def apply "4-" <| flip (-) 4
        def apply2 "+" <| (+)
        def apply2 "-" <| flip (-)
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
        def applyBool2 "<" <| flip (<)
        def applyBool2 ">" <| flip (>)
        def applyBool2 "<=" <| flip (<=)
        def applyBool2 ">=" <| flip (>=)
        def applyBool2 ">" <| flip (>)
    
        def applyBool "0=" <| flip (=) 0
        def applyBool "0<>" <| flip (<>) 0
        def applyBool "0<" <| flip (<) 0
        def applyBool "0>" <| flip (>) 0
        def applyBool "0<=" <| flip (<=) 0
        def applyBool "0>=" <| flip (>=) 0
        //bitwise
        def apply2 "AND" (&&&) 
        def apply2 "OR" (|||) 
        def apply2 "XOR" (^^^) 
        def apply "INVERT" (~~~) 
    
        let LIT = x.defcodeRetCodeword "LIT" Flags.NONE (fun vm ->
            vm.W <- Memory.getInt vm.memory vm.IP//current codeword
            vm.IP <- vm.IP + Memory.baseSize
            //let l = Memory.getInt vm.memory vm.W
            vm.SP.push vm.W//todo fix
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
                | ('\\', ReadMode.SKIP) -> readWord vm ReadMode.COMMENT
                | (_, _) -> vm.word_buffer.write c
                            readWord vm ReadMode.WORD

        let _WORD vm = 
            let str = readWord vm ReadMode.SKIP
            str

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
                else let flagsLen = vm.memory.[wordAddr + Memory.baseSize] |> int
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
            let length = vm.memory.[flagsLenAddress] |> int &&& LENMASK
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

        let toggleWordFlag wordAddress (flag:Flags) (vm:ForthVM)  = 
            let flagsAddress = wordAddress + Memory.baseSize
            let flagsLen = vm.memory.[flagsAddress]|> int
            let flagsLen = flagsLen ^^^ int flag //toggle flag
            vm.memory.[flagsAddress] <- byte flagsLen
            words.NEXT 

        let immediate (vm:ForthVM) = toggleWordFlag vm.LATEST.value Flags.IMMEDIATE vm

        let IMMEDIATE = x.defcodeRetCodeword "IMMEDIATE" Flags.IMMEDIATE immediate

        let hidden (vm:ForthVM) = 
            let addr = vm.SP.pop()
            toggleWordFlag addr Flags.HIDDEN vm

        let HIDDEN = x.defcodeRetCodeword "HIDDEN" Flags.IMMEDIATE hidden

        x.defword ":" Flags.NONE 
            [|
                WORD;// Get the name of the new word
                CREATE;// CREATE the dictionary entry / header
                LIT; words.DOCOL; COMMA;// Append DOCOL  (the codeword).
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
            let exec cfa = 
                vm.W<-cfa
                Memory.getInt vm.memory cfa

            let word = _WORD vm // Returns %ecx = length, %edi = pointer to word.
            
            let pointer = _FIND vm word//pointer to header or 0 if not found.
            if pointer <> 0 //found word
            then
                let nameFlags = vm.memory.[pointer + Memory.baseSize] |> int
                let codeword = _TCFA vm pointer
                let isImmediate = (nameFlags &&& int Flags.IMMEDIATE) <> 0
                if isImmediate
                then exec codeword// If IMMED, jump straight to executing.
                else if vm.STATE.value = 0// Are we compiling or executing?
                        then exec codeword// Jump if executing.
                        else // Compiling - just append the word to the current dictionary definition.
                            //printfn "compiling %s" debug
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
            //printfn "read word %s" <| Memory.toString vm.memory str
            let c = int vm.memory.[str.address]
            vm.SP.push c
            words.NEXT
        )
        x.defcode "EXECUTE" Flags.NONE (fun vm ->
            let addr = vm.SP.pop()// Get xt into %eax and jump to it. After xt runs its NEXT will continue executing the current word.
            addr
        )
        x.setQUIT QUIT //cold start

