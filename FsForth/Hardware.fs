module Hardware

open System
open System.Text

module Memory = 

    [<Literal>]
    let baseSize = 4//sizeof<Int32>
    //allign memory address to baseSize 
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
    //top address of a span
    let top span = span.address + span.size - 1

    type Cursor = {
        span : Span
        mutable current : int
    }


    let reset cursor = cursor.current <- cursor.span.address
    let used cursor = cursor.current - cursor.span.address
    let isFull cursor = used cursor = cursor.span.size
    let createCursor address size = 
        { 
            span = { address = address; size = size }
            current = address
        }

    let validate (cursor:Cursor) = 
        if cursor.current < cursor.span.address || cursor.current > (top cursor.span + 1)
        then failwith <| sprintf "broken span bounds %A" cursor

    let change op  (v:Cursor) count = 
        let prev = v.current
        v.current <- op v.current count
        validate v
        prev

    let inc = change (+)
    let dec = change (-) 

    type Memory = byte[]
    let createMemory (size:int) = Array.create size 0uy 

    let getInt (memory:Memory) (address:int) = BitConverter.ToInt32(memory, address |> int)

    let setInt (memory:Memory) (address:int) (v:int) = BitConverter.GetBytes(v).CopyTo(memory, address |> int);

    let copyFromBytes (memory:Memory) (address:int) (arr:byte array) = arr.CopyTo(memory, int address)
    let copyToBytes (memory:Memory) (address:int) (arr:byte array) = Array.Copy(memory, address, arr, 0, arr.Length)

    let toString (memory:Memory) span = 
        seq {span.address..(top span)}
        |> Seq.map (Array.get memory)
        |> Seq.map char 
        |> String.Concat

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

        member x.flush() =
            use out = System.Console.OpenStandardOutput()
            out.Write(memory, int cursor.span.address, used cursor)
            out.Flush()
            reset cursor

        member x.setString span = 
            let debug = toString memory span
            for i in span.address..(top span) do
                x.set memory.[i]

        member x.set c = 
            write memory cursor c
            if true //isFull cursor || c = byte '\n' 
            then x.flush()

    type Stack(memory : Memory, cursor : Cursor)=
        do
            cursor.current <- top cursor.span
        member x.reset () = cursor.current <- x.S0
        member x.top 
            with get () = cursor.current
            and set (value) = cursor.current <- value
        member x.S0 = top cursor.span 
        member x.count () = (x.S0 - x.top) / baseSize 
        member x.push v = 
            dec cursor baseSize |> ignore
            setInt memory cursor.current v
    
        member x.pop () = 
            let v = getInt memory cursor.current
            inc cursor baseSize |> ignore
            v
        //get n-th value from the stack. Latest pushed has offset 0. No state changes
        member x.peek (offset:int) = getInt memory (cursor.current + (baseSize * offset))
        //replace a top value from the stack by a result of a function f applied to the top value
        member x.apply f = x.peek 0 |> f |> setInt memory cursor.current 
        //get string representation of elements in the stack
        override x.ToString() = 
            seq {x.top..baseSize..(x.S0-baseSize)} 
            |> Seq.map (getInt memory)
            |> Seq.map string
            |> String.concat ";"
    //allocates memory for a single value
    type Variable(memory:Memory, cursor : Cursor, init: int)=
        do  
            setInt memory cursor.span.address init
        member x.address = cursor.span.address
        member x.value 
            with get () = getInt memory cursor.span.address
            and set v = setInt memory cursor.span.address v

module CodeMemory = 
    type FnPointer = Int32//function idx in Fn[]
    
    //native funcs addressable storage
    type CodeMemory<'Fn>()=
        let nativeFuncs : 'Fn[] = Array.zeroCreate (1<<<8) 
        let mutable nextFnPointer : FnPointer = 0
        let addFn f = 
            nativeFuncs.[nextFnPointer] <- f
            nextFnPointer <- nextFnPointer + 1
            nextFnPointer - 1

        member x.addFunction = addFn
    
        member x.get (idx: FnPointer) = nativeFuncs.[idx]


module ForthVM =
    open Memory
    open CodeMemory
    type Fn = ForthVM -> FnPointer//take vm do modifications and return address of the next function
    and ForthVM = {
        memory : Memory//byte array wrapper
        //main registers
        mutable IP : int //istruction pointer the same as %esi in assembly implementation
        mutable W : int //work register the same as %eax in assembly implementation

        allocated : Cursor//all allocated memory regions, not used
        data : Cursor//memory region for forth words, not actually used
        SP : Stack//data stack
        RSP : Stack//return stack
        input_buffer : InputBuffer//input buffer to read from stdin
        out_buffer : OutputBuffer//output buffer to write to stdout
        word_buffer : StringBuffer//temp string storage for input parser
        //reserved memory for main forth variables, they will be described later
        STATE : Variable
        LATEST : Variable
        HERE : Variable
        BASE : Variable
        //reserved memory for parser error messages
        errmsg : Span
        errmsgnl : Span
        //indirect fn calls
        NEXT : Variable
        DOCOL : Variable
        EXIT : Variable
        //cold start
        EntryPoint : Variable
        CodeMemory : CodeMemory<Fn>
    }

    type PredefinedWords = {
        NEXT : FnPointer
        DOCOL : FnPointer//NEST
        EXIT : FnPointer//UNNEST
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
            EntryPoint = allocate baseSize  <| var 0
            CodeMemory = CodeMemory<Fn>()
        }

    let rec private runFn (vm:ForthVM) (fn : FnPointer)= 
        let fn = vm.CodeMemory.get fn vm
        runFn vm fn

    let run (vm:ForthVM) next =
        vm.IP <- vm.EntryPoint.address
        runFn vm next

