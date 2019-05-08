module Hardware

open System

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

        member x.peek (offset:int) = getInt memory (cursor.current + (baseSize * offset))
    
        member x.apply f = x.peek 0 |> f |> setInt memory cursor.current 
        override x.ToString() = 
            seq {x.top..baseSize..(x.S0-baseSize)} 
            |> Seq.map (getInt memory)
            |> Seq.map string
            |> String.concat ";"

    type Variable(memory:Memory, cursor : Cursor, init: int)=
        do  
            setInt memory cursor.span.address init
        member x.address = cursor.span.address
        member x.value 
            with get () = getInt memory cursor.span.address
            and set v = setInt memory cursor.span.address v
