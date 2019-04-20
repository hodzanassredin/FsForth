
module Forth 

open System
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Runtime.CompilerServices

type BaseType = Int32
type FnPointer = BaseType
let baseSize = sizeof<BaseType>

if sizeof<nativeint> = baseSize |> not then failwith "not supported build mode, use x86"
//type NativeFnDelegate = delegate of unit -> unit
//let d = new NativeFnDelegate(f)
//let pointer = Marshal.GetFunctionPointerForDelegate(f)

type Memory(memory : byte[]) =
    member x.getBase offset = 
        let memoryInts = MemoryMarshal.Cast<byte,BaseType>(new Span<byte>(memory))
        memoryInts.[offset]

    member x.get offset = memory.[offset]

    member x.set offset v = memory.[offset] <- v 

    member x.setBase offset v = //offset in bytes
        assert (offset % baseSize = 0)
        let memoryInts = MemoryMarshal.Cast<byte,BaseType>(new Span<byte>(memory))
        memoryInts.[offset] <- v 

type Register(init:BaseType)=
    let mutable r = init;
    member x.value = r
    member x.set v = r <- v
    member x.inc () = r <- r + baseSize
    member x.dec () = r <- r - baseSize

type StackRegister(memory : Memory, base_:int, size:int)=
    let mutable sp = Register(-1) //stack pointer
    member x.push v = 
        assert (sp.value < size)
        sp.inc()
        memory.setBase (sp.value + base_) v

    member x.pop () = 
        assert (sp.value <> -1)
        let v = memory.getBase (sp.value + base_)
        sp.dec()
        v

[<Flags>]
type Flags =
    | NONE = 0
    | IMMEDIATE = 0x80
    | HIDDEN = 0x20
let LENMASK = 0x1f



type ForthState(memory : byte[]) =
    let mutable memoryTop = memory.Length - 1
    let reserveMemory size = memoryTop <- memoryTop-(size * baseSize)
                             memoryTop
    //memory for main forth variables
    let STATE : BaseType = reserveMemory 1//Is the interpreter executing code (0) or compiling a word (non-zero)?
    let LATEST : BaseType = reserveMemory 1//Points to the latest (most recently defined) word in the dictionary.
    let HERE : BaseType = reserveMemory 1//Points to the next free byte of memory.  When compiling, compiled words go here.
    let S0 : BaseType = reserveMemory 1//Stores the address of the top of the parameter stack.
    let BASE : BaseType = reserveMemory 1//The current base for printing and reading numbers.
    //native funcs addressable storage
    let nativeFuncs : Fn[] = Array.zeroCreate (1<<<8) 
    let mutable nextFnPointer = 0
    
    //constant stack offsets
    let spBase : BaseType = reserveMemory 256//offset of data stack
    let rspBase : BaseType = reserveMemory 256//offset of return stack
    //registers
    [<DefaultValue>] val mutable IP : BaseType//istruction pointer in bytes
    [<DefaultValue>] val mutable W : BaseType//work
    [<DefaultValue>] val mutable SP : BaseType//data stack pointer
    //[<DefaultValue>] val mutable RSP : BaseType//return data stack pointer
    val SP 
    let addFunction f = 
        nativeFuncs.[nextFnPointer] <- f
        nextFnPointer <- nextFnPointer + 1
        nextFnPointer - 1

    
    
    member x.pushSP v = 
        incReg(&x.SP)
        x.setMemory (x.SP + spBase) v

    member x.popSP () = 
        assert (x.SP <> 0)
        let v = x.getMemory (x.SP + spBase)
        decReg(&x.SP)
        v

    member x.pushRSP v = 
        incReg(&x.RSP)
        x.setMemory (x.RSP + rspBase) v

    member x.popRSP () = 
        assert (x.RSP <> 0)
        let v = x.getMemory (x.RSP + rspBase)
        decReg(&x.RSP)
        v

    member x.run (fn:Fn) =  
        let nextFn = fn x
        x.run <| nativeFuncs.[nextFn]
    
    member x.AddToDictionary (v:byte) = 
        let pointer = x.getMemory HERE
        memory.[pointer] <- v
        x.setMemory HERE (pointer + 1)
        
    member x.AddToDictionary (v:BaseType) = 
        let pointer = x.getMemory HERE
        x.setMemory pointer v
        x.setMemory HERE (pointer + baseSize)
    
    //add forth word
    member x.defword (name: string) (flags:Flags) (code:BaseType[]) = 
        let link = x.getMemory LATEST
        let name = System.Text.Encoding.ASCII.GetBytes(name)
        let nameSize = byte name.Length
        ()
    //add native word
    member x.defcode (name: string) (flags:Flags) (code:Fn) = 
        ()
    //add variable
    member x.defvar (name: string) (flags:Flags) (varAddress:BaseType) = ()

and Fn = ForthState -> FnPointer
//VM core funcs
let initCoreFuncs (state:ForthState) = 
    let next (s:ForthState) = 
        s.W <- s.IP
        incReg(&s.IP)
        state.getMemory s.W
    //alternative names: docolon, enter
    let nest (s:ForthState) =
        s.pushRSP(s.IP)
        s.IP <- s.W
        incReg(&s.IP)
        next s
    //alternative names: exit
    let unnest  (s:ForthState) =
        state.IP <- state.popRSP()
        next s

    state.defcode "DROP" Flags.NONE (fun s -> 
        s.popSP() |> ignore
        next s
    ) 

    state.defcode "SWAP" Flags.NONE (fun s -> 
        let x = s.popSP()
        let y = s.popSP()
        s.pushSP(x)
        s.pushSP(y)
        next s
    ) 
    state.defcode "DUP" Flags.NONE (fun s -> 
        let x = s.popSP()// duplicate top of stack
        s.pushSP(x)
        s.pushSP(x)
        next s
    ) 
    state.defcode "OVER" Flags.NONE (fun s -> 
        let sndElemAddr = s.SP - baseSize
        assert sndElemAddr >= 0
        x = s.getMemory ()// get the second element of stack
        s.pushSP(x)// and push it on top
        next s
    ) 
    state.defcode "ROT" Flags.NONE (fun s -> 
        pop %eax
        pop %ebx
        pop %ecx
        push %ebx
        push %eax
        push %ecx
        next s
    ) 
    state.defcode "-ROT" Flags.NONE (fun s -> 
        pop %eax
        pop %ebx
        pop %ecx
        push %eax
        push %ecx
        push %ebx
        next s
    ) 
    
    state.defcode "2DROP" Flags.NONE (fun s ->  // drop top two elements of stack
        pop %eax
        pop %eax
        next s
    ) 
    state.defcode "2DUP" Flags.NONE (fun s ->  // duplicate top two elements of stack
        mov (%esp),%eax
        mov 4(%esp),%ebx
        push %ebx
        push %eax
        next s
    ) 
    state.defcode "2SWAP" Flags.NONE (fun s ->  // swap top two pairs of elements of stack
        pop %eax
        pop %ebx
        pop %ecx
        pop %edx
        push %ebx
        push %eax
        push %edx
        push %ecx
        next s
    ) 
    state.defcode "?DUP", Flags.NONE (fun s -> // duplicate top of stack if non-zero
        movl (%esp),%eax
        test %eax,%eax
        jz 1f
        push %eax
        next s
    ) 
    state.defcode "1+" Flags.NONE (fun s -> 
        incl (%esp)		// increment top of stack
        next s
    ) 
    state.defcode "1-" Flags.NONE (fun s -> 
        decl (%esp)		// decrement top of stack
        next s
    ) 
    
    state.defcode "4+" Flags.NONE (fun s -> 
        addl $4,(%esp)		// add 4 to top of stack
        next s
    ) 
    
    state.defcode "4-" Flags.NONE (fun s -> 
        subl $4,(%esp)		// subtract 4 from top of stack
        next s
    ) 
    
    state.defcode "+" Flags.NONE (fun s -> 
        pop %eax		// get top of stack
        addl %eax,(%esp)	// and add it to next word on stack
        next s
    ) 
    
    state.defcode "-" Flags.NONE (fun s -> 
        pop %eax		// get top of stack
        subl %eax,(%esp)	// and subtract it from next word on stack
        next s
    ) 
    
    state.defcode "*" Flags.NONE (fun s -> 
        pop %eax
        pop %ebx
        imull %ebx,%eax
        push %eax		// ignore overflow
        next s
    ) 
    
    //In this FORTH, only /MOD is primitive.  Later we will define the / and MOD words in
    //terms of the primitive /MOD.  The design of the i386 assembly instruction idiv which
    //leaves both quotient and remainder makes this the obvious choice.
    
    state.defcode "/MOD" Flags.NONE (fun s -> 
        xor %edx,%edx
        pop %ebx
        pop %eax
        idivl %ebx
        push %edx		// push remainder
        push %eax		// push quotient
        next s
    ) 
    
    //Lots of comparison operations like =, <, >, etc..
    
    //ANS FORTH says that the comparison words should return all (binary) 1's for
    //TRUE and all 0's for FALSE.  However this is a bit of a strange convention
    //so this FORTH breaks it and returns the more normal (for C programmers ...)
    //1 meaning TRUE and 0 meaning FALSE.
    
    state.defcode "=" Flags.NONE (fun s -> // top two words are equal?
        pop %eax
        pop %ebx
        cmp %ebx,%eax
        sete %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "<>" Flags.NONE (fun s -> // top two words are not equal?
        pop %eax
        pop %ebx
        cmp %ebx,%eax
        setne %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "<" Flags.NONE (fun s -> 
        pop %eax
        pop %ebx
        cmp %eax,%ebx
        setl %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode ">" Flags.NONE (fun s -> 
        pop %eax
        pop %ebx
        cmp %eax,%ebx
        setg %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "<=" Flags.NONE (fun s -> 
        pop %eax
        pop %ebx
        cmp %eax,%ebx
        setle %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode ">=" Flags.NONE (fun s -> 
        pop %eax
        pop %ebx
        cmp %eax,%ebx
        setge %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "0=" Flags.NONE (fun s -> // top of stack equals 0?
        pop %eax
        test %eax,%eax
        setz %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "0<>" Flags.NONE (fun s -> // top of stack not 0?
        pop %eax
        test %eax,%eax
        setnz %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "0<" Flags.NONE (fun s -> // comparisons with 0
        pop %eax
        test %eax,%eax
        setl %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "0>" Flags.NONE (fun s -> 
        pop %eax
        test %eax,%eax
        setg %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "0<=" Flags.NONE (fun s -> 
        pop %eax
        test %eax,%eax
        setle %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "0>=" Flags.NONE (fun s -> 
        pop %eax
        test %eax,%eax
        setge %al
        movzbl %al,%eax
        pushl %eax
        next s
    ) 
    
    state.defcode "AND" Flags.NONE (fun s -> // bitwise AND
        pop %eax
        andl %eax,(%esp)
        NEXT
    
    state.defcode "OR" Flags.NONE (fun s -> // bitwise OR
        pop %eax
        orl %eax,(%esp)
        next s
    ) 
    
    state.defcode "XOR" Flags.NONE (fun s -> // bitwise XOR
        pop %eax
        xorl %eax,(%esp)
        next s
    ) 
    
    state.defcode "INVERT" Flags.NONE (fun s -> // this is the FORTH bitwise "NOT" function (cf. NEGATE and NOT)
        notl (%esp)
        next s
    ) 


