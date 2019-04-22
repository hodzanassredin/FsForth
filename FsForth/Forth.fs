
module Forth 

open System
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Runtime.CompilerServices

type BaseType = Int32
type FnPointer = BaseType
let baseSize = sizeof<BaseType>

let inc (v:BaseType byref) count = v <- v + count
let dec (v:BaseType byref) count = v <- v - count

type Memory(memory : BaseType[]) =
    let mutable memoryTop = 0
    
    member x.get offset = memory.[offset]

    member x.set offset v = memory.[offset] <- v 

    member x.reserveMemory size = 
        let free = memoryTop
        memoryTop <- memoryTop + size
        free
    member x.getByte offset : BaseType = 
        let memoryBytes = MemoryMarshal.Cast<BaseType,byte>(new Span<BaseType>(memory))
        int memoryBytes.[offset]
    member x.setByte offset (v:BaseType) = 
        let memoryBytes = MemoryMarshal.Cast<BaseType,byte>(new Span<BaseType>(memory))
        memoryBytes.[offset] <- byte v 


type Variable(memory:Memory)=
    let address = memory.reserveMemory 1
    member x.get () = memory.get address
    member x.set v = memory.set address v

type Stack(memory : Memory, size:int)=
    let base_ = memory.reserveMemory size
    let mutable sp = 0 //stack pointer
    member x.push v = 
        assert (sp < size)
        memory.set (sp + base_) v
        inc &sp baseSize
    
    member x.pop () = 
        assert (sp <> 0)
        dec &sp baseSize
        memory.get (sp + base_)

    member x.peek offset = memory.get (sp + base_ - offset)
    
    member x.apply f = x.peek 0 |> f |> memory.set (sp + base_)  

[<Flags>]
type Flags =
    | NONE = 0
    | IMMEDIATE = 0x80
    | HIDDEN = 0x20

let LENMASK = 0x1f

type Fn = unit -> FnPointer
and CodeMemory()=
    //native funcs addressable storage
    let nativeFuncs : Fn[] = Array.zeroCreate (1<<<8) 
    let mutable nextFnPointer = 0

    member x.addFunction f = 
        nativeFuncs.[nextFnPointer] <- f
        nextFnPointer <- nextFnPointer + 1
        nextFnPointer - 1

    member x.get idx = nativeFuncs.[idx]

and ForthState(memory : Memory) =
    let code = CodeMemory()
    //memory for main forth variables
    let STATE = Variable(memory)//Is the interpreter executing code (0) or compiling a word (non-zero)?
    let LATEST = Variable(memory)//Points to the latest (most recently defined) word in the dictionary.
    let HERE = Variable(memory)//Points to the next free byte of memory.  When compiling, compiled words go here.
    let S0 = Variable(memory)//Stores the address of the top of the parameter stack.
    let BASE = Variable(memory)//The current base for printing and reading numbers.
    
    let sp = Stack(memory, 256)//data stack
    let rsp = Stack(memory, 256)//return stack

    [<DefaultValue>] val mutable IP : BaseType//istruction pointer in bytes
    [<DefaultValue>] val mutable W : BaseType//work

    member x.memory = memory
    member x.SP = sp
    member x.RSP = rsp

    member x.Run (fn:Fn) =  
        let nextFn = fn ()
        x.Run <| code.get nextFn
    
    member x.AddToDictionary (v:BaseType) = 
        let pointer = HERE.get ()
        memory.set pointer v
        HERE.set (pointer + 1)
    
    //add forth word
    member x.defword (name: string) (flags:Flags) (code:BaseType[]) = 
        let link = LATEST.get()
        let name = System.Text.Encoding.ASCII.GetBytes(name)
        let nameSize = byte name.Length
        ()
    //add native word
    member x.defcode (name: string) (flags:Flags) (code:Fn) = 
        ()
    //add variable
    member x.defvar (name: string) (flags:Flags) (varAddress:BaseType) = ()


//VM core funcs
let initCoreFuncs (s:ForthState) = 
    let push = s.SP.push
    let pop = s.SP.pop
    let peek = s.SP.peek
    
    let next () = 
        s.W <- s.IP
        inc &s.IP baseSize
        s.memory.get s.W
    //alternative names: docolon, enter
    let nest () =
        s.RSP.push(s.IP)
        s.IP <- s.W
        inc &s.IP baseSize
        next ()
    //alternative names: exit
    let unnest () =
        s.IP <- s.RSP.pop()
        next ()

    s.defcode "DROP" Flags.NONE (fun s -> 
        pop() |> ignore
        next s
    ) 

    s.defcode "SWAP" Flags.NONE (fun s -> 
        let x = pop()
        let y = pop()
        push(x)
        push(y)
        next s
    ) 
    s.defcode "DUP" Flags.NONE (fun s -> 
        push(peek 0)// duplicate top of stack
        next s
    ) 
    s.defcode "OVER" Flags.NONE (fun s -> 
        push(peek 1)//get the second element of stack and push it on top
        next s
    ) 
    //( a b c -- b c a )
    s.defcode "ROT" Flags.NONE (fun s -> 
        let eax = pop()
        let ebx = pop()
        let ecx = pop()
        push ebx
        push eax
        push ecx
        next s
    ) 
    //( a b c -- c a b ) rot rot 
    s.defcode "-ROT" Flags.NONE (fun s -> 
        let eax = pop()
        let ebx = pop()
        let ecx = pop()
        push eax
        push ecx
        push ebx
        next s
    ) 
    //( a b -- ) drop drop ;
    s.defcode "2DROP" Flags.NONE (fun s ->  // drop top two elements of stack
        pop () |> ignore
        pop () |> ignore
        next s
    ) 
    //( a b -- a b a b ) over over ;
    s.defcode "2DUP" Flags.NONE (fun s ->  // duplicate top two elements of stack
        let b = peek 0
        let a = peek 1
        push a
        push b
        next s
    ) 
    //( d1 d2 — d2 d1 )
    s.defcode "2SWAP" Flags.NONE (fun s ->  // swap top two pairs of elements of stack
        let eax = pop ()
        let ebx = pop ()
        let ecx = pop ()
        let edx = pop ()
        push ebx
        push eax
        push edx
        push ecx
        next s
    ) 

    let apply = s.SP.apply
    let apply2 (f: BaseType->BaseType->BaseType) = s.SP.apply (f <| pop())
    let boolToBase b = if b then -1 else 0
    
    let applyBool f = apply (fun a -> f a |> boolToBase)
    let applyBool2 f = apply2 (fun a b -> f a b |> boolToBase)

    let def applier name f= s.defcode name Flags.NONE (fun s -> 
        applier f
        next s
    ) 

    //( a -- a a | 0 ) dup if dup then 
    //Duplicate x if it is non-zero. 
    s.defcode "?DUP" Flags.NONE (fun s -> // duplicate top of stack if non-zero
        let eax = peek 0
        if eax <> 0 then push eax
        next s
    ) 
    def apply "1+" <| (+) 1
    def apply "1-" <| (-) 1
    def apply "4+" <| (+) 4
    def apply "4-" <| (-) 4
    def apply2 "+" <| (+)
    def apply2 "-" <| (-)
    def apply2 "*" <| (*)
    
    //( n1 n2 -- n3 n4 ) Divide n1 by n2, giving the single-cell remainder n3 and the single-cell quotient n4
    s.defcode "/MOD" Flags.NONE (fun s -> 
        let divisor = pop()
        let dividend = pop()
        let quotient, remainder = Math.DivRem(dividend, divisor)
        push remainder// push remainder
        push quotient// push quotient
        next s
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
    
    let lit () =
        s.W <- s.IP
        inc &s.IP baseSize
        s.memory.get(s.W) |> push
    
    s.defcode "!" Flags.NONE (fun () -> 
        let address = pop()
        let data = pop()
        s.memory.set address data
        next ()
    )
    s.defcode "@" Flags.NONE (fun () -> 
        let address = pop()
        let data = s.memory.get address 
        push data
        next ()
    )
    s.defcode "+!" Flags.NONE (fun () -> 
        let address = pop()
        let amount = pop()
        s.memory.set address (s.memory.get address + amount)
        next ()
    )
    s.defcode "-!" Flags.NONE (fun () -> 
        let address = pop()
        let amount = pop()
        s.memory.set address (s.memory.get address - amount)
        next ()
    )

    //! and @ (STORE and FETCH) store 32-bit words.  It's also useful to be able to read and write bytes
    //so we also define standard words C@ and C!.
    //Byte-oriented operations only work on architectures which permit them (i386 is one of those).

    s.defcode "C!" Flags.NONE (fun () -> 
        let address = pop()
        let data = pop()
        s.memory.setByte address data
        next ()
    )
    s.defcode "C@" Flags.NONE (fun () -> 
        let address = pop()
        let data = s.memory.getByte address 
        push data
        next ()
    )

    // C@C! is a useful byte copy primitive. 
    s.defcode "C@C!" Flags.NONE (fun () -> 
        let destination = pop()
        let source = pop()
        s.memory.setByte destination <| s.memory.getByte source
        push (source + 1)// increment source address
        push (destination + 1)// increment destination address
        next ()
    )

    // and CMOVE is a block copy operation. 
    s.defcode "CMOVE" Flags.NONE (fun () -> 
        let length = pop()
        let destination = pop()
        let source = pop()
        for i in 0..length do
            s.memory.setByte (destination + i) <| s.memory.getByte (source + i)
        next ()
    )
    ()
        
