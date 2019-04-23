﻿
module Forth 

open System
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text

type BaseType = Int32
type FnPointer = BaseType
let baseSize = sizeof<BaseType>

let inc (v:BaseType byref) count = v <- v + count
let dec (v:BaseType byref) count = v <- v - count

type Memory(memory : BaseType[]) =
    let mutable memoryTop = 0
    member x.HERE = memoryTop

    member x.get offset = memory.[offset/baseSize]

    member x.set offset v = memory.[offset/baseSize] <- v 

    member x.reserveMemoryBytes size = 
        let free = memoryTop
        memoryTop <- memoryTop + size
        free
    member x.reserveMemory size = x.reserveMemoryBytes size * baseSize

    member x.WriteByte (v:byte) = 
        let address = x.reserveMemoryBytes 1
        x.setByte address <| int v

    member x.Write (v:BaseType) = 
        let address = x.reserveMemory baseSize
        x.setByte address <| v
        
    member x.WriteString (str:String) = Encoding.ASCII.GetBytes str |> Array.iter x.WriteByte 

    member x.Align () = memoryTop <- memoryTop + memoryTop % baseSize 

    member x.getByte offset : BaseType = 
        let memoryBytes = MemoryMarshal.Cast<BaseType,byte>(new Span<BaseType>(memory))
        int memoryBytes.[offset]
    member x.setByte offset (v:BaseType) = 
        let memoryBytes = MemoryMarshal.Cast<BaseType,byte>(new Span<BaseType>(memory))
        memoryBytes.[offset] <- byte v 


type Variable(memory:Memory, initial: BaseType)=
    let addr = memory.reserveMemory 1
    memory.set addr initial
    member x.address = addr
    member x.get () = memory.get addr
    member x.set v = memory.set addr v

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



type ForthVM(memory : Memory) =
    //memory for main forth variables
    member val STATE = Variable(memory, 0)//Is the interpreter executing code (0) or compiling a word (non-zero)?
    member val LATEST = Variable(memory, 0)//Points to the latest (most recently defined) word in the dictionary.
    member val HERE = Variable(memory, 0)//Points to the next free byte of memory.  When compiling, compiled words go here.
    member val S0 = Variable(memory, 0)//Stores the address of the top of the parameter stack.
    member val BASE = Variable(memory, 10)//The current base for printing and reading numbers.
    
    member val SP = Stack(memory, 256)//data stack
    member val RSP = Stack(memory, 256)//return stack
    [<DefaultValue>] val mutable IP : BaseType//istruction pointer in bytes
    [<DefaultValue>] val mutable W : BaseType//work
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

type Forth(memory : BaseType[]) =
    let memory = Memory(memory)
    let code = CodeMemory()
    let vm = ForthVM(memory)

    //member x.Run (fn:Fn) =  
    //    let nextFn = fn vm
    //    x.Run <| code.get nextFn
    
    //member x.AddToDictionary (v:BaseType) = 
    //    let pointer = vm.HERE.get ()
    //    memory.set pointer v
    //    vm.HERE.set (pointer + 1)
    
    //add forth word

    let createHeader (name: string) (flags:Flags) (codeWord: BaseType) (data:BaseType[])=
        let link = vm.LATEST.get()
        let nameSize = byte name.Length
        assert (nameSize <= 31uy)
        let flags = byte flags
        vm.LATEST.set vm.memory.HERE//set link
        memory.Write(link)
        nameSize + flags |> int |> memory.Write
        memory.WriteString name
        memory.Align()
        memory.Write codeWord //codeword
        for d in data do
            memory.Write d 
        vm.HERE.set vm.memory.HERE//refresh pointer

    member x.defword (name: string) (flags:Flags) (program:BaseType[]) = 
        createHeader name flags code.NEST program

    //add native word
    member x.defcode (name: string) (flags:Flags) (f:Fn) = 
        let fAddr = code.addFunction f
        createHeader name flags fAddr [||]

    //add variable
    member x.defvar (name: string) (flags:Flags) (varAddress:BaseType) (initial:BaseType option) = 
        match initial with  
            | Some(initial) -> vm.memory.set varAddress initial
            | _ -> ()
        x.defcode name flags (fun vm -> 
            vm.SP.push varAddress
            code.NEXT
        )
    member x.defconst (name: string) (flags:Flags) (value:BaseType) = 
        x.defcode name flags (fun vm -> 
            vm.SP.push value
            code.NEXT
        )

    member x.init () = 

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
        let apply2 (vm:ForthVM) (f: BaseType->BaseType->BaseType)  = vm.SP.apply (f <| vm.SP.pop())
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
    
        x.defcode "LIT" Flags.NONE (fun vm ->
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
        x.defcode "@" Flags.NONE (fun vm -> 
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
        x.defvar "LATEST" Flags.NONE vm.LATEST.address Option.None //name_SYSCALL0 // SYSCALL0 must be last in built-in dictionary
        x.defvar "S0" Flags.NONE  vm.S0.address Option.None
        x.defvar "BASE" Flags.NONE vm.BASE.address Option.None
        
        x.defconst "VERSION",7,,VERSION,JONES_VERSION
        x.defconst "R0",2,,RZ,return_stack_top
        x.defconst "DOCOL",5,,__DOCOL,DOCOL
        x.defconst "F_IMMED",7,,__F_IMMED,F_IMMED
        x.defconst "F_HIDDEN",8,,__F_HIDDEN,F_HIDDEN
        x.defconst "F_LENMASK",9,,__F_LENMASK,F_LENMASK
        
        x.defconst "SYS_EXIT",8,,SYS_EXIT,__NR_exit
        x.defconst "SYS_OPEN",8,,SYS_OPEN,__NR_open
        x.defconst "SYS_CLOSE",9,,SYS_CLOSE,__NR_close
        x.defconst "SYS_READ",8,,SYS_READ,__NR_read
        x.defconst "SYS_WRITE",9,,SYS_WRITE,__NR_write
        x.defconst "SYS_CREAT",9,,SYS_CREAT,__NR_creat
        x.defconst "SYS_BRK",7,,SYS_BRK,__NR_brk
        
        x.defconst "O_RDONLY",8,,__O_RDONLY,0
        x.defconst "O_WRONLY",8,,__O_WRONLY,1
        x.defconst "O_RDWR",6,,__O_RDWR,2
        x.defconst "O_CREAT",7,,__O_CREAT,0100
        x.defconst "O_EXCL",6,,__O_EXCL,0200
        x.defconst "O_TRUNC",7,,__O_TRUNC,01000
        x.defconst "O_APPEND",8,,__O_APPEND,02000
        x.defconst "O_NONBLOCK",10,,__O_NONBLOCK,04000
