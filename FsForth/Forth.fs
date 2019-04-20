
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
let incReg (reg: int byref) = reg <- reg + baseSize
let decReg (reg: int byref) = reg <- reg - baseSize
type ForthState(memory : byte[], spBase : BaseType, rspBase : BaseType) =
    [<DefaultValue>] val mutable IP : BaseType//istruction pointer in bytes
    [<DefaultValue>] val mutable W : BaseType//work
    [<DefaultValue>] val mutable SP : BaseType//data stack pointer
    [<DefaultValue>] val mutable RSP : BaseType//return data stack pointer
    member x.getMemory offset = 
        let memoryInts = MemoryMarshal.Cast<byte,BaseType>(new Span<byte>(memory))
        memoryInts.[offset]
    member x.setMemory offset v = 
        let memoryInts = MemoryMarshal.Cast<byte,BaseType>(new Span<byte>(memory))
        memoryInts.[offset] <- v 
    member x.pushSP v = 
        incReg(&x.SP)
        x.setMemory (x.SP + spBase) v
    member x.popSP () = 
        let v = x.getMemory (x.SP + spBase)
        decReg(&x.SP)
    member x.pushRSP v = 
        incReg(&x.RSP)
        x.setMemory (x.RSP + rspBase) v
    member x.popRSP () = 
        let v = x.getMemory (x.RSP + rspBase)
        decReg(&x.RSP)
        
and Fn = ForthState -> FnPointer
//VM funcs



//forthFuncs
let nativeFuncs : Fn[] = Array.zeroCreate (1<<<8) 

let rec run state (fn:Fn) = 
    let nextFn = fn state
    run state <| nativeFuncs.[nextFn]
    
let mutable nextFnPointer = 0
let addFunction f = 
    nativeFuncs.[nextFnPointer] <- f
    nextFnPointer <- nextFnPointer + 1
    nextFnPointer - 1

let nextFn (state:ForthState) : FnPointer =   
    state.W <- state.IP
    incReg(&state.IP)
    state.getMemory state.W

let next = addFunction nextFn
            
let addNativeFunctionWithNext f =
    f()
    next

 

    


    

