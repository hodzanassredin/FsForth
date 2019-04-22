// Learn more about F# at http://fsharp.org

open System
open System.Runtime.InteropServices

[<EntryPoint>]
let main argv =
    let memory = Forth.Memory(Array.create (1<<<16) 0)
    let state = Forth.ForthState(memory)
    state.Run (fun fn -> 0)
    0 // return an integer exit code
