// Learn more about F# at http://fsharp.org

open System
open System.Runtime.InteropServices

[<EntryPoint>]
let main argv =
    
    

    let memory = new Span<byte>(Array.create (1<<<16) 0uy)
    let state = Forth.ForthState(memory)

    0 // return an integer exit code
