// Learn more about F# at http://fsharp.org

open System
open System.Runtime.InteropServices

[<EntryPoint>]
let main argv =
    let memory = Array.create (1<<<16) 0
    let f = Forth.Forth(memory)
    f.init()
    f.coldStart ()
    0 // return an integer exit code
