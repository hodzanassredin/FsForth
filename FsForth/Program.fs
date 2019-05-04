// Learn more about F# at http://fsharp.org

open System
open System.Runtime.InteropServices

[<EntryPoint>]
let main argv =
    Forth.Forth.run()
    0 // return an integer exit code
