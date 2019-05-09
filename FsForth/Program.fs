open Hardware
open Forth
open CodeMemory


let run () =
    let vm = ForthVM.create (65536 * 2) Memory.defaultConfig
    let dictWriter = Forth.Writer(vm)
    Forth.init dictWriter 
    ForthVM.run vm dictWriter.PredefinedWords.NEXT

[<EntryPoint>]
let main argv =
    run ()
    0 // return an integer exit code
