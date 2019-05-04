// Learn more about F# at http://fsharp.org

open System
open System.Runtime.InteropServices
open Forth

let rec private runFn (vm:ForthVM.ForthVM) (code:ForthVM.CodeMemory) (fn : ForthVM.FnPointer)= 
    let fn = code.get fn vm
    runFn vm code fn

let run () =
    let vm = ForthVM.create (65536 * 2) Memory.defaultConfig
    let code = ForthVM.CodeMemory()
    let dictWriter = Words.Writer(vm, code)
    let coldStartCodeword = Words.init dictWriter code.DirectPredefinedWords
    vm.IP <- coldStartCodeword

    let wordsList = ForthDebug.getWordsList vm

    runFn vm code code.DirectPredefinedWords.NEXT

[<EntryPoint>]
let main argv =
    run ()
    0 // return an integer exit code
