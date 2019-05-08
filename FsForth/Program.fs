// Learn more about F# at http://fsharp.org

open System
open System.Runtime.InteropServices
open Forth

let printState (vm:ForthVM.ForthVM) =
    let wordsList = ForthDebug.getWordsList vm
    let lastStr : String = List.last wordsList |> sprintf "%A"
    //Console.Write("\tLAST WORD:{0}",lastStr.Replace("\n",""))
    if vm.SP.count()>0 then 
        Console.Write("\tSTACK:{0}",vm.SP.ToString())
        //Console.Write("\tLATEST:{0}",vm.LATEST.value)
        //Console.Write("\tBASE:{0}",vm.BASE.value)
        //Console.Write("\tSTATE:{0}",vm.STATE.value)
        //Console.Write("\tHERE:{0}",vm.HERE.value)
        Console.WriteLine()
    ()


let rec private runFn (vm:ForthVM.ForthVM) (code:ForthVM.CodeMemory) (fn : ForthVM.FnPointer)= 
    printState vm
    let fn = code.get fn vm
    runFn vm code fn

let run () =
    let vm = ForthVM.create (65536 * 2) Memory.defaultConfig
    let code = ForthVM.CodeMemory()
    let dictWriter = Words.Writer(vm, code)
    Words.init dictWriter code.DirectPredefinedWords
    vm.IP <- vm.QUIT.address

    let wordsList = ForthDebug.getWordsList(vm)

    runFn vm code code.DirectPredefinedWords.NEXT

[<EntryPoint>]
let main argv =
    run ()
    0 // return an integer exit code
