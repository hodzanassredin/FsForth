module ForthDebug

open Forth
open System.Text

type Word = {
    link : int
    length : int
    isHidden : bool
    isImmediate : bool
    name:string
    cfa : int
    cfaAddress : int
    address : int
    payload : int[]
    program : string
}

let rec findName (vm:ForthVM.ForthVM) words cfa = 
    if cfa = vm.EXIT.address then "QUIT"
    elif cfa = vm.DOCOL.address then "DOCOL"
    elif cfa = vm.NEXT.address then "DOCOL"
    else match words with
        | [] -> cfa.ToString()
        | h::_ when h.cfaAddress = cfa -> sprintf "%s(%d)" h.name cfa
        | _::t -> findName vm t cfa

let translateProgram (vm:ForthVM.ForthVM) words word = 
    if word.cfa = 1 // is DOCOL
    then {word with program = word.payload |> Array.map (findName vm words) |> String.concat " "}
    else word

let rec private getWords (vm:ForthVM.ForthVM) prevLink wordAddr words = 
    if wordAddr = 0
    then words |> List.map (translateProgram vm words)
    else 
        let link = Memory.getInt vm.memory wordAddr
        let flagsLen = vm.memory.[wordAddr + Memory.baseSize] |> int
        let length = Words.LENMASK &&& flagsLen
        let isHidden = int Words.Flags.HIDDEN &&& flagsLen <> 0
        let isImmediate = int Words.Flags.IMMEDIATE &&& flagsLen <> 0
        let str = Array.zeroCreate length
        Memory.copyToBytes vm.memory (wordAddr + Memory.baseSize + 1) str
        let name = Encoding.ASCII.GetString(str)
        let cfaAddr = wordAddr + Memory.baseSize + 1 + length |> Memory.pad
        let cfa = Memory.getInt vm.memory cfaAddr

        let payloadStart = cfaAddr + Memory.baseSize
        let payloadEnd = prevLink - Memory.baseSize
        let payload = [payloadStart..Memory.baseSize..payloadEnd] 
                      |> Seq.ofList
                      |> Seq.map (Memory.getInt vm.memory)
                      
        
        let current = {
            payload = payload |> Seq.toArray
            program = null
            address = wordAddr
            cfaAddress = cfaAddr
            link = link
            length = length
            isHidden = isHidden
            isImmediate = isImmediate
            name = name
            cfa = cfa
        }
        getWords (vm:ForthVM.ForthVM) wordAddr link (current::words)

let getWordsList (vm:ForthVM.ForthVM) = getWords vm vm.HERE.value vm.LATEST.value []
