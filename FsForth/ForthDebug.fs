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
}

let rec private getWords (vm:ForthVM.ForthVM) wordAddr words = 
    if wordAddr = 0
    then words
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
        let current = {
            address = wordAddr
            cfaAddress = cfaAddr
            link = link
            length = length
            isHidden = isHidden
            isImmediate = isImmediate
            name = name
            cfa = cfa
        }
        getWords (vm:ForthVM.ForthVM) link (current::words)

let getWordsList (vm:ForthVM.ForthVM) = getWords vm vm.LATEST.value []
