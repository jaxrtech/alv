module Alv.Program

open System
open FParsec
open FParsecTrace
open Alv
open Alv.CompilerFunctions

let userState = { Debug = { Message = ""; Indent = 0 } }
let path = "Tests\simple.alv"
let encoding = System.Text.Encoding.UTF8

let test parser runner isDebugEnabled =
    match runner parser with
        | Success (result, us, _) ->
            printfn "Success: %A" result
            if isDebugEnabled then
                printfn "Debug:\n\n%s" us.Debug.Message

            Some result

        | Failure (msg, _, us)   ->
            printfn "Failure: %A\n" msg
            if isDebugEnabled then 
                printfn "Debug:\n\n%s" us.Debug.Message

            None

let rec tryRun () =
    let run parser = runParserOnFile parser userState path encoding
    
    let ast = test Parser.parser run true
    
    ast
    |> Option.map compile
    |> Option.map (fun result ->
        result.)

    let command = Console.ReadLine()
    if (command.StartsWith("q")) then ()
    else
        let sep = String.replicate Console.WindowWidth "="
        printfn "\n\n%s%s\n\n" sep sep
        tryRun ()

[<EntryPoint>]
let main argv =
    tryRun()

    0
