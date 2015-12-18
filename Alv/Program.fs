module Alv.Program

open System
open System.IO
open System.Text
open FParsec
open FParsecTrace
open Alv
open Alv.CompilerFunctions
open Alv.SerializerTypes
open Alv.SerializerFunctions

let userState = { Debug = { Message = ""; Indent = 0 } }
let path = "Tests\simple.alv"
let encoding = System.Text.Encoding.UTF8
let isDebugEnabled = true

let printParserResult (isDebugEnabled: bool) (result: ParserResult<'a, UserState>) =
    match result with
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

module CharStreamExt =
    open System.IO
    open System.Text

    let createFromStream (userState: 'UserState) (streamName: string) (byteStream: Stream) (encoding: Encoding) =
        let stream = new CharStream<'UserState>(byteStream, encoding)
        stream.UserState <- userState
        stream.Name <- streamName
        stream

    let createFromFile (userState: 'UserState) (path: string) (encoding: System.Text.Encoding) =
        let stream = new CharStream<'UserState>(path, encoding)
        stream.UserState <- userState
        stream

let applyParser (stream: CharStream<'UserState>) (parser: Parser<'Result,'UserState>)  =
    let reply = parser stream
    if reply.Status = Ok then
        Success(reply.Result, stream.UserState, stream.Position)
    else
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        Failure(error.ToString(stream), error, stream.UserState)

let rec tryRun () =
    let stream = CharStreamExt.createFromFile userState path encoding
    
    Parser.parser >>= Compiler.compile
    |> applyParser stream
    |> printParserResult isDebugEnabled 
    |> Option.map (fun compilation ->
        let name = Path.GetFileNameWithoutExtension(path)
        let ext = "8xp"
        let directory = Path.GetDirectoryName(path)

        let outputPath = Path.Combine(directory, name + "." + ext)
        use outputStream = File.Create(outputPath)
        
        let ctx =
            { Name = name
              TokenStream = compilation.TokenStream
              OutputStream = outputStream }

        ProgramWriter.write ctx
        outputStream.Close()

        printfn "\nOutput: %s\n\n%s" outputPath)
    |> ignore

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
