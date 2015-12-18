module Alv.CompilerFunctions

open System.IO
open Ast
open CompilerTypes

module StackContext =
    open System.Collections.Generic

    type FreeResult =
         | Success
         | DoesNotExist

    let create =
        { Stack = List()
          Variables = Dictionary() }

    let push (name: string) (value: StackValue) (ctx: StackContext) =
        ctx.Stack.Add(value)
        
        let pos = ctx.Stack.Count - 1
        ctx.Variables.Add(name, pos)

        pos

    let pop (name: string) (ctx: StackContext) =
        if ctx.Variables.Remove(name) then Success
        else DoesNotExist

    let exists (name: string) (ctx: StackContext) =
        ctx.Variables.ContainsKey(name)

    let length (ctx: StackContext) =
        ctx.Stack.Count

module NumberWriter =
    open Alv.Tokens

    let private map =
        Map.ofSeq
            [('0', _0)
             ('1', _1)
             ('2', _2)
             ('3', _3)
             ('4', _4)
             ('5', _5)
             ('6', _6)
             ('7', _7)
             ('8', _8)
             ('9', _9)
             ('.', dot)]

    let write x (stream: Stream) =
        let str = sprintf "%f" x

        str.ToCharArray() |> Array.iter (fun c ->
            match map.TryFind c with
            | None -> failwithf "Invalid character '%c'" c
            | Some token -> stream.WriteByte(token))

module StackTracker =
    open Alv.Tokens

    let create stream =
        { TokenStream = stream
          Stack = StackContext.create
          Length = 0 }

    let push (name: string) (value: StackValue) (ctx: StackTracker) =
        let pos = ctx.Stack |> StackContext.push name value
        let length' = ctx.Stack |> StackContext.length

        // (value)
        ctx.TokenStream |> NumberWriter.write value
            
        // →∟X(1+dim(∟X
        [| store list X lparen _1 add dim list X |]
        |> Array.iter ctx.TokenStream.WriteByte

        ctx

    let exists (name: string) (ctx: StackTracker) =
        ctx.Stack |> StackContext.exists name

type RootNode = FunctionDecl list

module CompilationContext =
    let create () =
        let stream = new MemoryStream()

        { Status = Success
          TokenStream = stream
          Stack = StackTracker.create stream }

module ProgramWriter =
    open System
    open System.Text

    type private Writable =
        | UInt8 of byte
        | UInt16 of uint16
        | Buffer of byte[]
        | Stream of Stream
        | StringBuffer of string * int
        | Seq of Writable list

    let rec private write (writer: BinaryWriter) (x: Writable) =
        let makeStringBuffer (str: string) (maxLength: int) =
            let encoded = Encoding.ASCII.GetBytes(str)

            if encoded.Length > maxLength then
                failwithf "string buffer length cannot be greater that %d bytes" maxLength
            else

            Array.Resize(ref encoded, maxLength)
            encoded |> Buffer

        match x with
        | UInt8 x  -> x |> writer.Write

        | UInt16 x -> x |> writer.Write

        | Buffer x -> x |> writer.Write

        | Stream x -> x.CopyTo(writer.BaseStream)

        | StringBuffer (str, len) ->
            makeStringBuffer str len
            |> write writer

        | Seq xs ->
            xs |> List.iter (write writer)

    module private Packet =
        
        let private header (ctx: ProgramWriter) =
            let signature = StringBuffer ("**TI83F*", 8)
    
            let signatureExt = [| 0x1Auy; 0x0Auy; 0x0Auy |] |> Buffer
        
            let comment = 
                let version =
                    System.Reflection.Assembly
                        .GetExecutingAssembly()
                        .GetName()
                        .Version
                        .ToString()

                let maxLength = 42
                let str = sprintf "Alv Compiler v%s" version
                StringBuffer (str, maxLength)

            //

            [signature
             signatureExt
             comment]
            |> Seq

        let private variable (ctx: ProgramWriter) =
            let signature = 11uy |> UInt8
        
            // +2 for the length in the program node
            let length = uint16 (ctx.TokenStream.Length + 2L) |> UInt16

            let typeId = 5uy |> UInt8

            let name = StringBuffer (ctx.Name.ToUpper(), 8)

            let version = 6uy |> UInt8

            let flag = 0uy |> UInt8

            let length_dup = length

            let program (ctx: ProgramWriter) =
                let length = uint16 ctx.TokenStream.Length |> UInt16

                let stream = ctx.TokenStream |> Stream

                [length; stream]

            //

            List.append
                ([signature
                  length
                  typeId
                  name
                  version
                  flag
                  length_dup])

                (program ctx)
            |> Seq

        let root (ctx: ProgramWriter) =
            [header
             variable]
            |> List.map (fun f -> f ctx)
            |> Seq

    let write (ctx: ProgramWriter) =
        use writer = new BinaryWriter(ctx.OutputStream)
        
        Packet.root ctx
        |> write writer

let compile (ast: RootNode) =
    let ctx = CompilationContext.create ()

    // Find main method
    let main = ast |> List.tryFind (fun fn -> fn.Name = "main")

    match main with
    | None -> ()
    | Some fn ->
        let apply x (ctx: CompilationContext) =
            let stack = ctx.Stack

            let ok = ctx

            let fail errors =
                { ctx with Status = FParsec.CharParsers.ParserResult( }

            match x with
            | VariableDecl x ->
                // Check if name already exists
                if stack |> StackTracker.exists x.Name then
                    let msg = sprintf "%s is already defined" x.Name
                    fail (FParsec.ErrorMessageList(FParsec.ErrorMessage.Message(msg)))
                else

                match x.Expression with
                | None ->
                    stack |> StackTracker.push x.Name 0.0 |> ignore
                    ok

                | Some (Number a) ->
                    stack |> StackTracker.push x.Name a |> ignore
                    ok
                
                | _ -> raise (System.NotImplementedException())

            | VariableAssignment x ->
                ok

        fn.Block
        |> List.fold
            (fun ctx node -> apply node ctx)
            (ctx)

        |> ignore

    ctx.Status