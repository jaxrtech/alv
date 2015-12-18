module Alv.CompilerFunctions

open System.IO
open Alv.Ast
open Alv.CompilerTypes
open FParsec

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

// TODO: We probably should make the tokens have more information 
//       than just the byte value
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
        [| store; list; X; lparen; _1; add; dim; list; X; newline; |]
        |> Array.iter ctx.TokenStream.WriteByte

        ctx

    let exists (name: string) (ctx: StackTracker) =
        ctx.Stack |> StackContext.exists name

type RootNode = FunctionDecl list

module CompilationContext =
    let create () =
        let stream = new MemoryStream()

        { CompilationContext.TokenStream = stream
          Stack = StackTracker.create stream }

let private eval (f: 'ctx -> 'a -> Reply<'ctx>) (ctx: 'ctx) (nodes: 'a list) =
    let rec eval' = function
    | []    -> Reply(ctx)
    | [x]   -> f ctx x
    | x::xs ->
        let reply = f ctx x
        if reply.Status <> ReplyStatus.Ok then
            reply
        else
            eval' xs

    eval' nodes
    
module Compiler =
    let compile (ast: RootNode) = fun stream ->
        let ctx = CompilationContext.create ()

        // Find main method
        let main = ast |> List.tryFind (fun fn -> fn.Name = "main")

        match main with
        | None ->
            Reply(ctx)

        | Some fn ->
            let apply ctx x: Reply<CompilationContext> =
                let stack = ctx.Stack

                let ok = Reply(ctx)

                let error error = Reply(Error, error)

                let errorMessage msg = error (messageError msg)

                match x with
                | VariableDecl x ->
                    // Check if name already exists
                    if stack |> StackTracker.exists x.Name then
                        let msg = sprintf "%s is already defined" x.Name
                        errorMessage msg
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
            |> eval apply ctx