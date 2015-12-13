open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let testString = 
    "fn main()->void{\n\
       val x = 10; val y = 20\n\
       val z = (x + y) * 5 + 2\n\
       return z\n\
     }"

module Core =
    let private isAsciiIdStart c =
        isAsciiLetter c || c = '_'

    let private isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_'

    let ident: Parser<string, 'a> =
        identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart,
                                      isAsciiIdContinue = isAsciiIdContinue))

    let str = pstring

module Aliases =
    open Core

    let name: Parser<string, 'a> = ident

    let alvtype: Parser<string, 'a> = ident

module Function =
    open Core
    open Aliases

    // just for indirection if we need it later
    type AlvType = string

    type Parameter = { Name: string; Type: AlvType }

    type Signature = { Name: string; Parameters: Parameter list; ReturnType: AlvType option }

    let parameter =
        let colon = spaces .>> str ":" .>> spaces

        pipe3 name colon alvtype
            (fun name _ atype -> { Name = name; Type = atype})

    let parameters =
        sepBy parameter (str ",")

    let alvreturn =
        opt ((pstring "->" .>> spaces) >>. alvtype)

    let signature =
        let paramList = between (str "(") (str ")") parameters

        pipe3 (name .>> spaces) paramList (spaces >>. alvreturn)
            (fun name parameters alvreturn ->
                { Name = name;
                  Parameters = parameters;
                  ReturnType = alvreturn })

    let keyword = str "fn"

    let header = keyword .>> spaces1 >>. signature

    let block = between (str "{") (str "}") spaces // TODO

    let parser = (header .>> spaces) .>>. block

module CompilationUnit =
    let parser = sepBy Function.parser spaces

[<EntryPoint>]
let main argv =
    test CompilationUnit.parser testString

    System.Console.ReadLine() |> ignore
    0
