module Alv.Parser

open FParsec
open FParsecTrace

// TODO: The entire `P<_>` seems a bit like a code smell considering that is coming from
//       `FParsecTrace` and we might want to swap it out later

module private Core =
    let private isAsciiIdStart c =
        isAsciiLetter c || c = '_'

    let private isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_'

    let ident: Parser<string, 'a> =
        identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart,
                                      isAsciiIdContinue = isAsciiIdContinue))

    let name: Parser<string, 'a> = ident

    let alvtype: Parser<Ast.AlvType, 'a> = ident

module private Aliases =
    open Core

    let str = pstring

    let chr = pchar

    let ws_strict: P<_> = many (tab <|> chr ' ') |>> ignore

    let ws = spaces

    let ws1 = spaces1
    
    let str_ws s = str s >>. ws

    let str_ws1 s = str s >>. ws1

    let name_ws = name .>> ws

module private Expression =
    open Core
    open Aliases
    open Ast
    open AstFunctions
    
    module private x =
        let opp = new OperatorPrecedenceParser<Ast.Expression, unit, UserState>()

        let expr = opp.ExpressionParser
        
        module Terms =
            let paren_expr = between (str_ws "(") (str_ws ")") expr  <!> "expr.parens"

            let number = (pfloat .>> ws) |>> Number  <!> "expr.number"

            let reference = (name .>> ws) |>> Reference  <!> "expr.ref"

            module private FunctionCall =
                let reference = name_ws   <!> "expr.fn_call.ref"

                let parameter = expr  <!> "expr.fn_call.param"

                let parameters = between (str_ws "(") (str_ws ")") (sepBy parameter (str_ws ","))  <!> "expr.fn_call.param[]"

                let parser =
                    pipe2 reference parameters
                        (fun ref parameters -> FunctionCall { Reference = ref; Parameters = parameters} )

            let function_call = FunctionCall.parser  <!> "expr.fn_call"

            let all = [number; reference; function_call; paren_expr]

        let term = choice Terms.all  <!> "expr.term"
        opp.TermParser <- term

        type Assoc = Associativity

        let makeInfix op precedence assoc =
            InfixOperator(Operator.toString op, ws, precedence, assoc, fun lhs rhs -> Node.create op lhs rhs)

        opp.AddOperator(makeInfix Add 1 Assoc.Left)
        opp.AddOperator(makeInfix Subtract 1 Assoc.Left)
        opp.AddOperator(makeInfix Multiply 2 Assoc.Left)
        opp.AddOperator(makeInfix Divide 2 Assoc.Left)

    let parser = x.expr  <!> "expr"


module private VariableDecl =
    open Core
    open Aliases
    open Ast

    module private x =
        let keyword =
            let term = (str "val" >>% Immutable) <|> (str "var" >>% Mutable)
            term .>> spaces1  <!> "var_decl.keyword"

        let assignment = opt (str_ws "=" >>. Expression.parser)  <!> "var_decl.assign"

        let typespec = opt (str_ws ":" >>. alvtype)  <!> "var_decl.type"

        let parser =
            pipe4 keyword name_ws typespec assignment
                (fun mutability name alvtype expr ->
                    VariableDecl
                        { VariableDecl.Name = name
                          Mutablility = mutability
                          Type = alvtype
                          Expression = expr })
    
    let parser = x.parser  <!> "var_decl"


module private VariableAssignment =
    open Core
    open Aliases
    open Ast

    module private x =
        let reference = name_ws  <!> "var_assign.ref"

        let parser =
            pipe3 reference (str_ws "=") Expression.parser
                (fun ref _ expr -> VariableAssignment { Reference = ref; Expression = expr })

    let parser = x.parser  <!> "var_assign"


module private Statements =
    open Core
    open Aliases

    let statement = choice [VariableDecl.parser; VariableAssignment.parser] <!> "statement"

    let semicolon = chr ';' |>> ignore  <!> "semicolon"

    let end_of_statement = semicolon .>> ws  <!> "end_of_statement"

    let block = between (str_ws "{") (str_ws "}") (sepEndBy statement (end_of_statement))  <!> "block"


module private FunctionDecl =
    open Core
    open Aliases
    open Ast

    module private x =
        let parameter =
            let colon = spaces .>> str ":" .>> spaces

            pipe3 name colon alvtype
                (fun name _ atype ->
                    { FunctionParameter.Name = name; Type = atype })

        let parameters =
            sepBy parameter (str_ws ",")

        let return_type =
            opt (str_ws "->" >>. alvtype)

        let paramList = between (str_ws "(") (str_ws ")") parameters

        let keyword = str_ws1 "fn"

        let parser =
            pipe4 (keyword >>. name_ws) paramList (ws >>. return_type .>> ws) Statements.block
                (fun name parameters return_type block ->
                    { Name = name
                      Parameters = parameters
                      ReturnType = return_type
                      Block = block })

    let parser = x.parser  <!> "fn_decl"


module private CompilationUnit =
    let parser = (sepEndBy FunctionDecl.parser spaces) .>> eof

let parser = CompilationUnit.parser