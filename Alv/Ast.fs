module Alv.Ast

// just for indirection if we need it later
type AlvType = string

type Mutablility =
     | Immutable
     | Mutable

type Operator =
     | Add
     | Subtract
     | Multiply
     | Divide

type Expression =
     | Number of float
     | Reference of string
     | FunctionCall of FunctionCall
     | Node of Node

and Node = {
    Operator: Operator
    Left: Expression
    Right: Expression
}

and FunctionCall = {
    Reference: string
    Parameters: Expression list
}

type VariableDecl = {
    Name: string
    Mutablility: Mutablility
    Type: AlvType option
    Expression: Expression option
}

type VariableAssignment = {
    Reference: string
    Expression: Expression
}

type Statement =
     | VariableDecl of VariableDecl
     | VariableAssignment of VariableAssignment

type Block = Statement list

type FunctionParameter = {
    Name: string
    Type: AlvType
}

type FunctionDecl = {
    Name: string
    Parameters: FunctionParameter list
    ReturnType: AlvType option
    Block: Block
}