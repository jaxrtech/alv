module Alv.AstFunctions

open Alv.Ast

module Operator =
    let toString = function
        | Add -> "+"
        | Subtract -> "-"
        | Multiply -> "*"
        | Divide -> "/"

module Node =
    let create op lhs rhs = Node { Operator = op; Left = lhs; Right = rhs }

