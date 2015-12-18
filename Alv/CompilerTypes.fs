module Alv.CompilerTypes

open System.IO
open System.Collections.Generic
open FParsec

type StackPosition = int32

type StackValue = float

type StackContext = {
    Stack: List<StackValue>
    Variables: Dictionary<string, StackPosition>
}

type StackTracker = {
    TokenStream: Stream
    Stack: StackContext
    Length: int
}

type CompilationContext = {
    Status: ParserResult<Stream, unit>
    TokenStream: Stream
    Stack: StackTracker
}

type ProgramWriter = {
    Name: string
    TokenStream: Stream
    OutputStream: Stream
}