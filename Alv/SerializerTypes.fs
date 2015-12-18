module Alv.SerializerTypes

open System.IO

type Writable =
    | UInt8 of byte
    | UInt16 of uint16
    | Buffer of byte[]
    | Stream of Stream
    | StringBuffer of string * int

type ProgramWriter = {
    Name: string
    TokenStream: Stream
    OutputStream: Stream
}