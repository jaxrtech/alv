module Alv.SerializerFunctions

module Writable =
    open System
    open System.IO
    open System.Text
    open Alv.SerializerTypes

    let rec write (writer: BinaryWriter) (x: Writable) =
        let makeStringBuffer (str: string) (maxLength: int) =
            let encoded = Encoding.ASCII.GetBytes(str)

            if encoded.Length > maxLength then
                failwithf "string buffer length cannot be greater that %d bytes" maxLength
            else

            let buffer = Array.zeroCreate<byte> maxLength
            Array.Copy(encoded, buffer, encoded.Length)
            
            Buffer buffer

        printfn "write: %A" x

        match x with
        | UInt8 x  -> x |> writer.Write

        | UInt16 x -> x |> writer.Write

        | Buffer x -> x |> writer.Write

        | Stream x ->
            x.Position <- 0L
            x.CopyTo(writer.BaseStream)

        | StringBuffer (str, len) ->
            makeStringBuffer str len
            |> write writer

module ProgramWriter =
    open System
    open System.IO
    open Alv.SerializerTypes

    let private applySerialize f x =
        use stream = new MemoryStream()
        use writer = new BinaryWriter(stream)
            
        x
        |> f writer
        |> ignore
            
        stream.ToArray()
    
    let private serialize = applySerialize Writable.write

    let private serializeList = applySerialize (fun writer -> List.map (Writable.write writer))

    //

    let private program (ctx: ProgramWriter) =
        let length = uint16 ctx.TokenStream.Length |> UInt16
        let stream = ctx.TokenStream |> Stream |> serialize |> Buffer

        [length; stream]

    let private variable (ctx: ProgramWriter) =
        let child = program ctx |> serializeList

        //

        let signature = 11us |> UInt16
        
        let length = uint16 (child.Length) |> UInt16

        let typeId = 5uy |> UInt8

        let name = StringBuffer (ctx.Name.ToUpper(), 8)

        let version = 6uy |> UInt8

        let flag = 0uy |> UInt8

        let length_dup = length

        let innerBuffer = Buffer child

        //

        [signature
         length
         typeId
         name
         version
         flag
         length_dup
         innerBuffer]

    let private root (ctx: ProgramWriter) =
        let child = variable ctx |> serializeList

        //

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

        let length = uint16 (child.Length) |> UInt16

        let innerBuffer = Buffer child

        let checksum = child |> Array.sumBy (uint64) |> uint16 |> UInt16

        //

        [signature
         signatureExt
         comment
         length
         innerBuffer
         checksum]

    let write (ctx: ProgramWriter) =
        use writer = new System.IO.BinaryWriter(ctx.OutputStream)

        root ctx
        |> List.map (Writable.write writer)
        |> ignore

        writer.Close()