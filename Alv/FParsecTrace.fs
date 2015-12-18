// https://gist.github.com/CRogers/6011424

module FParsecTrace

open System.Reflection
open FParsec
open FParsec.Primitives

type DebugInfo = { Message: string; Indent: int }
type UserState = { mutable Debug: DebugInfo }
type P<'t> = Parser<'t, UserState>

type DebugType<'a> = Enter | Leave of Reply<'a>

let addToDebug (stream:CharStream<UserState>) label dtype =
    let msgPadLen = 50

    let startIndent = stream.UserState.Debug.Indent
    let (str, curIndent, nextIndent) =
        match dtype with
        | Enter    -> sprintf "Entering %s" label, startIndent, startIndent+1
        | Leave res ->
            let debugDisplayOpt = 
                // Because it's great when stuff gets marked as `internal`
                let error = res.Error

                let obj = error |> Option.ofObj

                let dynamicMethod =
                    obj |> Option.bind (fun x ->
                        x.GetType().GetMethod(
                            "GetDebuggerDisplay", 
                            BindingFlags.NonPublic ||| BindingFlags.Static)
                        |> Some)
                
                let invoke (x: MethodInfo option) =
                    x |> Option.bind (fun x ->
                            x.Invoke(error, [| error |]) :?> string
                            |> Some)

                invoke dynamicMethod

            let status =
                match res.Status, debugDisplayOpt with
                | (ReplyStatus.Ok as status), _ ->
                      sprintf "(%A)" status
                | status, Some error ->
                      sprintf "(%A)\n%s\n" status error
                | status, None ->
                      sprintf "(%A)" status

            let str = sprintf "Leaving %s %s" label status
            let resStr = sprintf "%s %A" (str.PadRight(msgPadLen-startIndent-1)) res.Result
            resStr, startIndent-1, startIndent-1

    let indentStr =
        if curIndent = 0 then ""
        else "\u251C".PadRight(curIndent, '\u251C')

    let posStr = (sprintf "%A: " stream.Position).PadRight(20)
    let posIdentStr = posStr + indentStr

    // The %A for res.Result makes it go onto multiple lines - pad them out correctly
    let replaceStr = "\n" + "".PadRight(posStr.Length) + "".PadRight(curIndent, '\u2502').PadRight(msgPadLen)
    let correctedStr = str.Replace("\n", replaceStr)

    let fullStr = sprintf "%s %s\n" posIdentStr correctedStr

    stream.UserState.Debug <- {
        Message = stream.UserState.Debug.Message + fullStr
        Indent = nextIndent
    }

let (<!>) (p: P<_>) label :P<_> =
    fun stream ->
        addToDebug stream label Enter
        let reply = p stream
        addToDebug stream label (Leave reply)
        reply

let (<?!>) (p: P<_>) label :P<_> =
    p <?> label <!> label