﻿module Alv.TokenFunctions

open Alv.Tokens

let fromFloat (f: float) =
    let str = string f

    let validStr = "0123456789."
    let validChars = validStr.ToCharArray()
    
    let x = ``)``
    

    str.ToCharArray()
    |> Array.filter (fun c ->
        validChars |> Array.contains c )

    |> Array.map (function
      (*| '0' -> Some ``0``
        | '1' -> Some ``1``
        | '2' -> Some ``2``
        | '3' -> Some ``3``
        | '4' -> Some ``4``
        | '5' -> Some ``5``
        | '6' -> Some ``6``
        | '7' -> Some ``7``
        | '8' -> Some ``8``
        | '9' -> Some ``9``
        | '.' -> Some period *)
        | _ -> None)

    |> Array.filter Option.isSome
    |> Array.map Option.get

let toString = function
| 0x10 -> "►DMS"
| 0x20 -> "►Dec"
| 0x30 -> "►Frac"
| 0x40 -> "→"
| 0x50 -> "Boxplot"
| 0x60 -> "["
| 0x70 -> "]"
| 0x80 -> "{"
| 0x90 -> "}"
| 0xA0 -> "r"
| 0xB0 -> "°"
| 0xC0 -> "⁻¹"
| 0xD0 -> "²"
| 0xE0 -> "T"
| 0xF0 -> "³"
| 0x01 -> "("
| 0x11 -> ")"
| 0x21 -> "round("
| 0x31 -> "pxl-Test("
| 0x41 -> "augment("
| 0x51 -> "rowSwap("
| 0x61 -> "row+("
| 0x71 -> "*row("
| 0x81 -> "*row+("
| 0x91 -> "max("
| 0xA1 -> "min("
| 0xB1 -> "R►Pr("
| 0xC1 -> "R►Pθ("
| 0xD1 -> "P►Rx("
| 0xE1 -> "P►Ry"
| 0xF1 -> "median("
| 0x02 -> "randM("
| 0x12 -> "mean("
| 0x22 -> "solve("
| 0x32 -> "seq("
| 0x42 -> "fnInt("
| 0x52 -> "nDeriv("
| 0x72 -> "fMin("
| 0x82 -> "fMax("
| 0x92 -> "(space)"
| 0xA2 -> "\""
| 0xB2 -> ","
| 0xC2 -> "i"
| 0xD2 -> "!"
| 0xE2 -> "CubicReg"
| 0xF2 -> "QuartReg"
| 0x03 -> "0"
| 0x13 -> "1"
| 0x23 -> "2"
| 0x33 -> "3"
| 0x43 -> "4"
| 0x53 -> "5"
| 0x63 -> "6"
| 0x73 -> "7"
| 0x83 -> "8"
| 0x93 -> "9"
| 0xA3 -> "."
| 0xB3 -> "E"
| 0xC3 -> "or"
| 0xD3 -> "xor"
| 0xE3 -> ":"
| 0xF3 -> "newline"
| 0x04 -> "and"
| 0x14 -> "A"
| 0x24 -> "B"
| 0x34 -> "C"
| 0x44 -> "D"
| 0x54 -> "E"
| 0x64 -> "F"
| 0x74 -> "G"
| 0x84 -> "H"
| 0x94 -> "I"
| 0xA4 -> "J"
| 0xB4 -> "K"
| 0xC4 -> "L"
| 0xD4 -> "M"
| 0xE4 -> "N"
| 0xF4 -> "O"
| 0x05 -> "P"
| 0x15 -> "Q"
| 0x25 -> "R"
| 0x35 -> "S"
| 0x45 -> "T"
| 0x55 -> "U"
| 0x65 -> "V"
| 0x75 -> "W"
| 0x85 -> "X"
| 0x95 -> "Y"
| 0xA5 -> "Z"
| 0xB5 -> "θ"
| 0xF5 -> "prgm"
| 0x46 -> "Radian"
| 0x56 -> "Degree"
| 0x66 -> "Normal"
| 0x76 -> "Sci"
| 0x86 -> "Eng"
| 0x96 -> "Float"
| 0xA6 -> "="
| 0xB6 -> "<"
| 0xC6 -> ">"
| 0xD6 -> "≤"
| 0xE6 -> "≥"
| 0xF6 -> "≠"
| 0x07 -> "+"
| 0x17 -> "- (sub.)"
| 0x27 -> "Ans"
| 0x37 -> "Fix"
| 0x47 -> "Horiz"
| 0x57 -> "Full"
| 0x67 -> "Func"
| 0x77 -> "Param"
| 0x87 -> "Polar"
| 0x97 -> "Seq"
| 0xA7 -> "IndpntAuto"
| 0xB7 -> "IndpntAsk"
| 0xC7 -> "DependAuto"
| 0xD7 -> "DependAsk"
| 0xF7 -> " mark"
| 0x08 -> " mark"
| 0x18 -> " mark"
| 0x28 -> "*"
| 0x38 -> "/"
| 0x48 -> "Trace"
| 0x58 -> "ClrDraw"
| 0x68 -> "ZStandard"
| 0x78 -> "ZTrig"
| 0x88 -> "ZBox"
| 0x98 -> "Zoom In"
| 0xA8 -> "Zoom Out"
| 0xB8 -> "ZSquare"
| 0xC8 -> "ZInteger"
| 0xD8 -> "ZPrevious"
| 0xE8 -> "ZDecimal"
| 0xF8 -> "ZoomStat"
| 0x09 -> "ZoomRcl"
| 0x19 -> "PrintScreen"
| 0x29 -> "ZoomSto"
| 0x39 -> "Text("
| 0x49 -> "nPr"
| 0x59 -> "nCr"
| 0x69 -> "FnOn"
| 0x79 -> "FnOff"
| 0x89 -> "StorePic"
| 0x99 -> "RecallPic"
| 0xA9 -> "StoreGDB"
| 0xB9 -> "RecallGDB"
| 0xC9 -> "Line("
| 0xD9 -> "Vertical"
| 0xE9 -> "Pt-On("
| 0xF9 -> "Pt-Off("
| 0x0A -> "Pt-Change("
| 0x1A -> "Pxl-On("
| 0x2A -> "Pxl-Off("
| 0x3A -> "Pxl-Change("
| 0x4A -> "Shade("
| 0x5A -> "Circle("
| 0x6A -> "Horizontal"
| 0x7A -> "Tangent("
| 0x8A -> "DrawInv"
| 0x9A -> "DrawF"
| 0xBA -> "rand"
| 0xCA -> "π"
| 0xDA -> "getKey"
| 0xEA -> "'"
| 0xFA -> "?"
| 0x0B -> "- (neg.)"
| 0x1B -> "int("
| 0x2B -> "abs("
| 0x3B -> "det("
| 0x4B -> "identity("
| 0x5B -> "dim("
| 0x6B -> "sum("
| 0x7B -> "prod("
| 0x8B -> "not("
| 0x9B -> "iPart("
| 0xAB -> "fPart("
| 0xCB -> "√("
| 0xDB -> "³√("
| 0xEB -> "ln("
| 0xFB -> "e^("
| 0x0C -> "log("
| 0x1C -> "10^("
| 0x2C -> "sin("
| 0x3C -> "sinֿ¹("
| 0x4C -> "cos("
| 0x5C -> "cosֿ¹("
| 0x6C -> "tan("
| 0x7C -> "tanֿ¹("
| 0x8C -> "sinh("
| 0x9C -> "sinhֿ¹("
| 0xAC -> "cosh("
| 0xBC -> "coshֿ¹("
| 0xCC -> "tanh("
| 0xDC -> "tanhֿ¹("
| 0xEC -> "If"
| 0xFC -> "Then"
| 0x0D -> "Else"
| 0x1D -> "While"
| 0x2D -> "Repeat"
| 0x3D -> "For("
| 0x4D -> "End"
| 0x5D -> "Return"
| 0x6D -> "Lbl"
| 0x7D -> "Goto"
| 0x8D -> "Pause"
| 0x9D -> "Stop"
| 0xAD -> "IS>("
| 0xBD -> "DS<("
| 0xCD -> "Input"
| 0xDD -> "Prompt"
| 0xED -> "Disp"
| 0xFD -> "DispGraph"
| 0x0E -> "Output("
| 0x1E -> "ClrHome"
| 0x2E -> "Fill("
| 0x3E -> "SortA("
| 0x4E -> "SortD("
| 0x5E -> "DispTable"
| 0x6E -> "Menu("
| 0x7E -> "Send("
| 0x8E -> "Get("
| 0x9E -> "PlotsOn"
| 0xAE -> "PlotsOff"
| 0xBE -> "∟"
| 0xCE -> "Plot1("
| 0xDE -> "Plot2("
| 0xEE -> "Plot3("
| 0x0F -> "^"
| 0x1F -> "×√"
| 0x2F -> "1-Var Stats"
| 0x3F -> "2-Var Stats"
| 0x4F -> "LinReg(a+bx)"
| 0x5F -> "ExpReg"
| 0x6F -> "LnReg"
| 0x7F -> "PwrReg"
| 0x8F -> "Med-Med"
| 0x9F -> "QuadReg"
| 0xAF -> "ClrList"
| 0xBF -> "ClrTable"
| 0xCF -> "Histogram"
| 0xDF -> "xyLine"
| 0xEF -> "Scatter"
| 0xFF -> "LinReg(ax+b)"
