module UnderstandingComputation.Chap6.ULCParser

open System
open FParsec
open UntypedLambdaCalculus

type Parser<'a> = Parser<'a, unit>

/// Rubyのラムダ式をパース
[<RequireQualifiedAccess>]
module ULCParser =

    let ws = spaces
    let str_ws s = pstring s .>> ws

    let plcexpr, plcexprRef: Parser<LCExpr> * Parser<LCExpr> ref = createParserForwardedToRef()

    /// [a-z]+
    let psymbol = many1 (satisfy isAsciiLower) |>> String.Concat

    let pvar: Parser<LCExpr> = psymbol |>> LCV

    /// -> param { body }
    let pfunc =
        parse {
            do! ws >>. skipString "->" >>. ws
            let! param = psymbol .>> ws
            let! body = between (str_ws "{") (str_ws "}") (ws >>. plcexpr .>> ws)
            let lcf = LCF(param, body)
            return lcf
        }

    /// first[rest]
    let pcall =
        parse {
            let! first = pvar <|> pfunc
            let! rest = between (pchar '[') (pchar ']') plcexpr
            let lcc = LCC(first, rest)
            return lcc
        }

    do plcexprRef := choice
                         [ attempt pcall
                           attempt pvar
                           attempt pfunc ]

    let parse (code: string): LCExpr =
        match run (ws >>. plcexpr .>> ws .>> eof) code with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg
