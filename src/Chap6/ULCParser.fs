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

    let plcterm, plctermRef: Parser<LCTerm> * Parser<LCTerm> ref = createParserForwardedToRef()

    /// [a-z]+
    let psymbol = many1 (satisfy isAsciiLower) |>> String.Concat

    let pvar: Parser<LCTerm> = psymbol |>> LCV

    let pfunc =
        parse {
            do! ws >>. skipString "->" >>. ws
            let! param = psymbol .>> ws
            let! body = between (str_ws "{") (str_ws "}") (ws >>. plcterm .>> ws)
            let lcf = LCF(param, body)
            return lcf
        }

    let pcall =
        parse {
            let! first = pvar <|> pfunc
            let! rest = between (pchar '[') (pchar ']') plcterm
            let lcc = LCC(first, rest)
            return lcc
        }

    do plctermRef := choice
                         [ attempt pcall
                           attempt pvar
                           attempt pfunc ]

    let parse (code: string): LCTerm =
        match run (ws >>. plcterm .>> ws .>> eof) code with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg
