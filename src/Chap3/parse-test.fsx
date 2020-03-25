#r "../../packages/FParsec/lib/netstandard2.0/FParsecCS.dll"
#r "../../packages/FParsec/lib/netstandard2.0/FParsec.dll"
#r "bin/Release/netstandard2.0/Chap3.dll"

// dllをロードする順番: FParsecCS -> FParsec

open FParsec
open UnderstandingComputation.Chap3.Pattern

type Parser<'a> = Parser<'a, unit>

[<RequireQualifiedAccess>]
module PatternParser =

    // let ppattern, ppatternRef: Parser<Pattern> * Parser<Pattern> ref = createParserForwardedToRef()

    /// Empty
    let pempty: Parser<Pattern> =
        parse {
            do! skipString ""
            return Empty
        }

    /// Literal ['a' - 'z']
    let pliteral: Parser<Pattern> = satisfy isAsciiLower |>> Literal

    let ppattern, ppatternRef: Parser<Pattern> * Parser<Pattern> ref = createParserForwardedToRef()

    let pconcat =
        parse {
            let! pats = many1 ppattern
            let res =
                match pats with
                | [] -> Empty
                | [ p ] -> p
                | p :: ps -> List.fold (fun acc a -> Concat(acc, a)) p ps
            return res
        }

    let pbrackets = between (pstring "(") (pstring ")") pconcat

    let prepeat =
        parse {
            let! res = attempt pbrackets <|> pliteral
            do! skipString "*"
            return Repeat res
        }

    let pchoose =
        let isEmpty =
            function
            | Empty -> true
            | _ -> false
        parse {
            let pat = attempt pbrackets <|> attempt prepeat <|> pliteral
            let! first = attempt (pempty .>> skipString "|") <|> pat
            do! if isEmpty first then skipString "" else skipString "|"
            let! second = ppattern
            return Choose(first, second)
        }

    do ppatternRef := choice
                          [ attempt pchoose
                            attempt prepeat
                            attempt pbrackets
                            pliteral ]

    let pconcatorempty = pconcat <|> pempty

    let parse (input: string) =
        match run (pconcatorempty .>> eof) input with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg

let main() =
    let parse = PatternParser.parse
    let inline put s = printfn "%A" s
    parse "a" |> put
    parse "" |> put
    parse "a*" |> put
    parse "|b" |> put
    parse "a|b" |> put
    parse "a|(b|c)" |> put
    parse "(a|b)" |> put
    parse "(a|b)*" |> put
    parse "(a*|b*)" |> put
    parse "ab" |> put
    parse "(a(|b))*" |> put

main()
