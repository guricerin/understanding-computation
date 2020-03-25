module UnderstandingComputation.Chap3.PatternParser

open FParsec
open Pattern

type Parser<'a> = Parser<'a, unit>

[<RequireQualifiedAccess>]
module PatternParser =

    let pempty: Parser<Pattern> = parse { return Empty }

    /// ['a' - 'z']
    let pliteral: Parser<Pattern> = satisfy isAsciiLower |>> Literal

    let ppattern, ppatternRef: Parser<Pattern> * Parser<Pattern> ref = createParserForwardedToRef()

    let pconcatorempty =
        parse {
            let! pats = many1 ppattern
            let res =
                match pats with
                | [] -> Empty
                | [ p ] -> p
                | p :: ps -> List.fold (fun acc a -> Concat(acc, a)) p ps
            return res
        }

    let pbrackets = between (pstring "(") (pstring ")") pconcatorempty

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

    let parse (input: string) =
        match run (pconcatorempty .>> eof) input with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg
