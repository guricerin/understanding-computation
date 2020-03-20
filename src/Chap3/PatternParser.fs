module UnderstandingComputation.Chap3.PatternParser

open FParsec
open Pattern

type Parser<'a> = Parser<'a, unit>

[<RequireQualifiedAccess>]
module PatternParser =

    let ws = spaces
    let strWS s = pstring s .>> ws

    let ppattern, ppatternRef: Parser<Pattern> * Parser<Pattern> ref = createParserForwardedToRef()

    /// Empty
    let pempty: Parser<Pattern> =
        parse {
            do! skipString ""
            return Empty
        }

    /// Literal ['a' - 'z']
    let pliteral: Parser<Pattern> = satisfy isAsciiLower |>> Literal

    let pconcat =
        parse {
            let! first = ppattern
            let! second = ppattern
            let res = Concat(first, second)
            return res
        }

    /// 演算子の優先順位
    let opp = OperatorPrecedenceParser<Pattern, unit, unit>()

    let poperator = opp.ExpressionParser
    let term = pliteral <|> between (pstring "(") (pstring ")") poperator

    opp.TermParser <- term
    opp.AddOperator(InfixOperator("|", ws, 1, Associativity.Left, (fun x y -> Choose(x, y))))
    opp.AddOperator(PostfixOperator("*", ws, 2, true, (fun x -> Repeat(x))))

    let parse (input: string) =
        match run (ppattern .>> eof) input with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg
