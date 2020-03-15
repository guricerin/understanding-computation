namespace UnderstandingComputation.Chap2

open FParsec

// 本と同じく最小限の実装

type Parser<'a> = Parser<'a, unit>

[<RequireQualifiedAccess>]
module Parsing =

    let ws = spaces
    let strWS s = pstring s .>> ws

    let numberFormat = NumberLiteralOptions.AllowMinusSign

    let pnumliteral: Parser<Expr> =
        parse {
            let num = pint32
            let! num = num |>> Number
            return num
        }

    let pboolliteral: Parser<Expr> =
        parse {
            let b = pstring "true" <|> pstring "false"
            let! b = b |>> fun x ->
                         if x = "true" then Boolean true else Boolean false
            return b
        }

    let isIdentHead c = isLetter c || c = '_'
    let isIdentTail c = isLetter c || isDigit c || c = '_'
    let pident = many1Satisfy2L isIdentHead isIdentTail "identifier"
    let pidentWS = pident .>> ws

    let pvariable: Parser<Expr> =
        parse {
            let var = pident
            let! var = var |>> Variable
            return var
        }

    let pvalue = pnumliteral <|> pboolliteral <|> pvariable

    /// 演算子の優先順位
    let oppa = OperatorPrecedenceParser<Expr, unit, unit>()

    let parithmetic = oppa.ExpressionParser
    let terma = (pvalue .>> ws) <|> between (strWS "(") (strWS ")") parithmetic

    oppa.TermParser <- terma
    oppa.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, (fun x y -> Add(x, y))))
    oppa.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, (fun x y -> Multiply(x, y))))

    let oppc = OperatorPrecedenceParser<Expr, unit, unit>()
    let pcomparison = oppc.ExpressionParser
    let termc = (parithmetic .>> ws) <|> between (strWS "(") (strWS ")") pcomparison

    oppc.TermParser <- termc
    oppc.AddOperator(InfixOperator("<", ws, 2, Associativity.Left, (fun x y -> LessThan(x, y))))

    let pexpr, pexprRef: Parser<Expr> * Parser<Expr> ref = createParserForwardedToRef()

    do pexprRef := choice
                       [ attempt parithmetic
                         attempt pcomparison
                         attempt pvalue ]

    let pstmt, pstmtRef: Parser<Stmt> * Parser<Stmt> ref = createParserForwardedToRef()

    let passign: Parser<Stmt> =
        pipe3 (pidentWS) (strWS "=") (parithmetic) (fun ident wsString expr -> Assign(ident, expr))

    let pcond = attempt pcomparison <|> attempt pvalue

    let pwhile: Parser<Stmt> =
        parse {
            do! skipString "while" >>. spaces1
            let! cond = pcond |> between (strWS "(") (strWS ")")
            do! ws
            let! body = pstmt |> between (strWS "{") (strWS "}")
            return While(cond, body)
        }

    let psequence: Parser<Stmt> = pipe3 (pstmt) (strWS ";") (pstmt) (fun first _ second -> Sequence(first, second))

    pstmtRef := choice
                    [ attempt pwhile
                      attempt passign ]

    let parseBy (p: Parser<'a>) (code: string) =
        match run (ws >>. p .>> ws .>> eof) code with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg

    let parse (code: string) = parseBy pstmt code
