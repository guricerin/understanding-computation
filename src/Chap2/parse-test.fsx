#r "../../packages/FParsec/lib/netstandard2.0/FParsecCS.dll"
#r "../../packages/FParsec/lib/netstandard2.0/FParsec.dll"
#r "bin/Release/netstandard2.0/Chap2.dll"

// dllをロードする順番: FParsecCS -> FParsec

open FParsec
open UnderstandingComputation.Chap2

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

    let psequence: Parser<Stmt> =
        pipe3 (passign) (strWS ";") (pstmt) (fun first _ second -> Sequence(first, second))

    pstmtRef := choice
                    [ attempt pwhile
                      attempt passign
                      attempt psequence ]

    let parseBy (p: Parser<'a>) (code: string) =
        match run (ws >>. p .>> ws .>> eof) code with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "%s" msg

    let parseByExpr code = parseBy pexpr code

// -------------------------------------------------------------
let inline put x = printfn "%A" x

Parsing.parseByExpr "0" |> put
Parsing.parseByExpr " -1" |> put
Parsing.parseByExpr "    -90008   " |> put
Parsing.parseByExpr " 32 " |> put
Parsing.parseByExpr "1 + 2 * 3 + 4" |> put
Parsing.parseByExpr "1 * 2 + 3 * 4" |> put
// Parsing.parseByExpr "1 < y" |> put
// Parsing.parseByExpr "  1  < y    " |> put
Parsing.parseByExpr "false" |> put
Parsing.parseByExpr "x" |> put
Parsing.parseByExpr " xyx" |> put
Parsing.parseByExpr " truh  " |> put
Parsing.parseByExpr " true  " |> put
// Parsing.parseByExpr " x  * 10  " |> put
Parsing.parseBy Parsing.pstmt "while (true) { x = x + 1 }" |> put
Parsing.parseBy Parsing.pstmt "while (y) { x = x + 1 }" |> put
Parsing.parseBy Parsing.pstmt "while (x + 19) { x = x + 1 }" |> put
Parsing.parseBy Parsing.pstmt "while (x < 19) { x = x + 1 }" |> put
Parsing.parseBy Parsing.pstmt "while (x < 5) { x = x * 3 }" |> put
// Parsing.parseBy Parsing.pstmt "   while  (x  < 19  ) {   x  = x + 1 }" |> put
