module UnderstandingComputation.Chap4.Lexer

open System
open System.Text.RegularExpressions

type Token = char

type Pattern = string

type SIMPLELexicalRule =
    { token: Token
      pattern: Pattern }

[<RequireQualifiedAccess>]
module SIMPLELexicalRule =

    let create token pattern: SIMPLELexicalRule =
        { token = token
          pattern = pattern }

    let grammer =
        [ create 'i' "if"
          create 'e' "else"
          create 'w' "while"
          create 'd' "do-nothing"
          create '(' "\\("
          create ')' "\\)"
          create '{' "{"
          create '}' "}"
          create ';' ";"
          create '=' "="
          create '+' "\\+"
          create '*' "\\*"
          create '<' "<"
          create 'n' "[0-9]+"
          create 'b' "(true|false)(?![a-z])"
          create 'v' "[a-z]+" ]

/// SIMPLEの字句解析器
type Lexer(code: string) =
    let mutable _code = code
    let grammer = SIMPLELexicalRule.grammer

    member private self.MatchAtBegin(pattern, input): Match =
        let pattern = sprintf "\A%s" pattern
        Regex.Match(input, pattern)

    member private self.RuleMatching(input: string): SIMPLELexicalRule * Match =
        let matches = List.map (fun rule -> self.MatchAtBegin(rule.pattern, input)) grammer
        let rulesAndMatches = List.zip grammer matches |> List.filter (fun (rule, mat) -> mat.Success)
        List.maxBy (fun (rule, mat: Match) -> mat.Length) rulesAndMatches

    member private self.NextToken(): Token =
        let rule, mat = self.RuleMatching(_code)
        _code <- _code.Substring(mat.Length)
        rule.token

    member self.Analyze(): Token list =
        let mutable res = []
        while String.IsNullOrEmpty(_code) |> not do
            let head = _code.[0]
            if head = ' ' then
                _code <- _code.Substring(1)
            else
                let token = self.NextToken()
                res <- token :: res
        List.rev res
