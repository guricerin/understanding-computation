module UnderstandingComputation.Chap4.Parser

open PDA
open NPDA
open Lexer

module SIMPLEParser =
    let startRule = PDARule.create 1 None 2 '$' [ 'S'; '$' ]

    /// 文法規則に従い文・式を展開 -> スタックを拡張する
    let symbolRules =
        [ /// <statement> ::= <while> | <assign>
          PDARule.create 2 None 2 'S' [ 'W' ]
          PDARule.create 2 None 2 'S' [ 'A' ]

          /// <while> ::= 'w' '(' <expression> ')' '{' <statement> '}'
          PDARule.create 2 None 2 'W' (List.ofSeq "w(E){S}")

          /// <assign> ::= 'v' '=' <expression>
          PDARule.create 2 None 2 'A' (List.ofSeq "v=E")

          /// <expresion> ::= <less-than>
          PDARule.create 2 None 2 'E' (List.ofSeq "L")

          /// <less-than> ::= <multiply> '<' <less-than> | <multiply>
          PDARule.create 2 None 2 'L' (List.ofSeq "M<L")
          PDARule.create 2 None 2 'L' (List.ofSeq "M")

          /// <multiply> ::= <term> '*' <multiply> | <term>
          PDARule.create 2 None 2 'M' (List.ofSeq "T*M")
          PDARule.create 2 None 2 'M' (List.ofSeq "T")

          /// <term> ::= 'n' | 'v'
          PDARule.create 2 None 2 'T' (List.ofSeq "n")
          PDARule.create 2 None 2 'T' (List.ofSeq "v") ]

    /// 常にスタックを小さくし、入力を消費していく
    let tokenRules =
        let f (rule: SIMPLELexicalRule) = PDARule.create 2 (Some rule.token) 2 rule.token []
        List.map f SIMPLELexicalRule.grammer

    /// スタックが空になれば受理状態にさせる
    let stopRule = PDARule.create 2 None 3 '$' [ '$' ]

    let rulebook = [ startRule; stopRule ] @ symbolRules @ tokenRules

    let npdaDesign = NPDADesign.create 1 '$' (Set.ofList [ 3 ]) rulebook
