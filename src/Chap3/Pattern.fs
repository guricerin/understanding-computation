module UnderstandingComputation.Chap3.Pattern

open Automaton
open NFA

type Pattern =
    /// ""
    | Empty
    /// a
    | Literal of char
    /// ab
    | Concat of Pattern * Pattern
    /// a|b
    | Choose of Pattern * Pattern
    /// a*
    | Repeat of Pattern

    member self.Precedence =
        match self with
        | Empty -> 3
        | Literal _ -> 3
        | Concat _ -> 1
        | Choose _ -> 0
        | Repeat _ -> 2

    member self.Bracket(outer: Pattern) =
        if self.Precedence < outer.Precedence then self.Tos |> sprintf "(%s)" else self.Tos

    member self.Tos =
        let f (pat: Pattern) = pat.Bracket(self)
        match self with
        | Empty -> ""
        | Literal c -> c.ToString()
        | Concat(first, second) -> List.map f [ first; second ] |> String.concat ""
        | Choose(first, second) -> List.map f [ first; second ] |> String.concat "|"
        | Repeat pattern -> pattern.Bracket(self) |> sprintf "%s*"

    override self.ToString() = self.Tos |> sprintf "/%s/"

[<RequireQualifiedAccess>]
module Pattern =

    let rec toNFA' start pattern =
        match pattern with
        | Empty ->
            let rulebook = NFARulebook.ofList []
            NFA.create [ start ] [ start ] rulebook, start
        | Literal c ->
            let accept = start + 1
            let rule = NFARule.create start (Some c) accept
            let rulebook = NFARulebook.ofList [ rule ]
            NFA.create [ start ] [ accept ] rulebook, accept
        | Concat(first, second) ->
            let fnfa, faccept = toNFA' start first
            let snfa, saccept = toNFA' faccept second
            let rules = NFARulebook.append fnfa.rulebook snfa.rulebook

            let f (state: State) =
                { NFARule.current = state
                  input = None
                  next = State faccept }

            // firstの受理状態とsecondの開始状態を自由移動で繋げる
            let extraRules =
                Set.map f fnfa.accepts
                |> Set.toList
                |> NFARulebook.ofList

            let start = States.ofList [ start ]
            let accepts = snfa.accepts
            let rulebook = NFARulebook.append rules extraRules
            { NFA.currents = start
              accepts = accepts
              rulebook = rulebook }, saccept

    /// 等価なNFAに変換
    let toNFA pattern =
        let start = 1
        let res, _ = toNFA' start pattern
        res

    /// 指定の文字列にマッチするか
    let matches str pattern =
        pattern
        |> toNFA
        |> NFA.accepts str
