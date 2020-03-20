module UnderstandingComputation.Chap3.Pattern

open Automaton
open NFA

/// 正規表現
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

    /// NFAとその受理状態の中で番号が最もでかいやつの組を返す
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

            // firstの受理状態とsecondの開始状態を自由移動で繋げる
            let f (state: State) =
                { NFARule.current = state
                  input = None
                  next = State faccept }
            let extraRules =
                Set.map f fnfa.accepts
                |> Set.toList
                |> NFARulebook.ofList

            let start = States.ofList [ start ] // [start] か fnfa.currents のどっちだ？
            let accepts = snfa.accepts
            let rulebook = NFARulebook.append rules extraRules
            { NFA.currents = start
              accepts = accepts
              rulebook = rulebook }, saccept
        | Choose(first, second) ->
            let fstart = start
            let fnfa, faccept = toNFA' fstart first
            let snfa, saccept = toNFA' faccept second

            let accepts = Set.union fnfa.accepts snfa.accepts
            let rules = NFARulebook.append fnfa.rulebook snfa.rulebook

            // 新たな開始状態を作り、そこからfirstの開始状態とsecondの開始状態への分岐を繋げる
            let extraRules =
                [ NFARule.create start None fstart
                  NFARule.create start None faccept ]
                |> NFARulebook.ofList

            let rulebook = NFARulebook.append rules extraRules

            { NFA.currents = States.ofList [ start ]
              accepts = accepts
              rulebook = rulebook }, saccept
        | Repeat pattern ->
            // 元のNFA
            let patstart = start
            let patnfa, pataccept = toNFA' patstart pattern
            // 受理状態でもある新たな開始状態
            let newstart = pataccept + 1
            let accepts = Set.union patnfa.accepts (Set.singleton (State newstart))

            // 元のNFAの受理状態を元の開始状態に繋げるための自由移動
            let f (state: State) =
                { NFARule.current = state
                  input = None
                  next = State patstart }
            let extraRules =
                Set.map f patnfa.accepts
                |> Set.toList
                |> List.append [ NFARule.create newstart None patstart ] // 新たな開始状態を元の開始状態に繋げる自由移動
                |> NFARulebook.ofList

            let rulebook = NFARulebook.append patnfa.rulebook extraRules
            { NFA.currents = States.ofList [ newstart ]
              accepts = accepts
              rulebook = rulebook }, newstart

    /// 等価なNFAに変換
    let toNFA pattern =
        let start = 0
        let res, _ = toNFA' start pattern
        res

    /// 指定の文字列にマッチするか
    let matches str pattern =
        pattern
        |> toNFA
        |> NFA.accepts str
