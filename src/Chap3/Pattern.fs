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

    member private self.Precedence =
        match self with
        | Empty -> 3
        | Literal _ -> 3
        | Concat _ -> 1
        | Choose _ -> 0
        | Repeat _ -> 2

    member private self.Bracket(outer: Pattern) =
        if self.Precedence < outer.Precedence then self.Tos |> sprintf "(%s)" else self.Tos

    member private self.Tos =
        let f (pat: Pattern) = pat.Bracket(self)
        match self with
        | Empty -> ""
        | Literal c -> c.ToString()
        | Concat(first, second) -> List.map f [ first; second ] |> String.concat ""
        | Choose(first, second) -> List.map f [ first; second ] |> String.concat "|"
        | Repeat pattern -> pattern.Bracket(self) |> sprintf "%s*"

    override self.ToString() = self.Tos |> sprintf "/%s/"

type PatternConverter(pattern: Pattern) =
    let _pattern = pattern
    let mutable _state = 0

    /// 原著では新しい状態を適当なメモリアドレスから取得している
    /// ここでは代わりにクラスの中に可変な値を閉じ込め、それをインクリさせて呼び出し元に返す方針を取る
    member private self.NewState() =
        let res = _state
        _state <- _state + 1
        res

    member private self.ToNFADesign(pattern: Pattern) =
        match pattern with
        | Empty ->
            let start = self.NewState()
            let accepts = [ start ]
            let rulebook = NFARulebook.ofList []
            NFADesign.create start accepts rulebook
        | Literal c ->
            let start, accept = self.NewState(), self.NewState()
            let rule = NFARule.create start (Some c) accept
            let rulebook = NFARulebook.ofList [ rule ]
            NFADesign.create start [ accept ] rulebook
        | Concat(first, second) ->
            let firstNFADesign = self.ToNFADesign first
            let secondNFADesign = self.ToNFADesign second
            let start = firstNFADesign.start
            let accepts = secondNFADesign.accepts
            let rules = NFARulebook.append firstNFADesign.rulebook secondNFADesign.rulebook
            // firstの受理状態とsecondの開始状態を自由移動で繋げる
            let extraRules =
                Set.map (fun state -> NFARule.create state None secondNFADesign.start) firstNFADesign.accepts
                |> Set.toList
                |> NFARulebook.ofList

            let rulebook = NFARulebook.append rules extraRules
            { NFADesign.start = start
              accepts = accepts
              rulebook = rulebook }
        | Choose(first, second) ->
            let firstNFADesign = self.ToNFADesign first
            let secondNFADesign = self.ToNFADesign second
            let start = self.NewState()
            let accepts = Set.union firstNFADesign.accepts secondNFADesign.accepts
            let rules = NFARulebook.append firstNFADesign.rulebook secondNFADesign.rulebook
            // 新たな開始状態を作り、そこからfirstの開始状態とsecondの開始状態へと分岐させる
            let extraRules =
                [ NFARule.create start None firstNFADesign.start
                  NFARule.create start None secondNFADesign.start ]
                |> NFARulebook.ofList

            let rulebook = NFARulebook.append rules extraRules
            { NFADesign.start = start
              accepts = accepts
              rulebook = rulebook }
        | Repeat pattern ->
            let patNFADesign = self.ToNFADesign pattern
            // 受理状態でもある新たな開始状態
            let start = self.NewState()
            let accepts = Set.union patNFADesign.accepts (Set.singleton start)
            let rules = patNFADesign.rulebook

            let extraRules =
                patNFADesign.accepts
                |> Set.map (fun accept -> NFARule.create accept None patNFADesign.start) // 元の受理状態と元の開始状態を自由移動で繋げる（循環）
                |> Set.toList
                |> List.append [ NFARule.create start None patNFADesign.start ] // 新たな開始状態と元の開始状態を自由移動で繋げる
                |> NFARulebook.ofList

            let rulebook = NFARulebook.append rules extraRules
            { NFADesign.start = start
              accepts = accepts
              rulebook = rulebook }

    member self.ToNFADesign() =
        do _state <- 0
        self.ToNFADesign(_pattern)

    member self.Matches(str: string): bool = self.ToNFADesign() |> NFADesign.accepts str
