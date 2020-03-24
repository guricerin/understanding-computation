module UnderstandingComputation.Chap7.CyclicTagSystem

open System
open FSharpx.Collections // Queue

[<RequireQualifiedAccess>]
type CyclicChar =
    | Zero
    | One

    member self.toChar =
        match self with
        | Zero -> '0'
        | One -> '1'

    static member ofChar (ch: char) =
        match ch with
        | '0' -> Zero
        | '1' -> One
        | _ ->
            let msg = sprintf "%c is unexpected char" ch
            invalidArg "ch" msg

type CyclicStr = CyclicChar list

[<RequireQualifiedAccess>]
module CyclicStr =

    let ofStr (str: string) =
        str
        |> List.ofSeq
        |> List.map CyclicChar.ofChar

    let toStr (ls: CyclicStr) =
        ls
        |> List.map (fun ch -> ch.toChar)
        |> String.Concat

type CTagRule =
    { appends: CyclicStr }

    override self.ToString() =
        self.appends
        |> CyclicStr.toStr
        |> sprintf "#<CyclicTagRule %s>"

[<RequireQualifiedAccess>]
module CTagRule =

    let create (str: string) =
        let str = CyclicStr.ofStr str
        { CTagRule.appends = str }

    let follow (cstr: CyclicStr) rule = cstr @ rule.appends

type CTagRulebook = Queue<CTagRule>

[<RequireQualifiedAccess>]
module CTagRulebook =

    [<Literal>]
    let DeletionNum = 1

    let create rules = Queue.ofList rules

    /// 規則を適用可能か？
    let appliesTo (cstr: CyclicStr) = List.length cstr >= DeletionNum && List.head cstr = CyclicChar.One

    /// 規則集を循環させつつ、先頭の規則と共に返す
    let cycle (rulebook: CTagRulebook): CTagRule * CTagRulebook =
        let head, tail =
            match rulebook |> Queue.tryUncons with
            | Some(head, tail) -> (head, tail)
            | None -> invalidArg "rulebook" "rulebook is empty"

        let tail = Queue.conj head tail
        head, tail

    let followNextRule (cstr: CyclicStr) (rulebook: CTagRulebook): CyclicStr * CTagRulebook =
        // 規則が適用可能かに関係なく、規則集自体は循環させる
        let rule, rulebook = cycle rulebook
        if appliesTo cstr then
            let cstr = CTagRule.follow cstr rule
            cstr, rulebook
        else
            cstr, rulebook

    /// 遷移関数
    let nextString (cstr: CyclicStr) (rulebook: CTagRulebook): CyclicStr * CTagRulebook =
        let cstr, rulebook = followNextRule cstr rulebook

        let cstr =
            match cstr with
            | [] -> invalidArg "cstr" "先頭文字を削除する前から空文字列という状況はありえない"
            | x :: xs -> xs
        cstr, rulebook

type CTagSystem =
    { current: CyclicStr
      rulebook: CTagRulebook }

[<RequireQualifiedAccess>]
module CTagSystem =

    let create str rulebook =
        { CTagSystem.current = CyclicStr.ofStr str
          rulebook = rulebook }

    let step (system: CTagSystem) =
        let next, rulebook = CTagRulebook.nextString system.current system.rulebook
        { system with
              current = next
              rulebook = rulebook }

    let run (system: CTagSystem) =
        let rec loop cur =
            let next = step cur
            // 文字列が空になったら停止
            match next.current with
            | [] -> next
            | _ -> loop next

        loop system

open TagSystem

/// タグシステムを循環タグシステムに変換
type CTagEncoder(system: TagSystem) =

    /// タグシステムで使用されるアルファベットをダブりなしで列挙し、文字列として連結
    let _alphabet =
        let f (acc: string) (rule: TagRule) = acc + (rule.first.ToString()) + rule.appends
        system.rulebook.rules
        |> Seq.fold f ""
        |> (+) system.current
        |> Set.ofSeq
        |> String.Concat

    let _system = system

    member self.Alphabet = _alphabet

    member self.EncodeChar(ch: char): string =
        seq {
            for e in _alphabet do
                if ch = e then yield CyclicChar.One.toChar else yield CyclicChar.Zero.toChar
        }
        |> String.Concat

    member self.EncodeString(str: string): string =
        str
        |> Seq.map self.EncodeChar
        |> String.Concat

    member self.ToCTagRule(rule: TagRule): CTagRule =
        rule.appends
        |> self.EncodeString
        |> CTagRule.create

    member self.CRules =
        let cruleFor (ch: char) =
            match TagRulebook.ruleFor (ch.ToString()) _system.rulebook with
            | Some rule -> self.ToCTagRule rule
            | None -> CTagRule.create ""
        _alphabet
        |> Seq.map cruleFor
        |> List.ofSeq

    /// 元のタグシステムの削除数をシミュレートするため、余分な空の規則を生成
    member self.CyclicPaddingRules =
        let addNum = _alphabet.Length * (_system.rulebook.deletionNum - 1)
        if addNum <= 0
        then []
        else List.init addNum (fun _ -> CTagRule.create "")

    member self.CTagRulebook =
        let crules, padding = self.CRules, self.CyclicPaddingRules
        CTagRulebook.create (crules @ padding)

    member self.CTagSystem =
        let encoded = self.EncodeString _system.current
        let ctagrulebook = self.CTagRulebook
        CTagSystem.create encoded ctagrulebook
