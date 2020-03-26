module UnderstandingComputation.Chap3.DFA

open Automaton

/// 決定性有限オートマトン (Deterministic Finite Machine)
/// 状態と入力の組み合わせに対して、必ず規則を1つだけもつ

/// 決定性有限オートマトンの規則集
type DFARulebook<'a> = DFARulebook of FARule<'a> list

[<RequireQualifiedAccess>]
module DFARulebook =

    let ofList ls = DFARulebook ls

    /// 適用すべき規則を特定
    let ruleFor state input (DFARulebook rulebook) =
        let f rule = FARule.appliesTo state input rule
        match List.tryFind f rulebook with
        | Some rule -> rule
        | None -> invalidArg "rulebook" "適用可能な規則なし"

    /// 遷移関数
    let nextState state input rulebook = ruleFor state input rulebook |> FARule.follow


/// 決定性有限オートマトン
type DFA<'a> =
    { current: 'a
      accepts: 'a list
      rulebook: DFARulebook<'a> }

[<RequireQualifiedAccess>]
module DFA =

    let create state accepts rulebook =
        { DFA.current = state
          accepts = accepts
          rulebook = rulebook }

    /// 現在は受理状態か
    let isAccepting dfa = List.contains dfa.current dfa.accepts

    /// 入力から1文字読み、状態を変更
    let readChar c dfa =
        let next = DFARulebook.nextState dfa.current c dfa.rulebook
        { dfa with current = next }

    /// 入力文字列を全て読む
    let readString (str: string) dfa =
        let f d c = readChar c d
        Seq.fold f dfa str

type DFADesign<'a> =
    { start: 'a
      accepts: 'a list
      rulebook: DFARulebook<'a> }

[<RequireQualifiedAccess>]
module DFADesign =

    let create start accepts rulebook =
        { DFADesign.start = start
          accepts = accepts
          rulebook = rulebook }

    let toDFA design = DFA.create design.start design.accepts design.rulebook

    /// 入力文字列を受理するか
    let accepts str design =
        design
        |> toDFA
        |> DFA.readString str
        |> DFA.isAccepting
