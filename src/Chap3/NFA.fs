module UnderstandingComputation.Chap3.NFA

open Automaton

/// 非決定性有限オートマトン (Nondeterministic Finite Machine)
/// 状態と入力の組み合わせに対して、必ず規則を1つだけもつ

type States = Set<State>

[<RequireQualifiedAccess>]
module States =

    let ofList ls: States =
        ls
        |> List.map State
        |> Set.ofList

/// オートマトンの規則
type NFARule =
    { current: State
      input: char option
      next: State }

    override self.ToString() =
        let cur, next = self.current.ToString(), self.next.ToString()

        let input =
            match self.input with
            | Some c -> c
            | None -> '_'
        sprintf "#<FARule %s --%c--> %s>" cur input next

[<RequireQualifiedAccess>]
module NFARule =

    let create cur input next =
        { NFARule.current = State cur
          input = input
          next = State next }

    /// 規則を適用できるか
    let appliesTo state c (rule: NFARule) = rule.current = state && rule.input = c

    /// 規則を適用した際の次の状態
    let follow rule = rule.next

/// 決定性有限オートマトンの規則集
type NFARulebook = NFARulebook of NFARule list

[<RequireQualifiedAccess>]
module NFARulebook =

    let ofList ls = NFARulebook ls

    /// 適用すべき規則を列挙
    let rulesFor state input (NFARulebook rulebook) =
        let f rule = NFARule.appliesTo state input rule
        List.filter f rulebook

    /// 取りうる次の状態を列挙
    let followRulesFor state input rulebook =
        rulebook
        |> rulesFor state input
        |> List.map NFARule.follow
        |> Set.ofList

    /// 遷移関数
    let nextStates (states: States) input (rulebook: NFARulebook): States =
        let f state = followRulesFor state input rulebook
        states
        |> Set.map f
        |> Set.unionMany // 集合の集合を集合にする (flat)

    /// 与えられた状態の集合から自由移動によって到達可能な状態を全列挙
    let rec followFreeMoves (states: States) rulebook =
        let moreStates = nextStates states None rulebook
        if Set.isSubset moreStates states
        then states
        else followFreeMoves (Set.union states moreStates) rulebook

    let append (NFARulebook xs) (NFARulebook ys) = NFARulebook(xs @ ys)

type NFA =
    { currents: States
      accepts: States
      rulebook: NFARulebook }

[<RequireQualifiedAccess>]
module NFA =

    let create curs accepts rulebook =
        { NFA.currents = States.ofList curs
          accepts = States.ofList accepts
          rulebook = rulebook }

    /// 受理状態である可能性があるか
    let isAccepting nfa =
        Set.intersect nfa.currents nfa.accepts // 積集合
        |> Set.isEmpty
        |> not

    let readChar c nfa =
        let freemoved = NFARulebook.followFreeMoves nfa.currents nfa.rulebook // まず自由移動
        let currents = NFARulebook.nextStates freemoved c nfa.rulebook
        { nfa with currents = currents }

    let readString str nfa =
        let f n c = readChar (Some c) n
        Seq.fold f nfa str

    let accepts str nfa =
        nfa
        |> readString str
        |> isAccepting
