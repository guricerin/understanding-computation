module UnderstandingComputation.Chap3.NFA

open Automaton
open DFA

/// 非決定性有限オートマトン (Nondeterministic Finite Machine)
/// 状態と入力の組み合わせに対して、必ず規則を1つだけもつ

/// オートマトンの規則
type NFARule<'a> =
    { current: 'a
      input: char option
      next: 'a }

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
        { NFARule.current = cur
          input = input
          next = next }

    /// 規則を適用できるか
    let appliesTo state c rule = rule.current = state && rule.input = c

    /// 規則を適用した際の次の状態
    let follow rule = rule.next

/// 決定性有限オートマトンの規則集
type NFARulebook<'a> = NFARulebook of NFARule<'a> list

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
    let nextStates states input rulebook =
        let f state = followRulesFor state input rulebook
        states
        |> Set.map f
        |> Set.unionMany // 集合の集合を集合にする (flat)

    /// 与えられた状態の集合から自由移動によって到達可能な状態を全列挙
    let rec followFreeMoves states rulebook =
        let moreStates = nextStates states None rulebook
        if Set.isSubset moreStates states
        then states
        else followFreeMoves (Set.union states moreStates) rulebook

    let append (NFARulebook xs) (NFARulebook ys) = NFARulebook(xs @ ys)

    /// 取りうる入力文字を列挙（自由移動は除く）
    let alphabet (NFARulebook ls) =
        ls
        |> List.filter (fun rule -> rule.input <> None)
        |> List.map (fun rule -> Option.get rule.input)
        |> List.distinct

type NFA<'a when 'a: comparison> =
    { currents: Set<'a>
      accepts: Set<'a>
      rulebook: NFARulebook<'a> }

[<RequireQualifiedAccess>]
module NFA =

    let freeMove nfa =
        let freemoved = NFARulebook.followFreeMoves nfa.currents nfa.rulebook
        { nfa with currents = freemoved }

    let create curs accepts rulebook =
        let nfa =
            { NFA.currents = Set.ofList curs
              accepts = Set.ofList accepts
              rulebook = rulebook }
        freeMove nfa

    /// 受理状態である可能性があるか
    let isAccepting nfa =
        Set.intersect nfa.currents nfa.accepts // 積集合
        |> Set.isEmpty
        |> not

    let readChar c nfa =
        let freemoved = NFARulebook.followFreeMoves nfa.currents nfa.rulebook // まず自由移動
        let currents = NFARulebook.nextStates freemoved c nfa.rulebook
        { nfa with currents = currents } |> freeMove

    let readString str nfa =
        let f n c = readChar (Some c) n
        Seq.fold f nfa str

type NFADesign<'a when 'a: comparison> =
    { start: 'a
      accepts: Set<'a>
      rulebook: NFARulebook<'a> }

[<RequireQualifiedAccess>]
module NFADesign =

    let create start accepts rulebook =
        { NFADesign.start = start
          accepts = Set.ofList accepts
          rulebook = rulebook }

    let toNFA design =
        { NFA.currents = Set.ofList [ design.start ]
          accepts = design.accepts
          rulebook = design.rulebook }
        |> NFA.freeMove

    let accepts str design =
        design
        |> toNFA
        |> NFA.readString str
        |> NFA.isAccepting

    /// 任意の状態を指定してNFAを作成
    let toNFAForSimulation start design =
        { NFA.currents = start
          accepts = design.accepts
          rulebook = design.rulebook }
        |> NFA.freeMove

type NFASimulation<'a when 'a: comparison> =
    { design: NFADesign<'a> }

[<RequireQualifiedAccess>]
module NFASimulation =

    let create nfaDesign = { NFASimulation.design = nfaDesign }

    let nextStates (states: Set<'a>) (ch: char) simulation: Set<'a> =
        NFADesign.toNFAForSimulation states simulation.design
        |> NFA.readChar (Some ch)
        |> fun nfa -> nfa.currents

    let rulesFor (states: Set<'a>) simulation: FARule<Set<'a>> list =
        let alphabets = simulation.design.rulebook |> NFARulebook.alphabet

        let f ch = nextStates states ch simulation |> FARule.create states ch
        List.map f alphabets

    let rec discoverStatesAndRules (states: Set<Set<'a>>) simulation: Set<Set<'a>> * List<FARule<Set<'a>>> =
        let rules =
            states
            |> Set.map (fun state -> rulesFor state simulation)
            |> Set.toList
            |> List.collect id // flatten

        let moreStates = List.map FARule.follow rules |> Set.ofList
        if Set.isSubset moreStates states
        then states, rules
        else discoverStatesAndRules (Set.union states moreStates) simulation

    let toDFADesign simulation: DFADesign<Set<'a>> =
        let start = (NFADesign.toNFA simulation.design).currents
        let starts = Set.ofList [ start ]
        let states, rules = discoverStatesAndRules starts simulation
        let f state = NFADesign.toNFAForSimulation state simulation.design |> NFA.isAccepting
        let accepts = Set.filter f states |> Set.toList
        DFADesign.create start accepts (DFARulebook.ofList rules)
