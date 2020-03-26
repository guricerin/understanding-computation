module UnderstandingComputation.Chap3.NFATest

open Expecto
open UnderstandingComputation.Chap3
open Automaton
open DFA
open NFA

module NFATest =
    // 最後から3番目の文字がbである文字列を受理
    let ls =
        [ NFARule.create 1 (Some 'a') 1
          NFARule.create 1 (Some 'b') 1
          NFARule.create 1 (Some 'b') 2
          NFARule.create 2 (Some 'a') 3
          NFARule.create 2 (Some 'b') 3
          NFARule.create 3 (Some 'a') 4
          NFARule.create 3 (Some 'b') 4 ]

    let rulebook = NFARulebook.ofList ls

    [<Tests>]
    let ``nfa rulebook`` =
        test "nfa rulebook" {
            let actual = NFARulebook.nextStates (Set.ofList [ 1 ]) (Some 'b') rulebook
            let expect = Set.ofList [ 1; 2 ]
            Expect.equal actual expect ""

            let actual = NFARulebook.nextStates (Set.ofList [ 1; 2 ]) (Some 'a') rulebook
            let expect = Set.ofList [ 1; 3 ]
            Expect.equal actual expect ""

            let actual = NFARulebook.nextStates (Set.ofList [ 1; 3 ]) (Some 'b') rulebook
            let expect = Set.ofList [ 1; 2; 4 ]
            Expect.equal actual expect ""
        }

    [<Tests>]
    let ``nfa`` =
        test "nfa" {
            let nfa = NFA.create [ 1 ] [ 4 ] rulebook
            Expect.isFalse (NFA.isAccepting nfa) ""

            let nfa = NFA.create [ 1; 2; 4 ] [ 4 ] rulebook
            Expect.isTrue (NFA.isAccepting nfa) ""
        }

    [<Tests>]
    let ``nfa readChar`` =
        test "nfa readChar" {
            let nfa = NFA.create [ 1 ] [ 4 ] rulebook
            let nfa = NFA.readChar (Some 'b') nfa
            Expect.isFalse (NFA.isAccepting nfa) ""

            let nfa = NFA.readChar (Some 'a') nfa
            Expect.isFalse (NFA.isAccepting nfa) ""

            let nfa = NFA.readChar (Some 'b') nfa
            Expect.isTrue (NFA.isAccepting nfa) ""
        }

module NFAFreeMove =
    // aからなる長さが2か3の倍数の文字列を受理
    let ls =
        [ NFARule.create 1 None 2
          NFARule.create 1 None 4
          NFARule.create 2 (Some 'a') 3
          NFARule.create 3 (Some 'a') 2
          NFARule.create 4 (Some 'a') 5
          NFARule.create 5 (Some 'a') 6
          NFARule.create 6 (Some 'a') 4 ]

    let rulebook = NFARulebook.ofList ls

    [<Tests>]
    let ``nfa free move`` =
        test "nfa free move" {
            let actual = NFARulebook.nextStates (Set.ofList [ 1 ]) None rulebook
            let expect = Set.ofList [ 2; 4 ]
            Expect.equal actual expect ""

            let actual = NFARulebook.followFreeMoves (Set.ofList [ 1 ]) rulebook
            let expect = Set.ofList [ 1; 2; 4 ]
            Expect.equal actual expect ""
        }

    [<Tests>]
    let `` nfa accepts`` =
        test "nfa accepts" {
            let design = NFADesign.create 1 [ 2; 4 ] rulebook
            let actual = NFADesign.accepts "aa" design
            Expect.isTrue actual ""
            let actual = NFADesign.accepts "aaa" design
            Expect.isTrue actual ""
            let actual = NFADesign.accepts "aaaaa" design
            Expect.isFalse actual ""
            let actual = NFADesign.accepts "aaaaaa" design
            Expect.isTrue actual ""
        }

module NFASimulation =
    let ls =
        [ NFARule.create 1 (Some 'a') 1
          NFARule.create 1 (Some 'a') 2
          NFARule.create 1 None 2
          NFARule.create 2 (Some 'b') 3
          NFARule.create 3 (Some 'b') 1
          NFARule.create 3 None 2 ]

    let rulebook = NFARulebook.ofList ls
    let nfaDesign = NFADesign.create 1 [ 3 ] rulebook
    let simulation = NFASimulation.create nfaDesign

    [<Tests>]
    let ``nfa design current`` =
        test "nfa design current" {
            let design = NFADesign.create 1 [ 3 ] rulebook
            let actual = (NFADesign.toNFA design).currents
            let expect = Set.ofList [ 1; 2 ]
            Expect.equal actual expect ""

            let actual = (NFADesign.toNFAForSimulation (Set.ofList [ 2 ]) design).currents
            let expect = Set.ofList [ 2 ]
            Expect.equal actual expect ""

            let nfa = NFADesign.toNFAForSimulation (Set.ofList [ 2; 3 ]) design
            let actual = nfa.currents
            let expect = Set.ofList [ 2; 3 ]
            Expect.equal actual expect ""
            let nfa = NFA.readChar (Some 'b') nfa
            let actual = nfa.currents
            let expect = Set.ofList [ 1; 2; 3 ]
            Expect.equal actual expect ""
        }

    [<Tests>]
    let ``next states`` =
        test "next state" {

            let actual = NFASimulation.nextStates (Set.ofList [ 1; 2 ]) 'a' simulation
            let expect = Set.ofList [ 1; 2 ]
            Expect.equal actual expect ""

            let actual = NFASimulation.nextStates (Set.ofList [ 1; 2 ]) 'b' simulation
            let expect = Set.ofList [ 2; 3 ]
            Expect.equal actual expect ""

            let actual = NFASimulation.nextStates (Set.ofList [ 3; 2 ]) 'b' simulation
            let expect = Set.ofList [ 1; 2; 3 ]
            Expect.equal actual expect ""

            let actual = NFASimulation.nextStates (Set.ofList [ 1; 3; 2 ]) 'b' simulation
            let expect = Set.ofList [ 1; 2; 3 ]
            Expect.equal actual expect ""

            let actual = NFASimulation.nextStates (Set.ofList [ 1; 3; 2 ]) 'a' simulation
            let expect = Set.ofList [ 1; 2 ]
            Expect.equal actual expect ""
        }

    [<Tests>]
    let ``rule for`` =
        test "rule for" {

            let actual = NFASimulation.rulesFor (Set.ofList [ 1; 2 ]) simulation
            let expect = "[#<FARule set [1; 2] --a--> set [1; 2]>; #<FARule set [1; 2] --b--> set [2; 3]>]"
            Expect.equal (actual.ToString()) expect ""
            let actual = NFASimulation.rulesFor (Set.ofList [ 3; 2 ]) simulation
            let expect = "[#<FARule set [2; 3] --a--> set []>; #<FARule set [2; 3] --b--> set [1; 2; 3]>]"
            Expect.equal (actual.ToString()) expect ""
        }

    [<Tests>]
    let ``discover states and rules`` =
        test "discover states and rules" {
            let start = (NFADesign.toNFA nfaDesign).currents
            let states, rules = NFASimulation.discoverStatesAndRules (Set.ofList [ start ]) simulation

            let states = Set.toList states
            Expect.equal (List.length states) 4 ""
            let actual = List.item 0 states
            let expect = Set.ofList []
            Expect.equal actual expect ""
            let actual = List.item 1 states
            let expect = Set.ofList [ 1; 2 ]
            Expect.equal actual expect ""
            let actual = List.item 2 states
            let expect = Set.ofList [ 3; 1; 2 ]
            Expect.equal actual expect ""
            let actual = List.item 3 states
            let expect = Set.ofList [ 3; 2 ]
            Expect.equal actual expect ""

            let expect =
                [ FARule.create (Set.ofList [ 1; 2 ]) 'a' (Set.ofList [ 1; 2 ])
                  FARule.create (Set.ofList [ 1; 2 ]) 'b' (Set.ofList [ 3; 2 ])
                  FARule.create (Set.ofList [ 3; 2 ]) 'a' (Set.ofList [])
                  FARule.create (Set.ofList [ 3; 2 ]) 'b' (Set.ofList [ 1; 2; 3 ])
                  FARule.create (Set.ofList []) 'a' (Set.ofList [])
                  FARule.create (Set.ofList []) 'b' (Set.ofList [])
                  FARule.create (Set.ofList [ 1; 2; 3 ]) 'a' (Set.ofList [ 1; 2 ])
                  FARule.create (Set.ofList [ 1; 2; 3 ]) 'b' (Set.ofList [ 1; 2; 3 ]) ]
                |> List.sort
            Expect.equal rules expect ""
        }

    [<Tests>]
    let ``convert nfa to dfa`` =
        test "convert nfa to dfa" {

            let design = NFASimulation.toDFADesign simulation
            let actual = DFADesign.accepts "aaa" design
            Expect.isFalse actual ""
            let actual = DFADesign.accepts "aab" design
            Expect.isTrue actual ""
            let actual = DFADesign.accepts "bbbabb" design
            Expect.isTrue actual ""
        }
