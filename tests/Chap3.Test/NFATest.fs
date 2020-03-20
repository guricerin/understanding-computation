module UnderstandingComputation.Chap3.NFATest

open Expecto
open UnderstandingComputation.Chap3
open Automaton
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
            let actual = NFARulebook.nextStates (States.ofList [ 1 ]) (Some 'b') rulebook
            let expect = States.ofList [ 1; 2 ]
            Expect.equal actual expect ""

            let actual = NFARulebook.nextStates (States.ofList [ 1; 2 ]) (Some 'a') rulebook
            let expect = States.ofList [ 1; 3 ]
            Expect.equal actual expect ""

            let actual = NFARulebook.nextStates (States.ofList [ 1; 3 ]) (Some 'b') rulebook
            let expect = States.ofList [ 1; 2; 4 ]
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
            let actual = NFARulebook.nextStates (States.ofList [ 1 ]) None rulebook
            let expect = States.ofList [ 2; 4 ]
            Expect.equal actual expect ""

            let actual = NFARulebook.followFreeMoves (States.ofList [ 1 ]) rulebook
            let expect = States.ofList [ 1; 2; 4 ]
            Expect.equal actual expect ""
        }

    [<Tests>]
    let `` nfa accepts`` =
        test "nfa accepts" {
            let nfa = NFA.create [ 1 ] [ 2; 4 ] rulebook
            let actual = NFA.accepts "aa" nfa
            Expect.isTrue actual ""
            let actual = NFA.accepts "aaa" nfa
            Expect.isTrue actual ""
            let actual = NFA.accepts "aaaaa" nfa
            Expect.isFalse actual ""
            let actual = NFA.accepts "aaaaaa" nfa
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

    [<Tests>]
    let ``nfa simulation`` =
        test "nfa simulation" {
            let orgNFA = NFA.create [ 1 ] [ 3 ] rulebook
            let simulation = NFASimulation.create orgNFA

            let nfa = NFASimulation.toNFA (States.ofList []) simulation
            let actual = (NFA.freeMove nfa).currents
            let expect = States.ofList [ 1; 2 ]
            Expect.equal actual expect ""

            let nfa = NFASimulation.toNFA (States.ofList [ 2 ]) simulation
            let actual = (NFA.freeMove nfa).currents
            let expect = States.ofList [ 2 ]
            Expect.equal actual expect ""

            let nfa = NFASimulation.toNFA (States.ofList [ 3 ]) simulation
            let actual = (NFA.freeMove nfa).currents
            let expect = States.ofList [ 2; 3 ]
            Expect.equal actual expect ""

            let nfa = NFASimulation.toNFA (States.ofList [ 2; 3 ]) simulation
            let nfa = NFA.readChar (Some 'b') nfa
            let actual = (NFA.freeMove nfa).currents
            let expect = States.ofList [ 1; 2; 3 ]
            Expect.equal actual expect ""
        }

    [<Tests>]
    let ``nfa simulation nextStates`` =
        test "nfa simulation nextStates" {
            let orgNFA = NFA.create [ 1 ] [ 3 ] rulebook
            let simulation = NFASimulation.create orgNFA

            let actual = NFASimulation.nextStates (States.ofList [ 1; 2 ]) 'a' simulation
            let expect = States.ofList [ 1; 2 ]
            Expect.equal actual expect ""

            let actual = NFASimulation.nextStates (States.ofList [ 1; 2 ]) 'b' simulation
            let expect = States.ofList [ 3; 2 ]
            Expect.equal actual expect ""

            let actual = NFASimulation.nextStates (States.ofList [ 2; 3 ]) 'b' simulation
            let expect = States.ofList [ 1; 3; 2 ]
            Expect.equal actual expect ""

            let actual = NFASimulation.nextStates (States.ofList [ 1; 2; 3 ]) 'b' simulation
            let expect = States.ofList [ 1; 3; 2 ]
            Expect.equal actual expect ""

            let actual = NFASimulation.nextStates (States.ofList [ 1; 2; 3 ]) 'a' simulation
            let expect = States.ofList [ 1; 2 ]
            Expect.equal actual expect ""
        }
