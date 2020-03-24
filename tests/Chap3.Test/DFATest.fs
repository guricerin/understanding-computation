module UnderstandingComputation.Chap3.DFATest

open Expecto
open UnderstandingComputation.Chap3
open Automaton
open DFA

module DFATest =
    let ls =
        [ FARule.create 1 'a' 2
          FARule.create 1 'b' 1
          FARule.create 2 'a' 2
          FARule.create 2 'b' 3
          FARule.create 3 'a' 3
          FARule.create 3 'b' 3 ]

    let rulebook = DFARulebook.ofList ls

    [<Tests>]
    let ``dfa rulebook`` =
        test "dfa rulebook" {
            let actual = DFARulebook.nextState 1 'a' rulebook
            let expect = 2
            Expect.equal actual expect ""
            let actual = DFARulebook.nextState 1 'b' rulebook
            let expect = 1
            Expect.equal actual expect ""
            let actual = DFARulebook.nextState 2 'b' rulebook
            let expect = 3
            Expect.equal actual expect ""
        }

    [<Tests>]
    let ``dfa`` =
        test "dfa" {
            let dfa = DFA.create 1 [ 1; 3 ] rulebook
            let actual = DFA.isAccepting dfa
            Expect.isTrue actual ""
            let dfa = DFA.create 1 [ 3 ] rulebook
            let actual = DFA.isAccepting dfa
            Expect.isFalse actual ""
        }

    [<Tests>]
    let ``dfa readChar`` =
        test "dfa readChar" {
            let dfa = DFA.create 1 [ 3 ] rulebook
            let dfa = DFA.readChar 'b' dfa
            Expect.isFalse (DFA.isAccepting dfa) ""
            let mutable dfa = dfa
            for _ in 0 .. 2 do
                dfa <- DFA.readChar 'a' dfa
                Expect.isFalse (DFA.isAccepting dfa) ""
            let dfa = DFA.readChar 'b' dfa
            Expect.isTrue (DFA.isAccepting dfa) ""
        }

    [<Tests>]
    let ``dfa readStr`` =
        test "dfa readStr" {
            let dfa = DFA.create 1 [ 3 ] rulebook
            let dfa = DFA.readString "baaab" dfa
            Expect.isTrue (DFA.isAccepting dfa) ""
        }
