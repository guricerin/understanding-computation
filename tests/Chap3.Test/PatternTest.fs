module UnderstandingComputation.Chap3.PatternTest

open Expecto
open UnderstandingComputation.Chap3
open NFA
open Pattern

[<Tests>]
let ``pattern to string`` =
    test "pattern to string" {
        let pattern = Repeat(Choose(Concat(Literal 'a', Literal 'b'), Literal 'a'))
        let actual = pattern.ToString()
        Expect.equal actual "/(ab|a)*/" ""
    }

[<Tests>]
let ``pattern to nfa`` =
    test "pattern to nfa" {
        let nfa = Pattern.toNFA Empty
        let actual = NFA.accepts "" nfa
        Expect.isTrue actual ""
        let actual = NFA.accepts "a" nfa
        Expect.isFalse actual ""

        let nfa = Pattern.toNFA (Literal 'a')
        let actual = NFA.accepts "" nfa
        Expect.isFalse actual ""
        let actual = NFA.accepts "a" nfa
        Expect.isTrue actual ""
        let actual = NFA.accepts "b" nfa
        Expect.isFalse actual ""
    }

[<Tests>]
let ``pattern matches`` =
    test "pattern matches" {
        let actual = Pattern.matches "a" Empty
        Expect.isFalse actual ""

        let actual = Pattern.matches "a" (Literal 'a')
        Expect.isTrue actual ""

        let pattern = Concat(Literal 'a', Literal 'b')
        Expect.equal (pattern.ToString()) "/ab/" ""
        let actual = Pattern.matches "a" pattern
        Expect.isFalse actual ""
        let actual = Pattern.matches "ab" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "abc" pattern
        Expect.isFalse actual ""
    }
