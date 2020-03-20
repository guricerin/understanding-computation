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
let ``empty pattern`` =
    test "empty pattern" {
        let actual = Pattern.matches "a" Empty
        Expect.isFalse actual ""
    }

[<Tests>]
let ``literal pattern`` =
    test "literal pattern" {
        let actual = Pattern.matches "a" (Literal 'a')
        Expect.isTrue actual ""
    }

[<Tests>]
let ``concatenate pattern`` =
    test "concatenate pattern" {
        let pattern = Concat(Literal 'a', Literal 'b')
        Expect.equal (pattern.ToString()) "/ab/" ""
        let actual = Pattern.matches "a" pattern
        Expect.isFalse actual ""
        let actual = Pattern.matches "ab" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "abc" pattern
        Expect.isFalse actual ""
    }

[<Tests>]
let ``choose pattern`` =
    test "choose pattern" {
        let pattern = Choose(Literal 'a', Literal 'b')
        Expect.equal (pattern.ToString()) "/a|b/" ""
        let actual = Pattern.matches "a" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "b" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "c" pattern
        Expect.isFalse actual ""
    }

[<Tests>]
let ``repeat pattern`` =
    test "repeat matches" {
        let pattern = Repeat(Literal 'a')
        Expect.equal (pattern.ToString()) "/a*/" ""
        let actual = Pattern.matches "" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "a" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "aaaa" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "b" pattern
        Expect.isFalse actual ""
    }

[<Tests>]
let ``complex pattern`` =
    test "complex pattern" {
        let pattern = Repeat(Concat(Literal 'a', Choose(Empty, Literal 'b')))
        Expect.equal (pattern.ToString()) "/(a(|b))*/" ""
        let actual = Pattern.matches "" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "a" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "ab" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "aba" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "abab" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "abaab" pattern
        Expect.isTrue actual ""
        let actual = Pattern.matches "abba" pattern
        Expect.isFalse actual ""
    }
