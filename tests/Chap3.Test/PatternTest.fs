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
        let design = PatternConverter(Empty).ToNFADesign()
        let actual = NFADesign.accepts "" design
        Expect.isTrue actual ""
        let actual = NFADesign.accepts "a" design
        Expect.isFalse actual ""

        let design = PatternConverter(Literal 'a').ToNFADesign()
        let actual = NFADesign.accepts "" design
        Expect.isFalse actual ""
        let actual = NFADesign.accepts "a" design
        Expect.isTrue actual ""
        let actual = NFADesign.accepts "b" design
        Expect.isFalse actual ""
    }

[<Tests>]
let ``empty pattern`` =
    test "empty pattern" {
        let actual = PatternConverter(Empty).Matches "a"
        Expect.isFalse actual ""
    }

[<Tests>]
let ``literal pattern`` =
    test "literal pattern" {
        let actual = PatternConverter(Literal 'a').Matches "a"
        Expect.isTrue actual ""
    }

[<Tests>]
let ``concatenate pattern`` =
    test "concatenate pattern" {
        let pattern = Concat(Literal 'a', Literal 'b')
        Expect.equal (pattern.ToString()) "/ab/" ""
        let pattern = PatternConverter(pattern)
        let actual = pattern.Matches "a"
        Expect.isFalse actual ""
        let actual = pattern.Matches "ab"
        Expect.isTrue actual ""
        let actual = pattern.Matches "abc"
        Expect.isFalse actual ""
    }

[<Tests>]
let ``choose pattern`` =
    test "choose pattern" {
        let pattern = Choose(Literal 'a', Literal 'b')
        Expect.equal (pattern.ToString()) "/a|b/" ""
        let pattern = PatternConverter(pattern)
        let actual = pattern.Matches "a"
        Expect.isTrue actual ""
        let actual = pattern.Matches "b"
        Expect.isTrue actual ""
        let actual = pattern.Matches "c"
        Expect.isFalse actual ""
    }

[<Tests>]
let ``repeat pattern`` =
    test "repeat matches" {
        let pattern = Repeat(Literal 'a')
        Expect.equal (pattern.ToString()) "/a*/" ""
        let pattern = PatternConverter(pattern)
        let actual = pattern.Matches ""
        Expect.isTrue actual ""
        let actual = pattern.Matches "a"
        Expect.isTrue actual ""
        let actual = pattern.Matches "aaaa"
        Expect.isTrue actual ""
        let actual = pattern.Matches "b"
        Expect.isFalse actual ""
    }

[<Tests>]
let ``complex pattern`` =
    test "complex pattern" {
        let pattern = Repeat(Concat(Literal 'a', Choose(Empty, Literal 'b')))
        Expect.equal (pattern.ToString()) "/(a(|b))*/" ""
        let pattern = PatternConverter(pattern)
        let actual = pattern.Matches ""
        Expect.isTrue actual ""
        let actual = pattern.Matches "a"
        Expect.isTrue actual ""
        let actual = pattern.Matches "ab"
        Expect.isTrue actual ""
        let actual = pattern.Matches "aba"
        Expect.isTrue actual ""
        let actual = pattern.Matches "abab"
        Expect.isTrue actual ""
        let actual = pattern.Matches "abaab"
        Expect.isTrue actual ""
        let actual = pattern.Matches "abba"
        Expect.isFalse actual ""
    }
