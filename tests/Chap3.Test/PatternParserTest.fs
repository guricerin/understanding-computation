module UnderstandingComputation.Chap3.PatternParserTest

open Expecto
open UnderstandingComputation.Chap3
open NFA
open Pattern
open PatternParser

[<Tests>]
let ``pattern parser`` =
    test "pattern parser" {
        let code = "(a(|b))*"
        let actual = PatternParser.parse code
        let expect = Repeat(Concat(Literal 'a', Choose(Empty, Literal 'b')))
        Expect.equal actual expect ""

        let pattern = PatternConverter(actual)
        let actual = pattern.Matches("abaab")
        Expect.isTrue actual ""
        let actual = pattern.Matches("abba")
        Expect.isFalse actual ""
    }
