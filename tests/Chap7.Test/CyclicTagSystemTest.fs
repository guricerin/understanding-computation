module UnderstandingComputation.Chap7.CyclicTagSystemTest

open Expecto
open UnderstandingComputation.Chap7
open CyclicTagSystem

[<Tests>]
let ``ctag system`` =
    test "ctag system" {
        let rules =
            [ CTagRule.create "1"
              CTagRule.create "0010"
              CTagRule.create "10" ]

        let rulebook = CTagRulebook.create rules
        let mutable system = CTagSystem.create "11" rulebook
        for _ in 1 .. 16 do
            system <- CTagSystem.step system
        let actual = system.current |> CyclicStr.toStr
        Expect.equal actual "00101" ""

        for _ in 1 .. 20 do
            system <- CTagSystem.step system
        let actual = system.current |> CyclicStr.toStr
        Expect.equal actual "101" ""
    }

module Encoder =

    open TagSystem

    let rules =
        [ TagRule.create 'a' "ccdd"
          TagRule.create 'b' "dd" ]

    let rulebook = TagRulebook.create 2 rules
    let system = TagSystem.create "aabbbb" rulebook
    let encoder = CTagEncoder(system)

    [<Tests>]
    let ``ctag encoder`` =
        test "ctag encoder" {
            Expect.equal (encoder.Alphabet) "abcd" ""

            let actual = encoder.EncodeChar 'c'
            Expect.equal actual "0010" ""

            let actual = encoder.EncodeString "cab"
            Expect.equal actual "001010000100" ""
        }

    [<Tests>]
    let ``ctag to-ctag-rule`` =
        test "ctag to-ctag-rule" {
            let rule = rules |> List.head
            let crule = encoder.ToCTagRule(rule)
            let actual = crule.appends |> CyclicStr.toStr
            Expect.equal actual "0010001000010001" ""
        }

    [<Tests>]
    let ``ctag crules`` =
        test "ctag crules" {
            let crules = encoder.CRules

            let rule = List.item 0 crules
            let actual = rule.appends |> CyclicStr.toStr
            Expect.equal actual "0010001000010001" ""

            let rule = List.item 1 crules
            let actual = rule.appends |> CyclicStr.toStr
            Expect.equal actual "00010001" ""

            let rule = List.item 2 crules
            let actual = rule.appends |> CyclicStr.toStr
            Expect.equal actual "" ""

            let rule = List.item 3 crules
            let actual = rule.appends |> CyclicStr.toStr
            Expect.equal actual "" ""
        }

    [<Tests>]
    let ``ctag padding rules`` =
        test "ctag padding rules" {
            let padding = encoder.CyclicPaddingRules
            let emptyrule = CTagRule.create ""
            let actual = List.forall (fun rule -> rule = emptyrule) padding
            Expect.isTrue actual ""
        }

    [<Tests>]
    let ``ctag encode`` =
        test "ctag encode" {
            let csystem = encoder.CTagSystem
            let csystem = CTagSystem.run csystem // 停止する
            Expect.equal (csystem.current |> CyclicStr.toStr) "" ""
        }
