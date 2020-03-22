module UnderstandingComputation.Chap7.TagSystemTest

open Expecto
open UnderstandingComputation.Chap7
open TagSystem

[<Tests>]
let ``tag system for double number`` =
    test "tag system for double number" {
        let rules =
            [ TagRule.create 'a' "aa"
              TagRule.create 'b' "bbbb" ]

        let rulebook = TagRulebook.create 2 rules
        let system = TagSystem.create "aabbbbbb" rulebook
        let actual = system.current
        let expect = "aabbbbbb" // 3
        Expect.equal actual expect ""

        let system = TagSystem.step system
        Expect.equal system.current "bbbbbbaa" ""

        let system = TagSystem.step system
        Expect.equal system.current "bbbbaabbbb" ""

        let system = TagSystem.step system
        Expect.equal system.current "bbaabbbbbbbb" ""

        let system = TagSystem.step system
        Expect.equal system.current "aabbbbbbbbbbbb" ""
    }

[<Tests>]
let ``run tag system`` =
    test "run tag system" {
        let rules =
            [ TagRule.create 'a' "cc"
              TagRule.create 'b' "dddd" ]

        let rulebook = TagRulebook.create 2 rules
        let system = TagSystem.create "aabbbbbb" rulebook
        let actual = system.current
        let expect = "aabbbbbb" // 3
        Expect.equal actual expect ""

        let system = TagSystem.run system
        let actual = system.current
        let expect = "ccdddddddddddd" // 6
        Expect.equal actual expect ""
    }

[<Tests>]
let ``tag system for half number`` =
    test "tag system for half number" {
        let rules =
            [ TagRule.create 'a' "cc"
              TagRule.create 'b' "d" ]

        let rulebook = TagRulebook.create 2 rules
        let system = TagSystem.create "aabbbbbbbbbbbb" rulebook // 6
        let system = TagSystem.run system
        let actual = system.current
        let expect = "ccdddddd" // 3
        Expect.equal actual expect ""
    }

[<Tests>]
let ``tag system for increment`` =
    test "tag system for increment" {
        let rules =
            [ TagRule.create 'a' "ccdd"
              TagRule.create 'b' "dd" ]

        let rulebook = TagRulebook.create 2 rules
        let system = TagSystem.create "aabbbb" rulebook // 2
        let system = TagSystem.run system
        let actual = system.current
        let expect = "ccdddddd" // 3
        Expect.equal actual expect ""
    }

[<Tests>]
let ``tag system for double and increment`` =
    test "tag system for double and increment" {
        // x * 2 + 1
        let rules =
            [ TagRule.create 'a' "cc"
              TagRule.create 'b' "dddd"
              TagRule.create 'c' "eeff"
              TagRule.create 'd' "ff" ]

        let rulebook = TagRulebook.create 2 rules
        let system = TagSystem.create "aabbbb" rulebook // 2
        let system = TagSystem.run system
        let actual = system.current
        let expect = "eeffffffffff" // 5
        Expect.equal actual expect ""
    }

[<Tests>]
let ``tag system for check parity`` =
    test "tag system for check parity" {
        let rules =
            [ TagRule.create 'a' "cc"
              TagRule.create 'b' "d"
              TagRule.create 'c' "eo"
              TagRule.create 'd' ""
              TagRule.create 'e' "e" ]

        let rulebook = TagRulebook.create 2 rules
        let system = TagSystem.create "aabbbbbbbb" rulebook // 4
        let system = TagSystem.run system
        let actual = system.current
        let expect = "e" // even
        Expect.equal actual expect ""

        let system = TagSystem.create "aabbbbbbbbbb" rulebook // 5
        let system = TagSystem.run system
        let actual = system.current
        let expect = "o" // odd
        Expect.equal actual expect ""
    }
