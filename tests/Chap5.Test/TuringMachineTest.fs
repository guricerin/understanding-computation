module UnderstandingComputation.Chap5.TuringmachineTest

open System
open Expecto
open UnderstandingComputation.Chap5
open TuringMachine

[<Tests>]
let ``tape`` =
    test "tape" {
        let tape = Tape.create [ '1'; '0'; '1' ] '1' [] '_'
        Expect.equal (tape.ToString()) "#<Tape 101(1)>" ""

        let actual = Tape.moveLeft tape
        Expect.equal (actual.ToString()) "#<Tape 10(1)1>" ""

        let actual = Tape.write '0' tape
        Expect.equal (actual.ToString()) "#<Tape 101(0)>" ""

        let actual = Tape.moveRight tape
        Expect.equal (actual.ToString()) "#<Tape 1011(_)>" ""

        let actual = Tape.moveRight tape |> Tape.write '0'
        Expect.equal (actual.ToString()) "#<Tape 1011(0)>" ""
    }

[<Tests>]
let ``tmrule`` =
    test "tmrule" {
        let rule = TMRule.create 1 '0' 2 '1' Direction.Right

        let config = TMConfig.create 1 (Tape.create [] '0' [] '_')
        let actual = TMRule.appliesTo config rule
        Expect.isTrue actual ""

        let config = TMConfig.create 1 (Tape.create [] '1' [] '_')
        let actual = TMRule.appliesTo config rule
        Expect.isFalse actual ""

        let config = TMConfig.create 2 (Tape.create [] '0' [] '_')
        let actual = TMRule.appliesTo config rule
        Expect.isFalse actual ""

        let config = TMConfig.create 1 (Tape.create [] '0' [] '_')
        let actual = TMRule.follow config rule
        let expect = TMConfig.create 2 (Tape.create [ '1' ] '_' [] '_')
        Expect.equal actual expect ""
    }
