module UnderstandingComputation.Chap5.DTM

open System
open Expecto
open UnderstandingComputation.Chap5
open TuringMachine
open DTM

module BinaryIncrement =

    let rulebook =
        [ TMRule.create 1 '0' 2 '1' Direction.Right
          TMRule.create 1 '1' 1 '0' Direction.Left
          TMRule.create 1 '_' 2 '1' Direction.Right
          TMRule.create 2 '0' 2 '0' Direction.Right
          TMRule.create 2 '1' 2 '1' Direction.Right
          TMRule.create 2 '_' 3 '_' Direction.Left ]

    [<Tests>]
    let ``dtm rulebook`` =
        test "dtm rulebook" {
            let tape = Tape.create [ '1'; '0'; '1' ] '1' [] '_'
            let config = TMConfig.create 1 tape
            Expect.equal (config.tape.ToString()) "#<Tape 101(1)>" ""

            let config = DTMRulebook.nextConfig config rulebook
            Expect.equal config.state 1 ""
            Expect.equal (config.tape.ToString()) "#<Tape 10(1)0>" ""

            let config = DTMRulebook.nextConfig config rulebook
            Expect.equal config.state 1 ""
            Expect.equal (config.tape.ToString()) "#<Tape 1(0)00>" ""

            let config = DTMRulebook.nextConfig config rulebook
            Expect.equal config.state 2 ""
            Expect.equal (config.tape.ToString()) "#<Tape 11(0)0>" ""
        }

    [<Tests>]
    let ``dtm`` =
        test "dtm" {
            let tape = Tape.create [ '1'; '0'; '1' ] '1' [] '_'
            let config = TMConfig.create 1 tape
            let dtm = DTM.create config (Set.ofList [ 3 ]) rulebook

            Expect.isFalse (DTM.isAccepting dtm) ""

            let dtm = DTM.step dtm
            Expect.equal dtm.config.state 1 ""
            Expect.equal (dtm.config.tape.ToString()) "#<Tape 10(1)0>" ""
            Expect.isFalse (DTM.isAccepting dtm) ""

            let dtm = DTM.run dtm
            Expect.equal dtm.config.state 3 ""
            Expect.equal (dtm.config.tape.ToString()) "#<Tape 110(0)_>" ""
            Expect.isTrue (DTM.isAccepting dtm) ""
        }

    [<Tests>]
    let ``dtm stuck`` =
        test "dtm stuck" {
            let tape = Tape.create [ '1'; '2'; '1' ] '1' [] '_'
            let config = TMConfig.create 1 tape
            let dtm = DTM.create config (Set.ofList [ 3 ]) rulebook

            let dtm = DTM.run dtm
            Expect.isFalse (DTM.isAccepting dtm) ""
            Expect.isTrue (DTM.isStuck dtm) ""
        }

module SameElemNumString =

    let rulebook =
        [ /// 1: aを探して右にスキャン
          TMRule.create 1 'X' 1 'X' Direction.Right
          TMRule.create 1 'a' 2 'X' Direction.Right
          TMRule.create 1 '_' 6 '_' Direction.Left

          /// 2: bを探して右にスキャン
          TMRule.create 2 'a' 2 'a' Direction.Right
          TMRule.create 2 'X' 2 'X' Direction.Right
          TMRule.create 2 'b' 3 'X' Direction.Right

          /// 3: cを探して右にスキャン
          TMRule.create 3 'b' 3 'b' Direction.Right
          TMRule.create 3 'X' 3 'X' Direction.Right
          TMRule.create 3 'c' 4 'X' Direction.Right

          /// 4: 文字列の末尾を探して右にスキャン
          TMRule.create 4 'c' 4 'c' Direction.Right
          TMRule.create 4 '_' 5 '_' Direction.Left

          /// 5: 文字列の先頭を探して左にスキャン
          TMRule.create 5 'a' 5 'a' Direction.Left
          TMRule.create 5 'b' 5 'b' Direction.Left
          TMRule.create 5 'c' 5 'c' Direction.Left
          TMRule.create 5 'X' 5 'X' Direction.Left
          TMRule.create 5 '_' 1 '_' Direction.Right ]

    [<Tests>]
    let ``dtm same elem num string`` =
        test "dtm same elem num string" {
            let tape = Tape.create [] 'a' (Seq.toList "aabbbccc") '_'
            Expect.equal (tape.ToString()) "#<Tape (a)aabbbccc>" ""

            let mutable dtm = DTM.create (TMConfig.create 1 tape) (Set.ofList [ 6 ]) rulebook
            for _ in 1 .. 10 do
                dtm <- DTM.step dtm
            Expect.equal dtm.config.state 5 ""
            Expect.equal (dtm.config.tape.ToString()) "#<Tape XaaXbbXc(c)_>" ""

            for _ in 1 .. 25 do
                dtm <- DTM.step dtm
            Expect.equal dtm.config.state 5 ""
            Expect.equal (dtm.config.tape.ToString()) "#<Tape _XXa(X)XbXXc_>" ""

            let dtm = DTM.run dtm
            Expect.equal dtm.config.state 6 ""
            Expect.equal (dtm.config.tape.ToString()) "#<Tape _XXXXXXXX(X)_>" ""
        }

module CopyHeadToTail =

    let rulebook =
        [ /// 1: テープから最初の文字を読む
          TMRule.create 1 'a' 2 'a' Direction.Right
          TMRule.create 1 'b' 3 'b' Direction.Right
          TMRule.create 1 'c' 4 'c' Direction.Right

          /// 2: 文字列の末尾を探して右にスキャン（aを覚えている）
          TMRule.create 2 'a' 2 'a' Direction.Right
          TMRule.create 2 'b' 2 'b' Direction.Right
          TMRule.create 2 'c' 2 'c' Direction.Right
          TMRule.create 2 '_' 5 'a' Direction.Right

          /// 3: 文字列の末尾を探して右にスキャン（bを覚えている）
          TMRule.create 3 'a' 3 'a' Direction.Right
          TMRule.create 3 'b' 3 'b' Direction.Right
          TMRule.create 3 'c' 3 'c' Direction.Right
          TMRule.create 3 '_' 5 'b' Direction.Right

          /// 4: 文字列の末尾を探して右にスキャン（cを覚えている）
          TMRule.create 4 'a' 4 'a' Direction.Right
          TMRule.create 4 'b' 4 'b' Direction.Right
          TMRule.create 4 'c' 4 'c' Direction.Right
          TMRule.create 4 '_' 5 'c' Direction.Right ]

    [<Tests>]
    let ``dtm copy head to tail`` =
        test "dtm copy head to tail" {
            let tape = Tape.create [] 'b' (List.ofSeq "cbca") '_'
            Expect.equal (tape.ToString()) "#<Tape (b)cbca>" ""

            let dtm = DTM.create (TMConfig.create 1 tape) (Set.ofList [ 5 ]) rulebook
            let dtm = DTM.run dtm
            Expect.equal (dtm.config.tape.ToString()) "#<Tape bcbcab(_)>" ""
        }

module Subroutine =

    // 数をインクリメントする機械を3つ繋げて、数に3を足す機械を構築
    let incrementRules startState returnState =
        let incrementing, finishing, finished = startState, Int32.MinValue + returnState, returnState

        [ TMRule.create incrementing '0' finishing '1' Direction.Right
          TMRule.create incrementing '1' incrementing '0' Direction.Left
          TMRule.create incrementing '_' finishing '1' Direction.Right
          TMRule.create finishing '0' finishing '0' Direction.Right
          TMRule.create finishing '1' finishing '1' Direction.Right
          TMRule.create finishing '_' finished '_' Direction.Left ]

    let addedZero, addedOne, addedTwo, addedThree = 0, 1, 2, 3

    let rulebook =
        incrementRules addedZero addedOne @ incrementRules addedOne addedTwo @ incrementRules addedTwo addedThree

    [<Tests>]
    let ``dtm subroutine`` =
        test "dtm subroutine" {
            let tape = Tape.create (List.ofSeq "101") '1' [] '_'
            Expect.equal (tape.ToString()) "#<Tape 101(1)>" "" // 11

            let dtm = DTM.create (TMConfig.create addedZero tape) (Set.ofList [ addedThree ]) rulebook
            let dtm = DTM.run dtm
            Expect.equal (dtm.config.tape.ToString()) "#<Tape 111(0)_>" "" // 14
        }
