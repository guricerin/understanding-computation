module UnderstandingComputation.Chap9.SignTest

#nowarn "0035" // for scoped union

open Expecto
open UnderstandingComputation.Chap9
open Sign

[<Tests>]
let ``abstract multiply`` =
    test "abstract multiply" {
        let actual = Sign.Possible * Sign.Possible
        let expect = Sign.Possible
        Expect.equal actual expect ""
    }

[<Tests>]
let ``abstract add`` =
    test "abstract add" {
        let actual = Sign.Possible + Sign.Possible
        let expect = Sign.Possible
        Expect.equal actual expect ""

        let actual = Sign.Negative + Sign.Zero
        let expect = Sign.Negative
        Expect.equal actual expect ""

        let actual = Sign.Negative + Sign.Possible
        let expect = Sign.Unknown
        Expect.equal actual expect ""

        let actual = (Sign.Possible + Sign.Negative) * Sign.Zero + Sign.Possible
        let expect = Sign.Possible
        Expect.equal actual expect ""
    }

[<Tests>]
let ``abstract lte`` =
    test "abstract lte" {
        let actual = Sign.Possible <= Sign.Possible
        Expect.isTrue actual ""

        let actual = Sign.Possible <= Sign.Unknown
        Expect.isTrue actual ""

        let actual = Sign.Possible <= Sign.Negative
        Expect.isFalse actual ""

        let actual = Sign.sign (6 * -9) <= Sign.sign 6 * Sign.sign -9
        Expect.isTrue actual ""

        let actual = Sign.sign (-5 + 0) <= Sign.sign -5 + Sign.sign 0
        Expect.isTrue actual ""

        let actual = Sign.sign (6 + -9) <= Sign.sign 6 + Sign.sign -9
        Expect.isTrue actual ""
    }

[<Tests>]
let ``abstract sign`` =
    test "generic sign" {
        let actual = Sign.sign 6
        let expect = Sign.Possible
        Expect.equal actual expect ""

        let actual = Sign.sign 6.4
        let expect = Sign.Possible
        Expect.equal actual expect ""

        let actual = Sign.sign -9.7
        let expect = Sign.Negative
        Expect.equal actual expect ""

        let actual = (Sign.sign 6) * (Sign.sign -9)
        let expect = Sign.Negative
        Expect.equal actual expect ""

        let actual = Sign.sign (10 + 3) = Sign.sign 10 + Sign.sign 3
        Expect.isTrue actual ""

        let actual = Sign.sign (-5 + 0) = Sign.sign -5 + Sign.sign 0
        Expect.isTrue actual ""

        let actual = Sign.sign (6 + -9) = Sign.sign 6 + Sign.sign -9
        Expect.isFalse actual ""

        let actual = Sign.sign (6 + -9)
        let expect = Sign.Negative
        Expect.equal actual expect ""

        let actual = Sign.sign 6 + Sign.sign -9
        let expect = Sign.Unknown
        Expect.equal actual expect ""
    }

[<Tests>]
let ``calculate`` =
    test "calculate" {
        let actual = Sign.calculate 3 -5 0
        Expect.equal actual 0 ""

        let actual = Sign.calculate Sign.Possible Sign.Negative Sign.Zero
        Expect.equal actual Sign.Zero ""
    }

[<Tests>]
let ``sum of squares`` =
    test "sum of squares" {
        let inputs = [ Sign.Negative; Sign.Zero; Sign.Possible ]

        let outputs =
            [ for x in inputs do
                for y in inputs do
                    Sign.sumOfSquares x y ]
            |> List.distinct

        let expect = [ Sign.Possible; Sign.Zero ]
        Expect.equal outputs expect ""
    }
