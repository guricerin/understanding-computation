module UnderstandingComputation.Chap6.ULCParserTest

open Expecto
open UnderstandingComputation.Chap6
open UntypedLambdaCalculus
open ULCParser

[<Tests>]
let ``ulc parse`` =
    test "ulc parse" {
        let code = "-> x { x[x] }[-> y { y }]"
        let expr = ULCParser.parse code
        let expect = LCC(LCF("x", LCC(LCV "x", LCV "x")), LCF("y", LCV "y"))
        Expect.equal expr expect ""
        Expect.equal (expr.ToString()) code ""

        let expr = LCTerm.reduce expr
        let expect = "-> y { y }[-> y { y }]"
        Expect.equal (expr.ToString()) expect ""
    }
