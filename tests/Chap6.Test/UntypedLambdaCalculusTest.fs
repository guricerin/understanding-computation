module UnderstandingComputation.Chap6.UnderstandingComputationTest

open Expecto
open UnderstandingComputation.Chap6
open UntypedLambdaCalculus

let zero = LCF("p", LCF("x", LCV("x")))

let one = LCF("p", LCF("x", LCC(LCV("p"), LCV("x"))))

let increment = LCF("n", LCF("p", LCF("x", LCC(LCV("p"), LCC(LCC(LCV("n"), LCV("p")), LCV("x"))))))

let add = LCF("m", LCF("n", LCC(LCC(LCV("n"), increment), LCV("m"))))

[<Tests>]
let ``lcterm tostring`` =
    test "lcterm tostring" {
        let actual = zero.ToString()
        let expect = "-> p { -> x { x } }"
        Expect.equal actual expect ""

        let actual = one.ToString()
        let expect = "-> p { -> x { p[x] } }"
        Expect.equal actual expect ""

        let actual = increment.ToString()
        let expect = "-> n { -> p { -> x { p[n[p][x]] } } }"
        Expect.equal actual expect ""

        let actual = add.ToString()
        let expect = "-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }"
        Expect.equal actual expect ""
    }

[<Tests>]
let ``lcterm replace`` =
    test "lcterm replace" {
        let expr = LCV("x")
        let expect = "x"
        Expect.equal (expr.ToString()) expect ""

        let expr' = LCExpr.replace "x" (LCF("y", LCV("y"))) expr
        let expect = "-> y { y }"
        Expect.equal (expr'.ToString()) expect ""

        let expr' = LCExpr.replace "z" (LCF("y", LCV("y"))) expr
        let expect = "x"
        Expect.equal (expr'.ToString()) expect ""

        let expr = LCC(LCC(LCC(LCV("a"), LCV("b")), LCV("c")), LCV("b"))
        let expect = "a[b][c][b]"
        Expect.equal (expr.ToString()) expect ""

        let expr' = LCExpr.replace "a" (LCV("x")) expr
        let expect = "x[b][c][b]"
        Expect.equal (expr'.ToString()) expect ""

        let expr' = LCExpr.replace "b" (LCF("x", LCV("x"))) expr
        let expect = "a[-> x { x }][c][-> x { x }]"
        Expect.equal (expr'.ToString()) expect ""
    }

[<Tests>]
let ``replace free variable (not bound variable)`` =
    test "replace free variable (not bound variable)" {
        let expr = LCF("y", LCC(LCV("x"), LCV("y")))
        let expect = "-> y { x[y] }"
        Expect.equal (expr.ToString()) expect ""

        let expr' = LCExpr.replace "x" (LCV("z")) expr
        let expect = "-> y { z[y] }"
        Expect.equal (expr'.ToString()) expect ""

        let expr' = LCExpr.replace "y" (LCV("z")) expr
        let expect = "-> y { x[y] }"
        Expect.equal (expr'.ToString()) expect ""

        let expr = LCC(LCC(LCV "x", LCV "y"), LCF("y", LCC(LCV "y", LCV "x")))
        let expect = "x[y][-> y { y[x] }]"
        Expect.equal (expr.ToString()) expect ""

        let expr' = LCExpr.replace "x" (LCV "z") expr
        let expect = "z[y][-> y { y[z] }]" // 2つのxはどちらも元の式で自由変数
        Expect.equal (expr'.ToString()) expect ""

        let expr' = LCExpr.replace "y" (LCV "z") expr
        let expect = "x[z][-> y { y[x] }]" // 最初のyだけが自由変数
        Expect.equal (expr'.ToString()) expect ""
    }

[<Tests>]
let ``lcterm call`` =
    test "lcterm call" {
        let func = LCF("x", LCF("y", LCC(LCV "x", LCV "y")))
        let expect = "-> x { -> y { x[y] } }"
        Expect.equal (func.ToString()) expect ""

        let arg = LCF("z", LCV "z")
        let expect = "-> z { z }"
        Expect.equal (arg.ToString()) expect ""

        let actual = LCExpr.call arg func
        let expect = "-> y { -> z { z }[y] }"
        Expect.equal (actual.ToString()) expect ""
    }

[<Tests>]
let ``lcterm reduce`` =
    test "lcterm reduce" {
        let mutable expr = LCC(LCC(add, one), one)
        let expr = LCExpr.eval expr
        let expect = "-> p { -> x { p[-> p { -> x { p[x] } }[p][x]] } }"
        Expect.equal (expr.ToString()) expect ""

        let inc, zero = LCV "inc", LCV "zero"
        let expr = LCC(LCC(expr, inc), zero)
        let expect = "-> p { -> x { p[-> p { -> x { p[x] } }[p][x]] } }[inc][zero]"
        Expect.equal (expr.ToString()) expect ""

        let expr = LCExpr.eval expr
        let expect = "inc[inc[zero]]"
        Expect.equal (expr.ToString()) expect ""
    }
