module UnderstandingComputation.Chap2.ExpressionTest

open Expecto
open UnderstandingComputation.Chap2.Expression

[<Tests>]
let ``reduce number`` =
    test "reduce number" {
        let env = Env.empty
        let expr = Add(Multiply(Number(1), Number(2)), Multiply(Number(3), Number(4)))
        Expect.equal "<<1 * 2 + 3 * 4>>" (Expr.inspect expr) ""
        Expect.isTrue (Expr.isReducible expr) ""

        let expr, _ = Expr.reduce expr env
        Expect.equal "<<2 + 3 * 4>>" (Expr.inspect expr) ""
        Expect.isTrue (Expr.isReducible expr) ""

        let expr, _ = Expr.reduce expr env
        Expect.equal "<<2 + 12>>" (Expr.inspect expr) ""
        Expect.isTrue (Expr.isReducible expr) ""

        let expr, _ = Expr.reduce expr env
        Expect.equal "<<14>>" (Expr.inspect expr) ""
        Expect.isFalse (Expr.isReducible expr) ""
    }

[<Tests>]
let ``reduce lessThan`` =
    test "reduce lessThan" {
        let expr = LessThan(Number(5), Add(Number(2), Number(2)))
        Expect.equal "<<5 < 2 + 2>>" (Expr.inspect expr) ""
        Expect.isTrue (Expr.isReducible expr) ""

        let env = Env.empty
        let expr, _ = Expr.reduce expr env
        Expect.equal "<<5 < 4>>" (Expr.inspect expr) ""
        Expect.isTrue (Expr.isReducible expr) ""

        let expr, _ = Expr.reduce expr env
        Expect.equal "<<false>>" (Expr.inspect expr) ""
        Expect.isFalse (Expr.isReducible expr) ""
    }

[<Tests>]
let ``reduce variable`` =
    test "reduce variable" {
        let expr = Add(Variable("x"), Variable("y"))
        Expect.equal "<<x + y>>" (Expr.inspect expr) ""
        Expect.isTrue (Expr.isReducible expr) ""

        let env =
            Env.ofList
                [ ("x", Number 3)
                  ("y", Number 4) ]

        let expr, _ = Expr.reduce expr env
        Expect.equal "<<3 + y>>" (Expr.inspect expr) ""
        Expect.isTrue (Expr.isReducible expr) ""

        let expr, _ = Expr.reduce expr env
        Expect.equal "<<3 + 4>>" (Expr.inspect expr) ""
        Expect.isTrue (Expr.isReducible expr) ""

        let expr, _ = Expr.reduce expr env
        Expect.equal "<<7>>" (Expr.inspect expr) ""
        Expect.isFalse (Expr.isReducible expr) ""
    }

[<Tests>]
let ``eval`` =
    test "eval" {
        let expr = Number 23
        let env = Env.empty
        let expr, _ = Expr.evaluate expr env
        Expect.equal "<<23>>" (Expr.inspect expr) ""

        let expr = Variable "x"
        let env = Env.ofList [ ("x", Number 23) ]
        let expr, _ = Expr.evaluate expr env
        Expect.equal "<<23>>" (Expr.inspect expr) ""

        let expr = LessThan(Add(Variable "x", Number 2), Variable "y")

        let env =
            Env.ofList
                [ ("x", Number 2)
                  ("y", Number 5) ]

        let expr, _ = Expr.evaluate expr env
        Expect.equal "<<true>>" (Expr.inspect expr) ""
    }

[<Tests>]
let ``toRuby expr`` =
    test "toRuby expr" {
        let expr = Number 5
        let actual = Expr.toRuby expr
        Expect.equal actual "-> e { 5 }" ""

        let expr = Boolean false
        let actual = Expr.toRuby expr
        Expect.equal actual "-> e { false }" ""

        let expr = Variable "x"
        let actual = Expr.toRuby expr
        Expect.equal actual "-> e { e[:x] }" ""

        let expr = Add(Variable "x", Number 1)
        let actual = Expr.toRuby expr
        Expect.equal actual "-> e { (-> e { e[:x] }).call(e) + (-> e { 1 }).call(e) }" ""

        let expr = LessThan(Add(Variable "x", Number 1), Number 3)
        let actual = Expr.toRuby expr
        Expect.equal actual
            "-> e { (-> e { (-> e { e[:x] }).call(e) + (-> e { 1 }).call(e) }).call(e) < (-> e { 3 }).call(e) }" ""
    }
