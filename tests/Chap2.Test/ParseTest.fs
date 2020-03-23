module UnderstandingComputation.Chap2.ParseTest

open Expecto
open UnderstandingComputation.Chap2
open Expression
open Statement
open Parser

[<Tests>]
let ``parse while`` =
    test "parse while" {
        let code = "while (x < 5) { x = x * 3 }"
        let stmt = Parsing.parse code
        let expect =
            While
                (Expr.LessThan(Expr.Variable "x", Expr.Number 5),
                 Assign("x", Expr.Multiply(Expr.Variable "x", Expr.Number 3)))
        Expect.equal stmt expect ""
        Expect.equal "<<while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""

        let env = Env.ofList [ ("x", Expr.Number 1) ]
        let expr, env = Stmt.evaluate stmt env
        Expect.equal expr (Expr.Boolean false) ""
        let actual = Env.tryFind "x" env
        let expect = Some(Expr.Number 9)
        Expect.equal actual expect ""

        let actual = Stmt.toRuby stmt
        let expect =
            "-> e { while (-> e { (-> e { e[:x] }).call(e) < (-> e { 5 }).call(e) }).call(e); e = (-> e { e.merge({ :x => (-> e { (-> e { e[:x] }).call(e) * (-> e { 3 }).call(e) }).call(e) }) }).call(e); end; e }"
        Expect.equal actual expect ""
    }
