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
        let expect = While(LessThan(Variable "x", Number 5), Assign("x", Multiply(Variable "x", Number 3)))
        Expect.equal stmt expect ""
        Expect.equal "<<while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""

        let env = Env.ofList [ ("x", Number 1) ]
        let expr, env = Stmt.evaluate stmt env
        Expect.equal expr (Boolean false) ""
        let actual = Env.tryFind "x" env
        let expect = Some(Number 9)
        Expect.equal actual expect ""

        let actual = Stmt.toRuby stmt
        let expect =
            "-> e { while (-> e { (-> e { e[:x] }).call(e) < (-> e { 5 }).call(e) }).call(e); e = (-> e { e.merge({ :x => (-> e { (-> e { e[:x] }).call(e) * (-> e { 3 }).call(e) }).call(e) }) }).call(e); end; e }"
        Expect.equal actual expect ""
    }
