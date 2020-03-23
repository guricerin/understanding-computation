module UnderstandingComputation.Chap2.StatementTest

open Expecto
open UnderstandingComputation.Chap2
open Expression
open Statement

[<Tests>]
let ``reduce assign`` =
    test "reduce assign" {
        let stmt = Assign("x", Expr.Add(Expr.Variable "x", Expr.Number 1))
        Expect.equal "<<x = x + 1>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let env = Env.ofList [ ("x", Expr.Number 2) ]
        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<x = 2 + 1>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<x = 3>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<do-nothing>>" (Stmt.inspect stmt) ""
        Expect.isFalse (Stmt.isReducible stmt) ""
    }

[<Tests>]
let ``reduce if`` =
    test "reduce if" {
        let stmt = If(Expr.Variable "x", Assign("y", Expr.Number 1), Assign("y", Expr.Number 2))
        Expect.equal "<<if (x) { y = 1 } else { y = 2 }>>" (Stmt.inspect stmt) ""

        let env = Env.ofList [ ("x", Expr.Boolean true) ]
        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (true) { y = 1 } else { y = 2 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<y = 1>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<do-nothing>>" (Stmt.inspect stmt) ""
        Expect.isFalse (Stmt.isReducible stmt) ""
    }

[<Tests>]
let ``reduce sequence`` =
    test "reduce sequence" {
        let stmt =
            Sequence
                (Assign("x", Expr.Add(Expr.Number 1, Expr.Number 1)),
                 Assign("y", Expr.Add(Expr.Variable "x", Expr.Number 3)))
        Expect.equal "<<x = 1 + 1; y = x + 3>>" (Stmt.inspect stmt) ""

        let env = Env.empty
        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<x = 2; y = x + 3>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<do-nothing; y = x + 3>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<y = x + 3>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<y = 2 + 3>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<y = 5>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<do-nothing>>" (Stmt.inspect stmt) ""
        Expect.isFalse (Stmt.isReducible stmt) ""
    }

[<Tests>]
let ``reduce while`` =
    test "reduce while" {
        let stmt =
            While
                (Expr.LessThan(Expr.Variable "x", Expr.Number 5),
                 Assign("x", Expr.Multiply(Expr.Variable "x", Expr.Number 3)))
        Expect.equal "<<while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""

        let env = Env.ofList [ ("x", Expr.Number 1) ]
        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (x < 5) { x = x * 3; while (x < 5) { x = x * 3 } } else { do-nothing }>>"
            (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (1 < 5) { x = x * 3; while (x < 5) { x = x * 3 } } else { do-nothing }>>"
            (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (true) { x = x * 3; while (x < 5) { x = x * 3 } } else { do-nothing }>>"
            (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<x = x * 3; while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<x = 1 * 3; while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<x = 3; while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<do-nothing; while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (x < 5) { x = x * 3; while (x < 5) { x = x * 3 } } else { do-nothing }>>"
            (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (3 < 5) { x = x * 3; while (x < 5) { x = x * 3 } } else { do-nothing }>>"
            (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (true) { x = x * 3; while (x < 5) { x = x * 3 } } else { do-nothing }>>"
            (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<x = x * 3; while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<x = 3 * 3; while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<x = 9; while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<do-nothing; while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (x < 5) { x = x * 3; while (x < 5) { x = x * 3 } } else { do-nothing }>>"
            (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (9 < 5) { x = x * 3; while (x < 5) { x = x * 3 } } else { do-nothing }>>"
            (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<if (false) { x = x * 3; while (x < 5) { x = x * 3 } } else { do-nothing }>>"
            (Stmt.inspect stmt) ""
        Expect.isTrue (Stmt.isReducible stmt) ""

        let stmt, env = Stmt.reduce stmt env
        Expect.equal "<<do-nothing>>" (Stmt.inspect stmt) ""
        Expect.isFalse (Stmt.isReducible stmt) ""
    }

[<Tests>]
let ``eval sequence`` =
    test "eval sequence" {
        let stmt =
            Sequence
                (Assign("x", Expr.Add(Expr.Number 1, Expr.Number 1)),
                 Assign("y", Expr.Add(Expr.Variable "x", Expr.Number 3)))
        Expect.equal "<<x = 1 + 1; y = x + 3>>" (Stmt.inspect stmt) ""

        let env = Env.empty
        let expr, env = Stmt.evaluate stmt env
        Expect.equal expr (Expr.Number 5) ""
        let actual = Env.tryFind "x" env
        let expect = Some(Expr.Number 2)
        Expect.equal actual expect ""
        let actual = Env.tryFind "y" env
        let expect = Some(Expr.Number 5)
        Expect.equal actual expect ""
    }

[<Tests>]
let ``eval while`` =
    test "eval while" {
        let stmt =
            While
                (Expr.LessThan(Expr.Variable "x", Expr.Number 5),
                 Assign("x", Expr.Multiply(Expr.Variable "x", Expr.Number 3)))
        Expect.equal "<<while (x < 5) { x = x * 3 }>>" (Stmt.inspect stmt) ""

        let env = Env.ofList [ ("x", Expr.Number 1) ]
        let expr, env = Stmt.evaluate stmt env
        Expect.equal expr (Expr.Boolean false) ""
        let actual = Env.tryFind "x" env
        let expect = Some(Expr.Number 9)
        Expect.equal actual expect ""
    }

[<Tests>]
let ``toRuby stmt`` =
    test "toRuby stmt" {
        let stmt = Assign("y", Expr.Add(Expr.Variable "x", Expr.Number 1))
        let actual = Stmt.toRuby stmt
        let expect = "-> e { e.merge({ :y => (-> e { (-> e { e[:x] }).call(e) + (-> e { 1 }).call(e) }).call(e) }) }"
        Expect.equal actual expect ""

        let stmt =
            While
                (Expr.LessThan(Expr.Variable "x", Expr.Number 5),
                 Assign("x", Expr.Multiply(Expr.Variable "x", Expr.Number 3)))
        let actual = Stmt.toRuby stmt
        let expect =
            "-> e { while (-> e { (-> e { e[:x] }).call(e) < (-> e { 5 }).call(e) }).call(e); e = (-> e { e.merge({ :x => (-> e { (-> e { e[:x] }).call(e) * (-> e { 3 }).call(e) }).call(e) }) }).call(e); end; e }"
        Expect.equal actual expect ""
    }
