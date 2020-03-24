module UnderstandingComputation.Chap9.TypeSystemTest

#nowarn "0035" // for scoped union

open Expecto
open UnderstandingComputation.Chap2
open Expression
open Statement
open UnderstandingComputation.Chap9
open TypeSystem

[<Tests>]
let ``expr type`` =
    test "expr type" {
        let context = TypeContext.empty
        let actual = Expr.Add(Expr.Number 1, Expr.Number 2) |> exprType context
        let expect = Type.Number
        Expect.equal actual expect ""

        Expect.throwsT<TypeMismatchException> (fun _ ->
            Expr.Add(Expr.Number 1, Expr.Boolean true)
            |> exprType context
            |> ignore) ""

        let actual = Expr.LessThan(Expr.Number 1, Expr.Number 2) |> exprType context
        let expect = Type.Boolean
        Expect.equal actual expect ""

        Expect.throwsT<TypeMismatchException> (fun _ ->
            Expr.LessThan(Expr.Number 1, Expr.Boolean true)
            |> exprType context
            |> ignore) ""
    }

[<Tests>]
let ``type context`` =
    test "type context" {
        let context = TypeContext.empty
        let expr = Expr.Add(Expr.Variable "x", Expr.Variable "y")
        let context = TypeContext.add "x" Type.Number context
        let context = TypeContext.add "y" Type.Number context
        let actual = exprType context expr
        let expect = Type.Number
        Expect.equal actual expect ""
    }

[<Tests>]
let ``stmt type`` =
    test "stmt type" {
        let context = TypeContext.empty
        let stmt = Stmt.If(Expr.LessThan(Expr.Number 1, Expr.Number 2), Stmt.DoNothing, Stmt.DoNothing)
        let actual = stmtType context stmt
        let expect = Type.Void
        Expect.equal actual expect ""

        let stmt = Stmt.If(Expr.Add(Expr.Number 1, Expr.Number 2), Stmt.DoNothing, Stmt.DoNothing)
        Expect.throwsT<TypeMismatchException> (fun _ ->
            stmt
            |> stmtType context
            |> ignore) ""

        let stmt = Stmt.While(Expr.Variable "x", Stmt.DoNothing)
        let context = TypeContext.ofList [ ("x", Type.Boolean) ]
        let actual = stmtType context stmt
        let expect = Type.Void
        Expect.equal actual expect ""

        let context = TypeContext.ofList [ ("x", Type.Number) ]
        Expect.throwsT<TypeMismatchException> (fun _ ->
            stmt
            |> stmtType context
            |> ignore) ""

        let stmt =
            Stmt.While
                (Expr.LessThan(Expr.Variable "x", Expr.Number 5),
                 Stmt.Assign("x", Expr.Add(Expr.Variable "x", Expr.Number 3)))
        let context = TypeContext.empty
        Expect.throwsT<UntypedVariableException> (fun _ ->
            stmt
            |> stmtType context
            |> ignore) ""

        let context = TypeContext.ofList [ ("x", Type.Number) ]
        let actual = stmtType context stmt
        let expect = Type.Void
        Expect.equal actual expect ""

        let context = TypeContext.ofList [ ("x", Type.Boolean) ]
        Expect.throwsT<TypeMismatchException> (fun _ ->
            stmt
            |> stmtType context
            |> ignore) ""
    }

[<Tests>]
let ``infinite loop`` =
    test "infinite loop" {
        let stmt =
            Stmt.Sequence
                (Stmt.Assign("x", Expr.Number 0),
                 Stmt.While(Expr.Boolean true, Stmt.Assign("x", Expr.Add(Expr.Variable "x", Expr.Number 1))))
        let context = TypeContext.ofList [ ("x", Type.Number) ]
        let actual = stmtType context stmt
        let expect = Type.Void
        Expect.equal actual expect ""

        // 無限ループ
        // let env= Env.empty
        // let actual = Stmt.evaluate stmt env

        let stmt = Stmt.Sequence(stmt, Stmt.Assign("x", Expr.Boolean true))
        Expect.throwsT<TypeMismatchException> (fun _ ->
            stmt
            |> stmtType context
            |> ignore) ""
    }

[<Tests>]
let ``num or bool`` =
    test "num or bool" {
        // 変数xは条件次第でNumber型かBoolean型の値になる文
        let stmt =
            Stmt.Sequence
                (Stmt.If(Expr.Variable "b", Stmt.Assign("x", Expr.Number 6), Stmt.Assign("x", Expr.Boolean true)),
                 Stmt.Sequence
                     (Stmt.If(Expr.Variable "b", Stmt.Assign("y", Expr.Variable "x"), Stmt.Assign("y", Expr.Number 1)),
                      Stmt.Assign("z", Expr.Add(Expr.Variable "y", Expr.Number 1))))
        let env = Env.ofList [ "b", Expr.Boolean true ]
        let expr, env = Stmt.evaluate stmt env

        let expect =
            Env.ofList
                [ "b", Expr.Boolean true
                  "x", Expr.Number 6
                  "y", Expr.Number 6
                  "z", Expr.Number 7 ]
        Expect.equal env expect ""

        let env = Env.ofList [ "b", Expr.Boolean false ]
        let expr, env = Stmt.evaluate stmt env

        let expect =
            Env.ofList
                [ "b", Expr.Boolean false
                  "x", Expr.Boolean true
                  "y", Expr.Number 1
                  "z", Expr.Number 2 ]
        Expect.equal env expect ""

        // 変数bが未定義
        let context = TypeContext.empty
        Expect.throwsT<UntypedVariableException> (fun _ ->
            stmt
            |> stmtType context
            |> ignore) ""

        // 変数xが未定義
        let context =
            TypeContext.ofList
                [ "b", Type.Boolean
                  "y", Type.Number
                  "z", Type.Number ]
        Expect.throwsT<UntypedVariableException> (fun _ ->
            stmt
            |> stmtType context
            |> ignore) ""

        // 変数xの型をNumberに固定
        let context' = TypeContext.add "x" Type.Number context
        Expect.throwsT<TypeMismatchException> (fun _ ->
            stmt
            |> stmtType context'
            |> ignore) ""

        // 変数xの型をBooleanに固定
        let context' = TypeContext.add "x" Type.Boolean context
        Expect.throwsT<TypeMismatchException> (fun _ ->
            stmt
            |> stmtType context'
            |> ignore) ""
    }
