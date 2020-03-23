module UnderstandingComputation.Chap9.TypeSystemTest

#nowarn "0035" // for scoped union

open Expecto
open UnderstandingComputation.Chap2
open Expression
open UnderstandingComputation.Chap9
open TypeSystem

[<Tests>]
let ``expr type`` =
    test "expr type" {
        let actual = Expr.Add(Expr.Number 1, Expr.Number 2) |> exprType
        let expect = Type.Number
        Expect.equal actual expect ""
        Expect.throws (fun _ ->
            Expr.Add(Expr.Number 1, Expr.Boolean true)
            |> exprType
            |> ignore) ""
    }
