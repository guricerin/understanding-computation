module UnderstandingComputation.Chap9.TypeSystem

open UnderstandingComputation.Chap2.Expression
open UnderstandingComputation.Chap2.Statement

[<RequireQualifiedAccess>]
type Type =
    | Number
    | Boolean

    override self.ToString() =
        match self with
        | Number -> "number"
        | Boolean -> "boolean"
        |> sprintf "#<Type %s>"

exception TypeMismatchException of string

let rec exprType (expr: Expr): Type =
    match expr with
    | Expr.Number _ -> Type.Number
    | Expr.Boolean _ -> Type.Boolean
    | Expr.Add(l, r) ->
        match exprType l, exprType r with
        | Type.Number, Type.Number -> Type.Number
        | _ ->
            let msg = sprintf "multiply: type error. left = %s, right = %s" (l.ToString()) (r.ToString())
            raise <| TypeMismatchException(msg)
    | Expr.LessThan(l, r) ->
        match exprType l, exprType r with
        | Type.Number, Type.Number -> Type.Boolean
        | _ ->
            let msg = sprintf "lessthan: type error. left = %s, right = %s" (l.ToString()) (r.ToString())
            raise <| TypeMismatchException(msg)
