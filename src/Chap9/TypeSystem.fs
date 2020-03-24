module UnderstandingComputation.Chap9.TypeSystem

open UnderstandingComputation.Chap2
open Expression
open Statement

/// SIMPLEの文・式の型
[<RequireQualifiedAccess>]
type Type =
    | Number
    | Boolean
    | Void

    override self.ToString() =
        match self with
        | Number -> "number"
        | Boolean -> "boolean"
        | Void -> "void"
        |> sprintf "#<Type %s>"

/// 変数名とその型
type TypeContext = TypeContext of Map<string, Type>

[<RequireQualifiedAccess>]
module TypeContext =

    let empty = Map.empty |> TypeContext

    let ofList ls = Map.ofList ls |> TypeContext

    let add name ty (TypeContext context) = Map.add name ty context |> TypeContext

    let tryFind name (TypeContext context) = Map.tryFind name context

exception TypeMismatchException of string

exception UntypedVariableException of string

/// SIMPLEの式の型を判別
let rec exprType (context: TypeContext) (expr: Expr): Type =
    let f = exprType context
    match expr with
    | Expr.Number _ -> Type.Number
    | Expr.Boolean _ -> Type.Boolean
    | Expr.Add(l, r) ->
        match f l, f r with
        | Type.Number, Type.Number -> Type.Number
        | _ ->
            let msg = sprintf "Add: type error. left = %s, right = %s" (l.ToString()) (r.ToString())
            raise <| TypeMismatchException(msg)
    | Expr.LessThan(l, r) ->
        match f l, f r with
        | Type.Number, Type.Number -> Type.Boolean
        | _ ->
            let msg = sprintf "LessThan: type error. left = %s, right = %s" (l.ToString()) (r.ToString())
            raise <| TypeMismatchException(msg)
    | Expr.Multiply(l, r) ->
        match f l, f r with
        | Type.Number, Type.Number -> Type.Number
        | _ ->
            let msg = sprintf "Multiply: type error. left = %s, right = %s" (l.ToString()) (r.ToString())
            raise <| TypeMismatchException(msg)
    | Expr.Variable name ->
        match TypeContext.tryFind name context with
        | Some ty -> ty
        | None -> raise <| UntypedVariableException(name)

/// SIMPLEの文の型を判別
let rec stmtType (context: TypeContext) (stmt: Stmt): Type =
    let f = stmtType context
    match stmt with
    | Stmt.DoNothing -> Type.Void
    // 個々の変数の型は事前に決まっていて変わることがない（シャドウイング不可）という実装方針
    | Stmt.Assign(name, expr) ->
        match TypeContext.tryFind name context, exprType context expr with
        | Some nameTy, exprTy when nameTy = exprTy -> Type.Void
        | Some nameTy, exprTy when nameTy <> exprTy ->
            let msg = sprintf "Assign: type error. name = %s, expr = %s" (nameTy.ToString()) (exprTy.ToString())
            raise <| TypeMismatchException(msg)
        | _ -> raise <| UntypedVariableException(name)
    | Stmt.If(cond, conseq, alt) ->
        match exprType context cond, f conseq, f alt with
        | Type.Boolean, Type.Void, Type.Void -> Type.Void
        | condTy, conseqTy, altTy ->
            let msg =
                sprintf "If: type error. cond = %s, conseq = %s, alt = %s" (condTy.ToString()) (conseqTy.ToString())
                    (altTy.ToString())
            raise <| TypeMismatchException(msg)
    | Stmt.Sequence(first, second) ->
        match f first, f second with
        | Type.Void, Type.Void -> Type.Void
        | l, r ->
            let msg = sprintf "Sequence: type error. first = %s, second = %s" (l.ToString()) (r.ToString())
            raise <| TypeMismatchException(msg)
    | Stmt.While(cond, body) ->
        match exprType context cond, f body with
        | Type.Boolean, Type.Void -> Type.Void
        | condTy, bodyTy ->
            let msg = sprintf "While: type error. cond = %s, body = %s" (condTy.ToString()) (bodyTy.ToString())
            raise <| TypeMismatchException(msg)
