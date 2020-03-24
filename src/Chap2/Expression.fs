module UnderstandingComputation.Chap2.Expression

/// SIMPLEの式
[<RequireQualifiedAccess>]
type Expr =
    | Number of int
    | Add of Expr * Expr
    | Multiply of Expr * Expr
    | Boolean of bool
    | LessThan of Expr * Expr
    | Variable of string

    override self.ToString() =
        match self with
        | Number n -> n.ToString()
        | Add(l, r) -> sprintf "%s + %s" (l.ToString()) (r.ToString())
        | Multiply(l, r) -> sprintf "%s * %s" (l.ToString()) (r.ToString())
        | Boolean b -> b.ToString().ToLower()
        | LessThan(l, r) -> sprintf "%s < %s" (l.ToString()) (r.ToString())
        | Variable name -> name

type Env = Env of Map<string, Expr>

[<RequireQualifiedAccess>]
module Env =

    let empty = Env(Map.empty)

    let add name value (Env env) = Env(Map.add name value env)

    let ofList ls = Env(Map.ofList ls)

    let tryFind name (Env env) = Map.tryFind name env

exception UnboundVariableException of string

[<RequireQualifiedAccess>]
module Expr =

    /// SIMPLEのソースコード文字列として表示
    let inspect (expr: Expr) = expr.ToString() |> sprintf "<<%s>>"

    /// 簡約可能な式か？
    let isReducible =
        function
        | Expr.Number _ -> false
        | Expr.Boolean _ -> false
        | _ -> true

    /// スモールステップ意味論
    /// 式を簡約し、新たな式を返す
    let rec reduce (expr: Expr) (env: Env) =
        match expr with
        | Expr.Number _ -> invalidArg "Numer" "value is not reducible"
        | Expr.Add(Expr.Number l, Expr.Number r) -> Expr.Number(l + r), env
        | Expr.Add(l, r) ->
            match isReducible l, isReducible r with
            | true, _ ->
                let l, env = reduce l env
                Expr.Add(l, r), env
            | false, true ->
                let r, env = reduce r env
                Expr.Add(l, r), env
            | _ -> invalidArg "Add" "unreachable!"
        | Expr.Multiply(Expr.Number l, Expr.Number r) -> Expr.Number(l * r), env
        | Expr.Multiply(l, r) ->
            match isReducible l, isReducible r with
            | true, _ ->
                let l, env = reduce l env
                Expr.Multiply(l, r), env
            | false, true ->
                let r, env = reduce r env
                Expr.Multiply(l, r), env
            | _ -> invalidArg "Multiply" "unreachable!"
        | Expr.Boolean _ -> invalidArg "Boolean" "value is not reducible"
        | Expr.LessThan(Expr.Number l, Expr.Number r) -> Expr.Boolean(l < r), env
        | Expr.LessThan(l, r) ->
            match isReducible l, isReducible r with
            | true, _ ->
                let l, env = reduce l env
                Expr.LessThan(l, r), env
            | false, true ->
                let r, env = reduce r env
                Expr.LessThan(l, r), env
            | _ -> invalidArg "LessThan" "unreachable!"
        | Expr.Variable name ->
            match Env.tryFind name env with
            | Some v -> v, env
            | _ -> raise <| UnboundVariableException(name)

    let private num n =
        match n with
        | Expr.Number n -> n
        | _ -> invalidArg "n" "expected Number"

    /// ビッグステップ意味論
    /// 抽象構文木を走査し、値を返す
    let rec evaluate expr (env: Env) =
        match expr with
        | Expr.Number _
        | Expr.Boolean _ -> expr, env
        | Expr.Variable name ->
            match Env.tryFind name env with
            | Some v -> v, env
            | _ ->
                let msg = sprintf "%sは未定義変数" name
                invalidArg "Valiable: " msg
        | Expr.Add(l, r) ->
            let l, _ = evaluate l env
            let r, _ = evaluate r env
            Expr.Number(num l + num r), env
        | Expr.Multiply(l, r) ->
            let l, _ = evaluate l env
            let r, _ = evaluate r env
            Expr.Number(num l * num r), env
        | Expr.LessThan(l, r) ->
            let l, _ = evaluate l env
            let r, _ = evaluate r env
            Expr.Boolean(num l < num r), env

    /// 表示的意味論
    /// 抽象構文木を、その木が意図した意味を表現する別の言語（ここではRuby）に変換する
    let rec toRuby expr =
        match expr with
        | Expr.Number _
        | Expr.Boolean _ -> sprintf "-> e { %s }" (expr.ToString())
        | Expr.Variable name -> sprintf "-> e { e[:%s] }" name
        | Expr.Add(l, r) ->
            let l, r = toRuby l, toRuby r
            sprintf "-> e { (%s).call(e) + (%s).call(e) }" l r
        | Expr.Multiply(l, r) ->
            let l, r = toRuby l, toRuby r
            sprintf "-> e { (%s).call(e) * (%s).call(e) }" l r
        | Expr.LessThan(l, r) ->
            let l, r = toRuby l, toRuby r
            sprintf "-> e { (%s).call(e) < (%s).call(e) }" l r


// for SRTP
type Expr with
    static member inline Inspect(x: Expr) = Expr.inspect x
    static member inline IsReducible(x: Expr) = Expr.isReducible x
    static member inline Reduce(x: Expr, env: Env) = Expr.reduce x env
