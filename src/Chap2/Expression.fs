module UnderstandingComputation.Chap2.Expression

/// SIMPLEの式
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

[<RequireQualifiedAccess>]
module Expr =

    /// SIMPLEのソースコード文字列として表示
    let inspect (expr: Expr) = expr.ToString() |> sprintf "<<%s>>"

    /// 簡約可能な式か？
    let isReducible =
        function
        | Number _ -> false
        | Boolean _ -> false
        | _ -> true

    /// スモールステップ意味論
    /// 式を簡約し、新たな式を返す
    let rec reduce (expr: Expr) (env: Env) =
        match expr with
        | Number _ -> invalidArg "Numer" "値は簡約できない"
        | Add(Number l, Number r) -> Number(l + r), env
        | Add(l, r) ->
            match isReducible l, isReducible r with
            | true, _ ->
                let l, env = reduce l env
                Add(l, r), env
            | false, true ->
                let r, env = reduce r env
                Add(l, r), env
            | _ -> invalidArg "Add" "unreachable!"
        | Multiply(Number l, Number r) -> Number(l * r), env
        | Multiply(l, r) ->
            match isReducible l, isReducible r with
            | true, _ ->
                let l, env = reduce l env
                Multiply(l, r), env
            | false, true ->
                let r, env = reduce r env
                Multiply(l, r), env
            | _ -> invalidArg "Multiply" "unreachable!"
        | Boolean _ -> invalidArg "Boolean" "値は簡約できない"
        | LessThan(Number l, Number r) -> Boolean(l < r), env
        | LessThan(l, r) ->
            match isReducible l, isReducible r with
            | true, _ ->
                let l, env = reduce l env
                LessThan(l, r), env
            | false, true ->
                let r, env = reduce r env
                LessThan(l, r), env
            | _ -> invalidArg "LessThan" "unreachable!"
        | Variable name ->
            match Env.tryFind name env with
            | Some v -> v, env
            | _ ->
                let msg = sprintf "%sは未定義変数" name
                invalidArg "Valiable: " msg

    let private num (Number n) = n

    /// ビッグステップ意味論
    /// 抽象構文木を走査し、値を返す
    let rec evaluate expr (env: Env) =
        match expr with
        | Number _
        | Boolean _ -> expr, env
        | Variable name ->
            match Env.tryFind name env with
            | Some v -> v, env
            | _ ->
                let msg = sprintf "%sは未定義変数" name
                invalidArg "Valiable: " msg
        | Add(l, r) ->
            let l, _ = evaluate l env
            let r, _ = evaluate r env
            Number(num l + num r), env
        | Multiply(l, r) ->
            let l, _ = evaluate l env
            let r, _ = evaluate r env
            Number(num l * num r), env
        | LessThan(l, r) ->
            let l, _ = evaluate l env
            let r, _ = evaluate r env
            Boolean(num l < num r), env

    /// 表示的意味論
    /// 抽象構文木を、その木が意図した意味を表現する別の言語（ここではRuby）に変換する
    let rec toRuby expr =
        match expr with
        | Number _
        | Boolean _ -> sprintf "-> e { %s }" (expr.ToString())
        | Variable name -> sprintf "-> e { e[:%s] }" name
        | Add(l, r) ->
            let l, r = toRuby l, toRuby r
            sprintf "-> e { (%s).call(e) + (%s).call(e) }" l r
        | Multiply(l, r) ->
            let l, r = toRuby l, toRuby r
            sprintf "-> e { (%s).call(e) * (%s).call(e) }" l r
        | LessThan(l, r) ->
            let l, r = toRuby l, toRuby r
            sprintf "-> e { (%s).call(e) < (%s).call(e) }" l r


// for srtp
type Expr with
    static member inline Inspect(x: Expr) = Expr.inspect x
    static member inline IsReducible(x: Expr) = Expr.isReducible x
    static member inline Reduce(x: Expr, env: Env) = Expr.reduce x env
