module UnderstandingComputation.Chap2.Statement

open Expression

/// SIMPLEの文
type Stmt =
    /// プログラムの実行が正しく終了したことを示す
    | DoNothing
    // 代入文
    | Assign of string * Expr
    // else節のない条件文は、altにDoNothingを格納することで再現可能
    | If of Expr * Stmt * Stmt
    /// 複文
    | Sequence of Stmt * Stmt
    | While of Expr * Stmt

    override self.ToString() =
        match self with
        | DoNothing -> "do-nothing"
        | Assign(name, expr) -> sprintf "%s = %s" name (expr.ToString())
        | If(cond, conseq, alt) ->
            let cond, conseq, alt = cond.ToString(), conseq.ToString(), alt.ToString()
            sprintf "if (%s) { %s } else { %s }" cond conseq alt
        | Sequence(first, second) ->
            let first, second = first.ToString(), second.ToString()
            sprintf "%s; %s" first second
        | While(cond, body) ->
            let cond, body = cond.ToString(), body.ToString()
            sprintf "while (%s) { %s }" cond body

[<RequireQualifiedAccess>]
module Stmt =

    let inspect stmt = stmt.ToString() |> sprintf "<<%s>>"

    let isReducible =
        function
        | DoNothing -> false
        | _ -> true

    let rec reduce stmt (env: Env) =
        match stmt with
        | DoNothing -> invalidArg "DoNothing" "unreachable!"
        | Assign(name, expr) ->
            match Expr.isReducible expr with
            | true ->
                let expr, _ = Expr.reduce expr env
                Assign(name, expr), env
            | _ ->
                // 環境を更新
                let env = Env.add name expr env
                (DoNothing, env)
        | If(cond, conseq, alt) ->
            match Expr.isReducible cond with
            | true ->
                let cond, _ = Expr.reduce cond env
                If(cond, conseq, alt), env
            | _ ->
                match cond with
                | Boolean true -> conseq, env
                | Boolean false -> alt, env
                | _ -> invalidArg "If" "条件式がbooleanではない"
        | Sequence(first, second) ->
            match first with
            | DoNothing -> second, env
            | _ ->
                let first, env = reduce first env
                Sequence(first, second), env
        | While(cond, body) ->
            let conseq, alt = Sequence(body, stmt), DoNothing
            If(cond, conseq, alt), env

    let rec evaluate stmt (env: Env) =
        match stmt with
        | Assign(name, expr) ->
            // 環境を更新
            let expr, _ = Expr.evaluate expr env
            let env = Env.add name expr env
            expr, env
        | DoNothing -> Boolean true, env // 適当にbool値を返すことにする
        | If(cond, conseq, alt) ->
            match Expr.evaluate cond env with
            | Boolean true, _ -> evaluate conseq env
            | Boolean false, _ -> evaluate alt env
            | _ -> invalidArg "If" "条件式がbooleanではない"
        | Sequence(first, second) ->
            let _, env = evaluate first env
            evaluate second env
        | While(cond, body) ->
            match Expr.evaluate cond env with
            | Boolean true, _ ->
                // 環境を更新して再帰
                let _, env = evaluate body env
                evaluate stmt env
            | Boolean false, env as res -> res
            | _ -> invalidArg "While" "条件式がbooleanではない"

    let rec toRuby stmt =
        match stmt with
        | DoNothing -> "-> e { e }"
        | Assign(name, expr) ->
            let expr = Expr.toRuby expr
            sprintf "-> e { e.merge({ :%s => (%s).call(e) }) }" name expr
        | If(cond, conseq, alt) ->
            let cond, conseq, alt = Expr.toRuby cond, toRuby conseq, toRuby alt
            sprintf "-> e { if (%s).call(e) then (%s).call(e) else (%s).call(e) end }" cond conseq alt
        | Sequence(first, second) ->
            let first, second = toRuby first, toRuby second
            sprintf "-> e { (%s).call((%s).call(e)) }" second first
        | While(cond, body) ->
            let cond, body = Expr.toRuby cond, toRuby body
            sprintf "-> e { while (%s).call(e); e = (%s).call(e); end; e }" cond body


// for srtp
type Stmt with
    static member inline Inspect(x: Stmt) = Stmt.inspect x
    static member inline IsReducible(x: Stmt) = Stmt.isReducible x
    static member inline Reduce(x: Stmt, env: Env) = Stmt.reduce x env
