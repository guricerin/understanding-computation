namespace UnderstandingComputation.Chap2

/// SIMPLEソースコードを評価する抽象機械
[<RequireQualifiedAccess>]
module Machine =

    let inline inspect (code: ^a) = (^a: (static member Inspect: ^a -> string) code)
    let inline isReducible (code: ^a) = (^a: (static member IsReducible: ^a -> bool) code)
    let inline reduce (code: ^a) (env: Env) = (^a: (static member Reduce: ^a * Env -> ^a * Env) (code, env))
    let inline toRuby (code: ^a) = (^a: (static member ToRuby: ^a -> string) code)

    let inline print a (Env env) = sprintf "%s, %A" (inspect a) env |> printfn "%s"

    let inline step (code: ^a) env = reduce code env

    /// 式 or 文を評価しつつ逐次表示
    let inline run (code: ^a) env =
        let rec loop code env =
            match isReducible code with
            | false ->
                print code env
                code, env
            | _ ->
                print code env
                reduce code env |> fun (c, e) -> loop c e

        loop code env
