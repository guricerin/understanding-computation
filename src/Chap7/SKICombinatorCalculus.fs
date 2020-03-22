module UnderstandingComputation.Chap7.SKICombinatorCalculus

type SKIExpr =
    | S
    | K
    | I
    | Iota
    | SKISymbol of name: string
    | SKICall of left: SKIExpr * right: SKIExpr

    override self.ToString() =
        match self with
        | S -> "S"
        | K -> "K"
        | I -> "I"
        | Iota -> "ι"
        | SKISymbol name -> name
        | SKICall(left, right) -> sprintf "%s[%s]" (left.ToString()) (right.ToString())

[<RequireQualifiedAccess>]
module SKIExpr =

    /// 簡約規則
    let call expr args =
        match expr with
        | S ->
            // S[a][b][c] -> a[c][b[c]]
            match args with
            | [ a; b; c ] -> SKICall(SKICall(a, c), SKICall(b, c))
            | _ ->
                let msg = sprintf "S call expected 3 args, actual: %d" (List.length args)
                invalidArg "args" msg
        | K ->
            // K[a][b] -> a
            match args with
            | [ a; b ] -> a
            | _ ->
                let msg = sprintf "K call expected 2 args, actual: %d" (List.length args)
                invalidArg "args" msg
        | I ->
            // I[a] -> a
            match args with
            | [ a ] -> a
            | _ ->
                let msg = sprintf "I call expected 1 args, actual: %d" (List.length args)
                invalidArg "args" msg
        | Iota ->
            // Iota[a] -> a[S][K]
            match args with
            | [ a ] -> SKICall(SKICall(a, S), K)
            | _ ->
                let msg = sprintf "Iota call expected 1 args, actual: %d" (List.length args)
                invalidArg "args" msg
        | _ ->
            let msg = sprintf "%s is not callable" (expr.ToString())
            invalidArg "expr" msg

    /// 式の左端のシンボルを返す
    let rec combinator expr =
        match expr with
        | SKICall(left, right) -> combinator left // 二分木からコンビネータを取り出す
        | _ -> expr

    /// 式の左端以外を返す
    let rec arguments expr =
        match expr with
        | SKICall(left, right) -> (arguments left) @ [ right ]
        | _ -> []

    let isCallable expr args =
        let argnum = List.length args
        match expr with
        | S -> argnum = 3
        | K -> argnum = 2
        | I -> argnum = 1
        | Iota -> argnum = 1
        | _ -> false

    let rec isReducible expr =
        match expr with
        | SKICall(left, right) ->
            isReducible left || isReducible right || isCallable (combinator expr) (arguments expr)
        | _ -> false

    /// 簡約
    let rec reduce expr =
        if isReducible expr then
            match expr with
            | SKICall(left, right) ->
                match isReducible left, isReducible right with
                | true, _ -> SKICall(reduce left, right)
                | _, true -> SKICall(left, reduce right)
                | _ -> call (combinator expr) (arguments expr)
            | _ -> invalidArg "expr" "Only SKICall is reducible"
        else
            expr

    /// 簡約しきる
    let eval expr =
        let rec loop cur =
            if isReducible cur then
                cur
                |> reduce
                |> loop
            else
                cur
        loop expr

    /// ラムダ計算の表示的意味論を与える
    /// SKI式を、1つの引数で呼び出されると元のSKI式に戻るような新しいSKI式を返す
    /// SKI式を関数本体として扱える
    let rec asAFunctionOf name expr =
        match expr with
        | S
        | K
        | I -> SKICall(K, expr)
        | SKISymbol name' ->
            if name' = name then I else SKICall(K, expr)
        | SKICall(left, right) ->
            let left' = asAFunctionOf name left
            let right' = asAFunctionOf name right
            SKICall(SKICall(S, left'), right')
        | Iota -> invalidArg "expr" "Iota is not as-a-functon-of"

[<RequireQualifiedAccess>]
module ULCConverter =
    open UnderstandingComputation.Chap6.UntypedLambdaCalculus

    let rec toSKI lcexpr =
        match lcexpr with
        | LCV name -> SKISymbol(name)
        | LCC(left, right) -> SKICall(toSKI left, toSKI right)
        | LCF(param, body) ->
            body
            |> toSKI
            |> SKIExpr.asAFunctionOf param

[<RequireQualifiedAccess>]
module IotaConverter =

    let rec toIota ski =
        match ski with
        | Iota
        | SKISymbol _ -> ski
        | SKICall(left, right) -> SKICall(toIota left, toIota right)
        // S -> i[i[i[i[i]]]]
        | S -> SKICall(Iota, SKICall(Iota, SKICall(Iota, SKICall(Iota, Iota))))
        // K -> i[i[i[i]]]
        | K -> SKICall(Iota, SKICall(Iota, SKICall(Iota, Iota)))
        // I -> i[i]
        | I -> SKICall(Iota, Iota)
