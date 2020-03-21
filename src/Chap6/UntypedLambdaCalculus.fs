module UnderstandingComputation.Chap6.UntypedLambdaCalculus

/// ラムダ計算の項
type LCTerm =
    /// 識別子
    | LCV of name: string
    /// 無名関数
    | LCF of param: string * body: LCTerm
    /// 関数適用
    | LCC of left: LCTerm * right: LCTerm

    override self.ToString() =
        match self with
        | LCV name -> name
        | LCF(param, body) -> sprintf "-> %s { %s }" param (body.ToString())
        | LCC(left, right) -> sprintf "%s[%s]" (left.ToString()) (right.ToString())

[<RequireQualifiedAccess>]
module LCTerm =

    /// 式の中の特定の変数を別の式に置き換える
    let rec replace name replacement term =
        match term with
        | LCV(name') ->
            if name' = name then replacement else term
        // 関数本体の自由変数のみを置き換える。束縛変数（関数の引数）は置き換えない
        | LCF(param, body) ->
            if param = name then
                term
            else
                let body' = replace name replacement body
                LCF(param, body')
        | LCC(left, right) ->
            let left' = replace name replacement left
            let right' = replace name replacement right
            LCC(left', right')

    /// 関数本体の変数を実引数に置き換え、関数を評価する
    let call arg term =
        match term with
        | LCF(param, body) -> replace param arg body
        | _ -> invalidArg "term" "LCV and LCC are not callable"

    let isCallable =
        function
        | LCF _ -> true
        | _ -> false

    let rec isReducible =
        function
        | LCC(left, right) -> isReducible left || isReducible right || isCallable left
        | _ -> false

    /// 式を簡約
    /// 値渡し（引数を簡約しきってから呼び出しを実行）
    let rec reduce =
        function
        | LCC(left, right) ->
            match isReducible left, isReducible right with
            | true, _ -> LCC(reduce left, right)
            | _, true -> LCC(left, reduce right)
            | _ -> left |> call right // leftはLCFのはず
        | _ -> invalidArg "term" "LCV and LCF are not reducible"

    let eval term =
        let rec loop cur =
            if isReducible cur then
                cur
                |> reduce
                |> loop
            else
                cur
        loop term
