/// 部分再帰関数
module UnderstandingComputation.Chap7.PartialRecursiveFunction

let zero = 0

let increment = (+) 1

/// 再帰呼び出しの度に引数が0に近づき、必ず0になって再帰が停止
/// zero, increment, recurse から組み立てられるプログラムを原始再帰関数と呼ぶ
/// 原始再帰関数を使って、文字の読み込み、文字の書き込み、テープヘッドの操作をシミュレートできる
let rec recurse f g values =
    let lastValue, otherValues =
        match List.rev values with
        | [] -> invalidArg "values" "values is empty"
        | x :: xs -> x, List.rev xs
    if lastValue = zero then
        f otherValues
    else
        let easierLastValue = lastValue - 1
        let easierValues = otherValues @ [ easierLastValue ]
        let easierResult = recurse f g easierValues
        g (List.head easierValues) (List.tail easierValues) easierResult

/// 繰り返しをシミュレートできる
let minimize f =
    let rec loop n =
        if f n = 0 then n else loop (n + 1)
    loop 0
