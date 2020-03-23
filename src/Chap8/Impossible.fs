/// アルゴリズム：入力値を出力値に変えるプロセスを記述した命令のリスト。以下の性質を満たす。
/// 有限性：命令は有限個
/// 単純性：髪と鉛筆で実行できるくらい単純
/// 停止性：任意の入力について有限回のステップで終了できる
/// 正確性：任意の入力について正しい答えを生成できる

module UnderstandingComputation.Chap8.Impossible

open System

let euclid x y =
    let mutable x, y = x, y
    while x <> y do
        if x > y then x <- x - y else y <- y - x
    x

/// 文字列を２進数文字列の配列に変換
let strToBinaries (str: string) =
    let bytes = Text.Encoding.UTF8.GetBytes(str)
    Array.map (fun (b: byte) -> Convert.ToString(b, 2).PadLeft(8, '0')) bytes

/// ２進数文字列の配列を文字列に変換
let binariesToStr (binary: string array) =
    let bytes = Array.map (fun (s: string) -> Convert.ToByte(s, 2) |> Convert.ToByte) binary
    Text.Encoding.UTF8.GetString(bytes)
