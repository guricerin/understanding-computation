module UnderstandingComputation.Chap9.Sign

[<RequireQualifiedAccess>]
type Sign =
    | Negative
    | Zero
    | Possible
    | Unknown

    override self.ToString() =
        match self with
        | Negative -> "negative"
        | Zero -> "zero"
        | Possible -> "possible"
        | Unknown -> "unknown"
        |> sprintf "#<Sign %s>"

    static member inline (*) (l: Sign, r: Sign) =
        match l, r with
        | Zero, _
        | _, Zero -> Zero
        | Unknown, _
        | _, Unknown -> Unknown
        | _ when l = r -> Possible
        | _ -> Negative

    static member inline (+) (l: Sign, r: Sign) =
        match l, r with
        | _ when l = r -> l
        | Zero, _ -> r
        | _, Zero -> l
        | _ -> Unknown

    /// LessThanOrEqual (<=)
    static member inline Lte(l: Sign, r: Sign) = l = r || r = Unknown

[<RequireQualifiedAccess>]
module Sign =

    /// sign は組み込みで既にある（require static member Sign）のでモジュールに閉じ込める
    let inline sign (x: ^a) =
        let zero = LanguagePrimitives.GenericZero
        if x < zero then Sign.Negative
        elif x = zero then Sign.Zero
        else Sign.Possible

    let inline calculate x y z = (x * y) * (x * z)

    let inline sumOfSquares x y = (x * x) + (y * y)
