/// これで勘弁して

module UnderstandingComputation.Chap6.FizzBuzz

type Church =
    | Zero
    | Succ of Church

type Comp =
    | Eq
    | Gt
    | Lt

let rec comp x y =
    match x, y with
    | Zero, Zero -> Eq
    | Succ _, Zero -> Gt
    | Zero, Succ _ -> Lt
    | Succ x', Succ y' -> comp x' y'

let inc c = Succ c

let dec =
    function
    | Zero -> invalidArg "x" "x = Zero"
    | Succ c -> c

let rec toInt c =
    match c with
    | Zero -> 0
    | Succ c -> 1 + (toInt c)

let rec toChurch n =
    match n with
    | 0 -> Zero
    | _ -> Succ(toChurch (n - 1))

let rec add x y =
    match x, y with
    | Zero, _ -> y
    | _, Zero -> x
    | Succ x', Succ y' -> inc (inc (add x' y'))

let rec sub x y =
    match x, y with
    | Zero, Zero -> Zero
    | Zero, Succ _ -> invalidArg "y" "x < y"
    | Succ _, Zero -> x
    | Succ x', Succ y' -> sub x' y'

let mul x y =
    let rec loop a b acc =
        match a, b with
        | Zero, _ -> acc
        | _, Zero -> acc
        | _, Succ b' -> loop a b' (add a acc)
    loop x y Zero

let quotAndRem x y =
    match y with
    | Zero -> invalidArg "y" "divided by zero"
    | _ ->
        let rec loop a b quotAcc remAcc =
            match comp a b with
            | Eq -> add quotAcc (Succ Zero), remAcc
            | Lt -> quotAcc, add a remAcc
            | Gt ->
                let rem = sub a b
                loop rem b (add quotAcc (Succ Zero)) remAcc
        loop x y Zero Zero

let div x y = quotAndRem x y |> fst

let modulo x y = quotAndRem x y |> snd

let zero = Zero
let one = Succ zero
let two = Succ one
let three = Succ two
let five = add three two
let ten = mul two five
let hundred = mul ten ten

let (|FizzBuzz|Fizz|Buzz|Num|) c =
    match modulo c three, modulo c five with
    | Zero, Zero -> FizzBuzz
    | Zero, _ -> Fizz
    | _, Zero -> Buzz
    | _ -> Num

let fizzbuzz x =
    match x with
    | FizzBuzz -> "FizzBuzz"
    | Fizz -> "Fizz"
    | Buzz -> "Buzz"
    | Num ->
        x
        |> toInt
        |> string

let iter l r action =
    let rec loop cur =
        match comp cur r with
        | Gt -> ()
        | _ ->
            action cur |> ignore
            loop (inc cur)
    loop l
