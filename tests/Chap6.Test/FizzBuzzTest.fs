module UnderstandingComputation.Chap6.FizzBuzzTest

open Expecto
open UnderstandingComputation.Chap6.FizzBuzz

let nomalFizzBuzz l r =
    let rec loop cur acc =
        if cur > r then
            acc
        else
            let x =
                if cur % 15 = 0 then "FizzBuzz"
                elif cur % 3 = 0 then "Fizz"
                elif cur % 5 = 0 then "Buzz"
                else cur.ToString()
            loop (cur + 1) (x :: acc)
    loop l [] |> List.rev

[<Tests>]
let ``fizzbuzz`` =
    test "fizzbuzz" {
        let mutable insane = []
        let action str = insane <- str :: insane
        do iter one hundred (fizzbuzz >> action)
        let actual = List.rev insane
        let expect = nomalFizzBuzz 1 100
        Expect.equal actual expect ""
    }
