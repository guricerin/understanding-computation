module UnderstandingComputation.Chap7.PRFTest

open Expecto
open UnderstandingComputation.Chap7
open PartialRecursiveFunction

[<Tests>]
let ``zero, increment`` =
    test "zero, increment" {
        let two = increment (increment zero)
        Expect.equal two 2 ""

        let three = increment two
        Expect.equal three 3 ""

        let addThree x = increment (increment (increment x))
        Expect.equal (addThree two) 5 ""
    }

module PRFSample =

    let add x y =
        let addZeroToX xs = List.head xs
        let incrementEasierResult _ _ easierResult = increment easierResult
        recurse addZeroToX incrementEasierResult [ x; y ]

    let multiply x y =
        let multXByZero _ = zero
        let addXToEasierResult x _ easierResult = add x easierResult
        recurse multXByZero addXToEasierResult [ x; y ]

    let decrement x =
        let easierX x _ _ = x
        let cons x _ = x
        recurse (cons zero) easierX [ x ]

    let subtract x y =
        let subZeroFromX x = List.head x
        let decEasierResult _ _ easierResult = decrement easierResult
        recurse subZeroFromX decEasierResult [ x; y ]

    let divide x y =
        let f n = subtract (increment x) (multiply y (increment n))
        minimize f


    [<Tests>]
    let ``prf samples`` =
        test "prf samples" {
            let two = increment (increment zero)
            let three = increment two
            let actual = add two three
            Expect.equal actual 5 ""

            let six = multiply two three
            Expect.equal six 6 ""

            let actual = decrement six
            Expect.equal actual 5 ""

            let actual = subtract six two
            Expect.equal actual 4 ""

            let actual = subtract two six
            Expect.equal actual 0 ""

            let actual = divide six two
            Expect.equal actual 3 ""

            let ten = increment (multiply three three)
            Expect.equal ten 10 ""

            let actual = divide ten two
            Expect.equal actual 5 ""
        }
