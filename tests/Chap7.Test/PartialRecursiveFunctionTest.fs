module UnderstandingComputation.Chap7.PartialRecursiveFunctionTest

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
