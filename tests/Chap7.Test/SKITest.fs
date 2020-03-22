module UnderstandingComputation.Chap7.SKITest

open Expecto
open UnderstandingComputation.Chap7
open SKICombinatorCalculus

[<Tests>]
let ``ski expr`` =
    test "ski expr" {
        let x = SKISymbol "x"
        Expect.equal (x.ToString()) "x" ""

        let expr = SKICall(SKICall(S, K), SKICall(I, x))
        Expect.equal (expr.ToString()) "S[K][I[x]]" ""
    }

[<Tests>]
let ``ski call`` =
    test "ski call" {
        let x, y, z = SKISymbol "x", SKISymbol "y", SKISymbol "z"
        let expr = SKICall(SKICall(SKICall(S, x), y), z)
        Expect.equal (expr.ToString()) "S[x][y][z]" ""

        let combinator = SKIExpr.combinator expr
        let arguments = SKIExpr.arguments expr
        Expect.equal combinator S ""
        Expect.equal arguments [ x; y; z ] ""

        let expr = SKIExpr.call combinator arguments
        Expect.equal (expr.ToString()) "x[z][y[z]]" ""
    }

[<Tests>]
let ``ski callable`` =
    test "ski callable" {
        let x, y, z = SKISymbol "x", SKISymbol "y", SKISymbol "z"
        let expr = SKICall(SKICall(x, y), z)
        let comb = SKIExpr.combinator expr
        let args = SKIExpr.arguments expr
        Expect.isFalse (SKIExpr.isCallable comb args) ""

        let expr = SKICall(SKICall(S, x), y)
        let comb = SKIExpr.combinator expr
        let args = SKIExpr.arguments expr
        Expect.isFalse (SKIExpr.isCallable comb args) ""

        let expr = SKICall(SKICall(SKICall(S, x), y), z)
        let comb = SKIExpr.combinator expr
        let args = SKIExpr.arguments expr
        Expect.isTrue (SKIExpr.isCallable comb args) ""
    }

[<Tests>]
let ``ski reduce`` =
    test "ski reduce" {
        let x, y, z = SKISymbol "x", SKISymbol "y", SKISymbol "z"
        let swap = SKICall(SKICall(S, SKICall(K, SKICall(S, I))), K)
        Expect.equal (swap.ToString()) "S[K[S[I]]][K]" ""

        let expr = SKICall(SKICall(swap, x), y)
        Expect.equal (expr.ToString()) "S[K[S[I]]][K][x][y]" ""

        let expr' = SKIExpr.eval expr
        Expect.equal (expr'.ToString()) "y[x]" ""
    }

[<Tests>]
let ``ski as-a-function-of`` =
    test "ski as-a-function-of" {
        let org = SKICall(SKICall(S, K), I)
        Expect.equal (org.ToString()) "S[K][I]" ""

        let func = SKIExpr.asAFunctionOf "x" org
        Expect.equal (func.ToString()) "S[S[K[S]][K[K]]][K[I]]" ""
        Expect.isFalse (SKIExpr.isReducible func) ""

        let y = SKISymbol("y")
        let expr = SKICall(func, y)
        let expr = SKIExpr.eval expr
        Expect.equal (expr.ToString()) "S[K][I]" ""
        Expect.equal expr org ""
    }

[<Tests>]
let ``ski as-a-function-of replace`` =
    test "ski as-a-function-of replace" {
        let x, y, z = SKISymbol "x", SKISymbol "y", SKISymbol "z"
        let org = SKICall(SKICall(S, x), I)
        Expect.equal (org.ToString()) "S[x][I]" ""

        let func = SKIExpr.asAFunctionOf "x" org
        Expect.equal (func.ToString()) "S[S[K[S]][I]][K[I]]" ""
        Expect.isFalse (SKIExpr.isReducible func) ""

        let expr = SKICall(func, y)
        let expr = SKIExpr.eval expr
        Expect.equal (expr.ToString()) "S[y][I]" ""
        Expect.notEqual expr org ""
    }

open UnderstandingComputation.Chap6
open UntypedLambdaCalculus
open ULCParser

[<Tests>]
let ``to ski`` =
    test "to ski" {
        // ラムダ式の「2」
        let code = "-> p { -> x { p[p[x]] } }"
        let two = ULCParser.parse code
        Expect.equal (two.ToString()) code ""

        let actual = ULCConverter.toSKI two
        let expect = "S[S[K[S]][S[K[K]][I]]][S[S[K[S]][S[K[K]][I]]][K[I]]]"
        Expect.equal (actual.ToString()) expect ""

        // SKI式の「(+) 1」「0」
        let inc, zero = SKISymbol "inc", SKISymbol "zero"
        let expr = SKICall(SKICall(ULCConverter.toSKI two, inc), zero)
        let expr = SKIExpr.eval expr
        let actual = expr.ToString()
        let expect = "inc[inc[zero]]"
        Expect.equal actual expect ""
    }

[<Tests>]
let ``S[K][K] = I`` =
    test "S[K][K] = I" {
        let identity = SKICall(SKICall(S, K), K)
        let x = SKISymbol "x"
        let expr = SKICall(identity, x)
        Expect.equal (expr.ToString()) "S[K][K][x]" ""
        let actual = SKIExpr.eval expr
        Expect.equal (actual.ToString()) "x" ""
    }

[<Tests>]
let ``to iota`` =
    test "to iota" {
        let expr = IotaConverter.toIota S
        let expect = "ι[ι[ι[ι[ι]]]]"
        Expect.equal (expr.ToString()) expect ""

        let expr = SKIExpr.eval expr
        let expect = "S"
        Expect.equal (expr.ToString()) expect ""
    }

[<Tests>]
let ``I to iota`` =
    test "I to iota" {
        let expr = IotaConverter.toIota I
        let expr = SKIExpr.eval expr
        let expect = "S[K][K[K]]"
        Expect.equal (expr.ToString()) expect ""

        let identity = SKICall(SKICall(S, K), SKICall(K, K))
        let expect = "S[K][K[K]]"
        Expect.equal (identity.ToString()) expect ""
        let x = SKISymbol "x"
        let expr = SKICall(identity, x)
        let expect = "S[K][K[K]][x]"
        Expect.equal (expr.ToString()) expect ""
        let expr = SKIExpr.eval expr
        let expect = "x"
        Expect.equal (expr.ToString()) expect ""
    }

[<Tests>]
let ``ulc to iota`` =
    test "ulc to iota" {
        let code = "-> p { -> x { p[p[x]] } }"
        let two = ULCParser.parse code
        let inc, zero = SKISymbol "inc", SKISymbol "zero"

        let two =
            two
            |> ULCConverter.toSKI
            |> IotaConverter.toIota

        let expr = SKICall(SKICall(two, inc), zero)
        let expr = SKIExpr.eval expr
        let expect = "inc[inc[zero]]"
        Expect.equal (expr.ToString()) expect ""
    }
