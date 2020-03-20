module UnderstandingComputation.Chap4.Analysis

open System
open Expecto
open UnderstandingComputation.Chap4
open NPDA
open Lexer
open Parser

[<Tests>]
let ``lexer`` =
    test "lexer" {
        let lexer = Lexer("y = x * 7")
        let actual = lexer.Analyze()
        let expect = [ 'v'; '='; 'v'; '*'; 'n' ]
        Expect.equal actual expect ""

        let lexer = Lexer("while (x < 5) { x = x * 3 }")
        let actual = lexer.Analyze()
        let expect = "w(v<n){v=v*n}" |> List.ofSeq
        Expect.equal actual expect ""

        let lexer = Lexer("if (x < 10) { y = true; x = 0 } else { do-nothing }")
        let actual = lexer.Analyze()
        let expect = "i(v<n){v=b;v=n}e{d}" |> List.ofSeq
        Expect.equal actual expect ""

        let lexer = Lexer("x = falsehood")
        let actual = lexer.Analyze()
        let expect = "v=v" |> List.ofSeq
        Expect.equal actual expect ""
    }

[<Tests>]
let ``parser`` =
    test "parser" {
        let lexer = Lexer("while (x < 5) { x = x * 3 }")
        let tokens = lexer.Analyze() |> fun t -> String.Join("", t)
        let actual = NPDADesign.accepts tokens SIMPLEParser.npdaDesign
        Expect.isTrue actual ""

        let lexer = Lexer("while (x < 5 x = x * }")
        let tokens = lexer.Analyze() |> fun t -> String.Join("", t)
        let actual = NPDADesign.accepts tokens SIMPLEParser.npdaDesign
        Expect.isFalse actual ""
    }
