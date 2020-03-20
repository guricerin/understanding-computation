module UnderstandingComputation.Chap4.DPDATest

open Expecto
open UnderstandingComputation.Chap4
open PDA
open DPDA

module StackTest =

    [<Tests>]
    let ``stack test`` =
        test "stack test" {
            let stack = [ 'a'; 'b'; 'c'; 'd'; 'e' ]
            let actual = Stack.toString stack
            let expect = "#<Stack (a)bcde>"
            Expect.equal actual expect ""

            let actual = Stack.top stack
            Expect.equal actual 'a' ""

            let actual =
                stack
                |> Stack.pop
                |> Stack.pop
                |> Stack.top
            Expect.equal actual 'c' ""

            let actual =
                stack
                |> Stack.push 'x'
                |> Stack.push 'y'
                |> Stack.top
            Expect.equal actual 'y' ""

            let actual =
                stack
                |> Stack.push 'x'
                |> Stack.push 'y'
                |> Stack.pop
                |> Stack.top
            Expect.equal actual 'x' ""
        }

[<Tests>]
let ``pda rule`` =
    test "pda rule" {
        let rule = PDARule.create 1 (Some '(') 2 '$' [ 'b'; '$' ]
        let config = PDAConfig.create 1 [ Bottom ]
        Expect.isTrue (PDARule.appliesTo config (Some '(') rule) ""

        let actual = PDARule.follow config rule
        let expect = PDAConfig.create 2 [ 'b'; '$' ]
        Expect.equal actual expect ""
    }

module DPDATest =

    // バランスの取れた括弧を受理する
    let rulebook =
        [ PDARule.create 1 (Some '(') 2 '$' [ 'b'; '$' ]
          PDARule.create 2 (Some '(') 2 'b' [ 'b'; 'b' ]
          PDARule.create 2 (Some ')') 2 'b' []
          PDARule.create 2 None 1 '$' [ '$' ] ]

    [<Tests>]
    let ``dpda rulebook`` =
        test "dpda rulebook" {
            let config = PDAConfig.create 1 [ '$' ]
            let config = DPDARulebook.nextConfig config (Some '(') rulebook
            Expect.equal config.state 2 ""
            Expect.equal (Stack.toString config.stack) "#<Stack (b)$>" ""

            let config = DPDARulebook.nextConfig config (Some '(') rulebook
            Expect.equal config.state 2 ""
            Expect.equal (Stack.toString config.stack) "#<Stack (b)b$>" ""

            let config = DPDARulebook.nextConfig config (Some ')') rulebook
            Expect.equal config.state 2 ""
            Expect.equal (Stack.toString config.stack) "#<Stack (b)$>" ""
        }

    [<Tests>]
    let ``dpda`` =
        test "dpda" {
            let config = PDAConfig.create 1 [ '$' ]
            let dpda = DPDA.create config (Set.ofList [ 1 ]) rulebook
            Expect.isTrue (DPDA.isAccepting dpda) ""

            let dpda = DPDA.readString "(()" dpda
            Expect.isFalse (DPDA.isAccepting dpda) ""
            Expect.equal dpda.config.state 2 ""
            Expect.equal (Stack.toString dpda.config.stack) "#<Stack (b)$>" ""
        }

    [<Tests>]
    let ``read string`` =
        test "read string" {
            let config = PDAConfig.create 1 [ '$' ]
            let dpda = DPDA.create config (Set.ofList [ 1 ]) rulebook
            let dpda = DPDA.readString "(()(" dpda
            Expect.isFalse (DPDA.isAccepting dpda) ""
            Expect.equal (Stack.toString dpda.config.stack) "#<Stack (b)b$>" ""

            let dpda = DPDA.readString "))()" dpda
            Expect.isTrue (DPDA.isAccepting dpda) ""
            Expect.equal dpda.config.state 1 ""
            Expect.equal (Stack.toString dpda.config.stack) "#<Stack ($)>" ""
        }

    [<Tests>]
    let ``accepts`` =
        test "accepts" {
            let design = DPDADesign.create 1 '$' (Set.ofList [ 1 ]) rulebook
            let actual = DPDADesign.accepts "(((((((((())))))))))" design
            Expect.isTrue actual ""

            let actual = DPDADesign.accepts "()(())((()))(()(()))" design
            Expect.isTrue actual ""

            let actual = DPDADesign.accepts "(()(()(()()(()()))()" design
            Expect.isFalse actual ""

            // 行き詰まり
            let actual = DPDADesign.accepts "())" design
            Expect.isFalse actual ""
        }

module DPDAPalindrome =

    // 真ん中にマーカー文字がある回文
    let rulebook =
        [ PDARule.create 1 (Some 'a') 1 '$' [ 'a'; '$' ]
          PDARule.create 1 (Some 'a') 1 'a' [ 'a'; 'a' ]
          PDARule.create 1 (Some 'a') 1 'b' [ 'a'; 'b' ]
          PDARule.create 1 (Some 'b') 1 '$' [ 'b'; '$' ]
          PDARule.create 1 (Some 'b') 1 'a' [ 'b'; 'a' ]
          PDARule.create 1 (Some 'b') 1 'b' [ 'b'; 'b' ]
          PDARule.create 1 (Some 'm') 2 '$' [ '$' ]
          PDARule.create 1 (Some 'm') 2 'a' [ 'a' ]
          PDARule.create 1 (Some 'm') 2 'b' [ 'b' ]
          PDARule.create 2 (Some 'a') 2 'a' []
          PDARule.create 2 (Some 'b') 2 'b' []
          PDARule.create 2 None 3 '$' [ '$' ] ]

    [<Tests>]
    let ``dpda palindrome`` =
        test "dpda palindrome" {
            let design = DPDADesign.create 1 '$' (Set.ofList [ 3 ]) rulebook

            let actual = DPDADesign.accepts "abmba" design
            Expect.isTrue actual ""

            let actual = DPDADesign.accepts "babbamabbab" design
            Expect.isTrue actual ""

            let actual = DPDADesign.accepts "abmb" design
            Expect.isFalse actual ""

            let actual = DPDADesign.accepts "baambaa" design
            Expect.isFalse actual ""
        }
