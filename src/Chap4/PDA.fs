module UnderstandingComputation.Chap4.PDA

open System

type State = int

type States = Set<State>

type Stack = char list

[<Literal>]
let Bottom = '$'

[<RequireQualifiedAccess>]
module Stack =

    let empty: Stack = [ Bottom ]

    let push c (stack: Stack) = c :: stack

    let pop (stack: Stack) =
        match stack with
        | [] -> invalidArg "stack" "stack is empty"
        | x :: xs -> xs

    let top (stack: Stack) =
        match stack with
        | [] -> invalidArg "stack" "stack is empty"
        | x :: xs -> x

    let toString (stack: Stack) =
        let head, tail =
            match stack with
            | [] -> "", ""
            | x :: xs -> x.ToString(), String.Join("", xs)
        sprintf "#<Stack (%s)%s>" head tail

/// 状態とスタックの組み合わせの1つを表現
type PDAConfig =
    { state: State
      stack: Stack }

[<RequireQualifiedAccess>]
module PDAConfig =

    let create state stack =
        { PDAConfig.state = state
          stack = stack }

type PDARule =
    { state: State
      input: char option
      next: State
      popchar: char
      pushchars: char list }

[<RequireQualifiedAccess>]
module PDARule =

    let create state input next popc pushcs =
        { PDARule.state = state
          input = input
          next = next
          popchar = popc
          pushchars = pushcs }

    /// 規則を適用可能か
    let appliesTo config input rule =
        let top = Stack.top config.stack
        rule.state = config.state && rule.popchar = top && rule.input = input

    /// stackに文字列をプッシュ
    let nextStack config rule =
        let poped = Stack.pop config.stack
        let pushcs = rule.pushchars
        pushcs @ poped

    /// 規則を適用し、次の構成を返す
    let follow config rule =
        let stack = nextStack config rule
        PDAConfig.create rule.next stack
