module UnderstandingComputation.Chap5.TuringMachine

open System

/// チューリングマシンの構成部品

type Tape =
    { rLeft: char list
      mid: char
      right: char list
      blank: char }

    override self.ToString() =
        let left = List.rev self.rLeft |> String.Concat
        let right = String.Concat self.right
        sprintf "#<Tape %s(%c)%s>" left self.mid right

[<RequireQualifiedAccess>]
module Tape =

    let create left mid right blank =
        let rLeft = List.rev left
        { Tape.rLeft = rLeft
          mid = mid
          right = right
          blank = blank }

    /// 現在位置に文字を書き込む
    let write c tape = { tape with mid = c }

    let moveLeft tape =
        let rleft, mid =
            match tape.rLeft with
            | [] -> [], tape.blank
            | l :: ls -> ls, l

        let right = tape.mid :: tape.right
        { tape with
              rLeft = rleft
              mid = mid
              right = right }

    let moveRight tape =
        let mid, right =
            match tape.right with
            | [] -> tape.blank, []
            | r :: rs -> r, rs

        let rLeft = tape.mid :: tape.rLeft
        { tape with
              rLeft = rLeft
              mid = mid
              right = right }

type State = int

/// 状態とテープの組み合わせ
type TMConfig =
    { state: State
      tape: Tape }

[<RequireQualifiedAccess>]
module TMConfig =

    let create state tape =
        { TMConfig.state = state
          tape = tape }

[<RequireQualifiedAccess>]
type Direction =
    | Left
    | Right

type TMRule =
    { state: State
      readc: char
      next: State
      writec: char
      direction: Direction }

[<RequireQualifiedAccess>]
module TMRule =

    let create state readc next writec direction =
        { TMRule.state = state
          readc = readc
          next = next
          writec = writec
          direction = direction }

    /// 現在の構成に対して規則を適用可能か
    let appliesTo (config: TMConfig) (rule: TMRule) = rule.state = config.state && rule.readc = config.tape.mid

    let nextTape config rule =
        let tape = Tape.write rule.writec config.tape
        match rule.direction with
        | Direction.Left -> Tape.moveLeft tape
        | Direction.Right -> Tape.moveRight tape

    /// 規則を適用し、構成を更新
    let follow config rule =
        let state = rule.next
        let tape = nextTape config rule
        TMConfig.create state tape
