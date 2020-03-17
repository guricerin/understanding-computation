module UnderstandingComputation.Chap3.Automaton

type State =
    | State of int

    override self.ToString() =
        match self with
        | State s -> s.ToString()

/// オートマトンの規則
type FARule =
    { current: State
      input: char
      next: State }

    override self.ToString() =
        let cur, next = self.current.ToString(), self.next.ToString()
        sprintf "#<FARule %s --%c--> %s>" cur self.input next

[<RequireQualifiedAccess>]
module FARule =

    let create (cur: int) (input: char) (next: int) =
        { FARule.current = State cur
          input = input
          next = State next }

    /// 規則を適用できるか
    let appliesTo (state: State) (input: char) rule = rule.current = state && rule.input = input

    /// 規則を適用した際の次の状態
    let follow rule = rule.next
