module UnderstandingComputation.Chap3.Automaton

type State = int

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

    let create (cur: State) (input: char) (next: int) =
        { FARule.current = cur
          input = input
          next = next }

    /// 規則を適用できるか
    let appliesTo (state: State) (input: char) rule = rule.current = state && rule.input = input

    /// 規則を適用した際の次の状態
    let follow rule = rule.next
