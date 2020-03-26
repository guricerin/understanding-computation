module UnderstandingComputation.Chap3.Automaton

/// オートマトンの規則
type FARule<'a> =
    { current: 'a
      input: char
      next: 'a }

    override self.ToString() =
        let cur, next = self.current.ToString(), self.next.ToString()
        sprintf "#<FARule %s --%c--> %s>" cur self.input next

[<RequireQualifiedAccess>]
module FARule =

    let create cur input next =
        { FARule.current = cur
          input = input
          next = next }

    /// 規則を適用できるか
    let appliesTo state input rule = rule.current = state && rule.input = input

    /// 規則を適用した際の次の状態
    let follow rule = rule.next
