module UnderstandingComputation.Chap7.TagSystem

type TagRule =
    { first: char
      appends: string }

[<RequireQualifiedAccess>]
module TagRule =

    let create first appends =
        { TagRule.first = first
          appends = appends }

    /// 入力に対して規則を適用できるか
    let appliesTo (input: string) rule =
        if input = "" then false else input.[0] = rule.first

    /// 規則を適用（入力の末尾に文字列を追加）
    let follow (input: string) rule = input + rule.appends

type TagRulebook =
    { deletionNum: int
      rules: TagRule list }

[<RequireQualifiedAccess>]
module TagRulebook =

    let create deletionNum rules =
        { TagRulebook.deletionNum = deletionNum
          rules = rules }

    /// 適用可能な規則
    let ruleFor (input: string) rulebook =
        let f rule = TagRule.appliesTo input rule
        List.tryFind f rulebook.rules

    /// 遷移関数
    let nextString (input: string) rulebook =
        match ruleFor input rulebook with
        | Some rule ->
            let next = TagRule.follow input rule
            if next.Length < rulebook.deletionNum
            then None
            else next.Remove(0, rulebook.deletionNum) |> Some
        | None -> None

    let appliesTo input rulebook =
        match ruleFor input rulebook with
        | Some rule -> input.Length >= rulebook.deletionNum
        | None -> false

type TagSystem =
    { current: string
      rulebook: TagRulebook }

[<RequireQualifiedAccess>]
module TagSystem =

    let create current rulebook =
        { TagSystem.current = current
          rulebook = rulebook }

    let step system =
        match TagRulebook.nextString system.current system.rulebook with
        | Some next -> { system with current = next }
        | None -> system // 停止

    /// 停止するまで実行
    let run system =
        let rec loop cur =
            if TagRulebook.appliesTo cur.current cur.rulebook then
                cur
                |> step
                |> loop
            else
                cur
        loop system
