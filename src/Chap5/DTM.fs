module UnderstandingComputation.Chap5.DTM

open TuringMachine

/// 決定性チューリングマシンの規則集
type DTMRulebook = TMRule list

[<RequireQualifiedAccess>]
module DTMRulebook =

    /// 現在の構成に対して適用可能な規則を返す
    let ruleFor config rules = List.tryFind (fun rule -> TMRule.appliesTo config rule) rules

    /// 現在の構成に対して規則を適用可能か
    let appliesTo config rules =
        match ruleFor config rules with
        | Some _ -> true
        | None -> false

    let nextConfig config rules =
        match ruleFor config rules with
        | Some rule -> TMRule.follow config rule
        | None -> invalidArg "rules" "行き詰まり"

type States = Set<State>

/// 決定性チューリングマシン
type DTM =
    { config: TMConfig
      accepts: States
      rulebook: DTMRulebook }

[<RequireQualifiedAccess>]
module DTM =

    let create config accepts rulebook =
        { DTM.config = config
          accepts = accepts
          rulebook = rulebook }

    /// 受理状態か
    let isAccepting dtm =
        let current = dtm.config.state
        Set.contains current dtm.accepts

    /// 行き詰まり状態か
    let isStuck dtm = (not <| isAccepting dtm) && (not <| DTMRulebook.appliesTo dtm.config dtm.rulebook)

    let step dtm =
        let next = DTMRulebook.nextConfig dtm.config dtm.rulebook
        { dtm with config = next }

    /// 停止するまでシミュレーションを実行
    let run dtm =
        let rec loop cur =
            match isAccepting cur, isStuck cur with
            | false, false ->
                cur
                |> step
                |> loop
            | _ -> cur
        loop dtm
