module UnderstandingComputation.Chap4.DPDA

open System
open PDA

type DPDARulebook = PDARule list

[<RequireQualifiedAccess>]
module DPDARulebook =

    /// 適用可能な規則を返す
    let ruleFor config input rules =
        let f rule = PDARule.appliesTo config input rule
        List.tryFind f rules

    let nextConfig config input rules =
        rules
        |> ruleFor config input
        |> function
        | Some rule -> rule
        // | None -> invalidArg "rules" "適用可能な規則なし"
        | None -> PDARule.create Int32.MinValue None Int32.MaxValue ',' (List.ofSeq "STACK STATE")
        |> PDARule.follow config

    let appliesTo config input rules =
        match ruleFor config input rules with
        | Some _ -> true
        | None -> false

    /// 可能な限り自由移動
    let rec followFreeMoves config rules =
        if appliesTo config None rules then
            let config = nextConfig config None rules
            followFreeMoves config rules
        else
            config

type DPDA =
    { config: PDAConfig
      accepts: States
      rulebook: DPDARulebook }

[<RequireQualifiedAccess>]
module DPDA =

    let freeMove dpda =
        let freemoved = DPDARulebook.followFreeMoves dpda.config dpda.rulebook
        { dpda with config = freemoved }

    let create config accepts rules =
        let dpda =
            { DPDA.config = config
              accepts = accepts
              rulebook = rules }
        freeMove dpda

    /// 受理状態であるか
    let isAccepting dpda = Set.contains dpda.config.state dpda.accepts

    /// 入力から1文字読み、構成を変更
    let readChar input dpda =
        let dpda = freeMove dpda
        let config = DPDARulebook.nextConfig dpda.config input dpda.rulebook
        { dpda with config = config } |> freeMove

    let readString str dpda =
        let f d c = readChar (Some c) d
        Seq.fold f dpda str

/// 開始状態を保存
type DPDADesign =
    { start: State
      bottom: char
      accepts: States
      rulebook: DPDARulebook }

[<RequireQualifiedAccess>]
module DPDADesign =

    let create start bottom accepts rulebook =
        { DPDADesign.start = start
          bottom = bottom
          accepts = accepts
          rulebook = rulebook }

    let toDPDA design =
        let stack = [ design.bottom ]
        let config = PDAConfig.create design.start stack
        DPDA.create config design.accepts design.rulebook

    /// 入力文字列を受理するか
    let accepts str design =
        let dpda = toDPDA design
        dpda
        |> DPDA.readString str
        |> DPDA.isAccepting
