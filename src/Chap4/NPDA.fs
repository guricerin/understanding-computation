module UnderstandingComputation.Chap4.NPDA

open System
open PDA

type Configs = Set<PDAConfig>

type NPDARulebook = PDARule list

[<RequireQualifiedAccess>]
module NPDARulebook =

    /// 適用可能な規則を全て返す
    let rulesFor config input rules =
        let f rule = PDARule.appliesTo config input rule
        List.filter f rules

    let followRulesFor config input rules =
        rules
        |> rulesFor config input
        |> List.map (fun rule -> PDARule.follow config rule)

    /// 現在の構成に規則を適用した結果を全て返す
    let nextConfigs (configs: Configs) input rules =
        configs
        |> Set.map ((fun config -> followRulesFor config input rules) >> Set.ofList)
        |> Set.unionMany

    /// 可能な限り自由移動
    let rec followFreeMoves configs rules =
        let moreConfigs = nextConfigs configs None rules
        if Set.isSubset moreConfigs configs
        then configs
        else followFreeMoves (Set.union configs moreConfigs) rules

type NPDA =
    { configs: Configs
      accepts: States
      rulebook: NPDARulebook }

[<RequireQualifiedAccess>]
module NPDA =

    let freeMove npda =
        let freemoved = NPDARulebook.followFreeMoves npda.configs npda.rulebook
        { npda with configs = freemoved }

    let create configs accepts rules =
        { NPDA.configs = configs
          accepts = accepts
          rulebook = rules }
        |> freeMove

    /// 受理状態である可能性があるか
    let isAccepting npda =
        let f (config: PDAConfig) = Set.contains config.state npda.accepts
        Set.exists f npda.configs

    /// 入力から1文字読み、構成を変更
    let readChar input npda =
        let npda = freeMove npda
        let configs = NPDARulebook.nextConfigs npda.configs input npda.rulebook
        { npda with configs = configs } |> freeMove

    let readString str dpda =
        let f d c = readChar (Some c) d
        Seq.fold f dpda str

/// 開始状態を保存
type NPDADesign =
    { start: State
      bottom: char
      accepts: States
      rulebook: NPDARulebook }

[<RequireQualifiedAccess>]
module NPDADesign =

    let create start bottom accepts rules =
        { NPDADesign.start = start
          bottom = bottom
          accepts = accepts
          rulebook = rules }

    let toNPDA design =
        let stack = [ design.bottom ]
        let config = PDAConfig.create design.start stack
        NPDA.create (Set.ofList [ config ]) design.accepts design.rulebook

    let accepts str design =
        design
        |> toNPDA
        |> NPDA.readString str
        |> NPDA.isAccepting
