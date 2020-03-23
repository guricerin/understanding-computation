open Expecto
open UnderstandingComputation.Chap8.Impossible

[<Tests>]
let ``algorithm`` =
    test "algorithm" {
        let actual = euclid 18 12
        Expect.equal actual 6 ""

        let actual = euclid 867 5309
        Expect.equal actual 1 ""
    }

[<Tests>]
let ``code as data`` =
    test "code as data" {
        let program = "printfn \"hello, world\""
        let actual = strToBinaries program
        let expect =
            [|"01110000"; "01110010"; "01101001"; "01101110"; "01110100"; "01100110";
              "01101110"; "00100000"; "00100010"; "01101000"; "01100101"; "01101100";
              "01101100"; "01101111"; "00101100"; "00100000"; "01110111"; "01101111";
              "01110010"; "01101100"; "01100100"; "00100010"|]
        Expect.equal actual expect ""

        let actual = binariesToStr actual
        Expect.equal actual program ""
    }

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
