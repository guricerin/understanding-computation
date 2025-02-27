#if !FAKE
#load ".fake/build.fsx/intellisense.fsx"
#r "netstandard"
#endif

open Fake.Core
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

let testProjects =
    [ "Chap2.Test"; "Chap3.Test"; "Chap4.Test"; "Chap5.Test"; "Chap6.Test"; "Chap7.Test"; "Chap8.Test"; "Chap9.Test" ]

Target.create "Test" (fun _ ->
    [ for x in testProjects -> sprintf "tests/%s/bin/Release/**/%s.dll" x x ]
    |> function
    | [] -> printfn "There is no test project"
    | x :: xs -> Seq.fold (++) (!!x) xs |> Expecto.run id)

let dotnet cmd arg = DotNet.exec id cmd arg |> ignore

Target.create "Tool" (fun _ ->
    dotnet "tool" "update paket"
    dotnet "tool" "update fake-cli")

Target.create "Setup" (fun _ -> dotnet "paket" "update")

Target.create "Clean" (fun _ -> !!"src/**/bin" ++ "src/**/obj" ++ "tests/**/bin" ++ "tests/**/obj" |> Shell.cleanDirs)

Target.create "Build" (fun _ -> !!"src/**/*.*proj" ++ "tests/**/*.*proj" |> Seq.iter (DotNet.build id))

Target.create "All" ignore

"Clean" ==> "Build" ==> "Test" ==> "All"
//"Tool"
"Tool" ==> "Setup"

Target.runOrDefault "All"
