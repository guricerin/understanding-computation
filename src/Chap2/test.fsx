#r "bin/Release/netstandard2.0/Chap2.dll"

open UnderstandingComputation.Chap2
open Expression
open Statement
open Machine

// Machine.succPrint expr
let inline show (x: ^a) = (^a: (static member Inspect: ^a -> string) x)

let expr = Multiply(Number(1), Multiply(Add(Number(2), Number(3)), Number(4)))

Machine.run expr (Env.empty)
printfn ""

let stmt = Assign("x", Add(Variable "x", Number 1))
let env = Env.ofList [ ("x", Number(41)) ]

Machine.run stmt env
printfn ""

let stmt2 = Sequence(Assign("x", Add(Number 1, Number 1)), Assign("y", Add(Variable "x", Number 3)))
let env2 = Env.empty

Machine.run stmt2 env2
printfn ""

let stmt3 = Sequence(Assign("x", Boolean true), Assign("x", Add(Variable "x", Number 1)))
let env3 = Env.empty

// Machine.run stmt3 env3
// printfn ""

let stmt4 = Sequence(Assign("x", Add(Number 1, Number 1)), Assign("y", Add(Variable "x", Number 3)))
let expr4, env4 = Stmt.evaluate stmt4 env3

printfn "%s" (expr4.ToString())
printfn "%A" env4
printfn "%A" (Env.tryFind "x" env4)
printfn "%A" (Env.tryFind "y" env4)
printfn ""

let stmt5 = While(LessThan(Variable "x", Number 5), Assign("x", Multiply(Variable "x", Number 3)))
let env5 = Env.ofList [ ("x", Number 1) ]
let expr6, env6 = Stmt.evaluate stmt5 env5

printfn "%s" (expr6.ToString())
printfn "%A" env6
printfn "%A" (Env.tryFind "x" env6)
printfn "%A" (Env.tryFind "y" env6)
printfn ""
