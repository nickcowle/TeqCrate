namespace TeqCrate

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open System

[<RequireQualifiedAccess>]
module Reflection =

    let invokeStaticMethod (e : Expr) (ts : Type seq) (vs : obj seq) : obj =

        let rec getMethodInfo =
            function
            | Call (_, mi, _) -> mi
            | Lambda (_, e) -> getMethodInfo e
            | _ -> failwith "Could not get MethodInfo"

        e
        |> getMethodInfo
        |> fun mi -> mi.GetGenericMethodDefinition ()
        |> fun mi -> mi.MakeGenericMethod (ts |> Array.ofSeq)
        |> fun mi -> mi.Invoke(null, vs |> Array.ofSeq)
