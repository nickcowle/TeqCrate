namespace TeqCrate

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open System

[<RequireQualifiedAccess>]
module Reflection =

    let invokeStaticMethod (e : Expr) (ts : Type seq) (vs : obj seq) : obj =

        match e with
        | Lambda (_, Call (_, mi, _)) -> mi
        | _ -> failwith "Could not get MethodInfo"
        |> fun mi -> mi.GetGenericMethodDefinition ()
        |> fun mi -> mi.MakeGenericMethod (ts |> Array.ofSeq)
        |> fun mi -> mi.Invoke(null, vs |> Array.ofSeq)
