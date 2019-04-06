namespace TeqCrate

open HCollections
open TeqCrate.Patterns

[<RequireQualifiedAccess>]
module Tuple =

    let tryFoldTuple (folder : 's HListFolder) (s : 's) (tuple : 't) : 's option =
        match tType<'t> with
        | Tuple c ->
            c.Apply
                { new TupleConvCrateEvaluator<_,_> with
                    member __.Eval _ conv =
                        tuple |> conv.To |> HList.fold folder s
                }
            |> Some
        | _ -> None
