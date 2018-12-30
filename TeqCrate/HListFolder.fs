namespace TeqCrate

open TeqCrate.Patterns
open TypeEquality

[<RequireQualifiedAccess>]
module HListFolder =

    let makeElementFolder (f : 's -> 'a -> 's) : 's HListFolder =
        let tA = typ<'a>
        { new HListFolder<_> with
            member __.F s x =
                match typ<_> with
                | Teq tA teq -> f s (Teq.castTo teq x)
                | _ -> s
        }

    let makeGappedElementFolder (f : 's -> 'a option -> 's) : 's HListFolder =
        let tA = typ<'a>
        { new HListFolder<_> with
            member __.F s x =
                let v =
                    match typ<_> with
                    | Teq tA teq -> x |> Teq.castTo teq |> Some
                    | _ -> None
                f s v
        }
