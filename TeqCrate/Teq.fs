namespace TeqCrate

open TypeEquality

[<RequireQualifiedAccess>]
module Teq =

    let tryMake<'a, 'b> : Teq<'a, 'b> option =
        if typeof<'a> = typeof<'b> then
            Teq.refl<'a> |> unbox |> Some
        else
            None
