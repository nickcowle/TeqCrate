namespace TeqCrate

open System
open TeqCrate.TypePatterns

[<RequireQualifiedAccess>]
module Type =

    let rec print (t : Type) =
        match t with
        | Generic (t, ts) ->
            sprintf "%s<%s>" t.Name (ts |> Seq.map print |> String.concat ", ")
        | Fun (domain, range) ->
            sprintf "(%s -> %s)" (print domain) (print range)
        | _ -> t.Name
