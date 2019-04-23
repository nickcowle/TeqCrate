namespace TeqCrate

open System.Collections.Generic
open TeqCrate.TypePatterns
open TypeEquality


type ArrayTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b array> -> 'ret
type 'a ArrayTeqCrate = abstract member Apply : ArrayTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module ArrayTeqCrate =

    let make () =
            { new ArrayTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Array e ->
            let c = Reflection.invokeStaticMethod <@ make @> [| e |] [| |]
            c |> unbox<'a ArrayTeqCrate> |> Some
        | _ ->
            None


type ListTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b list> -> 'ret
type 'a ListTeqCrate = abstract member Apply : ListTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module ListTeqCrate =

    let make () =
            { new ListTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<_ list> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a ListTeqCrate> |> Some
        | _ -> None


type SeqTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b seq> -> 'ret
type 'a SeqTeqCrate = abstract member Apply : SeqTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module SeqTeqCrate =

    let make () =
            { new SeqTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<_ seq> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a SeqTeqCrate> |> Some
        | _ -> None


type SetTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b Set> -> 'ret
type 'a SetTeqCrate = abstract member Apply : SetTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module SetTeqCrate =

    let make<'a when 'a : comparison> () =
            { new SetTeqCrate<'a Set> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<_ Set> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a SetTeqCrate> |> Some
        | _ -> None


type MapTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, Map<'k, 'v>> -> 'ret
type 'a MapTeqCrate = abstract member Apply : MapTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module MapTeqCrate =

    let make<'k, 'v when 'k : comparison> () =
            { new MapTeqCrate<Map<'k, 'v>> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<Map<_,_>> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a MapTeqCrate> |> Some
        | _ -> None


type DictionaryTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, Dictionary<'k, 'v>> -> 'ret
type 'a DictionaryTeqCrate = abstract member Apply : DictionaryTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module DictionaryTeqCrate =

    let make () =
            { new DictionaryTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<Dictionary<_,_>> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a DictionaryTeqCrate> |> Some
        | _ -> None


type ResizeArrayTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b ResizeArray> -> 'ret
type 'a ResizeArrayTeqCrate = abstract member Apply : ResizeArrayTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module ResizeArrayTeqCrate =

    let make () =
            { new ResizeArrayTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<_ ResizeArray> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a ResizeArrayTeqCrate> |> Some
        | _ -> None


type FunTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b -> 'c> -> 'ret
type 'a FunTeqCrate = abstract member Apply : FunTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module FunTeqCrate =

    let make () =
            { new FunTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Fun (dom, ran) ->
            Reflection.invokeStaticMethod <@ make @> [| dom ; ran |] [| |]
            |> unbox<'a FunTeqCrate> |> Some
        | _ -> None


type PairTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b * 'c> -> 'ret
type 'a PairTeqCrate = abstract member Apply : PairTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module PairTeqCrate =

    let make () =
            { new PairTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Tuple ts when ts.Length = 2 ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a PairTeqCrate> |> Some
        | _ -> None


type TripleTeqEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b * 'c * 'd> -> 'ret
type 'a TripleTeqCrate = abstract member Apply : TripleTeqEvaluator<'a, 'ret> -> 'ret

[<RequireQualifiedAccess>]
module TripleTeqCrate =

    let make () =
            { new TripleTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Tuple ts when ts.Length = 3 ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a TripleTeqCrate> |> Some
        | _ -> None
