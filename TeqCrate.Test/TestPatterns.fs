namespace TeqCrate.Test

open HCollections
open TeqCrate
open TeqCrate.Patterns
open TypeEquality
open Xunit

[<RequireQualifiedAccess>]
module TestPatterns =

    [<Fact>]
    let ``Teq active pattern distinguishes int from string`` () =

        let t1 = tType<int>
        let t2 = tType<string>

        match t1 with
        | Teq t2 teq ->
            Assert.True false
        | _ ->
            Assert.True true

    let tryGetArrayLength (arr : 'a) : int option =
        match tType<'a> with
        | Array c ->
            c.Apply
                { new ArrayTeqCrateEvaluator<_,_> with
                    member __.Eval teq = (Teq.castTo teq arr).Length |> Some
                }
        | _ -> None

    [<Fact>]
    let ``Array active pattern recognises an array`` () =

        let arr = [| "foo" ; "bar" |]
        Assert.Equal (Some 2, tryGetArrayLength arr)

    let tryGetListLength (xs : 'a) : int option =
        match tType<'a> with
        | List c ->
            c.Apply
                { new ListTeqCrateEvaluator<_,_> with
                    member __.Eval teq = xs |> Teq.castTo teq |> List.length |> Some
                }
        | _ -> None

    [<Fact>]
    let ``List active pattern recognises a list`` () =

        let xs = [ 1..10 ]
        Assert.Equal (Some 10, tryGetListLength xs)

    let tryGetMapCount (map : 'a) : int option =
        match tType<'a> with
        | Map c ->
            c.Apply
                { new MapTeqCrateEvaluator<_,_> with
                    member __.Eval teq = map |> Teq.castTo teq |> Map.count |> Some
                }
        | _ -> None

    [<Fact>]
    let ``Map active pattern recognises a map`` () =

        let map = Map.empty |> Map.add "foo" 3 |> Map.add "bar" 12
        Assert.Equal (Some 2, tryGetMapCount map)

    [<Fact>]
    let ``Tuple active pattern recognises a tuple`` () =

        let tuple = 5, "hello", false, 8, 2
        let sumOfInts = Tuple.tryFoldTuple (HListFolder.makeElementFolder (+)) 0 tuple
        Assert.Equal (Some 15, sumOfInts)

    [<Fact>]
    let ``Fun active pattern recognises a function`` () =

        match tType<int -> string> with
        | Fun c ->
            let dom, ran =
                c.Apply
                    { new FunTeqCrateEvaluator<_,_> with
                        member __.Eval (teq : Teq<int -> string, 'a -> 'b>) =
                            typeof<'a>, typeof<'b>
                    }

            Assert.Equal(typeof<int>, dom)
            Assert.Equal(typeof<string>, ran)

        | _ -> Assert.True false

    [<Fact>]
    let ``Pair active pattern recognises a pair`` () =

        match tType<int * string> with
        | Pair c ->
            let t1, t2 =
                c.Apply
                    { new PairTeqCrateEvaluator<_,_> with
                        member __.Eval (teq : Teq<int * string, 'a * 'b>) =
                            typeof<'a>, typeof<'b>
                    }

            Assert.Equal(typeof<int>, t1)
            Assert.Equal(typeof<string>, t2)

        | _ -> Assert.True false

    [<Fact>]
    let ``Triple active pattern recognises a triple`` () =

        match tType<int * string * bool> with
        | Triple c ->
            let t1, t2, t3 =
                c.Apply
                    { new TripleTeqCrateEvaluator<_,_> with
                        member __.Eval (teq : Teq<int * string * bool, 'a * 'b * 'c>) =
                            typeof<'a>, typeof<'b>, typeof<'c>
                    }

            Assert.Equal(typeof<int>, t1)
            Assert.Equal(typeof<string>, t2)
            Assert.Equal(typeof<bool>, t3)

        | _ -> Assert.True false

    type TestRecord =
        {
            Foo : string
            Bar : int
            Baz : string
        }

    let tryGetStringKeyValues (record : 'a) : Map<string, string> option =
        match tType<'a> with
        | Record c ->
            c.Apply
                { new RecordConvCrateEvaluator<_,_> with
                    member __.Eval names conv =

                        let folder =
                            let f (names : string list, map) (value : string option) =
                                let map =
                                    match value with
                                    | Some v -> Map.add (names |> List.head) v map
                                    | None -> map
                                names |> List.tail, map
                            HListFolder.makeGappedElementFolder f

                        record |> conv.To |> HList.fold folder (names, Map.empty) |> snd
                }
                |> Some
        | _ -> None

    [<Fact>]
    let ``Record active pattern recognises a record`` () =

        let r = { Foo = "hello"; Bar = 1234 ; Baz = "world" }
        let pairs = tryGetStringKeyValues r |> Option.map (Map.toSeq >> Seq.sort >> List.ofSeq)
        let expected = Some [ "Baz", "world" ; "Foo", "hello" ]

        Assert.Equal(expected, pairs)

    type TestUnion =
    | Foo
    | Bar of int * string * bool
    | Baz of string * float
    | Quux of string

    [<Fact>]
    let ``Union active pattern recognises a union`` () =

        let testValue = Bar (1234, "test", true)

        let result =
            match tType<TestUnion> with
            | Union c ->
                c.Apply
                    { new UnionConvCrateEvaluator<_,_> with
                        member __.Eval names (conv : Conv<TestUnion, 'a HUnion>) =

                            let expectedNames = [ "Foo" ; "Bar" ; "Baz" ; "Quux" ]
                            Assert.Equal<string list>(expectedNames, names)

                            let expectedUnionType = tType<(unit -> (int * string * bool) -> (string * float) -> string -> unit) HUnion>
                            match tType<'a HUnion> with
                            | Teq expectedUnionType teq ->
                                let converted = testValue |> conv.To |> Teq.castTo teq
                                match HUnion.split converted with
                                | Choice1Of2 v -> false
                                | Choice2Of2 union ->
                                    match HUnion.split union with
                                    | Choice1Of2 hList -> true
                                    | Choice2Of2 _ -> false
                            | _ ->
                                false
                    }
            | _ ->
                false

        Assert.True result
