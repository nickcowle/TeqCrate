namespace TeqCrate.Test

open System

open Xunit
open FsUnitTyped

open TeqCrate
open TeqCrate.Patterns

module TestUnion =

    [<AttributeUsage(AttributeTargets.Property, Inherited = true)>]
    type Foo() =
        inherit Attribute()

    [<AttributeUsage(AttributeTargets.All, Inherited = false)>]
    type Bar() =
        inherit Attribute()

    type UnionType =
        | Case1 of string
        | [<Foo>] Case2 of int
        | [<Bar>] Case3
        | [<Foo ; Bar>] Case4 of int64

    [<Fact>]
    let ``Custom attributes are populated correctly for union cases`` () =
        let data =
            match tType<UnionType> with
            | Union data -> data
            | _ -> failwith "Unexpected type"

        let fields =
            { new UnionConvEvaluator<UnionType, _> with
                member _.Eval (fields : UnionTypeField list) _ _ = fields
            }
            |> data.Apply

        let attributes =
            fields
            |> List.map (fun field ->
                // The F# compiler puts in a bunch of attributes on each of these cases; we're not interested in that.
                let attributes =
                    field.Attributes
                    |> List.filter (fun attr ->
                        [
                            typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>
                            typeof<System.SerializableAttribute>
                            typeof<System.Diagnostics.DebuggerDisplayAttribute>
                        ]
                        |> List.forall (fun t -> attr.AttributeType <> t)
                    )

                field.Name, attributes
            )
            |> Map.ofList

        attributes.Count |> shouldEqual 4

        attributes.["Case1"] |> shouldBeEmpty

        attributes.["Case2"]
        |> List.exactlyOne
        |> fun data -> data.AttributeType
        |> shouldEqual typeof<Foo>

        attributes.["Case3"]
        |> List.exactlyOne
        |> fun data -> data.AttributeType
        |> shouldEqual typeof<Bar>

        attributes.["Case4"]
        |> List.map (fun data -> data.AttributeType)
        |> List.sortBy (fun ty -> ty.Name)
        |> shouldEqual [ typeof<Bar> ; typeof<Foo> ]
