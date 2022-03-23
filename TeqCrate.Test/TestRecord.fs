namespace TeqCrate.Test

open System

open Xunit
open FsUnitTyped

open TeqCrate
open TeqCrate.Patterns

module TestRecord =

    [<AttributeUsage(AttributeTargets.Property, Inherited = false)>]
    type Foo() =
        inherit Attribute()

    [<AttributeUsage(AttributeTargets.All, Inherited = false)>]
    type Bar() =
        inherit Attribute()

    type RecordType =
        {
            Field1 : string
            [<Foo>]
            Field2 : int
            [<Bar>]
            Field3 : int64
            [<Foo ; Bar>]
            Field4 : int64
        }

    [<Fact>]
    let ``Custom attributes are populated correctly for record fields`` () =
        let data =
            match tType<RecordType> with
            | Record data -> data
            | _ -> failwith "Unexpected type"

        let fields =
            { new RecordConvEvaluator<_, _> with
                member _.Eval (fields : TypeField list) _ _ = fields
            }
            |> data.Apply

        let attributes =
            fields
            |> List.map (fun field ->
                // The F# compiler puts in CompilationMappingAttribute on each of these fields; we're not interested in that.
                let attributes =
                    field.Attributes
                    |> List.filter (fun attr ->
                        attr.AttributeType
                        <> typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>
                    )

                field.Name, attributes
            )
            |> Map.ofList

        attributes.Count |> shouldEqual 4

        attributes.["Field1"] |> shouldBeEmpty

        attributes.["Field2"]
        |> List.exactlyOne
        |> fun data -> data.AttributeType
        |> shouldEqual typeof<Foo>

        attributes.["Field3"]
        |> List.exactlyOne
        |> fun data -> data.AttributeType
        |> shouldEqual typeof<Bar>

        attributes.["Field4"]
        |> List.map (fun data -> data.AttributeType)
        |> List.sortBy (fun ty -> ty.Name)
        |> shouldEqual [ typeof<Bar> ; typeof<Foo> ]
