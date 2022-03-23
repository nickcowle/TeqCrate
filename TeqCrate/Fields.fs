namespace TeqCrate

open System.Reflection

[<NoComparison ; NoEquality>]
type TypeField =
    {
        Name : string
        Attributes : CustomAttributeData list
    }

[<RequireQualifiedAccess>]
module TypeField =

    let name (field : TypeField) : string = field.Name

    let attributes (field : TypeField) : CustomAttributeData list = field.Attributes
