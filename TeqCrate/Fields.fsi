namespace TeqCrate

open System.Reflection

/// A field in a record or discriminated union.
[<NoComparison ; NoEquality>]
type TypeField =
    {
        /// The name of the field; for example, the name of this very field is "Name".
        Name : string
        /// Any attributes that were present on the field.
        Attributes : CustomAttributeData list
    }

[<RequireQualifiedAccess>]
module TypeField =

    /// Get the Name contained in a TypeField.
    val name : TypeField -> string

    /// Get the Attributes contained in a TypeField.
    val attributes : TypeField -> CustomAttributeData list
