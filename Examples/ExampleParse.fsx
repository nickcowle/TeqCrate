#load "CsvParser.fsx"

open System
open System.IO

type MyRecord =
    {
        Id : int
        Name : string
        DateOfBirth : DateTime
        NewUser : bool
        Balance: float
    }

Path.Combine(__SOURCE_DIRECTORY__, "TestData.csv")
|> CsvParser.tryParse<MyRecord>
|> Option.get
|> Seq.iter (printfn "%A")
