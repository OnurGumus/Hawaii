[<AutoOpen>]
module Domain

open Fantomas.FCS.Syntax
open Newtonsoft.Json.Linq

[<RequireQualifiedAccess>]
type EmptyDefinitionResolution =
    | Ignore
    | GenerateFreeForm

[<RequireQualifiedAccess>]
/// <summary>Describes the compilation target</summary>
type Target =
    | FSharp
    | Fable

/// <summary>Describes the async return type of the functions of the generated clients</summary>
[<RequireQualifiedAccess>]
type AsyncReturnType =
    | Async
    | Task

[<RequireQualifiedAccess>]
type FactoryFunction =
    | Create
    | None

type CodegenConfig = {
    schema: string
    output: string
    target: Target
    project : string
    asyncReturnType: AsyncReturnType
    synchronous: bool
    resolveReferences: bool
    emptyDefinitions: EmptyDefinitionResolution
    overrideSchema: JToken option
    filterTags: string list
    odataSchema: bool
    /// Operation ids whose binary response should be generated as a
    /// `System.IO.Stream` instead of `byte[]`. Only honored by the .NET
    /// (`fsharp`) target; ignored by the Fable target. Default: empty.
    streamingOperations: string list
    /// When true, every generated operation also returns the HTTP response
    /// headers as `(string * string) list`. Default: false.
    responseHeaders: bool
}

type OperationParameter = {
    parameterName: string
    parameterIdent: string
    required: bool
    parameterType: SynType
    docs : string
    location: string
    style: string
    properties: string list
}