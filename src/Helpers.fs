[<AutoOpen>]
module Helpers

open System
open System.Linq
open System.Net.Http
open System.Text.Json.Nodes
open Microsoft.OpenApi

let inline isNotNull (x: 't) = not (isNull x)

let capitalize (input: string) =
    if String.IsNullOrWhiteSpace input
    then ""
    else input.First().ToString().ToUpper() + String.Join("", input.Skip(1))

let camelCase (input: string) =
    if String.IsNullOrWhiteSpace input
    then ""
    else input.First().ToString().ToLower() + String.Join("", input.Skip(1))

let normalizeFullCaps (input: string) =
    let fullCaps =
        input |> Seq.forall Char.IsUpper

    if fullCaps
    then input.ToLower()
    else input

let sanitizeTypeName (typeName: string) =
    if String.IsNullOrWhiteSpace typeName then
        typeName
    else
        // drop a generic-arity marker first (e.g. "KeyValuePair`2" -> "KeyValuePair")
        let withoutArity =
            if typeName.Contains "`" then
                match typeName.Split '`' with
                | [| name; _typeArgArity |] -> name
                | _ -> typeName.Replace("`", "")
            else
                typeName
        // then strip every separator character, regardless of order or combination
        // (e.g. "workers-kv_key_name" -> "workerskvkeyname")
        [ "."; "_"; "-"; "["; "]" ]
        |> List.fold (fun (acc: string) separator -> acc.Replace(separator, "")) withoutArity

let invalidTitle (title: string) =
    String.IsNullOrWhiteSpace title
    || (title.Contains "Mediatype identifier" && title.Contains "application/")
    || (title.Split(' ').Length >= 1)

// ----------------------------------------------------------------------------
// Microsoft.OpenApi 3.x model helpers
//
// In the 3.x object model schemas/parameters/responses are interfaces and a
// `$ref` is a distinct `*Reference` implementation that proxies its members to
// the resolved target. `JsonSchemaType` is a [<Flags>] enum so OpenAPI 3.1 can
// express `type: ["string", "null"]`. These helpers translate that model back
// into the shapes the code generator works with.
// ----------------------------------------------------------------------------

/// The `$ref` id of a schema when it is a reference, otherwise null.
let schemaReferenceId (schema: IOpenApiSchema) : string =
    match box schema with
    | :? OpenApiSchemaReference as reference when isNotNull reference.Reference -> reference.Reference.Id
    | _ -> null

/// True when the schema is a `$ref`.
let isSchemaReference (schema: IOpenApiSchema) : bool =
    isNotNull (schemaReferenceId schema)

/// The `type` of a schema as a nullable, tolerating `$ref`s whose target could
/// not be resolved (accessing `.Type` on such a reference would otherwise throw).
let schemaTypeFlags (schema: IOpenApiSchema) : Nullable<JsonSchemaType> =
    if isNull (box schema) then
        Nullable()
    else
        match box schema with
        | :? OpenApiSchemaReference as reference when reference.UnresolvedReference || isNull (box reference.Target) ->
            Nullable()
        | _ -> schema.Type

/// The primary JSON Schema type as the legacy lowercase string
/// ("integer", "number", "string", "boolean", "object", "array"), or null.
/// The OpenAPI 3.1 `null` type flag is masked out - see `schemaIsNullable`.
let schemaTypeName (schema: IOpenApiSchema) : string =
    let schemaType = schemaTypeFlags schema
    if not schemaType.HasValue then
        null
    else
        match schemaType.Value &&& ~~~JsonSchemaType.Null with
        | JsonSchemaType.Integer -> "integer"
        | JsonSchemaType.Number -> "number"
        | JsonSchemaType.String -> "string"
        | JsonSchemaType.Boolean -> "boolean"
        | JsonSchemaType.Object -> "object"
        | JsonSchemaType.Array -> "array"
        | _ -> null

/// True when the schema's type includes `null` (OpenAPI 3.1 type arrays, or a
/// 3.0 `nullable: true` which the reader normalizes into the type flags).
let schemaIsNullable (schema: IOpenApiSchema) : bool =
    let schemaType = schemaTypeFlags schema
    schemaType.HasValue && schemaType.Value.HasFlag JsonSchemaType.Null

/// Reads a JSON value node as a string when possible.
let nodeAsString (node: JsonNode) : string option =
    match node with
    | :? JsonValue as value ->
        match value.TryGetValue<string>() with
        | true, text -> Some text
        | _ -> None
    | _ -> None

/// Reads a JSON value node as an int when possible.
let nodeAsInt (node: JsonNode) : int option =
    match node with
    | :? JsonValue as value ->
        match value.TryGetValue<int>() with
        | true, number -> Some number
        | _ -> None
    | _ -> None

/// Reads a JSON value node as a bool when possible.
let nodeAsBool (node: JsonNode) : bool option =
    match node with
    | :? JsonValue as value ->
        match value.TryGetValue<bool>() with
        | true, flag -> Some flag
        | _ -> None
    | _ -> None

/// The string payload of a specification extension (`x-*`), when it is a string.
let extensionString (extension: IOpenApiExtension) : string option =
    match extension with
    | :? JsonNodeExtension as node when isNotNull node.Node -> nodeAsString node.Node
    | _ -> None

/// The bool payload of a specification extension (`x-*`), when it is a bool.
let extensionBool (extension: IOpenApiExtension) : bool option =
    match extension with
    | :? JsonNodeExtension as node when isNotNull node.Node -> nodeAsBool node.Node
    | _ -> None

/// The string-array payload of a specification extension (`x-*`), when it is an array.
let extensionStringArray (extension: IOpenApiExtension) : string list option =
    match extension with
    | :? JsonNodeExtension as node ->
        match node.Node with
        | :? JsonArray as array -> array |> Seq.choose (fun item -> nodeAsString item) |> Seq.toList |> Some
        | _ -> None
    | _ -> None

/// The HTTP method as a capitalized name ("Get", "Post", ...) for identifier generation.
let httpMethodName (method: HttpMethod) : string =
    capitalize (method.Method.ToLowerInvariant())

/// Wraps a raw value as a specification extension.
let stringExtension (value: string) : IOpenApiExtension =
    JsonNodeExtension(JsonValue.Create value) :> IOpenApiExtension

let isEmptySchema (schema: IOpenApiSchema) =
    if isNull (box schema) then
        true
    else
        match box schema with
        | :? OpenApiSchemaReference as reference when reference.UnresolvedReference || isNull (box reference.Target) ->
            // an unresolved $ref cannot be inspected - treat it as a named, non-empty type
            false
        | _ ->
            (let typeName = schemaTypeName schema in isNull typeName || typeName = "object")
            && schema.Properties.Count = 0
            && schema.AllOf.Count = 0
            && schema.AnyOf.Count = 0
            && (isNull schema.OneOf || schema.OneOf.Count = 0)
