open System
open Microsoft.OpenApi
open Microsoft.OpenApi.Reader
open System.Net.Http
open FsAst
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open System.Text.Json.Nodes
open System.IO
open System.Xml.Linq
open System.Net
open System.Collections.Generic
open Newtonsoft.Json.Linq
open System.Text
open Microsoft.OpenApi.OData
open Microsoft.OData.Edm.Csdl
open System.Text.Json
open Newtonsoft.Json

let logo = """

     _   _                     _ _
    | | | |                   (_|_)
    | |_| | __ ___      ____ _ _ _
    |  _  |/ _` \ \ /\ / / _` | | |
    | | | | (_| |\ V  V / (_| | | |
    \_| |_/\__,_| \_/\_/ \__,_|_|_|


    ❤️  Open source https://www.github.com/Zaid-Ajaj/Hawaii
    ⚖️  MIT LICENSE
"""

let resolveFile (path: string) =
    if Path.IsPathRooted path then
        path
    elif System.Diagnostics.Debugger.IsAttached then
        Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, path))
    else
        Path.GetFullPath (Path.Combine(Environment.CurrentDirectory, path))

let resolveRelativeFile current (path: string) =
    if Path.IsPathRooted path then
        path
    else
        Path.GetFullPath (Path.Combine(current, path))

let safeSeq (xs: seq<'a>) =
    if isNull xs
    then Seq.empty
    else xs

let readConfig file =
    try
        if not (File.Exists file) then
            Error $"Hawaii configuration file {file} was not found"
        else
        let content = File.ReadAllText(file)
        let parts = JObject.Parse(content)
        if not (parts.ContainsKey "schema") then
            Error "Missing required configuration element 'schema'"
        elif isNotNull parts.["schema"] && parts.["schema"].Type <> JTokenType.String then
            Error "Configuration element 'schema' must be a string"
        elif not (parts.ContainsKey "output") then
            Error "Missing required configuration element 'output'"
        elif not (parts.ContainsKey "project") then
            Error "Missing required configuration element 'project'"
        elif isNotNull parts.["output"] && parts.["output"].Type <> JTokenType.String then
            Error "The 'output' configuration element must be a string"
        elif isNotNull parts.["target"] && parts.["target"].Type <> JTokenType.String then
            Error "The 'target' configuration element must be a string"
        elif isNotNull parts.["target"] && parts.["target"].ToObject<string>().ToLower().Trim() <> "fable" && parts.["target"].ToObject<string>().ToLower().Trim() <> "fsharp" then
            Error "The 'target' configuration element can only be 'fable' or 'fsharp'"
        elif isNotNull parts.["project"] && parts.["project"].Type <> JTokenType.String then
            Error "The 'project' configuration element must be a string"
        elif isNotNull parts.["project"] && String.IsNullOrWhiteSpace(parts.["project"].ToString().Trim()) then
            Error "The 'project' configuration element cannot be empty"
        elif isNotNull parts.["asyncReturnType"] && parts.["asyncReturnType"].Type <> JTokenType.String then
            Error "The 'asyncReturnType' configuration element must be a string"
        elif isNotNull parts.["asyncReturnType"] && parts.["asyncReturnType"].ToObject<string>().ToLower().Trim() <> "task" && parts.["asyncReturnType"].ToObject<string>().ToLower().Trim() <> "async" then
            Error "The 'asyncReturnType' configuration element can only be 'async' (default) or 'task'"
        elif isNotNull parts.["synchronous"] && parts.["synchronous"].Type <> JTokenType.Boolean then
            Error "The 'synchronous' configuration element must be a boolean"
        elif isNotNull parts.["resolveReferences"] && parts.["resolveReferences"].Type <> JTokenType.Boolean then
            Error "The 'resolveReferences' configuration element must be a boolean"
        elif isNotNull parts.["emptyDefinitions"] && parts.["emptyDefinitions"].ToObject<string>().ToLower().Trim() <> "ignore" && parts.["emptyDefinitions"].ToObject<string>().ToLower().Trim() <> "free-form" then
            Error "The 'emptyDefinitions' configuration element must either be 'ignore' or 'free-form'"
        elif isNotNull parts.["streamingOperations"] && parts.["streamingOperations"].Type <> JTokenType.Array then
            Error "The 'streamingOperations' configuration element must be an array of strings"
        elif isNotNull parts.["responseHeaders"] && parts.["responseHeaders"].Type <> JTokenType.Boolean then
            Error "The 'responseHeaders' configuration element must be a boolean"
        else
            let configParent = Path.GetDirectoryName file
            Ok {
                schema = parts.["schema"].ToObject<string>()
                output = resolveRelativeFile configParent (parts.["output"].ToObject<string>())
                project = parts.["project"].ToString().Replace("(", "").Replace(")", "")
                target =
                    if isNotNull parts.["target"] && parts.["target"].ToString() = "fable"
                    then Target.Fable
                    else Target.FSharp
                asyncReturnType =
                    if isNotNull parts.["asyncReturnType"] && parts.["asyncReturnType"].ToString() = "task"
                    then AsyncReturnType.Task
                    else AsyncReturnType.Async
                synchronous =
                    if isNotNull parts.["synchronous"]
                    then parts.["synchronous"].ToObject<bool>()
                    else false
                resolveReferences =
                    if isNotNull parts.["resolveReferences"]
                    then parts.["resolveReferences"].ToObject<bool>()
                    else false
                emptyDefinitions =
                    if isNotNull parts.["emptyDefinitions"] && parts.["emptyDefinitions"].ToString() = "free-form"
                    then EmptyDefinitionResolution.GenerateFreeForm
                    else EmptyDefinitionResolution.Ignore
                overrideSchema =
                    if isNotNull parts.["overrideSchema"]
                    then Some parts.["overrideSchema"]
                    else None
                filterTags =
                    if isNotNull parts.["filterTags"] && parts.["filterTags"].Type = JTokenType.Array
                    then [
                            for tag in unbox<JArray> parts.["filterTags"] do
                            if tag.Type = JTokenType.String then
                                tag.ToObject<string>() ]
                    else [ ]
                odataSchema = false
                streamingOperations =
                    if isNotNull parts.["streamingOperations"] && parts.["streamingOperations"].Type = JTokenType.Array
                    then [
                            for operationId in unbox<JArray> parts.["streamingOperations"] do
                            if operationId.Type = JTokenType.String then
                                operationId.ToObject<string>() ]
                    else [ ]
                responseHeaders =
                    if isNotNull parts.["responseHeaders"]
                    then parts.["responseHeaders"].ToObject<bool>()
                    else false
            }
    with
    | error ->
        Error $"Error ocurred while reading the configuration file: {error.Message}"

let escapeDocs (value: string) = value.Replace(">", "&gt;").Replace("<", "&lt;").Replace("&", "&amp;")

let xmlDocs (description: string) =
    if String.IsNullOrWhiteSpace description then
        PreXmlDoc.Create [ ]
    else
        description.Split("\r\n")
        |> Seq.collect (fun line -> line.Split("\n"))
        |> Seq.filter (fun line -> not (String.IsNullOrWhiteSpace line))
        |> Seq.map escapeDocs
        |> PreXmlDoc.Create

let xmlDocsWithParams (description: string) (parameters: (string * string) seq) =
    if String.IsNullOrWhiteSpace description then
        PreXmlDoc.Create [ ]
    else
        description.Split("\r\n")
        |> Seq.collect (fun line -> line.Split("\n"))
        |> Seq.filter (fun line -> not (String.IsNullOrWhiteSpace line))
        |> Seq.map escapeDocs
        |> fun summary ->
            let containsParamDocs =
                parameters
                |> Seq.map snd
                |> Seq.exists (fun docs -> not (String.IsNullOrWhiteSpace docs))
            PreXmlDoc.Create [
                yield "<summary>"
                yield! summary
                yield "</summary>"
                if containsParamDocs then
                    for (param, paramDocs) in parameters do
                        if not (String.IsNullOrWhiteSpace paramDocs) then
                            let docs =
                                paramDocs.Split "\r\n"
                                |> Seq.collect (fun line -> line.Split("\n"))
                                |> Seq.filter (fun line -> not (String.IsNullOrWhiteSpace line))
                                |> Seq.map escapeDocs

                            if Seq.length docs = 1 then
                                yield $"<param name=\"{param}\">{Seq.head docs}</param>"
                            else
                                yield $"<param name=\"{param}\">"
                                yield! docs
                                yield "</param>"
                        else
                            yield $"<param name=\"{param}\"></param>"
            ]

let client = new HttpClient()

let simplifyRedundantSchemaParts (schema: JObject) =
    let rec iterate (part: JObject) =
        let properties = List.ofSeq(part.Properties())
        for property in properties do
            if property.Name.StartsWith "application/vnd" && property.Name.EndsWith "+json" && property.Value.Type = JTokenType.Object && not (part.ContainsKey "application/json") then
                // rewrite JSON-like media types into application/json
                let mediaType = unbox<JObject> property.Value
                part.Add("application/json", mediaType)
                part.Remove(property.Name) |> ignore
            elif property.Name = "application/ld+json" && property.Value.Type = JTokenType.Object && not (part.ContainsKey "application/json") then
                // rewrite JSON-like media types into application/json
                let mediaType = unbox<JObject> property.Value
                part.Add("application/json", mediaType)
                part.Remove(property.Name) |> ignore
            elif property.Name = "anyOf" && property.Value.Type = JTokenType.Array then
                // simplify this shape
                // { anyOf: [ first ] }
                // into
                // { ...first }
                let anyOfArray = unbox<JArray> property.Value
                if anyOfArray.Count = 1 && anyOfArray.[0].Type = JTokenType.Object then
                    let innerObject = unbox<JObject> anyOfArray.[0]
                    for innerProp in innerObject.Properties() do
                        part.Add(innerProp)
                    part.Remove("anyOf") |> ignore
            elif property.Name = "oneOf" && property.Value.Type = JTokenType.Array then
                // simplify this shape
                // { oneOf: [ first ] }
                // into
                // { ...first }
                let oneOfArray = unbox<JArray> property.Value
                if oneOfArray.Count = 1 && oneOfArray.[0].Type = JTokenType.Object then
                    let innerObject = unbox<JObject> oneOfArray.[0]
                    for innerProp in innerObject.Properties() do
                        part.Add(innerProp)
                    part.Remove("oneOf") |> ignore
            elif property.Name = "allOf" && property.Value.Type = JTokenType.Array then
                // simplify this shape
                // { allOf: [ first, { "example": ... } ] }
                // into
                // { ...first }
                let allOfArray = unbox<JArray> property.Value
                if allOfArray.Count = 2 && allOfArray.[0].Type = JTokenType.Object && allOfArray.[1].Type = JTokenType.Object then
                    let firstObject = unbox<JObject> allOfArray.[0]
                    let secondObject = unbox<JObject> allOfArray.[1]
                    if secondObject.Count = 1 && secondObject.ContainsKey "example" then
                        for innerProp in firstObject.Properties() do
                            part.Add(innerProp)
                        part.Remove("allOf") |> ignore
                    else
                        for element in allOfArray do
                            if element.Type = JTokenType.Object then
                                iterate (unbox<JObject> element)
                else
                    for element in allOfArray do
                        if element.Type = JTokenType.Object then
                            iterate (unbox<JObject> element)
            elif property.Value.Type = JTokenType.Array then
                let elements = unbox<JArray> property.Value
                for element in elements do
                    if element.Type = JTokenType.Object then
                        iterate (unbox<JObject> element)
            if property.Value.Type = JTokenType.Object then
                iterate (unbox<JObject> property.Value)

    iterate schema
    schema

let readExternalODataSchema (schemaUrl: string) =
    let content =
        schemaUrl
        |> client.GetStringAsync
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let odataModel = CsdlReader.Parse(XElement.Parse(content).CreateReader())
    let openApiModel = odataModel.ConvertToOpenApi();
    use stringTextWriter = new StringWriter()
    let writer = OpenApiJsonWriter(stringTextWriter)
    openApiModel.SerializeAsV3(writer);
    stringTextWriter.ToString()

let readLocalODataSchema (schemaUrl: string) =
    let content = File.ReadAllText schemaUrl
    let odataModel = CsdlReader.Parse(XElement.Parse(content).CreateReader())
    let openApiModel = odataModel.ConvertToOpenApi();
    use stringTextWriter = new StringWriter()
    let writer = OpenApiJsonWriter(stringTextWriter)
    openApiModel.SerializeAsV3(writer);
    stringTextWriter.ToString()

let getSchema(schema: string) (overrideSchema: JToken option) =
    let schemaContents =
        if File.Exists schema && schema.EndsWith ".json" then
            let content = File.ReadAllText schema
            JObject.Parse(content)
        elif File.Exists schema && schema.EndsWith ".xml" then
            Console.WriteLine "Detected local OData schema"
            let openApiJson = readLocalODataSchema schema
            JObject.Parse openApiJson
        elif schema.StartsWith "http" && schema.EndsWith "$metadata" then
            Console.WriteLine "Detected external OData schema"
            let openApiJson = readExternalODataSchema schema
            JObject.Parse openApiJson
        elif schema.StartsWith "http" then
            let content =
                client.GetStringAsync(schema)
                |> Async.AwaitTask
                |> Async.RunSynchronously
            JObject.Parse(content)
        else
            // assume the schema is coming in as a string
            // convert it into a memory stream
            // this is useful for unit tests
            JObject.Parse(schema)

    match overrideSchema with
    | None -> ()
    | Some miniSchema -> schemaContents.Merge(miniSchema)

    // Pre-process NSwag schemas and add { "produces": ["application/json"] } if missing for operations
    // we assume Hawaii is working with schemas that produce JSON
    if schemaContents.ContainsKey "x-generator" && schemaContents.["x-generator"].ToObject<string>().StartsWith "NSwag" then
        if schemaContents.ContainsKey "paths" && schemaContents.["paths"].Type = JTokenType.Object then
            let pathsObject = unbox<JObject> schemaContents.["paths"]
            for path in pathsObject.Properties() do
                if path.Value.Type = JTokenType.Object then
                    let operations = unbox<JObject> path.Value
                    for operation in operations.Properties() do
                        if operation.Value.Type = JTokenType.Object then
                            let operationAsObject = unbox<JObject> operation.Value
                            if not (operationAsObject.ContainsKey "produces") then
                                operationAsObject.Add(JProperty("produces", [| "application/json" |]))

    let simplified = simplifyRedundantSchemaParts schemaContents
    let simplifiedContents = simplified.ToString()
    if simplifiedContents.Contains "#/components/schemas/odata.error" then
        simplified.Add(JProperty("x-odata", true))
        let schemaBytes = System.Text.Encoding.UTF8.GetBytes(simplified.ToString())
        new MemoryStream(schemaBytes) :> Stream
    else
        let schemaBytes = System.Text.Encoding.UTF8.GetBytes simplifiedContents
        new MemoryStream(schemaBytes) :> Stream

let nextTick (name: string) (visited: ResizeArray<string>) =
    if not (visited.Contains name) then
        name
    else
    visited
    |> Seq.toList
    |> List.filter (fun visitedName -> visitedName.StartsWith name)
    |> List.map (fun visitedName -> visitedName.Replace(name, ""))
    |> List.choose(fun rest ->
        match Int32.TryParse rest with
        | true, n -> Some n
        | _ -> None)
    |> function
        | [ ] -> name + "1"
        | ns -> name + (string (List.max ns + 1))

let findNextTypeName fieldName objectName (selections: string list) (visitedTypes: ResizeArray<string>) (isGlobalRef: bool) =
    let nestedSelectionType =
        selections
        |> List.map capitalize
        |> String.concat "And"

    if not (visitedTypes.Contains objectName) && not isGlobalRef then
        objectName
    elif not (visitedTypes.Contains (capitalize fieldName)) && not isGlobalRef then
        capitalize fieldName
    elif not (visitedTypes.Contains (objectName + capitalize fieldName)) && not isGlobalRef then
        objectName + capitalize fieldName
    elif not (visitedTypes.Contains nestedSelectionType) && selections.Length <= 3 && selections.Length > 1 then
        nestedSelectionType
    elif not (visitedTypes.Contains (capitalize fieldName + "From" + objectName)) then
        capitalize fieldName + "From" + objectName
    else
        nextTick (capitalize fieldName + "From" + objectName) visitedTypes

let findNextEnumTypeName (fieldName: string) objectName (visitedTypes: ResizeArray<string>) =
    let fieldName =
        if fieldName.Contains "." then
            fieldName.Split('.')
            |> Array.map capitalize
            |> String.concat ""
        elif fieldName.Contains " " then
            fieldName.Split(' ')
            |> Array.map capitalize
            |> String.concat ""
        else
            fieldName

    if not (visitedTypes.Contains (capitalize fieldName)) then
        capitalize fieldName
    elif not (visitedTypes.Contains (objectName + capitalize fieldName)) then
        objectName + capitalize fieldName
    elif not (visitedTypes.Contains (capitalize fieldName + "From" + objectName)) then
        capitalize fieldName + "From" + objectName
    else
        nextTick (capitalize fieldName + "From" + objectName) visitedTypes

let isEnumType (schema: IOpenApiSchema) =
    (let schemaType = schemaTypeName schema in schemaType = "string" || schemaType = "integer" || String.IsNullOrEmpty schemaType)
    && not (isNull schema.Enum)
    && schema.Enum.Count > 0

let (|StringEnum|_|) (schema: IOpenApiSchema) =
    if isEnumType schema then
        let cases =
            schema.Enum
            |> Seq.choose (fun enumCase -> nodeAsString enumCase)

        let containsDigitsOnly (case: string) =
            Seq.forall Char.IsDigit case

        let allDigitCases =
            Seq.forall containsDigitsOnly cases

        let containsQoutes =
            cases |> Seq.exists (fun case -> case.Contains "\"")

        let containsTimeZone =
            cases |> Seq.exists (fun case -> case.Contains "00Z")

        if not (Seq.isEmpty cases) && not allDigitCases && not containsQoutes && not containsTimeZone then
            Some (Seq.toList cases)
        else
            None
    else
        None

let rec cleanOperationName (operationName: string) =
    let operation = operationName.Replace("{", "").Replace("}", "")

    if operation.Contains "?" then
        match operation.Split '?' with
        | [| path; parameters |] ->
            let queryParams =
                parameters.Split([|'&'; '=' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun part -> part.Replace("{", "").Replace("}", ""))
                |> Array.distinctBy id
                |> Array.map capitalize
                |> String.concat "And"
            cleanOperationName (path + "By" + queryParams)
        | _ ->
            operation.Split([| '?'; '='; '&' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map capitalize
            |> String.concat ""
            |> cleanOperationName
    else
        let invalidChars = [| '-'; '#'; '_'; '.'; '+'; '$'; '&'; '['; ']'; '/'; '\\'; '*'; '"'; '`' |]
        operation.Split(invalidChars, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map capitalize
        |> String.concat ""

let rec deriveOperationName (operationName: string) (path: string) (operationType: HttpMethod) (visitedTypes: ResizeArray<string>) =
    if not (String.IsNullOrWhiteSpace operationName) then
        if not (visitedTypes.Contains (cleanOperationName operationName)) then
           cleanOperationName operationName
        elif not (visitedTypes.Contains (httpMethodName operationType + cleanOperationName operationName)) then
            httpMethodName operationType + cleanOperationName operationName
        else
            deriveOperationName "" path operationType visitedTypes
    else
        let parts = path.Split("/")
        let parameters =
            parts
            |> Array.filter (fun part -> part.Contains "{" && part.Contains "}")
            |> Array.map (fun part -> part.Replace("{", "").Replace("}", ""))
            |> Array.map capitalize
            |> Array.map cleanOperationName
            |> String.concat "And"

        let segments =
            parts
            |> Array.filter (fun part -> not (part.Contains "{" && part.Contains "}"))
            |> Array.mapi (fun index part ->
                if index <> 0
                then cleanOperationName (capitalize part)
                else cleanOperationName part)
            |> String.concat ""

        if String.IsNullOrEmpty parameters then
            cleanOperationName (httpMethodName operationType + segments)
        else
            cleanOperationName (httpMethodName operationType + segments + "By" + parameters)

let deriveMemberName (operationName: string) (path: string) (operationType: HttpMethod) =
    if not (String.IsNullOrWhiteSpace operationName) then
        cleanOperationName operationName
    else
        let parts = path.Split("/")
        let parameters =
            parts
            |> Array.filter (fun part -> part.Contains "{" && part.Contains "}")
            |> Array.map (fun part -> part.Replace("{", "").Replace("}", ""))
            |> Array.map capitalize
            |> Array.map cleanOperationName
            |> String.concat "And"

        let segments =
            parts
            |> Array.filter (fun part -> not (part.Contains "{" && part.Contains "}"))
            |> Array.mapi (fun index part ->
                if index <> 0
                then cleanOperationName (capitalize part)
                else cleanOperationName part)
            |> String.concat ""

        if String.IsNullOrEmpty parameters then
            cleanOperationName (httpMethodName operationType + segments)
        else
            cleanOperationName (httpMethodName operationType + segments + "By" + parameters)

module MediaTypes =
    let [<Literal>] ApplicationJson = "application/json"
    let [<Literal>] OctetStream = "application/octet-stream"
    let [<Literal>] ApplicationPdf = "application/pdf"
    let [<Literal>] ApplicationZip = "application/zip"
    let [<Literal>] AppliationZipCompressed = "application/x-zip-compressed"
    let [<Literal>] ImagePng = "image/png"
    let [<Literal>] ImageJpg = "image/jpg"
    let [<Literal>] ImageJpeg = "image/jpeg"
    let [<Literal>] ImageGif = "image/gif"

let (|IntEnum|_|) (typeName: string) (schema: IOpenApiSchema) =
    if isEnumType schema then
        let cases =
            schema.Enum
            |> Seq.choose (fun enumCase -> nodeAsInt enumCase)

        let caseNames =
            if not (isNull schema.Extensions) && schema.Extensions.ContainsKey "x-enumNames" then
                match extensionStringArray schema.Extensions.["x-enumNames"] with
                | Some names -> names
                | None -> []
            else
                cases
                |> Seq.map (fun caseValue -> typeName + string caseValue)
                |> Seq.toList

        if not (Seq.isEmpty cases) && not (Seq.isEmpty caseNames) && Seq.length cases = Seq.length caseNames then
            cases
            |> Seq.zip caseNames
            |> Some
        else
            None
    else
        None

let rec createFieldType recordName required (propertyName: string) (propertySchema: IOpenApiSchema) (config: CodegenConfig) =
    if not required then
        let optionalType : SynType = createFieldType recordName true propertyName propertySchema config
        SynType.Option(optionalType)
    elif isNull propertySchema then
        if config.target = Target.FSharp
        then SynType.JToken()
        else SynType.Object()
    elif isSchemaReference propertySchema then
        // working with a reference type
        let typeName =
            if invalidTitle propertySchema.Title
            then sanitizeTypeName (schemaReferenceId propertySchema)
            else sanitizeTypeName propertySchema.Title
        SynType.Create typeName
    else
        match schemaTypeName propertySchema with
        | "integer" when propertySchema.Format = "int64" -> SynType.Int64()
        | "integer" -> SynType.Int()
        | "number" when propertySchema.Format = "float" -> SynType.Float32()
        | "number" ->  SynType.Double()
        | "boolean" -> SynType.Bool()
        | "string" when propertySchema.Format = "uuid" -> SynType.Guid()
        | "string" when propertySchema.Format = "guid" -> SynType.Guid()
        | "string" when propertySchema.Format = "date-time" -> SynType.DateTimeOffset()
        | "string" when propertySchema.Format = "time-span" || propertySchema.Format = "date-span" -> SynType.TimeSpan()
        | "string" when propertySchema.Format = "byte" ->
            // base64 encoded characters
            SynType.ByteArray()
        | "array" ->
            let itemsSchema = propertySchema.Items
            let arrayItemsType =
                if isNull (box itemsSchema) || isEmptySchema itemsSchema then
                    // free-form array items (`items: {}` or no item schema)
                    if config.target = Target.FSharp then SynType.JToken() else SynType.Object()
                else
                    createFieldType recordName required propertyName itemsSchema config
            SynType.List(arrayItemsType)
        | _ ->
            SynType.String()

let compiledName (name: string) = SynAttribute.CompiledName name

/// Turns an arbitrary string (enum value or discriminator mapping key) into a
/// valid, capitalized F# union-case identifier.
let cleanCaseName (case: string) =
    if String.IsNullOrWhiteSpace case then
        "EmptyString"
    else
        match case.Trim() with
        | "<" -> "LessThan"
        | "<=" -> "LessThanOrEqual"
        | ">" -> "GreaterThan"
        | ">=" -> "GreaterThanOrEqual"
        | "=" | "==" -> "Equal"
        | "!=" | "<>" -> "NotEqual"
        | "*" -> "Star"
        | trimmed ->
            // split into alphanumeric chunks - this drops @, /, -, . and every
            // other non-identifier character - then capitalize each chunk and join
            // into a single valid F# identifier (no backticks, no Numeric_ in the
            // middle - the prefix is only added when the whole name starts with a digit)
            let separators =
                trimmed
                |> Seq.filter (fun character -> not (Char.IsLetterOrDigit character))
                |> Seq.distinct
                |> Array.ofSeq
            let identifier =
                trimmed.Split(separators, StringSplitOptions.RemoveEmptyEntries)
                |> Array.map capitalize
                |> String.concat ""
            if String.IsNullOrEmpty identifier then "EmptyString"
            elif Char.IsDigit identifier.[0] then "Numeric_" + identifier
            else identifier

let createEnumType (enumName: string) (values: seq<string>) (docs: string option) (target: Target) =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [
            SynAttributeList.Create [
                match target with
                | Target.Fable -> SynAttribute.Create [ "Fable";"Core"; "StringEnum" ]
                | Target.FSharp -> ()
                SynAttribute.RequireQualifiedAccess()
            ]
        ]
        Id = [ Ident.Create enumName ]
        XmlDoc =
            match docs with
            | None -> PreXmlDoc.Empty
            | Some value -> xmlDocs value
        Parameters = None
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let cleanEnumValue = cleanCaseName

    let distinctValues =
        values
        |> Seq.distinctBy (fun value -> cleanEnumValue value)

    let enumRepresentation = SynTypeDefnSimpleReprUnionRcd.Create([
        for value in distinctValues ->
            let caseAttribute =
                match target with
                | Target.Fable -> compiledName value
                | Target.FSharp ->
                    SynAttribute.Create([ Ident.Create "System"; Ident.Create "Text"; Ident.Create "Json"; Ident.Create "Serialization"; Ident.Create "JsonName" ], SynConst.CreateString value)
            let attrs = [ SynAttributeList.Create [| caseAttribute |] ]
            let docs = PreXmlDoc.Empty
            SynUnionCase.SynUnionCase(attrs, SynIdent(Ident.Create (cleanEnumValue value), None), SynUnionCaseKind.Fields [], docs, None, range0, { BarRange = None })
    ])

    let simpleType = SynTypeDefnSimpleReprRcd.Union(enumRepresentation)

    let members : SynMemberDefn list = [
        let unitConst : SynPatConstRcd = {
            Const = SynConst.Unit
            Range = range0
        }

        let matchClauses = [
            for value in distinctValues ->
                let id = SynLongIdent.CreateString (cleanEnumValue value)
                let matchedValue = SynPat.LongIdent(id, None, None, SynArgPats.Empty, None, range0)
                let result = SynExpr.CreateConstString value
                SynMatchClause.SynMatchClause(matchedValue, None, result, range0, DebugPointAtTarget.No, { ArrowRange = Some range0; BarRange = Some range0 })
        ]

        SynMemberDefn.CreateMember
            { SynBindingRcd.Null with
                Pattern = SynPatRcd.CreateLongIdent(SynLongIdent.CreateString "this.Format", [SynPatRcd.Const unitConst])
                Expr = SynExpr.CreateMatch(SynExpr.Ident(Ident.Create "this"), matchClauses)
            }
    ]

    SynModuleDecl.CreateSimpleType(info, simpleType, members)

/// True when a schema is semantically a discriminated union: it has a non-empty
/// `oneOf` and a `discriminator` with a non-empty `mapping`.
let isDiscriminatedUnionSchema (schema: IOpenApiSchema) : bool =
    isNotNull (box schema)
    && isNotNull schema.OneOf
    && schema.OneOf.Count > 0
    && isNotNull schema.Discriminator
    && not (String.IsNullOrWhiteSpace schema.Discriminator.PropertyName)
    && isNotNull schema.Discriminator.Mapping
    && schema.Discriminator.Mapping.Count > 0

/// Builds a named-argument expression `propName = valueExpr` as it appears
/// inside an attribute's argument tuple.
let private namedAttributeArg (propName: string) (valueExpr: SynExpr) : SynExpr =
    let equalsIdent =
        SynExpr.LongIdent(
            false,
            SynLongIdent([ Ident.Create "op_Equality" ], [], [ Some (IdentTrivia.OriginalNotation "=") ]),
            None, range0)
    let lhs =
        SynExpr.App(ExprAtomicFlag.NonAtomic, true, equalsIdent, SynExpr.Ident(Ident.Create propName), range0)
    SynExpr.App(ExprAtomicFlag.NonAtomic, false, lhs, valueExpr, range0)

/// Generates an F# discriminated union from an OpenAPI `oneOf` + `discriminator`
/// schema. For the .NET target the union is annotated with a
/// `[<JsonFSharpConverter(...)>]` attribute that makes FSharp.SystemTextJson
/// serialize it with an internal discriminator tag (a flat JSON object). For the
/// Fable target the same DU is emitted without that attribute.
let createDiscriminatedUnion (unionName: string) (schema: IOpenApiSchema) (target: Target) : SynModuleDecl =
    let discriminator = schema.Discriminator
    let attributes =
        [
            SynAttributeList.Create [
                match target with
                | Target.FSharp ->
                    // [<JsonFSharpConverter(UnionEncoding = (JsonUnionEncoding.InternalTag ||| JsonUnionEncoding.UnwrapRecordCases), UnionTagName = "<prop>", UnionUnwrapSingleFieldCases = true)>]
                    // InternalTag puts the discriminator inside the object; UnwrapRecordCases
                    // hoists the case's record fields to the top level so the JSON is a flat
                    // object (e.g. {"type":"assets","name":...}) instead of a nested/array shape.
                    let encodingFlag (name: string) =
                        SynExpr.LongIdent(
                            false,
                            SynLongIdent(
                                [ Ident.Create "System"; Ident.Create "Text"; Ident.Create "Json"; Ident.Create "Serialization"; Ident.Create "JsonUnionEncoding"; Ident.Create name ],
                                [ range0; range0; range0; range0; range0 ],
                                [ None; None; None; None; None; None ]),
                            None, range0)
                    let bitwiseOr =
                        SynExpr.LongIdent(
                            false,
                            SynLongIdent([ Ident.Create "op_BitwiseOr" ], [], [ Some (IdentTrivia.OriginalNotation "|||") ]),
                            None, range0)
                    let unionEncoding =
                        // (JsonUnionEncoding.InternalTag ||| JsonUnionEncoding.UnwrapRecordCases)
                        SynExpr.Paren(
                            SynExpr.App(
                                ExprAtomicFlag.NonAtomic, false,
                                SynExpr.App(ExprAtomicFlag.NonAtomic, true, bitwiseOr, encodingFlag "InternalTag", range0),
                                encodingFlag "UnwrapRecordCases",
                                range0),
                            range0, Some range0, range0)
                    let argExpr =
                        SynExpr.Paren(
                            SynExpr.Tuple(
                                false,
                                [
                                    namedAttributeArg "UnionEncoding" unionEncoding
                                    namedAttributeArg "UnionTagName" (SynExpr.CreateConstString discriminator.PropertyName)
                                    namedAttributeArg "UnionUnwrapSingleFieldCases" (SynExpr.Const(SynConst.Bool true, range0))
                                ],
                                [ range0; range0 ],
                                range0),
                            range0, Some range0, range0)
                    { SynAttribute.TypeName = mkSynLongIdent [ Ident.Create "System"; Ident.Create "Text"; Ident.Create "Json"; Ident.Create "Serialization"; Ident.Create "JsonFSharpConverter" ]
                      SynAttribute.ArgExpr = argExpr
                      SynAttribute.Target = None
                      SynAttribute.AppliesToGetterAndSetter = false
                      SynAttribute.Range = range0 }
                | Target.Fable -> ()
                SynAttribute.RequireQualifiedAccess()
            ]
        ]

    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = attributes
        Id = [ Ident.Create unionName ]
        XmlDoc =
            if String.IsNullOrWhiteSpace schema.Description
            then PreXmlDoc.Empty
            else xmlDocs schema.Description
        Parameters = None
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let cases =
        [
            for mapping in discriminator.Mapping do
                let mappingKey = mapping.Key
                let mappedTypeName = sanitizeTypeName (schemaReferenceId mapping.Value)
                if isNotNull mappedTypeName then
                    let caseName = cleanCaseName mappingKey
                    // [<JsonName>] comes from FSharp.SystemTextJson, which only the .NET
                    // target references - the Fable target gets the bare case.
                    let caseAttrs =
                        match target with
                        | Target.FSharp ->
                            [ SynAttributeList.Create [| SynAttribute.Create([ Ident.Create "System"; Ident.Create "Text"; Ident.Create "Json"; Ident.Create "Serialization"; Ident.Create "JsonName" ], SynConst.CreateString mappingKey) |] ]
                        | Target.Fable -> []
                    let field = SynField.SynField([], false, None, SynType.Create mappedTypeName, false, PreXmlDoc.Empty, None, range0, SynFieldTrivia.Zero)
                    SynUnionCase.SynUnionCase(caseAttrs, SynIdent(Ident.Create caseName, None), SynUnionCaseKind.Fields [ field ], PreXmlDoc.Empty, None, range0, { BarRange = None })
        ]

    let unionRepresentation = { Access = None; Cases = cases; Range = range0 }
    let simpleType = SynTypeDefnSimpleReprRcd.Union(unionRepresentation)
    SynModuleDecl.CreateSimpleType(info, simpleType, [])



/// In an OpenAPI `oneOf` + `discriminator` schema, each member schema also
/// declares the discriminator property as one of its own fields. The generated
/// discriminated union already carries the discriminator (it is written from the
/// union case), so keeping that field on the member records would emit a
/// duplicate JSON key. Remove the discriminator property from the member schemas.
let stripDiscriminatorMemberProperties (document: OpenApiDocument) =
    if isNotNull (box document) && isNotNull document.Components && isNotNull document.Components.Schemas then
        let schemas = document.Components.Schemas
        for schemaEntry in schemas do
            if isDiscriminatedUnionSchema schemaEntry.Value then
                let propertyName = schemaEntry.Value.Discriminator.PropertyName
                for mapping in schemaEntry.Value.Discriminator.Mapping do
                    let memberId = schemaReferenceId mapping.Value
                    if isNotNull memberId && schemas.ContainsKey memberId then
                        match box schemas.[memberId] with
                        | :? OpenApiSchema as memberSchema ->
                            if isNotNull memberSchema.Properties then
                                memberSchema.Properties.Remove propertyName |> ignore
                            if isNotNull memberSchema.Required then
                                memberSchema.Required.Remove propertyName |> ignore
                        | _ -> ()

let statusCode = function
    | "200" -> Some (nameof HttpStatusCode.OK)
    | "201" -> Some (nameof HttpStatusCode.Created)
    | "202" -> Some (nameof HttpStatusCode.Accepted)
    | "204" -> Some (nameof HttpStatusCode.NoContent)
    | "206" -> Some (nameof HttpStatusCode.PartialContent)
    | "301" -> Some (nameof HttpStatusCode.MovedPermanently)
    | "302" -> Some (nameof HttpStatusCode.Found)
    | "400" -> Some (nameof HttpStatusCode.BadRequest)
    | "401" -> Some (nameof HttpStatusCode.Unauthorized)
    | "403" -> Some (nameof HttpStatusCode.Forbidden)
    | "404" -> Some (nameof HttpStatusCode.NotFound)
    | "405" -> Some (nameof HttpStatusCode.MethodNotAllowed)
    | "409" -> Some (nameof HttpStatusCode.Conflict)
    | "415" -> Some (nameof HttpStatusCode.UnsupportedMediaType)
    | "416" -> Some (nameof HttpStatusCode.RequestedRangeNotSatisfiable)
    | "422" -> Some (nameof HttpStatusCode.UnprocessableEntity)
    | "500" -> Some (nameof HttpStatusCode.InternalServerError)
    | "503" -> Some (nameof HttpStatusCode.ServiceUnavailable)
    | "default" -> Some "DefaultResponse"
    | _ -> None

let createFlagsEnum (enumName: string) (values: seq<string * int>) =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [
            SynAttributeList.Create [
                SynAttribute.RequireQualifiedAccess()
            ]
        ]
        Id = [ Ident.Create enumName ]
        XmlDoc = PreXmlDoc.Empty
        Parameters = None
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let enumRepresentation = SynTypeDefnSimpleReprEnumRcd.Create([
        for (enumName, enumValue) in values ->
            let attrs = []
            let docs = PreXmlDoc.Empty
            SynEnumCaseRcd.Create(Ident.Create (capitalize enumName), SynConst.Int32 enumValue)
    ])

    let simpleType = SynTypeDefnSimpleReprRcd.Enum enumRepresentation

    SynModuleDecl.CreateSimpleType(info, simpleType)



/// Creates a declaration: type {typeName} = {aliasedType}
///
/// This is used when there are global schema components that map to primitive types
let createTypeAbbreviation (abbreviation: string) (aliasedType: SynType) =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create (sanitizeTypeName abbreviation) ]
        XmlDoc = PreXmlDoc.Empty
        Parameters = None
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let typeAbbrev = SynTypeDefnSimpleRepr.TypeAbbrev(ParserDetail.Ok, aliasedType, range0)
    let typeRepr = SynTypeDefnRepr.Simple(typeAbbrev, range0)
    let typeInfo = SynTypeDefn.SynTypeDefn(info.FromRcd, typeRepr, [], None, range0, { LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0; EqualsRange = Some range0; WithKeyword = None })
    SynModuleDecl.Types ([ typeInfo ], range0)

/// Creates a declaration: type {typeName} = {aliasedType}
///
/// This is used when there are global schema components that map to primitive types
let createTypeAbbreviationWithDocs (abbreviation: string) (aliasedType: SynType) (docs: string) =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create (sanitizeTypeName abbreviation) ]
        XmlDoc = xmlDocs docs
        Parameters = None
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let typeAbbrev = SynTypeDefnSimpleRepr.TypeAbbrev(ParserDetail.Ok, aliasedType, range0)
    let typeRepr = SynTypeDefnRepr.Simple(typeAbbrev, range0)
    let typeInfo = SynTypeDefn.SynTypeDefn(info.FromRcd, typeRepr, [], None, range0, { LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0; EqualsRange = Some range0; WithKeyword = None })
    SynModuleDecl.Types ([ typeInfo ], range0)

let isGlobalRef (name: string) (openApiDocument: OpenApiDocument) =
    let typeName = sanitizeTypeName name
    let schemas =
        if isNotNull openApiDocument.Components
        then List.ofSeq (openApiDocument.Components.Schemas)
        else []

    let isGlobal =
        schemas
        |> List.exists (fun pair ->
            let pairName = sanitizeTypeName pair.Key
            let isRef = isSchemaReference pair.Value
            isRef && (
                typeName = pairName
                || typeName = sanitizeTypeName (schemaReferenceId pair.Value)
            )
        )

    isGlobal

let rec createRecordFromSchema (recordName: string) (schema: IOpenApiSchema) (visitedTypes: ResizeArray<string>) (config: CodegenConfig) (openApiDocument: OpenApiDocument) (factory: FactoryFunction) : SynModuleDecl list =
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create recordName ]
        XmlDoc = xmlDocs schema.Description
        Parameters = None
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let nestedObjects = ResizeArray<SynModuleDecl>()
    let recordFields = ResizeArray<SynFieldRcd>()
    let addedFields = ResizeArray<string * bool * SynType>()

    let rec createPropertyType (propertyName: string) (propertyType: IOpenApiSchema) =
        if isNull (box propertyType) then
            None
        else
        let isEnum = isEnumType propertyType
        let required = schema.Required.Contains propertyName && not (schemaIsNullable propertyType)
        let isObjectArray =
            schemaTypeName propertyType = "array"
            && isNotNull (box propertyType.Items)
            && schemaTypeName propertyType.Items = "object"
            && not (isSchemaReference propertyType.Items)
            && isNotNull propertyType.Items.Properties
            && propertyType.Items.Properties.Count > 0

        let isAdditionalProperties =
            propertyType.AdditionalPropertiesAllowed
            && not (isNull (box propertyType.AdditionalProperties))
            && not (isNull (schemaTypeName propertyType.AdditionalProperties) && propertyType.AdditionalProperties.Properties.Count > 0)

        let isEnumArray =
            schemaTypeName propertyType = "array"
            && isNotNull (box propertyType.Items)
            && isEnumType propertyType.Items

        let isEmptyObjectDefinition =
            schemaTypeName propertyType = "object"
            && propertyType.Properties.Count = 0
            && (isNull propertyType.AllOf || propertyType.AllOf.Count = 0)
            && (isNull propertyType.AnyOf || propertyType.AnyOf.Count = 0)

        let isEmptyDefinition = not (schemaTypeFlags propertyType).HasValue

        let isKeyValuePairObject =
            schemaTypeName propertyType = "object"
            && propertyType.Title = "KeyValuePair`2"
            && propertyType.Properties.Count = 2
            && propertyType.Properties.ContainsKey "Key"
            && propertyType.Properties.ContainsKey "Value"

        let isArrayOfKeyValuePairObject =
            schemaTypeName propertyType = "array"
            && isNotNull (box propertyType.Items)
            && schemaTypeName propertyType.Items = "object"
            && propertyType.Items.Title = "KeyValuePair`2"
            && propertyType.Items.Properties.Count = 2
            && propertyType.Items.Properties.ContainsKey "Key"
            && propertyType.Items.Properties.ContainsKey "Value"

        let isArrayOfEmptyObject =
            schemaTypeName propertyType = "array"
            && isNotNull (box propertyType.Items)
            && schemaTypeName propertyType.Items = "object"
            && propertyType.Items.Properties.Count = 0
            && not (isSchemaReference propertyType.Items)
            && (isNull propertyType.Items.AllOf || propertyType.Items.AllOf.Count = 0)
            && (isNull propertyType.Items.AnyOf || propertyType.Items.AnyOf.Count = 0)

        let isPrimitve = List.forall id [
            (schemaTypeName propertyType <> "object" || isSchemaReference propertyType)
            not isEnum
            not isObjectArray
            not isEnumArray
            not isEmptyObjectDefinition
            not isEmptyDefinition
            not isKeyValuePairObject
            not isArrayOfKeyValuePairObject
            not isArrayOfEmptyObject
        ]

        if propertyType.Deprecated then
            // skip deprecated propertie
            None
        elif isAdditionalProperties then
            let fieldType = createFieldType recordName true propertyName propertyType.AdditionalProperties config
            match required, schemaIsNullable propertyType.AdditionalProperties with
            | false, false -> SynType.Option(SynType.Map(SynType.String(), fieldType))
            | true, false -> SynType.Map(SynType.String(), fieldType)
            | false, true -> SynType.Option(SynType.Map(SynType.String(), SynType.Option(fieldType)))
            | true, true -> SynType.Map(SynType.String(), SynType.Option(fieldType))
            |> Some
        elif isPrimitve then
            let fieldType = createFieldType recordName required propertyName propertyType config
            Some fieldType
        else if isEnum && not (isSchemaReference propertyType) then
            // nested enum -> not a reference to a global usable enum
            let enumPropertyName = sanitizeTypeName propertyName
            let enumTypeName = findNextEnumTypeName enumPropertyName recordName visitedTypes
            match propertyType with
            | StringEnum cases ->
                visitedTypes.Add enumTypeName
                let createdEnumType = createEnumType enumTypeName cases None config.target
                nestedObjects.Add createdEnumType
                let fieldType =
                    if required
                    then SynType.Create enumTypeName
                    else SynType.Option(SynType.Create enumTypeName)
                Some fieldType
            | IntEnum enumTypeName cases ->
                visitedTypes.Add enumTypeName
                let createdEnumType = createFlagsEnum enumTypeName cases
                nestedObjects.Add createdEnumType
                let fieldType =
                    if required
                    then SynType.Create enumTypeName
                    else SynType.Option(SynType.Create enumTypeName)
                Some fieldType
            | _ ->
                None
        else if isEnum && isSchemaReference propertyType then
            // referenced enum
            let typeName =
                if invalidTitle propertyType.Title
                then sanitizeTypeName (schemaReferenceId propertyType)
                else sanitizeTypeName propertyType.Title
            let fieldType =
                if required
                then SynType.Create typeName
                else SynType.Option(SynType.Create typeName)
            Some fieldType
        else if isEmptyObjectDefinition then
            // empty object definition
            let fieldType =
                if required then
                    if config.target = Target.FSharp
                    then SynType.JObject()
                    else SynType.Object()
                else
                    if config.target = Target.FSharp
                    then SynType.Option(SynType.JObject())
                    else SynType.Option(SynType.Object())
            Some fieldType
        else if isEmptyDefinition then
            // empty object definition
            let fieldType =
                if required then
                    if config.target = Target.FSharp
                    then SynType.JToken()
                    else SynType.Object()
                else
                    if config.target = Target.FSharp
                    then SynType.Option(SynType.JToken())
                    else SynType.Option(SynType.Object())
            Some fieldType

        else if isKeyValuePairObject then
            let keySchema = propertyType.Properties.["Key"]
            let valueSchema = propertyType.Properties.["Value"]
            match createPropertyType (propertyName + "Key") keySchema, createPropertyType (propertyName + "Value") valueSchema with
            | Some keyType, Some valueType ->
                let pairType = SynType.KeyValuePair(keyType, valueType)
                let fieldType =
                    if required
                    then pairType
                    else SynType.Option(pairType)
                Some fieldType
            | _ ->
                None
        else if schemaTypeName propertyType = "object" then
            // handle nested objects
            let nestedPropertyNames =
                propertyType.Properties
                |> Seq.map (fun pair -> pair.Key)
                |> Seq.toList

            if isEmptySchema propertyType then
                let freeFormType =
                    if config.target = Target.FSharp
                    then SynType.JToken()
                    else SynType.Object()
                Some freeFormType
            else
                let objectPropertyName = (capitalize (sanitizeTypeName propertyName))
                let isGlobal = isGlobalRef objectPropertyName openApiDocument
                let nestedObjectTypeName = findNextTypeName objectPropertyName recordName nestedPropertyNames visitedTypes isGlobal
                visitedTypes.Add nestedObjectTypeName
                let nestedObject = createRecordFromSchema nestedObjectTypeName propertyType visitedTypes config openApiDocument factory
                nestedObjects.AddRange nestedObject
                let fieldType =
                    if required
                    then SynType.Create nestedObjectTypeName
                    else SynType.Option(SynType.Create nestedObjectTypeName)
                Some fieldType
        else if isObjectArray then
             // handle arrays of nested objects
            let arrayItemsType = propertyType.Items
            let nestedPropertyNames =
                arrayItemsType.Properties
                |> Seq.map (fun pair -> pair.Key)
                |> Seq.toList

            let objectTypeName = capitalize (sanitizeTypeName propertyName)
            let isGlobal = isGlobalRef objectTypeName openApiDocument
            let nestedObjectTypeName = findNextTypeName objectTypeName recordName nestedPropertyNames visitedTypes isGlobal
            visitedTypes.Add nestedObjectTypeName
            let nestedObject = createRecordFromSchema nestedObjectTypeName arrayItemsType visitedTypes config openApiDocument factory
            nestedObjects.AddRange nestedObject
            let fieldType =
                if required
                then SynType.List(SynType.Create nestedObjectTypeName)
                else SynType.Option(SynType.List(SynType.Create nestedObjectTypeName))
            Some fieldType
        else if isEnumArray then
            let arrayItemsType = propertyType.Items
            if not (isSchemaReference arrayItemsType) then
                // nested enum type -> not a global reference
                let enumTypeName = findNextEnumTypeName propertyName recordName visitedTypes
                match arrayItemsType with
                | StringEnum cases ->
                    visitedTypes.Add enumTypeName
                    let createdEnumType = createEnumType enumTypeName cases None config.target
                    nestedObjects.Add createdEnumType
                    let fieldType =
                        if required
                        then SynType.List(SynType.Create enumTypeName)
                        else SynType.Option(SynType.List(SynType.Create enumTypeName))
                    Some fieldType
                | IntEnum enumTypeName cases ->
                    visitedTypes.Add enumTypeName
                    let createdEnumType = createFlagsEnum enumTypeName cases
                    nestedObjects.Add createdEnumType
                    let fieldType =
                        if required
                        then SynType.List(SynType.Create enumTypeName)
                        else SynType.Option(SynType.List(SynType.Create enumTypeName))
                    Some fieldType
                | _ ->
                    None
            else
                // referenced enum type
                let typeName =
                    if invalidTitle arrayItemsType.Title
                    then sanitizeTypeName (schemaReferenceId arrayItemsType)
                    else sanitizeTypeName arrayItemsType.Title

                let fieldType =
                    if required
                    then SynType.List(SynType.Create typeName)
                    else SynType.Option(SynType.List(SynType.Create typeName))
                Some fieldType
        elif isArrayOfKeyValuePairObject then
            let keySchema = propertyType.Items.Properties.["Key"]
            let valueSchema = propertyType.Items.Properties.["Value"]
            match createPropertyType (propertyName + "Key") keySchema, createPropertyType (propertyName + "Value") valueSchema with
            | Some keyType, Some valueType ->
                let pairType = SynType.KeyValuePair(keyType, valueType)
                let fieldType =
                    if required
                    then SynType.List(pairType)
                    else SynType.Option(SynType.List(pairType))
                Some fieldType
            | _ ->
                None
        elif isArrayOfEmptyObject then
            let fieldType =
                if required
                then
                    if config.target = Target.FSharp
                    then SynType.CreateLongIdent "System.Text.Json.Nodes.JsonArray"
                    else SynType.ResizeArray(SynType.Create "obj")

                else
                    if config.target = Target.FSharp
                    then SynType.Option (SynType.CreateLongIdent "System.Text.Json.Nodes.JsonArray")
                    else SynType.Option(SynType.ResizeArray(SynType.Create "obj"))

            Some fieldType
        else
            None

    let alreadyContainsProperty (name: string) =
        addedFields
        |> Seq.exists (fun (fieldName, _, _) -> fieldName = name)

    /// An F# keyword as a property name otherwise produces an awkward ``backticked``
    /// record field. For the .NET target, give it a clean field name plus a
    /// [<JsonPropertyName>] attribute that preserves the original JSON name.
    let recordFieldName (jsonName: string) : string * SynAttributeList list =
        let isKeywordLike =
            not (String.IsNullOrEmpty jsonName)
            && PrettyNaming.DoesIdentifierNeedBackticks jsonName
            && jsonName |> Seq.forall (fun character -> Char.IsLetterOrDigit character || character = '_')
            && not (Char.IsDigit jsonName.[0])
        if isKeywordLike && config.target = Target.FSharp then
            let attribute =
                SynAttributeList.Create [
                    SynAttribute.Create([ Ident.Create "System"; Ident.Create "Text"; Ident.Create "Json"; Ident.Create "Serialization"; Ident.Create "JsonPropertyName" ], SynConst.CreateString jsonName)
                ]
            jsonName + "_", [ attribute ]
        else
            jsonName, []

    let rec handleAllOf (currentSchema: IOpenApiSchema) =
        if not (isNull currentSchema.AllOf) then
            for innerSchema in currentSchema.AllOf do
                if schemaTypeName innerSchema = "object" || isNotNull innerSchema.Properties then
                    for property in innerSchema.Properties do
                        match createPropertyType property.Key property.Value with
                        | None -> ()
                        | Some fieldType ->
                            let propertyName = property.Key
                            let propertyType = property.Value
                            let required = schema.Required.Contains propertyName && not (schemaIsNullable propertyType)
                            let fsharpFieldName, fieldAttributes = recordFieldName propertyName
                            let field = SynFieldRcd.Create(fsharpFieldName, fieldType)
                            let docs = xmlDocs propertyType.Description
                            if not (alreadyContainsProperty fsharpFieldName) then
                                recordFields.Add { field with XmlDoc = docs; Attributes = fieldAttributes }
                                addedFields.Add((fsharpFieldName, required, fieldType))

                if isNotNull innerSchema.AllOf && innerSchema.AllOf.Count > 0 then
                    // handle recursive allOf references
                    handleAllOf innerSchema

    handleAllOf schema

    for property in schema.Properties do
        match createPropertyType property.Key property.Value with
        | None -> ()
        | Some fieldType ->
            let propertyName = property.Key
            let propertyType = property.Value
            let required = schema.Required.Contains propertyName && not (schemaIsNullable propertyType)
            let fsharpFieldName, fieldAttributes = recordFieldName propertyName
            let field = SynFieldRcd.Create(fsharpFieldName, fieldType)
            let docs = xmlDocs propertyType.Description
            if not (alreadyContainsProperty fsharpFieldName) then
                recordFields.Add { field with XmlDoc = docs; Attributes = fieldAttributes }
                addedFields.Add((fsharpFieldName, required, fieldType))

    let containsPreservedProperty =
        schema.Properties |> Seq.exists (fun prop -> prop.Key = "additionalProperties")

    let includeAdditionalProperties =
        schema.AdditionalPropertiesAllowed
        && not (isNull schema.AdditionalProperties)
        && not containsPreservedProperty
        && not (not (schemaTypeFlags schema.AdditionalProperties).HasValue && schema.Properties.Count > 0)

    if includeAdditionalProperties then
        // when there are additional properties
        // and fixed properties while targeting fable
        // then ignore the fixed properties and only get the dictionary type
        // when only additional properties are present
        // then create type abbreviation
        let isFreeForm = not (schemaTypeFlags schema.AdditionalProperties).HasValue
        match createPropertyType "additionalProperties" schema.AdditionalProperties with
        | None -> [ ]
        | Some additionalType ->
            let valueType =
                if isFreeForm && config.target = Target.FSharp
                then SynType.JToken()
                elif isFreeForm && config.target = Target.Fable
                then SynType.Object()
                else additionalType
            let dictionaryType = SynType.Map(SynType.String(), valueType)
            [ createTypeAbbreviation recordName dictionaryType ]
    elif recordFields.Count = 0 then
        // couldn't add any fields
        let valueType =
            if config.target = Target.FSharp
            then SynType.JToken()
            else SynType.Object()
        let dictionaryType = SynType.Map(SynType.String(), valueType)
        [ createTypeAbbreviation recordName dictionaryType ]
    else
        let odataTypeNameField = "ODataTypeName"
        if config.odataSchema && config.target = Target.FSharp && not (String.IsNullOrWhiteSpace schema.Title) then
            let required = false
            let fieldType = SynType.Option(SynType.String())
            let recordField = SynFieldRcd.Create(odataTypeNameField, fieldType)
            let attributes = SynAttributeList.Create [
                // create [<System.Text.Json.Serialization.JsonPropertyName "@odata.type">]
                SynAttribute.Create([ Ident.Create "System"; Ident.Create "Text"; Ident.Create "Json"; Ident.Create "Serialization"; Ident.Create "JsonPropertyName" ], SynConst.CreateString "@odata.type")
            ]
            recordFields.Insert(0, { recordField with Attributes = [ attributes ] })
            addedFields.Insert(0, (odataTypeNameField, required, fieldType))
        elif config.odataSchema && not (String.IsNullOrWhiteSpace schema.Title) then
            // fable
            let propertyName = "@odata.type"
            let required = false
            let fieldType = SynType.Option(SynType.String())

            recordFields.Insert(0, SynFieldRcd.Create(propertyName, fieldType))
            addedFields.Insert(0, (propertyName, required, fieldType))

        let recordRepr = SynTypeDefnSimpleReprRecordRcd.Create (List.ofSeq recordFields)
        let simpleRecordType = SynTypeDefnSimpleReprRcd.Record recordRepr

        let members : SynMemberDefn list = [
            SynMemberDefn.CreateStaticMember
                {
                    SynBindingRcd.Null with
                        XmlDoc = PreXmlDoc.Create $"Creates an instance of {recordName} with all optional fields initialized to None. The required fields are parameters of this function"
                        Pattern =
                            SynPatRcd.Typed {
                                Type = SynType.Create recordName
                                Range = range0
                                Pattern =
                                    SynPatRcd.CreateLongIdent(SynLongIdent.CreateString "Create", [
                                        SynPatRcd.CreateParen(
                                            SynPatRcd.Tuple {
                                                Patterns = [
                                                    for (fieldName, required, fieldType) in addedFields do
                                                        if fieldName = "additionalProperties" && not containsPreservedProperty then
                                                            ()
                                                        elif fieldName = "@odata.type" || (fieldName = "ODataTypeName" && config.odataSchema) then
                                                            ()
                                                        else
                                                            if required then yield SynPatRcd.Typed {
                                                                Type = fieldType
                                                                Pattern = SynPatRcd.CreateLongIdent(SynLongIdent.CreateString (camelCase fieldName), [])
                                                                Range = range0
                                                            }
                                                ]
                                                Range  = range0
                                            }
                                        )
                                    ])
                            }

                        // create a record with the required fields
                        Expr = SynExpr.CreateRecord [
                            for (fieldName, required, fieldType) in addedFields do
                                let expr =
                                    if fieldName = "additionalProperties" && not containsPreservedProperty
                                    then Some(SynExpr.CreateLongIdent(SynLongIdent.CreateString "Map.empty"))
                                    elif fieldName = "@odata.type" || (fieldName = "ODataTypeName" && config.odataSchema)
                                    then Some(SynExpr.CreatePartialApp([ "Some" ], [ SynExpr.CreateConstString $"#{schema.Title}" ]))
                                    elif required
                                    then Some(SynExpr.Ident(Ident.Create (camelCase fieldName)))
                                    else Some(SynExpr.Ident(Ident.Create "None"))
                                ((SynLongIdent.CreateFromLongIdent([ Ident.Create fieldName ]), false), expr)
                        ]
                }
        ]

        let anyFieldHasDots =
            addedFields
            |> Seq.exists (fun (fieldName, _, _) -> (fieldName.Contains "." || fieldName.Contains "/" || fieldName.Contains "@") && fieldName <> "@odata.type")

        // when fields have dots, they are not escaped for some reason
        // TODO: fix it later in fantomas
        // right now, we just won't generate the `Create` function
        let eventualMembers =
            if anyFieldHasDots || factory = FactoryFunction.None
            then []
            else members

        [
            yield! nestedObjects
            SynModuleDecl.CreateSimpleType(info, simpleRecordType, eventualMembers)
        ]

/// <summary>
/// Returns whether the given operation is opted-in to binary response streaming
/// (its OperationId is listed in `config.streamingOperations`). Streaming is only
/// honored by the .NET (`fsharp`) target; the Fable target always ignores it.
/// </summary>
let isStreamingOperation (config: CodegenConfig) (operation: OpenApiOperation) =
    config.target = Target.FSharp
    && isNotNull operation.OperationId
    && config.streamingOperations |> List.contains operation.OperationId

/// <summary>
/// The F# type used to carry a binary response payload: `System.IO.Stream` for
/// streaming operations on the .NET target, otherwise `byte[]`.
/// </summary>
let binaryPayloadType (config: CodegenConfig) (operation: OpenApiOperation) =
    if isStreamingOperation config operation
    then SynType.Stream()
    else SynType.ByteArray()

/// <summary>
/// Rewrites application/vnd.api+json into application/json to simplify the rest of the codegen pipeline
/// </summary>
/// <param name="operation">The operation to rewrite</param>
let rewriteOperationVendorJson  (operation: OpenApiOperation) =
    for response in operation.Responses do
        if response.Value.Content.ContainsKey "application/vnd.api+json" && not (response.Value.Content.ContainsKey "application/json") then
            let mediaType = response.Value.Content.["application/vnd.api+json"]
            response.Value.Content.Remove "application/vnd.api+json" |> ignore
            response.Value.Content.Add("application/json", mediaType)

    if isNotNull operation.RequestBody && isNotNull operation.RequestBody.Content then
        if operation.RequestBody.Content.ContainsKey "application/vnd.api+json" && not (operation.RequestBody.Content.ContainsKey "application/json") then
            let mediaType = operation.RequestBody.Content.["application/vnd.api+json"]
            operation.RequestBody.Content.Remove "application/vnd.api+json" |> ignore
            operation.RequestBody.Content.Add("application/json", mediaType)

let createResponseType (operation: OpenApiOperation) (path: string) (operationType: HttpMethod) (visitedTypes: ResizeArray<string>) (config: CodegenConfig) (document: OpenApiDocument) =
    // rewrite application/vnd.api+json into application/json
    rewriteOperationVendorJson operation
    let intermediateTypes = ResizeArray<SynModuleDecl>()
    let operationName = deriveOperationName (capitalize operation.OperationId) path operationType visitedTypes
    visitedTypes.Add operationName
    // hack: add it to the operation and retrieve it later
    operation.Extensions.Add("ResponseTypeName", stringExtension operationName)
    let rec getFieldType (schema: IOpenApiSchema) (status: string) (wrapODataResponse: bool) =
        match schemaTypeName schema with
        | "integer" when schema.Format = "int64" ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.Int64())
            else SynType.Int64()
        | "integer" ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.Int())
            else SynType.Int()
        | "number" when schema.Format = "float" ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.Float32())
            else SynType.Float32()
        | "number" ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.Double())
            else SynType.Double()
        | "boolean" ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.Bool())
            else SynType.Bool()

        | "string" when schema.Format = "uuid" ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.Guid())
            else SynType.Guid()
        | "string" when schema.Format = "guid" ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.Guid())
            else SynType.Guid()
        | "string" when schema.Format = "date-time" ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.DateTimeOffset())
            else SynType.DateTimeOffset()
        | "string" when schema.Format = "time-span" || schema.Format = "date-span" ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.TimeSpan())
            else SynType.TimeSpan()
        | "string" when schema.Format = "byte" ->
            // base64 encoded characters
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(binaryPayloadType config operation)
            else binaryPayloadType config operation
        | "file" ->
            binaryPayloadType config operation
        | "array" when isNotNull schema.Items && not (isEmptySchema schema.Items) ->
            let elementSchema = schema.Items
            let elementType = getFieldType elementSchema status false
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.List elementType)
            else SynType.List elementType
        | "array" ->
            // element type schema is null
            let elementType =
                if config.target = Target.FSharp
                then SynType.CreateLongIdent "System.Text.Json.Nodes.JsonArray"
                else SynType.ResizeArray(SynType.Create "obj")

            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(elementType)
            else elementType

        | _ when isSchemaReference schema ->
            // working with a reference type (the type match above lets a $ref
            // to a primitive resolve to that primitive, as in Hawaii's 1.x model)
            let typeName =
                if invalidTitle schema.Title
                then sanitizeTypeName (schemaReferenceId schema)
                else sanitizeTypeName schema.Title
            SynType.Create typeName
        | _ when schema.AdditionalPropertiesAllowed && not (isNull schema.AdditionalProperties) ->
            let valueType = getFieldType schema.AdditionalProperties status false
            let keyType = SynType.String()
            SynType.Map(keyType, valueType)
        | "object" ->
            let recordName = $"{operationName}_{status}"
            visitedTypes.Add recordName
            let factory = FactoryFunction.None
            for generatedType in createRecordFromSchema recordName schema visitedTypes config document factory do
                intermediateTypes.Add generatedType
            SynType.Create recordName
        | _ ->
            if config.odataSchema && wrapODataResponse
            then SynType.ODataResponse(SynType.String())
            else SynType.String()

    let hasLoosePayloadRequestBody =
        isNotNull operation.RequestBody
        && operation.RequestBody.Content.ContainsKey MediaTypes.ApplicationJson
        && isNotNull operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema
        && schemaTypeName operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema = "object"
        && not (isSchemaReference operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema)
        && isNotNull operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema.Properties
        && operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema.Properties.Count > 0

    if hasLoosePayloadRequestBody then
        let schema = operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema
        let payloadTypeName = $"{operationName}Payload"
        visitedTypes.Add payloadTypeName
        let factory = FactoryFunction.Create
        for generatedType in createRecordFromSchema payloadTypeName schema visitedTypes config document factory do
            intermediateTypes.Add generatedType
        operation.Extensions.Add("RequestTypePayload", stringExtension payloadTypeName)

    let hasArrayOfLooseObjectsInRequestPayload =
        isNotNull operation.RequestBody
        && operation.RequestBody.Content.ContainsKey MediaTypes.ApplicationJson
        && isNotNull operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema
        && schemaTypeName operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema = "array"
        && not (isSchemaReference operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema)
        && isNotNull operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema.Items
        && schemaTypeName operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema.Items = "object"
        && isNotNull operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema.Items.Properties
        && operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema.Items.Properties.Count > 0

    if hasArrayOfLooseObjectsInRequestPayload then
        let payloadTypeName = $"{operationName}Payload"
        let elementTypeName = $"{payloadTypeName}ArrayItem"
        let schema = operation.RequestBody.Content.[MediaTypes.ApplicationJson].Schema.Items
        visitedTypes.Add payloadTypeName
        visitedTypes.Add elementTypeName
        let factory = FactoryFunction.Create
        for generatedType in createRecordFromSchema elementTypeName schema visitedTypes config document factory do
            intermediateTypes.Add generatedType
        intermediateTypes.Add(createTypeAbbreviation payloadTypeName (SynType.List(SynType.Create elementTypeName)))
        operation.Extensions.Add("RequestTypePayload", stringExtension payloadTypeName)

    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [
            SynAttributeList.Create [
                SynAttribute.RequireQualifiedAccess()
            ]
        ]
        Id = [ Ident.Create operationName ]
        XmlDoc = PreXmlDoc.Empty
        Parameters = None
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let containsOkOrDefault =
        operation.Responses.ContainsKey "200"
        || operation.Responses.ContainsKey "201"
        || operation.Responses.ContainsKey "204"
        || operation.Responses.ContainsKey "202"
        || operation.Responses.ContainsKey "default"

    let enumRepresentation = SynTypeDefnSimpleReprUnionRcd.Create([
        for response in operation.Responses do
            match statusCode response.Key with
            | Some caseName ->
                let fieldTypes =
                    if response.Value.Content.ContainsKey "application/json" then
                        let responsePayloadType = response.Value.Content.["application/json"]
                        if not (isNull responsePayloadType.Schema) && not (isEmptySchema responsePayloadType.Schema) then
                            let fieldType = getFieldType responsePayloadType.Schema caseName true
                            [SynFieldRcd.Create("payload", fieldType).FromRcd]
                        elif isNotNull responsePayloadType.Schema && isEmptySchema responsePayloadType.Schema then
                            if responsePayloadType.Schema.AdditionalPropertiesAllowed && isNotNull responsePayloadType.Schema.AdditionalProperties then
                                let valueType = getFieldType responsePayloadType.Schema.AdditionalProperties caseName true
                                let keyType = SynType.String()
                                let fieldType =  SynType.Map(keyType, valueType)
                                [SynFieldRcd.Create("payload", fieldType).FromRcd]
                            elif isSchemaReference responsePayloadType.Schema then
                                // reference to an empty schema
                                if config.emptyDefinitions = EmptyDefinitionResolution.GenerateFreeForm then
                                    let fieldType = getFieldType responsePayloadType.Schema caseName true
                                    [SynFieldRcd.Create("payload", fieldType).FromRcd]
                                else
                                    []
                            elif schemaTypeName responsePayloadType.Schema = "object" then
                                if config.target = Target.FSharp then
                                    let fieldType = SynType.JToken()
                                    [SynFieldRcd.Create("payload", fieldType).FromRcd]
                                else
                                    let fieldType = SynType.Object()
                                    [SynFieldRcd.Create("payload", fieldType).FromRcd]
                            else
                                []
                        else
                            []
                    elif response.Value.Content.ContainsKey "*/*" then
                        let responsePayloadType = response.Value.Content.["*/*"]
                        if not (isNull responsePayloadType.Schema) && not (isEmptySchema responsePayloadType.Schema) then
                            let fieldType = getFieldType responsePayloadType.Schema caseName false
                            [SynFieldRcd.Create("payload", fieldType).FromRcd]
                        else
                            []
                    elif response.Value.Content.ContainsKey "text/plain" && isNotNull response.Value.Content.["text/plain"].Schema then
                        let fieldType = SynType.String()
                        [SynFieldRcd.Create("text", fieldType).FromRcd]
                    elif response.Value.Content.ContainsKey "application/octet-stream" || response.Value.Content.ContainsKey "application/pdf" || response.Value.Content.ContainsKey "application/zip" then
                        let fieldType = binaryPayloadType config operation
                        [SynFieldRcd.Create("payload", fieldType).FromRcd]
                    elif response.Value.Content.ContainsKey "image/png" && isNotNull response.Value.Content.["image/png"].Schema && response.Value.Content.["image/png"].Schema.Format = "binary" then
                        let fieldType = binaryPayloadType config operation
                        [SynFieldRcd.Create("payload", fieldType).FromRcd]
                    elif response.Value.Content.ContainsKey "image/png" then
                        let fieldType = binaryPayloadType config operation
                        [SynFieldRcd.Create("payload", fieldType).FromRcd]
                    else
                        []
                let docs = xmlDocs response.Value.Description
                yield SynUnionCase.SynUnionCase([], SynIdent(Ident.Create (capitalize caseName), None), SynUnionCaseKind.Fields fieldTypes, docs, None, range0, { BarRange = None })
            | None ->
                ()

        if not containsOkOrDefault then
            let docs = PreXmlDoc.Empty
            yield SynUnionCase.SynUnionCase([], SynIdent(Ident.Create (capitalize "DefaultResponse"), None), SynUnionCaseKind.Fields [], docs, None, range0, { BarRange = None })
    ])


    let simpleType = SynTypeDefnSimpleReprRcd.Union(enumRepresentation)

    [
        yield! intermediateTypes
        yield SynModuleDecl.CreateSimpleType(info, simpleType, [])
    ]

// type KeyValuePair<'TKey, 'TValue> = { Key: 'TKey, Value: 'TValue }
let createKeyValuePair() =
    let keyTypeArg = SynTypar.SynTypar(Ident.Create "TKey", TyparStaticReq.None, false)
    let valueTypeArg = SynTypar.SynTypar(Ident.Create "TValue", TyparStaticReq.None, false)

    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create "KeyValuePair" ]
        XmlDoc = PreXmlDoc.Empty
        Parameters = Some (SynTyparDecls.PostfixList([
            SynTyparDecl.SynTyparDecl([], keyTypeArg, [], { AmpersandRanges = [] })
            SynTyparDecl.SynTyparDecl([], valueTypeArg, [], { AmpersandRanges = [] })
        ], [], range0))
        Constraints = [ ]
        PreferPostfix = true
        Range = range0
    }

    let recordRepr = SynTypeDefnSimpleReprRecordRcd.Create [
        SynFieldRcd.Create("Key", SynType.Var(keyTypeArg, range0))
        SynFieldRcd.Create("Value", SynType.Var(valueTypeArg, range0))
    ]

    let simpleRecordType = SynTypeDefnSimpleReprRcd.Record recordRepr

    SynModuleDecl.CreateSimpleType(info, simpleRecordType)

// type ODataResponse<'TValue> = { value: 'TValue }
let createODataResponse(config: CodegenConfig) =
    let valueTypeArg = SynTypar.SynTypar(Ident.Create "TValue", TyparStaticReq.None, false)

    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create "ODataResponse" ]
        XmlDoc = PreXmlDoc.Empty
        Parameters = Some (SynTyparDecls.PostfixList([ SynTyparDecl.SynTyparDecl([], valueTypeArg, [], { AmpersandRanges = [] }) ], [], range0))
        Constraints = [ ]
        PreferPostfix = true
        Range = range0
    }

    let attributes = SynAttributeList.Create [
        // [<System.Text.Json.Serialization.JsonPropertyName "@odata.context">]
        SynAttribute.Create([ Ident.Create "System"; Ident.Create "Text"; Ident.Create "Json"; Ident.Create "Serialization"; Ident.Create "JsonPropertyName" ], SynConst.CreateString "@odata.context")
    ]

    let odataContextField = SynFieldRcd.Create("ODataContext", SynType.Option(SynType.String()))

    let recordRepr = SynTypeDefnSimpleReprRecordRcd.Create [
        if config.target = Target.FSharp then
            { odataContextField with Attributes = [ attributes ] }
        SynFieldRcd.Create("value", SynType.Var(valueTypeArg, range0))
    ]

    let simpleRecordType = SynTypeDefnSimpleReprRcd.Record recordRepr

    SynModuleDecl.CreateSimpleType(info, simpleRecordType)


let rec isPrimitiveAllOf (schema: IOpenApiSchema) =
    if isNotNull schema.AllOf && schema.AllOf.Count > 0 then
        schema.AllOf
        |> Seq.forall(fun innerSchema ->
            let isPrimitive =
                schemaTypeName innerSchema = "string"
                || schemaTypeName innerSchema = "boolean"
                || schemaTypeName innerSchema = "integer"
                || schemaTypeName innerSchema = "number"

            if isNotNull innerSchema.AllOf && innerSchema.AllOf.Count > 0 then
                isPrimitive && isPrimitiveAllOf innerSchema
            else
                isPrimitive
        )
    else
        false

let rec collectPrimitiveAllOf (schema: IOpenApiSchema) =
    if isNotNull schema.AllOf then
        [
            for innerSchema in schema.AllOf do
                yield schemaTypeName innerSchema

                if isNotNull innerSchema.AllOf then
                    for nestedSchema in innerSchema.AllOf do
                        yield! collectPrimitiveAllOf nestedSchema
        ]
    else
        [

        ]

let includeOperation (operation: OpenApiOperation) (config: CodegenConfig) : bool =
    if config.filterTags.IsEmpty then
        true
    elif operation.Tags.Count = 0 && config.filterTags.Length > 0 then
        false
    else
        operation.Tags
        |> Seq.exists (fun tag ->
            config.filterTags
            |> List.exists (fun configTag -> tag.Name.StartsWith configTag)
        )

/// Emits an F# double-quoted string literal (with surrounding quotes) for an
/// arbitrary value, escaping backslashes and quotes so it is safe to splice
/// into generated source text.
let escapeFsString (value: string) : string =
    let escaped =
        value
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
    "\"" + escaped + "\""

/// For the Fable target, generates raw F# source text holding Thoth.Json
/// `extra` coders. For every `oneOf` + `discriminator` schema it emits a custom
/// `Encoder`/`Decoder` pair and registers it in an `extraCoders` value. The
/// `Serializer` in OpenApiHttp.fs feeds `extraCoders` to `Encode.Auto`/
/// `Decode.Auto`, so discriminator unions round-trip as the flat OpenAPI
/// discriminator JSON `{"type":"...", ...fields...}` even when nested.
/// When the schema declares no discriminator unions, `extraCoders` is just
/// `Extra.empty`. The result is appended verbatim after the formatted Types.fs.
let createFableThothCoders (openApiDocument: OpenApiDocument) (config: CodegenConfig) : string =
    // discriminator union -> (duTypeName, discriminatorPropertyName, (caseName, memberTypeName) list)
    let discriminatorUnions =
        if isNull (box openApiDocument.Components) || isNull openApiDocument.Components.Schemas then
            []
        else
            [
                for topLevelObject in openApiDocument.Components.Schemas do
                    if isDiscriminatedUnionSchema topLevelObject.Value then
                        let canUseTitle =
                            not (invalidTitle topLevelObject.Value.Title)
                            && not (isGlobalRef topLevelObject.Value.Title openApiDocument)
                        let typeName =
                            if canUseTitle
                            then sanitizeTypeName topLevelObject.Value.Title
                            else sanitizeTypeName topLevelObject.Key
                        let propertyName = topLevelObject.Value.Discriminator.PropertyName
                        let cases =
                            [
                                for mapping in topLevelObject.Value.Discriminator.Mapping do
                                    let memberTypeName = sanitizeTypeName (schemaReferenceId mapping.Value)
                                    if isNotNull memberTypeName then
                                        mapping.Key, cleanCaseName mapping.Key, memberTypeName
                            ]
                        if not (List.isEmpty cases) then
                            typeName, propertyName, cases
            ]
            |> List.distinctBy (fun (typeName, _, _) -> typeName)

    let builder = StringBuilder()
    let line (text: string) = builder.AppendLine(text) |> ignore
    line ""
    line (sprintf "namespace %s.Types" config.project)
    line ""
    line "/// Thoth.Json custom coders for `oneOf` + `discriminator` unions."
    line "module rec ThothCoders ="
    line ""
    line "    open Thoth.Json"
    line ""
    line "    #nowarn \"40\""
    line ""

    // Thoth's `Encode.Auto`/`Decode.Auto` cannot handle int64/uint64/decimal/
    // bigint without explicit extra coders, so register the built-in ones for
    // these primitives - OpenAPI `integer`/`number` fields map onto them.
    // Thoth.Json has no built-in TimeSpan coder; OpenAPI `time-span`/`date-span`
    // string fields map onto System.TimeSpan, so register a custom one.
    line "    let private timeSpanEncoder : Encoder<System.TimeSpan> ="
    line "        fun value -> Encode.string (value.ToString())"
    line "    let private timeSpanDecoder : Decoder<System.TimeSpan> ="
    line "        Decode.string"
    line "        |> Decode.andThen (fun text ->"
    line "            match System.TimeSpan.TryParse text with"
    line "            | true, value -> Decode.succeed value"
    line "            | _ -> Decode.fail (sprintf \"Invalid TimeSpan '%s'\" text))"
    line ""
    line "    /// Primitive coders Thoth.Json's Auto cannot synthesise on its own"
    line "    /// (int64, uint64, decimal, bigint, TimeSpan); always part of the extra coders."
    line "    let private primitiveCoders : ExtraCoders ="
    line "        Extra.empty"
    line "        |> Extra.withInt64"
    line "        |> Extra.withUInt64"
    line "        |> Extra.withDecimal"
    line "        |> Extra.withBigInt"
    line "        |> Extra.withCustom timeSpanEncoder timeSpanDecoder"
    line ""

    if List.isEmpty discriminatorUnions then
        line "    /// No discriminator unions in this schema; only the primitive coders."
        line "    let extraCoders : ExtraCoders = primitiveCoders"
    else
        for (typeName, propertyName, cases) in discriminatorUnions do
            let lowerName = camelCase typeName
            // Encoder: auto-encode the member record; for the Fable target the
            // member record keeps the discriminator field, so the result is the
            // flat discriminator object `{"<prop>":"<key>", ...fields...}`.
            line (sprintf "    let %sEncoder : Encoder<%s> =" lowerName typeName)
            line "        fun value ->"
            line "            match value with"
            for (_, caseName, memberTypeName) in cases do
                line (sprintf "            | %s.%s payload -> Encode.Auto.generateEncoder<%s>(extra = extraCoders) payload" typeName caseName memberTypeName)
            line ""
            // Decoder: read the discriminator field, dispatch each mapping key to
            // the matching case, auto-decoding the member type.
            line (sprintf "    let %sDecoder : Decoder<%s> =" lowerName typeName)
            line (sprintf "        Decode.field \"%s\" Decode.string" propertyName)
            line "        |> Decode.andThen (fun discriminator ->"
            line "            match discriminator with"
            for (mappingKey, caseName, memberTypeName) in cases do
                line (sprintf "            | %s -> Decode.map %s.%s (Decode.Auto.generateDecoder<%s>(extra = extraCoders))" (escapeFsString mappingKey) typeName caseName memberTypeName)
            line (sprintf "            | other -> Decode.fail (sprintf \"Unknown discriminator value '%%s' for union %s\" other))" typeName)
            line ""

        line "    /// All custom discriminator coders, registered for `Encode.Auto`/`Decode.Auto`."
        line "    let extraCoders : ExtraCoders ="
        line "        primitiveCoders"
        for (typeName, _, _) in discriminatorUnions do
            let lowerName = camelCase typeName
            line (sprintf "        |> Extra.withCustom %sEncoder %sDecoder" lowerName lowerName)

    builder.ToString()

let createGlobalTypesModule (openApiDocument: OpenApiDocument) (config: CodegenConfig) =
    let visitedTypes = ResizeArray<string>()
    let moduleTypes = ResizeArray<SynModuleDecl>()
    // .NET target only: FSharp.SystemTextJson writes the discriminator itself, so
    // the member records must not also carry it. The Fable/Thoth target keeps the
    // field - its generated encoder serialises the member record as-is.
    if config.target = Target.FSharp then
        stripDiscriminatorMemberProperties openApiDocument

    if config.odataSchema then
        if config.target = Target.Fable then
            // Fable target will output @odata.type
            moduleTypes.Add (SynModuleDecl.CreateHashDirective("nowarn", [ "1104" ]))
        moduleTypes.Add (createODataResponse config)
        visitedTypes.Add "ODataResponse"

    if isNotNull openApiDocument.Components then

        // first add all global enum types
        for topLevelObject in openApiDocument.Components.Schemas do
            let typeName =
                if invalidTitle topLevelObject.Value.Title
                then sanitizeTypeName topLevelObject.Key
                else sanitizeTypeName topLevelObject.Value.Title

            if schemaTypeName topLevelObject.Value = "string" then
                match topLevelObject.Value with
                | StringEnum cases ->
                    // create global enum type
                    moduleTypes.Add (createEnumType typeName cases (Some topLevelObject.Value.Description) config.target)
                    visitedTypes.Add typeName
                | _ ->
                    // create abbreviated type
                    let abbreviatedType =
                        match topLevelObject.Value.Format with
                        | "guid" | "uuid" -> SynType.Guid()
                        | "date-time" -> SynType.DateTimeOffset()
                        | "time-span" | "date-span" -> SynType.TimeSpan()
                        | "byte" -> SynType.ByteArray()
                        | _ -> SynType.String()

                    moduleTypes.Add (createTypeAbbreviationWithDocs typeName abbreviatedType topLevelObject.Value.Description)
                    visitedTypes.Add typeName
            elif schemaTypeName topLevelObject.Value = "integer" then
                match topLevelObject.Value with
                | IntEnum typeName cases ->
                    // create global enum type
                    moduleTypes.Add(createFlagsEnum typeName cases)
                    visitedTypes.Add typeName
                | _ ->
                    // create type abbreviation
                    let abbreviatedType =
                        match topLevelObject.Value.Format with
                        | "int64" -> SynType.Int64()
                        | _ -> SynType.Int()
                    moduleTypes.Add (createTypeAbbreviation typeName abbreviatedType)
                    visitedTypes.Add typeName
            elif schemaTypeName topLevelObject.Value = "number" then
                // create type abbreviation
                let abbreviatedType =
                    match topLevelObject.Value.Format with
                    | "float" -> SynType.Float32()
                    | _ -> SynType.Double()
                moduleTypes.Add (createTypeAbbreviation typeName abbreviatedType)
                visitedTypes.Add typeName
            elif schemaTypeName topLevelObject.Value = "boolean" then
                // create type abbreviation
                moduleTypes.Add (createTypeAbbreviation typeName (SynType.Bool()))
                visitedTypes.Add typeName
            elif isPrimitiveAllOf topLevelObject.Value then
                let collectedTypes = collectPrimitiveAllOf topLevelObject.Value
                let primitiveType =
                    collectedTypes
                    |> List.groupBy id
                    |> List.sortByDescending (fun (key, types) -> List.length types)
                    |> List.tryHead
                    |> Option.map (fun (key, types) -> key)

                match primitiveType with
                | Some "string" ->
                    moduleTypes.Add (createTypeAbbreviation typeName (SynType.String()))
                    visitedTypes.Add typeName
                | Some "boolean" ->
                    moduleTypes.Add (createTypeAbbreviation typeName (SynType.Bool()))
                    visitedTypes.Add typeName
                | Some "integer" ->
                    moduleTypes.Add (createTypeAbbreviation typeName (SynType.Int()))
                    visitedTypes.Add typeName
                | Some "number" ->
                    moduleTypes.Add (createTypeAbbreviation typeName (SynType.Double()))
                    visitedTypes.Add typeName
                | _ ->
                    ()
            elif schemaTypeName topLevelObject.Value = "array" then
                let elementType = topLevelObject.Value.Items
                if isNull elementType then
                    if config.target = Target.FSharp then
                        moduleTypes.Add (createTypeAbbreviation typeName (SynType.JArray()))
                        visitedTypes.Add typeName
                    else
                        moduleTypes.Add (createTypeAbbreviation typeName (SynType.List(SynType.Object())))
                        visitedTypes.Add typeName
                elif isSchemaReference elementType then
                    let referencedType = schemaReferenceId elementType
                    moduleTypes.Add (createTypeAbbreviation typeName (SynType.List(SynType.Create referencedType)))
                    visitedTypes.Add typeName
                elif schemaTypeName elementType = "string" then
                    match elementType with
                    | StringEnum cases ->
                        // create global enum type
                        let enumTypeName = $"EnumFor{typeName}";
                        moduleTypes.Add (createEnumType enumTypeName cases None config.target)
                        let arrayOfEnum = SynType.List(SynType.Create enumTypeName)
                        moduleTypes.Add (createTypeAbbreviation typeName arrayOfEnum)

                        visitedTypes.Add enumTypeName
                        visitedTypes.Add typeName
                    | _ ->
                        // create abbreviated type
                        let abbreviatedType =
                            match topLevelObject.Value.Format with
                            | "guid" | "uuid" -> SynType.Guid()
                            | "date-time" -> SynType.DateTimeOffset()
                            | "time-span" | "date-span" -> SynType.TimeSpan()
                            | "byte" -> SynType.ByteArray()
                            | _ -> SynType.String()

                        let listOfAbbrev = SynType.List abbreviatedType

                        moduleTypes.Add (createTypeAbbreviationWithDocs typeName listOfAbbrev topLevelObject.Value.Description)
                        visitedTypes.Add typeName
                elif schemaTypeName elementType = "integer" then
                    match elementType with
                    | IntEnum typeName cases ->
                        // create global enum type
                        let enumTypeName = $"EnumFor{typeName}";
                        moduleTypes.Add(createFlagsEnum typeName cases)
                        let arrayOfEnum = SynType.List(SynType.Create enumTypeName)
                        moduleTypes.Add (createTypeAbbreviation typeName arrayOfEnum)
                        visitedTypes.Add typeName
                    | _ ->
                        // create type abbreviation
                        let abbreviatedType =
                            match elementType.Format with
                            | "int64" -> SynType.List(SynType.Int64())
                            | _ -> SynType.List(SynType.Int())
                        moduleTypes.Add (createTypeAbbreviation typeName abbreviatedType)
                        visitedTypes.Add typeName
                elif schemaTypeName elementType = "number" then
                    // create type abbreviation
                    let abbreviatedType =
                        match elementType.Format with
                        | "float" -> SynType.List(SynType.Float32())
                        | _ -> SynType.List(SynType.Double())
                    moduleTypes.Add (createTypeAbbreviation typeName abbreviatedType)
                    visitedTypes.Add typeName
                elif schemaTypeName elementType = "boolean" then
                    // create type abbreviation
                    moduleTypes.Add (createTypeAbbreviation typeName (SynType.List(SynType.Bool())))
                    visitedTypes.Add typeName
                elif schemaTypeName elementType = "object" && not (visitedTypes.Contains $"{typeName}ArrayItem") then
                    let elementTypeName = $"{typeName}ArrayItem"
                    visitedTypes.Add typeName
                    visitedTypes.Add elementTypeName
                    let factory = FactoryFunction.Create
                    for createdType in createRecordFromSchema elementTypeName elementType visitedTypes config openApiDocument factory do
                        moduleTypes.Add createdType
                    let abbreviatedType = SynType.List(SynType.Create elementTypeName)
                    moduleTypes.Add (createTypeAbbreviation typeName abbreviatedType)
            else
                ()

        // then handle the global objects
        for topLevelObject in openApiDocument.Components.Schemas do
            let canUseTitle =
                not (invalidTitle topLevelObject.Value.Title)
                && not (isGlobalRef topLevelObject.Value.Title openApiDocument)

            let typeName =
                if canUseTitle
                then sanitizeTypeName topLevelObject.Value.Title
                else sanitizeTypeName topLevelObject.Key

            if config.odataSchema then
                match box topLevelObject.Value with
                | :? OpenApiSchema as concreteSchema -> concreteSchema.Title <- topLevelObject.Key
                | _ -> ()

            let isAllOf =
                not (schemaTypeFlags topLevelObject.Value).HasValue
                && not (isNull topLevelObject.Value.AllOf)
                && topLevelObject.Value.AllOf.Count > 0

            let isKeyValuePairObject =
                schemaTypeName topLevelObject.Value = "object"
                && topLevelObject.Value.Title = "KeyValuePair`2"
                && topLevelObject.Value.Properties.Count = 2
                && topLevelObject.Value.Properties.ContainsKey "Key"
                && topLevelObject.Value.Properties.ContainsKey "Value"

            if isEmptySchema topLevelObject.Value then
                match config.emptyDefinitions with
                | EmptyDefinitionResolution.Ignore -> ()
                | EmptyDefinitionResolution.GenerateFreeForm ->
                    let freeFormType =
                        if config.target = Target.FSharp
                        then SynType.JToken()
                        else SynType.Object()
                    moduleTypes.Add (createTypeAbbreviationWithDocs typeName freeFormType topLevelObject.Value.Description)
                    visitedTypes.Add typeName
            elif isPrimitiveAllOf topLevelObject.Value then
                // handled in primitive types case
                ()
            elif isKeyValuePairObject && not (visitedTypes.Contains "KeyValuePair") then
                // create specialized key value pair when encountering auto generated type
                // from .NET backend services that encode System.Collections.Generic.KeyValuePair
                moduleTypes.Add(createKeyValuePair())
                visitedTypes.Add "KeyValuePair"
            elif isKeyValuePairObject then
                // skip generating more key value pair type
                ()
            elif isDiscriminatedUnionSchema topLevelObject.Value then
                // a `oneOf` + `discriminator` (with mapping) schema is a discriminated union
                if not (visitedTypes.Contains typeName) then
                    visitedTypes.Add typeName
                    moduleTypes.Add (createDiscriminatedUnion typeName topLevelObject.Value config.target)
            elif schemaTypeName topLevelObject.Value = "object" || isAllOf || (not (schemaTypeFlags topLevelObject.Value).HasValue && topLevelObject.Value.Properties.Count > 0) then
                if not (visitedTypes.Contains typeName) then
                    visitedTypes.Add typeName
                    let factory = FactoryFunction.Create
                    for createdType in createRecordFromSchema typeName topLevelObject.Value visitedTypes config openApiDocument factory do
                        moduleTypes.Add createdType
            else
                ()

    for path in safeSeq openApiDocument.Paths do
        for operation in safeSeq path.Value.Operations do
            if includeOperation operation.Value config then
                let responseTypes = createResponseType operation.Value path.Key operation.Key visitedTypes config openApiDocument
                moduleTypes.AddRange responseTypes

    let globalTypesModule = CodeGen.createNamespace [ config.project; "Types" ] (Seq.toList moduleTypes)

    visitedTypes, globalTypesModule

let paramReplace (parameter: string) (sep: char) =
    let parts = parameter.Split(sep)
    let firstPart = parts.[0]
    let otherParts = parts.[1..]
    let modified = [
        yield firstPart
        for part in otherParts do
            yield capitalize part
    ]

    modified
    |> List.map (fun part -> part.Replace("$", "").Replace("@", "").Replace(":", ""))
    |> String.concat ""
    |> camelCase

let responseContainsBinaryOutput (response: IOpenApiResponse) =
    if response.Content.ContainsKey MediaTypes.ApplicationJson then
        let jsonResponse = response.Content.[MediaTypes.ApplicationJson]
        let hasStringByteOutput =
            isNotNull jsonResponse.Schema
            && schemaTypeName jsonResponse.Schema = "string"
            && jsonResponse.Schema.Format = "byte"

        let hasFileOutput =
            isNotNull jsonResponse.Schema
            && schemaTypeName jsonResponse.Schema = "file"

        let hasBinaryOutput = hasStringByteOutput || hasFileOutput
        hasBinaryOutput
    else
        let hasBinaryOutput =
            response.Content.ContainsKey MediaTypes.OctetStream
            || response.Content.ContainsKey MediaTypes.ApplicationPdf
            || response.Content.ContainsKey MediaTypes.ApplicationZip
            || response.Content.ContainsKey MediaTypes.AppliationZipCompressed
            || response.Content.ContainsKey MediaTypes.ImagePng
            || response.Content.ContainsKey MediaTypes.ImageJpg
            || response.Content.ContainsKey MediaTypes.ImageJpeg
            || response.Content.ContainsKey MediaTypes.ImageGif
            || response.Content.ContainsKey MediaTypes.ImagePng
        hasBinaryOutput

let containsBinaryResponse (operation: OpenApiOperation) =
    operation.Responses
    |> Seq.exists (fun pair -> responseContainsBinaryOutput pair.Value)

let createIdent xs = SynExpr.CreateLongIdent(SynLongIdent.Create xs)
let stringExpr value = SynExpr.CreateConstString value
let createLetAssignment leftSide rightSide continuation =
    let emptySynValData = SynValData.SynValData(None, SynValInfo.Empty, None)
    let headPat = SynPat.Named(SynIdent(leftSide, None), false, None, range0)
    let binding = SynBinding.SynBinding(None, SynBindingKind.Normal, false, false, [], PreXmlDoc.Empty, emptySynValData, headPat, None, rightSide, range0, DebugPointAtBinding.Yes range0, { LeadingKeyword = SynLeadingKeyword.Let range0; InlineKeyword = None; EqualsRange = Some range0 } )
    SynExpr.LetOrUse(false, false, [binding], continuation, range0, SynExprLetOrUseTrivia.Zero)

let createLetBangAssignment leftSide body continuation =
    let emptySynValData = SynValData.SynValData(None, SynValInfo.Empty, None)
    let headPat = SynPat.Named(SynIdent(leftSide, None), false, None, range0)
    SynExpr.LetOrUseBang(DebugPointAtBinding.Yes range0, false, false, headPat, body, [], continuation, range0, { EqualsRange = Some range0 })

let createOpenApiClient
    (openApiDocument: OpenApiDocument)
    (visitedTypes: ResizeArray<string>)
    (config: CodegenConfig) =

    let extraTypes = ResizeArray<SynModuleDecl>()
    let lastProjectName = config.project.Split('.') |> Seq.last
    let clientTypeName = $"{lastProjectName}Client"
    let info : SynComponentInfoRcd = {
        Access = None
        Attributes = [ ]
        Id = [ Ident.Create clientTypeName ]
        XmlDoc = xmlDocs openApiDocument.Info.Description
        Parameters = None
        Constraints = [ ]
        PreferPostfix = false
        Range = range0
    }

    let clientMembers = ResizeArray<SynMemberDefn>()

    let httpClient = SynSimplePat.CreateTyped(Ident.Create "httpClient", SynType.Create "HttpClient")
    let urlContructorParam = SynSimplePat.CreateTyped(Ident.Create "url", SynType.String())
    let headersConstructorParam = SynSimplePat.CreateTyped(Ident.Create "headers", SynType.List(SynType.Create "Header"))

    if config.target = Target.FSharp then
        clientMembers.Add(SynMemberDefn.CreateImplicitCtor [ httpClient ])
    else
        clientMembers.Add(SynMemberDefn.CreateImplicitCtor [
            urlContructorParam
            headersConstructorParam
        ])

        let emptyHeadersList = SynExpr.CreateList [ ]
        let synValDataAsConstructor =
            match SynBindingRcd.Null.ValData with
            | SynValData(Some memberFlags, synValInfo, ident) ->
                let modifiedFlags = { memberFlags with MemberKind = SynMemberKind.Constructor }
                SynValData(Some modifiedFlags, synValInfo, ident)
            | _ ->
                SynBindingRcd.Null.ValData

        // generates new(url: string) = Client(url, [])
        // to initialize the client without extra headers
        let implicitConstructor = SynMemberDefn.CreateMember {
            SynBindingRcd.Null with
                XmlDoc = PreXmlDoc.Empty
                ValData = synValDataAsConstructor
                Expr = SynExpr.CreatePartialApp([ clientTypeName ], [ SynExpr.CreateParenedTuple [ createIdent ["url"]; emptyHeadersList ] ])
                // `new` is the secondary-constructor keyword here, not an identifier -
                // emit it bare (Ident.Create would backtick-escape it as a keyword, which
                // the F# compiler tolerates but Fable rejects).
                Pattern = SynPatRcd.CreateLongIdent(mkSynLongIdent [ Ident("new", range0) ], [
                    SynPatRcd.CreateParen(
                        SynPatRcd.Typed {
                            Range = range0
                            Type = SynType.String()
                            Pattern = SynPatRcd.CreateLongIdent(SynLongIdent.CreateString("url"), [])
                        })
                ])
        }

        clientMembers.Add(implicitConstructor)

    for path in safeSeq openApiDocument.Paths do
        let fullPath = path.Key
        let pathInfo = path.Value
        for operation in safeSeq pathInfo.Operations do
            let operationInfo = operation.Value
            if not operationInfo.Deprecated && includeOperation operationInfo config then

                if config.target = Target.FSharp then
                    operationInfo.Parameters.Add(OpenApiParameter(
                        Name = "cancellationToken",
                        In = ParameterLocation.Query,
                        Schema = OpenApiSchemaReference("CancellationToken", openApiDocument, null)))

                let parameters = operationParameters operationInfo pathInfo.Parameters config

                let summary =
                    if String.IsNullOrWhiteSpace operationInfo.Description
                    then operationInfo.Summary
                    else operationInfo.Description

                let parameterDocs = [
                    for p in parameters -> (p.parameterIdent, p.docs)
                ]

                let hasBinaryResponse = containsBinaryResponse operation.Value
                // streaming is opt-in (config.streamingOperations) and .NET-only;
                // such operations carry their binary payload as System.IO.Stream
                let isStreaming = hasBinaryResponse && isStreamingOperation config operation.Value
                let memberName = deriveMemberName operationInfo.OperationId fullPath operation.Key

                let contentIdent =
                    if hasBinaryResponse
                    then "contentBinary"
                    else "content"

                // The HTTP library always returns the response headers between the
                // status and the content. When config.responseHeaders is off the
                // headers slot is bound to a wildcard so they cause no warnings.
                let headersPat =
                    if config.responseHeaders
                    then SynPat.Named(SynIdent(Ident.Create "responseHeaders", None), false, None, range0)
                    else SynPat.Wild range0

                // for async calls
                // creates let! (status, headers, content) = {body} in {continuation}
                let deconstructAsyncResponse body continuation =
                    let status = SynPat.Named(SynIdent(Ident.Create "status", None), false, None, range0)
                    let content = SynPat.Named(SynIdent(Ident.Create contentIdent, None), false, None, range0)
                    let headPat = SynPat.Paren(SynPat.Tuple(false, [ status; headersPat; content ], [ range0; range0 ], range0), range0)
                    SynExpr.LetOrUseBang(DebugPointAtBinding.Yes range0, false, false, headPat, body, [], continuation, range0, { EqualsRange = Some range0 })

                // for synchronous calls
                // creates let (status, headers, content) = {body} in {continuation}
                let deconstructResponse body continuation =
                    let emptySynValData = SynValData.SynValData(None, SynValInfo.Empty, None)
                    let status = SynPat.Named(SynIdent(Ident.Create "status", None), false, None, range0)
                    let content = SynPat.Named(SynIdent(Ident.Create contentIdent, None), false, None, range0)
                    let headPat = SynPat.Paren(SynPat.Tuple(false, [ status; headersPat; content ], [ range0; range0 ], range0), range0)
                    let binding = SynBinding.SynBinding(None, SynBindingKind.Normal, false, false, [], PreXmlDoc.Empty, emptySynValData, headPat, None, body, range0, DebugPointAtBinding.Yes range0, { LeadingKeyword = SynLeadingKeyword.Let range0; InlineKeyword = None; EqualsRange = Some range0 } )
                    SynExpr.LetOrUse(false, false, [binding], continuation, range0, SynExprLetOrUseTrivia.Zero)

                let requestValues = [
                    for parameter in parameters do
                        if parameter.required then
                            if parameter.properties.Length = 0 then
                                yield SynExpr.CreatePartialApp(["RequestPart"; parameter.location], [
                                    if parameter.location <> "jsonContent" && parameter.location <> "binaryContent" then
                                        SynExpr.CreateParen(SynExpr.CreateTuple [
                                            stringExpr parameter.parameterName
                                            createIdent [ parameter.parameterIdent ]
                                        ])
                                    else
                                        createIdent [ parameter.parameterIdent ]
                                ])
                            else
                                for property in parameter.properties do
                                yield SynExpr.CreatePartialApp(["RequestPart"; parameter.location], [
                                    SynExpr.CreateParen(SynExpr.CreateTuple [
                                        stringExpr ($"{parameter.parameterName}[{property}]")
                                        createIdent [ parameter.parameterIdent; property ]
                                    ])
                                ])
                        elif parameter.style <> "cancellation-token" then
                            let condition = createIdent [ parameter.parameterIdent; "IsSome" ]
                            let value =
                                if parameter.properties.Length = 0 then
                                    SynExpr.CreatePartialApp([ "RequestPart"; parameter.location ], [
                                        if parameter.location <> "jsonContent" && parameter.location <> "binaryContent" then
                                            SynExpr.CreateParen(SynExpr.CreateTuple [
                                                stringExpr parameter.parameterName
                                                createIdent [ parameter.parameterIdent; "Value" ]
                                            ])
                                        else
                                            createIdent [ parameter.parameterIdent; "Value" ]
                                    ])
                                else
                                    let parametersToYield = [
                                        for property in parameter.properties do
                                            SynExpr.CreatePartialApp([ "RequestPart"; parameter.location ], [
                                                SynExpr.CreateParen(SynExpr.CreateTuple [
                                                    stringExpr ($"{parameter.parameterName}[{property}]")
                                                    createIdent [ parameter.parameterIdent; "Value"; property ]
                                                ])
                                            ])
                                    ]

                                    SynExpr.CreateSequential(parametersToYield)

                            yield SynExpr.CreateIfThen(condition, value)
                ]

                let httpFunction =
                    if isStreaming
                    then $"{operation.Key.ToString().ToLower()}Stream"
                    elif hasBinaryResponse
                    then $"{operation.Key.ToString().ToLower()}Binary"
                    else operation.Key.ToString().ToLower()

                let httpFunctionAsync = $"{httpFunction}Async"

                let requestParts = Ident.Create "requestParts"
                let httpCall httpFunc = SynExpr.CreatePartialApp(["OpenApiHttp"; httpFunc], [
                    if config.target = Target.FSharp then
                        // only use the HttpClient on F#/dotnet clients
                        SynExpr.CreateIdent (Ident.Create "httpClient")
                        SynExpr.CreateConstString fullPath
                        SynExpr.Ident requestParts
                        SynExpr.Ident <| Ident.Create "cancellationToken"
                    else
                        // apply the base path to the generated functions
                        SynExpr.CreateIdent (Ident.Create "url")
                        // the path the of the end point
                        SynExpr.CreateConstString fullPath
                        // the extra headers provided from the constructor
                        SynExpr.CreateIdent (Ident.Create "headers")
                        SynExpr.Ident requestParts
                ])

                let wrappedReturn expr =
                    // when response headers are requested, the operation yields a
                    // tuple of (responseValue, responseHeaders : (string * string) list)
                    let expr =
                        if config.responseHeaders
                        then SynExpr.CreateParen(SynExpr.CreateTuple [ expr; createIdent [ "responseHeaders" ] ])
                        else expr
                    match config.target with
                    | Target.FSharp when config.synchronous -> expr
                    | _ -> SynExpr.CreateReturn expr

                let responses =
                    operation.Value.Responses
                    |> Seq.choose (fun pair ->
                        match statusCode pair.Key with
                        | Some status -> Some (status, pair.Value)
                        | _ -> None
                    )
                    |> Seq.toList
                    |> List.groupBy fst
                    |> List.collect (fun (key, group) ->
                        if group.Length = 2 && key = "OK" then
                            let (_, response0) = group.[0]
                            let (_, response1) = group.[1]
                            if (response0.Content.Count = 0 && response1.Content.Count >= 1)
                            then [ group.[1] ]
                            else [ group.[0] ]
                        else
                            group
                    )

                let containsOkOrDefault =
                    responses
                    |> List.exists (fun (status, response) ->
                        status = "OK"
                        || status = "Created"
                        || status = "Accepted"
                        || status = "NoContent"
                        || status = "DefaultResponse")

                let responses =
                    if not containsOkOrDefault then
                        [
                            yield! responses
                            yield ("DefaultResponse", new OpenApiResponse(Content = new Dictionary<_,_>()))
                        ]
                    else
                        responses

                let responseType =
                    if operationInfo.Extensions.ContainsKey "ResponseTypeName" then
                        match extensionString operationInfo.Extensions.["ResponseTypeName"] with
                        | Some responseTypeName -> responseTypeName
                        | None -> capitalize memberName
                    else
                        capitalize memberName

                let returnExpr =
                    let createOutput (status: string,response: IOpenApiResponse) =
                        if response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && schemaTypeName response.Content.["application/json"].Schema = "string" && response.Content.["application/json"].Schema.Format = "byte" then
                            // Assume we have a binary response
                            SynExpr.CreatePartialApp([responseType; status], [
                                createIdent [ contentIdent ]
                            ])
                            |> wrappedReturn
                        elif response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && schemaTypeName response.Content.["application/json"].Schema = "string" && (response.Content.["application/json"].Schema.Format = "uuid" || response.Content.["application/json"].Schema.Format = "guid") then
                            SynExpr.CreatePartialApp([responseType; status], [
                                SynExpr.CreateParen(
                                    SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                        createIdent [ "content" ]
                                    ])
                                )
                            ])
                            |> wrappedReturn
                        elif response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && schemaTypeName response.Content.["application/json"].Schema = "file" then
                            // Assume we have a binary response
                            SynExpr.CreatePartialApp([responseType; status], [
                                createIdent [ contentIdent ]
                            ])
                            |> wrappedReturn
                        elif response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && schemaTypeName response.Content.["application/json"].Schema = "string" then
                            if hasBinaryResponse && config.target = Target.FSharp then
                                let body = SynExpr.CreatePartialApp(["Encoding"; "UTF8"; "GetString"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        createIdent [ "content" ]
                                    ])
                                    |> wrappedReturn
                                )
                            elif hasBinaryResponse && config.target = Target.Fable then
                                let body = SynExpr.CreatePartialApp(["Utilities"; "readBytesAsText"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetBangAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        createIdent [ "content" ]
                                    ])
                                    |> wrappedReturn
                                )
                            else
                                if config.odataSchema then
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        SynExpr.CreateParen(
                                            SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                createIdent [ "content" ]
                                            ])
                                        )
                                    ])
                                    |> wrappedReturn
                                else
                                    // when the media type is JSON but the return type is string
                                    // read the string as is without deserialization
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        createIdent [ "content" ]
                                    ])
                                    |> wrappedReturn
                        elif response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && schemaTypeName response.Content.["application/json"].Schema = "integer" && config.odataSchema then
                            // OData Schema and integer response schema combo
                            SynExpr.CreatePartialApp([responseType; status], [
                                SynExpr.CreateParen(
                                    SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                        createIdent [ "content" ]
                                    ])
                                )
                            ])
                            |> wrappedReturn
                        elif response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && schemaTypeName response.Content.["application/json"].Schema = "boolean" && config.odataSchema then
                            // OData Schema and boolean response schema combo
                            SynExpr.CreatePartialApp([responseType; status], [
                                SynExpr.CreateParen(
                                    SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                        createIdent [ "content" ]
                                    ])
                                )
                            ])
                            |> wrappedReturn
                        elif response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && schemaTypeName response.Content.["application/json"].Schema = "number" && config.odataSchema then
                            // OData Schema and number response schema combo
                            SynExpr.CreatePartialApp([responseType; status], [
                                SynExpr.CreateParen(
                                    SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                        createIdent [ "content" ]
                                    ])
                                )
                            ])
                            |> wrappedReturn
                        elif response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && not (isEmptySchema response.Content.["application/json"].Schema) then
                            if hasBinaryResponse && config.target = Target.FSharp then
                                let body = SynExpr.CreatePartialApp(["Encoding"; "UTF8"; "GetString"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        SynExpr.CreateParen(
                                            SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                createIdent [ "content" ]
                                            ])
                                        )
                                    ])
                                    |> wrappedReturn
                                )
                            elif hasBinaryResponse && config.target = Target.Fable then
                                let body = SynExpr.CreatePartialApp(["Utilities"; "readBytesAsText"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetBangAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        SynExpr.CreateParen(
                                            SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                createIdent [ "content" ]
                                            ])
                                        )
                                    ])
                                    |> wrappedReturn
                                )
                            else
                                SynExpr.CreatePartialApp([responseType; status], [
                                    SynExpr.CreateParen(
                                        SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                            createIdent [ "content" ]
                                        ])
                                    )
                                ])
                                |> wrappedReturn
                        elif response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && isEmptySchema response.Content.["application/json"].Schema && (isNotNull response.Content.["application/json"].Schema.AdditionalProperties || schemaTypeName response.Content.["application/json"].Schema = "object") then
                            if hasBinaryResponse && config.target = Target.FSharp then
                                let body = SynExpr.CreatePartialApp(["Encoding"; "UTF8"; "GetString"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        SynExpr.CreateParen(
                                            SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                createIdent [ "content" ]
                                            ])
                                        )
                                    ])
                                    |> wrappedReturn
                                )
                            elif hasBinaryResponse && config.target = Target.Fable then
                                let body = SynExpr.CreatePartialApp(["Utilities"; "readBytesAsText"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetBangAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        SynExpr.CreateParen(
                                            SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                createIdent [ "content" ]
                                            ])
                                        )
                                    ])
                                    |> wrappedReturn
                                )
                            else
                                SynExpr.CreatePartialApp([responseType; status], [
                                    SynExpr.CreateParen(
                                        SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                            createIdent [ "content" ]
                                        ])
                                    )
                                ])
                                |> wrappedReturn
                        elif response.Content.ContainsKey "application/json" && isNotNull response.Content.["application/json"].Schema && isEmptySchema response.Content.["application/json"].Schema then
                            // reference to an empty schema
                            if config.emptyDefinitions = EmptyDefinitionResolution.GenerateFreeForm then
                                if hasBinaryResponse && config.target = Target.FSharp then
                                    let body = SynExpr.CreatePartialApp(["Encoding"; "UTF8"; "GetString"], [
                                        createIdent [ "contentBinary" ]
                                    ])

                                    createLetAssignment (Ident.Create "content") body (
                                        // continuation
                                        SynExpr.CreatePartialApp([responseType; status], [
                                            SynExpr.CreateParen(
                                                SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                    createIdent [ "content" ]
                                                ])
                                            )
                                        ])
                                        |> wrappedReturn
                                    )
                                elif hasBinaryResponse && config.target = Target.Fable then
                                    let body = SynExpr.CreatePartialApp(["Utilities"; "readBytesAsText"], [
                                        createIdent [ "contentBinary" ]
                                    ])

                                    createLetBangAssignment (Ident.Create "content") body (
                                        // continuation
                                        SynExpr.CreatePartialApp([responseType; status], [
                                            SynExpr.CreateParen(
                                                SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                    createIdent [ "content" ]
                                                ])
                                            )
                                        ])
                                        |> wrappedReturn
                                    )
                                else
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        SynExpr.CreateParen(
                                            SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                createIdent [ "content" ]
                                            ])
                                        )
                                    ])
                                    |> wrappedReturn
                            else
                                // ignore
                                createIdent [ responseType; status ]
                                |> wrappedReturn

                        elif response.Content.ContainsKey "application/json" && isNull response.Content.["application/json"].Schema then
                            createIdent [ responseType; status ]
                            |> wrappedReturn
                        elif response.Content.ContainsKey "*/*" && isNotNull (response.Content.["*/*"].Schema) && not (isEmptySchema response.Content.["*/*"].Schema) then
                            if hasBinaryResponse && config.target = Target.FSharp then
                                let body = SynExpr.CreatePartialApp(["Encoding"; "UTF8"; "GetString"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        SynExpr.CreateParen(
                                            SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                createIdent [ "content" ]
                                            ])
                                        )
                                    ])
                                    |> wrappedReturn
                                )
                            elif hasBinaryResponse && config.target = Target.Fable then
                                let body = SynExpr.CreatePartialApp(["Utilities"; "readBytesAsText"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetBangAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        SynExpr.CreateParen(
                                            SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                                createIdent [ "content" ]
                                            ])
                                        )
                                    ])
                                    |> wrappedReturn
                                )
                            else
                                SynExpr.CreatePartialApp([responseType; status], [
                                    SynExpr.CreateParen(
                                        SynExpr.CreatePartialApp(["Serializer"; "deserialize"], [
                                            createIdent [ "content" ]
                                        ])
                                    )
                                ])
                                |> wrappedReturn
                        elif response.Content.ContainsKey "text/plain" && isNotNull response.Content.["text/plain"].Schema then
                            if hasBinaryResponse && config.target = Target.FSharp then
                                let body = SynExpr.CreatePartialApp(["Encoding"; "UTF8"; "GetString"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        createIdent [ "content" ]
                                    ])
                                    |> wrappedReturn
                                )
                            elif hasBinaryResponse && config.target = Target.Fable then
                                let body = SynExpr.CreatePartialApp(["Utilities"; "readBytesAsText"], [
                                    createIdent [ "contentBinary" ]
                                ])

                                createLetBangAssignment (Ident.Create "content") body (
                                    // continuation
                                    SynExpr.CreatePartialApp([responseType; status], [
                                        createIdent [ "content" ]
                                    ])
                                    |> wrappedReturn
                                )
                            else
                                SynExpr.CreatePartialApp([responseType; status], [
                                    createIdent [ "content" ]
                                ])
                                |> wrappedReturn
                        elif response.Content.ContainsKey "application/octet-stream" || response.Content.ContainsKey "application/pdf" || response.Content.ContainsKey "application/zip" then
                            SynExpr.CreatePartialApp([responseType; status], [
                                createIdent [ contentIdent ]
                            ])
                            |> wrappedReturn
                        elif response.Content.ContainsKey "image/png" && isNotNull response.Content.["image/png"].Schema && response.Content.["image/png"].Schema.Format = "binary" then
                            SynExpr.CreatePartialApp([responseType; status], [
                                createIdent [ contentIdent ]
                            ])
                            |> wrappedReturn
                        elif response.Content.ContainsKey "image/png" then
                            SynExpr.CreatePartialApp([responseType; status], [
                                createIdent [ contentIdent ]
                            ])
                            |> wrappedReturn
                        else
                            createIdent [ responseType; status ]
                            |> wrappedReturn

                    let statusCode status =
                        match status with
                        | nameof HttpStatusCode.OK -> 200
                        | nameof HttpStatusCode.Created -> 201
                        | nameof HttpStatusCode.Accepted -> 202
                        | nameof HttpStatusCode.NoContent -> 204
                        | nameof HttpStatusCode.PartialContent -> 206
                        | nameof HttpStatusCode.MovedPermanently -> 301
                        | nameof HttpStatusCode.Moved -> 301
                        | nameof HttpStatusCode.Found -> 302
                        | nameof HttpStatusCode.BadRequest -> 400
                        | nameof HttpStatusCode.Unauthorized -> 401
                        | nameof HttpStatusCode.PaymentRequired -> 402
                        | nameof HttpStatusCode.Forbidden -> 403
                        | nameof HttpStatusCode.NotFound -> 404
                        | nameof HttpStatusCode.MethodNotAllowed -> 405
                        | nameof HttpStatusCode.Conflict -> 409
                        | nameof HttpStatusCode.UnsupportedMediaType -> 415
                        | nameof HttpStatusCode.RequestedRangeNotSatisfiable -> 416
                        | nameof HttpStatusCode.UnprocessableEntity -> 422
                        | nameof HttpStatusCode.InternalServerError -> 500
                        | nameof HttpStatusCode.NotImplemented -> 501
                        | nameof HttpStatusCode.BadGateway -> 502
                        | nameof HttpStatusCode.ServiceUnavailable -> 503
                        | nameof HttpStatusCode.GatewayTimeout -> 504
                        | _ -> 0

                    let matchClause openApiResponse =
                        let (status, _) = openApiResponse
                        let ident = SynPat.Const(SynConst.Int32 (statusCode status), range0)

                        SynMatchClause.SynMatchClause (
                            ident,
                            None,
                            createOutput openApiResponse,
                            range0,
                            DebugPointAtTarget.Yes,
                            { ArrowRange = Some range0; BarRange = Some range0 }
                        )
                    if responses.Length = 1 then
                        createOutput responses.[0]
                    else
                        let matchWildClause openApiResponse =
                            SynMatchClause.SynMatchClause (
                                SynPat.Wild range0,
                                None,
                                createOutput openApiResponse,
                                range0,
                                DebugPointAtTarget.Yes,
                                { ArrowRange = Some range0; BarRange = Some range0 }
                        )

                        let responsesRevSorted =
                            if responses |> List.exists (fun (status, _) -> status = "DefaultResponse") then
                                (responses |> List.find(fun (status, _) -> status = "DefaultResponse"))
                                :: (responses
                                    |> List.filter (fun (status, _) -> status <> "DefaultResponse")
                                    |> List.sortByDescending (fun (status, _) -> statusCode status))
                            else
                                responses
                                |> List.sortByDescending (fun (status, _) -> statusCode status)

                        let allClausesButFirst =
                            responsesRevSorted |> List.tail |> List.map matchClause

                        let defaultClause =
                            responsesRevSorted |> List.head |> matchWildClause

                        let clausesSorted = defaultClause :: allClausesButFirst |> List.rev

                        SynExpr.CreateMatch(
                            SynExpr.CreateApp(
                                SynExpr.CreateIdent(
                                    Ident.Create "int") ,
                                createIdent [ "status" ]),
                            clausesSorted)

                let asyncBuilder expr =
                    if config.target = Target.Fable then
                        SynExpr.CreateAsync expr
                    else
                        if config.synchronous then
                            expr
                        else
                            match config.asyncReturnType with
                            | AsyncReturnType.Async -> SynExpr.CreateAsync expr
                            | AsyncReturnType.Task -> SynExpr.CreateTask expr

                let destructExpr httpFunc =
                    match config.target with
                    | Target.FSharp when config.synchronous -> deconstructResponse (httpCall httpFunc) returnExpr
                    | _ -> deconstructAsyncResponse (httpCall httpFunc) returnExpr

                let clientOperation httpFunc name = SynMemberDefn.CreateMember {
                    SynBindingRcd.Null with
                        XmlDoc = xmlDocsWithParams summary parameterDocs
                        Expr =
                            asyncBuilder (
                                createLetAssignment
                                    requestParts
                                    (SynExpr.CreateList requestValues)
                                    (destructExpr httpFunc)
                            )

                        Pattern =
                            SynPatRcd.CreateLongIdent(SynLongIdent.CreateString $"this.{name}", [
                                SynPatRcd.CreateParen(
                                    SynPatRcd.Tuple {
                                        Patterns = [
                                            for parameter in parameters do
                                                SynPatRcd.Typed {
                                                    Range = range0
                                                    Type = parameter.parameterType
                                                    Pattern =
                                                        if parameter.required
                                                        then SynPatRcd.CreateLongIdent(SynLongIdent.CreateString(parameter.parameterIdent), [])
                                                        else SynPatRcd.OptionalVal {
                                                            Range = range0
                                                            Id = Ident.Create parameter.parameterIdent
                                                        }
                                                }
                                        ]
                                        Range  = range0
                                    }
                                )
                            ])
                }

                match config.target with
                | Target.FSharp when config.synchronous ->
                    clientMembers.Add (clientOperation httpFunction memberName)
                | _ ->
                    clientMembers.Add (clientOperation httpFunctionAsync memberName)

    let clientType = SynModuleDecl.CreateType(info, Seq.toList clientMembers)

    let moduleContents = [
        if config.target = Target.FSharp then
            yield SynModuleDecl.CreateOpen "System.Net"
            yield SynModuleDecl.CreateOpen "System.Net.Http"
            yield SynModuleDecl.CreateOpen "System.Text"
            yield SynModuleDecl.CreateOpen "System.Threading"
        else
            yield SynModuleDecl.CreateOpen "Browser.Types"
            yield SynModuleDecl.CreateOpen "Fable.SimpleHttp"

        yield SynModuleDecl.CreateOpen $"{config.project}.Types"
        yield SynModuleDecl.CreateOpen $"{config.project}.Http"

        if config.asyncReturnType = AsyncReturnType.Task then
            // from the Ply package
            yield SynModuleDecl.CreateOpen "FSharp.Control.Tasks"
        // extra types generated from parameters
        for extraType in extraTypes do
            yield extraType
        // the main http client
        if Seq.length (safeSeq openApiDocument.Paths) > 0 then
            // only when we actually have members
            yield clientType
    ]

    let clientModule = CodeGen.createNamespace [ config.project ] moduleContents
    clientModule

let rec deleteFilesAndFolders directory isRoot =
    for file in Directory.GetFiles directory
        do File.Delete file
    for subdirectory in Directory.GetDirectories directory do
        deleteFilesAndFolders subdirectory false
        if not isRoot then Directory.Delete subdirectory

let path xs = Path.Combine(Array.ofList xs)
let write (content: string) filePath = File.WriteAllText(path filePath, content)

let generateProjectDocument
    (packageReferences: XElement seq)
    (files: XElement seq)
    (copyLocalLockFileAssemblies: bool option)
    (contentItems: XElement seq)
    (projectReferences: XElement seq)
    (disableImplicitFSharpCore: bool) =
    XDocument(
        XElement.ofStringName("Project",
            XAttribute.ofStringName("Sdk", "Microsoft.NET.Sdk"),
            seq {
            XElement.ofStringName("PropertyGroup",
                seq {
                    XElement.ofStringName("TargetFramework", "netstandard2.0")
                    XElement.ofStringName("LangVersion", "latest")
                    // when an explicit FSharp.Core is pinned (Fable target), the
                    // implicit SDK reference must be turned off to avoid a conflict
                    if disableImplicitFSharpCore then
                        XElement.ofStringName("DisableImplicitFSharpCoreReference", "true")
                    if copyLocalLockFileAssemblies.IsSome then
                        XElement.ofStringName("CopyLocalLockFileAssemblies",
                            if copyLocalLockFileAssemblies.Value
                            then "true"
                            else "false"
                        )
                })
            if not (files |> Seq.isEmpty) then
                XElement.ofStringName("ItemGroup", files)
            if not (contentItems |> Seq.isEmpty) then
                XElement.ofStringName("ItemGroup", contentItems)
            if not (packageReferences |> Seq.isEmpty) then
                XElement.ofStringName("ItemGroup", packageReferences)
            if not (projectReferences |> Seq.isEmpty) then
                XElement.ofStringName("ItemGroup", projectReferences)
        })
    )

let getJsonPart (url: string) : JObject option =
    match url.Split('#', StringSplitOptions.RemoveEmptyEntries) with
    | [| schemaUrl; path |] ->
        let schemaContent =
            client.GetStringAsync(schemaUrl)
            |> Async.AwaitTask
            |> Async.RunSynchronously

        let schemaJson = JObject.Parse(schemaContent)

        let jsonPath =
            path.Split('/', StringSplitOptions.RemoveEmptyEntries)
            |> String.concat "."

        let token = schemaJson.SelectToken(jsonPath)
        if token.Type = JTokenType.Object
        then Some (unbox<JObject> token)
        else None

    | [| schemaUrl |] ->
        let schemaContent =
            client.GetStringAsync(schemaUrl)
            |> Async.AwaitTask
            |> Async.RunSynchronously

        let schemaJson = JObject.Parse(schemaContent)
        Some schemaJson

    | _ ->
        None

let preprocessRelativeExternalReferences (schema: JObject) (url: string) =
    let rec iterate (part: JObject) =
        let properties = List.ofSeq(part.Properties())
        for property in properties do
            if property.Value.Type = JTokenType.Object then
                iterate (unbox<JObject> property.Value)
            elif property.Value.Type = JTokenType.Array then
                let elements = unbox<JArray> property.Value
                for element in elements do
                    if element.Type = JTokenType.Object then
                        iterate (unbox<JObject> element)
            elif property.Name = "$ref" && property.Value.Type = JTokenType.String then
                let refUrl = property.Value.ToObject<string>()
                // not absolute && not local -> relative
                if not (refUrl.StartsWith "http") && not (refUrl.StartsWith "#") then
                    // relative url
                    let modifiedUrl = Uri(Uri(url), refUrl)
                    match getJsonPart modifiedUrl.AbsoluteUri with
                    | Some resolvedObject ->
                        part.RemoveAll()
                        for resolvedProp in resolvedObject.Properties() do
                            part.Add(resolvedProp)
                    | None ->
                        property.Value <- JValue modifiedUrl.AbsoluteUri
                else
                    ()

    iterate schema
    schema


type ExternalResouceLoader(schema: string) =
    interface IStreamLoader with
        member self.LoadAsync(baseUri: Uri, uri: Uri, cancellationToken: System.Threading.CancellationToken) =
            let absoluteUri =
                if not uri.IsAbsoluteUri then
                    Uri(Uri(schema), uri.OriginalString)
                else
                    uri

            client.GetStreamAsync(absoluteUri)

/// Microsoft.OpenApi 3.x leaves optional collections null when they are absent
/// from the document, whereas the 1.x model always initialized them empty. The
/// generator relies on the old behaviour, so this walks the freshly-parsed
/// document and fills in empty collections.
let normalizeDocument (document: OpenApiDocument) =
    if isNotNull (box document) then
        let emptyExtensions () : IDictionary<string, IOpenApiExtension> = upcast Dictionary<string, IOpenApiExtension>()
        let visitedSchemas = HashSet<IOpenApiSchema>(HashIdentity.Reference)

        let rec normalizeSchema (schema: IOpenApiSchema) =
            if isNotNull (box schema) && visitedSchemas.Add schema then
                match box schema with
                | :? OpenApiSchema as concrete ->
                    if isNull concrete.Required then concrete.Required <- HashSet<string>()
                    if isNull concrete.Extensions then concrete.Extensions <- emptyExtensions()
                    if isNull concrete.Enum then concrete.Enum <- ResizeArray<JsonNode>()
                    if isNull concrete.AllOf then concrete.AllOf <- ResizeArray<IOpenApiSchema>()
                    if isNull concrete.AnyOf then concrete.AnyOf <- ResizeArray<IOpenApiSchema>()
                    if isNull concrete.OneOf then concrete.OneOf <- ResizeArray<IOpenApiSchema>()
                    if isNull concrete.Properties then concrete.Properties <- Dictionary<string, IOpenApiSchema>()
                    for property in concrete.Properties do normalizeSchema property.Value
                    for inner in concrete.AllOf do normalizeSchema inner
                    for inner in concrete.AnyOf do normalizeSchema inner
                    for inner in concrete.OneOf do normalizeSchema inner
                    normalizeSchema concrete.Items
                    normalizeSchema concrete.AdditionalProperties
                | _ ->
                    // a $ref - its target is normalized where it is defined
                    ()

        let normalizeMediaTypes (content: IDictionary<string, IOpenApiMediaType>) =
            if isNotNull content then
                for media in content.Values do
                    if isNotNull (box media) then normalizeSchema media.Schema

        let normalizeParameter (parameter: IOpenApiParameter) =
            match box parameter with
            | :? OpenApiParameter as concrete ->
                if isNull concrete.Extensions then concrete.Extensions <- emptyExtensions()
                if isNull concrete.Content then concrete.Content <- Dictionary<string, IOpenApiMediaType>()
                normalizeSchema concrete.Schema
                normalizeMediaTypes concrete.Content
            | _ -> ()

        let normalizeResponse (response: IOpenApiResponse) =
            match box response with
            | :? OpenApiResponse as concrete ->
                if isNull concrete.Extensions then concrete.Extensions <- emptyExtensions()
                if isNull concrete.Content then concrete.Content <- Dictionary<string, IOpenApiMediaType>()
                if isNull concrete.Headers then concrete.Headers <- Dictionary<string, IOpenApiHeader>()
                normalizeMediaTypes concrete.Content
            | _ -> ()

        let normalizeRequestBody (body: IOpenApiRequestBody) =
            match box body with
            | :? OpenApiRequestBody as concrete ->
                if isNull concrete.Extensions then concrete.Extensions <- emptyExtensions()
                if isNull concrete.Content then concrete.Content <- Dictionary<string, IOpenApiMediaType>()
                normalizeMediaTypes concrete.Content
            | _ -> ()

        let normalizeOperation (operation: OpenApiOperation) =
            if isNotNull (box operation) then
                if isNull operation.Extensions then operation.Extensions <- emptyExtensions()
                if isNull operation.Parameters then operation.Parameters <- ResizeArray<IOpenApiParameter>()
                if isNull operation.Tags then operation.Tags <- HashSet<OpenApiTagReference>()
                if isNull operation.Responses then operation.Responses <- OpenApiResponses()
                for parameter in operation.Parameters do normalizeParameter parameter
                for response in operation.Responses.Values do normalizeResponse response
                if isNotNull (box operation.RequestBody) then normalizeRequestBody operation.RequestBody

        if isNull document.Extensions then document.Extensions <- emptyExtensions()

        if isNotNull document.Components then
            let components = document.Components
            if isNotNull components.Schemas then
                for schema in components.Schemas.Values do normalizeSchema schema
            if isNotNull components.Responses then
                for response in components.Responses.Values do normalizeResponse response
            if isNotNull components.Parameters then
                for parameter in components.Parameters.Values do normalizeParameter parameter
            if isNotNull components.RequestBodies then
                for body in components.RequestBodies.Values do normalizeRequestBody body

        if isNotNull document.Paths then
            for pathItem in document.Paths.Values do
                if isNotNull (box pathItem) then
                    match box pathItem with
                    | :? OpenApiPathItem as concrete ->
                        if isNull concrete.Parameters then concrete.Parameters <- ResizeArray<IOpenApiParameter>()
                    | _ -> ()
                    if isNotNull pathItem.Parameters then
                        for parameter in pathItem.Parameters do normalizeParameter parameter
                    if isNotNull pathItem.Operations then
                        for operation in pathItem.Operations.Values do normalizeOperation operation

/// Reads and parses an OpenAPI/Swagger document from a JSON or YAML stream
/// using the Microsoft.OpenApi 3.x reader (supports OpenAPI 2.0, 3.0 and 3.1).
let loadOpenApiDocument (schema: Stream) (config: CodegenConfig) : OpenApiDocument * OpenApiDiagnostic =
    let settings = OpenApiReaderSettings()
    settings.AddYamlReader()
    if config.schema.StartsWith "http" then
        // customize how external references are resolved
        settings.CustomExternalLoader <- ExternalResouceLoader(config.schema) :> IStreamLoader
        settings.LoadExternalRefs <- true
    let format =
        if config.schema.EndsWith ".yaml" || config.schema.EndsWith ".yml"
        then "yaml"
        else "json"
    let result =
        OpenApiModelFactory.LoadAsync(schema, format, settings)
        |> Async.AwaitTask
        |> Async.RunSynchronously
    normalizeDocument result.Document
    result.Document, result.Diagnostic

let runConfig filePath =
    let config = resolveFile filePath
    match readConfig config with
    | Error errorMsg ->
        Console.WriteLine errorMsg
        1
    | Ok config ->
        let schema =
            if config.schema.StartsWith "http" && config.resolveReferences then
                let schemaContent =
                    config.schema
                    |> client.GetStringAsync
                    |> Async.AwaitTask
                    |> Async.RunSynchronously

                let schemaJson = JObject.Parse(schemaContent)
                let processedSchema = preprocessRelativeExternalReferences schemaJson config.schema
                getSchema (processedSchema.ToString()) config.overrideSchema

            elif config.schema.StartsWith "http" && config.schema.EndsWith ".json" then
                getSchema config.schema config.overrideSchema
            elif config.schema.StartsWith "http" && config.schema.EndsWith ".yaml" then
                let schemaContent =
                    config.schema
                    |> client.GetStringAsync
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
                let schemaBytes = Encoding.UTF8.GetBytes(schemaContent)
                new MemoryStream(schemaBytes) :> Stream
            elif config.schema.StartsWith "http" then
                getSchema config.schema config.overrideSchema
            else
                getSchema (resolveFile config.schema) config.overrideSchema
        let openApiDocument, diagnostics = loadOpenApiDocument schema config
        if diagnostics.Errors.Count > 0 && isNull openApiDocument then
            for error in diagnostics.Errors do
                System.Console.WriteLine error.Message
            1
        else
            let config = { config with odataSchema = openApiDocument.Extensions.ContainsKey "x-odata" }
            let outputDir = config.output
            // prepare output directory
            if Directory.Exists outputDir
            then deleteFilesAndFolders outputDir true
            else ignore(Directory.CreateDirectory outputDir)
            // generate global schema types
            let visitedTypes, globalTypesModule = createGlobalTypesModule openApiDocument config
            let formattedTypes = CodeGen.formatAst (CodeGen.createFile [ globalTypesModule ])
            // For the Fable target, append Thoth.Json `extra` coders for the
            // discriminator unions (declared in their own module after the types).
            let code =
                if config.target = Target.Fable
                then formattedTypes + createFableThothCoders openApiDocument config
                else formattedTypes
            // generate HTTP client wrapper, pass visited types
            let clientModule = createOpenApiClient openApiDocument visitedTypes config
            let clientModuleCode = CodeGen.formatAst (CodeGen.createFile [ clientModule ])
            write code [ outputDir; "Types.fs" ]
            write clientModuleCode [ outputDir; "Client.fs" ]
            let projectFile =
                let packages = [
                    if config.target = Target.FSharp then
                        XElement.PackageReference("FSharp.SystemTextJson", "1.4.36")
                        if config.asyncReturnType = AsyncReturnType.Task
                        then XElement.PackageReference("Ply", "0.3.1")
                    else
                        // pinned so Fable's project cracker does not hit an
                        // FSharp.Core downgrade (NU1605) from Thoth.Json's floor
                        XElement.PackageReference("FSharp.Core", "10.1.203")
                        XElement.PackageReference("Thoth.Json", "10.5.0")
                        XElement.PackageReference("Fable.SimpleHttp", "3.0.0")
                ]

                let files =
                    if config.target = Target.Fable then
                        // Fable target: Types.fs holds the Thoth `extra` coders that the
                        // Serializer (in OpenApiHttp.fs) references, so Types.fs must come
                        // first. Types.fs is pure types and does not depend on OpenApiHttp.fs.
                        [
                            XElement.Compile "Types.fs"
                            XElement.Compile "OpenApiHttp.fs"
                            XElement.Compile "Client.fs"
                        ]
                    else
                        [
                            XElement.Compile "OpenApiHttp.fs"
                            XElement.Compile "Types.fs"
                            XElement.Compile "Client.fs"
                        ]

                let copyLocalLockFileAssemblies = None
                let contentItems = [ ]
                let projectReferences = [ ]
                generateProjectDocument packages files copyLocalLockFileAssemblies contentItems projectReferences (config.target = Target.Fable)

            if config.target = Target.FSharp then
                let httpLibrary = HttpLibrary.library (config.asyncReturnType = AsyncReturnType.Task) config.project
                write httpLibrary [ outputDir; "OpenApiHttp.fs" ]
            else
                let httpLibrary = HttpLibrary.fableLibrary config.project
                write httpLibrary [ outputDir; "OpenApiHttp.fs" ]

            write (projectFile.ToString()) [ outputDir; $"{config.project}.fsproj" ]
            printfn "Succesfully generated project %s" (path [outputDir; $"{config.project}.fsproj" ])
            0

let showTags filePath =
    let config = resolveFile filePath
    match readConfig config with
    | Error errorMsg ->
        Console.WriteLine errorMsg
        1
    | Ok config ->
        let schema =
            if config.schema.StartsWith "http" && config.resolveReferences then
                let schemaContent =
                    config.schema
                    |> client.GetStringAsync
                    |> Async.AwaitTask
                    |> Async.RunSynchronously

                let schemaJson = JObject.Parse(schemaContent)
                let processedSchema = preprocessRelativeExternalReferences schemaJson config.schema
                getSchema (processedSchema.ToString()) config.overrideSchema

            elif config.schema.StartsWith "http" && config.schema.EndsWith ".json" then
                getSchema config.schema config.overrideSchema
            elif config.schema.StartsWith "http" && config.schema.EndsWith ".yaml" then
                let schemaContent =
                    config.schema
                    |> client.GetStringAsync
                    |> Async.AwaitTask
                    |> Async.RunSynchronously
                let schemaBytes = Encoding.UTF8.GetBytes(schemaContent)
                new MemoryStream(schemaBytes) :> Stream
            elif config.schema.StartsWith "http" then
                getSchema config.schema config.overrideSchema
            else
                getSchema (resolveFile config.schema) config.overrideSchema
        let openApiDocument, diagnostics = loadOpenApiDocument schema config
        if diagnostics.Errors.Count > 0 && isNull openApiDocument then
            for error in diagnostics.Errors do
                Console.WriteLine error.Message
            1
        elif isNull openApiDocument then
            Console.WriteLine "Could not parse the OpenAPI schema"
            1
        else
            let tags = [
                for path in safeSeq openApiDocument.Paths do
                for operation in safeSeq path.Value.Operations do
                if isNotNull operation.Value.OperationId then
                    for tag in operation.Value.Tags do tag.Name, operation.Value.OperationId
            ]

            let content =
                tags
                |> List.groupBy fst
                |> List.sortByDescending (fun (tag, operations) -> operations.Length)
                |> List.map (fun (tag, operations) -> $"Tag {tag} has {operations.Length} operations(s)")
                |> String.concat "\n"

            File.WriteAllText("tags.txt", content)
            printfn "Tags information saved to tags.txt"
            0

[<EntryPoint>]
let main argv =
    Console.InputEncoding <- Encoding.UTF8
    Console.OutputEncoding <- Encoding.UTF8
    match argv with
    | [| "--version" |] ->
        printfn "0.66.0"
        0
    | [| |] ->
        Console.WriteLine(logo)
        runConfig "./hawaii.json"
    | [| "--no-logo" |] ->
        runConfig "./hawaii.json"
    | [|"--config"; file|] ->
        Console.WriteLine(logo)
        runConfig file
    | [|"--config"; file; "--no-logo" |] ->
        runConfig file
    | [| "--from-odata-schema"; schema; "--output"; output |] ->
        printfn "Generating OpenAPI specs from OData schema at %s" schema
        if schema.StartsWith "http" then
            let schemaWithMetadata =
                if schema.EndsWith "$metadata"
                then schema
                else $"{schema.TrimEnd '/'}/$metadata"
            let openApiSchema = readExternalODataSchema schemaWithMetadata
            let simplified = simplifyRedundantSchemaParts (JObject.Parse openApiSchema)
            File.WriteAllText(resolveFile output, simplified.ToString(Formatting.Indented))
            printfn "Generated OpenAPI specs saved as %s" (resolveFile output)
            0
        elif schema.EndsWith ".xml" && File.Exists (resolveFile schema) then
            let openApiSchema = readLocalODataSchema schema
            let simplified = simplifyRedundantSchemaParts (JObject.Parse openApiSchema)
            File.WriteAllText(resolveFile output, simplified.ToString(Formatting.Indented))
            printfn "Generated OpenAPI specs saved as %s" (resolveFile output)
            0
        else
            printfn "Invalid OData schema"
            printfn "Schema %s" schema
            1

    | [| "--show-tags"; "--config"; filePath |] ->
        printfn "Extracting OpenAPI tags schema at %s" filePath
        showTags filePath
    | [| "--config"; filePath; "--show-tags" |] ->
        printfn "Extracting OpenAPI tags schema at %s" filePath
        showTags filePath
    | [| "--show-tags" |] ->
        showTags "./hawaii.json"
    | arguments ->
        printfn "Unknown arguments [%s]" (String.concat ", " arguments)
        1
