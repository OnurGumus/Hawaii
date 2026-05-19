module CodeGen

open FsAst
open Fantomas.FCS.Xml
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.Core
open Fantomas.FCS.Text
open Fantomas.Core

let createNamespace (names: seq<string>) declarations =
    let nameParts =
        names
        |> Seq.collect (fun name ->
            if name.Contains "."
            then name.Split('.')
            else [| name |]
        )

    let xmlDoc = PreXmlDoc.Create [ ]
    SynModuleOrNamespace.SynModuleOrNamespace([ for name in nameParts -> Ident.Create name ], true, SynModuleOrNamespaceKind.DeclaredNamespace,declarations,  xmlDoc, [ ], None, range.Zero, { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.Namespace range.Zero })

let createQualifiedModule (idens: seq<string>) declarations =
    let nameParts =
        idens
        |> Seq.collect (fun name ->
            if name.Contains "."
            then name.Split('.')
            else [| name |]
        )

    let xmlDoc = PreXmlDoc.Create [ ]
    SynModuleOrNamespace.SynModuleOrNamespace([ for ident in nameParts -> Ident.Create ident ], true, SynModuleOrNamespaceKind.NamedModule,declarations,  xmlDoc, [ SynAttributeList.Create [ SynAttribute.RequireQualifiedAccess()  ]  ], None, range.Zero, { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.Module range.Zero })

let createFile modules =
    let qualfiedNameOfFile = QualifiedNameOfFile.QualifiedNameOfFile(Ident.Create "IrrelevantFileName")
    ParsedImplFileInput.ParsedImplFileInput("IrrelevantFileName", false, qualfiedNameOfFile, [], [], modules, (false, false), { ConditionalDirectives = []; CodeComments = [] }, Set.empty)

let formatAstInternal ast =
    let cfg = {
        FormatConfig.Default
            with
                IndentSize = 4
                MaxIfThenElseShortWidth = 4
    }

    CodeFormatter.FormatASTAsync(ast, cfg)

let formatAst file =
    formatAstInternal (ParsedInput.ImplFile file)
    |> Async.RunSynchronously