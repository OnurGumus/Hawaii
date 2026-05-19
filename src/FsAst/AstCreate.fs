[<AutoOpen>]
module FsAst.AstCreate
open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open Fantomas.FCS.Xml

let range0 = range.Zero

type Ident with
    static member Create (text: string) =
        // Fantomas 7's mkIdent decides whether to surround an identifier with backticks
        // purely by comparing the ident's range width to its text length: if the range
        // is `text.Length + 4` wide it emits ``text``. Hawaii uses zero ranges everywhere,
        // so for identifiers that need escaping (F# keywords, names with spaces, etc.) we
        // synthesise a range of exactly that width to make Fantomas backtick them.
        if not (String.IsNullOrEmpty text) && PrettyNaming.DoesIdentifierNeedBackticks text then
            let escapedRange =
                Range.mkRange "" (Position.mkPos 0 0) (Position.mkPos 0 (text.Length + 4))
            Ident(text, escapedRange)
        else
            Ident(text, range.Zero)
    static member CreateLong (text: string) =
        text.Split([|'.'|]) |> List.ofArray |> List.map Ident.Create

[<AutoOpen>]
module SynLongIdentHelpers =
    let mkSynLongIdent (idents: LongIdent) : SynLongIdent =
        SynLongIdent(idents, List.replicate (max 0 (idents.Length - 1)) range.Zero, List.replicate idents.Length None)

type SynLongIdent with
    static member Create (texts: string list) : SynLongIdent =
        mkSynLongIdent (texts |> List.map Ident.Create)
    static member CreateString (text: string) : SynLongIdent =
        mkSynLongIdent (Ident.CreateLong text)
    static member CreateFromLongIdent (longIdent: LongIdent) : SynLongIdent =
        mkSynLongIdent longIdent

    member x.AsString =
        let lid = x.LongIdent
        let sb = Text.StringBuilder()
        for i in 0 .. lid.Length - 2 do
            sb.Append lid.[i].idText |> ignore
            sb.Append '.' |> ignore
        sb.Append lid.[lid.Length-1].idText |> ignore
        sb.ToString()

type SynPatLongIdentRcd with
    static member Create (id, args, ?extraId, ?typarDecls, ?access) =
        { Id = id; ExtraId = extraId; TyparDecls = typarDecls ; Args = args; Access = access; Range = range.Zero }

type SynArgPats with
    static member Empty =
        SynArgPats.Pats[]

type SynPatRcd with
    static member CreateLongIdent (id, args: SynPatRcd list) =
        SynPatRcd.LongIdent (SynPatLongIdentRcd.Create(id, args |> List.map (fun a -> a.FromRcd) |> SynArgPats.Pats ))
    static member CreateTuple patterns =
        SynPatRcd.Tuple { Patterns = patterns; Range = range.Zero }
    static member CreateParen pattern =
        SynPatRcd.Paren { Pattern = pattern; Range = range.Zero }
    static member CreateAttrib (pattern, attributes) =
        SynPatRcd.Attrib { Pattern = pattern; Attributes = attributes; Range = range.Zero }
    static member CreateTyped (pattern, typ) =
        SynPatRcd.Typed { Pattern = pattern; Type = typ; Range = range.Zero }
    static member CreateNamed (id, pattern) =
        SynPatRcd.Named { Pattern = pattern; Id = id; IsThis = false; Access = None; Range = range.Zero }
    static member CreateWild =
        SynPatRcd.Wild { Range = range.Zero }

type QualifiedNameOfFile with
    static member Create name =
        QualifiedNameOfFile(Ident.Create name)

type SynMemberFlags with
    static member InstanceMember : SynMemberFlags =
        { IsInstance = true
          IsDispatchSlot = false
          IsOverrideOrExplicitImpl = false
          IsFinal = false
          GetterOrSetterIsCompilerGenerated = false
          MemberKind = SynMemberKind.Member }
    static member StaticMember : SynMemberFlags =
        { SynMemberFlags.InstanceMember with IsInstance = false }

type SynConst with
    static member CreateString s =
        SynConst.String(s, SynStringKind.Regular, range.Zero)

type SynExpr with
    static member CreateConst cnst =
        SynExpr.Const(cnst, range.Zero)
    static member CreateConstString s =
        SynExpr.CreateConst (SynConst.CreateString s)
    static member CreateTyped (expr, typ) =
        SynExpr.Typed(expr, typ, range.Zero)
    static member CreateApp (funcExpr, argExpr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, false, funcExpr, argExpr, range.Zero)
    static member CreateAppInfix (funcExpr, argExpr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, true, funcExpr, argExpr, range.Zero)
    static member CreateIdent id =
        SynExpr.Ident(id)
    static member CreateIdentString id =
        SynExpr.Ident(Ident.Create id)
    static member CreateLongIdent (isOptional, id, altNameRefCell) =
        SynExpr.LongIdent(isOptional, id, altNameRefCell, range.Zero)
    static member CreateLongIdent id =
        SynExpr.CreateLongIdent(false, id, None)
    static member CreateLongIdent (names: string list) =
        SynExpr.CreateLongIdent(SynLongIdent.Create names)
    static member CreateParen expr =
        SynExpr.Paren(expr, range.Zero, Some range.Zero, range.Zero)
    static member CreateTuple list =
        SynExpr.Tuple(false, list, List.replicate (max 0 (List.length list - 1)) range.Zero, range.Zero)
    static member CreateParenedTuple list =
        SynExpr.CreateTuple list
        |> SynExpr.CreateParen
    static member CreateUnit =
        SynExpr.CreateConst SynConst.Unit
    static member CreateNull =
        SynExpr.Null(range.Zero)
    static member CreateIfThen(ifExpr, thenExpr) =
        SynExpr.IfThenElse(ifExpr, thenExpr, None, DebugPointAtBinding.Yes range0, false, range0, { IfKeyword = range0; IsElif = false; ThenKeyword = range0; ElseKeyword = None; IfToThenRange = range0 })
    static member CreateIfThenElse(ifExpr, thenExpr, elseExpr) =
        SynExpr.IfThenElse(ifExpr, thenExpr, Some elseExpr, DebugPointAtBinding.Yes range0, false, range0, { IfKeyword = range0; IsElif = false; ThenKeyword = range0; ElseKeyword = Some range0; IfToThenRange = range0 })
    static member CreateList(exprs: seq<SynExpr>) =
        SynExpr.ArrayOrList(false, Seq.toList exprs, range0)
    static member CreateRecord (fields: list<RecordFieldName * option<SynExpr>>) =
        let fields = fields |> List.map (fun (rfn, synExpr) -> SynExprRecordField(rfn, Some range.Zero, synExpr, None))
        SynExpr.Record(None, None, fields, range.Zero )
    static member CreateAsync(expr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.CreateIdent(Ident.Create "async"), SynExpr.ComputationExpr(false, expr, range0), range0)
    static member CreateTask(expr) =
        SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.CreateIdent(Ident.Create "task"), SynExpr.ComputationExpr(false, expr, range0), range0)
    static member  CreateReturn(expr) =
        SynExpr.YieldOrReturn((false, true), expr, range0)
    static member CreatePartialApp(name: string, exprs: SynExpr list) =
        let funcParts =
            if name.Contains "."
            then List.ofArray (name.Split '.')
            else [ name ]

        let functionArg = SynExpr.CreateLongIdent funcParts
        exprs
        |> List.fold (fun expr argument -> SynExpr.CreateApp(expr, argument)) functionArg

    static member CreatePartialApp(names: string list, exprs: SynExpr list) =
        let functionArg = SynExpr.CreateLongIdent names
        exprs
        |> List.fold (fun expr argument -> SynExpr.CreateApp(expr, argument)) functionArg
    static member CreateRecordUpdate (copyInfo: SynExpr, fieldUpdates: SynExprRecordField list ) =
        let blockSep = (range.Zero, None)
        let copyInfo = Some (copyInfo, blockSep)
        SynExpr.Record (None, copyInfo, fieldUpdates, range.Zero)
    /// Creates:
    ///
    /// ```
    /// match matchExpr with
    /// | clause1
    /// | clause2
    /// ...
    /// | clauseN
    /// ```
    static member CreateMatch(matchExpr, clauses) =
        SynExpr.Match(DebugPointAtBinding.Yes range0, matchExpr, clauses, range0, { MatchKeyword = range0; WithKeyword = range0 })
    /// Creates : `instanceAndMethod(args)`
    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, args) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        SynExpr.CreateApp(valueExpr, args)
    /// Creates : `instanceAndMethod()`
    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent) =
        SynExpr.CreateInstanceMethodCall(instanceAndMethod, SynExpr.CreateUnit)
    /// Creates : `instanceAndMethod<type1, type2,... type}>(args)`
    static member CreateInstanceMethodCall(instanceAndMethod : SynLongIdent, instanceMethodsGenericTypes, args) =
        let valueExpr = SynExpr.CreateLongIdent instanceAndMethod
        let valueExprWithType = SynExpr.TypeApp(valueExpr, range0, instanceMethodsGenericTypes, [], None, range0, range0 )
        SynExpr.CreateApp(valueExprWithType, args)
    /// Creates: expr1; expr2; ... exprN
    static member CreateSequential exprs =
        let seqExpr expr1 expr2 = SynExpr.Sequential(DebugPointAtSequential.SuppressNeither, false, expr1, expr2, range0, SynExprSequentialTrivia.Zero)
        let rec inner exprs state =
            match state, exprs with
            | None, [] -> SynExpr.CreateConst SynConst.Unit
            | Some expr, [] -> expr
            | None, [single] -> single
            | None, [one;two] -> seqExpr one two
            | Some exp, [single] -> seqExpr exp single
            | None, head::shoulders::tail ->
                seqExpr head shoulders
                |> Some
                |> inner tail
            | Some expr, head::tail ->
                seqExpr expr head
                |> Some
                |> inner tail
        inner exprs None


type SynType with
    static member CreateApp (typ, args, ?isPostfix) =
        SynType.App(typ, Some range.Zero, args, List.replicate (max 0 (List.length args - 1)) range.Zero, Some range.Zero, (defaultArg isPostfix false), range.Zero)
    static member CreateLongIdent id =
        SynType.LongIdent(id)
    static member CreateLongIdent s =
        SynType.CreateLongIdent(SynLongIdent.CreateString s)
    static member CreateUnit =
        SynType.CreateLongIdent("unit")
    static member CreateFun (fieldTypeIn, fieldTypeOut) =
        SynType.Fun (fieldTypeIn, fieldTypeOut, range.Zero, { ArrowRange = range.Zero })

    static member Create(name: string) = SynType.CreateLongIdent name

    static member Option(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Option",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member ResizeArray(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "ResizeArray",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member Set(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Set",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member NativePointer(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "nativeptr",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member Option(inner: string) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Option",
            typeArgs=[ SynType.Create inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member Dictionary(key, value) =
        SynType.App(
            typeName=SynType.LongIdent(SynLongIdent.Create [ "System"; "Collections"; "Generic"; "Dictionary" ]),
            typeArgs=[ key; value ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member Map(key, value) =
        SynType.App(
            typeName=SynType.CreateLongIdent "Map",
            typeArgs=[ key; value ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member List(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "list",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member Array(inner: SynType) =
        SynType.App(
            typeName=SynType.CreateLongIdent "array",
            typeArgs=[ inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member List(inner: string) =
        SynType.App(
            typeName=SynType.CreateLongIdent "list",
            typeArgs=[ SynType.Create inner ],
            commaRanges = [ ],
            isPostfix = false,
            range=range0,
            greaterRange=Some range0,
            lessRange=Some range0
        )

    static member DateTimeOffset() =
        SynType.LongIdent(SynLongIdent.Create [ "System"; "DateTimeOffset" ])

    static member TimeSpan() =
        SynType.LongIdent(SynLongIdent.Create [ "System"; "TimeSpan" ])

    static member DateTime() =
        SynType.LongIdent(SynLongIdent.Create [ "System"; "DateTime" ])

    static member Guid() =
        SynType.LongIdent(SynLongIdent.Create [ "System"; "Guid" ])

    static member Int() =
        SynType.Create "int"

    static member UInt() =
        SynType.Create "uint"

    static member Int8() =
        SynType.Create "int8"

    static member UInt8() =
        SynType.Create "uint8"

    static member Int16() =
        SynType.Create "int16"

    static member UInt16() =
        SynType.Create "uint16"

    static member Int64() =
        SynType.Create "int64"

    static member UInt64() =
        SynType.Create "uint64"

    static member String() =
        SynType.Create "string"

    static member Bool() =
        SynType.Create "bool"

    static member Float() =
        SynType.Create "float"

    static member Float32() =
        SynType.Create "float32"

    static member Double() =
        SynType.Create "float"

    static member Decimal() =
        SynType.Create "decimal"

    static member Unit() =
        SynType.Create "unit"

    static member BigInt() =
        SynType.Create "bigint"

    static member Byte() =
        SynType.Create "byte"

    static member Char() =
        SynType.Create "char"

type SynArgInfo with
    static member Empty =
        SynArgInfo(SynAttributes.Empty, false, None)
    static member CreateId id =
        SynArgInfo(SynAttributes.Empty, false, Some id)
    static member CreateIdString id =
        SynArgInfo.CreateId(Ident.Create id)

type SynPatRcd with
    static member CreateNull =
        SynPatRcd.Null { Range = range.Zero }

type SynValInfo with
    static member Empty =
        SynValInfo([], SynArgInfo.Empty)

type SynBindingReturnInfoRcd with
    static member Create typ =
        { Type = typ; Range = range.Zero; Attributes = [] }

type SynBindingRcd with
    static member Null =
        {   Access = None
            Kind = SynBindingKind.Normal
            IsInline = false
            IsMutable = false
            Attributes = SynAttributes.Empty
            XmlDoc = PreXmlDoc.Empty
            ValData = SynValData(Some SynMemberFlags.InstanceMember, SynValInfo.Empty, None)
            Pattern = SynPatRcd.CreateNull
            ReturnInfo = None
            Expr = SynExpr.Null range.Zero
            Range = range.Zero
            Bind = DebugPointAtBinding.NoneAtInvisible
        }
    static member Let =
        { SynBindingRcd.Null with
            ValData = SynValData(None, SynValInfo([], SynArgInfo.Empty), None)
            Expr = SynExpr.CreateTyped(SynExpr.CreateNull, SynType.CreateUnit)
        }

type SynComponentInfoRcd with
    static member Create id =
        {   Attributes = SynAttributes.Empty
            Parameters = None
            Constraints = []
            Id = id
            XmlDoc = PreXmlDoc.Empty
            PreferPostfix = false
            Access = None
            Range = range.Zero
        }

type SynMemberDefn with
    static member CreateImplicitCtor (ctorArgs: SynSimplePat list) =
        let pats =
            ctorArgs
            |> List.map (fun sp ->
                match sp with
                | SynSimplePat.Id(ident, _, _, _, _, range) -> SynPat.Named(SynIdent(ident, None), false, None, range)
                | SynSimplePat.Typed(SynSimplePat.Id(ident, _, _, _, _, idRange), typ, range) ->
                    SynPat.Typed(SynPat.Named(SynIdent(ident, None), false, None, idRange), typ, range)
                | _ -> SynPat.Wild range0)
        let ctorPat =
            match pats with
            | [] -> SynPat.Paren(SynPat.Const(SynConst.Unit, range0), range0)
            | [ single ] -> SynPat.Paren(single, range0)
            | many -> SynPat.Paren(SynPat.Tuple(false, many, List.replicate (many.Length - 1) range0, range0), range0)
        SynMemberDefn.ImplicitCtor(None, SynAttributes.Empty, ctorPat, None, PreXmlDoc.Empty, range.Zero, { AsKeyword = None } )
    static member CreateImplicitCtor() =
        SynMemberDefn.CreateImplicitCtor []

    static member CreateMember (binding:SynBindingRcd) =
        SynMemberDefn.Member(binding.FromRcd, range.Zero)
    static member CreateInterface(interfaceType, members) =
        SynMemberDefn.Interface(interfaceType, None, members, range.Zero)

type SynTypeDefnReprObjectModelRcd with
    static member Create members =
        {   Kind = SynTypeDefnKind.Unspecified
            Members = members
            Range = range.Zero
        }

type SynTypeDefnRcd with
    static member Create (info: SynComponentInfoRcd, members) =
        {   Info = info
            Repr = SynTypeDefnReprObjectModelRcd.Create(members).FromRcd
            Members = []
            Range = range.Zero
        }
    static member CreateSimple (info: SynComponentInfoRcd, simple: SynTypeDefnSimpleRepr, ?members) =
        {   Info = info
            Repr =  SynTypeDefnRepr.Simple(simple, range.Zero)
            Members = Option.defaultValue [] members
            Range = range.Zero
        }

type SynModuleDecl with
    static member CreateType (info, members) =
        SynModuleDecl.Types([SynTypeDefnRcd.Create(info, members).FromRcd], range.Zero)
    static member CreateSimpleType (info, simple: SynTypeDefnSimpleReprRcd, ?members) =
        SynModuleDecl.Types( [SynTypeDefnRcd.CreateSimple(info, simple.FromRcd, members = Option.defaultValue [] members).FromRcd], range.Zero)
    static member CreateOpen id =
        SynModuleDecl.Open(id, range.Zero)
    static member CreateOpen (fullNamespaceOrModuleName: string) =
        SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent.CreateString fullNamespaceOrModuleName, range.Zero), range.Zero)
    static member CreateHashDirective (directive, values: string list) =
        let args = values |> List.map (fun v -> ParsedHashDirectiveArgument.String(v, SynStringKind.Regular, range.Zero))
        SynModuleDecl.HashDirective (ParsedHashDirective (directive, args, range.Zero), range.Zero)
    static member CreateLet (bindings: SynBindingRcd list) =
        SynModuleDecl.Let(false, bindings |> List.map(fun b -> b.FromRcd), range.Zero)
    static member CreateAttribute(ident, expr, isProp, ?target) =
            { SynAttribute.TypeName = ident
              SynAttribute.ArgExpr = expr
              SynAttribute.Target = target
              SynAttribute.AppliesToGetterAndSetter = isProp
              SynAttribute.Range = range.Zero }
    static member CreateAttributes(attributes) =
        SynModuleDecl.Attributes(attributes, range.Zero)
    static member CreateNestedModule(info : SynComponentInfoRcd, members) =
        SynModuleDecl.NestedModule(info.FromRcd, false, members, false, range.Zero, SynModuleDeclNestedModuleTrivia.Zero)

type SynModuleOrNamespaceRcd with
    static member CreateModule id =
        {   Id = id
            IsRecursive = false
            Kind = SynModuleOrNamespaceKind.NamedModule
            Declarations = []
            XmlDoc = PreXmlDoc.Empty
            Attributes = SynAttributes.Empty
            Access = None
            Range = range.Zero
        }
    static member CreateNamespace id =
        { SynModuleOrNamespaceRcd.CreateModule id with
            Kind = SynModuleOrNamespaceKind.DeclaredNamespace
        }
    member x.AddDeclarations decls =
        { x with
            Declarations = List.append x.Declarations decls
        }
    member x.AddDeclaration decl =
        x.AddDeclarations [decl]

type ParsedImplFileInputRcd with
    static member CreateFs name =
        {   File = sprintf "%s.fs" name
            IsScript = false
            QualName = QualifiedNameOfFile.Create name
            Pragmas = []
            HashDirectives = []
            Modules = []
            IsLastCompiland = true
            IsExe = false
        }
    member x.AddModules (modules: SynModuleOrNamespaceRcd list) =
        { x with
            Modules = List.append x.Modules (modules |> List.map (fun m -> m.FromRcd))
        }

    member x.AddModule mdl =
        x.AddModules [mdl]

type ParsedInput with
    static member CreateImplFile (implFile: ParsedImplFileInputRcd) =
        ParsedInput.ImplFile implFile.FromRcd

type SynTypeDefnSimpleReprEnumRcd with
    static member Create (cases: SynEnumCaseRcd list) =
        { Cases = cases |> List.map (fun c -> c.FromRcd)
          Range = range.Zero }

type SynTypeDefnSimpleReprRecordRcd with
    static member Create (fields: SynFieldRcd list) =
        { Access = None
          Fields = (fields |> List.map (fun f -> f.FromRcd))
          Range = range.Zero }

type SynTypeDefnSimpleReprUnionRcd with
    static member Create cases =
        { Access = None; Cases = cases; Range = range.Zero }

    static member Create (fields: SynUnionCaseRcd list) : SynTypeDefnSimpleReprUnionRcd=
        { Access = None
          Cases = fields |> List.map (fun f -> f.FromRcd)
          Range = range.Zero }

type SynUnionCaseRcd with
    static member Create(id, typ) : SynUnionCaseRcd =
        { Attributes = SynAttributes.Empty
          Id = id
          Type = typ
          XmlDoc = PreXmlDoc.Empty
          Access = None
          Range = range.Zero }

type SynUnionCaseKind with
    static member Create(synFieldList : SynFieldRcd list) =
        SynUnionCaseKind.Fields(synFieldList |> List.map (fun sf -> sf.FromRcd ))

type SynEnumCaseRcd with
    static member Create (id, cnst) =
        {   Attributes = SynAttributes.Empty
            Id = id
            Constant = cnst
            XmlDoc = PreXmlDoc.Empty
            Range = range.Zero
        }

type SynFieldRcd with
    static member Create(id, typ, ?isMutable) : SynFieldRcd =
        let isMutable = defaultArg isMutable false
        {   Attributes = SynAttributes.Empty
            IsStatic = false
            Id = Some id
            Type = typ
            IsMutable = isMutable
            XmlDoc = PreXmlDoc.Empty
            Access = None
            Range = range.Zero
        }
    static member Create(id, typ) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent typ)
    static member CreateInt(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "int")
    static member CreateIntOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "int" ]) ]
    static member CreateString(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "string")
    static member CreateStringOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "string" ]) ]
    static member CreateFloat(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "float")
    static member CreateFloatOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "float" ]) ]
    static member CreateBool(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "bool")
    static member CreateBoolOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "bool" ]) ]
    static member CreateDecimal(id) =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateLongIdent "decimal")
    static member CreateDecimalOption(id) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ "decimal" ]) ]
    static member CreateOption(id, optional) =
        SynFieldRcd.CreateApp id (SynLongIdent.Create [ "Option" ]) [ (SynLongIdent.Create [ optional ]) ]
    static member CreateApp id typ args =
        SynFieldRcd.Create(Ident.Create id, SynType.CreateApp(SynType.CreateLongIdent typ, args |> List.map (SynType.CreateLongIdent)))

type SynAttributeList with
    static member Create(attrs: SynAttribute list) : SynAttributeList =
        {
            Attributes = attrs
            Range = range0
        }

    static member Create(attr: SynAttribute) : SynAttributeList =
        {
            Attributes = [ attr ]
            Range = range0
        }

    static member Create([<ParamArray>] attrs: SynAttribute[]) : SynAttributeList =
        {
            Attributes = List.ofArray attrs
            Range = range0
        }

type SynAttribute with
    static member Create(name: string) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Unit, range0)
           Range = range0
           Target = None
           TypeName = mkSynLongIdent [ Ident.Create name ]
        }

    static member Create(name: string, argument: string) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.String(argument, SynStringKind.Regular, range0), range0)
           Range = range0
           Target = None
           TypeName = mkSynLongIdent [ Ident.Create name ]
        }

    static member Create(name: string, argument: bool) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Bool argument, range0)
           Range = range0
           Target = None
           TypeName = mkSynLongIdent [ Ident.Create name ]
        }

    static member Create(name: string, argument: int) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (SynConst.Int32 argument, range0)
           Range = range0
           Target = None
           TypeName = mkSynLongIdent [ Ident.Create name ]
        }

    static member Create(name: string, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = mkSynLongIdent [ Ident.Create name ]
        }

    static member Create(name: Ident, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = mkSynLongIdent [ name ]
        }

    static member Create(name: Ident list, argument: SynConst) : SynAttribute =
        {
           AppliesToGetterAndSetter = false
           ArgExpr = SynExpr.Const (argument, range0)
           Range = range0
           Target = None
           TypeName = mkSynLongIdent name
        }

    static member RequireQualifiedAccess() =
        SynAttribute.Create("RequireQualifiedAccess")

    static member CompiledName(valueArg: string) =
        SynAttribute.Create("CompiledName", valueArg)

type PreXmlDoc with
    static member Create lines = PreXmlDoc.Create(Seq.toArray lines, range0)

    static member Create (docs: string option) =
        PreXmlDoc.Create [
            if docs.IsSome
            then docs.Value
        ]

    static member Create(docs: string) =
        PreXmlDoc.Create [
            if not (String.IsNullOrWhiteSpace docs)
            then docs
        ]

type SynSimplePat with
    static member CreateTyped(ident, ``type``) =
        let ssp = SynSimplePat.Id(ident, None, false, false, false, range.Zero)
        SynSimplePat.Typed(ssp, ``type``, range.Zero )