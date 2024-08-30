//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.XSharp
using XSharp.Parser
USING System.Diagnostics

BEGIN NAMESPACE XSharpModel
    [DebuggerDisplay("Orphaned files")];
CLASS OrphanedFilesProject IMPLEMENTS IXSharpProject
    PUBLIC CONST OrphanName := "(OrphanedFiles)" AS STRING
#region properties
    PROPERTY IntermediateOutputPath AS STRING GET ""
    PROPERTY OutputFile AS STRING GET ""
    PROPERTY ParseOptions AS XParseOptions GET _parseOptions
    PROPERTY PrefixClassesWithDefaultNamespace AS LOGIC GET FALSE
    PROPERTY Project AS XProject AUTO
    PROPERTY RootNameSpace AS STRING GET ""
    PROPERTY Url AS STRING GET OrphanName+".xsproj"
    PROPERTY Dialect AS XDialect GET XDialect.Core
    PROPERTY EnforceSelf as LOGIC GET FALSE
    PROPERTY DisplayName  AS STRING GET "Miscellaneous Files"
    PROPERTY Name as STRING GET OrphanName
#endregion
    PRIVATE _parseOptions AS XParseOptions
    CONSTRUCTOR()
        VAR options := List<STRING>{}
        options.Add("dialect:Core")
        _parseOptions   := XParseOptions.FromVsValues(options)

    METHOD AddFileNode(strFileName AS STRING) AS VOID
        RETURN

        //      METHOD AddIntellisenseError(file AS STRING, line AS LONG, column AS LONG, Length AS LONG, errCode AS STRING, message AS STRING, sev AS DiagnosticSeverity) AS VOID
        //         RETURN

    METHOD DeleteFileNode(strFileName AS STRING) AS VOID
        RETURN


    METHOD FindProject(sProject AS STRING) AS Object
        //
        RETURN NULL

    METHOD HasFileNode(strFileName AS STRING) AS LOGIC
        RETURN TRUE


END CLASS

END NAMESPACE

