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
        PROPERTY ParseOptions AS XSharpParseOptions GET _parseOptions
        PROPERTY PrefixClassesWithDefaultNamespace AS LOGIC GET FALSE
        PROPERTY Project AS XProject AUTO
        PROPERTY RootNameSpace AS STRING GET ""
        PROPERTY Url AS STRING GET OrphanName+".xsproj"
        PROPERTY Dialect AS XSharpDialect GET XSharpDialect.Core
        PROPERTY EnforceSelf as LOGIC GET FALSE
        PROPERTY Name  AS STRING GET OrphanName
        #endregion
        PRIVATE _parseOptions AS XSharpParseOptions
        CONSTRUCTOR()
            VAR options := List<STRING>{}
            options.Add("dialect:Core")
            _parseOptions   := XSharpParseOptions.FromVsValues(options)

        METHOD AddFileNode(strFileName AS STRING) AS VOID
            RETURN

            //      METHOD AddIntellisenseError(file AS STRING, line AS LONG, column AS LONG, Length AS LONG, errCode AS STRING, message AS STRING, sev AS DiagnosticSeverity) AS VOID
            //         RETURN


        METHOD ClearIntellisenseErrors(file AS STRING) AS VOID
            RETURN


        METHOD DeleteFileNode(strFileName AS STRING) AS VOID
            RETURN


        METHOD DocumentGetText(file AS STRING, isOpen REF LOGIC) AS STRING
            //
            isOpen := FALSE
            RETURN ""

        METHOD DocumentInsertLine(fileName AS STRING, line AS LONG, text AS STRING) AS LOGIC
            //
            RETURN FALSE

        METHOD DocumentSetText(fileName AS STRING, text AS STRING) AS LOGIC
            //
            RETURN FALSE

        METHOD FindProject(sProject AS STRING) AS Object
            //
            RETURN NULL

        METHOD GetIntellisenseErrorPos(fileName AS STRING) AS System.Collections.Generic.List<IXErrorPosition>
            RETURN List<IXErrorPosition>{}

        METHOD HasFileNode(strFileName AS STRING) AS LOGIC
            RETURN TRUE

        METHOD OpenElement(file AS STRING, line AS LONG, column AS LONG) AS VOID
            RETURN


        METHOD RunInForeGroundThread( a as Action) AS VOID
            a()
            RETURN

        METHOD ShowIntellisenseErrors() AS VOID
            RETURN

        METHOD SynchronizeKeywordCase(code AS STRING, fileName AS STRING) AS STRING
            RETURN code



    END CLASS

END NAMESPACE

