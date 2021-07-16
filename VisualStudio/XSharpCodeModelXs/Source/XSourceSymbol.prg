//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharpModel
USING System.Diagnostics
USING System.Collections.Generic
USING LanguageService.SyntaxTree

BEGIN NAMESPACE XSharpModel
    /// <summary>An symbol in the source code. Can be an entity but also a local or parameter</summary>
    CLASS XSourceSymbol INHERIT XSymbol IMPLEMENTS IXSourceSymbol
    PROTECTED _id    := -1                AS INT64
        PROPERTY Id   AS INT64                GET _id INTERNAL SET _id := VALUE
        PROPERTY File AS XFile                AUTO
        PROPERTY Range AS TextRange           AUTO
        PROPERTY Interval AS TextInterval     AUTO
        PROPERTY FileUsings AS IList<STRING>  GET IIF(SELF:File != NULL, SELF:File:Usings, <STRING>{})

        CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers)
            SUPER(name, kind, attributes)

        CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers,range AS TextRange, interval AS TextInterval)
            SUPER(name, kind, attributes)
            SELF:Range := range
            SELF:Interval := interval

        METHOD OpenEditor() AS VOID
            IF SELF:File?:Project?:ProjectNode != NULL
                SELF:File:Project:ProjectNode:OpenElement(SELF:File:SourcePath, SELF:Range:StartLine+1, (SELF:Range:StartColumn ))
            ENDIF

        METHOD InSameProject(otherSymbol AS XSourceSymbol) AS LOGIC
            IF SELF:File != NULL .AND. otherSymbol:File != NULL
                RETURN SELF:File:Project:FindXFile(otherSymbol:File:FullPath) != NULL
            ENDIF
            RETURN FALSE

        METHOD CopyValuesFrom(dbresult AS XDbResult) AS VOID
            SELF:Range    := dbresult:TextRange
            SELF:Interval := dbresult:TextInterval
            IF SELF:File == NULL
                SELF:File := XSolution.FindFullPath(dbresult:FileName)
            ELSEIF !String.IsNullOrEmpty(dbresult:FileName)
                IF String.Compare(SELF:File:FullPath, dbresult:FileName, TRUE) != 0
                    SELF:File := XSolution.FindFullPath(dbresult:FileName)
                ENDIF
            ENDIF

    END CLASS
END NAMESPACE
