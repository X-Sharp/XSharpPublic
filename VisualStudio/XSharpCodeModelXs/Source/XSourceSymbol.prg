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
    /// <summary>
    /// Location in the source where the Symbol is in line/column
    /// </summary>
    /// <value></value>
    PROPERTY Range AS TextRange           AUTO
    /// <summary>
    /// Location in the source where the item is in start / end position
    /// </summary>
    /// <value></value>
    PROPERTY Prototype      as STRING GET SELF:KindKeyword+" "+SELF:Name
    PROPERTY Interval       AS TextInterval     AUTO
    PROPERTY FileUsings     AS IList<STRING>  GET  IIF(SELF:File != NULL, SELF:File:Usings, (IList<STRING>) STRING[]{0})
    /// <summary>
    /// 1 Based Line Number
    /// </summary>
    PROPERTY LineNumber     AS INT GET SELF:Range:StartLine+1
    /// <summary>
    /// 1 Based Column Number
    /// </summary>
    PROPERTY ColumnNumber   AS INT GET SELF:Range:StartColumn+1
    PROPERTY Location       AS STRING
        GET
            IF SELF:File == NULL
                RETURN ""
            ENDIF
            RETURN SELF:File:SourcePath + " (" + SELF:LineNumber:ToString()+" , "+SELF:ColumnNumber:ToString()+")"
        END GET
    END PROPERTY
    CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers)
        SUPER(name, kind, attributes)

    CONSTRUCTOR(name AS STRING, kind AS Kind, attributes AS Modifiers,Range AS TextRange, Interval AS TextInterval)
        SUPER(name, kind, attributes)
        SELF:Range := Range
        SELF:Interval := Interval

    METHOD OpenEditor() AS VOID
        IF SELF:File != NULL
            XSettings.OpenDocument(SELF:File:SourcePath, SELF:Range:StartLine, SELF:Range:StartColumn , FALSE)
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
                var newFile := XSolution.FindFullPath(dbresult:FileName)
                if newFile != null
                    SELF:File := newFile
                endif
            ENDIF
        ENDIF

END CLASS
END NAMESPACE
