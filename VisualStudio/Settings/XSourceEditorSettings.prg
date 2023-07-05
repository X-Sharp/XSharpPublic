//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp.Settings

    PUBLIC CLASS SourceCodeEditorSettings
        PUBLIC PROPERTY TabSize                    AS LONG AUTO
        PUBLIC PROPERTY IndentSize                 AS LONG AUTO
        PUBLIC PROPERTY TabsAsSpaces               AS LOGIC AUTO
        PUBLIC PROPERTY IndentStyle                AS LONG AUTO
        PUBLIC PROPERTY IndentFactor               AS LONG AUTO
        PUBLIC PROPERTY KeywordCase                AS KeywordCase AUTO
        PUBLIC PROPERTY UDCKeywordCase             AS LOGIC AUTO
        PUBLIC PROPERTY IdentifierCase             AS LOGIC AUTO
        PUBLIC PROPERTY TrimTrailingWhiteSpace     AS LOGIC AUTO
        PUBLIC PROPERTY InsertFinalNewline         AS LOGIC AUTO

        PUBLIC PROPERTY IndentTypeMembers          AS LOGIC AUTO
        PUBLIC PROPERTY IndentTypeFields           AS LOGIC AUTO
        PUBLIC PROPERTY IndentStatements           AS LOGIC AUTO
        PUBLIC PROPERTY IndentCaseContent          AS LOGIC AUTO
        PUBLIC PROPERTY IndentCaseLabel            AS LOGIC AUTO
        PUBLIC PROPERTY IndentContinuedLines       AS LOGIC AUTO
        PUBLIC PROPERTY IndentPreprocessorLines    AS LOGIC AUTO
        PUBLIC PROPERTY IndentNamespace            AS LOGIC AUTO

        CONSTRUCTOR()
            TabSize      := XEditorSettings.TabSize
            IndentSize   := XEditorSettings.IndentSize
            TabsAsSpaces := XEditorSettings.TabsAsSpaces
            IndentStyle  := XEditorSettings.IndentStyle
            IndentFactor := XEditorSettings.IndentFactor
            KeywordCase  := XEditorSettings.KeywordCase
            UDCKeywordCase := XEditorSettings.UDCKeywordCase
            IdentifierCase := XEditorSettings.IdentifierCase
            TrimTrailingWhiteSpace := XEditorSettings.TrimTrailingWhiteSpace
            InsertFinalNewline := XEditorSettings.InsertFinalNewline
            IndentTypeMembers := XEditorSettings.IndentTypeMembers
            IndentTypeFields := XEditorSettings.IndentTypeFields
            IndentStatements := XEditorSettings.IndentStatements
            IndentCaseContent := XEditorSettings.IndentCaseContent
            IndentCaseLabel := XEditorSettings.IndentCaseLabel
            IndentContinuedLines := XEditorSettings.IndentContinuedLines
            IndentPreprocessorLines := XEditorSettings.IndentPreprocessorLines
            IndentNamespace := XEditorSettings.IndentNamespace

    END CLASS

END NAMESPACE
