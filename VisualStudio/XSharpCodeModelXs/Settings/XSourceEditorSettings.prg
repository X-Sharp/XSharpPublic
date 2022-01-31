﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharpModel

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

        PUBLIC PROPERTY IndentEntityContent            AS LOGIC AUTO
        PUBLIC PROPERTY IndentBlockContent            AS LOGIC AUTO
        PUBLIC PROPERTY IndentCaseContent            AS LOGIC AUTO
        PUBLIC PROPERTY IndentCaseLabel            AS LOGIC AUTO
        PUBLIC PROPERTY IndentMultiLines              AS LOGIC AUTO

        CONSTRUCTOR()
            TabSize      := XSettings.EditorTabSize
            IndentSize   := XSettings.EditorIndentSize
            TabsAsSpaces := XSettings.EditorTabsAsSpaces
            IndentStyle  := XSettings.EditorIndentStyle
            IndentFactor := XSettings.EditorIndentFactor
            KeywordCase  := XSettings.KeywordCase
            UDCKeywordCase := XSettings.UDCKeywordCase
            IdentifierCase := XSettings.IdentifierCase
            TrimTrailingWhiteSpace := XSettings.EditorTrimTrailingWhiteSpace
            InsertFinalNewline := XSettings.EditorInsertFinalNewline
            IndentEntityContent := XSettings.IndentEntityContent
            IndentBlockContent := XSettings.IndentBlockContent
            IndentCaseContent := XSettings.IndentCaseContent
            IndentCaseLabel := XSettings.IndentCaseLabel
            IndentMultiLines := XSettings.IndentMultiLines

    END CLASS

END NAMESPACE
