﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Concurrent
USING System.Collections.Generic
USING System.IO
USING System.Linq
USING System
BEGIN NAMESPACE XSharpModel
    STATIC CLASS XSettings
        // Fields
        PUBLIC STATIC PROPERTY EnableLogging                      AS LOGIC GET EnableFileLogging .or. EnableOutputWindowLogging .or. EnableDebugLogging
        PUBLIC STATIC PROPERTY EnableBraceMatchLog                AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableCodeCompletionLog            AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableDatabaseLog                  AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableParameterLog                 AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableParseLog                     AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableQuickInfoLog                 AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableReferenceInfoLog             AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableTypelookupLog                AS LOGIC AUTO

        PUBLIC STATIC PROPERTY EnableOutputWindowLogging          AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableFileLogging                  AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableDebugLogging                 AS LOGIC AUTO

        PUBLIC STATIC PROPERTY DisableAssemblyReferences          AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableBraceMatching               AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableCaseSynchronization         AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableClassViewObjectView         AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableCodeCompletion              AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableEditorDropDowns             AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableEntityParsing               AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableForeignProjectReferences    AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableGotoDefinition              AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableHighLightWord               AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableLightBulb                   AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableParameterInfo               AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisablePeekDefinition              AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableQuickInfo                   AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableRegions                     AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableSyntaxHighlighting          AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableXSharpProjectReferences     AS LOGIC AUTO

        PUBLIC STATIC PROPERTY KeywordCase                        AS KeywordCase AUTO
        PUBLIC STATIC PROPERTY IdentifierCase                     AS LOGIC AUTO
        PUBLIC STATIC PROPERTY UDCKeywordCase                     AS LOGIC AUTO

        PUBLIC STATIC PROPERTY EditorKeywordsInAll                AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorHideAdvancedMembers          AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorCompletionListTabs           AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorCompletionListAfterEachChar  AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorCompletionAutoPairs          AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorCommitChars                  AS STRING AUTO

        PUBLIC STATIC PROPERTY EditorNavigationIncludeFields        AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorNavigationSorted               AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorNavigationMembersOfCurrentTypeOnly AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorNavigationExcludeMembersFromOtherFiles AS LOGIC AUTO

        PUBLIC STATIC PROPERTY EditorTabSize                     AS LONG AUTO
        PUBLIC STATIC PROPERTY EditorIndentSize                 AS LONG AUTO
        PUBLIC STATIC PROPERTY EditorTabsAsSpaces               AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorIndentStyle                AS LONG AUTO
        PUBLIC STATIC PROPERTY EditorIndentFactor               AS LONG AUTO
        PUBLIC STATIC PROPERTY EditorInsertFinalNewline         AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorTrimTrailingWhiteSpace     AS LOGIC AUTO

        PUBLIC STATIC PROPERTY EditorShowDividers               AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorShowSingleLineDividers     AS LOGIC AUTO
        PUBLIC STATIC PROPERTY FormEditorMakeBackupFiles        AS LOGIC AUTO

        PUBLIC STATIC PROPERTY CodeGeneratorShowXmlComments     AS LOGIC AUTO
        PUBLIC STATIC PROPERTY CodeGeneratorPublicStyle         AS PublicStyle AUTO
        PUBLIC STATIC PROPERTY CodeGeneratorPrivateStyle        AS PrivateStyle AUTO

        PUBLIC STATIC PROPERTY IndentEntityContent               AS LOGIC AUTO
        PUBLIC STATIC PROPERTY IndentBlockContent                AS LOGIC AUTO
        PUBLIC STATIC PROPERTY IndentCaseContent                 AS LOGIC AUTO
        PUBLIC STATIC PROPERTY IndentCaseLabel                   AS LOGIC AUTO
        PUBLIC STATIC PROPERTY IndentMultiLines                  AS LOGIC AUTO

        PUBLIC STATIC PROPERTY MaxCompletionEntries             AS LONG AUTO := Int32.MaxValue
        PUBLIC STATIC PROPERTY ShellLink                        AS IXVsShellLink AUTO
        PUBLIC STATIC PROPERTY LanguageService                  AS OBJECT AUTO

        PUBLIC STATIC PROPERTY DebuggerMode                AS DebuggerMode AUTO
        PUBLIC STATIC PROPERTY DebuggerIsRunning           AS LOGIC GET DebuggerMode != DebuggerMode.Design

        PUBLIC STATIC METHOD LogMessage(message AS STRING) AS VOID
            IF EnableLogging .and. ShellLink != NULL
                ShellLink:LogMessage(message)
            ENDIF
         RETURN

        PUBLIC STATIC METHOD LogException(ex AS Exception, msg as STRING) AS VOID
         IF ShellLink != NULL
            ShellLink:LogException(ex, msg)
         ENDIF
         RETURN

        PUBLIC STATIC METHOD ShowMessageBox(cMessage as STRING) AS INT
         IF ShellLink != NULL
            RETURN ShellLink:ShowMessageBox(cMessage)
         ENDIF
         RETURN 0

      PUBLIC STATIC METHOD OpenDocument(file AS STRING, line AS LONG, column AS LONG, lPreview as LOGIC) AS VOID
         IF ShellLink != NULL
            ShellLink:OpenDocument(file, line, column, lPreview)
         ENDIF

        PUBLIC STATIC METHOD IsDocumentOpen(file AS STRING) AS LOGIC
         IF ShellLink != NULL
            RETURN ShellLink:IsDocumentOpen(file)
         ENDIF
         RETURN FALSE

        PUBLIC STATIC METHOD SetStatusBarText(cText AS STRING) AS VOID
        IF ShellLink != NULL_OBJECT
            ShellLink:SetStatusBarText(cText)
        ENDIF

        PUBLIC STATIC METHOD SetStatusBarProgress(cMessage as STRING, nItem AS LONG, nTotal as LONG) AS VOID
        IF ShellLink != NULL_OBJECT
           ShellLink:SetStatusBarProgress(cMessage, nItem, nTotal)
        ENDIF

      PUBLIC STATIC METHOD SetStatusBarAnimation(onOff AS LOGIC, id AS SHORT) AS VOID
        IF ShellLink != NULL_OBJECT
            ShellLink:SetStatusBarAnimation(onOff, id)
        ENDIF

       PUBLIC STATIC PROPERTY IsVsBuilding AS LOGIC GET IIF(ShellLink != NULL, ShellLink:IsVsBuilding, FALSE)

        PUBLIC STATIC METHOD FormatKeyword(sKeyword AS STRING) AS STRING
            RETURN FormatKeyword(sKeyword, XSettings.KeywordCase)

        PUBLIC STATIC METHOD FormatKeyword(sKeyword AS STRING, nKeywordCase AS KeywordCase) AS STRING
            IF sKeyword == NULL
                RETURN ""
            ENDIF
            SWITCH nKeywordCase
                CASE KeywordCase.None
                    RETURN sKeyword
                CASE KeywordCase.Upper
                    RETURN sKeyword:ToUpper()
                CASE KeywordCase.Lower
                    RETURN sKeyword:ToLower()
                CASE KeywordCase.Title
                    RETURN IIF(sKeyword:Length > 1 , sKeyword:Substring(0, 1):ToUpper() + sKeyword:Substring(1):ToLower() , sKeyword:ToUpper())
            END SWITCH
        RETURN sKeyword

        PUBLIC STATIC METHOD FormatKeyword(sKeyword AS OBJECT) AS STRING
            RETURN FormatKeyword(sKeyword:ToString(), XSettings.KeywordCase)

        PUBLIC STATIC METHOD FormatKeyword(sKeyword AS OBJECT, nKeywordCase AS KeywordCase) AS STRING
            RETURN FormatKeyword(sKeyword:ToString(), nKeywordCase)

        PUBLIC STATIC METHOD FormatKeyword(keyword AS Kind) AS STRING
            RETURN FormatKeyword(keyword , XSettings.KeywordCase)

        PUBLIC STATIC METHOD FormatKeyword(keyword AS Kind, nKeywordCase AS KeywordCase) AS STRING
            SWITCH (keyword)
                CASE Kind.VODefine
                    RETURN XSettings.FormatKeyword("define",nKeywordCase)
                CASE Kind.VOGlobal
                    RETURN XSettings.FormatKeyword("global",nKeywordCase)
                CASE Kind.VODLL
                    RETURN XSettings.FormatKeyword("_dll function",nKeywordCase)
            END SWITCH
            RETURN XSettings.FormatKeyword(keyword:ToString(),nKeywordCase)

    END CLASS

END NAMESPACE
