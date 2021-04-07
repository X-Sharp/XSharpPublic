//
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
    DELEGATE DisplayOutputMessage(message AS STRING) AS VOID
    DELEGATE DisplayException(ex AS Exception) AS VOID
    DELEGATE ShowMessageBox(message AS STRING) AS INT
    STATIC CLASS XSettings
        // Fields
        PUBLIC STATIC PROPERTY EnableLogging                      AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableBraceMatchLog                AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableCodeCompletionLog            AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableDatabaseLog                  AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableParameterLog                 AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableParseLog                     AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableQuickInfoLog                 AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableReferenceInfoLog             AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EnableTypelookupLog                AS LOGIC AUTO
        
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
        
        PUBLIC STATIC PROPERTY EditorUseDotAsUniversalSelector    AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorKeywordsInAll                AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorHideAdvancedMembers          AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorCompletionListTabs           AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorCompletionListAfterEachChar  AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorCompletionAutoPairs          AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorCommitChars                  AS STRING AUTO
        
        PUBLIC STATIC PROPERTY EditorFormatAlignDoCase            AS LOGIC AUTO
        PUBLIC STATIC PROPERTY EditorFormatAlignMethod            AS LOGIC AUTO
        //PUBLIC STATIC PROPERTY EditorFormatAlignDoCase            AS LOGIC AUTO
        
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
        
        PUBLIC STATIC PROPERTY DisplayOutputMessage             AS DisplayOutputMessage AUTO
        PUBLIC STATIC PROPERTY DisplayException                 AS DisplayException AUTO
        PUBLIC STATIC PROPERTY ShowMessageBox                   AS ShowMessageBox AUTO

        PUBLIC STATIC PROPERTY DebuggerIsRunning                AS LOGIC AUTO

        PRIVATE STATIC METHOD NoOutput(message AS STRING) AS VOID
        RETURN
        PRIVATE STATIC METHOD NoException(ex AS Exception) AS VOID
        RETURN
        PRIVATE STATIC METHOD NoMessageBox(message AS STRING) AS INT
        RETURN 0
        
        
        STATIC CONSTRUCTOR
            DisplayOutputMessage := NoOutput
            DisplayException     := NoException
            ShowMessageBox       := NoMessageBox

        STATIC METHOD FormatKeyword(sKeyword AS STRING) AS STRING
            RETURN FormatKeyword(sKeyword, XSettings.KeywordCase)
            
        STATIC METHOD FormatKeyword(sKeyword AS STRING, nKeywordCase AS KeywordCase) AS STRING
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
         
        STATIC METHOD FormatKeyword(sKeyword AS OBJECT) AS STRING
            RETURN FormatKeyword(sKeyword:ToString(), XSettings.KeywordCase)
        
        STATIC METHOD FormatKeyword(sKeyword AS OBJECT, nKeywordCase AS KeywordCase) AS STRING
            RETURN FormatKeyword(sKeyword:ToString(), nKeywordCase)
        
        STATIC METHOD FormatKeyword(keyword AS Kind) AS STRING
            RETURN FormatKeyword(keyword , XSettings.KeywordCase)
            
        STATIC METHOD FormatKeyword(keyword AS Kind, nKeywordCase AS KeywordCase) AS STRING
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
