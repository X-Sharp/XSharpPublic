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
        PUBLIC STATIC PROPERTY DisableCaseSynchronization         AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableClassViewObjectView         AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableEditorDropDowns             AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableEntityParsing               AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableForeignProjectReferences    AS LOGIC AUTO
        PUBLIC STATIC PROPERTY DisableXSharpProjectReferences     AS LOGIC AUTO

        PUBLIC STATIC PROPERTY CodeGeneratorShowXmlComments     AS LOGIC AUTO
        PUBLIC STATIC PROPERTY CodeGeneratorPublicStyle         AS PublicStyle AUTO
        PUBLIC STATIC PROPERTY CodeGeneratorPrivateStyle        AS PrivateStyle AUTO

        PUBLIC STATIC PROPERTY ShellLink                        AS IXVsShellLink AUTO
        PUBLIC STATIC PROPERTY LanguageService                  AS OBJECT AUTO

        PUBLIC STATIC PROPERTY DebuggerMode                     AS DebuggerMode AUTO
        PUBLIC STATIC PROPERTY DebuggerIsRunning                AS LOGIC GET DebuggerMode != DebuggerMode.Design

        PUBLIC STATIC PROPERTY Disassembler AS STRING AUTO := ""
        PUBLIC STATIC PROPERTY HideIncludes AS LOGIC AUTO := FALSE

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
            RETURN FormatKeyword(sKeyword, XEditorSettings.KeywordCase)

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
            RETURN FormatKeyword(sKeyword:ToString(), XEditorSettings.KeywordCase)

        PUBLIC STATIC METHOD FormatKeyword(sKeyword AS OBJECT, nKeywordCase AS KeywordCase) AS STRING
            RETURN FormatKeyword(sKeyword:ToString(), nKeywordCase)

        PUBLIC STATIC METHOD FormatKeyword(keyword AS Kind) AS STRING
            RETURN FormatKeyword(keyword , XEditorSettings.KeywordCase)

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
