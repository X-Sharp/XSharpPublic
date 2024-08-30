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
USING XSharpModel
BEGIN NAMESPACE XSharp.Settings
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
    PUBLIC STATIC PROPERTY Logger                           AS XSharpModel.ILogger AUTO := DummyLogger{}
    PUBLIC STATIC PROPERTY LanguageService                  AS OBJECT AUTO

    PUBLIC STATIC PROPERTY Disassembler AS STRING AUTO := ""
    PUBLIC STATIC PROPERTY HideIncludes AS LOGIC AUTO := FALSE
    PUBLIC STATIC PROPERTY Version AS Version AUTO := Version{}
    PUBLIC STATIC PROPERTY UseMicrosoftSQLite AS LOGIC AUTO := false

    PUBLIC STATIC PROPERTY IsVs15 AS LOGIC => Version:Major == 15
    PUBLIC STATIC PROPERTY IsVs16 AS LOGIC => Version:Major == 16
    PUBLIC STATIC PROPERTY IsVs17 AS LOGIC => Version:Major == 17
    PUBLIC STATIC PROPERTY IsArm  AS LOGIC AUTO GET PRIVATE SET

    STATIC CONSTRUCTOR
        SWITCH System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture
        CASE System.Runtime.InteropServices.Architecture.Arm
        CASE System.Runtime.InteropServices.Architecture.Arm64
            IsArm := TRUE
        OTHERWISE
            IsArm := FALSE
        END SWITCH

    PUBLIC STATIC METHOD EnableAll() AS VOID
        EnableBraceMatchLog           := TRUE
        EnableCodeCompletionLog       := TRUE
        EnableDatabaseLog             := TRUE
        EnableParameterLog            := TRUE
        EnableParseLog                := TRUE
        EnableQuickInfoLog            := TRUE
        EnableReferenceInfoLog        := TRUE
        EnableTypelookupLog           := TRUE
        RETURN

    PUBLIC STATIC METHOD Information(message AS STRING) AS VOID
        IF EnableLogging
            Logger:Information(message)
        ENDIF
        RETURN

    STATIC METHOD Log(cMessage AS STRING) AS VOID
        IF XSettings.EnableDatabaseLog .AND. XSettings.EnableLogging
            Logger.Information(cMessage)
        ENDIF
        RETURN


    PUBLIC STATIC METHOD Exception(ex AS Exception, msg as STRING) AS VOID
        Logger:Exception(ex, msg)
        RETURN

    PUBLIC STATIC METHOD Debug(msg as STRING) AS VOID
        Logger:Debug(msg)
        RETURN

    PUBLIC STATIC METHOD ShowMessageBox(cMessage as STRING) AS INT
        IF ShellLink != NULL
            RETURN ShellLink:ShowMessageBox(cMessage)
        ENDIF
        RETURN 0

 

    PUBLIC STATIC METHOD SetStatusBarText(cText AS STRING) AS VOID
        IF ShellLink != NULL_OBJECT
            ShellLink:SetStatusBarText(cText)
        ENDIF


    PUBLIC STATIC METHOD ClearIntellisenseErrors(file AS STRING) AS VOID
        IF ShellLink != NULL_OBJECT
            ShellLink:ClearIntellisenseErrors(file)
        ENDIF
        RETURN

    PUBLIC STATIC METHOD SynchronizeKeywordCase(code as string, fileName as string) AS string
        IF ShellLink != NULL_OBJECT
            return ShellLink:SynchronizeKeywordCase(code, fileName)
        ENDIF
        RETURN code


    PUBLIC STATIC METHOD SetStatusBarProgress(cMessage as STRING, nItem AS LONG, nTotal as LONG) AS VOID
        IF ShellLink != NULL_OBJECT
            ShellLink:SetStatusBarProgress(cMessage, nItem, nTotal)
        ENDIF

    PUBLIC STATIC METHOD SetStatusBarAnimation(onOff AS LOGIC, id AS SHORT) AS VOID
        IF ShellLink != NULL_OBJECT
            ShellLink:SetStatusBarAnimation(onOff, id)
        ENDIF

    PUBLIC STATIC PROPERTY IsVsBuilding AS LOGIC GET IIF(ShellLink != NULL, ShellLink:IsVsBuilding, FALSE)


    PUBLIC STATIC METHOD RunInForeGroundThread( a as Action) AS VOID
        IF ShellLink != NULL_OBJECT
            ShellLink:RunInForeGroundThread(a)
        ENDIF
        a()
        RETURN

END CLASS


STATIC CLASS XDocuments
  PUBLIC STATIC METHOD IsOpen(file AS STRING) AS LOGIC
        IF XSettings.ShellLink != NULL
            RETURN XSettings.ShellLink:IsDocumentOpen(file)
        ENDIF
        RETURN FALSE

    PUBLIC STATIC METHOD GetText(file AS STRING, isOpen ref LOGIC) AS STRING
        IF XSettings.ShellLink != NULL
            RETURN XSettings.ShellLink:DocumentGetText(file, ref isOpen )
        ENDIF
        RETURN ""
    PUBLIC STATIC METHOD SetText(file AS STRING, text as string) AS LOGIC
        IF XSettings.ShellLink != NULL
            RETURN XSettings.ShellLink:DocumentSetText(file, text)
        ENDIF
        RETURN FALSE
    PUBLIC STATIC METHOD InsertLine(file AS STRING, line AS LONG, text AS STRING) AS LOGIC
        IF XSettings.ShellLink != NULL
            RETURN XSettings.ShellLink:DocumentInsertLine(file, line, text)
        ENDIF
        RETURN FALSE
   PUBLIC STATIC METHOD Open(file AS STRING, line AS LONG, column AS LONG, lPreview as LOGIC) AS VOID
        IF XSettings.ShellLink != NULL
            XSettings.ShellLink:OpenDocument(file, line, column, lPreview)
        ENDIF

        
END CLASS


CLASS DummyLogger IMPLEMENTS XSharpModel.ILogger

#region Implement ILogger

    PUBLIC METHOD Information(sMsg AS STRING) AS VOID
        RETURN

    PUBLIC METHOD Debug(sMsg AS STRING) AS VOID
        RETURN

    PUBLIC METHOD Start() AS VOID
        RETURN

    PUBLIC METHOD Stop() AS VOID
        RETURN

    PUBLIC PROPERTY Active AS LOGIC GET FALSE


    PUBLIC METHOD SingleLine() AS VOID
        RETURN

    PUBLIC METHOD DoubleLine() AS VOID
        RETURN
    METHOD Exception (e as Exception, sMsg as STRING) AS VOID
        RETURN
#endregion
END CLASS


END NAMESPACE
