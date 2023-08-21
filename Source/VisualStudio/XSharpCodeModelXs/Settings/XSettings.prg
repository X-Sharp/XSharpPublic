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
        PUBLIC STATIC Property Version as Version AUTO := Version{}

        PUBLIC STATIC PROPERTY IsVs15 AS LOGIC => Version:Major == 15
        PUBLIC STATIC PROPERTY IsVs16 AS LOGIC => Version:Major == 16
        PUBLIC STATIC PROPERTY IsVs17 AS LOGIC => Version:Major == 17

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



    END CLASS

    /// <summary>This interface allows the code model to call back into the VS Shell</summary>
   INTERFACE IXVsShellLink
        METHOD OpenDocument(file AS STRING, line AS LONG, column AS LONG, lPreview as LOGIC) AS VOID
        METHOD IsDocumentOpen(file as STRING) AS LOGIC
        METHOD SetStatusBarText(cText AS STRING) AS VOID
        METHOD SetStatusBarProgress(cMessage as STRING, nItem AS LONG, nTotal as LONG) AS VOID
        METHOD SetStatusBarAnimation(onOff AS LOGIC, id AS SHORT) AS VOID
        METHOD ShowMessageBox(message AS STRING) AS INT
        PROPERTY IsVsBuilding AS LOGIC GET
   END INTERFACE


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
