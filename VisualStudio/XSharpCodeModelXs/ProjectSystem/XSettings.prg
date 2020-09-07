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
   DELEGATE DisplayOutputMessage(message as string) AS VOID
   DELEGATE DisplayException(ex as Exception) AS VOID
   DELEGATE ShowMessageBox(message as string) AS INT
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
      
      PUBLIC STATIC PROPERTY KeywordCase                        AS INT AUTO
      
      PUBLIC STATIC PROPERTY DisplayOutputMessage             AS DisplayOutputMessage AUTO
      PUBLIC STATIC PROPERTY DisplayException                 AS DisplayException AUTO
      PUBLIC STATIC PROPERTY ShowMessageBox                   AS ShowMessageBox AUTO
      
      PRIVATE STATIC METHOD NoOutput(message as string) AS VOID
      RETURN
      PRIVATE STATIC METHOD NoException(ex as Exception) AS VOID
      RETURN
      PRIVATE STATIC METHOD NoMessageBox(message as string) AS INT
      RETURN 0
      
      
      STATIC CONSTRUCTOR
         DisplayOutputMessage := NoOutput
         DisplayException     := NoException
      ShowMessageBox       := NoMessageBox
      
      STATIC METHOD FormatKeyword(sKeyword as STRING) AS STRING
         IF sKeyword == NULL
            RETURN ""
         ENDIF
         SWITCH KeywordCase
            CASE 0
               RETURN sKeyword
            CASE 1
               RETURN sKeyword:ToUpper()
            CASE 2
               RETURN sKeyword:ToLower()
            CASE 3
               RETURN IIF(sKeyword:Length > 1 , sKeyword:Substring(0, 1):ToUpper() + sKeyword:Substring(1):ToLower() , sKeyword:ToUpper())
         END SWITCH
      RETURN sKeyword
      STATIC METHOD FormatKeyword(sKeyword as object) AS STRING
      return FormatKeyword(sKeyword:ToString())
      
      
      STATIC METHOD FormatKeyword(keyword as Kind) AS STRING
         switch (keyword)
            case Kind.VODefine
               return XSettings.FormatKeyword("define")
            case Kind.VOGlobal
               return XSettings.FormatKeyword("global")
            case Kind.VODLL
               return XSettings.FormatKeyword("_dll function")
         end switch
         return XSettings.FormatKeyword(keyword:ToString());
         
      END CLASS
END NAMESPACE