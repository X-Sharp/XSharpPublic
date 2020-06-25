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
   /// <summary>An entity in the source code</summary>
   CLASS XSourceElement INHERIT XElement
      PROTECTED _id    := -1                AS Int64                         
      PROPERTY Id   AS INT64                GET _id INTERNAL SET _id := value
      PROPERTY File AS XFile                AUTO 
      PROPERTY Range AS TextRange           AUTO
      PROPERTY Interval AS TextInterval     AUTO
      PROPERTY FileUsings AS IList<STRING>  GET IIF(SELF:File != NULL, SELF:File:Usings, <STRING>{})
      
      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes as Modifiers)
         SUPER(name, kind, attributes)
         
      CONSTRUCTOR(name AS STRING, kind AS Kind, attributes as Modifiers,range AS TextRange, interval AS TextInterval)
         SUPER(name, kind, attributes)
         SELF:Range := range
         SELF:Interval := interval
         
      METHOD OpenEditor() AS VOID
         IF SELF:File?:Project?:ProjectNode != NULL
            SELF:File:Project:ProjectNode:OpenElement(SELF:File:SourcePath, SELF:Range:StartLine+1, (SELF:Range:StartColumn ))
         ENDIF

   END CLASS
END NAMESPACE      