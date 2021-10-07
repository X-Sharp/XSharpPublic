//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.XSharp
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Threading.Tasks

BEGIN NAMESPACE XSharpModel
   INTERFACE IXSharpProject
      METHOD AddFileNode(fileName AS STRING) AS VOID
      //METHOD AddIntellisenseError(file AS STRING, line AS LONG, column AS LONG, Length AS LONG, errCode AS STRING, message AS STRING, sev AS DiagnosticSeverity) AS VOID
      METHOD ClearIntellisenseErrors(file AS STRING) AS VOID
      METHOD DeleteFileNode(fileName AS STRING) AS VOID
      METHOD DocumentGetText(file AS STRING, IsOpen REF LOGIC) AS STRING
      METHOD DocumentInsertLine(fileName AS STRING, line AS LONG, text AS STRING) AS LOGIC
      METHOD DocumentSetText(fileName AS STRING, text AS STRING) AS LOGIC
      METHOD FindProject(sProject AS STRING) AS Object
      METHOD GetIntellisenseErrorPos(fileName AS STRING) AS System.Collections.Generic.List<IXErrorPosition>
      METHOD HasFileNode(fileName AS STRING) AS LOGIC
      METHOD IsDocumentOpen(file AS STRING) AS LOGIC
      METHOD OpenElement(file AS STRING, line AS LONG, column AS LONG) AS VOID
      METHOD SetStatusBarAnimation(onOff AS LOGIC, id AS SHORT) AS VOID
      METHOD SetStatusBarText(message AS STRING) AS VOID
      METHOD ShowIntellisenseErrors() AS VOID
      METHOD SynchronizeKeywordCase(code as STRING, fileName as STRING) AS STRING
      METHOD RunInForeGroundThread( a as Action) AS VOID

         // Properties
      PROPERTY IntermediateOutputPath AS STRING GET
      PROPERTY IsVsBuilding AS LOGIC GET
      PROPERTY OutputFile AS STRING GET
      PROPERTY ParseOptions AS XSharpParseOptions GET
      PROPERTY PrefixClassesWithDefaultNamespace AS LOGIC GET
      PROPERTY RootNameSpace AS STRING GET
      PROPERTY Url AS STRING GET
      PROPERTY Dialect as XSharpDialect GET

   END INTERFACE

   INTERFACE IXErrorPosition
      PROPERTY Column AS LONG GET SET
      PROPERTY Length AS LONG GET SET
      PROPERTY Line AS LONG GET SET
   END INTERFACE

END NAMESPACE

