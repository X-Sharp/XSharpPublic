//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic
USING System.Diagnostics
USING XSharpModel
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser

BEGIN NAMESPACE XSharpModel
   
   [DebuggerDisplay("{DebuggerDisplay(),nq}")];
    CLASS XDbResult
      PROPERTY TypeName     AS STRING AUTO
      PROPERTY MemberName   AS STRING AUTO
      PROPERTY IsReference  AS LOGIC AUTO
      PROPERTY Kind         AS Kind AUTO
      PROPERTY Attributes   AS Modifiers AUTO
      PROPERTY FileName     AS STRING AUTO
      PROPERTY Project      AS STRING AUTO
      PROPERTY StartLine    AS INT AUTO 
      PROPERTY StartColum   AS INT AUTO
      PROPERTY EndLine      AS INT AUTO
      PROPERTY EndColumn    AS INT AUTO
      PROPERTY Start        AS INT AUTO
      PROPERTY Stop         AS INT AUTO
    END CLASS
END NAMESPACE
