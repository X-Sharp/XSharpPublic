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
      PUBLIC STATIC PROPERTY EnableLogging      AS LOGIC AUTO
      PUBLIC STATIC PROPERTY EnableDatabaseLog  AS LOGIC AUTO
      PUBLIC STATIC PROPERTY EnableParseLog     AS LOGIC AUTO
      PUBLIC STATIC PROPERTY EnableTypelookupLog  AS LOGIC AUTO
      PUBLIC STATIC PROPERTY EnableReferenceInfoLog  AS LOGIC AUTO
      PUBLIC STATIC PROPERTY KeywordCase        AS INT AUTO
         
      PUBLIC STATIC PROPERTY DisableEntityParsing AS LOGIC AUTO
      PUBLIC STATIC PROPERTY DisableSyntaxHighlighting AS LOGIC AUTO
      PUBLIC STATIC PROPERTY DisableRegions  AS LOGIC AUTO
      PUBLIC STATIC PROPERTY DisableAssemblyReferences AS LOGIC AUTO
      PUBLIC STATIC PROPERTY DisableForeignProjectReferences AS LOGIC AUTO
      PUBLIC STATIC PROPERTY DisableXSharpProjectReferences AS LOGIC AUTO
         
END CLASS
END NAMESPACE