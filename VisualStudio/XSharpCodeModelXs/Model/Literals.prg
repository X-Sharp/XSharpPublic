//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Diagnostics
USING System
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp

BEGIN NAMESPACE XSharpModel
   
   STATIC CLASS XLiterals
   STATIC _nKeywordCase  := -1 AS LONG
   STATIC PROPERTY Initialized as LOGIC GET _nKeywordCase == -1
   STATIC METHOD SetKeywordCase(lUpperCase as LOGIC) AS VOID
      _nKeywordCase := IIF(lUpperCase,1,0)
      RETURN
   
	STATIC PROPERTY AsKeyWord			AS STRING GET IIF(_nKeywordCase == 1, " AS ", " as ")
	STATIC PROPERTY RefKeyWord			AS STRING GET IIF(_nKeywordCase == 1, " REF ", " ref ")
	STATIC PROPERTY OutKeyWord			AS STRING GET IIF(_nKeywordCase == 1, " OUT ", " out ")
	STATIC PROPERTY ParamsKeyWord		AS STRING GET IIF(_nKeywordCase == 1, " PARAMS ", " params ")

	STATIC PROPERTY KeywordsUpperCase AS LOGIC
		GET 
			RETURN _nKeywordCase == 1
		END GET
	END PROPERTY


   STATIC METHOD ToDisplayString(SELF mods as Modifiers) AS STRING
      IF KeywordsUpperCase
         RETURN mods:ToString():ToUpper()
      ENDIF
      RETURN mods:ToString():ToLower()
   
   STATIC METHOD ToDisplayString(SELF kind as Kind) AS STRING
      IF KeywordsUpperCase
         RETURN kind:DisplayName():ToUpper()
      ENDIF
      RETURN kind:DisplayName():ToLower()

   END CLASS
   
END NAMESPACE