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
   STATIC _keywordCase AS LONG
   STATIC CONSTRUCTOR
      SetKeywordCase(1) // upper
      
   
   
   STATIC METHOD SetKeywordCase(nCase AS LONG) AS VOID
       // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
         _keywordCase := nCase
      SWITCH nCase
      CASE 2
         _asKeyword        := " as "
         _refKeyword       := " ref "
         _outKeyword       := " out "
         _paramsKeyword    := " params "
      CASE 3
         _asKeyword        := " As "
         _refKeyword       := " Ref "
         _outKeyword       := " Out "
         _paramsKeyword    := " Params "
      CASE 1
      OTHERWISE
         _asKeyword        := " AS "
         _refKeyword       := " REF "
         _outKeyword       := " OUT "
         _paramsKeyword    := " PARAMS "
      END SWITCH
      RETURN
   STATIC PRIVATE _asKeyword        AS STRING
   STATIC PRIVATE _refKeyword       AS STRING
   STATIC PRIVATE _outKeyword       AS STRING
   STATIC PRIVATE _paramsKeyword    AS STRING
	STATIC PROPERTY AsKeyWord			AS STRING  GET _asKeyword
	STATIC PROPERTY RefKeyWord			AS STRING  GET _refKeyword
	STATIC PROPERTY OutKeyWord			AS STRING  GET _outKeyword
	STATIC PROPERTY ParamsKeyWord		AS STRING  GET _paramsKeyword
   CONST PUBLIC GlobalName := "(Global Scope)" AS STRING
   CONST PUBLIC VarType := "$VAR$" AS STRING
   CONST PUBLIC UsualType := "USUAL" AS STRING
   CONST PUBLIC NoType := "$NOTYPE$" AS STRING
   CONST PUBLIC XppDeclaration := "_declaration" AS STRING


   STATIC METHOD ToDisplayString(SELF mods AS Modifiers) AS STRING
      VAR res := mods:ToString():Replace(",","")
      SWITCH _keywordCase
      CASE 2
         RETURN res:ToLower()
      CASE 3
         RETURN res:Substring(0,1):ToUpper()+res:Substring(2):ToLower()
      OTHERWISE
         RETURN res
      END SWITCH
   
   STATIC METHOD ToDisplayString(SELF kind as Kind) AS STRING
      SWITCH _keywordCase
      CASE 2
         RETURN kind:ToString():ToLower()
      CASE 3
         VAR s := kind:ToString()
         RETURN s:Substring(0,1):ToUpper()+s:Substring(2):ToLower()
      OTHERWISE
         RETURN kind:ToString():ToUpper()
      END SWITCH

   END CLASS
   
END NAMESPACE