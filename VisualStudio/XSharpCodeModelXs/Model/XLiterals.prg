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
   STATIC CONSTRUCTOR
       // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
      _asKeyword1        := " AS "
      _refKeyword1       := " REF "
      _outKeyword1       := " OUT "
      _paramsKeyword1    := " PARAMS "
      _asKeyword2        := " as "
      _refKeyword2       := " ref "
      _outKeyword2       := " out "
      _paramsKeyword2    := " params "
      _asKeyword3        := " As "
      _refKeyword3       := " Ref "
      _outKeyword3       := " Out "
      _paramsKeyword3    := " Params "
      RETURN
   STATIC METHOD Choose(kw1 as string, kw2 as string, kw3 as string) as string
      SWITCH XSettings.KeywordCase
         CASE 2
            return kw2
         CASE 3
            return kw3
      END SWITCH
      return kw1
            
         
   STATIC PRIVATE _asKeyword1        AS STRING
   STATIC PRIVATE _asKeyword2        AS STRING
   STATIC PRIVATE _asKeyword3        AS STRING
   STATIC PRIVATE _refKeyword1       AS STRING
   STATIC PRIVATE _refKeyword2       AS STRING
   STATIC PRIVATE _refKeyword3       AS STRING
   STATIC PRIVATE _outKeyword1       AS STRING
   STATIC PRIVATE _outKeyword2       AS STRING
   STATIC PRIVATE _outKeyword3       AS STRING
   STATIC PRIVATE _paramsKeyword1    AS STRING
   STATIC PRIVATE _paramsKeyword2    AS STRING
   STATIC PRIVATE _paramsKeyword3    AS STRING
	STATIC PROPERTY AsKeyWord			AS STRING  GET Choose(_asKeyword1,_asKeyword2,_asKeyword3)
	STATIC PROPERTY RefKeyWord			AS STRING  GET Choose(_refKeyword1,_refKeyword2,_refKeyword3)
	STATIC PROPERTY OutKeyWord			AS STRING  GET Choose(_outKeyword1, _outKeyword2, _outKeyword3)
	STATIC PROPERTY ParamsKeyWord		AS STRING  GET Choose(_paramsKeyword1, _paramsKeyword2,_paramsKeyword3)
   CONST PUBLIC GlobalName := "(Global Scope)" AS STRING
   CONST PUBLIC VarType := "$VAR$" AS STRING
   CONST PUBLIC UsualType := "USUAL" AS STRING
   CONST PUBLIC NoType := "$NOTYPE$" AS STRING
   CONST PUBLIC XppDeclaration := "_declaration" AS STRING


   STATIC METHOD ToDisplayString(SELF mods AS Modifiers) AS STRING
      if (mods == Modifiers.None)
         return ""
      endif
      VAR res := mods:ToString():Replace(",","")
      SWITCH XSettings.KeywordCase
      CASE 2
         RETURN res:ToLower()
      CASE 3
         RETURN res:Substring(0,1):ToUpper()+res:Substring(1):ToLower()
      OTHERWISE
         RETURN res
      END SWITCH
   
   STATIC METHOD ToDisplayString(SELF kind as Kind) AS STRING
      SWITCH XSettings.KeywordCase
      CASE 2
         RETURN kind:ToString():ToLower()
      CASE 3
         VAR s := kind:ToString()
         RETURN s:Substring(0,1):ToUpper()+s:Substring(1):ToLower()
      OTHERWISE
         RETURN kind:ToString():ToUpper()
      END SWITCH

   END CLASS
   
END NAMESPACE