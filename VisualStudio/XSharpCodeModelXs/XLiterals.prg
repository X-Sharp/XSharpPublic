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
using LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING XSharp.Settings

BEGIN NAMESPACE XSharpModel



STATIC CLASS XLiterals
    STATIC CONSTRUCTOR
        // 0 : none; 1 : UPPER; 2 : lower; 3 : TitleCase
        _asKeyword1        := " AS "
        _isKeyword1        := " IS "
        _refKeyword1       := " REF "
        _outKeyword1       := " OUT "
        _paramsKeyword1    := " PARAMS "
        _asKeyword2        := " as "
        _isKeyword2        := " is "
        _refKeyword2       := " ref "
        _outKeyword2       := " out "
        _paramsKeyword2    := " params "
        _asKeyword3        := " As "
        _isKeyword3        := " Is "
        _refKeyword3       := " Ref "
        _outKeyword3       := " Out "
        _paramsKeyword3    := " Params "

        var lexer := XSharpLexer.Create("", "rules.txt", XSharpParseOptions.Default)
        var ruleNames := lexer:RuleNames
        var keywords := XDictionary<STRING,STRING>{StringComparer.OrdinalIgnoreCase}
        for var i := XSharpLexer.FIRST_KEYWORD+1 to XSharpLexer.LAST_KEYWORD-1
            var rule := lexer:Vocabulary:GetSymbolicName(i)
            if ! rule:Contains("_")
                keywords:Add(rule, rule)
            endif
        next
        _Keywords := keywords
        RETURN
    STATIC METHOD IsKeyword(name as string) as logic
        return _Keywords:ContainsKey(name)
    static method EscapeName(name as string) as string
        if IsKeyword(name)
            return "@@"+ name
        endif
        return name
    static method GetKeywords() AS IEnumerable<String>
        return _Keywords:Keys

    STATIC METHOD Choose(kw1 as string, kw2 as string, kw3 as string) as string
        SWITCH XEditorSettings.KeywordCase
        CASE KeywordCase.Upper
            return kw1
        CASE KeywordCase.Lower
            return kw2
        CASE KeywordCase.Title
        OTHERWISE
            return kw3
        END SWITCH

#region Fields

    STATIC PRIVATE _asKeyword1        AS STRING
    STATIC PRIVATE _asKeyword2        AS STRING
    STATIC PRIVATE _asKeyword3        AS STRING
    STATIC PRIVATE _isKeyword1        AS STRING
    STATIC PRIVATE _isKeyword2        AS STRING
    STATIC PRIVATE _isKeyword3        AS STRING
    STATIC PRIVATE _refKeyword1       AS STRING
    STATIC PRIVATE _refKeyword2       AS STRING
    STATIC PRIVATE _refKeyword3       AS STRING
    STATIC PRIVATE _outKeyword1       AS STRING
    STATIC PRIVATE _outKeyword2       AS STRING
    STATIC PRIVATE _outKeyword3       AS STRING
    STATIC PRIVATE _paramsKeyword1    AS STRING
    STATIC PRIVATE _paramsKeyword2    AS STRING
    STATIC PRIVATE _paramsKeyword3    AS STRING
    PRIVATE STATIC _Keywords    AS IDictionary<STRING,STRING>
#endregion
#region Properties

    STATIC PROPERTY AsKeyWord			AS STRING  GET Choose(_asKeyword1,_asKeyword2,_asKeyword3)
    STATIC PROPERTY IsKeyWord			AS STRING  GET Choose(_isKeyword1,_isKeyword2,_isKeyword3)
    STATIC PROPERTY RefKeyWord			AS STRING  GET Choose(_refKeyword1,_refKeyword2,_refKeyword3)
    STATIC PROPERTY OutKeyWord			AS STRING  GET Choose(_outKeyword1, _outKeyword2, _outKeyword3)
    STATIC PROPERTY ParamsKeyWord		AS STRING  GET Choose(_paramsKeyword1, _paramsKeyword2,_paramsKeyword3)
#endregion
#region Constants
    CONST PUBLIC ConstructorName := ".ctor" AS STRING
    CONST PUBLIC DestructorName := ".dtor" AS STRING
    CONST PUBLIC GlobalName := "(Global Scope)" AS STRING
    CONST PUBLIC VarType := "$VAR$" AS STRING
    CONST PUBLIC UsualType := "USUAL" AS STRING
    CONST PUBLIC ObjectType := "OBJECT" AS STRING
    CONST PUBLIC NoType := "$NOTYPE$" AS STRING
    CONST PUBLIC XppDeclaration := "_declaration" AS STRING
#endregion
    STATIC METHOD Capitalize( result as STRING) AS STRING
        SWITCH XEditorSettings.KeywordCase
        CASE KeywordCase.Lower
            RETURN result:ToLower()
        CASE KeywordCase.Title
            IF result:Contains(" ")
                var parts := result:Split(<CHAR>{' '})
                result := ""
                FOREACH VAR s IN parts
                    if s:Length > 1
                        result += s:Substring(0,1):ToUpper()+s:Substring(1):ToLower()+" "
                    elseif s:Length == 1
                        result += s:ToUpper()
                    endif
                NEXT
                RETURN result:Substring(0, result:Length-1)
            ELSE
                RETURN result:Substring(0,1):ToUpper()+result:Substring(1):ToLower()
            ENDIF
        OTHERWISE
            RETURN result:ToUpper()
        END SWITCH

    PUBLIC STATIC METHOD FormatKeyword(sKeyword AS STRING) AS STRING
        RETURN FormatKeyword(sKeyword, XEditorSettings.KeywordCase)

    PUBLIC STATIC METHOD FormatKeyword(sKeyword AS STRING, nKeywordCase AS KeywordCase) AS STRING
        IF sKeyword == NULL
            RETURN ""
        ENDIF
        SWITCH nKeywordCase
        CASE KeywordCase.None
            RETURN sKeyword
        CASE KeywordCase.Upper
            RETURN sKeyword:ToUpper()
        CASE KeywordCase.Lower
            RETURN sKeyword:ToLower()
        CASE KeywordCase.Title
            RETURN IIF(sKeyword:Length > 1 , sKeyword:Substring(0, 1):ToUpper() + sKeyword:Substring(1):ToLower() , sKeyword:ToUpper())
        END SWITCH
        RETURN sKeyword

    PUBLIC STATIC METHOD FormatKeyword(sKeyword AS OBJECT) AS STRING
        RETURN FormatKeyword(sKeyword:ToString(), XEditorSettings.KeywordCase)

    PUBLIC STATIC METHOD FormatKeyword(sKeyword AS OBJECT, nKeywordCase AS KeywordCase) AS STRING
        RETURN FormatKeyword(sKeyword:ToString(), nKeywordCase)

    PUBLIC STATIC METHOD FormatKeyword(keyword AS Kind) AS STRING
        RETURN FormatKeyword(keyword , XEditorSettings.KeywordCase)

    PUBLIC STATIC METHOD FormatKeyword(keyword AS Kind, nKeywordCase AS KeywordCase) AS STRING
        SWITCH (keyword)
        CASE Kind.VODefine
            RETURN XLiterals.FormatKeyword("define",nKeywordCase)
        CASE Kind.VOGlobal
            RETURN XLiterals.FormatKeyword("global",nKeywordCase)
        CASE Kind.VODLL
            RETURN XLiterals.FormatKeyword("_dll function",nKeywordCase)
        END SWITCH
        RETURN XLiterals.FormatKeyword(keyword:ToString(),nKeywordCase)

END CLASS



END NAMESPACE

