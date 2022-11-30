
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharpModel
USING System.Diagnostics
USING System.Collections.Generic
using LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING System.Runtime.InteropServices


/// <summary>
/// This structure with the size of an Int is used to store
/// single token or double token keywords that are using
/// for the formatting code
/// </summary>
[StructLayout(LayoutKind.Explicit, Size := 4)];
STRUCTURE XSharpModel.XKeyword IMPLEMENTS IEquatable<XKeyword>, IEqualityComparer<XKeyword>
    [FieldOffset(0)];
    PUBLIC INITONLY Kw1 as XTokenType
    [FieldOffset(2)];
    PUBLIC INITONLY Kw2 as XTokenType
    [FieldOffset(0)];
    PRIVATE INITONLY _code as int
    PROPERTY IsEmpty  as LOGIC => _code == 0
    PROPERTY IsBegin  as LOGIC => Kw1 == XTokenType.Begin
    PROPERTY IsEnd    as LOGIC => Kw1 == XTokenType.End
    PROPERTY IsSingle as LOGIC => Kw2 == XTokenType.None

    CONSTRUCTOR(kw1 as LONG, kw2 as LONG)
        SELF( (XTokenType) kw1, (XTokenType) kw2)


    CONSTRUCTOR(kw1 as XTokenType, kw2 as XTokenType)
        _code := 0
        SELF:Kw1 := kw1
        if kw2 == XTokenType.Var
            SELF:Kw2 := XTokenType.None
        else
            SELF:Kw2 := kw2
        endif
        if kw1 == XTokenType.Id .or. kw2 == XTokenType.Id
            SELF:Kw1 := SELF:Kw2 := XTokenType.None
        endif

    CONSTRUCTOR(kw1 as LONG)
        SELF((XTokenType) kw1, XTokenType.None)

    CONSTRUCTOR(kw1 as XTokenType)
        SELF(kw1, XTokenType.None)

    OVERRIDE METHOD ToString() AS STRING
        IF Kw2 == XTokenType.None
            RETURN Kw1:ToString()
        ENDIF
        RETURN Kw1:ToString()+"_"+Kw2:ToString()

    METHOD Equals(x as XKeyword , y as XKeyword ) AS LOGIC
        RETURN x:Equals(y)

    METHOD Equals(other as XKeyword ) AS LOGIC
        RETURN _code == other._code

    METHOD GetHashCode(obj as XKeyword ) AS INT
        RETURN obj:_code

END STRUCTURE

STATIC CLASS XSharpModel.XKeywordExtensions
    STATIC METHOD IsEntity(SELF kw as XKeyword) AS LOGIC
        RETURN XFormattingRule.IsEntity(kw)

    STATIC METHOD IsStart(SELF kw as XKeyword) AS LOGIC
        RETURN XFormattingRule.IsStartKeyword(kw)

    STATIC METHOD IsMiddle(SELF kw as XKeyword) AS LOGIC
        RETURN XFormattingRule.IsMiddleKeyword(kw)

    STATIC METHOD IsStop(SELF kw as XKeyword) AS LOGIC
        RETURN XFormattingRule.IsEndKeyword(kw)

    STATIC METHOD IsMember(SELF kw as XKeyword) AS LOGIC
        RETURN XFormattingRule.IsMember(kw)

    STATIC METHOD IsAccessor(SELF kw as XKeyword) AS LOGIC
        RETURN XFormattingRule.IsAccessor(kw)

    STATIC METHOD IsType(SELF kw as XKeyword) AS LOGIC
        RETURN XFormattingRule.IsType(kw)

    STATIC METHOD IsStatement(SELF kw as XKeyword) AS LOGIC
        RETURN XFormattingRule.IsStatement(kw)

    STATIC METHOD IsGlobal(SELF kw as XKeyword) AS LOGIC
        RETURN XFormattingRule.IsGlobalEntity(kw)

END CLASS
