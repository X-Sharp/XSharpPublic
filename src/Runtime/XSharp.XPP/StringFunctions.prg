//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text



/// <include file="XSharp.XPP.Docs.xml" path="doc/PosUpper/*" />
FUNCTION PosUpper(cString, lNoLetter, nIgnoreCharsFromLeft) AS LONG CLIPPER
    EnforceType(cString, STRING)
    @@Default( REF lNoLetter, FALSE)
    @@Default( REF nIgnoreCharsFromLeft, 0)
    LOCAL sString := cString AS STRING
    LOCAL noLetter := lNoLetter AS LOGIC
    LOCAL ignoreChars := nIgnoreCharsFromLeft AS LONG
    RETURN PosWorker(sString, ignoreChars, {  c =>
            IF Char.IsLetter(c)
                IF Char.IsUpper(c)
                    RETURN TRUE
                ENDIF
            ELSEIF noLetter
                RETURN TRUE
            ENDIF
            RETURN FALSE
            } )




/// <include file="XSharp.XPP.Docs.xml" path="doc/PosLower/*" />
FUNCTION PosLower(cString, lNoLetter, nIgnoreCharsFromLeft) AS LONG CLIPPER
    EnforceType(cString, STRING)
    @@Default( REF lNoLetter, FALSE)
    @@Default( REF nIgnoreCharsFromLeft, 0)
    LOCAL sString := cString AS STRING
    LOCAL noLetter := lNoLetter AS LOGIC
    LOCAL ignoreChars := nIgnoreCharsFromLeft AS LONG
    RETURN PosWorker(sString, ignoreChars, {  c =>
        IF Char.IsLetter(c)
            IF Char.IsLower(c)
                RETURN TRUE
            ENDIF
        ELSEIF noLetter
            RETURN TRUE
        ENDIF
        RETURN FALSE
        } )







/// <include file="XSharp.XPP.Docs.xml" path="doc/PosAlpha/*" />
FUNCTION PosAlpha(cString, lNoLetter, nIgnoreCharsFromLeft) AS LONG CLIPPER
    EnforceType(cString, STRING)
    @@Default( REF lNoLetter, FALSE)
    @@Default( REF nIgnoreCharsFromLeft, 0)
    LOCAL sString := cString AS STRING
    LOCAL noLetter := lNoLetter AS LOGIC
    LOCAL ignoreChars := nIgnoreCharsFromLeft AS LONG
    RETURN PosWorker(sString, ignoreChars, {  c =>
                                IF Char.IsLetter(c)
                                    RETURN TRUE
                                ELSEIF noLetter
                                    RETURN TRUE
                                ENDIF
                                RETURN FALSE
                                } )

INTERNAL DELEGATE PosDelegate( cChar AS CHAR) AS LOGIC

    INTERNAL FUNCTION PosWorker(sString AS STRING, ignoreChars AS LONG, delCheck AS PosDelegate) AS LONG
    FOR VAR nI := ignoreChars TO sString:Length-1
        VAR cChar := sString[nI]
        IF delCheck(cChar)
            RETURN nI+1
        ENDIF
        NEXT
    RETURN 0



/// <include file="XSharp.XPP.Docs.xml" path="doc/PosChar/*" />
FUNCTION PosChar(cString, uChar, nPosition ) AS STRING CLIPPER
    EnforceType(cString, STRING)
    LOCAL sReplace AS STRING
    LOCAL sSource  AS STRING

    IF IsString(uChar)
        sReplace := uChar
    ELSEIF IsNumeric(uChar)
        sReplace := Chr(uChar)
    ELSE
        VAR cMessage := "Expected type: Numeric or String actual type "+ ((__UsualType) UsualType(uChar)):ToString()
        THROW Error.DataTypeError(ProcName(), nameof(uChar), 2, uChar, cMessage)
    ENDIF
    sSource := cString
    @@Default(REF nPosition, SLen(sSource))
    LOCAL sb AS StringBuilder
    sb := StringBuilder{sSource}
    IF nPosition <= sb:Length
        sb[nPosition-1] := sReplace[0]
    ENDIF
    RETURN sb:ToString()





/// <include file="XSharp.XPP.Docs.xml" path="doc/PosDel/*" />
FUNCTION PosDel(cString, nStartPos, nDeleteLen ) AS STRING CLIPPER
    EnforceType(cString, STRING)
    EnforceType(nDeleteLen,LONG)
    LOCAL sSource  AS STRING
    LOCAL nDelete as LONG
    sSource := cString
    nDelete    := nDeleteLen
    @@Default(REF nStartPos, SLen(sSource))
    LOCAL sb AS StringBuilder
    sb := StringBuilder{sSource}
    nDelete := Math.Min(sSource:Length - (int) nStartPos+1, nDelete)
    sb:Remove((int)nStartPos-1, nDelete)
    RETURN sb:ToString()





/// <include file="XSharp.XPP.Docs.xml" path="doc/PosIns/*" />
FUNCTION PosIns(cString, cInsertString, nPosition ) AS STRING CLIPPER
    EnforceType(cString, STRING)
    EnforceType(cInsertString,STRING)
    LOCAL sSource  AS STRING
    LOCAL nPos as LONG
    sSource := cString
    @@Default(REF nPosition, SLen(sSource))   // not + 1, StringBuilder is zero based
    nPos := nPosition
    LOCAL sb AS StringBuilder
    sb := StringBuilder{sSource}
    sb:Insert(nPos, cInsertString, 1)
    RETURN sb:ToString()




/// <include file="XSharp.XPP.Docs.xml" path="doc/PosRepl/*" />
FUNCTION PosRepl(cString, cReplace, nStartPos ) AS STRING CLIPPER
    EnforceType(cString, STRING)
    EnforceType(cReplace,STRING)
    LOCAL sSource  AS STRING
    LOCAL sReplace AS STRING
    LOCAL nStart as LONG
    sSource     := cString
    sReplace    := cReplace
    @@Default(REF nStartPos, sSource:Length - sReplace:Length+1)
    nStart := nStartPos
    nStart := Math.Max((int) nStart, 1L)
    LOCAL sb AS StringBuilder
    sb := StringBuilder{sSource}
    nStart -= 1 // make zero based
    VAR nToDel  := sReplace:Length
    var nRemain := sSource:Length- nStart
    IF nRemain < nToDel
        nToDel := nRemain
    endif
    sb:Remove(nStart, nToDel)
    sb:Insert(nStart, sReplace, 1)
    RETURN sb:ToString()



function Var2Char(uValue as usual) as string
    switch UsualType(uValue)
    case __UsualType.Array
        local aValue as array
        aValue := uValue
        var sb := StringBuilder{}
        sb:Append("{")
        var first := true
        foreach var element in aValue
            if (! first)
                sb:Append(", ")
            else
                first := false
            endif
            sb:Append(Var2Char(element))
        next
        sb:Append("}")
        return sb:ToString()

    case __UsualType.Binary
        return ((binary) uValue):ToString()
    case __UsualType.Codeblock
        return ((codeblock) uValue):ToString()
    case __UsualType.Currency
        return ((currency) uValue):ToString()
    case __UsualType.Date
        return DToS( (date) uValue)
    case __UsualType.DateTime
        return ((System.DateTime) uValue):ToString()
    case __UsualType.Decimal
        return ((decimal) uValue):ToString()
    case __UsualType.Float
        return _Str((float) uValue):TrimStart()
    case __UsualType.Int64
        return ((int64) uValue):ToString()
    case __UsualType.Logic
        return iif(uValue, ".T.",".F.")
    case __UsualType.Long
        return ((long) uValue):ToString()
    case __UsualType.Object
        local oValue as object
        oValue := uValue
        return oValue:GetType():Name
    case __UsualType.String
        return (string) uValue
    case __UsualType.Symbol
        return "#"+ (string) uValue
    case __UsualType.Void
        return "NIL"
    otherwise
        return uValue:ToString()
    end switch


