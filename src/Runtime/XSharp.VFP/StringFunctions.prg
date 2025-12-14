//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// String Functions


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Globalization

INTERNAL STATIC CLASS PathHelpers
    INTERNAL STATIC PROPERTY PathChar AS STRING AUTO
    STATIC CONSTRUCTOR()
        PathChar := Path.DirectorySeparatorChar:ToString()
END CLASS


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/addbs/*" />
/// <seealso cref='DefaultExt' />
/// <seealso cref='JustDrive' />
/// <seealso cref='JustExt' />
/// <seealso cref='JustFName' />
/// <seealso cref='JustPath' />
/// <seealso cref='JustStem' />
FUNCTION AddBs (cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    cPath := cPath:TrimEnd()
    IF ! cPath.EndsWith(PathHelpers.PathChar)
        cPath += PathHelpers.PathChar
    ENDIF
    RETURN cPath


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/forceext/*" />
FUNCTION ForceExt( cFileName AS STRING, cExtension AS STRING) AS STRING
    IF String.IsNullOrEmpty(cFileName)
        RETURN ""
    ENDIF
    VAR result := Path.ChangeExtension(cFileName,cExtension)
    RETURN result

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/forceext/*" />
FUNCTION ForceExt( cFileName AS STRING, cExtension AS STRING, tlOptAsVfp9 AS LOGIC) AS STRING
    *-- current take on matters is that the Dotnet-Version should be Default behaviour
    *-- as vfp9 version behaviour in edge cases could be seen as erroneous
    *-- work in progress and not tested, as existing code should only call 2-parameter overload should be safe
    IF tlOptAsVfp9 == .f.
        RETURN ForceExt( cFileName, cExtension)
    ENDIF
    cFileName := JustFName(cFileName)
    IF cFileName:EndsWith(".")
        *-- if filename ends with dot, cut that
        *-- but only rightmost one, ending in several dots cuts still only 1
        cFileName := cFileName:Substring(0 , cFileName:Length-1)
    ENDIF
    IF cExtension:StartsWith(".")
        cExtension := cExtension:Substring(1)
    ENDIF
    LOCAL cResult AS STRING
    IF Min(cFileName:Length, cExtension:Length)>0
        cResult := cFileName + "." + cExtension
    ELSE
        cResult := ""
    ENDIF
    RETURN cResult

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/forcepath/*" />
FUNCTION ForcePath( cFileName AS STRING, cPath AS STRING) AS STRING
    *-- check if path needs also check...
    IF String.IsNullOrEmpty(cFileName)
        RETURN ""
    ENDIF
    VAR cReturn := AddBs(cPath) + JustFName(cFileName)
    RETURN cReturn



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justdrive/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustDrive(cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := System.IO.Directory.GetDirectoryRoot(cPath)
    result := result:Replace(PathHelpers.PathChar,"")
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justext/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustExt(cPath AS STRING) AS STRING
    *-- Default for new parameter  lOptWithLeadingDot ist .f.
    *-- As returning all extensions with leading dot could lead to breaking changes
    RETURN JustExt(cPath, .f.)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justdrive/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustExt(cPath AS STRING, lOptWithLeadingDot AS LOGIC) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetExtension(cPath)
    IF lOptWithLeadingDot == .f. AND result:StartsWith(".")
        result := result:Substring(1)
    ENDIF
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justfname/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustFName(cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetFileName(cPath)
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justpath/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustPath(cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    LOCAL result := cPath AS STRING
    LOCAL nPos := result:LastIndexOf(PathHelpers.PathChar) AS LONG
    IF  nPos >= 0
        result := result:Substring(0, nPos )
    ENDIF
    RETURN result


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/juststem/*" />
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/justcommon/*" />
FUNCTION JustStem(cPath AS STRING) AS STRING
    IF String.IsNullOrEmpty(cPath)
        RETURN ""
    ENDIF
    VAR result := Path.GetFileNameWithoutExtension(cPath)
    RETURN result

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/leftc/*" />
FUNCTION LeftC( cExpression AS STRING, nExpression AS DWORD) AS STRING
    RETURN Left(cExpression, nExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/lenc/*" />
FUNCTION LenC( cExpression AS STRING ) AS DWORD
    RETURN SLen(cExpression)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/likec/*" />
FUNCTION LikeC( cExpression1, cExpression2) AS LOGIC CLIPPER
    RETURN Like(cExpression1, cExpression2)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rightc/*" />
FUNCTION RightC( cExpression AS STRING, nCharacters AS DWORD) AS STRING
    RETURN Right(cExpression, nCharacters)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/stuffc/*" />
FUNCTION StuffC( cExpression, nStartReplacement, nCharactersReplaced, cReplacement) AS STRING CLIPPER
    RETURN Stuff(cExpression, nStartReplacement, nCharactersReplaced, cReplacement)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/substrc/*" />
FUNCTION SubStrC(cExpression, nStartPosition , nCharactersReturned ) AS STRING CLIPPER
    RETURN SubStr(cExpression, nStartPosition, nCharactersReturned)





/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ratc/*" />
FUNCTION RAtC(cSearchExpression AS STRING , cExpressionSearched AS STRING , dwOccurrence := 1 AS DWORD ) AS DWORD
    RETURN RAt(cSearchExpression , cExpressionSearched , dwOccurrence )


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/rat/*" />
FUNCTION RAt(cSearchExpression AS STRING , cExpressionSearched AS STRING , dwOccurrence := 1 AS DWORD ) AS DWORD
    LOCAL dwPosition, dwOccurred AS DWORD
    LOCAL i , iPosition  AS INT
    dwOccurred := 0
    dwPosition := 0
    IF ! String.IsNullOrEmpty(cExpressionSearched) .AND. ! String.IsNullOrEmpty(cSearchExpression)

        iPosition := cExpressionSearched:Length

        FOR i := 1 UPTO dwOccurrence

            IF ( iPosition := cExpressionSearched:LastIndexOf(cSearchExpression,iPosition,StringComparison.Ordinal) ) == -1
                EXIT
            ENDIF

            dwOccurred++


            IF dwOccurred == dwOccurrence
                // Assign the found position before leaving the loop.
                dwPosition := (DWORD) (iPosition + 1)
                EXIT
            ENDIF

            //  Doing always a
            //
            //  iPosition--
            //
            //	, like the c# sources do, is not correct.
            //  That's only necessary if the len of the search string
            //  is 1.

            IF cSearchExpression:Length == 1 .AND. --iPosition < 0

                // something like:
                //
                // RAt("a","abracadabra", 12 )
                //
                // ends up here. If you do not check the iPosition value
                // the next search loop would cause a exception.
                //

                EXIT
            ENDIF
        NEXT
    ENDIF
    RETURN dwPosition



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/chrtranc/*" />
FUNCTION ChrTranC( cSearchIn AS STRING , cSearchFor AS STRING, cReplaceWith  AS STRING ) AS STRING
    RETURN ChrTran( cSearchIn , cSearchFor , cReplaceWith )

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/chrtran/*" />
FUNCTION ChrTran( cSearchIn AS STRING , cSearchFor AS STRING, cReplaceWith  AS STRING ) AS STRING
    LOCAL cRetVal ,cReplaceChar AS STRING
    LOCAL i AS INT
    cRetVal := ""

    IF  cSearchIn != NULL  .AND. cSearchFor != NULL .AND. cReplaceWith != NULL

        cRetVal := cSearchIn


        FOR i := 1 UPTO cSearchFor:Length

            IF cReplaceWith:Length <= i-1
                cReplaceChar := ""
            ELSE
                cReplaceChar := cReplaceWith[i-1]:ToString()
            ENDIF

            cRetVal := cRetVal:Replace(cSearchFor[i-1]:ToString(), cReplaceChar )
            //	    cRetVal := StrTran(cRetVal , cSearchFor[i-1]:ToString(), cReplaceChar , , SLen ( cRetVal ) )

        NEXT

    ENDIF

    RETURN cRetVal




/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/at/*" />
FUNCTION At(cSearchExpression AS STRING, cExpressionSearched AS STRING, dwOccurrence := 1 AS DWORD) AS DWORD
    LOCAL position := 0 AS DWORD
    IF ( cExpressionSearched != NULL .AND. cSearchExpression != NULL )
        IF cExpressionSearched:Length != 0 .AND. cSearchExpression:Length != 0
            DO WHILE dwOccurrence  > 0
                IF ( position := (DWORD) ( cExpressionSearched:IndexOf(cSearchExpression, (INT) position,StringComparison.Ordinal) + 1 ) ) == 0
                    EXIT
                ENDIF
                dwOccurrence -= 1
            ENDDO
        END IF
    ENDIF
    RETURN position

/// <inheritdoc cref="At" />
/// <remarks>This is an alias for the At() function. X# works with unicode and the difference
/// between single byte and multi byte characters does not exist in Unicode</remarks>
FUNCTION At_C(cSearchExpression AS STRING, cExpressionSearched AS STRING, dwOccurrence := 1 AS DWORD) AS DWORD
    RETURN At(cSearchExpression, cExpressionSearched, dwOccurrence)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/atc/*" />
FUNCTION AtC(cSearchExpression AS STRING, cExpressionSearched AS STRING, dwOccurrence := 1 AS DWORD) AS DWORD
    LOCAL position := 0 AS DWORD
    IF ( cExpressionSearched != NULL .AND. cSearchExpression != NULL )
        IF cExpressionSearched:Length != 0 .AND. cSearchExpression:Length != 0
            DO WHILE dwOccurrence  > 0
                IF ( position := (DWORD) ( cExpressionSearched:IndexOf(cSearchExpression, (INT) position,StringComparison.OrdinalIgnoreCase) + 1) ) == 0
                    EXIT
                ENDIF
                dwOccurrence -= 1
            ENDDO
        END IF
    ENDIF
    RETURN position

/// <inheritdoc cref="AtC" />
/// <remarks>This is an alias for the AtC() function. X# works with unicode and the difference
/// between single byte and multi byte characters does not exist in Unicode</remarks>

FUNCTION AtCC(cSearchExpression AS STRING, cExpressionSearched AS STRING, dwOccurrence := 1 AS DWORD) AS DWORD
    RETURN AtC(cSearchExpression, cExpressionSearched, dwOccurrence)

/// <inheritdoc cref="M:XSharp.Core.Functions.AllTrim(System.String)" />
/// <param name="Expression">Specifies an expression to remove leading and trailing spaces or 0 bytes from</param>
/// <param name="Flags">Specifies if trimming is case-sensitive when one or more parse characters
/// (cParseChar, cParseChar2, … are included. Trimming is case-sensitive if nFlags is zero or is omitted.
/// Trimming is case-insensitive if nFlags = 1.</param>
/// <param name="TrimChars">Specifies one or more character strings that are trimmed from the beginning of cExpression.
/// If cParseChar isn't included, then leading spaces or 0 bytes are removed from Expression. </param>
FUNCTION AllTrim(Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT
    RETURN Trim_helper(.T., .T., Expression, Flags, TrimChars)

/// <inheritdoc cref="M:XSharp.Core.Functions.LTrim(System.String)" />
/// <param name="Expression">Specifies an expression to remove leading spaces or 0 bytes from, respectively</param>
/// <param name="TrimChars">Specifies one or more character strings that are trimmed from the beginning of cExpression.
/// If cParseChar isn't included, then leading spaces or 0 bytes are removed from Expression. </param>
FUNCTION LTrim(Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT
    RETURN Trim_helper(.T., .F., Expression, Flags, TrimChars)

/// <inheritdoc cref="M:XSharp.Core.Functions.RTrim(System.String)" />
/// <param name="Expression">Specifies an expression to remove leading spaces or 0 bytes from, respectively</param>
/// <param name="TrimChars">Specifies one or more character strings that are trimmed from the beginning of cExpression.
/// If cParseChar isn't included, then leading spaces or 0 bytes are removed from Expression. </param>
FUNCTION RTrim(Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT
    RETURN Trim_helper(.F., .T., Expression, Flags, TrimChars)

/// <inheritdoc cref="M:XSharp.Core.Functions.Trim(System.String)" />
/// <param name="Expression">Specifies an expression to remove trailing spaces or 0 bytes from</param>
/// <param name="Flags">Specifies if trimming is case-sensitive when one or more parse characters
/// (cParseChar, cParseChar2, … are included. Trimming is case-sensitive if nFlags is zero or is omitted.
/// Trimming is case-insensitive if nFlags = 1.</param>
/// <param name="TrimChars">Specifies one or more character strings that are trimmed from the beginning of cExpression.
/// If cParseChar isn't included, then leading spaces or 0 bytes are removed from Expression. </param>
FUNCTION Trim(Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT
    RETURN Trim_helper(.F., .T., Expression, Flags, TrimChars)

STATIC FUNCTION Trim_helper(TrimLeft AS Boolean, TrimRight AS Boolean, Expression AS STRING, Flags AS INT, TrimChars PARAMS STRING[]) AS STRING STRICT

    LOCAL parmNdx AS INT
    LOCAL Trimmed = .T. AS Boolean
    LOCAL LRTrimmed AS INT
    LOCAL comparison = StringComparison.Ordinal AS System.StringComparison
    LOCAL compared AS STRING

    IF Expression = NULL
        RETURN Expression
    END IF

    IF Flags = 1
        comparison = StringComparison.OrdinalIgnoreCase
    END IF

    DO WHILE Trimmed

        Trimmed = .F.

        FOR parmNdx = 1 TO TrimChars:Length

            compared = TrimChars[parmNdx]

            IF TrimLeft
                LRTrimmed = 0

                DO WHILE String.Compare(Expression, LRTrimmed, compared, 0, compared:Length, comparison) = 0
                    LRTrimmed += compared:Length
                END DO
                IF LRTrimmed > 0
                    Expression = Expression:Substring(LRTrimmed)
                    Trimmed = .T.
                END IF
            END IF

            IF TrimRight
                LRTrimmed = Expression:Length - compared:Length

                DO WHILE LRTrimmed >= 0 AND String.Compare(Expression, LRTrimmed, compared, 0, compared:Length, comparison) = 0
                    LRTrimmed -= compared:Length
                END DO
                IF LRTrimmed < (Expression:Length - compared:Length)
                    Expression = Expression:Substring(0, LRTrimmed + compared:Length)
                    Trimmed = .T.
                END IF
            END IF

        NEXT

    END DO

    RETURN Expression

END FUNCTION


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/transform/*" />
FUNCTION Transform( uValue AS USUAL ) AS STRING
    RETURN AsString(uValue)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/str/*" />
FUNCTION Str(nNumber ,nLength ,nDecimals ) AS STRING CLIPPER
    IF PCount() < 1 .or. pCount() > 3
        RETURN ""
    ENDIF
    Default(REF nLength, 10)
    Default(REF nDecimals, 0)
    RETURN XSharp.RT.Functions.Str(nNumber, nLength, nDecimals)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/strextract/*" />
FUNCTION StrExtract(cSearchExpression as string, cBeginDelim as string, cEndDelim := "" as string, nOccurrence := 1 as int, nFlag := 0 as int) AS STRING
    if String.IsNullOrEmpty(cSearchExpression) || String.IsNullOrEmpty(cBeginDelim)
        return ""
    endif

    // Flag breakdown:
    // Bit 0 (1): Case insensitive search
    // Bit 1 (2): End delimiter not required (if not found, return rest of string)
    // Bit 2 (4): Include delimiters in result

    local lCaseInsensitive as logic
    local lNoEndDelim as logic
    local lIncludeDelims as logic

    lCaseInsensitive := (logic)((nFlag & 1) != 0)
    lNoEndDelim := (logic)((nFlag & 2) != 0)
    lIncludeDelims := (logic)((nFlag & 4) != 0)

    local compareType as StringComparison
    if lCaseInsensitive
        compareType := StringComparison.OrdinalIgnoreCase
    else
        compareType := StringComparison.Ordinal
    endif

    local nStart := 0 as int
    local i as int

    for i := 1 to nOccurrence
        var nAt := cSearchExpression:IndexOf(cBeginDelim, nStart, compareType)

        if nAt == -1
            return ""
        endif

        if lIncludeDelims and (i == nOccurrence)
            nStart := nAt
        else
            nStart := nAt + cBeginDelim:Length
        endif
    next

    local nSearchEndFrom as int
    if lIncludeDelims
        nSearchEndFrom := nStart + cBeginDelim:Length
    else
        nSearchEndFrom := nStart
    endif

    if String.IsNullOrEmpty(cEndDelim)
        return cSearchExpression:Substring(nStart)
    else
        var nAtEnd := cSearchExpression:IndexOf(cEndDelim, nSearchEndFrom, compareType)

        if nAtEnd == -1
            if lNoEndDelim
                return cSearchExpression:Substring(nStart)
            else
                return ""
            endif
        else
            local nLen as int
            nLen := nAtEnd - nStart

            if lIncludeDelims
                nLen += cEndDelim:Length
            endif

            return cSearchExpression:Substring(nStart, nLen)
        endif
    endif

END FUNCTION

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/difference/*" />
FUNCTION Difference( cExpression1 as string, cExpression2 as string) as int
    if String.IsNullOrEmpty(cExpression1) || String.IsNullOrEmpty(cExpression2)
        return 0
    endif

    local cS1 := SoundEx(cExpression1) as string
    local cS2 := SoundEx(cExpression2) as string

    local nDiff := 0 as int

    // We use a 0-3 loop because strings are 0-based in .NET
    for var i := 0 to 3
        if cS1[i] == cS2[i]
            nDiff += 1
        endif
    next

    return nDiff
END FUNCTION

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/strconv/*" />
FUNCTION StrConv(cExpression AS USUAL, nConversionSetting AS INT, nRegionalIdentifier := 0 AS INT, nRegionalIDType := 0 AS INT) AS USUAL

    local nCP := XSharp.RuntimeState.WinCodePage as int
    if nCP == 0; nCP := 1252; endif

    // working variables
    local encDefault := Encoding.GetEncoding(nCP) as Encoding
    local encUTF8 := Encoding.UTF8 as Encoding
    local encUnicode := Encoding.Unicode as Encoding
    local encTarget as Encoding

    // validate & normalize parameters
    local bytesInput as binary
    local strInput as string
    local isBinaryInput as logic

    // Unpack usual input
    if IsString(cExpression)
        strInput := (STRING)cExpression
        isBinaryInput := false
        bytesInput := encDefault:GetBytes(strInput)
    elseif IsBinary(cExpression)
        bytesInput := (BINARY)cExpression
        isBinaryInput := true
        strInput := encDefault:GetString(bytesInput)
    else
        throw Error.ArgumentError(__FUNCTION__, nameof(cExpression), "Expects String or Binary", 1)
    endif

    local uResult as usual

    try
        switch nConversionSetting

        // 1. Single-byte to Double-byte (DBCS)
        case STRCNV_SB_DBCS
            if nRegionalIDType == STRCNV_ID_LCID
                encTarget := StrConvHelpers.GetEncoding(nRegionalIdentifier, STRCNV_ID_LCID)
                uResult := encDefault:GetString(Encoding.Convert(encDefault, encTarget, bytesInput))
            else
                throw Error.ArgumentError(__FUNCTION__, nameof(nRegionalIDType), "Must be LCID for this conversion", 4)
            endif

        // 2. Double-byte to Single-byte
        case STRCNV_DBCS_SB
            if nRegionalIDType == STRCNV_ID_LCID
                encTarget := StrConvHelpers.GetEncoding(nRegionalIdentifier, STRCNV_ID_LCID)
                uResult := (BINARY) Encoding.Convert(encDefault, encTarget, bytesInput)
            else
                throw Error.ArgumentError(__FUNCTION__, nameof(nRegionalIDType), "Must be LCID for this conversion", 4)
            endif

        // 5. DBCS to Unicode
        case STRCNV_DBCS_UNI
            encTarget := StrConvHelpers.GetEncoding(nRegionalIdentifier, nRegionalIDType)
            uResult := encUnicode:GetString(Encoding.Convert(encTarget, encUnicode, bytesInput))

        // 6. Unicode to DBCS
        case STRCNV_UNI_DBCS
            encTarget := StrConvHelpers.GetEncoding(nRegionalIdentifier, nRegionalIDType)
            local unicodeBytes := encUnicode:GetBytes(strInput) as byte[]
            uResult := encDefault:GetString(Encoding.Convert(encUnicode, encTarget, unicodeBytes))

        // 7. Lowercase
        case STRCNV_LOWER
            var culture := StrConvHelpers.GetCulture(nRegionalIdentifier)
            if isBinaryInput
                uResult := (BINARY) encDefault:GetBytes(encDefault:GetString(bytesInput):ToLower(culture))
            else
                uResult := strInput:ToLower(culture)
            endif

        // 8. Uppercase
        case STRCNV_UPPER
            var culture := StrConvHelpers.GetCulture(nRegionalIdentifier)
            if isBinaryInput
                uResult := (BINARY) encDefault:GetBytes(encDefault:GetString(bytesInput):ToUpper(culture))
            else
                uResult := strInput:ToUpper(culture)
            endif

        // 9. DBCS to UTF8
        case STRCNV_DBCS_UTF8
            encTarget := StrConvHelpers.GetEncoding(nRegionalIdentifier, nRegionalIDType)
            uResult := (BINARY) Encoding.Convert(encTarget, encUTF8, bytesInput)

        // 10. Unicode to UTF8
        case STRCNV_UNI_UTF8
            uResult := (BINARY) Encoding.Convert(encUnicode, encUTF8, encUnicode:GetBytes(strInput))

        // 11. UTF8 to DBCS
        case STRCNV_UTF8_DBCS
            encTarget := StrConvHelpers.GetEncoding(nRegionalIdentifier, nRegionalIDType)
            uResult := encDefault:GetString(Encoding.Convert(encUTF8, encTarget, bytesInput))

        // 12. UTF8 to Unicode
        case STRCNV_UTF8_UNI
            uResult := encUnicode:GetString(Encoding.Convert(encUTF8, encUnicode, bytesInput))

        // 13. Base64 Encode
        case STRCNV_SB_BASE64
            uResult := Convert.ToBase64String(bytesInput)

        // 14. Base64 Decode
        case STRCNV_BASE64_SB
            uResult := (BINARY) Convert.FromBase64String(strInput)

        // 15. Hex Encode
        case STRCNV_SB_HEX
            uResult := BitConverter.ToString(bytesInput):Replace("-","")

        // 16. Hex Decode
        case STRCNV_HEX_SB
            local nLen := strInput:Length as int
            local hexBytes := BYTE[]{nLen / 2} as byte[]
            for var i := 0 to nLen - 1 step 2
                var bVal := Convert.ToByte(strInput:Substring(i, 2), 16)
                hexBytes:SetValue(bVal, i / 2)
            next
            uResult := (BINARY) hexBytes

        // X# Extensions
        case STRCNV_UNI_SB
            uResult := (BINARY) encUnicode:GetBytes(strInput)

        case STRCNV_UNIBE_SB
            uResult := (BINARY) Encoding.BigEndianUnicode:GetBytes(strInput)
        otherwise
            throw NotImplementedException{"Conversion setting " + nConversionSetting:ToString() + " not implemented."}
        end switch
    catch ex as Exception
        throw Error.ArgumentError(__FUNCTION__, nameof(cExpression), ex:Message, 1)
    end try

    return uResult
END FUNCTION

// Internal class for not contamining the global namespace
INTERNAL STATIC CLASS StrConvHelpers
    STATIC METHOD GetCulture(nLCID AS INT) AS CultureInfo
        if nLCID != 0
            RETURN CultureInfo.GetCultureInfo(nLCID)
        endif
        return CultureInfo.CurrentCulture
    END METHOD

    static method GetEncoding(nRegionalIdentifier as int, nRegionalIDType as int) as Encoding
        switch nRegionalIDType
        case STRCNV_ID_LCID
            var culture := GetCulture(nRegionalIdentifier)
            return Encoding.GetEncoding(culture:TextInfo:ANSICodePage)
        case STRCNV_ID_CODEPAGE
            return Encoding.GetEncoding(nRegionalIdentifier)
        case STRCNV_ID_CHARSET
            // Maps from charset to codepage (Legacy VFP logic)
            local nCP := 1252 as int
            switch nRegionalIdentifier
            case 128; nCP := 932
            case 129; nCP := 949
            case 130; nCP := 1361
            case 134; nCP := 936
            case 136; nCP := 950
            case 161; nCP := 1253
            case 162; nCP := 1254
            case 163; nCP := 1258
            case 177; nCP := 1255
            case 178; nCP := 1256
            case 186; nCP := 1257
            case 204; nCP := 1251
            case 222; nCP := 874
            case 238; nCP := 1250
            end switch

            return Encoding.GetEncoding(nCP)
        end switch

        // Default: WinCodePage
        return Encoding.GetEncoding(XSharp.RuntimeState.WinCodePage)
    end method

END CLASS
