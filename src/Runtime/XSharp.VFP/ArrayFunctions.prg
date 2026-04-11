//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.Internal
USING System.Text.RegularExpressions
USING System.Collections.Generic
USING System.Text
USING System.Reflection
USING System.Linq
USING System.Diagnostics
USING System.IO
USING XSharp.RDD
USING XSharp.Core


INTERNAL FUNCTION FoxALen(a as ARRAY) AS DWORD
    RETURN XSharp.VFP.Functions.ALen( (__FoxArray) a, 0)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/alen/*" />
[FoxProFunction("ALEN", FoxFunctionCategory.Array, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION ALen(a AS __FoxArray) AS DWORD
    RETURN XSharp.VFP.Functions.ALen(a, 0)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/alen/*" />
FUNCTION ALen(a AS __FoxArray, nArrayAttribute AS LONG) AS DWORD
    SWITCH nArrayAttribute
    CASE 1
        RETURN (DWORD) a:Rows
    CASE 2
        IF a:MultiDimensional
            RETURN (DWORD) a:Columns
        ELSE
            RETURN 0
        ENDIF
    CASE 0
        RETURN (DWORD) a:Count
    OTHERWISE
        var cMessage := __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE, nameof(nArrayAttribute))
        THROW ArgumentOutOfRangeException { nameof(nArrayAttribute),nArrayAttribute, cMessage}
    END SWITCH


/// <exclude/>
FUNCTION __FoxALen(a AS __FoxArray) AS DWORD
    RETURN XSharp.VFP.Functions.ALen(a, 0)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/aelement/*" />
[FoxProFunction("AELEMENT", FoxFunctionCategory.Array, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION AElement(ArrayName AS __FoxArray, nRowSubscript AS DWORD) AS USUAL
    IF ( nRowSubscript > 0 .AND. nRowSubscript <= ArrayName:Rows )
        RETURN nRowSubscript
    ENDIF
    var cMessage := __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE, nameof(nRowSubscript))
    THROW ArgumentOutOfRangeException { nameof(nRowSubscript),nRowSubscript, cMessage}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/aelement/*" />
FUNCTION AElement(ArrayName AS __FoxArray, nRowSubscript AS DWORD, nColumnSubscript AS DWORD) AS USUAL
    IF ArrayName:MultiDimensional
        IF nRowSubscript == 0 .OR. nRowSubscript >  ArrayName:Rows
            var cMessage := __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE,nameof(nRowSubscript))
            THROW ArgumentOutOfRangeException { nameof(nRowSubscript), nRowSubscript, cMessage}
        ELSEIF nColumnSubscript == 0 .OR. nColumnSubscript > ArrayName:Columns
            var cMessage := __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE,nameof(nColumnSubscript))
            THROW ArgumentOutOfRangeException { nameof(nColumnSubscript), nColumnSubscript, cMessage }
        ENDIF
        nRowSubscript --
        RETURN ( nRowSubscript * ArrayName:Columns ) + nColumnSubscript
    ENDIF
    THROW ArgumentException { __VfpStr(VFPErrors.VFP_ONE_DIM_NO_COLUMNS)}

/// <exclude/>
FUNCTION __FoxADel(foxArray AS __FoxArray, nElementNumber AS LONG, nDeleteType := 2 AS LONG) AS DWORD
    IF ! foxArray:MultiDimensional
        foxArray:Delete((LONG) nElementNumber)
    ELSE
        IF nDeleteType == 2
            foxArray:DeleteColumn( (LONG) nElementNumber)
        ELSE
            foxArray:DeleteRow((LONG) nElementNumber)
        ENDIF
    ENDIF
    RETURN 1

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/adel/*" />
[FoxProFunction("ADEL", FoxFunctionCategory.Array, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION ADel(foxArray AS __FoxArray, nElementNumber AS LONG, nDeleteType := 2 AS LONG) AS DWORD
    RETURN __FoxADel(foxArray, nElementNumber, nDeleteType)


/// <include file="XSharp.VFP.Docs.xml" path="doc/ADel/*" />
FUNCTION ADel(ArrayName AS ARRAY, nElementNumber AS LONG, nDeleteType AS LONG) AS DWORD
    IF ArrayName IS __FoxArray VAR foxArray
        RETURN __FoxADel(foxArray, nElementNumber, nDeleteType)
    ENDIF
    XSharp.RT.Functions.ADel(ArrayName, (DWORD) nElementNumber )
    RETURN 1


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asubscript/*" />
[FoxProFunction("ASUBSCRIPT", FoxFunctionCategory.Array, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION ASubScript(ArrayName AS __FoxArray, nElementNumber AS DWORD, nSubscript := 1 AS DWORD) AS DWORD
    IF nSubscript == 0 .OR. nSubscript > 2
        var cMessage := __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE, nameof(nSubscript))
        THROW ArgumentOutOfRangeException { nameof(nSubscript), nSubscript, cMessage }
    ELSEIF nElementNumber == 0 .OR. nElementNumber >  ArrayName:Count
        var cMessage := __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE, nameof(nElementNumber))
        THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, cMessage }
    ENDIF

    IF ArrayName:MultiDimensional

        IF nSubscript == 1

            // calculate the row

            // doesn't compile because GetRow() is a internal method
            RETURN (DWORD) ArrayName:GetRow((LONG) nElementNumber)

        ELSE
            // calculate the column
            RETURN (DWORD) ArrayName:GetColumn((LONG) nElementNumber)


        ENDIF

    ELSE

        IF nSubscript == 2
            THROW ArgumentException { __VfpStr(VFPErrors.VFP_ONE_DIM_NO_COLUMNS)}
        ENDIF

        RETURN nElementNumber

    ENDIF

/// <exclude/>
FUNCTION __FoxAIns(foxArray AS __FoxArray, nElementNumber AS DWORD, nInsertType := 1 AS DWORD) AS DWORD
    IF !foxArray:MultiDimensional
        IF nInsertType > 1
            THROW ArgumentException { __VfpStr(VFPErrors.VFP_ONE_DIM_NO_COLUMNS)}
        ELSEIF nElementNumber == 0 .OR. nElementNumber > (DWORD) foxArray:Count
            THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE, nameof(nElementNumber)) }
        ENDIF

        foxArray:Insert((LONG) nElementNumber)
    ELSE
        IF nInsertType > 2
            THROW ArgumentOutOfRangeException { nameof(nInsertType), nInsertType, __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE, nameof(nInsertType)) }
        ENDIF
        IF nInsertType == 2
            IF nElementNumber == 0 .OR. nElementNumber > (DWORD) foxArray:Columns
                THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE, nameof(nElementNumber)) }
            ENDIF

            foxArray:InsertColumn( (LONG) nElementNumber)
        ELSE
            IF nElementNumber == 0 .OR. nElementNumber > (DWORD) foxArray:Rows
                THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, __VfpStr(VFPErrors.VFP_ATTRIBUTE_OUT_OF_RANGE, nameof(nElementNumber))}
            ENDIF

            foxArray:InsertRow((LONG) nElementNumber)
        ENDIF
    ENDIF
    RETURN 1

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ains/*" />
[FoxProFunction("AINS", FoxFunctionCategory.Array, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION AIns(ArrayName AS __FoxArray, nElementNumber AS DWORD, nInsertType := 1 AS DWORD) AS DWORD
    RETURN __FoxAIns(ArrayName, nElementNumber, nInsertType)

INTERNAL FUNCTION FoxAIns(ArrayName AS ARRAY, nElementNumber AS DWORD, nInsertType AS DWORD) AS DWORD
    IF ArrayName IS __FoxArray VAR foxArray
        RETURN __FoxAIns(foxArray, nElementNumber, nInsertType)
    ENDIF
    XSharp.RT.Functions.AIns(ArrayName, nElementNumber)
    RETURN 1

/// <include file="XSharp.VFP.Docs.xml" path="doc/AIns/*" />
FUNCTION AIns(ArrayName AS ARRAY, nElementNumber AS DWORD, nInsertType AS DWORD) AS DWORD
    IF ArrayName IS __FoxArray VAR foxArray
        RETURN __FoxAIns(foxArray, nElementNumber, nInsertType)
    ENDIF
    XSharp.RT.Functions.AIns(ArrayName, nElementNumber)
    RETURN 1

/// <exclude/>
FUNCTION ASize(ArrayName AS ARRAY, nSize AS DWORD) AS ARRAY
    IF ArrayName IS __FoxArray VAR foxArray
        foxArray:Resize((LONG) nSize)
        RETURN foxArray
    ENDIF
    RETURN XSharp.RT.Functions.ASize(ArrayName, nSize)


/// <inheritdoc cref="ShowArray" />
FUNCTION ShowFoxArray ( aPar AS ARRAY , cPrefix := "" AS STRING ) AS VOID
    LOCAL i, j AS DWORD
    LOCAL aTest := (__FoxArray) aPar as __FoxArray
    LOCAL cLDelim as STRING
    LOCAL cRDelim as STRING
    IF cPrefix:Length == 0
        cPrefix := "a"
    ENDIF
    IF XSharp.RuntimeState.CompilerOptionFox2
        cLDelim := "("
        cRDelim := ")"
    ELSE
        cLDelim := "["
        cRDelim := "]"
    ENDIF

    IF aTest:MultiDimensional
        FOR i := 1 TO ALen ( aTest , 1 )
            FOR j := 1 TO ALen ( aTest , 2 )
                //var line := i"{cPrefix}{cLDelim}{AElement ( aTest , i , j )}{cRDelim} {cLDelim}{i},{j}{cRDelim} {aTest[i,j]} {GetElementValueType ( aTest[i,j] )}"
                var line := i"{cPrefix}{cLDelim}{i},{j}{cRDelim} = {aTest[i,j]} {GetElementValueType ( aTest[i,j] )}"
                QOut(line)
            NEXT
        NEXT
    ELSE
        FOR i := 1 TO ALen ( aTest , 0 )
            var line := i"{cPrefix}{cLDelim}{i}{cRDelim} = {aTest[i]}  {GetElementValueType ( aTest[i] )}"
            QOut(line)
        NEXT
    ENDIF

LOCAL FUNCTION GetElementValueType( uValue AS USUAL ) AS STRING
    IF IsNil ( uValue )
        RETURN "(Nil)"
    ELSE
        RETURN "(" + ValType ( uValue ) + ")"
    ENDIF

END FUNCTION

RETURN

END FUNCTION

/// <include file="VfpDocs.xml" path="Runtimefunctions/alines/*" />
[FoxArrayInputParameter(1)];
[FoxProFunction("ALINES", FoxFunctionCategory.Array, FoxEngine.RuntimeCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION ALines ( ArrayName AS USUAL, cExpression AS STRING, nFlags := 0 AS INT, cParseChars PARAMS STRING[]) AS DWORD
    IF cExpression == null
        cExpression := ""
    ENDIF
    LOCAL aFoxArray AS __FoxArray
    IF ArrayName IS __FoxArray var aFox
        aFoxArray := aFox
    ELSE
        var cMessage := __VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, nameof(ArrayName))
        THROW ArgumentException{cMessage}
    ENDIF

    VAR separators := List<STRING>{}

    IF cParseChars == NULL OR cParseChars:Length == 0
        separators:Add(e"\r\n")
        separators:Add(e"\r")
        separators:Add(e"\n")
    ELSE
        separators:AddRange(cParseChars)
    ENDIF

    LOCAL lTrim := (nFlags & ALINES_TRIM) != 0 AS LOGIC // 1
    LOCAL lIncludeLast := (nFlags & ALINES_INCLUDE_LAST) != 0 AS LOGIC // 2
    LOCAL lNoEmpty := (nFlags & ALINES_NO_EMPTY) != 0 AS LOGIC // 4
    LOCAL lIgnoreCase := (nFlags & ALINES_CASE_IGNORE) != 0 AS LOGIC // 8
    LOCAL lIncludeSep := (nFlags & ALINES_INCLUDE_SEP) != 0 AS LOGIC // 16

    var sbPattern := StringBuilder{}
    FOREACH VAR sep IN separators
        IF sbPattern:Length > 0
            sbPattern:Append("|")
        ENDIF

        VAR safeSep := Regex.Escape(sep)
        IF lIncludeSep
            sbPattern:Append("(" + safeSep + ")")
        ELSE
            sbPattern:Append("(?:" + safeSep + ")")
        ENDIF
    NEXT

    VAR @@regexOptions := RegexOptions.None
    IF lIgnoreCase
        @@regexOptions := RegexOptions.IgnoreCase
    ENDIF

    VAR aRawParts := Regex.Split(cExpression, sbPattern:ToString(), @@regexOptions)

    VAR finalLines := List<STRING>{}
    VAR nIndex := 0

    FOREACH VAR sLine IN aRawParts
        VAR sTemp := sLine

        VAR lIsSeparator := FALSE
        IF lIncludeSep
            lIsSeparator := (nIndex % 2) == 1
        ENDIF

        IF lTrim AND !lIsSeparator
            sTemp := sTemp:Trim()
        ENDIF

        IF lNoEmpty AND String.IsNullOrEmpty(sTemp) AND !lIsSeparator
            nIndex++
            LOOP
        ENDIF

        finalLines:Add(sTemp)
        nIndex++
    NEXT

    IF !lIncludeLast AND !lNoEmpty AND finalLines:Count > 0
        VAR nLastIdx := finalLines:Count - 1
        IF String.IsNullOrEmpty(finalLines[nLastIdx])
            finalLines:RemoveAt(nLastIdx)
        ENDIF
    ENDIF

    VAR nRows := (DWORD)finalLines:Count

    IF nRows == 0 AND !lNoEmpty
        nRows := 1
        finalLines:Add("")
    ENDIF

    ASize(aFoxArray, nRows)

    FOR VAR i := 0 TO (INT)nRows - 1
        aFoxArray[i + 1] := finalLines[i]
    NEXT
    RETURN nRows
END FUNCTION

/// <include file="VfpDocs.xml" path="Runtimefunctions/amembers/*" />
[FoxArrayInputParameter(1)];
[FoxProFunction("AMEMBERS", FoxFunctionCategory.Array, FoxEngine.LanguageCore, FoxFunctionStatus.Partial, FoxCriticality.High)];
FUNCTION AMembers (ArrayName AS USUAL, oObjectOrClass AS USUAL, nArrayContentsID := 0 AS INT, cFlags := "" AS STRING) AS DWORD
    LOCAL oType AS Type
    LOCAL aFoxArray AS __FoxArray
    IF ArrayName IS __FoxArray var aFox
        aFoxArray := aFox
    ELSE
        var cMessage := __VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, nameof(ArrayName))
        THROW ArgumentException{cMessage}
    ENDIF

    IF IsObject(oObjectOrClass)
        oType := ((OBJECT)oObjectOrClass):GetType()
    ELSEIF IsString(oObjectOrClass)
        VAR cName := (STRING)oObjectOrClass
        oType := Type.GetType(cName, FALSE, TRUE) // Case insensitive

        IF oType == NULL
            // TODO(irwin): serach in loaded assemblies if not found directly
            RETURN 0
        ENDIF
    ELSE
        RETURN 0
    ENDIF

    VAR bFlags := BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.FlattenHierarchy
    VAR @@aMembers := oType:GetMembers(bFlags)

    VAR resultList := List<STRING[]>{}

    FOREACH VAR oMember IN @@aMembers
        LOCAL cMemberName AS STRING
        LOCAL cMemberType AS STRING
        LOCAL lAdd := FALSE AS LOGIC

        cMemberName := oMember:Name:ToUpper()
        cMemberType := ""

        IF cMemberName:StartsWith("GET_") .OR. cMemberName:StartsWith("SET_") .OR. ;
            cMemberName:StartsWith("ADD_") .OR. cMemberName:StartsWith("REMOVE_")
            LOOP
        ENDIF

        SWITCH oMember:MemberType
        CASE MemberTypes.Property
        CASE MemberTypes.Field
            cMemberType := "Property"
            lAdd := TRUE

        CASE MemberTypes.Method
            cMemberType := "Method"
            lAdd := TRUE

        CASE MemberTypes.Event
            cMemberType := "Event"
            lAdd := TRUE

        CASE MemberTypes.NestedType
            NOP
        END SWITCH

        IF lAdd
            VAR exists := resultList:Any({ x => x[1] == cMemberName })
            IF !exists
                resultList:Add( <STRING>{ cMemberName, cMemberType })
            ENDIF
        ENDIF
    NEXT

    // Foxpro sorts it alphabetically
    resultList:Sort({ x, y => String.Compare(x[1], y[1]) })

    VAR nRows := (DWORD)resultList:Count
    IF nRows == 0
        RETURN 0
    ENDIF

    IF nArrayContentsID == 0
        // 1 dimension: Just name
        aFoxArray:Resize((INT)nRows) // 1D
        FOR VAR i := 0 TO (INT)nRows - 1
            aFoxArray[i+1] := resultList[i][1]
        NEXT
    ELSEIF nArrayContentsID == 1
        // 2 dimensions: Name and Type
        aFoxArray:ReDim(nRows, 2) // 2D
        FOR VAR i := 0 TO (INT)nRows - 1
            aFoxArray[i+1, 1] := resultList[i][1] // Col 1: Name
            aFoxArray[i+1, 2] := resultList[i][2] // Col 2: Type
        NEXT
    ELSE
        // nInfo 2, 3 not supported yet
        THROW NotImplementedException{"AMEMBERS with nInfo=" + nArrayContentsID:ToString() + " is not fully implemented yet."}
    ENDIF
    RETURN nRows
END FUNCTION

/// <include file="VfpDocs.xml" path="Runtimefunctions/agetfileversion/*" />
[FoxArrayInputParameter(1)];
[FoxProFunction("AGETFILEVERSION", FoxFunctionCategory.Array, FoxEngine.RuntimeCore, FoxFunctionStatus.Partial, FoxCriticality.Medium)];
FUNCTION AGetFileVersion (ArrayName AS USUAL, cFileName AS STRING) AS DWORD
    IF String.IsNullOrEmpty(cFileName) .OR. !File.Exists(cFileName)
        RETURN 0
    ENDIF
    LOCAL aFoxArray AS __FoxArray
    IF ArrayName IS __FoxArray var aFox
        aFoxArray := aFox
    ELSE
        var cMessage := __VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, nameof(ArrayName))
        THROW ArgumentException{cMessage}
    ENDIF
    LOCAL oInfo AS FileVersionInfo
    TRY
        oInfo := FileVersionInfo.GetVersionInfo(cFileName)
    CATCH
        RETURN 0
    END TRY

    IF String.IsNullOrEmpty(oInfo:FileVersion)
        RETURN 0
    ENDIF

    ASize(aFoxArray, 15)

    aFoxArray[1] := oInfo:Comments ?? "" // 1. Comments
    aFoxArray[2] := oInfo:CompanyName ?? "" // 2. Company Name
    aFoxArray[3] := oInfo:FileDescription ?? "" // 3. File Description
    aFoxArray[4] := oInfo:FileVersion ?? "" // 4. File Version
    aFoxArray[5] := oInfo:InternalName ?? "" // 5. Internal Name
    aFoxArray[6] := oInfo:LegalCopyright ?? "" // 6. Legal Copyright
    aFoxArray[7] := oInfo:LegalTrademarks ?? "" // 7. Legal Trademarks
    aFoxArray[8] := oInfo:OriginalFilename ?? "" // 8. Original File Name
    aFoxArray[9] := oInfo:PrivateBuild ?? "" // 9. Private Build
    aFoxArray[10] := oInfo:ProductName ?? "" // 10. Product Name
    aFoxArray[11] := oInfo:ProductVersion ?? "" // 11. Product Version
    aFoxArray[12] := oInfo:SpecialBuild ?? "" // 12. Special Build
    aFoxArray[13] := "" // 13. OLE Self Registration (Not available in FileVersionInfo)
    aFoxArray[14] := oInfo:Language ?? "" // 14. Language
    aFoxArray[15] := "" // 15. Translation Code (this is complex and requires Win32 API)

    // TODO(irwin): Elements 13, 15 require low-level Win32 VerQueryValue API logic
    // which FileVersionInfo wraps but does not expose fully.
    RETURN 15
END FUNCTION

/// <include file="VfpDocs.xml" path="Runtimefunctions/aused/*" />
[FoxArrayInputParameter(1)];
[FoxProFunction("AUSED", FoxFunctionCategory.Array, FoxEngine.WorkArea, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION AUsed (ArrayName AS USUAL, nDataSessionNumber := NIL AS USUAL, cTableName := NIL AS USUAL) AS DWORD

    VAR oWA := RuntimeState.Workareas

    LOCAL aFoxArray AS __FoxArray
    IF ArrayName IS __FoxArray var aFox
        aFoxArray := aFox
    ELSE
        var cMessage := __VfpStr(VFPErrors.VFP_VARIABLE_NOT_ARRAY, nameof(ArrayName))
        THROW ArgumentException{cMessage}
    ENDIF

    IF oWA == NULL
        RETURN 0
    ENDIF

    LOCAL cFilterName := NULL AS STRING
    IF IsString(cTableName)
        cFilterName := ((STRING)cTableName):ToUpper()
    ENDIF

    VAR resultList := List<KeyValuePair<STRING, DWORD>>{}

    FOREACH VAR pair IN oWA:OpenAliases
        VAR cAlias := pair:Key

        IF !String.IsNullOrEmpty(cFilterName)
            IF cAlias:ToUpper() != cFilterName
                LOOP
            ENDIF
        ENDIF

        resultList:Add(pair)
    NEXT

    // LIFO ordering (reverse order)
    // VFP places the last opened first
    VAR sortedList := resultList:OrderByDescending({ p => p:Value }):ToList()

    VAR nCount := (DWORD)sortedList:Count

    IF nCount == 0
        RETURN 0
    ENDIF

    aFoxArray:ReDim(nCount, 2)

    FOR VAR i := 0 TO (INT)nCount - 1
        aFoxArray[i+1, 1] := sortedList[i]:Key
        aFoxArray[i+1, 2] := sortedList[i]:Value
    NEXT
    RETURN nCount
END FUNCTION

