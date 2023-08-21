//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.Internal


INTERNAL FUNCTION FoxALen(a as ARRAY) AS DWORD
RETURN ALen( (__FoxArray) a, 0)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/alen/*" />
FUNCTION ALen(a AS __FoxArray) AS DWORD
    RETURN ALen(a, 0)

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
        var cMessage := __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE, nameof(nArrayAttribute))
        THROW ArgumentOutOfRangeException { nameof(nArrayAttribute),nArrayAttribute, cMessage}
    END SWITCH


/// <exclude/>
FUNCTION __FoxALen(a AS __FoxArray) AS DWORD
    RETURN ALen(a, 0)

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/aelement/*" />
FUNCTION AElement(ArrayName AS __FoxArray, nRowSubscript AS DWORD) AS USUAL
   IF ( nRowSubscript > 0 .AND. nRowSubscript <= ArrayName:Rows )
      RETURN nRowSubscript
   ENDIF
   var cMessage := __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE, nameof(nRowSubscript))
   THROW ArgumentOutOfRangeException { nameof(nRowSubscript),nRowSubscript, cMessage}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/aelement/*" />
FUNCTION AElement(ArrayName AS __FoxArray, nRowSubscript AS DWORD, nColumnSubscript AS DWORD) AS USUAL
    IF ArrayName:MultiDimensional
        IF nRowSubscript == 0 .OR. nRowSubscript >  ArrayName:Rows
           var cMessage := __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE,nameof(nRowSubscript))
           THROW ArgumentOutOfRangeException { nameof(nRowSubscript), nRowSubscript, cMessage}
        ELSEIF nColumnSubscript == 0 .OR. nColumnSubscript > ArrayName:Columns
           var cMessage := __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE,nameof(nColumnSubscript))
           THROW ArgumentOutOfRangeException { nameof(nColumnSubscript), nColumnSubscript, cMessage }
        ENDIF
        nRowSubscript --
        RETURN ( nRowSubscript * ArrayName:Columns ) + nColumnSubscript
    ENDIF
    THROW ArgumentException { __VfpStr(VFPErrors.ONE_DIM_NO_COLUMNS)}

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
FUNCTION ADel(foxArray AS __FoxArray, nElementNumber AS LONG, nDeleteType := 2 AS LONG) AS DWORD
    RETURN __FoxADel(foxArray, nElementNumber, nDeleteType)


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/adel/*" />
/// <remarks>The parameter to this function is a 'General Array'. The function decides at runtime if the array is a FoxPro array or a 'General' Array</remarks>
FUNCTION ADel(ArrayName AS ARRAY, nElementNumber AS LONG, nDeleteType AS LONG) AS DWORD
    IF ArrayName IS __FoxArray VAR foxArray
        RETURN __FoxADel(foxArray, nElementNumber, nDeleteType)
    ENDIF
    XSharp.RT.Functions.ADel(ArrayName, (DWORD) nElementNumber )
    RETURN 1


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asubscript/*" />
FUNCTION ASubScript(ArrayName AS __FoxArray, nElementNumber AS DWORD, nSubscript := 1 AS DWORD) AS DWORD
    IF nSubscript == 0 .OR. nSubscript > 2
        var cMessage := __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE, nameof(nSubscript))
        THROW ArgumentOutOfRangeException { nameof(nSubscript), nSubscript, cMessage }
    ELSEIF nElementNumber == 0 .OR. nElementNumber >  ArrayName:Count
        var cMessage := __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE, nameof(nElementNumber))
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
            THROW ArgumentException { __VfpStr(VFPErrors.ONE_DIM_NO_COLUMNS)}
        ENDIF

        RETURN nElementNumber

    ENDIF

/// <exclude/>
FUNCTION __FoxAIns(foxArray AS __FoxArray, nElementNumber AS DWORD, nInsertType := 1 AS DWORD) AS DWORD
    IF !foxArray:MultiDimensional
        IF nInsertType > 1
            THROW ArgumentException { __VfpStr(VFPErrors.ONE_DIM_NO_COLUMNS)}
        ELSEIF nElementNumber == 0 .OR. nElementNumber > (DWORD) foxArray:Count
            THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE, nameof(nElementNumber)) }
        ENDIF

        foxArray:Insert((LONG) nElementNumber)
    ELSE
        IF nInsertType > 2
            THROW ArgumentOutOfRangeException { nameof(nInsertType), nInsertType, __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE, nameof(nInsertType)) }
        ENDIF
        IF nInsertType == 2
            IF nElementNumber == 0 .OR. nElementNumber > (DWORD) foxArray:Columns
                THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE, nameof(nElementNumber)) }
            ENDIF

            foxArray:InsertColumn( (LONG) nElementNumber)
        ELSE
            IF nElementNumber == 0 .OR. nElementNumber > (DWORD) foxArray:Rows
                THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, __VfpStr(VFPErrors.ATTRIBUTE_OUT_OF_RANGE, nameof(nElementNumber))}
            ENDIF

            foxArray:InsertRow((LONG) nElementNumber)
        ENDIF
    ENDIF
    RETURN 1

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ains/*" />
FUNCTION AIns(ArrayName AS __FoxArray, nElementNumber AS DWORD, nInsertType := 1 AS DWORD) AS DWORD
    RETURN __FoxAIns(ArrayName, nElementNumber, nInsertType)


INTERNAL FUNCTION FoxAIns(ArrayName AS ARRAY, nElementNumber AS DWORD, nInsertType AS DWORD) AS DWORD
    IF ArrayName IS __FoxArray VAR foxArray
        RETURN __FoxAIns(foxArray, nElementNumber, nInsertType)
    ENDIF
    XSharp.RT.Functions.AIns(ArrayName, nElementNumber)
    RETURN 1



/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ains/*" />
/// <remarks>The parameter to this function is a 'General Array'. The function decides at runtime if the array is a FoxPro array or a 'General' Array</remarks>
FUNCTION AIns(ArrayName AS ARRAY, nElementNumber AS DWORD, nInsertType AS DWORD) AS DWORD
    IF ArrayName IS __FoxArray VAR foxArray
        RETURN __FoxAIns(foxArray, nElementNumber, nInsertType)
    ENDIF
    XSharp.RT.Functions.AIns(ArrayName, nElementNumber)
    RETURN 1

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asize/*" />
/// <remarks>The parameter to this function is a 'General Array'. The function decides at runtime if the array is a FoxPro array or a 'General' Array</remarks>
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


