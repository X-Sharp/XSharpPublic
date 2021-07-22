//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING XSharp.Internal


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
        THROW ArgumentOutOfRangeException { nameof(nArrayAttribute),nArrayAttribute, "'nArrayAttribute' number is out of range (expected 0, 1 or 2)"}
    END SWITCH


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/alen/*" />
/// <remarks>The parameter to this function is a 'General Array'. The function decides at runtime if the array is a FoxPro array or a 'General' Array</remarks>
FUNCTION ALen(a AS ARRAY) AS DWORD
    IF a IS __FoxArray VAR foxArray
        RETURN ALen(foxArray, 0)
    ELSE
        RETURN XSharp.RT.Functions.ALen(a)
    ENDIF

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/aelement/*" />
FUNCTION AElement(ArrayName AS __FoxArray, nRowSubscript AS DWORD) AS USUAL
   IF ( nRowSubscript > 0 .AND. nRowSubscript <= ArrayName:Rows )
      RETURN nRowSubscript
   ENDIF
   THROW ArgumentOutOfRangeException { nameof(nRowSubscript),nRowSubscript, "'nRowSubscript' number is out of range"}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/aelement/*" />
FUNCTION AElement(ArrayName AS __FoxArray, nRowSubscript AS DWORD, nColumnSubscript AS DWORD) AS USUAL
    IF ArrayName:MultiDimensional
        IF nRowSubscript == 0 .OR. nRowSubscript >  ArrayName:Rows
           THROW ArgumentOutOfRangeException { nameof(nRowSubscript), nRowSubscript, "'nRowSubscript' number is out of range" }
        ELSEIF nColumnSubscript == 0 .OR. nColumnSubscript > ArrayName:Columns
           THROW ArgumentOutOfRangeException { nameof(nColumnSubscript), nColumnSubscript, "'nColumnSubscript' number is out of range" }
        ENDIF
        nRowSubscript --
        RETURN ( nRowSubscript * ArrayName:Columns ) + nColumnSubscript
    ENDIF
THROW ArgumentException { "a one-dimensional array has no columns"}

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/adel/*" />
/// <remarks>The parameter to this function is a 'General Array'. The function decides at runtime if the array is a FoxPro array or a 'General' Array</remarks>
FUNCTION ADel(ArrayName AS ARRAY, nElementNumber AS LONG, nDeleteType := 2 AS LONG) AS DWORD
    IF ! ArrayName IS __FoxArray VAR foxArray
        XSharp.RT.Functions.ADel(ArrayName, (DWORD) nElementNumber )
        RETURN 1
    ENDIF
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


/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asubscript/*" />
FUNCTION ASubScript(ArrayName AS __FoxArray, nElementNumber AS DWORD, nSubscript := 1 AS DWORD) AS DWORD
    IF nSubscript == 0 .OR. nSubscript > 2
		THROW ArgumentOutOfRangeException { nameof(nSubscript), nSubscript, "'nSubscript' number is out of range" }
	ELSEIF nElementNumber == 0 .OR. nElementNumber >  ArrayName:Count
		THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, "'nElementNumber' number is out of range" }
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
			THROW ArgumentException { "a one-dimensional array has no columns"}
		ENDIF

		RETURN nElementNumber

    ENDIF

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/ains/*" />
/// <remarks>The parameter to this function is a 'General Array'. The function decides at runtime if the array is a FoxPro array or a 'General' Array</remarks>
FUNCTION AIns(ArrayName AS ARRAY, nElementNumber AS DWORD, nInsertType := 1 AS DWORD) AS DWORD
    IF ! ArrayName IS __FoxArray VAR foxArray
        XSharp.RT.Functions.AIns(ArrayName, nElementNumber)
        RETURN 1
    ENDIF
    IF !foxArray:MultiDimensional
        IF nInsertType > 1
			THROW ArgumentException { "a one-dimensional array has no columns"}
		ELSEIF nElementNumber == 0 .OR. nElementNumber > (DWORD) ArrayName:Count
			THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, "'nElementNumber' number is out of range" }
		ENDIF

		foxArray:Insert((LONG) nElementNumber)
    ELSE
        IF nInsertType > 2
			THROW ArgumentOutOfRangeException { nameof(nInsertType), nInsertType, "'nInsertType' number is out of range" }
		ENDIF
        IF nInsertType == 2
            IF nElementNumber == 0 .OR. nElementNumber > (DWORD) foxArray:Columns
				THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, "'nElementNumber' number is out of range" }
			ENDIF

			foxArray:InsertColumn( (LONG) nElementNumber)
        ELSE
            IF nElementNumber == 0 .OR. nElementNumber > (DWORD) foxArray:Rows
				THROW ArgumentOutOfRangeException { nameof(nElementNumber), nElementNumber, "'nElementNumber' number is out of range" }
			ENDIF

			foxArray:InsertRow((LONG) nElementNumber)
        ENDIF
    ENDIF
    RETURN 1

/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/asize/*" />
/// <remarks>The parameter to this function is a 'General Array'. The function decides at runtime if the array is a FoxPro array or a 'General' Array</remarks>
FUNCTION ASize(ArrayName AS ARRAY, nSize AS DWORD) AS ARRAY
    IF ! ArrayName IS __FoxArray VAR foxArray
        RETURN XSharp.RT.Functions.ASize(ArrayName, nSize)
    ENDIF
    foxArray:Resize((LONG) nSize)
    RETURN foxArray



/// <inheritdoc cref="ShowArray" />
FUNCTION ShowFoxArray ( aTest AS __FoxArray , cPrefix := "" AS STRING ) AS VOID
    LOCAL i, j AS DWORD

	IF cPrefix:Length == 0
		cPrefix := "a"
	ENDIF

	IF aTest:MultiDimensional
		FOR i := 1 TO ALen ( aTest , 1 )
			FOR j := 1 TO ALen ( aTest , 2 )
				 QOut(cPrefix + "[" + AsString(AElement ( aTest , i , j )) + "] [" + i:ToString() + "," + j:ToString() + "] = " + AsString ( aTest [i,j] ) + ;
					 " " + GetElementValueType ( aTest[i,j] ))
			NEXT
		NEXT
	ELSE
		FOR i := 1 TO ALen ( aTest , 0 )
			QOut( cPrefix + "[" + i:ToString() + "] = " + AsString ( aTest [i] ) + " " + GetElementValueType ( aTest[i] ))
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


