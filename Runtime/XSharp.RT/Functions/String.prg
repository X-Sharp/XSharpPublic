//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Text

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/seval/*" />
FUNCTION SEval(cString ,cbBlock ,nStart ,nCount ) AS STRING CLIPPER
	IF ! cString:IsString
		THROW Error.ArgumentError( __FUNCTION__, NAMEOF(cString), 1, <OBJECT>{ cString} )
	ENDIF
	IF ! cbBlock:IsCodeBlock .OR. cbBlock == NULL_CODEBLOCK
		THROW Error.ArgumentError( __FUNCTION__, NAMEOF(cbBlock), 2, <OBJECT>{ cbBlock} )
	ENDIF
	IF nStart == NIL
		nStart := 1
	ELSEIF ! nStart:IsNumeric
		THROW Error.ArgumentError( __FUNCTION__, NAMEOF(nStart), 3, <OBJECT>{ nStart} )
	ENDIF
	IF nCount == NIL
		nCount := SLen(cString) - nStart + 1
	ELSEIF ! nCount:IsNumeric
		THROW Error.ArgumentError( __FUNCTION__, NAMEOF(nCount), 4, <OBJECT>{ nCount} )
	ENDIF
	RETURN SEvalWorker(cString, cbBlock, nStart, nCount)


INTERNAL FUNCTION SEvalWorker(cSource AS STRING, oBlock AS CODEBLOCK, nStart AS INT, nCount AS INT) AS STRING
	LOCAL elements AS INT
	LOCAL nLast		AS INT
	LOCAL x AS LONG
	elements := cSource:Length
	nLast := nStart + nCount -1
	IF nLast > elements
		nLast := elements
	ENDIF
	// note: String is 0-based
	IF nStart > 0
		FOR x := nStart UPTO nLast 
			oBlock:Eval(cSource[x - 1], x )  
		NEXT
	ELSE
		nLast := Math.Max(1, nStart+nCount +1)
        FOR x := nStart DOWNTO nLast
			oBlock:Eval(cSource[x - 1], x )  
        NEXT
	ENDIF
	RETURN cSource

INTERNAL FUNCTION __AtLenForStrTran( c1 AS STRING, c2 AS STRING, uiLen AS INT, c2offset AS INT ) AS INT
   LOCAL uiPos  := 0 AS INT
   LOCAL i      := 0 AS INT
   LOCAL fFound := FALSE AS LOGIC

   IF c1 == "" || c2 == ""
      RETURN 0
   ENDIF

   DO WHILE uiLen != 0
      IF c1[0] == c2[uiPos + c2offset]
         fFound := TRUE

         DO WHILE ++i < c1:Length
            IF c1[i] != c2[uiPos+i+c2offset]
               fFound := FALSE
               i := 0
               EXIT
            ENDIF
         ENDDO

         IF fFound
            EXIT
         ENDIF
      ENDIF

      uiPos++

      uiLen--
   ENDDO


   IF fFound
      uiPos++
   ELSE
      uiPos := 0
   ENDIF

   RETURN uiPos

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/strtran/*" />
FUNCTION StrTran( uTarget, uSearch, uReplace, uStart, uCount ) AS STRING CLIPPER

   LOCAL cSource          AS STRING
   LOCAL cSearch         AS STRING
   LOCAL cReplace         AS STRING
   LOCAL nStart := 1      AS INT
   LOCAL iCount := 0xFFF8 AS INT   // This is MAX_ALLOC in VO
   LOCAL iSource          AS INT
   LOCAL iSearch          AS INT
   LOCAL iRepl            AS INT

   IF ! uTarget:IsString
      THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uTarget), 1 , <OBJECT>{uTarget})
   ELSE
      cSource := uTarget

      IF cSource == ""
         RETURN ""
      ENDIF
   ENDIF

   IF ! uSearch:IsString
      THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uSearch), 2 ,  <OBJECT>{uSearch})
   ELSE
      cSearch := uSearch
      IF cSearch == ""
         RETURN cSource
      ENDIF
   ENDIF

   IF uReplace:IsString
      cReplace := uReplace
   ELSEIF uReplace:IsNil
      cReplace := ""
   ELSE
      THROW  Error.ArgumentError( __FUNCTION__, NAMEOF(uReplace), 3 , <OBJECT>{uReplace})
   ENDIF

   IF PCount() > 3
      IF uStart:IsNumeric
         nStart := uStart
      ELSEIF uStart:IsNil
         nStart := 0
      ELSE
         THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uStart), 4 ,  <OBJECT>{uStart})
      ENDIF

      IF PCount() > 4
         IF uCount:IsNumeric
            iCount := uCount
         ELSEIF uCount:IsNil
            iCount := 0
         ELSE
            THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uCount), 5 , <OBJECT>{uCount})
         ENDIF
      ENDIF
   ENDIF

   iSource := cSource:Length
   iSearch := cSearch:Length
   iRepl   := cReplace:Length

   IF iSearch > iSource
      RETURN cSource
   ENDIF

   LOCAL iOccurs AS INT
   LOCAL iDiff   AS INT
   LOCAL iSize   AS INT

   iOccurs := (INT) Occurs3( cSearch, cSource, 0 )

   IF iCount > iOccurs
      iCount := iOccurs
   ENDIF

   IF nStart > iOccurs
      RETURN cSource
   ELSE
      iCount := Math.Min( iCount, iOccurs - nStart + 1 )
   ENDIF

   IF iRepl > iSearch
      iDiff := iRepl - iSearch

      IF iOccurs == 0
         RETURN cSource
      ENDIF

      iSize := iSource + ( iCount * iDiff )
   ELSEIF iRepl < iSearch
      iDiff := iSearch - iRepl

      IF iOccurs == 0
         RETURN cSource
      ENDIF
      iSize := iSource - ( iCount * iDiff )
   ELSE
      iSize   := iSource
      iOccurs := iSize / iSearch
   ENDIF

   LOCAL sbRet AS StringBuilder
   sbRet := StringBuilder{ iSize }
   sbRet:Append( c'\0', iSize )		// fill with chr(0)

   LOCAL iSrcPos := 0 AS INT
   LOCAL iFound  := 0 AS INT
   LOCAL iTemp   := 0 AS INT
   LOCAL iOffset := 0 AS INT

   LOCAL x, y, z AS INT

   DO WHILE iCount != 0 .AND. ( iFound <= iSize )
      iFound := __AtLenForStrTran( cSearch, cSource, iSource - iSrcPos, iSrcPos )

      IF iFound != 0
         iTemp ++
         iFound--

         IF iTemp < nStart
            //memcpy(&sbRet[iOffset], &cSource[iSrcPos], iFound + iSearch)
            x := iOffset
            y := iSrcPos

            FOR z := 1 UPTO iFound + iSearch
               sbRet[x++] := cSource[y++]
            NEXT

            iSrcPos += (iFound + iSearch)
            iOffset := iSrcPos
         ELSE
            //memcpy(&sbRet[iOffset], &cSource[iSrcPos], iFound)

            x := iOffset
            y := iSrcPos

            FOR z := 1 UPTO iFound
               sbRet[x++] := cSource[y++]
            NEXT

            iSrcPos += iFound + iSearch
            iOffset += iFound

            IF iRepl != 0
               //memcpy(&sbRet[iOffset], cRepl, iRepl)

               x := iOffset
               y := 0

               FOR z := 1 UPTO iRepl
                  sbRet[x++] := cReplace[y++]
               NEXT

               iOffset += iRepl
            ENDIF

            iCount--
         ENDIF

         iFound++
      ELSE
         EXIT
      ENDIF
   ENDDO

   IF iSize > iOffset
      iDiff := iSize - iOffset

      // memcpy(&sbRet[uiOffset], &cSource[uiSrcPos], uiDiff)
      x := iOffset
      y := iSrcPos

      FOR z := 1 UPTO iDiff
         sbRet[x++] := cSource[y++]
      NEXT
   ENDIF

   RETURN sbRet:ToString()

/// <inheritdoc cref="M:XSharp.RT.Functions.SubStr(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)" />
/// <seealso cref='M:XSharp.RT.Functions.SubStr(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)' />
/// <seealso cref='M:XSharp.Core.Functions.SubStr3(System.String,System.UInt32,System.UInt32)' />
FUNCTION SubS(cTarget ,nStart ,nCount ) AS STRING CLIPPER
	RETURN SubStr(cTarget, nStart, nCount)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/substr/*" />
/// <seealso cref='M:XSharp.Core.Functions.SubStr3(System.String,System.UInt32,System.UInt32)' />
/// <seealso cref='M:XSharp.RT.Functions.SubS(XSharp.__Usual,XSharp.__Usual,XSharp.__Usual)' />
FUNCTION SubStr(cTarget ,nStart ,nCount ) AS STRING CLIPPER
	IF ! cTarget:IsString
		THROW Error.ArgumentError(__FUNCTION__, NAMEOF(cTarget), 1, <OBJECT>{cTarget})
	ENDIF
	IF nStart:IsNil
		nStart := 1
	ELSEIF ! nStart:IsNumeric
		THROW Error.ArgumentError(__FUNCTION__, NAMEOF(nStart), 2, <OBJECT>{nStart})
	ENDIF
	VAR start := (INT) nStart 
	VAR strValue := (STRING) cTarget 
	IF nCount:isNil
		LOCAL nLen AS INT
		nLen := strValue:Length
		IF start < 0
			IF Math.Abs(start) > strValue:Length
				RETURN ""
			ELSE
				start := strValue:Length - Math.Abs(start) + 1
			END IF
		ENDIF
		IF nLen > strValue:Length - start + 1
			nLen := strValue:Length - start + 1
		ENDIF
		RETURN __SubStr(strValue, start, nLen )
	ELSEIF nCount:IsNumeric
		RETURN __SubStr(strValue, start, (INT) nCount)
	ELSE
		THROW Error.ArgumentError(__FUNCTION__, NAMEOF(nCount), 3, <OBJECT>{nCount})
	ENDIF

FUNCTION EmptyString (s AS STRING) AS LOGIC
	IF !String.IsNullOrEmpty(s)
		FOREACH c AS CHAR IN s
			SWITCH c
			CASE c'\r'
			CASE c'\n'
			CASE c'\t'
			CASE c' '
				NOP
			OTHERWISE
				RETURN FALSE
			END SWITCH
		NEXT
	ENDIF
	RETURN TRUE




