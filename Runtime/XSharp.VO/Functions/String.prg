//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Text

/// <summary>
/// Execute a code block for each of the individual characters in a string.
/// </summary>
/// <param name="c"></param>
/// <param name="cod"></param>
/// <param name="nStart"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function SEval(c as usual,cod as usual,nStart as usual,nCount as usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   

/// <summary>
/// Execute a code block for each of the individual characters in a string, changing the contents of the argument as well as the return value.
/// </summary>
/// <param name="c"></param>
/// <param name="cod"></param>
/// <param name="nStart"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
function SEvalA(c as usual,cod as usual,nStart as usual,nCount as usual) as string
	/// THROW NotImplementedException{}
	return String.Empty   



internal FUNCTION __AtLenForStrTran( c1 AS STRING, c2 AS STRING, uiLen AS INT, c2offset AS INT ) AS INT
   LOCAL uiPos  := 0 AS INT
   LOCAL i      := 0 AS INT
   LOCAL fFound := false AS LOGIC

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

/// <summary>
/// Search and replace characters within a string.
/// </summary>
/// <param name="c"></param>
/// <param name="cSearch"></param>
/// <param name="cReplace"></param>
/// <param name="iStart"></param>
/// <param name="nCount"></param>
/// <returns>
/// </returns>
FUNCTION StrTran( uTarget, uSearch, uReplace, uStart, uCount ) AS STRING CLIPPER

   LOCAL cSource          AS STRING
   LOCAL cSearch          AS STRING
   LOCAL cReplace         AS STRING
   LOCAL nStart := 1      AS INT
   LOCAL iCount := 0xFFF8 AS INT   // This is MAX_ALLOC in VO
   LOCAL iSource          AS INT
   LOCAL iSearch          AS INT
   LOCAL iRepl            AS INT

   IF ! uTarget:IsString
      THROW Error.DataTypeError( "StrTran", "cTarget", 1 , uTarget, uSearch, uReplace, uStart, uCount)
   ELSE
      cSource := uTarget

      IF cSource == ""
         RETURN ""
      ENDIF
   ENDIF

   IF ! uSearch:IsString
      THROW Error.DataTypeError( "StrTran", "cSearch", 2 , uTarget, uSearch, uReplace, uStart, uCount)
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
      THROW  Error.DataTypeError( "StrTran", "cReplace", 3 , uTarget, uSearch, uReplace, uStart, uCount)
   ENDIF

   IF PCount() > 3
      IF uStart:IsNumeric
         nStart := uStart
      ELSEIF uStart:IsNil
         nStart := 0
      ELSE
         THROW Error.DataTypeError( "StrTran", "nStart", 4 , uTarget, uSearch, uReplace, uStart, uCount)
      ENDIF

      IF PCount() > 4
         IF uCount:IsNumeric
            iCount := uCount
         ELSEIF uCount:IsNil
            iCount := 0
         ELSE
            THROW Error.DataTypeError( "StrTran", "nCount", 5 , uTarget, uSearch, uReplace, uStart, uCount)
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
   sbRet:Append( '\0', iSize )		// fill with chr(0)

   LOCAL iSrcPos := 0 AS INT
   LOCAL iFound  := 0 AS INT
   LOCAL iTemp   := 0 AS INT
   LOCAL iOffset := 0 AS INT

   LOCAL x, y, z AS INT

   DO WHILE iCount != 0 && ( iFound <= iSize )
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

/// <summary>
/// </summary>
/// <param name="c"></param>
/// <param name="iStart"></param>
/// <param name="wLen"></param>
/// <returns>
/// </returns>
function SubS(c ,iStart ,wLen ) as string CLIPPER
	return SubStr(c, iStart, wLen)


/// <summary>
/// Extract a substring from a string.
/// </summary>
/// <param name="c"></param>
/// <param name="iStart"></param>
/// <param name="wLen"></param>
/// <returns>
/// </returns>
function SubStr(c ,uStart ,uLen ) as string CLIPPER
	if ! c:IsString
		throw Error.DataTypeError(__ENTITY__, nameof(c), 1, c, uStart, uLen)
	endif
	if uStart:IsNil
		uStart := 1
	elseif ! uStart:IsNumeric
		throw Error.DataTypeError(__ENTITY__, nameof(uStart), 2, c, uStart, uLen)
	endif
	var start := (int) uStart 
	var strValue := (string) c 
	if uLen:isNil
		local nLen as int
		nLen := strValue:Length
		if start < 0
			start := Math.Abs(start)
		endif
		if nLen > start
			nLen := start 
		endif
		return __SubStr(strValue, start, nLen - start + 1)
	elseif uLen:IsNumeric
		return __SubStr(strValue, start, (int) uLen)
	else
		throw Error.DataTypeError(__ENTITY__, nameof(uLen), 3, c, uStart, uLen)
	endif

function EmptyString (s as string) as logic
	if !String.IsNullOrEmpty(s)
		foreach c as char in s
			switch c
			case '\r'
			case '\n'
			case '\t'
			case ' '
				nop
			otherwise
				return false
			end switch
		next
	endif
	return true
