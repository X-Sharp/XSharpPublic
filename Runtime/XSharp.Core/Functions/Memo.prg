//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//



PUBLIC CLASS MemoHelpers
	CONST BLANK      := 0x20 as INT
	CONST END_MEMO   := 0x1A as INT // ^Z
	CONST MAX_WIDTH  := 254 AS INT
	CONST TAB		 := 9 AS INT
	CONST LINE_FEED  := 10 AS INT
	CONST HARD_CR    := 13 AS INT
	CONST SOFT_CR    := 141 AS INT
	CONST HARD_CR_LF := (HARD_CR << 8) | LINE_FEED AS INT
	CONST SOFT_CR_LF := (SOFT_CR << 8) | LINE_FEED AS INT
	CONST STD_TAB_WIDTH	:= 4 AS INT	
	CONST STD_MEMO_WIDTH	 := 79	AS INT


	STATIC METHOD  MLCount( cMemo AS STRING, nLineLen:= MemoHelpers.STD_MEMO_WIDTH AS INT, ;
		nTabSize := MemoHelpers.STD_TAB_WIDTH AS INT,  lWrap := TRUE AS LOGIC) AS DWORD 
		LOCAL nTempLen AS INT
		LOCAL nLines := 0 AS DWORD
		LOCAL nIndex := 0 AS INT
		IF cMemo == NULL
			return 0
		endif
		IF nLineLen > 0 .and. nLineLen <= MemoHelpers.MAX_WIDTH
	
			IF nTabSize > nLineLen
				nTabSize := nLineLen
			ENDIF
		
			nTempLen := 1
			DO WHILE nTempLen != 0
				nTempLen := MemoHelpers.LineLen( cMemo, nIndex, nLineLen, nTabSize, lWrap )
				IF nTempLen != 0
					nIndex += nTempLen
					nLines ++
				ENDIF
			ENDDO
		ENDIF
		RETURN nLines

	STATIC METHOD GetTabPos( nPos AS INT, nTabSize AS INT ) AS INT
		RETURN ( nPos + nTabSize ) - ( nPos % nTabSize )
		
	STATIC METHOD MPosToLc(cMemo AS STRING,nLineLen AS INT,nPos AS INT,nTabSize AS INT,lWrap AS LOGIC) AS Tuple<INT, INT>
		LOCAL nLineNum := 0 AS INT
		LOCAL nColumn := 0 AS INT
		LOCAL nTempLen := 0 AS INT
		LOCAL nChar := 0 AS INT
		LOCAL nCrLf := 0 AS INT
		LOCAL nIndex:= 0  AS INT
		LOCAL nSrc  := 0 AS INT
		
		
		IF nPos <= cMemo:Length .and. nLineLen > 0 .and. nLineLen <= MemoHelpers.MAX_WIDTH
		
			IF nTabSize > nLineLen
				nTabSize := nLineLen
			ENDIF
			
			nTempLen := 1
			nLineNum := 1
			
			DO WHILE nIndex < nPos .and. nTempLen != 0
				nTempLen := LineLen( cMemo, nIndex, nLineLen, nTabSize, lWrap )
				nIndex += nTempLen
				IF nIndex <= nPos
					nSrc := nIndex
					nLineNum ++
				ENDIF
			ENDDO
			DO WHILE nSrc < nPos
				nChar := cMemo[(INT) nSrc]
				DO CASE
					CASE nChar == TAB
						nColumn := GetTabPos( nColumn, nTabSize )
					CASE IsCrLf( cMemo, nSrc, nCrLf )
						nColumn += 2
					OTHERWISE
						nColumn ++
				END CASE
				nSrc ++
			ENDDO
		ENDIF
		
		RETURN Tuple<INT,INT>{ nLineNum, nColumn }
		
		
	STATIC METHOD  MLcToPos( cMemo AS STRING, nLineLen AS INT, nLineNum as INT, nColumn as INT, nTabSize as INT, lWrap as LOGIC) AS DWORD 
		LOCAL nTempLen	:= 0 AS INT
		LOCAL nChar		:= 0 AS INT
		LOCAL nCrLf		:= 0 AS INT
		LOCAL nIndex	:= 0 AS INT
		LOCAL nPos		:= 0 AS INT
		
		
		IF  nLineNum > 0 .and. nLineLen > 4 .and. nLineLen <= MAX_WIDTH
		
			IF nTabSize >= nLineLen
				nTabSize := nLineLen - 1
			ENDIF
			
			nTempLen := 1
			
			nLineNum --
			DO WHILE nLineNum != 0 .and. nTempLen != 0
				nTempLen := LineLen( cMemo, nIndex, nLineLen, nTabSize, lWrap )
				nIndex += nTempLen
				nLineNum --
			ENDDO
			
			//  If a soft or hard CR falls just past the length in the
			//  previous line it won't be counted.  We must add as two chars
			//  to the buffer position in order for it to work out correctly.
			
			IF IsCrLf( cMemo, nIndex, nCrLf ) .and. nCrLf == SOFT_CR_LF
				nIndex += 2
			ENDIF
			
			DO WHILE nPos < nColumn
			
				IF nIndex < cMemo:Length
					nChar := cMemo[(int) nIndex]
				ELSE
					nChar := END_MEMO
				ENDIF
				DO CASE
					CASE nCrLf == SOFT_CR_LF .OR. nCrLf == HARD_CR_LF .OR. nChar == END_MEMO
						nPos := nColumn
					CASE nChar == TAB
						nPos := GetTabPos( nPos, nTabSize )
						IF nPos <= nColumn
							nIndex ++
						ENDIF
					OTHERWISE
						nIndex += 1
						nPos += 1
				ENDCASE
				
			ENDDO
			
		ENDIF
		
		RETURN (DWORD) ( nIndex + 1 )

	STATIC METHOD IsCrLf( cMemo AS STRING, nPos AS INT, nCrLf REF INT ) AS LOGIC
		LOCAL nChar AS INT
		
		nCrLf := 0
		IF nPos + 1 < cMemo:Length .and. (INT) cMemo[(INT) nPos + 1] == LINE_FEED
			nChar := (INT) cMemo[(INT) nPos]
			SWITCH nCHar
			CASE HARD_CR
				nCrLf := HARD_CR_LF
			CASE SOFT_CR
				nCrLf := SOFT_CR_LF
			END SWITCH
		ENDIF
		RETURN nCRLF != 0

		
	STATIC METHOD LineLen( cMemo AS STRING, nStart AS INT, nLineLen AS INT, nTabSize AS INT , lWrap AS LOGIC ) AS INT
	   LOCAL lCont AS LOGIC
		   LOCAL nChar AS INT
		   LOCAL nPos AS INT
		   LOCAL nWhite := 0 AS INT
		   LOCAL nWidth := 0 AS INT
		   LOCAL nLength AS INT
		   LOCAL nCrLf := 0 AS INT
   
		   nLength	:= cMemo:Length
		   nPos		:= nStart
		   lCont	:= TRUE
   
		   DO WHILE lCont .and. nPos < nLength .and. nWidth <= nLineLen
			  nChar := (INT) cMemo[nPos]
			  DO CASE
			  CASE nChar == TAB
				 nWhite := nPos
				 nWidth := GetTabPos( nWidth, nTabSize )
			  CASE nChar == BLANK
				 nWhite := nPos
				 nWidth ++
			  CASE IsCrLf( cMemo, nPos, nCrLf )
				 IF nCrLf == HARD_CR_LF
					lCont := FALSE
				 ELSE
					IF !lWrap
					   lCont := FALSE
					ENDIF
				 ENDIF
				 nPos ++
			  OTHERWISE
				 nWidth ++
			  END CASE
			  nPos ++
		   END DO
   
		   IF nWidth > nLineLen
			  IF lWrap
				 IF nWhite > 0
					nPos := nWhite + 1
				 ELSE
					nPos --
				 ENDIF
			  ELSE
				 nPos --
			  END IF
		   ENDIF
   
		   RETURN nPos - nStart
		
	STATIC METHOD MLine( cMemo AS STRING, nLineNum AS INT, nLineLen AS INT, nTabSize AS INT, lWrap AS LOGIC, lJustCheck AS LOGIC, dOffset REF INT ) AS STRING
		LOCAL oBuilder := NULL AS System.Text.StringBuilder
		LOCAL nTempLen AS INT
		LOCAL nSrc, nDes AS INT
		LOCAL nChar AS INT
		LOCAL nCrLf := 0 AS INT
		LOCAL cRet AS STRING
		LOCAL nIndex AS INT
		
		IF ! lJustCheck
			oBuilder := System.Text.StringBuilder{ (INT) nLineLen }
		END IF
		
		IF nLineNum > 0 .and. nLineLen > 0 .and. nLineLen <= MAX_WIDTH
		
			IF nTabSize > nLineLen
				nTabSize := nLineLen
			ENDIF
			nIndex :=  dOffset
			nLineNum --
			nTempLen := 1
			DO WHILE nLineNum != 0 .and. nTempLen != 0
				nTempLen := LineLen( cMemo, nIndex, nLineLen, nTabSize, lWrap )
				nIndex += nTempLen
				dOffset += nTempLen
				nLineNum --
			ENDDO
			
			IF nLineNum == 0 .and. nTempLen != 0
				nTempLen := LineLen( cMemo, nIndex, nLineLen, nTabSize, lWrap )
				nSrc := 0
				nDes := 0
				DO WHILE nSrc < nTempLen .and. nDes < nLineLen
					nChar := cMemo[ (INT) nSrc + nIndex]
					DO CASE
						CASE IsCrLf( cMemo, nSrc + nIndex, REF nCrLf )
							nSrc += 2
						CASE nChar == TAB
							LOCAL nTabPos AS INT
							nTabPos := GetTabPos( nDes, nTabSize )
							IF ! lJustCheck
								oBuilder:Append( ' ', (INT) nTabPos - nDes )
							END IF
							nDes := nTabPos
							nSrc ++
						OTHERWISE
							IF ! lJustCheck
								oBuilder:Append( (CHAR) nChar )
							END IF
							nSrc ++
							nDes ++
					END CASE
				ENDDO
			ENDIF
		ENDIF
		
		IF lJustCheck
			cRet := NULL
		ELSE
			IF oBuilder:Length < nLineLen
				oBuilder:Append( ' ',  (INT) nLineLen - oBuilder:Length )
			ENDIF
			cRet := oBuilder:ToString()
		ENDIF
		
		RETURN cRet
		END		 CLASS
		
		
	/// <summary>
	/// Extract a line of text from a string, specifying an optional offset argument.
	/// </summary>
	/// <param name="cMemo"></param>
	/// <param name="nLine"></param>
	/// <returns>
/// </returns>
FUNCTION MLine(cMemo AS STRING,nLineNum AS DWORD) AS STRING
	LOCAL nOffset := 0 AS DWORD
	RETURN MLine3(cMemo, nLineNum, REF nOffSet)
	
	
/// <summary>
/// Extract a line of text from a string, specifying an optional offset argument.
/// </summary>
/// <param name="cMemo"></param>
/// <param name="nLine"></param>
/// <param name="nOffset"></param>
/// <returns>
/// </returns>
FUNCTION MLine(cMemo AS STRING,nLineNum AS DWORD,nOffset REF DWORD) AS STRING
	RETURN MLine3(cMemo, nLineNum, REF nOffSet)
FUNCTION MLine(cMemo AS STRING,nLineNum AS DWORD,nOffset AS DWORD) AS STRING
	RETURN MLine3(cMemo, nLineNum, REF nOffSet)
	
	
/// <summary>
/// Extract a line of text from a string, specifying a required offset argument.
/// </summary>
/// <param name="cMemo"></param>
/// <param name="nLineNum"></param>
/// <param name="nOffSet"></param>
/// <returns>
/// </returns>
FUNCTION MLine3(cMemo AS STRING,nLineNum AS DWORD,nOffSet REF DWORD) AS STRING
	LOCAL cResult AS STRING
	LOCAL iOffSet := (INT) nOffSet as INT
	IF nOffSet < cMemo:Length
		cResult := Trim(MemoHelpers.MLine( cMemo , (int) nLineNum , MemoHelpers.STD_MEMO_WIDTH, MemoHelpers.STD_TAB_WIDTH, TRUE, FALSE, REF iOffSet ))
	ELSE
		cResult := ""
	ENDIF
	nOffSet := (DWORD) iOffSet
	RETURN cResult
	
	
/// <summary>
/// Extract a line of text from a string.
/// </summary>
/// <param name="cMemo"></param>
/// <param name="wWidth"></param>
/// <param name="wLineNum"></param> 
/// <param name="wTabSize"></param>
/// <param name="lWrap"></param>
/// <returns>
/// </returns>
FUNCTION MemoLine(cMemo AS STRING, nLineLen := MemoHelpers.STD_MEMO_WIDTH AS DWORD, nLineNum := 1 AS DWORD,;
nTabSize := MemoHelpers.STD_TAB_WIDTH AS DWORD,lWrap := TRUE AS LOGIC) AS STRING
	LOCAL dPos := 0 AS INT
	RETURN MemoHelpers.MLine(cMemo, (int) nLineNum, (int) nLineLen, (int) nTabSize, lWrap, FALSE, REF DPos)
	
/// <summary>
/// Return the contents of a text file as a string.
/// </summary>
/// <param name="cFile"></param>
/// <returns>
/// </returns>
FUNCTION MemoRead(cFile AS STRING) AS STRING
	LOCAL cResult AS STRING
	TRY
		IF File(cFile)
			cFile := FPathName()
			cResult := System.IO.File.ReadAllText(cFile)
		ELSE
			cResult := ""
		ENDIF
	CATCH
		cResult := ""
	END TRY
	RETURN cResult
	
	
	
/// <summary>
/// Write a string to a disk file.
/// </summary>
/// <param name="cFile"></param>
/// <param name="c"></param>
/// <returns>
/// </returns>
FUNCTION MemoWrit(cFile AS STRING,c AS STRING) AS LOGIC
	LOCAL lOk AS LOGIC
	TRY
		System.IO.File.WriteAllText(cFile, c)
		lOk := TRUE
	CATCH
		lOk := FALSE
	END TRY
	RETURN lOk
	
	
	
/// <summary>
/// Determine the position of a line in a string.
/// </summary>
/// <param name="cMemo"></param>
/// <param name="nLineNum"></param>
/// <returns>
/// </returns>
FUNCTION MLPos2(cMemo AS STRING,nLineNum AS DWORD) AS DWORD
	LOCAL nIndex := 0 AS INT
	MemoHelpers.MLine( cMemo, (int)  nLineNum, MemoHelpers.STD_MEMO_WIDTH, MemoHelpers.STD_TAB_WIDTH, TRUE, TRUE, REF nIndex )
	RETURN (DWORD) nIndex
	
	
	
	
	
/// <summary>
/// Count the number of lines in a string.
/// </summary>
/// <param name="cMemo"></param>
/// <returns>
/// </returns>
	
FUNCTION MLCount1( cMemo AS STRING) AS DWORD 
	RETURN MemoHelpers.MLCount(cMemo, MemoHelpers.STD_MEMO_WIDTH, MemoHelpers.STD_TAB_WIDTH , TRUE)
	
	
	
/// <summary>
/// Count the number of lines in a string or memo field.
/// </summary>
/// <param name="cMemo"></param>
/// <returns>
/// </returns>
FUNCTION MemLines(cMemo AS STRING) AS DWORD
	RETURN MemoHelpers.MLCount(cMemo, MemoHelpers.STD_MEMO_WIDTH, MemoHelpers.STD_TAB_WIDTH , TRUE)
	
	
FUNCTION _MPosToLc(cMemo AS STRING,nLineLen AS DWORD,nPos AS DWORD,nTabSize := MemoHelpers.STD_TAB_WIDTH AS DWORD,lWrap := TRUE AS LOGIC) AS Tuple<INT, INT>
	RETURN MemoHelpers.MPosToLc(cMemo, (int) nLineLen, (int) nPos, (int) nTabSize, lWrap)
	
FUNCTION MLcToPos( cMemo as STRING, nLineLen as DWORD, nLineNum as DWORD, nColumn as DWORD, nTabSize as DWORD, lWrap as LOGIC) AS DWORD CLIPPER
	RETURN MemoHelpers.MLcToPos(cMemo, (int) nLineLen, (int) nLineNum, (int) nColumn, (int) nTabSize, lWrap)
