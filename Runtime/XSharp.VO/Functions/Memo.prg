//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <summary>
/// Replace carriage-return/linefeeds with a character that can be displayed.
/// </summary>
/// <param name="cSource">The string that contains the carriage-return/linefeed. </param>
/// <param name="cReplaceHardCR">The character to replace a hard carriage-return/linefeed pair with.  The default is a semicolon (;).</param>
/// <param name="cReplaceSoftCR">The character to replace a soft carriage-return/linefeed pair with.  The default is a space.</param>
/// <returns>A copy of cString with the specified carriage-return/linefeed pairs replaced.</returns>
FUNCTION MemoTran(cSource ,cReplaceHardCR ,cReplaceSoftCR ) AS STRING CLIPPER

   IF cReplaceHardCR == NIL
      cReplaceHardCR := ";"
   ENDIF

   IF cReplaceSoftCR == NIL
      cReplaceSoftCR := " "
   ENDIF

   IF cSource == NIL .OR. ! cSource:IsString
      THROW Error.ArgumentError( __ENTITY__, "cSource", 1, <OBJECT>{ cSource } )
   ELSEIF ! cReplaceHardCR:IsString
      THROW Error.ArgumentError( __ENTITY__, "cReplaceHardCR", 2, <OBJECT>{ cReplaceHardCR } )
   ELSEIF ! cReplaceSoftCR:IsString
      THROW Error.ArgumentError( __ENTITY__, "cReplaceSoftCR", 3, <OBJECT>{ cReplaceSoftCR } )
   ENDIF
   LOCAL cSrc := cSource AS STRING
    cSrc := cSrc:Replace( e"\r\n" , cReplaceHardCR )
	cSrc := cSrc:Replace( e"\0x8D\n" , cReplaceSoftCR)
	RETURN cSrc
	
	/// <summary>
	/// Count the number of lines in a string.
	/// </summary>
	/// <param name="cMemo"></param>
	/// <param name="nWidth"></param>
	/// <param name="nTabsize"></param>
	/// <param name="lWrap"></param>
	/// <returns>
	/// </returns>
FUNCTION MLCount(cMemo ,nWidth ,nTabsize ,lWrap ) AS DWORD CLIPPER
	IF ! cMemo:IsString
		THROW Error.DataTypeError( "MLCount", NAMEOF(cMemo), 1, <OBJECT>{ cMemo} )
	ENDIF
	
	IF nWidth == NIL
		nWidth := MemoHelpers.STD_MEMO_WIDTH
	ELSEIF !nWidth:IsNumeric
		BREAK Error.DataTypeError( "MLCount", NAMEOF(nWidth), 2, <OBJECT>{ nWidth } )
	ENDIF
	
	IF nTabSize == NIL
		nTabSize := MemoHelpers.STD_TAB_WIDTH
	ELSEIF !nTabSize:IsNumeric
		BREAK Error.DataTypeError( "MLCount", NAMEOF(nTabSize), 3, <OBJECT>{ nTabSize } )
	ENDIF
	
	IF lWrap == NIL
		lWrap := TRUE
	ELSEIF !lWrap:IsLogic
		BREAK Error.DataTypeError( "MLCount", NAMEOF(lWrap), 4, <OBJECT>{ lWrap } )
	ENDIF
	RETURN MemoHelpers.MLCount(cMemo, nWidth, nTabsize, lWrap)
	
	
	
	/// <summary>
	/// Return the position of a character in a formatted string.
	/// </summary>
	/// <param name="cMemo"></param>
	/// <param name="nWidth"></param>
	/// <param name="nLineNum"></param>
	/// <param name="nCol"></param>
	/// <param name="nTabSize"></param>
	/// <param name="lWrap"></param>
	/// <returns>
	/// </returns>
FUNCTION MLcToPos( cMemo, nLineLen, nLineNum, nColumn, nTabSize, lWrap ) AS DWORD CLIPPER
	
	IF ! cMemo:IsString
		THROW Error.DataTypeError( "MLcToPos", NAMEOF(cMemo), 1, <OBJECT>{ cMemo} )
	ENDIF
	
	IF nLineLen == NIL
		nLineLen := MemoHelpers.STD_MEMO_WIDTH
	ELSEIF !nLineLen:IsNumeric
		BREAK Error.DataTypeError( "MLcToPos", NAMEOF(nLineLen), 2, <OBJECT>{ nLineLen } )
	ENDIF
	
	IF nLineNum == NIL
		nLineNum := 1
	ELSEIF !nLineNum:IsNumeric
		BREAK Error.DataTypeError( "MLcToPos", NAMEOF(nLineNum), 3, <OBJECT>{ nLineNum } )
	ENDIF
	
	IF nColumn == NIL
		nColumn := 0
	ELSEIF !nColumn:IsNumeric
		BREAK Error.DataTypeError( "MLcToPos", NAMEOF(nColumn), 4, <OBJECT>{ nColumn } )
	ENDIF
	
	IF nTabSize == NIL
		nTabSize := MemoHelpers.STD_TAB_WIDTH
	ELSEIF !nTabSize:IsNumeric
		BREAK Error.DataTypeError( "MLcToPos", NAMEOF(nTabSize), 5, <OBJECT>{ nTabSize } )
	ENDIF
	
	IF lWrap == NIL
		lWrap := TRUE
	ELSEIF !lWrap:IsLogic
		BREAK Error.DataTypeError( "MLcToPos", NAMEOF(lWrap), 6, <OBJECT>{ lWrap } )
	ENDIF
	RETURN MemoHelpers.MLcToPos(cMemo, nLineLen, nLineNum, nColumn, nTabSize, lWrap)   
	
	
	
	/// <summary>
	/// Determine the position of a line in a string.
	/// </summary>
	/// <param name="cMemo"></param>
	/// <param name="nLineLen"></param>
	/// <param name="nLineNum"></param>
	/// <param name="nTabSize"></param>
	/// <param name="lWrap"></param>
	/// <returns>
	/// </returns>
FUNCTION MLPos(cMemo ,nLineLen ,nLineNum ,nTabSize ,lWrap ) AS DWORD CLIPPER
   
	IF ! cMemo:IsString
		THROW Error.DataTypeError( "MLPos", NAMEOF(cMemo), 1, <OBJECT>{ cMemo} )
	ENDIF
	
	IF nLineLen == NIL
		nLineLen := MemoHelpers.STD_MEMO_WIDTH
	ELSEIF !nLineLen:IsNumeric
		THROW Error.DataTypeError( "MLPos", NAMEOF(nLineLen), 2, <OBJECT>{ nLineLen } )
	ENDIF
	
	IF nLineNum == NIL
		nLineNum := 1
	ELSEIF !nLineNum:IsNumeric
		THROW Error.DataTypeError( "MLPos", NAMEOF(nLineNum), 3, <OBJECT>{ nLineNum } )
	ENDIF
	
	IF nTabSize == NIL
		nTabSize := MemoHelpers.STD_TAB_WIDTH
	ELSEIF !nTabSize:IsNumeric
		THROW Error.DataTypeError( "MLPos", NAMEOF(nTabSize), 4, <OBJECT>{ nTabSize } )
	ENDIF
	
	IF lWrap == NIL
		lWrap := TRUE
	ELSEIF !lWrap:IsLogic
		THROW Error.DataTypeError( "MLPos", NAMEOF(lWrap), 5, <OBJECT>{ lWrap } )
	ENDIF
	LOCAL nIndex AS INT
	nIndex := 0
    MemoHelpers.MLine( cMemo, nLineNum, nLineLen, nTabSize, lWrap, TRUE, REF nIndex )
   
	RETURN (DWORD) nIndex
	/// <summary>
	/// Return the line and column position of a character in a formatted string.
	/// </summary>
	/// <param name="cMemo"></param>
	/// <param name="nWidth"></param>
	/// <param name="nPos"></param>
	/// <param name="nTabSize"></param>
	/// <param name="lWrap"></param>
	/// <returns>
	/// </returns>
FUNCTION MPosToLc(cMemo ,nLineLen ,nPos ,nTabSize ,lWrap ) AS ARRAY CLIPPER
	IF !cMemo:IsString
		THROW Error.DataTypeError( "MPosToLc", NAMEOF(cMemo), 1, <OBJECT>{ cMemo } )
	ENDIF
	
	IF nLineLen == NIL
		nLineLen := MemoHelpers.STD_MEMO_WIDTH
	ELSEIF !nLineLen:IsNumeric
		THROW  Error.DataTypeError( "MPosToLc", NAMEOF(nLineLen), 2, <OBJECT>{ nLineLen } )
	ENDIF
	
	IF nPos == NIL
		nPos := 0
	ELSEIF !nPos:IsNumeric
		THROW  Error.DataTypeError( "MPosToLc", NAMEOF(nPos), 3, <OBJECT>{ nPos } )
	ENDIF
	
	IF nTabSize == NIL
		nTabSize := MemoHelpers.STD_TAB_WIDTH
	ELSEIF !nTabSize:IsNumeric
		THROW Error.DataTypeError( "MPosToLc", NAMEOF(nTabSize), 4, <OBJECT>{ nTabSize } )
	ENDIF
	
	IF lWrap == NIL
		lWrap := TRUE
	ELSEIF !lWrap:IsLogic
		THROW Error.DataTypeError( "MPosToLc", NAMEOF(lWrap), 5, <OBJECT>{ lWrap } )
	ENDIF
	LOCAL result AS Tuple<INT, INT>
	result := MemoHelpers.MPosToLc(cMemo, nLineLen, nPos, nTabSize, lWrap)
	RETURN {result:Item1, result:Item2}
	
	
