//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/memotran/*" />	
FUNCTION MemoTran(cTarget ,cReplaceHardCR ,cReplaceSoftCR ) AS STRING CLIPPER

   IF cReplaceHardCR == NIL
      cReplaceHardCR := ";"
   ENDIF

   IF cReplaceSoftCR == NIL
      cReplaceSoftCR := " "
   ENDIF

   IF cTarget == NIL .OR. ! cTarget:IsString
      THROW Error.ArgumentError( __ENTITY__, nameof(cTarget), 1, <OBJECT>{ cTarget } )
   ELSEIF ! cReplaceHardCR:IsString
      THROW Error.ArgumentError( __ENTITY__, nameof(cReplaceHardCR), 2, <OBJECT>{ cReplaceHardCR } )
   ELSEIF ! cReplaceSoftCR:IsString
      THROW Error.ArgumentError( __ENTITY__, nameof(cReplaceSoftCR), 3, <OBJECT>{ cReplaceSoftCR } )
   ENDIF
   LOCAL cSrc := cTarget AS STRING
    cSrc := cSrc:Replace( e"\r\n" , cReplaceHardCR )
	cSrc := cSrc:Replace( e"\0x8D\n" , cReplaceSoftCR)
	RETURN cSrc
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mlcount/*" />	
FUNCTION MLCount(cString ,nLineLength ,nTabsize ,lWrap ) AS DWORD CLIPPER
	IF ! cString:IsString
		THROW Error.DataTypeError( "MLCount", NAMEOF(cString), 1, <OBJECT>{ cString} )
	ENDIF
	
	IF nLineLength == NIL
		nLineLength := MemoHelpers.STD_MEMO_WIDTH
	ELSEIF !nLineLength:IsNumeric
		THROW Error.DataTypeError( "MLCount", NAMEOF(nLineLength), 2, <OBJECT>{ nLineLength } )
	ENDIF
	
	IF nTabSize == NIL
		nTabSize := MemoHelpers.STD_TAB_WIDTH
	ELSEIF !nTabSize:IsNumeric
		THROW Error.DataTypeError( "MLCount", NAMEOF(nTabSize), 3, <OBJECT>{ nTabSize } )
	ENDIF
	
	IF lWrap == NIL
		lWrap := TRUE
	ELSEIF !lWrap:IsLogic
		THROW Error.DataTypeError( "MLCount", NAMEOF(lWrap), 4, <OBJECT>{ lWrap } )
	ENDIF
	RETURN MemoHelpers.MLCount(cString, nLineLength, nTabsize, lWrap)
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mlctopos/*" />	
FUNCTION MLcToPos( cText, nWidth, nLine, nCol, nTabSize, lWrap ) AS DWORD CLIPPER
	
	IF ! cText:IsString
		THROW Error.DataTypeError( "MLcToPos", NAMEOF(cText), 1, <OBJECT>{ cText} )
	ENDIF
	
	IF nWidth == NIL
		nWidth := MemoHelpers.STD_MEMO_WIDTH
	ELSEIF !nWidth:IsNumeric
		THROW Error.DataTypeError( "MLcToPos", NAMEOF(nWidth), 2, <OBJECT>{ nWidth } )
	ENDIF
	
	IF nLine == NIL
		nLine := 1
	ELSEIF !nLine:IsNumeric
		THROW Error.DataTypeError( "MLcToPos", NAMEOF(nLine), 3, <OBJECT>{ nLine } )
	ENDIF
	
	IF nCol == NIL
		nCol := 0
	ELSEIF !nCol:IsNumeric
		THROW Error.DataTypeError( "MLcToPos", NAMEOF(nCol), 4, <OBJECT>{ nCol } )
	ENDIF
	
	IF nTabSize == NIL
		nTabSize := MemoHelpers.STD_TAB_WIDTH
	ELSEIF !nTabSize:IsNumeric
		THROW Error.DataTypeError( "MLcToPos", NAMEOF(nTabSize), 5, <OBJECT>{ nTabSize } )
	ENDIF
	
	IF lWrap == NIL
		lWrap := TRUE
	ELSEIF !lWrap:IsLogic
		THROW Error.DataTypeError( "MLcToPos", NAMEOF(lWrap), 6, <OBJECT>{ lWrap } )
	ENDIF
	RETURN MemoHelpers.MLcToPos(cText, nWidth, nLine, nCol, nTabSize, lWrap)   
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mlpos/*" />	
FUNCTION MLPos(cText ,nWidth ,nLine ,nTabSize ,lWrap ) AS DWORD CLIPPER
   
	IF ! cText:IsString
		THROW Error.DataTypeError( "MLPos", NAMEOF(cText), 1, <OBJECT>{ cText} )
	ENDIF
	
	IF nWidth == NIL
		nWidth := MemoHelpers.STD_MEMO_WIDTH
	ELSEIF !nWidth:IsNumeric
		THROW Error.DataTypeError( "MLPos", NAMEOF(nWidth), 2, <OBJECT>{ nWidth } )
	ENDIF
	
	IF nLine == NIL
		nLine := 1
	ELSEIF !nLine:IsNumeric
		THROW Error.DataTypeError( "MLPos", NAMEOF(nLine), 3, <OBJECT>{ nLine } )
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
    MemoHelpers.MLine( cText, nLine, nWidth, nTabSize, lWrap, TRUE, REF nIndex )
   
	RETURN (DWORD) nIndex

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mpostolc/*" />
FUNCTION MPosToLc(cText ,nWidth ,nPos ,nTabSize ,lWrap ) AS ARRAY CLIPPER
	IF !cText:IsString
		THROW Error.DataTypeError( "MPosToLc", NAMEOF(cText), 1, <OBJECT>{ cText } )
	ENDIF
	
	IF nWidth == NIL
		nWidth := MemoHelpers.STD_MEMO_WIDTH
	ELSEIF !nWidth:IsNumeric
		THROW  Error.DataTypeError( "MPosToLc", NAMEOF(nWidth), 2, <OBJECT>{ nWidth } )
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
	result := MemoHelpers.MPosToLc(cText, nWidth, nPos, nTabSize, lWrap)
	RETURN {result:Item1, result:Item2}
	
	
