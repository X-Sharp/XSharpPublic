STATIC DEFINE __WCFixedPitch 		:= 1
STATIC DEFINE __WCHeavyWeight 		:= 700
STATIC DEFINE __WCLightWeight 		:= 200
STATIC DEFINE __WCNormalWeight 		:= 400
STATIC DEFINE __WCVariablePitch 	:= 2

CLASS Font INHERIT VObject
	PROTECT hFont 			AS PTR
	PROTECT lStdFont 		AS LOGIC
	PROTECT iStdFontType AS INT
	PROTECT bFontChanged AS LOGIC
	PROTECT sFaceName 	AS STRING
	PROTECT lfHeight 		AS INT
	PROTECT lfWidth 		AS INT
	PROTECT lfEscapement AS WORD
	PROTECT lfOrientation AS WORD
	PROTECT lfWeight 		AS WORD
	PROTECT lfItalic 		AS BYTE
	PROTECT lfUnderline AS BYTE
	PROTECT lfStrikeOut AS BYTE
	PROTECT lfCharSet 	AS BYTE
	PROTECT lfOutPrecision AS BYTE
	PROTECT lfClipPrecision AS BYTE
	PROTECT lfQuality 	AS BYTE
	PROTECT lfPitchAndFamily AS BYTE
	PROTECT iPointSize 	AS INT

	//PP-030828 Strong typing
ACCESS __FontCharSet AS INT STRICT 
	//PP-030828 Strong typing
	RETURN lfCharSet

ACCESS __FontFaceName AS STRING STRICT 
	//PP-030828 Strong typing
	RETURN sFaceName

ACCESS __FontHeight AS INT STRICT 
	//PP-030828 Strong typing
	RETURN lfHeight

ACCESS __FontPitchAndFamily AS INT STRICT 
	//PP-030828 Strong typing
	RETURN lfPitchAndFamily

ACCESS __PointSize AS INT STRICT 
	//PP-030828 Strong typing
	RETURN iPointSize

ACCESS PointSize AS INT STRICT 
	//PP-030828 Strong typing
	RETURN iPointSize

ASSIGN __PointSize(x AS INT)  STRICT 
	//PP-030828 Strong typing
	RETURN iPointSize := x

ACCESS Bold 
	RETURN lfWeight == __WCHeavyWeight

ASSIGN Bold(lfState) 
	LOCAL lfNewWeight AS WORD

	IF lfState == TRUE
		lfNewWeight := __WCHeavyWeight
	ELSE
		lfNewWeight := __WCNormalWeight
	ENDIF

	IF lfWeight != lfNewWeight
		lfWeight := lfNewWeight
		bFontChanged := TRUE
	ENDIF

	RETURN 

ACCESS CharSet 
   RETURN SELF:lfCharSet

ACCESS ClipPrecision 
   RETURN SELF:lfClipPrecision

ASSIGN ClipPrecision(nNewClipPrecision) 
	// DHer: 18/12/2008
	IF SELF:lfClipPrecision<>nNewClipPrecision
		SELF:lfClipPrecision := nNewClipPrecision
		SELF:bFontChanged := TRUE
	ENDIF

   RETURN 


METHOD ConvPntToDim(nPntSize, hDCConv) 
	LOCAL hDC AS PTR
	LOCAL wDim AS Dimension

	DEFAULT(@hDCConv, NULL_PTR)
	IF (hDCConv == NULL_PTR) .AND. ((hDC := GetDC(NULL_PTR)) != NULL_PTR)
		wDim := Dimension{0, -(MulDiv(nPntSize, GetDeviceCaps(hDC, LOGPIXELSY), 72))}
		ReleaseDC(NULL_PTR, hDC)
	ELSEIF (hDCConv != NULL_PTR)
		wDim := Dimension{0, -(MulDiv(nPntSize, GetDeviceCaps(hDCConv, LOGPIXELSY), 72))}
	ELSE
		wDim := Dimension{nPntSize, nPntSize}
	ENDIF

	RETURN wDim

METHOD Create(lPrinter, hdc) 
	LOCAL logFont IS _winLOGFONT
	LOCAL i, len AS INT
	LOCAL oDim AS Dimension

	DEFAULT(@lPrinter, FALSE)
	DEFAULT(@hdc, NULL_PTR)

	IF (hFont != NULL_PTR) .AND. bFontChanged
		DeleteObject(hFont)
		hFont := NULL_PTR
	ENDIF

	IF (hFont == NULL_PTR)
		IF lPrinter .AND. (hdc != NULL_PTR)
			logFont:lfQuality := PROOF_QUALITY
			logFont:lfOutPrecision := OUT_DEVICE_PRECIS
		ELSE
			logFont:lfQuality := lfQuality
			logFont:lfOutPrecision := lfOutPrecision
		ENDIF

		IF (lfHeight == 0) .AND. (lfWidth == 0) .AND. (iPointSize > 0)
			oDim 		:= SELF:ConvPntToDim(iPointSize, hDC)
			lfHeight := oDim:Height
			lfWidth 	:= oDim:Width
		ENDIF

		logFont:lfHeight := lfHeight
		IF lPrinter .AND. (hdc != NULL_PTR)
			IF lStdFont
				logFont:lfHeight := -(((GetDeviceCaps(hdc, LOGPIXELSY) * __WCStdFontPointSize[iStdFontType + 1]) + 36) / 72)
			ELSEIF (iPointSize > 0)
				logFont:lfHeight := -MulDiv(iPointSize, GetDeviceCaps(hdc, LOGPIXELSY), 72)
			ENDIF
		ENDIF

		logFont:lfWidth := lfWidth
		logFont:lfEscapement := lfEscapement
		logFont:lfOrientation := lfOrientation
		logFont:lfWeight := lfWeight
		logFont:lfItalic := lfItalic
		logFont:lfUnderline := lfUnderline
		logFont:lfStrikeOut := lfStrikeOut
		logFont:lfCharSet := lfCharSet
		logFont:lfClipPrecision := lfClipPrecision
		logFont:lfPitchAndFamily := lfPitchAndFamily

		IF (NULL_STRING == sFaceName)
			logFont:lfFaceName[1] := 0
		ELSE
			len := INT(_CAST, SLen(sFaceName))
			FOR i := 1 UPTO len
				logFont:lfFaceName[i] := Asc(SubStr(sFaceName, i, 1))
			NEXT  // i
			logFont:lfFaceName[i] := 0
		ENDIF

		hFont := CreateFontIndirect(@logFont)
	ENDIF

	bFontChanged := FALSE
	RETURN SELF

METHOD Destroy() 
	
	SUPER:Destroy()

	IF (hFont != NULL_PTR)
		DeleteObject(hFont)
		hFont := NULL_PTR
	ENDIF

	RETURN NIL

ACCESS Escapement 
	// DHer: 18/12/2008
   RETURN SELF:lfEscapement

ACCESS FaceName 
   RETURN SELF:sFaceName

METHOD Handle() AS PTR
	IF (hFont == NULL_PTR)
		SELF:Create()
	ENDIF

	RETURN hFont

ACCESS Height 
   RETURN SELF:lfHeight

ASSIGN Height( liHeight ) 
   IF SELF:lfHeight != liHeight
	   SELF:bFontChanged := TRUE
	   SELF:lfHeight:= liHeight
   ENDIF
   RETURN 

CONSTRUCTOR(kFont, oDimension, sTypeFace) 
	SUPER()

	lfWeight := __WCNormalWeight

	IF __WCIsTrueTypeEnabled()
		IF __WCUseTrueTypeOnly()
			lfOutPrecision := OUT_TT_ONLY_PRECIS
		ELSE
			lfOutPrecision := OUT_TT_PRECIS
		ENDIF
	ELSE
		lfOutPrecision := OUT_RASTER_PRECIS
	ENDIF

	lfQuality 			:= DRAFT_QUALITY
	lfClipPrecision 	:= CLIP_STROKE_PRECIS
	lfCharSet 			:= ANSI_CHARSET
	lfPitchAndFamily 	:= 0 // _Or(DEFAULT_PITCH, FF_DONTCARE)

	IF IsNil(oDimension)
		lStdFont 	 := TRUE
		iStdFontType := kFont

		lfHeight := __WCGetStdFontScreenHeight(iStdFontType)
		lfWidth := 0

		lfPitchAndFamily := _OR(lfPitchAndFamily, WORD(_CAST, __WCStdFontFamily[iStdFontType + 1]))
	ELSE
		IF IsNumeric(oDimension)
			//oDimension := self:ConvPntToDim(oDimension)
			iPointSize := oDimension
		ELSE
			lfHeight := oDimension:Height
			lfWidth := oDimension:Width
		ENDIF

		DEFAULT(@kFont, FONTANY)

		IF !IsNil(sTypeFace)
			sFaceName := sTypeFace
			lfCharSet := DEFAULT_CHARSET
		ENDIF

		lfPitchAndFamily := _OR(lfPitchAndFamily, __WCConvertFont(kFont))

		IF kFont == FontScript
			lfOutPrecision := OUT_STROKE_PRECIS
			lfCharSet := DEFAULT_CHARSET
		ENDIF
	ENDIF

	bFontChanged := TRUE

	

	RETURN 

ACCESS Italic 
	RETURN lfItalic == 1

ASSIGN Italic(lfState) 
	IF lfItalic != lfState
		lfItalic := lfState
		bFontChanged := TRUE
	ENDIF

	RETURN 

ACCESS Light 
	RETURN lfWeight == __WCLightWeight

ASSIGN Light(lfState) 
	LOCAL lfNewWeight AS WORD

	IF lfState == TRUE
		lfNewWeight := __WCLightWeight
	ELSE
		lfNewWeight := __WCNormalWeight
	ENDIF

	IF lfWeight != lfNewWeight
		lfWeight := lfNewWeight
		bFontChanged := TRUE
	ENDIF

	RETURN 

ACCESS Normal 
	RETURN lfWeight == __WCNormalWeight

ASSIGN Normal(lfState) 
	LOCAL lfNewWeight AS WORD

	IF lfState == TRUE
		lfNewWeight := __WCNormalWeight
	ELSE
		lfNewWeight := __WCNormalWeight
	ENDIF

	IF lfWeight != lfNewWeight
		lfWeight := lfNewWeight
		bFontChanged := TRUE
	ENDIF

	RETURN 

ACCESS Orientation 
	// DHer: 18/12/2008
   RETURN SELF:lfOrientation

ACCESS OutPrecision 
   RETURN SELF:lfOutPrecision

ASSIGN OutPrecision(nNewOutPrecision) 
	// DHer: 18/12/2008
	IF SELF:lfOutPrecision<>nNewOutPrecision
		SELF:lfOutPrecision := nNewOutPrecision
		SELF:bFontChanged := TRUE
	ENDIF

   RETURN 
   
ACCESS PitchAndFamily 
   RETURN SELF:lfPitchAndFamily

ASSIGN Family(nFamily)
   LOCAL lfNewPitchAndFamily	AS BYTE
   LOCAL lfFamily				AS WORD

	   // DHer: 18/12/2008
	   lfFamily := nFamily
	   lfNewPitchAndFamily := _OR(_AND(SELF:lfPitchAndFamily,0x0F),lfFamily)

	   IF lfNewPitchAndFamily<>SELF:lfPitchAndFamily
		   SELF:lfPitchAndFamily := lfNewPitchAndFamily
		   SELF:bFontChanged := TRUE
	   ENDIF

   RETURN 


ACCESS @@PitchFixed 
	RETURN _AND(lfPitchAndFamily, 0x03) == 1

ASSIGN @@PitchFixed(lfState) 
	LOCAL lfNewPitchAndFamily AS BYTE

	IF lfState == TRUE
		lfNewPitchAndFamily := _OR(_AND(lfPitchAndFamily, 0xFC), __WCFixedPitch)
	ELSE
		lfNewPitchAndFamily := _OR(_AND(lfPitchAndFamily, 0xFC), __WCVariablePitch)
	ENDIF

	IF lfPitchAndFamily != lfNewPitchAndFamily
		lfPitchAndFamily := lfNewPitchAndFamily
		bFontChanged := TRUE
	ENDIF

	RETURN 

ACCESS @@PitchVariable 
	RETURN _AND(lfPitchAndFamily, 0x03) == 2

ASSIGN @@PitchVariable(lfState) 
	LOCAL lfNewPitchAndFamily AS BYTE

	IF lfState == TRUE
		lfNewPitchAndFamily := _OR(_AND(lfPitchAndFamily, 0xFC), __WCVariablePitch)
	ELSE
		lfNewPitchAndFamily := _OR(_AND(lfPitchAndFamily, 0xFC), __WCFixedPitch)
	ENDIF

	IF lfPitchAndFamily != lfNewPitchAndFamily
		lfPitchAndFamily := lfNewPitchAndFamily
		bFontChanged := TRUE
	ENDIF

	RETURN 

ACCESS Quality 
RETURN SELF:lfQuality

ACCESS Size 
RETURN SELF:iPointSize

METHOD SizeText(cString ) 
   LOCAL oDimension AS Dimension
   LOCAL oWinRect IS _WinRect
   LOCAL hDC AS PTR

	   // DHer: 18/12/2008
	   DEFAULT(@cString," ")
	   hDC := CreateDC(Cast2Psz("DISPLAY"),NULL_PSZ,NULL_PSZ,NULL_PTR)
	   SetRect(@oWinRect,0,0,0,0)
	   SelectObject(hDC,SELF:Handle())
	   DrawText(hDc,String2Psz(cString),-1,@oWinRect,_OR(DT_LEFT,DT_NOPREFIX,DT_EXPANDTABS,DT_CALCRECT))
	   DeleteDC(hDc)			
	   oDimension := Dimension{oWinRect:Right,oWinRect:bottom}

   RETURN oDimension


ACCESS Strikethru 
	RETURN lfStrikeOut == 1

ASSIGN Strikethru(lfState) 
	IF lfStrikeOut != lfState
		lfStrikeOut := lfState
		bFontChanged := TRUE
	ENDIF

	RETURN 

ACCESS Underline 
	RETURN lfUnderline == 1

ASSIGN Underline(lfState) 
	IF lfUnderline != lfState
		lfUnderline := lfState
		bFontChanged := TRUE
	ENDIF

	RETURN 

ACCESS Weight 
RETURN SELF:lfWeight

ACCESS Width 
RETURN SELF:lfWidth
END CLASS

STATIC FUNCTION __GetStdFontPointSize(kStandardFont AS INT) AS INT STRICT
	LOCAL iPointSize AS INT

	iPointSize := 10

	RETURN iPointSize

STATIC FUNCTION __WCConvertFont(family AS WORD) AS WORD STRICT
	LOCAL retVal AS WORD

	SWITCH family
	CASE FontDecorative
		retVal := FF_DECORATIVE
	CASE FontModern
		retVal := FF_MODERN
	CASE FontRoman
		retVal := FF_ROMAN
	CASE FontScript
		retVal := FF_SCRIPT
	CASE FontSwiss
		retVal := FF_SWISS
	OTHERWISE
		retVal := FF_DONTCARE
	END SWITCH

	RETURN retVal

STATIC FUNCTION __WCGetStdFontScreenHeight(kStandardFont AS INT) AS INT STRICT
	LOCAL hDC AS PTR
	LOCAL iHeight AS INT

	hdc := CreateIC(String2Psz("DISPLAY"), NULL_PSZ, NULL_PSZ, NULL_PTR)
	iHeight := -(((GetDeviceCaps(hdc, LOGPIXELSY) * __WCStdFontPointSize[kStandardFont + 1]) + 36) / 72)
	DeleteDC(hdc)

	RETURN iHeight
STATIC GLOBAL __WCStdFontFamily 	:= {FF_MODERN, FF_MODERN, FF_MODERN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_SWISS, FF_SWISS, FF_SWISS, FF_SWISS, FF_SWISS, FF_SWISS, FF_DONTCARE} AS ARRAY
STATIC GLOBAL __WCStdFontPointSize := {8, 10, 12, 8, 10, 12, 14, 18, 24, 8, 10, 12, 14, 18, 24, 8} AS ARRAY

