STATIC DEFINE __WCFixedPitch 		:= 1
STATIC DEFINE __WCHeavyWeight 		:= 700
STATIC DEFINE __WCLightWeight 		:= 200
STATIC DEFINE __WCNormalWeight 		:= 400
STATIC DEFINE __WCVariablePitch 	:= 2


/// <include file="Gui.xml" path="doc/Font/*" />
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
 /// <exclude />
ACCESS __FontCharSet AS BYTE STRICT
	//PP-030828 Strong typing
	RETURN lfCharSet


 /// <exclude />
ACCESS __FontFaceName AS STRING STRICT
	//PP-030828 Strong typing
	RETURN sFaceName


 /// <exclude />
ACCESS __FontHeight AS INT STRICT
	//PP-030828 Strong typing
	RETURN lfHeight


 /// <exclude />
ACCESS __FontPitchAndFamily AS BYTE STRICT
	//PP-030828 Strong typing
	RETURN lfPitchAndFamily


 /// <exclude />
ACCESS __PointSize AS INT STRICT
	//PP-030828 Strong typing
	RETURN iPointSize


/// <include file="Gui.xml" path="doc/Font.PointSize/*" />
ACCESS PointSize AS INT STRICT
	//PP-030828 Strong typing
	RETURN iPointSize


 /// <exclude />
ASSIGN __PointSize(x AS INT)  STRICT
	//PP-030828 Strong typing
	RETURN iPointSize := x


/// <include file="Gui.xml" path="doc/Font.Bold/*" />
ACCESS Bold
	RETURN lfWeight == __WCHeavyWeight


/// <include file="Gui.xml" path="doc/Font.Bold/*" />
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


/// <include file="Gui.xml" path="doc/Font.CharSet/*" />
ACCESS CharSet
   RETURN SELF:lfCharSet


/// <include file="Gui.xml" path="doc/Font.ClipPrecision/*" />
ACCESS ClipPrecision
   RETURN SELF:lfClipPrecision


/// <include file="Gui.xml" path="doc/Font.ClipPrecision/*" />
ASSIGN ClipPrecision(nNewClipPrecision)
	// DHer: 18/12/2008
	IF SELF:lfClipPrecision<>nNewClipPrecision
		SELF:lfClipPrecision := nNewClipPrecision
		SELF:bFontChanged := TRUE
	ENDIF


   RETURN




/// <include file="Gui.xml" path="doc/Font.ConvPntToDim/*" />
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


/// <include file="Gui.xml" path="doc/Font.Create/*" />
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
				logFont:lfFaceName[i] := (BYTE) Asc(SubStr(sFaceName, i, 1))
			NEXT  // i
			logFont:lfFaceName[i] := 0
		ENDIF


		hFont := CreateFontIndirect(@logFont)
	ENDIF


	bFontChanged := FALSE
	RETURN SELF


/// <include file="Gui.xml" path="doc/Font.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER


	SUPER:Destroy()


	IF (hFont != NULL_PTR)
		DeleteObject(hFont)
		hFont := NULL_PTR
	ENDIF


	RETURN NIL


/// <include file="Gui.xml" path="doc/Font.Escapement/*" />
ACCESS Escapement
	// DHer: 18/12/2008
   RETURN SELF:lfEscapement


/// <include file="Gui.xml" path="doc/Font.FaceName/*" />
ACCESS FaceName
   RETURN SELF:sFaceName


/// <include file="Gui.xml" path="doc/Font.Handle/*" />
METHOD Handle() AS PTR
	IF (hFont == NULL_PTR)
		SELF:Create()
	ENDIF


	RETURN hFont


/// <include file="Gui.xml" path="doc/Font.Height/*" />
ACCESS Height
   RETURN SELF:lfHeight


/// <include file="Gui.xml" path="doc/Font.Height/*" />
ASSIGN Height( liHeight )
   IF SELF:lfHeight != liHeight
	   SELF:bFontChanged := TRUE
	   SELF:lfHeight:= liHeight
   ENDIF
   RETURN


/// <include file="Gui.xml" path="doc/Font.ctor/*" />
CONSTRUCTOR(kFont, oDimension, sTypeFace)
	SUPER()


	lfWeight := __WCNormalWeight


	lfOutPrecision      := OUT_TT_PRECIS


	lfQuality 			:= DRAFT_QUALITY
	lfClipPrecision 	:= CLIP_STROKE_PRECIS
	lfCharSet 			:= ANSI_CHARSET
	lfPitchAndFamily 	:= 0 // _Or(DEFAULT_PITCH, FF_DONTCARE)


	IF IsNil(oDimension)
		lStdFont 	 := TRUE
		iStdFontType := kFont


		lfHeight := __WCGetStdFontScreenHeight(iStdFontType)
		lfWidth := 0


		lfPitchAndFamily := (BYTE) _OR(lfPitchAndFamily, (WORD) __WCStdFontFamily[iStdFontType + 1])
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


		lfPitchAndFamily := (BYTE) _OR(lfPitchAndFamily, __WCConvertFont(kFont))


		IF kFont == FontScript
			lfOutPrecision := OUT_STROKE_PRECIS
			lfCharSet := DEFAULT_CHARSET
		ENDIF
	ENDIF


	bFontChanged := TRUE






	RETURN


/// <include file="Gui.xml" path="doc/Font.Italic/*" />
ACCESS Italic
	RETURN lfItalic == 1


/// <include file="Gui.xml" path="doc/Font.Italic/*" />
ASSIGN Italic(lfState)
	IF lfItalic != lfState
		lfItalic := lfState
		bFontChanged := TRUE
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Font.Light/*" />
ACCESS Light
	RETURN lfWeight == __WCLightWeight


/// <include file="Gui.xml" path="doc/Font.Light/*" />
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


/// <include file="Gui.xml" path="doc/Font.Normal/*" />
ACCESS Normal
	RETURN lfWeight == __WCNormalWeight


/// <include file="Gui.xml" path="doc/Font.Normal/*" />
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


/// <include file="Gui.xml" path="doc/Font.Orientation/*" />
ACCESS Orientation
	// DHer: 18/12/2008
   RETURN SELF:lfOrientation


/// <include file="Gui.xml" path="doc/Font.OutPrecision/*" />
ACCESS OutPrecision
   RETURN SELF:lfOutPrecision


/// <include file="Gui.xml" path="doc/Font.OutPrecision/*" />
ASSIGN OutPrecision(nNewOutPrecision)
	// DHer: 18/12/2008
	IF SELF:lfOutPrecision<>nNewOutPrecision
		SELF:lfOutPrecision := nNewOutPrecision
		SELF:bFontChanged := TRUE
	ENDIF


   RETURN


/// <include file="Gui.xml" path="doc/Font.PitchAndFamily/*" />
ACCESS PitchAndFamily
   RETURN SELF:lfPitchAndFamily


/// <include file="Gui.xml" path="doc/Font.Family/*" />
ASSIGN Family(nFamily)
   LOCAL lfNewPitchAndFamily	AS BYTE
   LOCAL lfFamily				AS WORD


	   // DHer: 18/12/2008
	   lfFamily := nFamily
	   lfNewPitchAndFamily := (BYTE) _OR(_AND(SELF:lfPitchAndFamily,0x0F),lfFamily)


	   IF lfNewPitchAndFamily<>SELF:lfPitchAndFamily
		   SELF:lfPitchAndFamily := lfNewPitchAndFamily
		   SELF:bFontChanged := TRUE
	   ENDIF


   RETURN




/// <include file="Gui.xml" path="doc/Font.PitchFixed/*" />
ACCESS PitchFixed
	RETURN _AND(lfPitchAndFamily, 0x03) == 1


/// <include file="Gui.xml" path="doc/Font.PitchFixed/*" />
ASSIGN PitchFixed(lfState)
	LOCAL lfNewPitchAndFamily AS BYTE


	IF lfState == TRUE
		lfNewPitchAndFamily := (BYTE) _OR(_AND(lfPitchAndFamily, 0xFC), __WCFixedPitch)
	ELSE
		lfNewPitchAndFamily := (BYTE) _OR(_AND(lfPitchAndFamily, 0xFC), __WCVariablePitch)
	ENDIF


	IF lfPitchAndFamily != lfNewPitchAndFamily
		lfPitchAndFamily := lfNewPitchAndFamily
		bFontChanged := TRUE
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Font.PitchVariable/*" />
ACCESS PitchVariable
	RETURN _AND(lfPitchAndFamily, 0x03) == 2


/// <include file="Gui.xml" path="doc/Font.PitchVariable/*" />
ASSIGN PitchVariable(lfState)
	LOCAL lfNewPitchAndFamily AS BYTE


	IF lfState == TRUE
		lfNewPitchAndFamily := (BYTE) _OR(_AND(lfPitchAndFamily, 0xFC), __WCVariablePitch)
	ELSE
		lfNewPitchAndFamily := (BYTE) _OR(_AND(lfPitchAndFamily, 0xFC), __WCFixedPitch)
	ENDIF


	IF lfPitchAndFamily != lfNewPitchAndFamily
		lfPitchAndFamily := lfNewPitchAndFamily
		bFontChanged := TRUE
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Font.Quality/*" />
ACCESS Quality
RETURN SELF:lfQuality


/// <include file="Gui.xml" path="doc/Font.Size/*" />
ACCESS Size
RETURN SELF:iPointSize


/// <include file="Gui.xml" path="doc/Font.SizeText/*" />
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




/// <include file="Gui.xml" path="doc/Font.Strikethru/*" />
ACCESS Strikethru
	RETURN lfStrikeOut == 1


/// <include file="Gui.xml" path="doc/Font.Strikethru/*" />
ASSIGN Strikethru(lfState)
	IF lfStrikeOut != lfState
		lfStrikeOut := lfState
		bFontChanged := TRUE
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Font.Underline/*" />
ACCESS Underline
	RETURN lfUnderline == 1


/// <include file="Gui.xml" path="doc/Font.Underline/*" />
ASSIGN Underline(lfState)
	IF lfUnderline != lfState
		lfUnderline := lfState
		bFontChanged := TRUE
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Font.Weight/*" />
ACCESS Weight
RETURN SELF:lfWeight


/// <include file="Gui.xml" path="doc/Font.Width/*" />
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


