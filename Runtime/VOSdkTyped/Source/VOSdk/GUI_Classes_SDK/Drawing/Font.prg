


#define __WCFixedPitch 1
#define __WCVariablePitch 2
#USING System.Runtime.InteropServices
CLASS Font INHERIT VObject
	PROTECT oFont 			AS System.Drawing.Font
	PROTECT oLogFont		AS Font.LOGFONT
	PROTECT lStdFont 		AS LOGIC
	PROTECT iStdFontType	AS INT
	PROTECT bFontChanged	AS LOGIC
	PROTECT iPointSize 		AS INT
	STATIC PROTECT aFontNames AS System.Collections.Generic.Dictionary<STRING,STRING>
	STATIC CONSTRUCTOR()
		aFontNames := System.Collections.Generic.Dictionary<STRING,STRING>{}
		

	ACCESS __Font AS System.Drawing.Font
		IF (oFont == NULL_OBJECT .or. SELF:bFontChanged)
			SELF:Create()
		ENDIF
		RETURN oFont

	ASSIGN __Font (oF AS System.Drawing.Font)
		oFont := oF
		SELF:__ToLogFont()

	METHOD __ToLogFont() AS VOID STRICT
		IF oFont != NULL_OBJECT
			oFont:ToLogFont(oLogFont)
			iPointSize := (INT) oFont:SizeInPoints
		ENDIF		


	ACCESS __FontCharSet AS INT STRICT 
		RETURN oLogFont:CharSet

	ACCESS __FontFaceName AS STRING STRICT 
		SELF:Create()
		RETURN oFont:Name

	ACCESS __FontHeight AS INT STRICT 
		SELF:Create()
		RETURN oLogFont:Height

	ACCESS __FontPitchAndFamily AS INT STRICT 
		SELF:Create()
		RETURN oLogFont:PitchAndFamily

	ACCESS __PointSize AS INT STRICT 
		RETURN iPointSize

	ACCESS PointSize AS INT STRICT 
		RETURN iPointSize

	ASSIGN __PointSize(x AS INT)  STRICT 
		iPointSize := x
		bFontChanged := TRUE
	
	ACCESS Bold AS LOGIC
		RETURN oLogFont:Weight == FontWeight.Heavy

	ASSIGN Bold(lfState AS LOGIC) 
		LOCAL lfNewWeight AS WORD

		IF lfState == TRUE
			lfNewWeight := (WORD) FontWeight.Heavy
		ELSE
			lfNewWeight := (WORD) FontWeight.Normal
		ENDIF

		IF oLogFont:Weight != lfNewWeight
			oLogFont:Weight := lfNewWeight
			bFontChanged := TRUE
		ENDIF

		RETURN 

	ACCESS CharSet  AS BYTE
		SELF:Create()
		RETURN oLogFont:CharSet

	ACCESS ClipPrecision AS BYTE
		RETURN oLogFont:ClipPrecision

	ASSIGN ClipPrecision(nNewClipPrecision AS BYTE) 
		IF oLogFont:ClipPrecision<>nNewClipPrecision
			oLogFont:ClipPrecision := nNewClipPrecision
			SELF:bFontChanged := TRUE
		ENDIF
		
		RETURN 

	METHOD ConvPntToDim(nPntSize AS LONG, hDCConv AS IntPtr) AS Dimension
		LOCAL hDC AS IntPtr
		LOCAL wDim AS Dimension

		IF (hDCConv == NULL_PTR) .AND. ((hDC := Win32.GetDC(NULL_PTR)) != NULL_PTR)
			wDim := Dimension{0, -(Win32.MulDiv(nPntSize, Win32.GetDeviceCaps(hDC, LOGPIXELSY), 72))}
			Win32.ReleaseDC(NULL_PTR, hDC)
		ELSEIF (hDCConv != NULL_PTR)
			wDim := Dimension{0, -(Win32.MulDiv(nPntSize, Win32.GetDeviceCaps(hDCConv, LOGPIXELSY), 72))}
		ELSE
			wDim := Dimension{nPntSize, nPntSize}
		ENDIF

		RETURN wDim

	METHOD Create(lPrinter, uhdc) 
		LOCAL oDim	  AS Dimension
		LOCAL hdc	  AS IntPtr
		DEFAULT(@lPrinter, FALSE)
		IF uhdc != NIL
			hdc := (IntPtr) uhdc
		ENDIF

		IF (oFont != NULL_OBJECT) .AND. bFontChanged
			oFont:Dispose()
			oFont := NULL_OBJECT
		ENDIF

		IF (oFont == NULL_OBJECT)
			
			IF oLogFont:Height == 0 .and. oLogFont:Height == 0 .and. iPointSize > 0
				oDim 			:= SELF:ConvPntToDim(iPointSize, hdc)
				oLogFont:Height	:= oDim:Height 
				oLogFont:Width 	:= oDim:Width
			ENDIF
			IF lPrinter .AND. (hdc != NULL_PTR)
				IF lStdFont
					oLogFont:Height := -(((Win32.GetDeviceCaps(hdc, LOGPIXELSY) * StdFontPointSize[iStdFontType + 1]) + 36) / 72)
				ELSEIF (iPointSize > 0)
					oLogFont:Height := -Win32.MulDiv(iPointSize, Win32.GetDeviceCaps(hdc, LOGPIXELSY), 72)
				ENDIF
			ENDIF
			
			// Check if we know the font name
			IF ! aFontNames:ContainsKey(oLogFont:FaceName)
				oFont := System.Drawing.Font{oLogFont:FaceName, iPointSize, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point}
				aFontNames:Add(oLogFont:FaceName, oFont:Name)
				oFont:Dispose()		
			ENDIF
			oLogFont:FaceName := aFontNames[oLogFont:FaceName]
			TRY
				//oLogFont:Height := (INT)  Convert.ToInt32(oLogFont:Height * 0.95)
				//oFont := System.Drawing.Font.FromLogFont(oLogFont)
				LOCAL nStyle AS System.Drawing.FontStyle
				IF oLogFont:Weight > FontWeight.Normal
					nStyle := System.Drawing.FontStyle.Bold
				ELSE
					nStyle := System.Drawing.FontStyle.Regular
				ENDIF
				IF oLogFont:Italic != 0
					nStyle |= System.Drawing.FontStyle.Italic
				ENDIF
				IF oLogFont:Underline != 0
					nStyle |= System.Drawing.FontStyle.Underline
				ENDIF
				IF oLogFont:StrikeOut != 0
					nStyle |= System.Drawing.FontStyle.Strikeout
				ENDIF
				oFont := System.Drawing.Font{oLogFont:FaceName, (REAL4)(iPointSize),  nStyle, System.Drawing.GraphicsUnit.Point}
				
			CATCH   AS Exception
				oFont := NULL_OBJECT
			END TRY
			IF oFont == NULL_OBJECT
				IF iPointSize == 0
					iPointSize := 8
				ENDIF
				oFont := System.Drawing.Font{oLogFont:FaceName, iPointSize, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point}
			ENDIF
			SELF:__ToLogFont()
		ENDIF

		bFontChanged := FALSE
		RETURN SELF
	

	METHOD Destroy() AS USUAL CLIPPER
		
		SUPER:Destroy()

		IF (oFont != NULL_OBJECT)
			oFont:Dispose()
			oFont := NULL_OBJECT
		ENDIF

		RETURN NIL

	ACCESS Escapement AS INT
		RETURN oLogFont:Escapement

	ACCESS FaceName AS STRING
		SELF:Create()
		RETURN oFont:Name

	METHOD Handle() AS IntPtr STRICT
		SELF:Create()
		RETURN oFont:ToHfont()

	ACCESS Height AS INT
		SELF:Create()
		RETURN oLogFont:Height
	

	ASSIGN Height( liHeight  AS INT) 
		IF oLogFont:Height != liHeight
			SELF:bFontChanged := TRUE
			oLogFont:Height:= liHeight
		ENDIF
		RETURN 

	CONSTRUCTOR(kFont, oDimension, sTypeFace) 
		SUPER()
		oLogFont := Font.LOGFONT{}
		IF ! IsNil(kFont)
			IF kFont:GetType() == typeof(System.Drawing.Font)
				oFont := kFont
				SELF:__ToLogFont()
				RETURN
			ENDIF
		ENDIF
		oLogFont:Weight := FontWeight.Normal

		//IF WC.IsTrueTypeEnabled()
		//	IF WC.UseTrueTypeOnly()
				oLogFont:OutPrecision := OUT_TT_ONLY_PRECIS
		//	ELSE
		//		oLogFont:OutPrecision := OUT_TT_PRECIS
		//	ENDIF
		//ELSE
		//	oLogFont:OutPrecision := OUT_RASTER_PRECIS
		//ENDIF

		oLogFont:Quality 			:= DRAFT_QUALITY
		oLogFont:ClipPrecision 		:= CLIP_STROKE_PRECIS
		oLogFont:CharSet 			:= ANSI_CHARSET
		oLogFont:PitchAndFamily 	:= 0 // _Or(DEFAULT_PITCH, FF_DONTCARE)

		IF IsNil(oDimension)
			lStdFont 	 := TRUE
			iStdFontType := kFont

			oLogFont:Height := GetStdFontScreenHeight(iStdFontType)
			oLogFont:Width := 0

			oLogFont:PitchAndFamily := (BYTE) _OR(oLogFont:PitchAndFamily, WORD(_CAST, StdFontFamily[iStdFontType + 1]))
		ELSE
			IF IsNumeric(oDimension)
				iPointSize := oDimension
			ELSE
				oLogFont:Height := oDimension:Height
				oLogFont:Width := oDimension:Width
			ENDIF

			DEFAULT(@kFont, FONTANY)

			IF !IsNil(sTypeFace)
				oLogFont:FaceName := sTypeFace
				oLogFont:CharSet := DEFAULT_CHARSET
			ENDIF

			oLogFont:PitchAndFamily := (BYTE) _OR(oLogFont:PitchAndFamily, ConvertFont(kFont))

			IF kFont == FontScript
				oLogFont:OutPrecision := OUT_STROKE_PRECIS
				oLogFont:CharSet := DEFAULT_CHARSET
			ENDIF
		ENDIF

		bFontChanged := TRUE
		RETURN 

	ACCESS Italic  AS LOGIC
		RETURN oLogFont:Italic == 1

	ASSIGN Italic(lfState AS LOGIC) 
		IF oLogFont:Italic != iif(lfState,1,0)
			oLogFont:Italic := iif(lfState,1,0)
			bFontChanged := TRUE
		ENDIF

		RETURN 

	ACCESS Light  AS LOGIC
		RETURN oLogFont:Weight == FontWeight.Light

	ASSIGN Light(lfState AS LOGIC) 
		LOCAL lfNewWeight AS WORD

		IF lfState == TRUE
			lfNewWeight := (WORD) FontWeight.Light
		ELSE
			lfNewWeight := (WORD) FontWeight.Normal
		ENDIF

		IF oLogFont:Weight != lfNewWeight
			oLogFont:Weight := lfNewWeight
			bFontChanged := TRUE
		ENDIF

		RETURN 

	ACCESS Normal  AS LOGIC
		RETURN oLogFont:Weight == FontWeight.Normal

	ASSIGN Normal(lfState AS LOGIC) 
		LOCAL lfNewWeight AS WORD

		IF lfState == TRUE
			lfNewWeight := (WORD) FontWeight.Normal
		ELSE
			lfNewWeight := (WORD) FontWeight.Heavy
		ENDIF
		IF oLogFont:Weight != lfNewWeight
			oLogFont:Weight := lfNewWeight
			bFontChanged := TRUE
		ENDIF

		RETURN 

	ACCESS Orientation AS LONG
		RETURN oLogFont:Orientation

	ACCESS OutPrecision  AS BYTE
		RETURN oLogFont:OutPrecision

	ASSIGN OutPrecision(nNewOutPrecision AS BYTE) 
		IF oLogFont:OutPrecision<>nNewOutPrecision
			oLogFont:OutPrecision := nNewOutPrecision
			SELF:bFontChanged := TRUE
		ENDIF

		RETURN 
	
	ACCESS PitchAndFamily AS BYTE
		RETURN oLogFont:PitchAndFamily

	ASSIGN Family(nFamily AS BYTE)
		LOCAL lfNewPitchAndFamily	AS BYTE
		LOCAL lfFamily				AS WORD

		lfFamily := nFamily
		lfNewPitchAndFamily := (BYTE) _OR(_AND(oLogFont:PitchAndFamily,0X0F),lfFamily)

		IF lfNewPitchAndFamily <> oLogFont:PitchAndFamily
			oLogFont:PitchAndFamily := lfNewPitchAndFamily
			SELF:bFontChanged := TRUE
		ENDIF

		RETURN 


	ACCESS @@PitchFixed  AS LOGIC
		RETURN !SELF:@@PitchVariable

	ASSIGN @@PitchFixed(lfState AS LOGIC) 
		SELF:@@PitchVariable := ! lfState
		RETURN 

	ACCESS @@PitchVariable AS LOGIC
		RETURN _AND(oLogFont:PitchAndFamily, 0x03) == 2

	ASSIGN @@PitchVariable(lfState AS LOGIC) 
		LOCAL lfNewPitchAndFamily AS BYTE

		IF lfState == TRUE
			lfNewPitchAndFamily := (BYTE) _OR(_AND(oLogFont:PitchAndFamily, 0xFC), __WCVariablePitch)
		ELSE
			lfNewPitchAndFamily := (BYTE) _OR(_AND(oLogFont:PitchAndFamily, 0xFC), __WCFixedPitch)
		ENDIF

		IF oLogFont:PitchAndFamily != lfNewPitchAndFamily
			oLogFont:PitchAndFamily := lfNewPitchAndFamily
			bFontChanged := TRUE
		ENDIF

		RETURN 

	ACCESS Quality AS BYTE
		RETURN oLogFont:Quality

	ACCESS Size AS LONG
		RETURN SELF:iPointSize

	METHOD SizeText(cString AS STRING)  AS Dimension 
		LOCAL oSize AS System.Drawing.Size
		LOCAL oDim AS Dimension
		oSize := System.Windows.Forms.TextRenderer.MeasureText(cString, oFont)
		oDim := oSize
		RETURN oDim


	ACCESS Strikethru  AS LOGIC
		RETURN oLogFont:StrikeOut == 1

	ASSIGN Strikethru(lfState AS LOGIC) 
		IF oLogFont:StrikeOut != iif(lfState,1,0)
			oLogFont:StrikeOut := iif(lfState,1,0)
			bFontChanged := TRUE
		ENDIF
		RETURN 

	ACCESS Underline AS LOGIC
		RETURN oLogFont:Underline == 1

	ASSIGN Underline(lfState AS LOGIC) 
		IF oLogFont:Underline != iif(lfState,1,0)
			oLogFont:Underline := iif(lfState,1,0)
			bFontChanged := TRUE
		ENDIF

		RETURN 

	ACCESS Weight AS LONG
		RETURN oLogFont:Weight

	ACCESS Width  AS LONG
		RETURN oLogFont:Width
	
	[StructLayout(LayoutKind.Sequential, CharSet:=System.Runtime.InteropServices.CharSet.Auto)];
	PUBLIC class LOGFONT
		EXPORT Height := 0         AS INT
		EXPORT Width := 0          AS INT
		EXPORT Escapement := 0     AS INT
		EXPORT Orientation := 0    AS INT
		EXPORT Weight := 0         AS INT
		EXPORT Italic := 0         AS BYTE
		EXPORT Underline := 0      AS BYTE
		EXPORT StrikeOut := 0      AS BYTE
		EXPORT CharSet := 0        AS BYTE
		EXPORT OutPrecision := 0   AS BYTE
		EXPORT ClipPrecision := 0  AS BYTE
		EXPORT Quality := 0        AS BYTE
		EXPORT PitchAndFamily := 0 AS BYTE
		[MarshalAs(UnmanagedType.ByValTStr, SizeConst:=32)];
		EXPORT FaceName := ""      AS STRING

	END CLASS
	
	OPERATOR IMPLICIT ( f AS System.Drawing.Font) AS Font
		RETURN Font{f}

	OPERATOR IMPLICIT ( f AS Font ) AS System.Drawing.Font
		RETURN f:__Font
	
	STATIC METHOD ConvertFont(family AS WORD) AS WORD STRICT
		LOCAL retVal AS WORD

		DO CASE
		CASE family == FontDecorative
			retVal := FF_DECORATIVE
		CASE family == FontModern
			retVal := FF_MODERN
		CASE family == FontRoman
			retVal := FF_ROMAN
		CASE family == FontScript
			retVal := FF_SCRIPT
		CASE family == FontSwiss
			retVal := FF_SWISS
		OTHERWISE
			retVal := FF_DONTCARE
		ENDCASE

		RETURN retVal
	
	STATIC METHOD GetStdFontScreenHeight(kStandardFont AS INT) AS INT STRICT
		LOCAL hDC AS PTR
		LOCAL iHeight AS INT

		hDC		:= Win32.CreateIC("DISPLAY", NULL_STRING, NULL_STRING, IntPtr.Zero)
		iHeight := -(((Win32.GetDeviceCaps(hDC, LOGPIXELSY) * StdFontPointSize[kStandardFont + 1]) + 36) / 72)
		Win32.DeleteDC(hDC)

		RETURN iHeight
	STATIC INITONLY PROTECT StdFontFamily 	:= {FF_MODERN, FF_MODERN, FF_MODERN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_ROMAN, FF_SWISS, FF_SWISS, FF_SWISS, FF_SWISS, FF_SWISS, FF_SWISS, FF_DONTCARE} AS ARRAY
	STATIC INITONLY PROTECT StdFontPointSize := {8, 10, 12, 8, 10, 12, 14, 18, 24, 8, 10, 12, 14, 18, 24, 8} AS ARRAY


	#region Extensions
	ACCESS Punkte AS INT
		RETURN iPointsize

	ASSIGN Punkte( uValue  AS INT) 
		iPointsize := uValue
	
	#endregion
	ENUM FontWeight
		MEMBER Heavy := 700
		MEMBER Light := 200
		MEMBER Normal := 400

	END ENUM
END CLASS




