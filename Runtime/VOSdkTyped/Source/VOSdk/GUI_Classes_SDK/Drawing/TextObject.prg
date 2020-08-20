

USING VOSDK := XSharp.VO.SDK
CLASS TextObject INHERIT DrawObject
	PROTECT oFont  AS Font
	PROTECT oColor AS Color
	PROTECT cText  AS STRING


	CONSTRUCTOR(oPoint AS Point, cText := NULL_STRING AS STRING, oFont := NULL_OBJECT AS Font, oColor := NULL_OBJECT AS Color) 
		LOCAL dwColor AS DWORD
		SUPER(oPoint)

		IF cText != NULL_STRING
			SELF:cText := cText
		ENDIF
		IF oFont != NULL_OBJECT
			SELF:oFont:=oFont
		ELSE
			SELF:oFont:=Font{FontSystem8}
		ENDIF

		IF oColor != NULL_OBJECT
			SELF:oColor := oColor
		ELSE
			dwColor	:=	GuiWin32.GetSysColor(Color_WindowText)
			SELF:oColor := Color{dwColor}
		ENDIF

		RETURN 

	ACCESS BoundingBox  AS BoundingBox
		LOCAL oOldFont AS Font
		LOCAL oDim AS Dimension
		IF (oWnd != NULL_OBJECT)
			oOldFont := oWnd:Font
			oWnd:Font := oFont
			oDim := oWnd:SizeText(cText)
			oWnd:Font := oOldFont
		ENDIF

		RETURN BoundingBox{SELF:Origin,oDim}

	ACCESS Color AS Color
		RETURN oColor

	ASSIGN Color(oNewColor AS Color) 
		oColor := oNewColor

	METHOD Destroy() AS USUAL CLIPPER
		oFont := NULL_OBJECT
		oColor := NULL_OBJECT
		cText := NULL_STRING
		SUPER:Destroy()

		RETURN NIL

	ACCESS DisplayText AS STRING
		RETURN cText

	ASSIGN DisplayText(cNewText AS STRING) 

		cText := cNewText
	#ifdef DONOTINCLUDE

	METHOD Draw() 
		LOCAL hDC AS PTR
		LOCAL hLastRop AS PTR
		LOCAL strucLogBrush IS _WinLogBrush
		LOCAL strucColor AS WCColor
		LOCAL wRop AS DWORD
		LOCAL oOldFont AS Font
		LOCAL oOldPen AS Pen

		hDC := SELF:Handle()
		wRop := SELF:RasterOperation
		hLastRop := __WCSetROP(hDC,wRop)
		oOldFont := oWnd:Font
		oWnd:Font := oFont
		oOldPen := oWnd:Pen

		IF (wRop == ROPBackground)

			__WCLogicalBackgroundBrush(oWnd,@strucLogBrush)
			strucColor := (WCColor PTR) @strucLogBrush:lbColor
			oWnd:Pen := Pen{Color{strucColor:bRed,strucColor:bBlue,strucColor:bGreen}}
		ELSE
			oWnd:Pen := Pen{oColor}
		ENDIF

		oWnd:TextPrint(cText,SELF:Origin)
		oWnd:Pen := oOldPen
		oWnd:Font := oOldFont
		SetROP2(hDC, INT(_CAST, hLastRop))

		RETURN NIL
	#endif
	
	ACCESS Font AS VOSDK.Font
		RETURN oFont

	ASSIGN Font(oNewFont AS VOSDK.Font) 
		oFont:=oNewFont



END CLASS

