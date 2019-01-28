CLASS TextObject INHERIT DrawObject
	PROTECT oFont AS Font
	PROTECT oColor AS Color
	PROTECT cText AS STRING

ACCESS BoundingBox 
	LOCAL oOldFont AS Font
	LOCAL oDim AS Dimension

	

	IF (oWnd != NULL_OBJECT)
		oOldFont := oWnd:Font
		oWnd:Font := oFont
		oDim := oWnd:SizeText(cText)
		oWnd:Font := oOldFont
	ENDIF

	RETURN BoundingBox{SELF:Origin,oDim}

ACCESS Color 
	

	RETURN oColor

ASSIGN Color(oNewColor) 
	

	IF !IsInstanceOfUsual(oNewColor,#Color)
		WCError{#Color,#TextObject,__WCSTypeError,oNewColor,1}:@@Throw()
	ENDIF

	RETURN (oColor := oNewColor)

METHOD Destroy() 
	

	IF !InCollect()
		oFont := NULL_OBJECT
		oColor := NULL_OBJECT
		cText := NULL_STRING
	ENDIF
	SUPER:Destroy()

	RETURN NIL

ACCESS DisplayText 
	

	RETURN cText

ASSIGN DisplayText(cNewText) 
	

	IF !IsString(cNewText)
		WCError{#DisplayText,#TextObject,__WCSTypeError,cNewText,1}:@@Throw()
	ENDIF

	RETURN (cText := cNewText)

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

ACCESS Font 
	

	RETURN oFont

ASSIGN Font(oNewFont) 
	

	IF !IsInstanceOfUsual(oNewFont,#Font)
		WCError{#Font,#TextObject,__WCSTypeError,oNewFont,1}:@@Throw()
	ENDIF

	RETURN oFont:=oNewFont

CONSTRUCTOR(oPoint, cText, oFont, oColor) 
	LOCAL strucColor AS WCColor
	LOCAL dwColor AS DWORD

	

	IF IsNil(oPoint)
		SUPER(Point{10,10})
	ELSE
		SUPER(oPoint)
	ENDIF

	IF !IsNil(cText)
		IF !IsString(cText)
			WCError{#Init,#TextObject,__WCSTypeError,cText,2}:@@Throw()
		ENDIF
		SELF:cText := cText
	ENDIF

	IF !IsNil(oFont)
		IF !IsInstanceOfUsual(oFont,#Font)
			WCError{#Init,#TextObject,__WCSTypeError,oFont,3}:@@Throw()
		ENDIF
		SELF:oFont:=oFont
	ELSE
		SELF:oFont:=Font{FontSystem8}
	ENDIF

	IF !IsNil(oColor)
		IF !IsInstanceOfUsual(oColor,#Color)
			WCError{#Init,#TextObject,__WCSTypeError,oColor,4}:@@Throw()
		ENDIF
		SELF:oColor := oColor
	ELSE
		dwColor:=GetSysColor(Color_WindowText)
#ifdef __VULCAN__		
		strucColor := (WCColor PTR) @dwColor
#else		
		strucColor := @dwColor
#endif		
		SELF:oColor := Color{strucColor:bRed,strucColor:bBlue,strucColor:bGreen}
	ENDIF

	RETURN 

END CLASS

