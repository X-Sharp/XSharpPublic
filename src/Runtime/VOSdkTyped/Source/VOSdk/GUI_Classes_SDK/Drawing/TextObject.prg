//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING VOSDK := XSharp.VO.SDK
CLASS TextObject INHERIT DrawObject


	CONSTRUCTOR(oPoint AS Point, cText := NULL_STRING AS STRING, oFont := NULL_OBJECT AS Font, oColor := NULL_OBJECT AS Color)
		SUPER(oPoint)
	    LOCAL dwColor AS DWORD

		IF cText != NULL_STRING
			SELF:DisplayText := cText
		ENDIF
		IF oFont != NULL_OBJECT
			SELF:Font:=oFont
		ELSE
			SELF:Font:=Font{FontSystem8}
		ENDIF

		IF oColor != NULL_OBJECT
			SELF:Color := oColor
		ELSE
			dwColor	:=	GuiWin32.GetSysColor(Color_WindowText)
			SELF:Color := Color{dwColor}
		ENDIF

		RETURN

	ACCESS BoundingBox  AS BoundingBox
		LOCAL oOldFont AS Font
		LOCAL oDim AS Dimension
		IF (oWnd != NULL_OBJECT)
			oOldFont := oWnd:Font
			oWnd:Font := SELF:Font
			oDim := oWnd:SizeText(SELF:DisplayText)
			oWnd:Font := oOldFont
		ENDIF

		RETURN BoundingBox{SELF:Origin,oDim}

	PROPERTY Color AS Color AUTO GET SET

	METHOD Destroy() AS USUAL CLIPPER
		SELF:Font := NULL_OBJECT
		SELF:Color := NULL_OBJECT
		SELF:DisplayText := NULL_STRING
		SUPER:Destroy()

		RETURN NIL

	PROPERTY DisplayText AS STRING AUTO GET SET
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

	PROPERTY Font AS VOSDK.Font AUTO GET SET



END CLASS

