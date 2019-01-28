CLASS PieObject INHERIT ShapeObject
	PROTECT iAngle1 AS INT
	PROTECT iAngle2 AS INT

METHOD Draw() 
	LOCAL hDC AS PTR
	LOCAL hLastRop AS PTR
	LOCAL hOldPen AS PTR
	LOCAL hOldBrush AS PTR
	LOCAL strucLogBrush IS _WinLogBrush
	LOCAL strucLogPen IS _WinLogPen
	LOCAL oPen AS Pen
	LOCAL oBrush AS Brush
	LOCAL oColor AS Color
	LOCAL strucColor AS WCColor
	LOCAL oPoint AS Point
	LOCAL oDim AS Dimension
	LOCAL wRop AS DWORD
	LOCAL iPX, iPY AS INT
	LOCAL iX1, iX2, iY1, iY2, iMX, iMY AS INT

	
	hDC := SELF:Handle()

	oPen := SELF:Pen
	IF (oPen == NULL_OBJECT)
		oPen := oWnd:Pen
	ENDIF

	oBrush := SELF:Brush
	IF (oBrush == NULL_OBJECT)
		oBrush := oWnd:Foreground
	ENDIF
	wRop := SELF:RasterOperation

	IF (wRop == ROPBackground)
		__WCLogicalPen(oPen,@strucLogPen)
		__WCLogicalBackgroundBrush(oWnd,@struclogBrush)
		strucColor := (WCColor PTR) @strucLogBrush:lbColor
		oColor := Color{strucColor:bRed,strucColor:bBlue,strucColor:bGreen}
		oPen := Pen{oColor,LineSolid,strucLogPen:lopnWidth:X}

		__WCLogicalBrush(oBrush,@strucLogBrush)
		oBrush := Brush{oColor, strucLogBrush:lbHatch }
	ENDIF
	hLastRop := __WCSetROP(hDC,wRop)

	IF (oPen != NULL_OBJECT)
		hOldPen := SelectObject(hDC, oPen:Handle())
	ELSE
		hOldPen := SelectObject(hDC, GetStockObject(Black_Pen))
	ENDIF

	IF (oBrush != NULL_OBJECT)
		hOldBrush := SelectObject(hDC, oBrush:Handle())
	ELSE
		hOldBrush := SelectObject(hDC, GetStockObject(BLACK_BRUSH) )
	ENDIF

	oPoint := SELF:Origin
	oDim := SELF:Size
	iPX := oPoint:X
	iPY := oPoint:Y

	iMX := iPX + (oDim:Width / 2)
	iMY := iPY + (oDim:Height / 2)

	iX1 := iMX + INT((Sin(iAngle1 * PI / 180.0) * 1000.0))
	iX2 := iMX + INT((Sin(iAngle2 * PI / 180.0) * 1000.0))

	iY1 := iMY + INT((Cos(iAngle1 * PI / 180.0) * 1000.0))
	iY2 := iMY + INT((Cos(iAngle2 * PI / 180.0) * 1000.0))

	Pie(hDC, iPX, (iPY + oDim:Height - 1), iPX + oDim:Width, (iPY - 1), iX2, iY2, iX1, iY1)

	SelectObject(hDC, hOldPen)
	SelectObject(hDC, hOldBrush)

	SetROP2(hDC, INT(_CAST,hLastRop))

	RETURN NIL

CONSTRUCTOR(oPoint, oDimension, oPen, oBrush, iStartAngle, iEndAngle) 
	SUPER(oPoint, oDimension, oPen, oBrush)

	iAngle1 := iStartAngle
	iAngle2 := iEndAngle

	RETURN 

END CLASS

