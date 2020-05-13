
CLASS DrawObject INHERIT VObject
	PROTECT oPoint AS Point
	PROTECT wROP AS LONGINT    	
	PROTECT oWnd AS Window


	CONSTRUCTOR(oPoint AS Point) 
		SUPER()
		SELF:oPoint:=oPoint
		RETURN 

	METHOD __SetWindow(oWindow AS Window) AS Window STRICT 
		oWnd := oWindow
		RETURN oWindow

	ACCESS BoundingBox AS BoundingBox
		RETURN BoundingBox{Point{0,0},Dimension{0,0}}

	METHOD Destroy() AS USUAL CLIPPER
		oPoint := NULL_OBJECT
		oWnd := NULL_OBJECT
		SUPER:Destroy()
		RETURN NIL

	METHOD Draw() 
		RETURN SELF

	METHOD Handle(nHandleType := 0 AS LONG) AS IntPtr STRICT

		IF (nHandleType == 0) .and. (oWnd != NULL_OBJECT)
			oWnd:__SetBrushNeeded(TRUE)
			oWnd:__SetPenNeeded(TRUE)
			RETURN oWnd:__GetDC()
		ENDIF

		RETURN 0

	METHOD HitTest(oPoint AS Point) AS LOGIC
		LOCAL oBB AS BoundingBox
		LOCAL wX, wY AS INT

		oBB := SELF:BoundingBox
		wX := oPoint:X
		wY := oPoint:Y

		RETURN	(wX >= oBB:Left) .and. (wX < oBB:Right) .and. (wY < oBB:Top) .and. (wY >= oBB:Bottom)



	ACCESS Origin AS Point
		RETURN oPoint

	ASSIGN Origin(oNewPoint AS Point) 
		oPoint := Point{oNewPoint:X, oNewPoint:Y}

	ACCESS RasterOperation AS LONG
		RETURN wROP

	ASSIGN RasterOperation(kRaster AS LONG) 
		wROP := kRaster

	ACCESS Size AS Dimension
		RETURN Dimension{0,0}

	ASSIGN Size(oNewSize AS Dimension) 

		RETURN 

END CLASS

