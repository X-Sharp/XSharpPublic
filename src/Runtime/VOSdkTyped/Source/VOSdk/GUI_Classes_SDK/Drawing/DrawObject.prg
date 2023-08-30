//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

CLASS DrawObject INHERIT VObject
	PROTECT oPoint AS Point
	PROTECT oWnd AS Window


	CONSTRUCTOR(oPoint AS Point)
		SUPER()
		SELF:oPoint:=oPoint
		RETURN

	METHOD __SetWindow(oWindow AS Window) AS Window STRICT
		oWnd := oWindow
		RETURN oWindow

	PROPERTY BoundingBox AS BoundingBox GET BoundingBox{Point{0,0},Dimension{0,0}}

	NEW METHOD Destroy() AS USUAL CLIPPER
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



	PROPERTY Origin AS Point GET oPoint SET oPoint := value:Clone()

	PROPERTY RasterOperation AS LONG AUTO GET SET

	PROPERTY Size AS Dimension GET Dimension{0,0} SET  

END CLASS

