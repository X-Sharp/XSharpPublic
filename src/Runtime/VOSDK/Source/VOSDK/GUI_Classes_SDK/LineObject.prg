/// <include file="Gui.xml" path="doc/LineObject/*" />
CLASS LineObject INHERIT DrawObject
	PROTECT oEnd AS Point
	PROTECT oPen AS Pen


/// <include file="Gui.xml" path="doc/LineObject.BoundingBox/*" />
ACCESS BoundingBox
	LOCAL oOrg AS Point
	LOCAL EndX, EndY, OrgX, OrgY AS LONGINT






	oOrg := SELF:Origin
	EndX := oEnd:X
	EndY := oEnd:Y
	OrgX := oOrg:X
	OrgY := oOrg:Y
	RETURN BoundingBox{Point{Min(EndX,OrgX), Min(EndY,OrgY)}, Dimension{Abs(EndX-OrgX),Abs(EndY-OrgY)}}
	//return BoundingBox{self:Origin,self:Size}


/// <include file="Gui.xml" path="doc/LineObject.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF !InCollect()
		oPen:=NULL_OBJECT
		oEnd:=NULL_OBJECT
	ENDIF


	SUPER:Destroy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/LineObject.Draw/*" />
METHOD Draw()
	LOCAL hDC AS PTR
	LOCAL hLastROP AS PTR
	LOCAL wROP AS DWORD
	LOCAL oWndPen AS Pen
	LOCAL lWndPen AS LOGIC
	LOCAL strucLogPen IS _WinLogPen
	LOCAL strucLogBrush IS _WinLogBrush
	LOCAL strucColor AS WCColor


	hDC := SELF:Handle()
	wROP := SELF:RasterOperation
	hLastROP := __WCSetROP(hDC,wROP)


	IF (wROP == ROPBackground)
		oWndPen:=oWnd:Pen //Save the window pen
		IF (oPen != NULL_OBJECT)
			oWnd:Pen:=oPen
		ELSEIF (oWndPen != NULL_OBJECT)
			lWndPen:=TRUE
		ELSE
			__WCLogicalPen(NULL_OBJECT,@strucLogPen)
			__WCLogicalBackgroundBrush(oWnd,@strucLogBrush)
			strucColor := (WCColor PTR) @strucLogBrush:lbColor
			oWnd:Pen:=Pen{ Color{strucColor:bRed,strucColor:bBlue,strucColor:bGreen}, strucLogPen:lopnStyle, strucLogPen:lopnWidth:X}
		ENDIF
		oWnd:MoveTo(SELF:Origin)
		oWnd:LineTo(oEnd)
		IF !lWndPen
			oWnd:Pen := oWndPen //restore the original window pen
		ENDIF
	ELSE
		IF (oPen != NULL_OBJECT)
			oWndPen:=oWnd:Pen
			oWnd:Pen:=oPen
		ENDIF
		oWnd:Moveto(SELF:Origin)
		oWnd:LineTo(oEnd)
		IF (oPen != NULL_OBJECT)
			oWnd:Pen:=oWndPen
		ENDIF
	ENDIF
	SetROP2(hDC, INT(_CAST, hLastRop))


	RETURN NIL


/// <include file="Gui.xml" path="doc/LineObject.ctor/*" />
CONSTRUCTOR(oPoint1, oPoint2, oPen)




	SUPER(oPoint1)


	IF !(oPoint1 IS Point)
		WCError{#Init,#LineObject,__WCSTypeError,oPoint1,1}:Throw()
	ENDIF
	IF !(oPoint2 IS Point)
		WCError{#Init,#LineObject,__WCSTypeError,oPoint2,2}:Throw()
	ENDIF


	IF !IsNil(oPen)
		IF !(oPen IS Pen)
			WCError{#Init,#LineObject,__WCSTypeError,oPen,3}:Throw()
		ENDIF
		SELF:oPen := oPen
	ENDIF


	oEnd := oPoint2


	RETURN


/// <include file="Gui.xml" path="doc/LineObject.Origin/*" />
ASSIGN Origin(oNewPoint)
	LOCAL oOldPoint AS Point






	oOldPoint := SUPER:Origin
	SUPER:Origin := oNewPoint


	oEnd:X := oEnd:X+oNewPoint:X-oOldPoint:X //Adjust end point
	oEnd:Y := oEnd:Y+oNewPoint:Y-oOldPoint:Y


	RETURN


/// <include file="Gui.xml" path="doc/LineObject.Pen/*" />
ACCESS Pen




	RETURN oPen


/// <include file="Gui.xml" path="doc/LineObject.Pen/*" />
ASSIGN Pen(oNewPen)




	IF !IsNil(oPen)
		IF !(oNewPen IS Pen)
			WCError{#Pen,#LineObject,__WCSTypeError,oNewPen,1}:Throw()
		ENDIF
	ENDIF


	RETURN (oPen := oNewPen)


/// <include file="Gui.xml" path="doc/LineObject.Size/*" />
ACCESS Size
	LOCAL oOrg AS Point






	oOrg := SELF:Origin


	RETURN Dimension{oEnd:X-oOrg:X, oEnd:Y-oOrg:Y}


/// <include file="Gui.xml" path="doc/LineObject.Size/*" />
ASSIGN Size(oNewSize)




	IF !(oNewSize IS Dimension)
		WCError{#Size,#LineObject,__WCSTypeError,oNewSize,1}:Throw()
	ENDIF


	oEnd:X := SELF:Origin:X + oNewSize:Width
	oEnd:Y := SELF:Origin:Y + oNewSize:Height


	RETURN
END CLASS


