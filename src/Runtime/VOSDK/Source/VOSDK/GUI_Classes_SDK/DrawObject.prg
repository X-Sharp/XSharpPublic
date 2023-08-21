/// <include file="Gui.xml" path="doc/DrawObject/*" />
CLASS DrawObject INHERIT VObject
	PROTECT oPoint AS Point
	PROTECT wROP AS LONGINT    		//RvdH 070205 changed from WORD to LONG
	PROTECT oWnd AS Window


	//PP-030828 Strong typing
 /// <exclude />
	METHOD __SetWindow(oWindow AS Window) AS Window STRICT 
	//PP-030828 Strong typing
	
	


	oWnd := oWindow


	RETURN oWindow


/// <include file="Gui.xml" path="doc/DrawObject.BoundingBox/*" />
ACCESS BoundingBox 
	
	


	RETURN BoundingBox{Point{0,0},Dimension{0,0}}


/// <include file="Gui.xml" path="doc/DrawObject.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	
	


	IF !InCollect()
		oPoint := NULL_OBJECT
		oWnd := NULL_OBJECT
	ENDIF


	SUPER:Destroy()


	RETURN NIL


/// <include file="Gui.xml" path="doc/DrawObject.Draw/*" />
METHOD Draw() 
	
	


	RETURN SELF


/// <include file="Gui.xml" path="doc/DrawObject.Handle/*" />
METHOD Handle(nHandleType) 
	
	


	Default(@nHandleType, 0L)


	IF !IsNil(nHandleType)
		IF !IsLong(nHandleType)
			WCError{#Handle,#DrawObject,__WCSTypeError,nHandleType,1}:Throw()
		ENDIF
	ENDIF


	IF (nHandleType == 0) .and. (oWnd != NULL_OBJECT)
		oWnd:__SetBrushNeeded(TRUE)
		oWnd:__SetPenNeeded(TRUE)
		RETURN oWnd:__GetDC()
	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/DrawObject.HitTest/*" />
METHOD HitTest(oPoint) 
	LOCAL oBB AS BoundingBox
	LOCAL wX, wY AS WORD


	
	


	IF !IsInstanceOfUsual(oPoint,#Point)
		WCError{#HitTest,#DrawObject,__WCSTypeError,oPoint,1}:Throw()
	ENDIF


	oBB := SELF:BoundingBox
	wX := oPoint:X
	wY := oPoint:Y


	RETURN	(wX >= oBB:Left) .and. (wX < oBB:Right) .and. (wY < oBB:Top) .and. (wY >= oBB:Bottom)


/// <include file="Gui.xml" path="doc/DrawObject.ctor/*" />
CONSTRUCTOR(oPoint) 
	
	


	SUPER()
	IF !IsInstanceOfUsual(oPoint,#Point)
		WCError{#Init,#DrawObject,__WCSTypeError,oPoint,1}:Throw()
	ENDIF


	
	
	SELF:oPoint:=oPoint


	RETURN 


/// <include file="Gui.xml" path="doc/DrawObject.Origin/*" />
ACCESS Origin 
	
	


	RETURN oPoint


/// <include file="Gui.xml" path="doc/DrawObject.Origin/*" />
ASSIGN Origin(oNewPoint) 
	
	


	IF !IsInstanceOfUsual(oNewPoint,#Point)
		WCError{#Origin,#DrawObject,__WCSTypeError,oNewPoint,1}:Throw()
	ENDIF


	RETURN oPoint := Point{oNewPoint:X, oNewPoint:Y}


/// <include file="Gui.xml" path="doc/DrawObject.RasterOperation/*" />
ACCESS RasterOperation 
	
	


	RETURN wRop


/// <include file="Gui.xml" path="doc/DrawObject.RasterOperation/*" />
ASSIGN RasterOperation(kRaster) 
	
	


	IF !IsLong(kRaster)
		WCError{#RasterOperation,#DrawObject,__WCSTypeError,kRaster,1}:Throw()
	ENDIF


	RETURN wRop := kRaster


/// <include file="Gui.xml" path="doc/DrawObject.Size/*" />
ACCESS Size 
	
	


	RETURN Dimension{0,0}


/// <include file="Gui.xml" path="doc/DrawObject.Size/*" />
ASSIGN Size(oNewSize) 
	
	


	IF !IsInstanceOfUsual(oNewSize,#Dimension)
		WCError{#Size,#DrawObject,__WCSTypeError,oNewSize,1}:Throw()
	ENDIF


	RETURN Dimension{0,0}
END CLASS


