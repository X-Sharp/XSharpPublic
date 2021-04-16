/// <include file="Gui.xml" path="doc/ShapeObject/*" />
CLASS ShapeObject INHERIT DrawObject
	PROTECT oDimension AS Dimension
	PROTECT oPen AS Pen
	PROTECT oBrush AS Brush


/// <include file="Gui.xml" path="doc/ShapeObject.BoundingBox/*" />
ACCESS BoundingBox 
	LOCAL oOrg AS Point
	LOCAL oDim AS Dimension


	
	


	oOrg := SELF:Origin
	oOrg := Point{oOrg:X,oOrg:Y} //Riz use OClone
	oDim := Dimension{oDimension:Width,oDimension:Height} //Riz use OClone


	IF (oDimension:Width < 0)
		oOrg:X := oOrg:X + oDimension:Width
		oDim:Width := -oDim:Width
	ENDIF


	IF (oDimension:Height < 0)
		oOrg:Y := oOrg:Y+oDimension:Height
		oDim:Height := -oDim:Height
	ENDIF


	RETURN BoundingBox{oOrg,oDim}


/// <include file="Gui.xml" path="doc/ShapeObject.Brush/*" />
ACCESS Brush 
	
	


	RETURN oBrush


/// <include file="Gui.xml" path="doc/ShapeObject.Brush/*" />
ASSIGN Brush(oNewBrush) 
	
	


	IF !IsNil(oNewBrush)
		IF !IsInstanceOfUsual(oNewBrush,#Brush)
			WCError{#Brush,#ShapeObject,__WCSTypeError,oNewBrush,1}:Throw()
		ENDIF
	ENDIF
	RETURN oBrush:=oNewBrush


/// <include file="Gui.xml" path="doc/ShapeObject.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	
	


	IF !InCollect()
		oDimension := NULL_OBJECT
		oPen := NULL_OBJECT
		oBrush := NULL_OBJECT
	ENDIF
	SUPER:Destroy()


	RETURN NIL


/// <include file="Gui.xml" path="doc/ShapeObject.ctor/*" />
CONSTRUCTOR(oPoint, oDimension, oPen, oBrush) 
	
	


	SUPER(oPoint)


	IF !IsInstanceOfUsual(oDimension,#Dimension)
		WCError{#Init,#ShapeObject,__WCSTypeError,oDimension,2}:Throw()
	ENDIF
	IF !IsNil(oPen)
		IF !IsInstanceOfUsual(oPen,#Pen)
			WCError{#Init,#ShapeObject,__WCSTypeError,oPen,3}:Throw()
		ENDIF
		SELF:oPen:=oPen
	ENDIF
	IF !IsNil(oBrush)
		IF !IsInstanceOfUsual(oBrush,#Brush)
			WCError{#Init,#ShapeObject,__WCSTypeError,oBrush,4}:Throw()
		ENDIF
		SELF:oBrush:=oBrush
	ENDIF
	SELF:oDimension:=oDimension


	RETURN 


/// <include file="Gui.xml" path="doc/ShapeObject.Pen/*" />
ACCESS Pen 
	
	
	RETURN oPen


/// <include file="Gui.xml" path="doc/ShapeObject.Pen/*" />
ASSIGN Pen(oNewPen) 
	
	


	IF !IsNil(oNewPen)
		IF !IsInstanceOfUsual(oNewPen,#Pen)
			WCError{#Pen,#ShapeObject,__WCSTypeError,oNewPen,1}:Throw()
		ENDIF
	ENDIF


	RETURN oPen:=oNewPen


/// <include file="Gui.xml" path="doc/ShapeObject.Size/*" />
ACCESS Size 
	RETURN oDimension


/// <include file="Gui.xml" path="doc/ShapeObject.Size/*" />
ASSIGN Size(oNewDimension) 
	IF !IsInstanceOfUsual(oNewDimension,#Dimension)
		WCError{#Size,#ShapeObject,__WCSTypeError,oNewDimension,1}:Throw()
	ENDIF
	RETURN oDimension := Dimension{oNewDimension:Width, oNewDimension:Height}
END CLASS


