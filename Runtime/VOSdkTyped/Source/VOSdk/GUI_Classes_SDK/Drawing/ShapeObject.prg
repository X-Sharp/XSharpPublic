
CLASS ShapeObject INHERIT DrawObject
	PROTECT oDimension AS Dimension
	PROTECT oPen AS Pen
	PROTECT oBrush AS Brush


CONSTRUCTOR(oPoint AS Point, oDimension AS Dimension) 
	SUPER(oPoint)
	SELF:oDimension:=oDimension


CONSTRUCTOR(oPoint AS Point, oDimension AS Dimension, oPen AS Pen) 
	SELF(oPoint, oDimension)
	SELF:oPen := oPen


CONSTRUCTOR(oPoint AS Point, oDimension AS Dimension, oPen AS Pen, oBrush AS Brush) 
	SELF(oPoint, oDimension, oPen)
	SELF:oBrush:=oBrush
	RETURN 

ACCESS BoundingBox AS BoundingBox
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

ACCESS Brush AS Brush
	RETURN oBrush

ASSIGN Brush(oNewBrush AS Brush) 
	oBrush:=oNewBrush

METHOD Destroy() AS USUAL CLIPPER

	oDimension := NULL_OBJECT
	oPen := NULL_OBJECT
	oBrush := NULL_OBJECT
	SUPER:Destroy()

	RETURN NIL




ACCESS Pen AS Pen
	RETURN oPen

ASSIGN Pen(oNewPen AS Pen) 
	oPen:=oNewPen

ACCESS Size AS Dimension
	RETURN oDimension

ASSIGN Size(oNewDimension AS Dimension) 
	oDimension := Dimension{oNewDimension:Width, oNewDimension:Height}

END CLASS

