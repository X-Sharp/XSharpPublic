//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

CLASS ShapeObject INHERIT DrawObject
	PROTECT oDimension AS Dimension

CONSTRUCTOR(oPoint AS Point, oDimension AS Dimension)
	SUPER(oPoint)
	SELF:oDimension:=oDimension


CONSTRUCTOR(oPoint AS Point, oDimension AS Dimension, oPen AS Pen)
	SELF(oPoint, oDimension)
	SELF:Pen := oPen


CONSTRUCTOR(oPoint AS Point, oDimension AS Dimension, oPen AS Pen, oBrush AS Brush)
	SELF(oPoint, oDimension)
	SELF:Pen := oPen
	SELF:Brush:=oBrush
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

PROPERTY Brush AS Brush AUTO GET SET

METHOD Destroy() AS USUAL CLIPPER

	oDimension := NULL_OBJECT
	Pen := NULL_OBJECT
	Brush := NULL_OBJECT
	SUPER:Destroy()

	RETURN NIL


PROPERTY Pen AS Pen AUTO GET SET

PROPERTY Size AS Dimension GET oDimension SET oDimension := value:Clone()

END CLASS

