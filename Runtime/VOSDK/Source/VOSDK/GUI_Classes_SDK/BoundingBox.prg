CLASS BoundingBox INHERIT VObject
	PROTECT _Origin AS Point
	PROTECT _Extent AS Point
	

ACCESS Bottom 
	LOCAL retVal AS INT
	
	IF WCGetCoordinateSystem() == WCCartesianCoordinates
		retVal := _Origin:Y
	ELSE
		retVal := _Extent:Y
	ENDIF
	
	RETURN retVal
	

ASSIGN Bottom(iCoord) 
	
	IF WCGetCoordinateSystem() == WCCartesianCoordinates
		_Origin:Y := iCoord
	ELSE
		_Extent:Y := iCoord
	ENDIF
	
	RETURN 
	

METHOD ConvertToScreen(oWindow) 
	//SE-041001 from S. Ebert
	_Origin:ConvertToScreen(oWindow)
	_Extent:ConvertToScreen(oWindow)
	RETURN TRUE
	

ACCESS Extent 
	RETURN _Extent
	

ASSIGN Extent(oPoint) 
	RETURN _Extent := oPoint 
	

ACCESS Height 
	RETURN _Extent:Y - _Origin:Y 
	

ASSIGN Height(iCoord) 
	_Extent:Y := _Origin:Y + iCoord
	RETURN 
	

CONSTRUCTOR(oPoint, xPoint) 
	SUPER()
	_Origin := Point{oPoint:X, oPoint:Y}
	_Extent := Point{0, 0}
	
	IF IsInstanceOfUsual(xPoint, #Dimension)
		SELF:Size := xPoint
	ELSEIF IsInstanceOfUsual(xPoint, #Point)
		_Extent := Point{xPoint:X, xPoint:Y}
	ENDIF
	
	RETURN 
	

ACCESS Left 
	RETURN _Origin:X
	

ASSIGN Left(iCoord) 
	RETURN _Origin:X := iCoord
	

ACCESS Origin 
	RETURN _Origin
	

ASSIGN Origin(oPoint) 
	RETURN _Origin := Point{oPoint:X, oPoint:Y}
	

METHOD PointInside(oPoint) 
	LOCAL lReturnValue AS LOGIC
	
	IF oPoint:X < _Origin:X
		lReturnValue := FALSE
		//PP-030929 bottom boundary should not be included: see X explanation below
	ELSEIF oPoint:Y <= _Origin:Y
		lReturnValue := FALSE
		//PP-030929 right boundary should not be included:
		// If left boundary = 1, and width is 3, extent x is 4
		// Points inside will only be 1,2,3 not 4
	ELSEIF oPoint:X >= _Extent:X
		lReturnValue := FALSE
	ELSEIF oPoint:Y > _Extent:Y
		lReturnValue := FALSE
	ELSE
		lReturnValue := TRUE
	ENDIF
	
	RETURN lReturnValue
	

ACCESS Right 
	
	RETURN _Extent:X
	

ASSIGN Right(iCoord) 
	
	RETURN _Extent:X := iCoord
	

ACCESS Size 
	
	
	RETURN Dimension{SELF:Width, SELF:Height}
	

ASSIGN Size(oDimension) 
	
	
	SELF:Width := oDimension:Width
	SELF:Height := oDimension:Height
	
	RETURN 
	

ACCESS Top 
	LOCAL retVal AS INT
	IF WCGetCoordinateSystem() == WCCartesianCoordinates
		retVal := _Extent:Y
	ELSE
		retVal := _Origin:Y
	ENDIF
	
	RETURN retVal
	

ASSIGN Top(iCoord) 
	IF WCGetCoordinateSystem() == WCCartesianCoordinates
		_Extent:Y := iCoord
	ELSE
		_Origin:Y := iCoord
	ENDIF
	
	RETURN 
	

METHOD Union_(oBB) 
	LOCAL oNewOrigin, oNewExtent AS Point
	oNewOrigin 	:= Point{Min(_Origin:X, oBB:origin:X), Min(_Origin:Y, oBB:origin:Y)} 
	oNewExtent	:= Point{Max(_Extent:X, oBB:extent:X), Max(_Extent:Y, oBB:extent:Y)}
	RETURN BoundingBox{ oNewOrigin, oNewExtent}
	

ACCESS Width 
	RETURN _Extent:X - _Origin:X
	

ASSIGN Width(iCoord) 
	
	_Extent:X := _Origin:X + iCoord
	RETURN 
	
END CLASS

/*
TEXTBLOCK Comment
	// BoundingBox supports right/left/top/bottom. The meaning of the values
	// which these accesses and assigns refer to are dependent on the coordinate
	// system which is in effect. Ideally in a flexible system would only support
	// origin and and extent. Those concepts, along with width and height
	// are coordinate system independent.

ENDTEXT
*/
