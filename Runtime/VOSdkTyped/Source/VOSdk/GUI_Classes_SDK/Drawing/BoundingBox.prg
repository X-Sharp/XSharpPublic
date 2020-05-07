
/*
// BoundingBox supports right/left/top/bottom. The meaning of the values
// which these accesses and assigns refer to are dependent on the coordinate
// system which is in effect. Ideally in a flexible system would only support
// origin and and extent. Those concepts, along with width and height
// are coordinate system independent.

ENDTEXT
*/
CLASS BoundingBox INHERIT VObject
	PROTECT _Origin AS Point
	PROTECT _Extent AS Point
	

	ACCESS Bottom AS INT
		LOCAL retVal AS INT
		retVal := _Extent:Y
		RETURN retVal
	

	ASSIGN Bottom(iCoord AS INT) 
		_Extent:Y := iCoord
		RETURN 
	

	METHOD Clone() AS BoundingBox STRICT
		LOCAL oBB AS BoundingBox
		oBB := BoundingBox{SELF:Origin, SELF:Size}
		oBB:Normalize()
		RETURN oBB

	METHOD ConvertToScreen(oWindow AS OBJECT) AS LOGIC 
		_Origin:ConvertToScreen(oWindow)
		_Extent:ConvertToScreen(oWindow)
		RETURN TRUE
	
	METHOD ConvertToScreen(hWindow AS IntPtr) AS LOGIC 
		_Origin:ConvertToScreen(hWindow)
		_Extent:ConvertToScreen(hWindow)
		RETURN TRUE


	ACCESS Extent AS Point
		RETURN _Extent
	

	ASSIGN Extent(oPoint AS Point) 
		_Extent := oPoint 
	
	ACCESS Height AS INT
		RETURN _Extent:Y - _Origin:Y 

	ASSIGN Height(iCoord AS INT) 
		_Extent:Y := _Origin:Y + iCoord
		RETURN 


	CONSTRUCTOR() STRICT
		SUPER()
		_Origin := Point{0,0}
		_Extent := Point{0,0}
		
	CONSTRUCTOR(oPoint AS USUAL, xPoint AS USUAL) 
		IF IsInstanceOfUsual(xPoint, #Dimension)
			SELF((Point) oPoint, (Dimension) xPoint)
		ELSEIF IsInstanceOfUsual(xPoint, #Point)
			SELF((Point) oPoint, (Point) xPoint)
		ELSE
			SELF()
		ENDIF	
	
	CONSTRUCTOR(oPoint AS Point, xPoint AS Dimension) 
		SUPER()
		_Origin := Point{oPoint:X, oPoint:Y}
		_Extent := Point{0, 0}
		SELF:Size := xPoint
		
		RETURN 

	CONSTRUCTOR(oPoint AS Point, xPoint AS Point) 
		SUPER()
		_Origin := Point{oPoint:X, oPoint:Y}
		_Extent := Point{xPoint:X, xPoint:Y}
		
		RETURN 

	ACCESS Left AS INT
		RETURN _Origin:X

	ASSIGN Left(iCoord AS INT) 
		_Origin:X := iCoord

	METHOD Normalize() AS VOID STRICT
		LOCAL nTemp AS LONG
		IF _Origin:Y > _Extent:Y
			nTemp := _Origin:Y
			_Origin:Y := _Extent:Y
			_Extent:Y := nTemp
		ENDIF
		IF _Origin:X > _Extent:X
			nTemp := _Origin:X
			_Origin:X := _Extent:X
			_Extent:X := nTemp
		ENDIF
		RETURN
	ACCESS Origin AS Point
		RETURN _Origin

	ASSIGN Origin(oPoint AS Point) 
		_Origin := Point{oPoint:X, oPoint:Y}
	

	METHOD PointInside(oPoint AS Point) AS LOGIC 
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
	

	ACCESS Right  AS INT
		
		RETURN _Extent:X
	

	ASSIGN Right(iCoord AS INT) 
		
		_Extent:X := iCoord
	

	ACCESS Size AS Dimension
		
		RETURN Dimension{SELF:Width, SELF:Height}
	

	ASSIGN Size(oDimension AS Dimension) 
		
		SELF:Width := oDimension:Width
		SELF:Height := oDimension:Height
		
		RETURN 
	

	ACCESS Top  AS INT
		LOCAL retVal AS INT
		retVal := _Origin:Y
		
		RETURN retVal
	

	ASSIGN Top(iCoord AS INT) 
		_Origin:Y := iCoord
		
		RETURN 
	

	METHOD Union_(oBB AS BoundingBox) AS BoundingBox 
		LOCAL oNewOrigin, oNewExtent AS Point
		oNewOrigin 	:= Point{Min(_Origin:X, oBB:Origin:X), Min(_Origin:Y, oBB:Origin:Y)} 
		oNewExtent	:= Point{Max(_Extent:X, oBB:Extent:X), Max(_Extent:Y, oBB:Extent:Y)}
		RETURN BoundingBox{ oNewOrigin, oNewExtent}
	

	ACCESS Width AS INT
		RETURN _Extent:X - _Origin:X
	

	ASSIGN Width(iCoord AS INT) 
		
		_Extent:X := _Origin:X + iCoord
		RETURN 

	OPERATOR IMPLICIT ( r AS System.Drawing.Rectangle) AS BoundingBox
		RETURN BoundingBox{Point{r:X, r:Y}, Dimension{r:Width, r:Height}}


	OPERATOR IMPLICIT ( b AS BoundingBox  ) AS System.Drawing.Rectangle
		RETURN System.Drawing.Rectangle{b:Left, b:Top, b:Width, b:Height}

	
END CLASS

