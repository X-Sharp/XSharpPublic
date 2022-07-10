
/*
// BoundingBox supports right/left/top/bottom. The meaning of the values
// which these accesses and assigns refer to are dependent on the coordinate
// system which is in effect. Ideally in a flexible system would only support
// origin and and extent. Those concepts, along with width and height
// are coordinate system independent.

ENDTEXT
*/
/// <include file="Gui.xml" path="doc/BoundingBox/*" />
CLASS BoundingBox INHERIT VObject
	PROTECT _Origin AS Point
	PROTECT _Extent AS Point


/// <include file="Gui.xml" path="doc/BoundingBox.Bottom/*" />
	ACCESS Bottom AS INT
		LOCAL retVal AS INT
		retVal := _Extent:Y
		RETURN retVal


/// <include file="Gui.xml" path="doc/BoundingBox.Bottom/*" />
	ASSIGN Bottom(iCoord AS INT)
		_Extent:Y := iCoord
		RETURN


	METHOD Clone() AS BoundingBox STRICT
		LOCAL oBB AS BoundingBox
		oBB := BoundingBox{SELF:Origin, SELF:Size}
		oBB:Normalize()
		RETURN oBB

/// <include file="Gui.xml" path="doc/BoundingBox.ConvertToScreen/*" />
	METHOD ConvertToScreen(oWindow AS OBJECT) AS LOGIC
		_Origin:ConvertToScreen(oWindow)
		_Extent:ConvertToScreen(oWindow)
		RETURN TRUE

/// <include file="Gui.xml" path="doc/BoundingBox.ConvertToScreen/*" />
	METHOD ConvertToScreen(oWindow AS IntPtr) AS LOGIC
		_Origin:ConvertToScreen(oWindow)
		_Extent:ConvertToScreen(oWindow)
		RETURN TRUE


/// <include file="Gui.xml" path="doc/BoundingBox.Extent/*" />
	ACCESS Extent AS Point
		RETURN _Extent


/// <include file="Gui.xml" path="doc/BoundingBox.Extent/*" />
	ASSIGN Extent(oPoint AS Point)
		_Extent := oPoint

/// <include file="Gui.xml" path="doc/BoundingBox.Height/*" />
	ACCESS Height AS INT
		RETURN _Extent:Y - _Origin:Y

/// <include file="Gui.xml" path="doc/BoundingBox.Height/*" />
	ASSIGN Height(iCoord AS INT)
		_Extent:Y := _Origin:Y + iCoord
		RETURN


/// <include file="Gui.xml" path="doc/BoundingBox.ctor/*" />
	CONSTRUCTOR() STRICT
		SUPER()
		_Origin := Point{0,0}
		_Extent := Point{0,0}
/// <include file="Gui.xml" path="doc/BoundingBox.ctor/*" />

	CONSTRUCTOR(oPoint AS USUAL, xPoint AS USUAL)
		IF IsInstanceOfUsual(xPoint, #Dimension)
			SELF((Point) oPoint, (Dimension) xPoint)
		ELSEIF IsInstanceOfUsual(xPoint, #Point)
			SELF((Point) oPoint, (Point) xPoint)
		ELSE
			SELF()
		ENDIF
/// <include file="Gui.xml" path="doc/BoundingBox.ctor/*" />

	CONSTRUCTOR(oPoint AS Point, xPoint AS Dimension)
		SUPER()
		_Origin := Point{oPoint:X, oPoint:Y}
		_Extent := Point{0, 0}
		SELF:Size := xPoint

		RETURN
/// <include file="Gui.xml" path="doc/BoundingBox.ctor/*" />

	CONSTRUCTOR(oPoint AS Point, xPoint AS Point)
		SUPER()
		_Origin := Point{oPoint:X, oPoint:Y}
		_Extent := Point{xPoint:X, xPoint:Y}

		RETURN

/// <include file="Gui.xml" path="doc/BoundingBox.Left/*" />
	ACCESS Left AS INT
		RETURN _Origin:X

/// <include file="Gui.xml" path="doc/BoundingBox.Left/*" />
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
/// <include file="Gui.xml" path="doc/BoundingBox.Origin/*" />
	ACCESS Origin AS Point
		RETURN _Origin

/// <include file="Gui.xml" path="doc/BoundingBox.Origin/*" />
	ASSIGN Origin(oPoint AS Point)
		_Origin := Point{oPoint:X, oPoint:Y}


/// <include file="Gui.xml" path="doc/BoundingBox.PointInside/*" />
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


/// <include file="Gui.xml" path="doc/BoundingBox.Right/*" />
	ACCESS Right  AS INT

		RETURN _Extent:X


/// <include file="Gui.xml" path="doc/BoundingBox.Right/*" />
	ASSIGN Right(iCoord AS INT)

		_Extent:X := iCoord


/// <include file="Gui.xml" path="doc/BoundingBox.Size/*" />
	ACCESS Size AS Dimension

		RETURN Dimension{SELF:Width, SELF:Height}


/// <include file="Gui.xml" path="doc/BoundingBox.Size/*" />
	ASSIGN Size(oDimension AS Dimension)

		SELF:Width := oDimension:Width
		SELF:Height := oDimension:Height

		RETURN


/// <include file="Gui.xml" path="doc/BoundingBox.Top/*" />
	ACCESS Top  AS INT
		LOCAL retVal AS INT
		retVal := _Origin:Y

		RETURN retVal


/// <include file="Gui.xml" path="doc/BoundingBox.Top/*" />
	ASSIGN Top(iCoord AS INT)
		_Origin:Y := iCoord

		RETURN


/// <include file="Gui.xml" path="doc/BoundingBox.Union_/*" />
	METHOD Union_(oBB AS BoundingBox) AS BoundingBox
		LOCAL oNewOrigin, oNewExtent AS Point
		oNewOrigin 	:= Point{Min(_Origin:X, oBB:Origin:X), Min(_Origin:Y, oBB:Origin:Y)}
		oNewExtent	:= Point{Max(_Extent:X, oBB:Extent:X), Max(_Extent:Y, oBB:Extent:Y)}
		RETURN BoundingBox{ oNewOrigin, oNewExtent}


/// <include file="Gui.xml" path="doc/BoundingBox.Width/*" />
	ACCESS Width AS INT
		RETURN _Extent:X - _Origin:X


/// <include file="Gui.xml" path="doc/BoundingBox.Width/*" />
	ASSIGN Width(iCoord AS INT)

		_Extent:X := _Origin:X + iCoord
		RETURN

	OPERATOR IMPLICIT ( r AS System.Drawing.Rectangle) AS BoundingBox
		RETURN BoundingBox{Point{r:X, r:Y}, Dimension{r:Width, r:Height}}


	OPERATOR IMPLICIT ( b AS BoundingBox  ) AS System.Drawing.Rectangle
		RETURN System.Drawing.Rectangle{b:Left, b:Top, b:Width, b:Height}


END CLASS

