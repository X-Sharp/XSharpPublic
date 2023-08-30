//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
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
    PROPERTY Bottom AS INT GET _Extent:Y SET _Extent:Y := Value

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
    PROPERTY Extent AS Point GET _Extent SET _Extent := Value

    /// <include file="Gui.xml" path="doc/BoundingBox.Height/*" />
    PROPERTY Height AS INT GET _Extent:Y - _Origin:Y SET _Extent:Y := _Origin:Y + value

    /// <include file="Gui.xml" path="doc/BoundingBox.ctor/*" />
    CONSTRUCTOR() STRICT
        SUPER()
        _Origin := Point{0,0}
        _Extent := Point{0,0}

    /// <include file="Gui.xml" path="doc/BoundingBox.ctor/*" />
    CONSTRUCTOR(oPoint AS USUAL, xPoint AS USUAL)

        IF xPoint IS Dimension var oDim
            SELF((Point) oPoint, oDim)
        ELSEIF xPoint is Point VAR oPt2
            SELF((Point) oPoint, oPt2)
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
    PROPERTY Left AS INT GET _Origin:X SET _Origin:X := Value

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
    PROPERTY Origin AS Point GET _Origin SET _Origin := value:Clone()


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
    PROPERTY Right  AS INT GET _Extent:X SET _Extent:X := value


    /// <include file="Gui.xml" path="doc/BoundingBox.Size/*" />
    PROPERTY Size AS Dimension
        GET
            RETURN Dimension{SELF:Width, SELF:Height}
        END GET

        SET
            SELF:Width := value:Width
            SELF:Height := value:Height
        END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/BoundingBox.Top/*" />
    PROPERTY Top  AS INT GET _Origin:Y SET _Origin:Y := value


    /// <include file="Gui.xml" path="doc/BoundingBox.Union_/*" />
    METHOD Union_(oBB AS BoundingBox) AS BoundingBox
        LOCAL oNewOrigin, oNewExtent AS Point
        oNewOrigin 	:= Point{Math.Min(_Origin:X, oBB:Origin:X), Math.Min(_Origin:Y, oBB:Origin:Y)}
        oNewExtent	:= Point{Math.Max(_Extent:X, oBB:Extent:X), Math.Max(_Extent:Y, oBB:Extent:Y)}
        RETURN BoundingBox{ oNewOrigin, oNewExtent}


    /// <include file="Gui.xml" path="doc/BoundingBox.Width/*" />
    PROPERTY Width AS INT GET _Extent:X - _Origin:X SET _Extent:X := _Origin:X + value

    /// <summary>
    /// Implicit operator to convert System.Drawing.Rectangle to BoundingBox
    /// </summary>
    /// <param name="r">Rectangle</param>
    /// <returns>Boundingbox</returns>
    OPERATOR IMPLICIT ( r AS System.Drawing.Rectangle) AS BoundingBox
        RETURN BoundingBox{Point{r:X, r:Y}, Dimension{r:Width, r:Height}}


    /// <summary>
    /// Implicit operator to convert BoundingBox to System.Drawing.Rectangle
    /// </summary>
    /// <param name="b">BoundingBox</param>
    /// <returns>Boundingbox</returns>
    OPERATOR IMPLICIT ( b AS BoundingBox  ) AS System.Drawing.Rectangle
        RETURN System.Drawing.Rectangle{b:Left, b:Top, b:Width, b:Height}


END CLASS

