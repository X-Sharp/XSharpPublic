//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Diagnostics
/// <include file="Gui.xml" path="doc/Pair/*" />

[DebuggerStepThrough];
[DebuggerDisplay("{iInt1}, {iInt2}")];
CLASS Pair INHERIT VObject
    PROTECT iInt1 AS INT
    PROTECT iInt2 AS INT
    CONSTRUCTOR() STRICT
        SUPER()

    /// <include file="Gui.xml" path="doc/Pair.ctor/*" />
    CONSTRUCTOR(Int1 AS INT, Int2 AS INT)
        SUPER()
        iInt1 := Int1
        iInt2 := Int2
        RETURN

    /// <summary>
    /// Is the pair empty ?(both values equal to 0)
    /// </summary>
    /// <value></value>
    PROPERTY Empty AS LOGIC GET iInt1 == 0 .AND. iInt2 == 0

    METHOD GetHashCode AS LONG STRICT
        RETURN iInt1:GetHashCode() + iInt2:GetHashCode()

END CLASS

/// <include file="Gui.xml" path="doc/Dimension/*" />
[DebuggerStepThrough];
[DebuggerDisplay("Width: {Width}, Height: {Height}")];
CLASS Dimension INHERIT Pair

    /// <include file="Gui.xml" path="doc/Dimension.ctor/*" />
    CONSTRUCTOR() STRICT
        SUPER()

    /// <include file="Gui.xml" path="doc/Dimension.ctor/*" />
    CONSTRUCTOR(nWidth AS INT, nHeight AS INT)
        SUPER(nWidth, nHeight)
        RETURN
    /// <include file="Gui.xml" path="doc/Dimension.Height/*" />
    PROPERTY Height  AS LONGINT  GET iInt2 SET iInt2 := VALUE

    /// <include file="Gui.xml" path="doc/Dimension.Width/*" />
    PROPERTY Width  AS LONGINT  GET iInt1 SET iInt1 := VALUE

    METHOD Clone() AS Dimension
        RETURN Dimension{SELF:Width, SELF:Height}

    OPERATOR IMPLICIT ( s AS System.Drawing.Size) AS Dimension
        RETURN Dimension{s:Width, s:Height}

    OPERATOR IMPLICIT ( p AS System.Windows.Forms.Padding) AS Dimension
        RETURN Dimension{p:Left, p:Top}

    OPERATOR IMPLICIT ( d AS Dimension ) AS System.Drawing.Size
        //IF d == NULL_OBJECT
        //	RETURN System.Drawing.Size.Empty
        //ENDIF
        RETURN System.Drawing.Size{d:Width, d:Height}

    OPERATOR IMPLICIT ( r AS System.Drawing.Rectangle) AS Dimension
        RETURN Dimension{r:Width, r:Height}

END CLASS

/// <include file="Gui.xml" path="doc/Point/*" />
[DebuggerStepThrough];
[DebuggerDisplay("X: {X}, Y: {Y}")];
CLASS Point INHERIT Pair

    /// <include file="Gui.xml" path="doc/Point.ctor/*" />
    CONSTRUCTOR() STRICT
        SUPER()
        RETURN
    /// <include file="Gui.xml" path="doc/Point.ctor/*" />
    CONSTRUCTOR(nX AS INT, nY AS INT)
        SUPER(nX, nY)
        RETURN
    /// <include file="Gui.xml" path="doc/Point.ConvertToScreen/*" />

    METHOD ConvertToScreen(hWnd AS IntPtr) AS LOGIC
        RETURN SELF:ConvertToScreen(hWnd, TRUE)

    METHOD ConvertToScreen(hWnd AS IntPtr, lWinRect AS LOGIC) AS LOGIC
        //Todo ConvertToScreen
        LOCAL sPoint := WINPOINT{} AS WINPOINT

        IF hWnd = IntPtr.Zero
            RETURN FALSE
        ENDIF

        sPoint:x := iInt1
        sPoint:y := iInt2

        GuiWin32.ClientToScreen(hWnd, REF sPoint)

        iInt1 := sPoint:x
        iInt2 := sPoint:y

        RETURN TRUE

    METHOD ConvertToScreen(oWindow AS OBJECT) AS LOGIC
        LOCAL hWnd   AS PTR
        LOCAL lOk AS LOGIC
        IF oWindow IS Window VAR oWin
            hWnd    := oWin:Handle(4)
            lOk		:= SELF:ConvertToScreen(hWnd, TRUE)
        ELSEIF oWindow IS Control VAR oC
            hWnd    := oC:Handle()
            lOk		:= SELF:ConvertToScreen(hWnd, FALSE)
        ELSE
            lOk := FALSE
        ENDIF
        RETURN lOk

    /// <include file="Gui.xml" path="doc/Point.X/*" />
    PROPERTY X  AS LONGINT  GET iInt1 SET iInt1 := VALUE
    /// <include file="Gui.xml" path="doc/Point.Y/*" />
    PROPERTY Y  AS LONGINT  GET iInt2 SET iInt2 := VALUE

    /// <exclude />
    METHOD Clone() AS Point
        RETURN (Point) SELF:MemberwiseClone()

    /// <exclude />
    OPERATOR IMPLICIT ( p AS System.Drawing.Point) AS Point
        RETURN Point{p:X, p:Y}

    /// <exclude />
    OPERATOR IMPLICIT ( p AS Point ) AS System.Drawing.Point
        RETURN System.Drawing.Point{p:X, p:Y}

    /// <exclude />
    OPERATOR + (p1 AS Point, p2 AS Point) AS Point
        LOCAL r AS Point
        IF p2:Empty
            RETURN p1:Clone()
        ELSE
            r := p1:Clone()
            r:iInt1 += p2:iInt1
            r:iInt2 += p2:iInt2
            RETURN r
        ENDIF

    /// <exclude />
    OPERATOR - (p1 AS Point, p2 AS Point) AS Point
        LOCAL r AS Point
        IF p2:Empty
            RETURN p1:Clone()
        ELSE
            r := p1:Clone()
            r:iInt1 -= p2:iInt1
            r:iInt2 -= p2:iInt2
            RETURN r
        ENDIF

END CLASS

/// <include file="Gui.xml" path="doc/Range/*" />
[DebuggerStepThrough];
[DebuggerDisplay("Min: {Min}, Max: {Max}")];
CLASS Range INHERIT Pair
    /// <include file="Gui.xml" path="doc/Range.ctor/*" />
    CONSTRUCTOR(nMin, nMax)
        SUPER(nMin, nMax)
        RETURN

    /// <include file="Gui.xml" path="doc/Range.IsInRange/*" />
    METHOD IsInRange(nValue AS INT) AS LOGIC
        RETURN nValue >= iInt1 .AND. nValue <= iInt2

    /// <include file="Gui.xml" path="doc/Range.Min/*" />
    PROPERTY Min AS LONGINT  GET iInt1
    /// <include file="Gui.xml" path="doc/Range.Max/*" />
    PROPERTY Max AS LONGINT  GET iInt2

    /// <exclude />
    METHOD Clone() AS Range
        RETURN (Range) SELF:MemberwiseClone()

END CLASS

/// <include file="Gui.xml" path="doc/Selection/*" />
[DebuggerStepThrough];
[DebuggerDisplay("Start: {Start}, Finish: {Finish}")];
CLASS Selection INHERIT Pair
    /// <include file="Gui.xml" path="doc/Selection.Finish/*" />
    PROPERTY Start  AS LONGINT  GET iInt1
    /// <include file="Gui.xml" path="doc/Selection.Finish/*" />
    PROPERTY Finish AS LONGINT  GET iInt2

    /// <include file="Gui.xml" path="doc/Selection.ctor/*" />
    CONSTRUCTOR(nStart, nFinish)
        SUPER(nStart, nFinish)
        RETURN
    /// <exclude />
    METHOD Clone() AS Selection
        RETURN (Selection) SELF:MemberwiseClone()

END CLASS

