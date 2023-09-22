/// <include file="Gui.xml" path="doc/Dimension/*" />
class Dimension inherit Pair
    /// <include file="Gui.xml" path="doc/Dimension.Height/*" />
    access Height as longint strict
        return iInt2


    /// <include file="Gui.xml" path="doc/Dimension.Height/*" />
    assign Height(nHeight as longint)  strict
        return iInt2 := nHeight


    /// <include file="Gui.xml" path="doc/Dimension.ctor/*" />
    constructor(nWidth, nHeight)
        super(nWidth, nHeight)
        return


    /// <include file="Gui.xml" path="doc/Dimension.Width/*" />
    access Width as longint strict
        return iInt1


    /// <include file="Gui.xml" path="doc/Dimension.Width/*" />
    assign Width(nWidth as longint)  strict
        return iInt1 := nWidth


end class


/// <include file="Gui.xml" path="doc/Pair/*" />
class Pair inherit VObject
    protect iInt1 as int
    protect iInt2 as int


    /// <include file="Gui.xml" path="doc/Pair.ctor/*" />
    constructor(uInt1, uInt2)
        super()
        if !IsNil(uInt1)
            iInt1 := uInt1
        endif
        if !IsNil(uInt2)
            iInt2 := uInt2
        endif


        return


end class


/// <include file="Gui.xml" path="doc/Point/*" />
class Point inherit Pair
    /// <include file="Gui.xml" path="doc/Point.ConvertToScreen/*" />
    method ConvertToScreen(oWindow)
        local sPoint is _winPoint
        local sRect  is _winRect
        local hWnd   as ptr


        if IsPtr(oWindow)
            hWnd := oWindow
            if hWnd = null_ptr
                return false
            endif
            sRect:left := 0
        elseif oWindow is Window var oWin
            hWnd       := oWin:Handle(4)
            sRect:left := 1
        elseif oWindow is Control var oC
            hWnd       := oC:Handle()
            sRect:left := 0
        else
            return false
        endif


        sPoint:x := iInt1
        sPoint:y := iInt2


        if WCGetCoordinateSystem()
            if sRect:left = 1
                GetClientRect(hWnd, @sRect)
            else
                GetWindowRect(hWnd, @sRect)
            endif
            sPoint:y := sRect:bottom - sRect:top - sPoint:y
            ClientToScreen(hWnd, @sPoint)
            sPoint:y := GetSystemMetrics(SM_CYSCREEN) - sPoint:y
        else
            ClientToScreen(hWnd, @sPoint)
        endif


        iInt1 := sPoint:x
        iInt2 := sPoint:y
        return true


    /// <include file="Gui.xml" path="doc/Point.ctor/*" />
    constructor(nX, nY)
        super(nX, nY)
        return


    /// <include file="Gui.xml" path="doc/Point.X/*" />
    access X  as longint strict
        return iInt1


    /// <include file="Gui.xml" path="doc/Point.X/*" />
    assign X(nX as longint)   strict
        return iInt1 := nX


    /// <include file="Gui.xml" path="doc/Point.Y/*" />
    access Y  as longint strict
        return iInt2


    /// <include file="Gui.xml" path="doc/Point.Y/*" />
    assign Y(nY as longint)   strict
        return iInt2 := nY


end class


/// <include file="Gui.xml" path="doc/Range/*" />
class Range inherit Pair
    /// <include file="Gui.xml" path="doc/Range.ctor/*" />
    constructor(nMin, nMax)
        super(nMin, nMax)
        return


    /// <include file="Gui.xml" path="doc/Range.IsInRange/*" />
    method IsInRange(nValue)
        //SE-060525
        local iVal as int
        if IsNumeric(nValue)
            iVal := nValue
            if iVal >= iInt1 .and. iVal <= iInt2
                return true
            endif
        endif

        return false


    /// <include file="Gui.xml" path="doc/Range.Max/*" />
    access Max as longint strict
        return iInt2


    /// <include file="Gui.xml" path="doc/Range.Max/*" />
    assign Max(nMax as longint)  strict
        return iInt2 := nMax


    /// <include file="Gui.xml" path="doc/Range.Min/*" />
    access Min as longint strict
        return iInt1


    /// <include file="Gui.xml" path="doc/Range.Min/*" />
    assign Min(nMin as longint)  strict
        return iInt1 := nMin


end class


/// <include file="Gui.xml" path="doc/Selection/*" />
class Selection inherit Pair
    /// <include file="Gui.xml" path="doc/Selection.Finish/*" />
    access Finish as longint strict
        return iInt2

    /// <include file="Gui.xml" path="doc/Selection.Finish/*" />
    assign Finish(nFinish as longint)  strict
        return iInt2 := nFinish

    /// <include file="Gui.xml" path="doc/Selection.ctor/*" />
    constructor(nStart, nFinish)
        super(nStart, nFinish)
        return

    /// <include file="Gui.xml" path="doc/Selection.Start/*" />
    access Start as longint strict
        return iInt1


    /// <include file="Gui.xml" path="doc/Selection.Start/*" />
    assign Start(nStart as longint)  strict
        return iInt1 := nStart


end class


