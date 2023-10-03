//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/HorizontalSelectionSlider/*" />
CLASS HorizontalSelectionSlider INHERIT SelectionSlider

    /// <exclude />
    PROPERTY Controltype AS ControlType GET ControlType.Slider

    /// <exclude />
    METHOD OnControlCreated(oC AS IVOControl) AS VOID
        VAR oControl := (System.Windows.Forms.TrackBar) oC
        oControl:Orientation := System.Windows.Forms.Orientation.Horizontal
        RETURN

    /// <include file="Gui.xml" path="doc/HorizontalSelectionSlider.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension)
        SUPER(oOwner, xID, oPoint, oDimension)
        RETURN

END CLASS

/// <include file="Gui.xml" path="doc/HorizontalSlider/*" />
CLASS HorizontalSlider INHERIT Slider

    /// <exclude />
    PROPERTY Controltype AS ControlType GET ControlType.Slider

    /// <exclude />
    METHOD OnControlCreated(oC AS IVOControl) AS VOID
        VAR oControl := (System.Windows.Forms.TrackBar) oC
        oControl:Orientation := System.Windows.Forms.Orientation.Horizontal
        RETURN

    /// <include file="Gui.xml" path="doc/HorizontalSlider.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension)
        SUPER(oOwner, xID, oPoint, oDimension)
        RETURN

END CLASS

/// <include file="Gui.xml" path="doc/SelectionSlider/*" />
CLASS SelectionSlider INHERIT Slider
    PROTECT oSelectionRange AS Range

    /// <exclude />
    PROPERTY Controltype AS ControlType GET ControlType.Slider

    /// <exclude />
    method __CreateControl(liStyle as long, liExStyle as long) as System.Windows.Forms.Control
        RETURN SUPER:__CreateControl(liStyle| (LONG) TBS_ENABLESELRANGE, liExStyle)

    /// <include file="Gui.xml" path="doc/SelectionSlider.ClearSelection/*" />
    METHOD ClearSelection() AS VOID
        // Todo SelectionSlider.ClearSelection
        //IF (hWnd != NULL_PTR)
        //	SELF:SelectionRange := Range{}
        //	SendMessage(hWnd, TBM_CLEARSEL, 0, 0)
        //ENDIF

        RETURN

    /// <include file="Gui.xml" path="doc/SelectionSlider.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension)
        SUPER(oOwner, xID, oPoint, oDimension)
        RETURN

    /// <include file="Gui.xml" path="doc/SelectionSlider.SelectionRange/*" />
    ACCESS SelectionRange as Range
        // Todo SelectionRange
        //LOCAL hSlider AS PTR
        //LOCAL nMin AS LONGINT
        //LOCAL nMax AS LONGINT

        //hSlider := SELF:Handle()
        //nMin := SendMessage(hSlider, TBM_GETSELSTART, 0, 0)
        //nMax := SendMessage(hSlider, TBM_GETSELEND, 0, 0)

        //RETURN Range{nMin, nMax}
        RETURN Range{}

    /// <include file="Gui.xml" path="doc/SelectionSlider.SelectionRange/*" />
    ASSIGN SelectionRange(oNewSelectionRange as Range)
        // Todo SelectionRange
        //SendMessage(SELF:Handle(), TBM_SETSELSTART, DWORD(_CAST, FALSE), oNewSelectionRange:Min)
        //SendMessage(SELF:Handle(), TBM_SETSELEND, DWORD(_CAST, TRUE), oNewSelectionRange:Max)

        RETURN

END CLASS

ABSTRACT CLASS Slider INHERIT ScrollBar
    PROTECT symTickAlignment AS SYMBOL

    /// <exclude />
    PROPERTY Controltype AS ControlType GET ControlType.Slider

    /// <include file="Gui.xml" path="doc/Slider.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension)
        SUPER(oOwner, xID, oPoint, oDimension)
        SELF:Range := Range{0,10}
        RETURN

    /// <exclude />
    ACCESS __TrackBar AS System.Windows.Forms.TrackBar
        RETURN (System.Windows.Forms.TrackBar) oCtrl

    /// <include file="Gui.xml" path="doc/Slider.BlockSize/*" />
    PROPERTY BlockSize AS INT GET __TrackBar:LargeChange SET __TrackBar:LargeChange := Value

    /// <include file="Gui.xml" path="doc/Slider.ChannelBoundingBox/*" />
    PROPERTY ChannelBoundingBox AS System.Drawing.Rectangle GET SELF:__TrackBar:Bounds

    /// <include file="Gui.xml" path="doc/Slider.ClearTicks/*" />
    METHOD ClearTicks() AS VOID
        // Todo Slider.ClearTicks
        RETURN


    /// <include file="Gui.xml" path="doc/Slider.GetTickPos/*" />
    METHOD GetTickPos(nIndex) AS LONG
        //Todo GetTickPos
        //RETURN SendMessage(SELF:__TrackBar:hWnd, TBM_GETTIC, 0, nIndex)
        RETURN 0

    /// <include file="Gui.xml" path="doc/Slider.Range/*" />
    PROPERTY Range as Range GET Range{__TrackBar:Minimum, __TrackBar:Maximum} SET __TrackBar:SetRange(value:Min, value:Max)

    /// <include file="Gui.xml" path="doc/Slider.SetTickPos/*" />
    METHOD SetTickPos(nPosition AS LONG) AS VOID
        __TrackBar:Value := nPosition

    /// <include file="Gui.xml" path="doc/Slider.ThumbBoundingBox/*" />
    ACCESS ThumbBoundingBox AS BoundingBox
        // Todo ThumbBoundingBox
        //LOCAL strucRect IS _winRect
        //LOCAL oOrigin AS Point
        //LOCAL oSize AS Dimension

        //SendMessage(SELF:Handle(), TBM_GETTHUMBRECT, 0, LONGINT(_CAST, @strucRect))
        //oOrigin := __WCConvertPoint(SELF, Point{strucRect:left, strucRect:bottom})
        //oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}

        //RETURN BoundingBox{oOrigin, oSize}
        RETURN BoundingBox{}

    /// <include file="Gui.xml" path="doc/Slider.ThumbLength/*" />
    ACCESS ThumbLength  AS LONG
        // Todo ThumbLength
        //RETURN SendMessage(SELF:Handle(), TBM_GETTHUMBLENGTH, 0, 0)
        RETURN 0

    /// <include file="Gui.xml" path="doc/Slider.ThumbLength/*" />
    ASSIGN ThumbLength(nThumbLength AS LONG)
        // Todo ThumbLength
        //SendMessage(SELF:Handle(), TBM_SETTHUMBLENGTH, nThumbLength, 0)
        RETURN

    /// <include file="Gui.xml" path="doc/Slider.ThumbPosition/*" />
    PROPERTY ThumbPosition AS INT GET __TrackBar:Value SET __TrackBar:Value := Value

    /// <include file="Gui.xml" path="doc/Slider.BlockSize/*" />
    PROPERTY TickAlignment AS Symbol
        GET
            RETURN symTickAlignment
        END GET
        SET
            var symNewTickAlignment := value
        IF __TrackBar:Orientation == System.Windows.Forms.Orientation.Horizontal
            IF symNewTickAlignment == #Top
                __TrackBar:TickStyle := System.Windows.Forms.TickStyle.TopLeft
                symTickAlignment := symNewTickAlignment
            ELSEIF symNewTickAlignment == #Bottom
                __TrackBar:TickStyle := System.Windows.Forms.TickStyle.BottomRight
                symTickAlignment := symNewTickAlignment
            ELSEIF symNewTickAlignment == #Both
                __TrackBar:TickStyle := System.Windows.Forms.TickStyle.Both
                symTickAlignment := symNewTickAlignment
            ENDIF
        ELSEIF __TrackBar:Orientation == System.Windows.Forms.Orientation.Vertical
            IF symNewTickAlignment == #Right
                __TrackBar:TickStyle := System.Windows.Forms.TickStyle.BottomRight
                symTickAlignment := symNewTickAlignment
            ELSEIF symNewTickAlignment == #LEFT
                __TrackBar:TickStyle := System.Windows.Forms.TickStyle.TopLeft
                symTickAlignment := symNewTickAlignment
            ELSEIF symNewTickAlignment == #Both
                __TrackBar:TickStyle := System.Windows.Forms.TickStyle.Both
                symTickAlignment := symNewTickAlignment
            ENDIF
        ENDIF
        end set
        end property

    /// <include file="Gui.xml" path="doc/Slider.TickCount/*" />
    ACCESS TickCount AS LONG
        // Todo TickCount
        //IF __TrackBar != NULL_OBJECT
        //	RETURN SendMessage(__TrackBar:hWnd, TBM_GETNUMTICS, 0, 0)
        //ENDIF
        RETURN 0

    /// <include file="Gui.xml" path="doc/Slider.UnitSize/*" />
    PROPERTY UnitSize AS INT GET __TrackBar:SmallChange SET __TrackBar:SmallChange := Value


END CLASS

CLASS VerticalSelectionSlider INHERIT SelectionSlider

    /// <exclude />
    PROPERTY Controltype AS ControlType GET ControlType.Slider

    /// <exclude />
    METHOD OnControlCreated(oC AS IVOControl) AS VOID
        VAR oControl := (System.Windows.Forms.TrackBar) oC
        oControl:Orientation := System.Windows.Forms.Orientation.Vertical
        RETURN

    /// <exclude />
    METHOD __CreateControl(liStyle AS LONG, liExStyle AS LONG) AS System.Windows.Forms.Control
        RETURN SUPER:__CreateControl(liStyle| (LONG) TBS_VERT, liExStyle)

    /// <include file="Gui.xml" path="doc/VerticalSelectionSlider.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension)
        SUPER(oOwner, xID, oPoint, oDimension)
        RETURN

END CLASS

CLASS VerticalSlider INHERIT Slider

    /// <exclude />
    PROPERTY Controltype AS ControlType GET ControlType.Slider

    /// <exclude />
    METHOD OnControlCreated(oC AS IVOControl) AS VOID
        VAR oControl := (System.Windows.Forms.TrackBar) oC
        oControl:Orientation := System.Windows.Forms.Orientation.Vertical
        RETURN

    /// <include file="Gui.xml" path="doc/VerticalSlider.ctor/*" />
    CONSTRUCTOR(oOwner, xID, oPoint, oDimension)
        SUPER(oOwner, xID, oPoint, oDimension)
        RETURN

END CLASS

