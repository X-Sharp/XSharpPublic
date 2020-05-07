
CLASS HorizontalSelectionSlider INHERIT SelectionSlider


    PROPERTY Controltype AS ControlType GET ControlType.Slider

    METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
        VAR oControl := (System.Windows.Forms.TrackBar) oC
		oControl:Orientation := System.Windows.Forms.Orientation.Horizontal
		RETURN 

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
		SUPER(oOwner, xID, oPoint, oDimension)
		RETURN 

END CLASS

CLASS HorizontalSlider INHERIT Slider

    PROPERTY Controltype AS ControlType GET ControlType.Slider

    METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
        VAR oControl := (System.Windows.Forms.TrackBar) oC
		oControl:Orientation := System.Windows.Forms.Orientation.Horizontal
		RETURN 

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
		SUPER(oOwner, xID, oPoint, oDimension)
		RETURN 

END CLASS

CLASS SelectionSlider INHERIT Slider
	PROTECT oSelectionRange AS Range

    PROPERTY Controltype AS ControlType GET ControlType.Slider

	METHOD __CreateControl(liStyle AS LONG, liExStyle AS LONG) AS System.Windows.Forms.Control
		RETURN SUPER:__CreateControl(liStyle| (LONG) TBS_ENABLESELRANGE, liExStyle)


	METHOD ClearSelection() AS VOID
		// Todo
		//IF (hWnd != NULL_PTR)
		//	SELF:SelectionRange := Range{}
		//	SendMessage(hWnd, TBM_CLEARSEL, 0, 0)
		//ENDIF

		RETURN 

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
		SUPER(oOwner, xID, oPoint, oDimension)
		RETURN 

	ACCESS SelectionRange as Range
		// Todo
		//LOCAL hSlider AS PTR
		//LOCAL nMin AS LONGINT
		//LOCAL nMax AS LONGINT

		//hSlider := SELF:Handle()
		//nMin := SendMessage(hSlider, TBM_GETSELSTART, 0, 0)
		//nMax := SendMessage(hSlider, TBM_GETSELEND, 0, 0)

		//RETURN Range{nMin, nMax}
		RETURN Range{}

	ASSIGN SelectionRange(oNewSelectionRange as Range) 
		// Todo
		//SendMessage(SELF:Handle(), TBM_SETSELSTART, DWORD(_CAST, FALSE), oNewSelectionRange:Min)
		//SendMessage(SELF:Handle(), TBM_SETSELEND, DWORD(_CAST, TRUE), oNewSelectionRange:Max)

		RETURN 

END CLASS

CLASS Slider INHERIT ScrollBar
	PROTECT symTickAlignment AS SYMBOL

    PROPERTY Controltype AS ControlType GET ControlType.Slider

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
		SUPER(oOwner, xID, oPoint, oDimension)
		SELF:Range := Range{0,10}
		RETURN 

	ACCESS __TrackBar AS System.Windows.Forms.TrackBar
		RETURN (System.Windows.Forms.TrackBar) oCtrl

	PROPERTY BlockSize AS INT GET __TrackBar:LargeChange SET __TrackBar:LargeChange := Value

	ACCESS ChannelBoundingBox AS System.Drawing.Rectangle
		RETURN SELF:__TrackBar:Bounds

	METHOD ClearTicks() AS VOID
		// Todo
		RETURN


	METHOD GetTickPos(nIndex) AS LONG
		//Todo
		//RETURN SendMessage(SELF:__TrackBar:hWnd, TBM_GETTIC, 0, nIndex)
		RETURN 0



	ACCESS Range as Range
		RETURN Range{__TrackBar:Minimum, __TrackBar:Maximum}

	ASSIGN Range(oNewRange as Range) 
		__TrackBar:SetRange(oNewRange:Min, oNewRange:Max)		
		RETURN 

	METHOD SetTickPos(nPosition AS LONG) 
		RETURN __TrackBar:Value := nPosition

	ACCESS ThumbBoundingBox 
		// Todo
		//LOCAL strucRect IS _winRect
		//LOCAL oOrigin AS Point
		//LOCAL oSize AS Dimension

		//SendMessage(SELF:Handle(), TBM_GETTHUMBRECT, 0, LONGINT(_CAST, @strucRect))
		//oOrigin := __WCConvertPoint(SELF, Point{strucRect:left, strucRect:bottom})
		//oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}

		//RETURN BoundingBox{oOrigin, oSize}
		RETURN BoundingBox{}

	ACCESS ThumbLength  AS LONG
		// Todo
		//RETURN SendMessage(SELF:Handle(), TBM_GETTHUMBLENGTH, 0, 0)
		RETURN 0

	ASSIGN ThumbLength(nThumbLength AS LONG) 
		// Todo
		//SendMessage(SELF:Handle(), TBM_SETTHUMBLENGTH, nThumbLength, 0)
		RETURN 

	PROPERTY ThumbPosition AS INT GET __TrackBar:Value SET __TrackBar:Value := Value

	ACCESS TickAlignment 
		RETURN symTickAlignment

	ASSIGN TickAlignment(symNewTickAlignment) 
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
		RETURN 

	ACCESS TickCount AS LONG
		// Todo
		//IF __TrackBar != NULL_OBJECT
		//	RETURN SendMessage(__TrackBar:hWnd, TBM_GETNUMTICS, 0, 0)
		//ENDIF
		RETURN 0

	PROPERTY UnitSize AS INT GET __TrackBar:SmallChange SET __TrackBar:SmallChange := Value


END CLASS

CLASS VerticalSelectionSlider INHERIT SelectionSlider

    PROPERTY Controltype AS ControlType GET ControlType.Slider

    METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
        VAR oControl := (System.Windows.Forms.TrackBar) oC
		oControl:Orientation := System.Windows.Forms.Orientation.Vertical
		RETURN 

	METHOD __CreateControl(liStyle AS LONG, liExStyle AS LONG) AS System.Windows.Forms.Control
		RETURN SUPER:__CreateControl(liStyle| (LONG) TBS_VERT, liExStyle) 

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
		SUPER(oOwner, xID, oPoint, oDimension)
		RETURN 

END CLASS

CLASS VerticalSlider INHERIT Slider

    PROPERTY Controltype AS ControlType GET ControlType.Slider

    METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
        VAR oControl := (System.Windows.Forms.TrackBar) oC
		oControl:Orientation := System.Windows.Forms.Orientation.Vertical
		RETURN 

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
		SUPER(oOwner, xID, oPoint, oDimension)
		RETURN 

END CLASS

