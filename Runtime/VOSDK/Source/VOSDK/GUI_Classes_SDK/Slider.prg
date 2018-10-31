PARTIAL CLASS HorizontalSelectionSlider INHERIT SelectionSlider

CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	

	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_HORZ)

	RETURN 

END CLASS

PARTIAL CLASS HorizontalSlider INHERIT Slider

CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	

	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_HORZ)

	RETURN 

END CLASS

PARTIAL CLASS SelectionSlider INHERIT Slider
	PROTECT oSelectionRange AS Range

METHOD ClearSelection() 
	

	IF (hWnd != NULL_PTR)
		SELF:SelectionRange := Range{}
		SendMessage(hWnd, TBM_CLEARSEL, 0, 0)
	ENDIF

	RETURN SELF

CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	

	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_ENABLESELRANGE)

	RETURN 

ACCESS SelectionRange 
	LOCAL hSlider AS PTR
	LOCAL nMin AS LONGINT
	LOCAL nMax AS LONGINT

	

	hSlider := SELF:Handle()
	nMin := SendMessage(hSlider, TBM_GETSELSTART, 0, 0)
	nMax := SendMessage(hSlider, TBM_GETSELEND, 0, 0)

	RETURN Range{nMin, nMax}

ASSIGN SelectionRange(oNewSelectionRange) 
	

	SendMessage(SELF:Handle(), TBM_SETSELSTART, DWORD(_CAST, FALSE), oNewSelectionRange:Min)
	SendMessage(SELF:Handle(), TBM_SETSELEND, DWORD(_CAST, TRUE), oNewSelectionRange:Max)

	RETURN 

END CLASS

PARTIAL CLASS Slider INHERIT ScrollBar
	PROTECT symTickAlignment AS SYMBOL

ACCESS BlockSize 
	

	RETURN SendMessage(SELF:Handle(), TBM_GETPAGESIZE, 0, 0)

ASSIGN BlockSize(nBlockSize) 
	

	IF !IsLong(nBlockSize) .OR. nBlockSize < 0
		WCError{#BlockSize, #Slider, __WCSTypeError, nBlockSize, 1}:@@Throw()
	ENDIF
	SendMessage(SELF:Handle(), TBM_SETPAGESIZE, 0, nBlockSize)

	RETURN 

ACCESS ChannelBoundingBox 
	LOCAL strucRect IS _winRect
	LOCAL oOrigin AS Point
	LOCAL oSize AS Dimension

	

	SendMessage(SELF:Handle(), TBM_GETCHANNELRECT, 0, LONGINT(_CAST, @strucRect))
	oOrigin := __WCConvertPoint(SELF, Point{strucRect:left, strucRect:bottom})
	oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}

	RETURN BoundingBox{oOrigin, oSize}

METHOD ClearTicks() 
	

	IF (hWnd != NULL_PTR)
		SendMessage(hWnd, TBM_CLEARTICS, 0, 0)
	ENDIF

	RETURN SELF

METHOD Create() 
	LOCAL wMin, wMax AS WORD
	IF (SUPER:Create() != NULL_PTR)
		wMin := WORD(_AND(oRange:Min,0xFFFF)) 
		wMax := WORD(_AND(oRange:Max,0xFFFF)) 
		SendMessage(hWnd, TBM_SETRANGE, DWORD(_CAST, TRUE), MakeLong(wMin, wMax))
	ENDIF

	RETURN hWnd

METHOD GetTickPos(nIndex) 
	

	RETURN SendMessage(SELF:Handle(), TBM_GETTIC, 0, nIndex)

CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	

	SUPER(oOwner, xID, oPoint, oDimension)

	SELF:SetStyle(TBS_AUTOTICKS)
	SELF:__ClassName := TRACKBAR_CLASS
	SELF:Range := Range{}

	RETURN 

ACCESS Range 
	LOCAL hSlider AS PTR
	LOCAL nMin AS LONGINT
	LOCAL nMax AS LONGINT

	

	hSlider := SELF:Handle()
	nMin := SendMessage(hSlider, TBM_GETRANGEMIN, 0, 0)
	nMax := SendMessage(hSlider, TBM_GETRANGEMAX, 0, 0)

	RETURN Range{nMin, nMax}

ASSIGN Range(oNewRange) 
	

	IF !IsInstanceOfUsual(oNewRange, #Range)
		WCError{#Range, #Slider, __WCSTypeError, oNewRange, 1}:@@Throw()
	ENDIF

	SendMessage(SELF:Handle(), TBM_SETRANGEMIN, DWORD(_CAST, FALSE), oNewRange:Min)
	SendMessage(SELF:Handle(), TBM_SETRANGEMAX, DWORD(_CAST, TRUE), oNewRange:Max)

	RETURN 

METHOD SetTickPos(nPosition) 
	

	RETURN LOGIC(_CAST, SendMessage(SELF:Handle(), TBM_SETTIC, 0, nPosition))

ACCESS ThumbBoundingBox 
	LOCAL strucRect IS _winRect
	LOCAL oOrigin AS Point
	LOCAL oSize AS Dimension

	

	SendMessage(SELF:Handle(), TBM_GETTHUMBRECT, 0, LONGINT(_CAST, @strucRect))
	oOrigin := __WCConvertPoint(SELF, Point{strucRect:left, strucRect:bottom})
	oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}

	RETURN BoundingBox{oOrigin, oSize}

ACCESS ThumbLength 
	

	RETURN SendMessage(SELF:Handle(), TBM_GETTHUMBLENGTH, 0, 0)

ASSIGN ThumbLength(nThumbLength) 
	

	SendMessage(SELF:Handle(), TBM_SETTHUMBLENGTH, nThumbLength, 0)
	RETURN 

ACCESS ThumbPosition 
	

	RETURN SendMessage(SELF:Handle(), TBM_GETPOS, 0, 0)

ASSIGN ThumbPosition(nThumbPosition) 
	

	IF !IsLong(nThumbPosition)
		WCError{#ThumbPosition, #Slider, __WCSTypeError, nThumbPosition, 1}:@@Throw()
	ENDIF
	SendMessage(SELF:Handle(), TBM_SETPOS, DWORD(_CAST, TRUE), nThumbPosition)

	RETURN 

ACCESS TickAlignment 
	

	RETURN symTickAlignment

ASSIGN TickAlignment(symNewTickAlignment) 
	

	IF IsInstanceOf(SELF, #HorizontalSlider) .OR. ISINSTANCEOF(SELF, #HorizontalSelectionSlider)
		IF symNewTickAlignment == #Top
			SELF:SetStyle(TBS_TOP)
			RETURN symTickAlignment := symNewTickAlignment
		ELSEIF symNewTickAlignment == #Bottom
			SELF:SetStyle(TBS_BOTTOM)
			RETURN symTickAlignment := symNewTickAlignment
		ELSEIF symNewTickAlignment == #Both
			SELF:SetStyle(TBS_BOTH)
			RETURN symTickAlignment := symNewTickAlignment
		ENDIF
	ELSEIF IsInstanceOf(SELF, #VerticalSlider) .OR. ISINSTANCEOF(SELF, #VerticalSelectionSlider)
		IF symNewTickAlignment == #Right
			SELF:SetStyle(TBS_RIGHT)
			RETURN symTickAlignment := symNewTickAlignment
		ELSEIF symNewTickAlignment == #LEFT
			SELF:SetStyle(TBS_LEFT)
			RETURN symTickAlignment := symNewTickAlignment
		ELSEIF symNewTickAlignment == #Both
			SELF:SetStyle(TBS_BOTH)
			RETURN symTickAlignment := symNewTickAlignment
		ENDIF
	ENDIF

	RETURN 

ACCESS TickCount 
	

	IF hWnd != 0
		RETURN SendMessage(hWnd, TBM_GETNUMTICS, 0, 0)
	ENDIF

	RETURN 0

ACCESS UnitSize 
	

	RETURN SendMessage(SELF:Handle(), TBM_GETLINESIZE, 0, 0)

ASSIGN UnitSize(nUnitSize) 
	

	IF !IsLong(nUnitSize) .OR. nUnitSize < 0
		WCError{#UnitSize, #Slider, __WCSTypeError, nUnitSize, 1}:@@Throw()
	ENDIF
	SendMessage(SELF:Handle(), TBM_SETLINESIZE, 0, nUnitSize)

	RETURN 
END CLASS

PARTIAL CLASS VerticalSelectionSlider INHERIT SelectionSlider

CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	

	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_VERT)

	RETURN 

END CLASS

PARTIAL CLASS VerticalSlider INHERIT Slider

CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	

	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_VERT)

	RETURN 

END CLASS

