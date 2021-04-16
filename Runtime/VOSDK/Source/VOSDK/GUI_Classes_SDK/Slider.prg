/// <include file="Gui.xml" path="doc/HorizontalSelectionSlider/*" />
CLASS HorizontalSelectionSlider INHERIT SelectionSlider


/// <include file="Gui.xml" path="doc/HorizontalSelectionSlider.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	
	


	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_HORZ)


	RETURN 


END CLASS


/// <include file="Gui.xml" path="doc/HorizontalSlider/*" />
CLASS HorizontalSlider INHERIT Slider


/// <include file="Gui.xml" path="doc/HorizontalSlider.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	
	


	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_HORZ)


	RETURN 


END CLASS


/// <include file="Gui.xml" path="doc/SelectionSlider/*" />
CLASS SelectionSlider INHERIT Slider
	PROTECT oSelectionRange AS Range


/// <include file="Gui.xml" path="doc/SelectionSlider.ClearSelection/*" />
METHOD ClearSelection() 
	
	


	IF (hWnd != NULL_PTR)
		SELF:SelectionRange := Range{}
		SendMessage(hWnd, TBM_CLEARSEL, 0, 0)
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/SelectionSlider.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	
	


	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_ENABLESELRANGE)


	RETURN 


/// <include file="Gui.xml" path="doc/SelectionSlider.SelectionRange/*" />
ACCESS SelectionRange 
	LOCAL hSlider AS PTR
	LOCAL nMin AS LONGINT
	LOCAL nMax AS LONGINT


	
	


	hSlider := SELF:Handle()
	nMin := SendMessage(hSlider, TBM_GETSELSTART, 0, 0)
	nMax := SendMessage(hSlider, TBM_GETSELEND, 0, 0)


	RETURN Range{nMin, nMax}


/// <include file="Gui.xml" path="doc/SelectionSlider.SelectionRange/*" />
ASSIGN SelectionRange(oNewSelectionRange) 
	
	


	SendMessage(SELF:Handle(), TBM_SETSELSTART, 0, oNewSelectionRange:Min)
	SendMessage(SELF:Handle(), TBM_SETSELEND, 1, oNewSelectionRange:Max)


	RETURN 


END CLASS


/// <include file="Gui.xml" path="doc/Slider/*" />
CLASS Slider INHERIT ScrollBar
	PROTECT symTickAlignment AS SYMBOL


/// <include file="Gui.xml" path="doc/Slider.BlockSize/*" />
ACCESS BlockSize 
	
	


	RETURN SendMessage(SELF:Handle(), TBM_GETPAGESIZE, 0, 0)


/// <include file="Gui.xml" path="doc/Slider.BlockSize/*" />
ASSIGN BlockSize(nBlockSize) 
	
	


	IF !IsLong(nBlockSize) .OR. nBlockSize < 0
		WCError{#BlockSize, #Slider, __WCSTypeError, nBlockSize, 1}:Throw()
	ENDIF
	SendMessage(SELF:Handle(), TBM_SETPAGESIZE, 0, nBlockSize)


	RETURN 


/// <include file="Gui.xml" path="doc/Slider.ChannelBoundingBox/*" />
ACCESS ChannelBoundingBox 
	LOCAL strucRect IS _winRect
	LOCAL oOrigin AS Point
	LOCAL oSize AS Dimension


	
	


	SendMessage(SELF:Handle(), TBM_GETCHANNELRECT, 0, LONGINT(_CAST, @strucRect))
	oOrigin := __WCConvertPoint(SELF, Point{strucRect:left, strucRect:bottom})
	oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}


	RETURN BoundingBox{oOrigin, oSize}


/// <include file="Gui.xml" path="doc/Slider.ClearTicks/*" />
METHOD ClearTicks() 
	
	


	IF (hWnd != NULL_PTR)
		SendMessage(hWnd, TBM_CLEARTICS, 0, 0)
	ENDIF


	RETURN SELF


/// <include file="Gui.xml" path="doc/Slider.Create/*" />
METHOD Create() 
	LOCAL wMin, wMax AS WORD
	IF (SUPER:Create() != NULL_PTR)
		wMin := WORD(_AND(oRange:Min,0xFFFF)) 
		wMax := WORD(_AND(oRange:Max,0xFFFF)) 
		SendMessage(hWnd, TBM_SETRANGE, 1, MakeLong(wMin, wMax))
	ENDIF


	RETURN hWnd


/// <include file="Gui.xml" path="doc/Slider.GetTickPos/*" />
METHOD GetTickPos(nIndex) 
	
	


	RETURN SendMessage(SELF:Handle(), TBM_GETTIC, 0, nIndex)


/// <include file="Gui.xml" path="doc/Slider.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	
	


	SUPER(oOwner, xID, oPoint, oDimension)


	SELF:SetStyle(TBS_AUTOTICKS)
	SELF:__ClassName := TRACKBAR_CLASS
	SELF:Range := Range{}


	RETURN 


/// <include file="Gui.xml" path="doc/Slider.Range/*" />
ACCESS Range 
	LOCAL hSlider AS PTR
	LOCAL nMin AS LONGINT
	LOCAL nMax AS LONGINT


	
	


	hSlider := SELF:Handle()
	nMin := SendMessage(hSlider, TBM_GETRANGEMIN, 0, 0)
	nMax := SendMessage(hSlider, TBM_GETRANGEMAX, 0, 0)


	RETURN Range{nMin, nMax}


/// <include file="Gui.xml" path="doc/Slider.Range/*" />
ASSIGN Range(oNewRange) 
	
	


	IF !IsInstanceOfUsual(oNewRange, #Range)
		WCError{#Range, #Slider, __WCSTypeError, oNewRange, 1}:Throw()
	ENDIF


	SendMessage(SELF:Handle(), TBM_SETRANGEMIN, 0, oNewRange:Min)
	SendMessage(SELF:Handle(), TBM_SETRANGEMAX, 1, oNewRange:Max)


	RETURN 


/// <include file="Gui.xml" path="doc/Slider.SetTickPos/*" />
METHOD SetTickPos(nPosition) 
	
	


	RETURN LOGIC(_CAST, SendMessage(SELF:Handle(), TBM_SETTIC, 0, nPosition))


/// <include file="Gui.xml" path="doc/Slider.ThumbBoundingBox/*" />
ACCESS ThumbBoundingBox 
	LOCAL strucRect IS _winRect
	LOCAL oOrigin AS Point
	LOCAL oSize AS Dimension


	
	


	SendMessage(SELF:Handle(), TBM_GETTHUMBRECT, 0, LONGINT(_CAST, @strucRect))
	oOrigin := __WCConvertPoint(SELF, Point{strucRect:left, strucRect:bottom})
	oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}


	RETURN BoundingBox{oOrigin, oSize}


/// <include file="Gui.xml" path="doc/Slider.ThumbLength/*" />
ACCESS ThumbLength 
	
	


	RETURN SendMessage(SELF:Handle(), TBM_GETTHUMBLENGTH, 0, 0)


/// <include file="Gui.xml" path="doc/Slider.ThumbLength/*" />
ASSIGN ThumbLength(nThumbLength) 
	
	


	SendMessage(SELF:Handle(), TBM_SETTHUMBLENGTH, nThumbLength, 0)
	RETURN 


/// <include file="Gui.xml" path="doc/Slider.ThumbPosition/*" />
ACCESS ThumbPosition 
	
	


	RETURN SendMessage(SELF:Handle(), TBM_GETPOS, 0, 0)


/// <include file="Gui.xml" path="doc/Slider.ThumbPosition/*" />
ASSIGN ThumbPosition(nThumbPosition) 
	
	


	IF !IsLong(nThumbPosition)
		WCError{#ThumbPosition, #Slider, __WCSTypeError, nThumbPosition, 1}:Throw()
	ENDIF
	SendMessage(SELF:Handle(), TBM_SETPOS, 1, nThumbPosition)


	RETURN 


/// <include file="Gui.xml" path="doc/Slider.TickAlignment/*" />
ACCESS TickAlignment 
	
	


	RETURN symTickAlignment


/// <include file="Gui.xml" path="doc/Slider.TickAlignment/*" />
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


/// <include file="Gui.xml" path="doc/Slider.TickCount/*" />
ACCESS TickCount 
	
	


	IF hWnd != 0
		RETURN SendMessage(hWnd, TBM_GETNUMTICS, 0, 0)
	ENDIF


	RETURN 0


/// <include file="Gui.xml" path="doc/Slider.UnitSize/*" />
ACCESS UnitSize 
	
	


	RETURN SendMessage(SELF:Handle(), TBM_GETLINESIZE, 0, 0)


/// <include file="Gui.xml" path="doc/Slider.UnitSize/*" />
ASSIGN UnitSize(nUnitSize) 
	
	


	IF !IsLong(nUnitSize) .OR. nUnitSize < 0
		WCError{#UnitSize, #Slider, __WCSTypeError, nUnitSize, 1}:Throw()
	ENDIF
	SendMessage(SELF:Handle(), TBM_SETLINESIZE, 0, nUnitSize)


	RETURN 
END CLASS


/// <include file="Gui.xml" path="doc/VerticalSelectionSlider/*" />
CLASS VerticalSelectionSlider INHERIT SelectionSlider


/// <include file="Gui.xml" path="doc/VerticalSelectionSlider.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	
	


	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_VERT)


	RETURN 


END CLASS


/// <include file="Gui.xml" path="doc/VerticalSlider/*" />
CLASS VerticalSlider INHERIT Slider


/// <include file="Gui.xml" path="doc/VerticalSlider.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension) 
	
	


	SUPER(oOwner, xID, oPoint, oDimension)
	SELF:SetStyle(TBS_VERT)


	RETURN 


END CLASS


