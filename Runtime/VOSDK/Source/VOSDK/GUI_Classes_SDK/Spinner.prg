CLASS HorizontalSpinner INHERIT Spinner

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	

	SUPER(oOwner, xID, oPoint, oDimension, kStyle)

	IF (hWnd == NULL_PTR) //if xID is not a resourceID
		SELF:SetStyle(UDS_HORZ)
	ENDIF

	IF !IsNil(kStyle)
		SELF:SetStyle(DWORD(kStyle))
	ENDIF

	RETURN 

END CLASS

CLASS Spinner INHERIT ScrollBar
	PROTECT oClient AS Control

ACCESS Client 
	

	RETURN oClient

ASSIGN Client(oNewClient) 
	

	IF !IsNil(oNewClient) .AND. IsInstanceOfUsual(oNewClient, #Control)
		SendMessage(SELF:Handle(), UDM_SETBUDDY, DWORD(_CAST, oNewClient:Handle()), 0L)
		RETURN oClient := oNewClient
	ENDIF

	RETURN 

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle, lDataAware) 
	

	Default(@lDataAware, TRUE)
	SUPER(oOwner, xID, oPoint, oDimension, kStyle, lDataAware)
	SELF:__ClassName := UPDOWN_CLASS

	IF !IsNil(kStyle)
		SELF:SetStyle(DWORD(kStyle))
	ENDIF

	RETURN 

ACCESS IsHexBased 
	

	RETURN (SendMessage(SELF:Handle(), UDM_GETBASE, 0, 0) == 16)

ASSIGN IsHexBased(lNewValue) 
	

	SendMessage(SELF:Handle(), UDM_SETBASE, IIF(lNewValue, 16, 10), 0)
	RETURN 

ACCESS Position 
	

	RETURN SELF:ThumbPosition

ASSIGN Position(liPos) 
	

	RETURN SELF:ThumbPosition := liPos

ACCESS Range 
	LOCAL iLow, iHigh AS LONG
	//LOCAL dwRes AS DWORD                                           
	//dwRes := DWORD(_CAST, SendMessage(SELF:Handle(), UDM_GETRANGE, 0, 0))
	//RETURN Range{SHORT(_CAST, HiWord(dwRes)), SHORT(_CAST, LoWord(dwRes))}
	SendMessage(SELF:Handle(), UDM_GETRANGE32, DWORD(_CAST, @iLow), LONG(_CAST, @iHigh))
	RETURN Range{iLow, iHigh}

ASSIGN Range(oNewRange) 
	LOCAL sUpper, sLower AS SHORTINT
	IF !IsInstanceOfUsual(oNewRange, #Range)
		WCError{#Range, #Spinner, __WCSTypeError, oNewRange, 1}:Throw()
	ENDIF
	sUpper := oNewRange:Max
	sLower := oNewRange:Min
    SendMessage(SELF:Handle(), UDM_SETRANGE32, sLower, sUpper)	

	RETURN 

ACCESS ThumbPosition 
   RETURN SendMessage(SELF:Handle(), UDM_GETPOS32, 0, 0)

ASSIGN ThumbPosition(nThumbPosition) 
	IF !IsLong(nThumbPosition)
		WCError{#ThumbPosition, #Spinner, __WCSTypeError, nThumbPosition, 1}:Throw()
	ENDIF
	SendMessage(SELF:Handle(), UDM_SETPOS32, 0, nThumbPosition)

	RETURN 


END CLASS

CLASS VerticalSpinner INHERIT Spinner

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	

	SUPER(oOwner, xID, oPoint, oDimension)

	IF !IsNil(kStyle)
		SELF:SetStyle(DWORD(kStyle))
	ENDIF

	RETURN 
END CLASS

