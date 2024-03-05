/// <include file="Gui.xml" path="doc/HorizontalSpinner/*" />
CLASS HorizontalSpinner INHERIT Spinner


/// <include file="Gui.xml" path="doc/HorizontalSpinner.ctor/*" />
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


/// <include file="Gui.xml" path="doc/Spinner/*" />
CLASS Spinner INHERIT ScrollBar
	PROTECT oClient AS Control


/// <include file="Gui.xml" path="doc/Spinner.Client/*" />
ACCESS Client




	RETURN oClient


/// <include file="Gui.xml" path="doc/Spinner.Client/*" />
ASSIGN Client(oNewClient)




	IF !IsNil(oNewClient) .AND. (oNewClient IS Control)
		SendMessage(SELF:Handle(), UDM_SETBUDDY, DWORD(_CAST, oNewClient:Handle()), 0L)
		RETURN oClient := oNewClient
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Spinner.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle, lDataAware)




	Default(@lDataAware, TRUE)
	SUPER(oOwner, xID, oPoint, oDimension, kStyle, lDataAware)
	SELF:__ClassName := UPDOWN_CLASS


	IF !IsNil(kStyle)
		SELF:SetStyle(DWORD(kStyle))
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/Spinner.IsHexBased/*" />
ACCESS IsHexBased




	RETURN (SendMessage(SELF:Handle(), UDM_GETBASE, 0, 0) == 16)


/// <include file="Gui.xml" path="doc/Spinner.IsHexBased/*" />
ASSIGN IsHexBased(lNewValue)




	SendMessage(SELF:Handle(), UDM_SETBASE, IIF(lNewValue, 16, 10), 0)
	RETURN


/// <include file="Gui.xml" path="doc/Spinner.Position/*" />
ACCESS Position




	RETURN SELF:ThumbPosition


/// <include file="Gui.xml" path="doc/Spinner.Position/*" />
ASSIGN Position(liPos)




	RETURN SELF:ThumbPosition := liPos


/// <include file="Gui.xml" path="doc/Spinner.Range/*" />
ACCESS Range
	LOCAL iLow, iHigh AS LONG
	//LOCAL dwRes AS DWORD
	//dwRes := DWORD(_CAST, SendMessage(SELF:Handle(), UDM_GETRANGE, 0, 0))
	//RETURN Range{SHORT(_CAST, HiWord(dwRes)), SHORT(_CAST, LoWord(dwRes))}
	SendMessage(SELF:Handle(), UDM_GETRANGE32, DWORD(_CAST, @iLow), LONG(_CAST, @iHigh))
	RETURN Range{iLow, iHigh}


/// <include file="Gui.xml" path="doc/Spinner.Range/*" />
ASSIGN Range(oNewRange)
	LOCAL sUpper, sLower AS SHORTINT
	IF !(oNewRange IS Range)
		WCError{#Range, #Spinner, __WCSTypeError, oNewRange, 1}:Throw()
	ENDIF
	sUpper := oNewRange:Max
	sLower := oNewRange:Min
    SendMessage(SELF:Handle(), UDM_SETRANGE32, sLower, sUpper)


	RETURN


/// <include file="Gui.xml" path="doc/Spinner.ThumbPosition/*" />
ACCESS ThumbPosition
   RETURN SendMessage(SELF:Handle(), UDM_GETPOS32, 0, 0)


/// <include file="Gui.xml" path="doc/Spinner.ThumbPosition/*" />
ASSIGN ThumbPosition(nThumbPosition)
   	LOCAL liThumbPos AS LONG
	IF !IsLong(nThumbPosition)
		WCError{#ThumbPosition, #Spinner, __WCSTypeError, nThumbPosition, 1}:Throw()
	ENDIF
	liThumbPos := nThumbPosition


	SendMessage(SELF:Handle(), UDM_SETPOS32, 0, liThumbPos)


	RETURN




END CLASS


/// <include file="Gui.xml" path="doc/VerticalSpinner/*" />
CLASS VerticalSpinner INHERIT Spinner


/// <include file="Gui.xml" path="doc/VerticalSpinner.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)




	SUPER(oOwner, xID, oPoint, oDimension)


	IF !IsNil(kStyle)
		SELF:SetStyle(DWORD(kStyle))
	ENDIF


	RETURN
END CLASS


