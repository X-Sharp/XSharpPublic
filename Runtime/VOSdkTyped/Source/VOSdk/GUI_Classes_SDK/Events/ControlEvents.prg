// ControlEvent.prg



USING System.Diagnostics
USING VOSDK := XSharp.VO.SDK

CLASS ComboBoxExEndEditEvent INHERIT ControlNotifyEvent
	CONSTRUCTOR(oC AS Control) STRICT
		SUPER(oC)
	/*
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 
	SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)
	RETURN 

	ACCESS IsChanged AS LOGIC STRICT 
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT
	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	RETURN sNMCBEENDEDIT:fChanged

	ACCESS NewSelection AS LONGINT STRICT 
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT
	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	RETURN sNMCBEENDEDIT:iNewSelection + 1L

	ACCESS TextValue AS STRING STRICT 
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT
	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	IF sNMCBEENDEDIT:szText[1] = 0
	RETURN NULL_STRING
	ENDIF
	RETURN Psz2String(PSZ(_CAST, @sNMCBEENDEDIT:szText[1]))

	ACCESS Why AS LONGINT STRICT    
	//SE-060519
	LOCAL sNMCBEENDEDIT AS _winNMCBEENDEDIT



	sNMCBEENDEDIT := PTR(_CAST, SELF:lParam)
	RETURN sNMCBEENDEDIT:iWhy

	//CBENF_DROPDOWN   The user activated the drop-down list.
	//CBENF_ESCAPE     The user pressed ESC.
	//CBENF_KILLFOCUS  The edit box lost the keyboard focus.
	//CBENF_RETURN     The user completed the edit operation by pressing ENTER.
	*/
END CLASS

CLASS ControlEvent INHERIT @@Event IMPLEMENTS INamedEvent
	PROTECTED oControl   AS VOSDK.Control
	
	PROPERTY Control     AS VOSDK.Control GET oControl
	PROPERTY ControlID   AS LONG GET Control:ControlID
	PROPERTY Description AS STRING GET HyperLabel:Description
	PROPERTY HelpContext AS STRING GET HyperLabel:HelpContext
	PROPERTY HyperLabel  AS HyperLabel 
		GET 
			IF Control != NULL_OBJECT .and. Control:HyperLabel != NULL_OBJECT
				RETURN Control:HyperLabel
			ELSE
				RETURN HyperLabel{""}
			ENDIF
		END GET
	END PROPERTY
	PROPERTY Name		AS STRING GET HyperLabel:Name
	PROPERTY NameSym	AS SYMBOL GET HyperLabel:NameSym

	[DebuggerStepThrough];	
	CONSTRUCTOR(loControl AS VOSDK.Control)
		SUPER()
		oControl := loControl

	[DebuggerStepThrough];
	CONSTRUCTOR (m REF System.Windows.Forms.Message)
		SUPER(m)
		IF m:lParam != IntPtr.Zero
			oControl := WC.GetControlByHandle(m:lParam)
		ENDIF		
END CLASS


CLASS ControlFocusChangeEvent INHERIT ControlEvent
	PROTECT lGotFocus AS LOGIC
	PROPERTY GotFocus AS LOGIC GET lGotFocus
	
	[DebuggerStepThrough];
	CONSTRUCTOR(loControl AS VOSDK.Control, lFocus AS LOGIC)
		SUPER(loControl)
		lGotFocus := lFocus
	
	[DebuggerStepThrough];
	CONSTRUCTOR(oFocusChangeEvent AS FocusChangeEvent, loControl AS VOSDK.Control)
		SUPER(loControl)
		lGotFocus := oFocusChangeEvent:GotFocus

END CLASS

CLASS ControlNotifyEvent INHERIT ControlEvent
	EXPORT NotifyCode AS DWORD 
	[DebuggerStepThrough];
	CONSTRUCTOR(oC AS Control)
		SUPER(oC)
	/*
	ACCESS NotifyCode AS DWORD STRICT 
	LOCAL strucNotify AS _winNMHDR
	strucNotify := PTR(_CAST, SELF:lParam)

	RETURN  strucNotify:_code
	*/
END CLASS


CLASS DateTimeSelectionEvent INHERIT ControlEvent

	[DebuggerStepThrough];
	CONSTRUCTOR(loControl AS VOSDK.Control)
		SUPER(loControl)


	ACCESS SelectedDate AS DATE STRICT 
		LOCAL oDT AS VODateTimePicker
		oDT := (VODateTimePicker) SELF:Control:__Control
		RETURN (DATE) oDT:Value

	ACCESS SelectedTime AS STRING STRICT 
		LOCAL dt AS DateTime
		LOCAL oDT AS VODateTimePicker
		LOCAL sReturn AS STRING
		oDT := (VODateTimePicker) SELF:Control:__Control
		dt := oDT:Value
		sReturn := ConTime((DWORD)dt:Hour, (DWORD)dt:Minute, (DWORD)dt:Second)
		RETURN sReturn



END CLASS


CLASS EditFocusChangeEvent INHERIT ControlFocusChangeEvent
	[DebuggerStepThrough];
	CONSTRUCTOR(loControl AS VOSDK.Control, lFocus AS LOGIC)
		SUPER(loControl, lFocus)
		lGotFocus := lFocus

	//CONSTRUCTOR(oFocusChangeEvent AS FocusChangeEvent, loControl AS VOSDK.Control)
	//	SUPER(oFocusChangeEvent, loControl)

END CLASS

CLASS MonthCalSelectionEvent INHERIT ControlEvent
	PROTECT _lExplicit AS LOGIC

	[DebuggerStepThrough];
	CONSTRUCTOR(loControl AS VOSDK.Control, lExplicit)
		SUPER(loControl)
		_lExplicit := lExplicit
		
	ACCESS Explicit AS LOGIC
		RETURN _lExplicit

	ACCESS Selection AS DateRange STRICT 
		LOCAL oMonthCal AS MonthCalendar
		oMonthCal := (MonthCalendar) SELF:Control
		RETURN oMonthCal:Range


END CLASS



CLASS RichEditProtectEvent INHERIT ControlNotifyEvent
	//Todo
	CONSTRUCTOR(oC AS Control) STRICT
		SUPER(oC)
	/*
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 

	SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


	RETURN 

	ACCESS Selection AS Selection STRICT 
	//PP-030910
	LOCAL strucENProtect AS _winENProtected



	strucENProtect := PTR(_CAST, lParam)

	RETURN Selection{strucENProtect:chrg:cpMin + 1, strucENProtect:chrg:cpMax + 1}


	ACCESS SelectionRange AS Range STRICT 
	LOCAL strucENProtect AS _winENProtected

	strucENProtect := PTR(_CAST, lParam)

	RETURN Range{strucENProtect:chrg:cpMin + 1, strucENProtect:chrg:cpMax + 1}
	*/
END CLASS

CLASS RichEditSelectionEvent INHERIT ControlNotifyEvent
	//Todo
	CONSTRUCTOR(oC AS Control) STRICT
		SUPER(oC)
	/*
	//RvdH 061218 Declared properties for performance
	CONSTRUCTOR(_hWnd, _uMsg, _wParam, _lParam, _oWindow) 

	SUPER(_hWnd, _uMsg, _wParam, _lParam, _oWindow)


	RETURN 

	ACCESS Selection AS Selection STRICT 
	//PP-030910
	LOCAL strucSelChange AS _winSelChange

	strucSelChange := PTR(_CAST, lParam)
	RETURN Selection{strucSelChange:chrg:cpMin + 1, strucSelChange:chrg:cpMax + 1}

	ACCESS SelectionRange AS Range STRICT 
	LOCAL strucSelChange AS _winSelChange

	strucSelChange := PTR(_CAST, lParam)
	RETURN Range{strucSelChange:chrg:cpMin + 1, strucSelChange:chrg:cpMax + 1}

	ACCESS SelectionType AS LONGINT STRICT 
	LOCAL strucSelChange AS _winSelChange

	strucSelChange := PTR(_CAST, lParam)
	RETURN strucSelChange:seltyp


	*/
END CLASS


CLASS ScrollEvent INHERIT @@Event
	//Todo
	PROPERTY ScrollBar AS ScrollBar Auto
	PROPERTY Position AS LONG Auto
	
END CLASS



CLASS SliderEvent INHERIT @@Event
	//Todo
	PROPERTY Slider AS Slider Auto
	PROPERTY Position AS LONG Auto
	PROPERTY IsWindowScroll AS LOGIC GET FALSE

END CLASS

CLASS SpinnerEvent INHERIT @@Event
	//Todo
	PROPERTY Spinner AS Spinner Auto
	PROPERTY Position AS LONG Auto

/*
ACCESS OldPosition AS LONGINT STRICT 
LOCAL oS AS Spinner
//LOCAL dwTemp AS DWORD
LOCAL lOk   AS LOGIC
LOCAL lRetVal AS LONG
oS := SELF:Spinner
IF (oS != NULL_OBJECT)
RETURN oS:Position
ENDIF
IF (lParam != 0)
//RvdH 081212 Use 32 bit version to get full range of values
//dwTemp := LoWord(DWORD(SendMessage(PTR(_CAST, lParam), UDM_GetPos, 0, 0)))
//IF HiWord(dwTemp) == 0
//	RETURN LoWord(dwTemp)
//ENDIF
lRetVal := 	SendMessage(oS:Handle(), UDM_GETPOS32, 0, LONG(_CAST, @lOk))
IF (lOk)
RETURN lRetVal
ENDIF

ENDIF

RETURN 0

ACCESS OldValue AS LONGINT STRICT 
RETURN SELF:OldPosition

ACCESS Position AS LONGINT STRICT 
LOCAL oS AS Spinner
LOCAL oRange AS OBJECT
LOCAL liRetVal, wUnit AS LONG
//LOCAL dwTemp AS DWORD                                           
LOCAL iLow, iHigh AS LONG
LOCAL wLow, wHigh   AS WORD

oS := SELF:Spinner
liRetVal := SELF:OldPosition

IF (oS != NULL_OBJECT)
oRange := oS:Range
wUnit  := oS:UnitSize
ELSE
wUnit := 1
// RvdH 081212 Use 32 bit version to get whole range
//dwTemp := DWORD(SendMessage(PTR(_CAST, lParam), UDM_GETRANGE, 0, 0))
//oRange := Range{LoWord(dwTemp), HiWord(dwTemp)}
SendMessage(oS:Handle(), UDM_GETRANGE32, DWORD(_CAST, @iLow), LONG(_CAST, @iHigh))
oRange := Range{iLow, iHigh}
ENDIF

wLow    := LOWORD(wParam)
wHigh   := HIWORD(wParam)                              

DO CASE
CASE (wLow == SB_LINEDOWN)
IF (liRetVal + wUnit) > oRange:Max
liRetVal := oRange:Max
ELSE
liRetVal += wUnit
ENDIF

CASE (wLow == SB_LINEUP)
IF (liRetVal - wUnit) < oRange:Min
liRetVal := oRange:Min
ELSE
liRetVal -= wUnit
ENDIF
CASE (wLow == SB_THUMBPOSITION)
liRetVal := SHORT(_CAST,wHigh) // Note that the high word is signed !
CASE (wLow == SB_THUMBTRACK)
liRetVal := SHORT(_CAST,wHigh) // Note that the high word is signed !
ENDCASE

RETURN LONGINT(liRetVal)

ACCESS Spinner AS OBJECT STRICT 
LOCAL oS AS OBJECT
oS :=  __WCGetControlByHandle(PTR(_CAST, lParam))
IF IsInstanceOf(oS, #Spinner)
RETURN oS
ENDIF
RETURN NULL_OBJECT 

ACCESS SpinnerID AS LONGINT STRICT 
LOCAL oS AS Spinner
oS := SELF:Spinner
IF (oS != NULL_OBJECT)
RETURN oS:ControlID
ENDIF

RETURN 0

ACCESS Type AS LONGINT STRICT 
LOCAL wType AS WORD
wType := LoWord(wParam)
DO CASE
CASE (wType == SB_LINEDOWN)
RETURN UnitIncrement
CASE (wType == SB_LINEUP)
RETURN UnitDecrement
CASE (wType == SB_TOP)
RETURN ScrollToTopLeft
CASE (wType == SB_BOTTOM)
RETURN ScrollToBottomRight
ENDCASE

RETURN ScrollEnd

ACCESS Value AS LONGINT STRICT 



RETURN SELF:Position


*/		
END CLASS


CLASS SysLinkSelectEvent INHERIT ControlNotifyEvent
	//Todo
	CONSTRUCTOR(oC AS Control)
		SUPER(oC)
	//RvdH 061218 Declared properties for performance
	/*
	ACCESS ID AS STRING STRICT 
		LOCAL DIM szUrl[MAX_LINKID_TEXT] AS BYTE
		LOCAL nml AS _winNMLink
		nml := PTR(_CAST, lParam)

		WideCharToMultiByte(CP_ACP, 0, @nml:item:szID[1], -1, PSZ(_CAST, @szUrl[1]), MAX_LINKID_TEXT, NULL_PTR, NULL_PTR)
		RETURN Psz2String(@szUrl[1])

	CONSTRUCTOR(oC AS Control) 
		SUPER(oC)
		RETURN 

	ACCESS LinkIndex AS LONGINT STRICT 
		LOCAL nml AS _winNMLink
		nml := PTR(_CAST, lParam)
		RETURN nml:item:iLink

	ACCESS URL AS STRING STRICT 
		LOCAL DIM szUrl[L_MAX_URL_LENGTH] AS BYTE
		LOCAL nml AS _winNMLink
		nml := PTR(_CAST, lParam)

		WideCharToMultiByte(CP_ACP, 0, @nml:item:szUrl[1], -1, PSZ(_CAST, @szUrl[1]), L_MAX_URL_LENGTH, NULL_PTR, NULL_PTR)
		RETURN Psz2String(@szUrl[1])
	*/
END CLASS


