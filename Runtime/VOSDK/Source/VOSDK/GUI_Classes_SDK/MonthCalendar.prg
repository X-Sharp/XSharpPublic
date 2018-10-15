PARTIAL CLASS DateRange INHERIT VObject
	PROTECT dStartDate AS DATE
	PROTECT dEndDate AS DATE

CONSTRUCTOR(dStart, dEnd) 
	Default(@dStart, Today())
	Default(@dEnd, Today())

	dStartDate := dStart
	dEndDate := dEnd
	RETURN 

ACCESS Max 
	RETURN dEndDate

ASSIGN Max(dNewVal) 
	RETURN (dEndDate := dNewVal)

ACCESS Min 
	RETURN dStartDate

ASSIGN Min(dNewVal) 
	RETURN (dStartDate := dNewVal)

END CLASS

PARTIAL CLASS MonthCalendar INHERIT TextControl

	//PP-030828 Strong typing
	METHOD __GetColor(dwColorID AS DWORD) AS color STRICT 
	//PP-030828 Strong typing
	LOCAL cr AS LONGINT

	cr := SendMessage(SELF:Handle(), MCM_GETCOLOR, dwColorID, 0L)

	RETURN Color{_AND(cr, 0xFF), _AND((cr>>8), 0xFF), _AND((cr>>16), 0xFF)}

METHOD __SetColor(oColor AS color, dwColorID AS DWORD) AS Color STRICT 
	

	SendMessage(SELF:Handle(), MCM_SETCOLOR, dwColorID, oColor:ColorRef)
	RETURN oColor

ASSIGN __Value(dValue AS USUAL)  STRICT 
	//PP-030828 Strong typing
	

	SUPER:__Value := dValue

	IF IsDate(uValue)
		SELF:Selection := uValue
	ENDIF
	RETURN 

ACCESS BackgroundColor 
	RETURN SELF:__GetColor(MCSC_BACKGROUND)

ASSIGN BackgroundColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_BACKGROUND)

METHOD Dispatch (oEvent) 
	LOCAL oEvt := oEvent AS @@Event
	//PP-030319 WM_GETDLGCODE used to support arrow keys in the control. Thanks to S Ebert

	IF oEvt:message == WM_GetDlgCode
		SELF:EventReturnValue := DLGC_WANTARROWS
		RETURN 1l
	ENDIF

	RETURN SUPER:Dispatch(oEvt)

ACCESS FirstDayOfWeek 
	RETURN LoWord(DWORD(SendMessage(SELF:Handle(), MCM_GETFIRSTDAYOFWEEK, 0, 0)))

ASSIGN FirstDayOfWeek(iNewVal) 
	SendMessage(SELF:handle(), MCM_SETFIRSTDAYOFWEEK, 0, iNewVal)
	RETURN 

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware) 
	

	Default(@lDataAware, TRUE)
	SUPER(oOwner, xID, oPoint, oDimension, "SysMonthCal32", dwStyle, lDataAware)
	RETURN 

ACCESS MaxSelCount 
	RETURN SendMessage(SELF:Handle(), MCM_GETMAXSELCOUNT, 0, 0)

ASSIGN MaxSelCount(iNewVal) 
	SendMessage(SELF:Handle(), MCM_SETMAXSELCOUNT, DWORD(_CAST, iNewVal), 0)
	RETURN 

ACCESS MonthBackgroundColor 
	RETURN SELF:__GetColor(MCSC_MONTHBK)

ASSIGN MonthBackgroundColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_MONTHBK)

ACCESS MonthDelta 
	RETURN SendMessage(SELF:Handle(), MCM_GETMONTHDELTA, 0, 0)

ASSIGN MonthDelta(iNewVal) 
	SendMessage(SELF:Handle(), MCM_SETMONTHDELTA, DWORD(_CAST, iNewVal), 0)
	RETURN 

ACCESS Range 
	LOCAL DIM dates[2] IS _winSYSTEMTIME

	IF (SendMessage(SELF:handle(), MCM_GETRANGE, 0, LONGINT(_CAST, @dates[1])) > 0)
		RETURN DateRange{ConDate(dates[1]:wYear, dates[1]:wMonth, dates[1]:wDay), ConDate(dates[2]:wYear, dates[2]:wMonth, dates[2]:wDay) }
	ENDIF

	RETURN NULL_OBJECT

ASSIGN Range(oNewRange) 
	LOCAL DIM dates[2] IS _winSYSTEMTIME

	dates[1]:wDay := WORD(Day(oNewRange:Min))
	dates[1]:wMonth := WORD(Month(oNewRange:Min))
	dates[1]:wYear := WORD(Year(oNewRange:Min))

	dates[2]:wDay := WORD(Day(oNewRange:Max))
	dates[2]:wMonth := WORD(Month(oNewRange:Max))
	dates[2]:wYear := WORD(Year(oNewRange:Max))

	SendMessage(SELF:handle(), MCM_SETRANGE, _OR(GDTR_MIN, GDTR_MAX), LONGINT(_CAST, @dates[1]))

	RETURN 

ACCESS Selection 
	LOCAL sDate IS _winSYSTEMTIME

	IF (_AND(GetWindowLong(SELF:Handle(), GWL_STYLE), MCS_MULTISELECT) == 0)
		IF (SendMessage(SELF:handle(), MCM_GETCURSEL, 0, LONGINT(_CAST, @sDate)) > 0)
			RETURN ConDate(sDate:wYear, sDate:wMonth, sDate:wDay)
		ENDIF
	ELSE
		RETURN SELF:SelectionRange:Min
	ENDIF

	RETURN NULL_DATE

ASSIGN Selection(dNewVal) 
	LOCAL sDate IS _winSYSTEMTIME

	IF (_AND(GetWindowLong(SELF:Handle(), GWL_STYLE), MCS_MULTISELECT) == 0)
		sDate:wDay := WORD(Day(dNewVal))
		sDate:wMonth := WORD(Month(dNewVal))
		sDate:wYear := WORD(Year(dNewVal))

		SendMessage(SELF:handle(), MCM_SETCURSEL, 0, LONGINT(_CAST, @sDate))
	ELSE
		SELF:SelectionRange := DateRange{dNewVal, SELF:SelectionRange:Max}
	ENDIF

	RETURN 

ACCESS SelectionRange 
	LOCAL DIM dates[2] IS _winSYSTEMTIME

	IF (SendMessage(SELF:handle(), MCM_GETSELRANGE, 0, LONGINT(_CAST, @dates[1])) > 0)
		RETURN DateRange{ConDate(dates[1]:wYear, dates[1]:wMonth, dates[1]:wDay), ConDate(dates[2]:wYear, dates[2]:wMonth, dates[2]:wDay) }
	ENDIF

	RETURN NULL_OBJECT

ASSIGN SelectionRange(oNewRange) 
	LOCAL DIM dates[2] IS _winSYSTEMTIME

	dates[1]:wDay := WORD(Day(oNewRange:Min))
	dates[1]:wMonth := WORD(Month(oNewRange:Min))
	dates[1]:wYear := WORD(Year(oNewRange:Min))

	dates[2]:wDay := WORD(Day(oNewRange:Max))
	dates[2]:wMonth := WORD(Month(oNewRange:Max))
	dates[2]:wYear := WORD(Year(oNewRange:Max))

	SendMessage(SELF:handle(), MCM_SETSELRANGE, 0, LONGINT(_CAST, @dates[1]))

	RETURN 

ACCESS TextColor 
	RETURN SELF:__GetColor(MCSC_TEXT)

ASSIGN TextColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_TEXT)

ACCESS TextValue 
	RETURN AsString(SELF:Selection)

ACCESS TitleBackgroundColor 
	RETURN SELF:__GetColor(MCSC_TITLEBK)

ASSIGN TitleBackgroundColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_TITLEBK)

ACCESS TitleTextColor 
	RETURN SELF:__GetColor(MCSC_TITLETEXT)

ASSIGN TitleTextColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_TITLETEXT)

ACCESS Today 
	LOCAL sDate IS _winSYSTEMTIME

	IF (SendMessage(SELF:handle(), MCM_GETTODAY, 0 , LONGINT(_CAST, @sDate)) > 0)
		RETURN ConDate(sDate:wYear, sDate:wMonth, sDate:wDay)
	ENDIF

	RETURN NULL_DATE

ASSIGN Today(dNewVal) 
	LOCAL sDate IS _winSYSTEMTIME

	sDate:wDay := WORD(Day(dNewVal))
	sDate:wMonth := WORD(Month(dNewVal))
	sDate:wYear := WORD(Year(dNewVal))

	SendMessage(SELF:handle(), MCM_SETTODAY, 0, LONGINT(_CAST, @sDate))

	RETURN 

ACCESS TrailingTextColor 
	RETURN SELF:__GetColor(MCSC_TRAILINGTEXT)

ASSIGN TrailingTextColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_TRAILINGTEXT)

ACCESS Value 
	uValue := SELF:Selection
	RETURN uValue
END CLASS

