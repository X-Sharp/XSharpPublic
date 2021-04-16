/// <include file="Gui.xml" path="doc/DateRange/*" />
CLASS DateRange INHERIT VObject
	PROTECT dStartDate AS DATE
	PROTECT dEndDate AS DATE


/// <include file="Gui.xml" path="doc/DateRange.ctor/*" />
CONSTRUCTOR(dStart, dEnd) 
	Default(@dStart, Today())
	Default(@dEnd, Today())


	dStartDate := dStart
	dEndDate := dEnd
	RETURN 


/// <include file="Gui.xml" path="doc/DateRange.Max/*" />
ACCESS Max 
	RETURN dEndDate


/// <include file="Gui.xml" path="doc/DateRange.Max/*" />
ASSIGN Max(dNewVal) 
	RETURN (dEndDate := dNewVal)


/// <include file="Gui.xml" path="doc/DateRange.Min/*" />
ACCESS Min 
	RETURN dStartDate


/// <include file="Gui.xml" path="doc/DateRange.Min/*" />
ASSIGN Min(dNewVal) 
	RETURN (dStartDate := dNewVal)


END CLASS


/// <include file="Gui.xml" path="doc/MonthCalendar/*" />
CLASS MonthCalendar INHERIT TextControl


	//PP-030828 Strong typing
 /// <exclude />
	METHOD __GetColor(dwColorID AS DWORD) AS color STRICT 
	//PP-030828 Strong typing
	LOCAL cr AS LONGINT


	cr := SendMessage(SELF:Handle(), MCM_GETCOLOR, dwColorID, 0L)


	RETURN Color{_AND(cr, 0xFF), _AND((cr>>8), 0xFF), _AND((cr>>16), 0xFF)}


 /// <exclude />
METHOD __SetColor(oColor AS color, dwColorID AS DWORD) AS Color STRICT 
	
	


	SendMessage(SELF:Handle(), MCM_SETCOLOR, dwColorID, oColor:ColorRef)
	RETURN oColor


 /// <exclude />
ASSIGN __Value(dValue AS USUAL)  STRICT 
	//PP-030828 Strong typing
	
	


	SUPER:__Value := dValue


	IF IsDate(uValue)
		SELF:Selection := uValue
	ENDIF
	RETURN 


/// <include file="Gui.xml" path="doc/MonthCalendar.BackgroundColor/*" />
ACCESS BackgroundColor 
	RETURN SELF:__GetColor(MCSC_BACKGROUND)


/// <include file="Gui.xml" path="doc/MonthCalendar.BackgroundColor/*" />
ASSIGN BackgroundColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_BACKGROUND)


/// <include file="Gui.xml" path="doc/MonthCalendar.Dispatch/*" />
METHOD Dispatch (oEvent) 
	LOCAL oEvt := oEvent AS @@Event
	//PP-030319 WM_GETDLGCODE used to support arrow keys in the control. Thanks to S Ebert


	IF oEvt:message == WM_GetDlgCode
		SELF:EventReturnValue := DLGC_WANTARROWS
		RETURN 1l
	ENDIF


	RETURN SUPER:Dispatch(oEvt)


/// <include file="Gui.xml" path="doc/MonthCalendar.FirstDayOfWeek/*" />
ACCESS FirstDayOfWeek 
	RETURN LoWord(DWORD(SendMessage(SELF:Handle(), MCM_GETFIRSTDAYOFWEEK, 0, 0)))


/// <include file="Gui.xml" path="doc/MonthCalendar.FirstDayOfWeek/*" />
ASSIGN FirstDayOfWeek(iNewVal) 
	SendMessage(SELF:handle(), MCM_SETFIRSTDAYOFWEEK, 0, iNewVal)
	RETURN 


/// <include file="Gui.xml" path="doc/MonthCalendar.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware) 
	
	


	Default(@lDataAware, TRUE)
	SUPER(oOwner, xID, oPoint, oDimension, "SysMonthCal32", dwStyle, lDataAware)
	RETURN 


/// <include file="Gui.xml" path="doc/MonthCalendar.MaxSelCount/*" />
ACCESS MaxSelCount 
	RETURN SendMessage(SELF:Handle(), MCM_GETMAXSELCOUNT, 0, 0)


/// <include file="Gui.xml" path="doc/MonthCalendar.MaxSelCount/*" />
ASSIGN MaxSelCount(iNewVal)
    EnForceNumeric(@iNewVal)
	SendMessage(SELF:Handle(), MCM_SETMAXSELCOUNT, DWORD(_CAST, iNewVal), 0)
	RETURN 


/// <include file="Gui.xml" path="doc/MonthCalendar.MonthBackgroundColor/*" />
ACCESS MonthBackgroundColor 
	RETURN SELF:__GetColor(MCSC_MONTHBK)


/// <include file="Gui.xml" path="doc/MonthCalendar.MonthBackgroundColor/*" />
ASSIGN MonthBackgroundColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_MONTHBK)


/// <include file="Gui.xml" path="doc/MonthCalendar.MonthDelta/*" />
ACCESS MonthDelta 
	RETURN SendMessage(SELF:Handle(), MCM_GETMONTHDELTA, 0, 0)


/// <include file="Gui.xml" path="doc/MonthCalendar.MonthDelta/*" />
ASSIGN MonthDelta(iNewVal) 
	SendMessage(SELF:Handle(), MCM_SETMONTHDELTA, DWORD(_CAST, iNewVal), 0)
	RETURN 


/// <include file="Gui.xml" path="doc/MonthCalendar.Range/*" />
ACCESS Range 
	LOCAL DIM dates[2] IS _winSYSTEMTIME


	IF (SendMessage(SELF:handle(), MCM_GETRANGE, 0, LONGINT(_CAST, @dates[1])) > 0)
		RETURN DateRange{ConDate(dates[1]:wYear, dates[1]:wMonth, dates[1]:wDay), ConDate(dates[2]:wYear, dates[2]:wMonth, dates[2]:wDay) }
	ENDIF


	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/MonthCalendar.Range/*" />
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


/// <include file="Gui.xml" path="doc/MonthCalendar.Selection/*" />
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


/// <include file="Gui.xml" path="doc/MonthCalendar.Selection/*" />
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


/// <include file="Gui.xml" path="doc/MonthCalendar.SelectionRange/*" />
ACCESS SelectionRange 
	LOCAL DIM dates[2] IS _winSYSTEMTIME


	IF (SendMessage(SELF:handle(), MCM_GETSELRANGE, 0, LONGINT(_CAST, @dates[1])) > 0)
		RETURN DateRange{ConDate(dates[1]:wYear, dates[1]:wMonth, dates[1]:wDay), ConDate(dates[2]:wYear, dates[2]:wMonth, dates[2]:wDay) }
	ENDIF


	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/MonthCalendar.SelectionRange/*" />
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


/// <include file="Gui.xml" path="doc/MonthCalendar.TextColor/*" />
ACCESS TextColor 
	RETURN SELF:__GetColor(MCSC_TEXT)


/// <include file="Gui.xml" path="doc/MonthCalendar.TextColor/*" />
ASSIGN TextColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_TEXT)


/// <include file="Gui.xml" path="doc/MonthCalendar.TextValue/*" />
ACCESS TextValue 
	RETURN AsString(SELF:Selection)


/// <include file="Gui.xml" path="doc/MonthCalendar.TitleBackgroundColor/*" />
ACCESS TitleBackgroundColor 
	RETURN SELF:__GetColor(MCSC_TITLEBK)


/// <include file="Gui.xml" path="doc/MonthCalendar.TitleBackgroundColor/*" />
ASSIGN TitleBackgroundColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_TITLEBK)


/// <include file="Gui.xml" path="doc/MonthCalendar.TitleTextColor/*" />
ACCESS TitleTextColor 
	RETURN SELF:__GetColor(MCSC_TITLETEXT)


/// <include file="Gui.xml" path="doc/MonthCalendar.TitleTextColor/*" />
ASSIGN TitleTextColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_TITLETEXT)


/// <include file="Gui.xml" path="doc/MonthCalendar.Today/*" />
ACCESS Today 
	LOCAL sDate IS _winSYSTEMTIME


	IF (SendMessage(SELF:handle(), MCM_GETTODAY, 0 , LONGINT(_CAST, @sDate)) > 0)
		RETURN ConDate(sDate:wYear, sDate:wMonth, sDate:wDay)
	ENDIF


	RETURN NULL_DATE


/// <include file="Gui.xml" path="doc/MonthCalendar.Today/*" />
ASSIGN Today(dNewVal) 
	LOCAL sDate IS _winSYSTEMTIME


	sDate:wDay := WORD(Day(dNewVal))
	sDate:wMonth := WORD(Month(dNewVal))
	sDate:wYear := WORD(Year(dNewVal))


	SendMessage(SELF:handle(), MCM_SETTODAY, 0, LONGINT(_CAST, @sDate))


	RETURN 


/// <include file="Gui.xml" path="doc/MonthCalendar.TrailingTextColor/*" />
ACCESS TrailingTextColor 
	RETURN SELF:__GetColor(MCSC_TRAILINGTEXT)


/// <include file="Gui.xml" path="doc/MonthCalendar.TrailingTextColor/*" />
ASSIGN TrailingTextColor(oColor) 
	RETURN SELF:__SetColor(oColor, MCSC_TRAILINGTEXT)


/// <include file="Gui.xml" path="doc/MonthCalendar.Value/*" />
ACCESS Value 
	uValue := SELF:Selection
	RETURN uValue
END CLASS


