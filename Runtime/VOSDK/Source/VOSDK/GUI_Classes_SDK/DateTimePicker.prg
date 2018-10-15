PARTIAL CLASS DateTimePicker INHERIT TextControl
    HIDDEN _lNoAssign AS LOGIC   
    PROTECT lNullFormat       AS LOGIC
    PROTECT cOldFormat        AS STRING
    PROTECT cFormat           AS STRING

	//PP-030828 Strong typing
METHOD __Gather() AS LOGIC STRICT 
  //SE-040929 DTS_SHOWNONE Workaround
  LOCAL lReturn AS LOGIC

  _lNoAssign := TRUE
  lReturn := SUPER:__Gather()
  _lNoAssign := FALSE

  RETURN lReturn


METHOD __GetMCColor(dwColorID AS DWORD) AS Color STRICT 
	//PP-030828 Strong typing
	LOCAL cr AS LONGINT

	cr := SendMessage(SELF:Handle(), DTM_GETMCCOLOR, dwColorID, 0L)

	RETURN Color{_AND(cr, 0xFF), _AND((cr>>8), 0xFF), _AND((cr>>16), 0xFF)}

METHOD __SetMCColor(oColor AS Color, dwColorID AS DWORD) AS Color STRICT 
	//PP-030828 Strong typing

	SendMessage(SELF:Handle(), DTM_SETMCCOLOR, dwColorID, oColor:ColorRef)
	RETURN oColor

ASSIGN __Value(uValue AS USUAL)  STRICT 
	//PP-030828 Strong typing
	//SE-040929 DTS_SHOWNONE Workaround

	IF ! _lNoAssign

	   SUPER:__Value := uValue

   	IF IsDate(uValue)
   		SELF:SelectedDate := uValue
   	ELSEIF IsString(uValue)
   		SELF:SelectedTime := uValue
   	ENDIF
   ENDIF
	RETURN 

ACCESS DateRange 
	LOCAL DIM dates[2] IS _winSYSTEMTIME

	IF (SendMessage(SELF:handle(), DTM_GETRANGE, 0, LONGINT(_CAST, @dates[1])) > 0)
		RETURN DateRange{ConDate(dates[1]:wYear, dates[1]:wMonth, dates[1]:wDay), ConDate(dates[2]:wYear, dates[2]:wMonth, dates[2]:wDay) }
	ENDIF

	RETURN NULL_OBJECT

ASSIGN DateRange(oNewRange) 
	LOCAL DIM dates[2] IS _winSYSTEMTIME

	dates[1]:wDay := WORD(Day(oNewRange:Min))
	dates[1]:wMonth := WORD(Month(oNewRange:Min))
	dates[1]:wYear := WORD(Year(oNewRange:Min))

	dates[2]:wDay := WORD(Day(oNewRange:Max))
	dates[2]:wMonth := WORD(Month(oNewRange:Max))
	dates[2]:wYear := WORD(Year(oNewRange:Max))
   // RvdH 090212 This makes sure that every time on the Max-Date is valid 
   dates[2]:wHour := 32
   dates[2]:wMinute := 59
   dates[2]:wSecond := 59
   dates[2]:wMilliseconds := 999


	SendMessage(SELF:handle(), DTM_SETRANGE, _OR(GDTR_MIN, GDTR_MAX), LONGINT(_CAST, @dates[1]))


ACCESS Format
    RETURN cFormat

ASSIGN Format(sNewFormat) 
    cFormat := sNewFormat
    IF (sNewFormat != "''")
        cOldFormat := sNewFormat
    ENDIF
    SendMessage(SELF:Handle(), DTM_SETFORMAT, 0, LONG(_CAST, String2Psz(sNewFormat)))

	RETURN 

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware) 
	

	Default(@lDataAware, TRUE)

	SUPER(oOwner, xID, oPoint, oDimension, "SysDateTimePick32", dwStyle, lDataAware)
	RETURN 

ACCESS MCBackgroundColor 
	RETURN SELF:__GetMCColor(MCSC_BACKGROUND)

ASSIGN MCBackgroundColor(oColor) 
	RETURN SELF:__SetMCColor(oColor, MCSC_BACKGROUND)

ASSIGN MCFont(oNewFont) 
	SendMessage(SELF:handle(), DTM_SETMCFONT, DWORD(_CAST, oNewFont:Handle()), LONGINT(_CAST, TRUE))
	RETURN 

ACCESS MCMonthBackgroundColor 
	RETURN SELF:__GetMCColor(MCSC_MONTHBK)

ASSIGN MCMonthBackgroundColor(oColor) 
	RETURN SELF:__SetMCColor(oColor, MCSC_MONTHBK)

ACCESS MCTextColor 
	RETURN SELF:__GetMCColor(MCSC_TEXT)

ASSIGN MCTextColor(oColor) 
	RETURN SELF:__SetMCColor(oColor, MCSC_TEXT)

ACCESS MCTitleBackgroundColor 
	RETURN SELF:__GetMCColor(MCSC_TITLEBK)

ASSIGN MCTitleBackgroundColor(oColor) 
	RETURN SELF:__SetMCColor(oColor, MCSC_TITLEBK)

ACCESS MCTitleTextColor 
	RETURN SELF:__GetMCColor(MCSC_TITLETEXT)

ASSIGN MCTitleTextColor(oColor) 
	RETURN SELF:__SetMCColor(oColor, MCSC_TITLETEXT)

ACCESS MCTrailingTextColor 
	RETURN SELF:__GetMCColor(MCSC_TRAILINGTEXT)

ASSIGN MCTrailingTextColor(oColor) 
	RETURN SELF:__SetMCColor(oColor, MCSC_TRAILINGTEXT)

ACCESS NullFormat
    RETURN SELF:lNullFormat
METHOD ParentNotify(nCode, lParam) 
   //SE-040929 For a correct focus after closing the calender

   IF nCode = DTN_CLOSEUP
      IF GetFocus() != SELF:Handle()
         SetFocus(SELF:Handle())
      ENDIF
      keybd_event(VK_RIGHT, 0, 0, 0)
   ENDIF

   RETURN 0l


METHOD PerformValidations() 
LOCAL lSuccess		AS LOGIC
LOCAL uOldValue		AS USUAL

	// DHer: 18/12/2008
	uOldValue := SELF:uValue
	IF _AND(GetWindowLong(SELF:Handle(),GWL_STYLE),DTS_TIMEFORMAT)>0
		SELF:uValue := SELF:SelectedTime
	ELSE	
		SELF:uValue := SELF:SelectedDate
	ENDIF
	lSuccess := SUPER:PerformValidations()
	SELF:uValue := uOldValue

RETURN lSuccess

ACCESS SelectedDate 
	LOCAL sDate IS _winSYSTEMTIME

	IF (SendMessage(SELF:handle(), DTM_GETSYSTEMTIME, 0, LONGINT(_CAST, @sDate)) == GDT_VALID)
		RETURN ConDate(sDate:wYear, sDate:wMonth, sDate:wDay)
	ENDIF

	RETURN NULL_DATE

ASSIGN SelectedDate(dNewDate) 
    // Suggestion from Håkon Clausen 
    SELF:SetDateTime(dNewDate, NULL_STRING)
	RETURN 

ACCESS SelectedTime 
	LOCAL sdate IS _winSYSTEMTIME

	IF (SendMessage(SELF:Handle(), DTM_GETSYSTEMTIME, 0, LONGINT(_CAST, @sDate)) == GDT_VALID)
	    RETURN ConTime(sDate:wHour, sDate:wMinute, sDate:wSecond)
	ENDIF

	RETURN NULL_STRING

ASSIGN SelectedTime(sNewTime) 
    // Suggestion from Håkon Clausen 
    SELF:SetDateTime(NULL_DATE, sNewTime)
	RETURN 
METHOD SetDateTime(dNewDate AS DATE, sNewTime AS STRING) AS VOID STRICT
    // Suggestion from Håkon Clausen and Dirk Herijgers 
    LOCAL sDate IS _winSYSTEMTIME
    LOCAL dwFlag AS DWORD

    // if no date or time given. Reset to default values and uncheck
    IF dNewDate == NULL_DATE .AND. SLen(sNewTime) == 0
        // clear the control 
        IF SELF:lNullFormat=FALSE                               
           SELF:cOldFormat := SELF:Format
        ENDIF                                                           
        SELF:Format := "''"
        SELF:lNullFormat := TRUE
        dwFlag := GDT_NONE
    ELSE
        SELF:Format := cOldFormat
        SELF:lNullFormat := FALSE
        dwFlag := GDT_VALID
        // get current value from control and set the parts that are given
        SendMessage(SELF:Handle(), DTM_GETSYSTEMTIME, 0, LONG(_CAST, @sDate))
        IF dNewDate != NULL_DATE
            sDate:wDay       := LoWord(Day(dNewDate))
            sDate:wMonth     := LoWord(Month(dNewDate))
            sDate:wYear      := LoWord(Year(dNewDate))
        ENDIF

        IF SLen(sNewTime) > 0
            sDate:wHour      := Val(Left(sNewTime, 2))
            sDate:wMinute    := Val(SubStr(sNewTime, 4,2))
            sDate:wSecond    := Val(SubStr(sNewTime, 7,2))
        ENDIF
    ENDIF

    SendMessage(SELF:Handle(), DTM_SETSYSTEMTIME, dwFlag, LONG(_CAST, @sDate))
    RETURN

ACCESS TextValue 
	IF (_AND(GetWindowLong(SELF:Handle(), GWL_STYLE), DTS_TIMEFORMAT) > 0)
		RETURN SELF:SelectedTime
	ENDIF
	RETURN AsString(SELF:SelectedDate)

ACCESS Value 
	// DHer: 18/12/2008
	IF _AND(GetWindowLong(SELF:Handle(),GWL_STYLE),DTS_TIMEFORMAT)>0
		SELF:uValue := SELF:SelectedTime
	ELSE	
		SELF:uValue := SELF:SelectedDate
	ENDIF

RETURN SUPER:Value
	
END CLASS

