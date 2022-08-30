#pragma options ("enforceself", on)
/// <include file="Gui.xml" path="doc/DateTimePicker/*" />
CLASS DateTimePicker INHERIT TextControl
    HIDDEN _lNoAssign AS LOGIC
    PROTECT lNullFormat       AS LOGIC
    PROTECT cOldFormat        AS STRING
    PROTECT cFormat           AS STRING


	//PP-030828 Strong typing
 /// <exclude />
METHOD __Gather() AS LOGIC STRICT
  //SE-040929 DTS_SHOWNONE Workaround
  LOCAL lReturn AS LOGIC


  _lNoAssign := TRUE
  lReturn := SUPER:__Gather()
  _lNoAssign := FALSE


  RETURN lReturn




 /// <exclude />
METHOD __GetMCColor(dwColorID AS DWORD) AS Color STRICT
	//PP-030828 Strong typing
	LOCAL cr AS LONGINT


	cr := SendMessage(SELF:Handle(), DTM_GETMCCOLOR, dwColorID, 0L)


	RETURN Color{_AND(cr, 0xFF), _AND((cr>>8), 0xFF), _AND((cr>>16), 0xFF)}


 /// <exclude />
METHOD __SetMCColor(oColor AS Color, dwColorID AS DWORD) AS Color STRICT
	//PP-030828 Strong typing


	SendMessage(SELF:Handle(), DTM_SETMCCOLOR, dwColorID, oColor:ColorRef)
	RETURN oColor


 /// <exclude />
ASSIGN __Value(uValue AS USUAL)
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


/// <include file="Gui.xml" path="doc/DateTimePicker.DateRange/*" />
ACCESS DateRange
	LOCAL DIM dates[2] IS _winSYSTEMTIME


	IF (SendMessage(SELF:handle(), DTM_GETRANGE, 0, LONGINT(_CAST, @dates[1])) > 0)
		RETURN DateRange{ConDate(dates[1]:wYear, dates[1]:wMonth, dates[1]:wDay), ConDate(dates[2]:wYear, dates[2]:wMonth, dates[2]:wDay) }
	ENDIF


	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/DateTimePicker.DateRange/*" />
ASSIGN DateRange(oNewRange)
	LOCAL DIM dates[2] IS _winSYSTEMTIME


	dates[1]:wDay := WORD(Day(oNewRange:Min))
	dates[1]:wMonth := WORD(Month(oNewRange:Min))
	dates[1]:wYear := WORD(Year(oNewRange:Min))


	dates[2]:wDay := WORD(Day(oNewRange:Max))
	dates[2]:wMonth := WORD(Month(oNewRange:Max))
	dates[2]:wYear := WORD(Year(oNewRange:Max))
	// RvdH 090212 This makes sure that every time on the Max-Date is valid
	dates[2]:wHour := 23
	dates[2]:wMinute := 59
	dates[2]:wSecond := 59
	dates[2]:wMilliseconds := 999




	SendMessage(SELF:handle(), DTM_SETRANGE, _OR(GDTR_MIN, GDTR_MAX), LONGINT(_CAST, @dates[1]))




/// <include file="Gui.xml" path="doc/DateTimePicker.Format/*" />
ACCESS Format
    RETURN cFormat


/// <include file="Gui.xml" path="doc/DateTimePicker.Format/*" />
ASSIGN Format(sNewFormat)
    cFormat := sNewFormat
    IF (sNewFormat != "''")
        cOldFormat := sNewFormat
    ENDIF
    SendMessage(SELF:Handle(), DTM_SETFORMAT, 0, LONG(_CAST, String2Psz(sNewFormat)))


	RETURN


/// <include file="Gui.xml" path="doc/DateTimePicker.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware)




	DEFAULT(@lDataAware, TRUE)


	SUPER(oOwner, xID, oPoint, oDimension, "SysDateTimePick32", dwStyle, lDataAware)
	RETURN


/// <include file="Gui.xml" path="doc/DateTimePicker.MCBackgroundColor/*" />
ACCESS MCBackgroundColor
	RETURN SELF:__GetMCColor(MCSC_BACKGROUND)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCBackgroundColor/*" />
ASSIGN MCBackgroundColor(oColor)
	RETURN SELF:__SetMCColor(oColor, MCSC_BACKGROUND)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCFont/*" />
ASSIGN MCFont(oNewFont)
	SendMessage(SELF:handle(), DTM_SETMCFONT, DWORD(_CAST, oNewFont:Handle()), 1)
	RETURN


/// <include file="Gui.xml" path="doc/DateTimePicker.MCMonthBackgroundColor/*" />
ACCESS MCMonthBackgroundColor
	RETURN SELF:__GetMCColor(MCSC_MONTHBK)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCMonthBackgroundColor/*" />
ASSIGN MCMonthBackgroundColor(oColor)
	RETURN SELF:__SetMCColor(oColor, MCSC_MONTHBK)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCTextColor/*" />
ACCESS MCTextColor
	RETURN SELF:__GetMCColor(MCSC_TEXT)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCTextColor/*" />
ASSIGN MCTextColor(oColor)
	RETURN SELF:__SetMCColor(oColor, MCSC_TEXT)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCTitleBackgroundColor/*" />
ACCESS MCTitleBackgroundColor
	RETURN SELF:__GetMCColor(MCSC_TITLEBK)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCTitleBackgroundColor/*" />
ASSIGN MCTitleBackgroundColor(oColor)
	RETURN SELF:__SetMCColor(oColor, MCSC_TITLEBK)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCTitleTextColor/*" />
ACCESS MCTitleTextColor
	RETURN SELF:__GetMCColor(MCSC_TITLETEXT)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCTitleTextColor/*" />
ASSIGN MCTitleTextColor(oColor)
	RETURN SELF:__SetMCColor(oColor, MCSC_TITLETEXT)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCTrailingTextColor/*" />
ACCESS MCTrailingTextColor
	RETURN SELF:__GetMCColor(MCSC_TRAILINGTEXT)


/// <include file="Gui.xml" path="doc/DateTimePicker.MCTrailingTextColor/*" />
ASSIGN MCTrailingTextColor(oColor)
	RETURN SELF:__SetMCColor(oColor, MCSC_TRAILINGTEXT)


/// <include file="Gui.xml" path="doc/DateTimePicker.NullFormat/*" />
ACCESS NullFormat
    RETURN SELF:lNullFormat


/// <include file="Gui.xml" path="doc/DateTimePicker.ParentNotify/*" />
METHOD ParentNotify(nCode, lParam)
   //SE-040929 For a correct focus after closing the calender


   IF nCode = DTN_CLOSEUP
      IF GetFocus() != SELF:Handle()
         SetFocus(SELF:Handle())
      ENDIF
      keybd_event(VK_RIGHT, 0, 0, 0)
   ENDIF


   RETURN 0l




/// <include file="Gui.xml" path="doc/DateTimePicker.PerformValidations/*" />
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


/// <include file="Gui.xml" path="doc/DateTimePicker.SelectedDate/*" />
ACCESS SelectedDate
	LOCAL sDate IS _winSYSTEMTIME


	IF (SendMessage(SELF:handle(), DTM_GETSYSTEMTIME, 0, LONGINT(_CAST, @sDate)) == GDT_VALID)
		RETURN ConDate(sDate:wYear, sDate:wMonth, sDate:wDay)
	ENDIF


	RETURN NULL_DATE


/// <include file="Gui.xml" path="doc/DateTimePicker.SelectedDate/*" />
ASSIGN SelectedDate(dNewDate)
    // Suggestion from H�kon Clausen
    SELF:SetDateTime(dNewDate, NULL_STRING)
	RETURN


/// <include file="Gui.xml" path="doc/DateTimePicker.SelectedTime/*" />
ACCESS SelectedTime
	LOCAL sdate IS _winSYSTEMTIME


	IF (SendMessage(SELF:Handle(), DTM_GETSYSTEMTIME, 0, LONGINT(_CAST, @sDate)) == GDT_VALID)
	    RETURN ConTime(sDate:wHour, sDate:wMinute, sDate:wSecond)
	ENDIF


	RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/DateTimePicker.SelectedTime/*" />
ASSIGN SelectedTime(sNewTime)
    // Suggestion from H�kon Clausen
    SELF:SetDateTime(NULL_DATE, sNewTime)
	RETURN


/// <include file="Gui.xml" path="doc/DateTimePicker.SetDateTime/*" />
METHOD SetDateTime(dNewDate AS DATE, sNewTime AS STRING) AS VOID STRICT
    // Suggestion from H�kon Clausen and Dirk Herijgers
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
            sDate:wDay       := LoWord ((DWORD) dNewDate:Day )
   	        sDate:wMonth     := LoWord ((DWORD) dNewDate:Month )
       	    sDate:wYear      := LoWord ((DWORD) dNewDate:Year )
        ENDIF


        IF SLen(sNewTime) > 0


        	// Start  KHR


        	// make sure that lateron DTM_SETSYSTEMTIME is always called with a valid date part !
		    dNewDate := ConDate(sDate:wYear, sDate:wMonth, sDate:wDay)
		    IF dNewDate == NULL_DATE
	            dNewDate          := Today()
	            sDate:wDay       := LoWord ((DWORD) dNewDate:Day )
	   	        sDate:wMonth     := LoWord ((DWORD) dNewDate:Month )
	       	    sDate:wYear      := LoWord ((DWORD) dNewDate:Year )
        	ENDIF
        	// End KHR


            sDate:wHour      := Val(Left(sNewTime, 2))
            sDate:wMinute    := Val(SubStr3(sNewTime, 4,2))
            sDate:wSecond    := Val(SubStr3(sNewTime, 7,2))


        ENDIF
    ENDIF


    SendMessage(SELF:Handle(), DTM_SETSYSTEMTIME, dwFlag, LONG(_CAST, @sDate))
    RETURN


/// <include file="Gui.xml" path="doc/DateTimePicker.TextValue/*" />
ACCESS TextValue
	IF (_AND(GetWindowLong(SELF:Handle(), GWL_STYLE), DTS_TIMEFORMAT) > 0)
		RETURN SELF:SelectedTime
	ENDIF
	RETURN AsString(SELF:SelectedDate)


/// <include file="Gui.xml" path="doc/DateTimePicker.Value/*" />
ACCESS Value
	// DHer: 18/12/2008
	IF _AND(GetWindowLong(SELF:Handle(),GWL_STYLE),DTS_TIMEFORMAT)>0
		SELF:uValue := SELF:SelectedTime
	ELSE
		SELF:uValue := SELF:SelectedDate
	ENDIF
    RETURN SUPER:Value


/// <include file="Gui.xml" path="doc/DateTimePicker.IsNone/*" />
ACCESS IsNone  AS LOGIC
   //SE-120210
   LOCAL sDate IS _winSYSTEMTIME


   RETURN (SendMessage(SELF:Handle(), DTM_GETSYSTEMTIME, 0, LONGINT(_CAST, @sDate)) == GDT_NONE)




/// <include file="Gui.xml" path="doc/DateTimePicker.IsTimePicker/*" />
ACCESS IsTimePicker AS LOGIC
	RETURN  _And( GetWindowLong(SELF:Handle(), GWL_STYLE) , DTS_TIMEFORMAT) > 0  .or. ;
			_And( GetWindowLong(SELF:Handle(), GWL_STYLE) , DTS_UPDOWN    ) > 0








END CLASS


