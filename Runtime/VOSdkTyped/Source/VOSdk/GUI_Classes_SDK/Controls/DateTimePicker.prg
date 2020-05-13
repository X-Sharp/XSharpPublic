

#USING System.Windows.Forms
#USING System.Runtime.InteropServices
CLASS DateTimePicker INHERIT TextControl
	HIDDEN _lNoAssign AS LOGIC   
	PROTECT cFormat        AS STRING

    PROPERTY ControlType AS ControlType GET ControlType.DateTimePicker

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware) 
		DEFAULT(@lDataAware, TRUE)
		//SELF:cFormat := Vulcan.Runtime.State.DateFormat_Internal
		SUPER(oOwner, xID, oPoint, oDimension, "SysDateTimePick32", dwStyle, lDataAware)
		RETURN 

    METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
        VAR oDT := (VODateTimePicker) oC
        oDT:ShowCheckBox:= TRUE
    	oDT:MinDate     := DateTime.MinValue
	    oDT:MaxDate     := DateTime.MaxValue
		oDT:Checked     := FALSE
        RETURN


	ACCESS __DateTimePicker AS VODateTimePicker
		RETURN (VODateTimePicker) oCtrl

	METHOD __Gather() AS LOGIC STRICT 
		LOCAL lReturn AS LOGIC

		_lNoAssign := TRUE
		lReturn := SUPER:__Gather()
		_lNoAssign := FALSE

		RETURN lReturn

	[Obsolete];
	METHOD __GetMCColor(dwColorID AS DWORD) AS Color STRICT 
		// Helper method from VO Dummy now
		RETURN Color{0}

	[Obsolete];
	METHOD __SetMCColor(oColor AS Color, dwColorID AS DWORD) AS Color STRICT 
		RETURN oColor

	METHOD __SetText(cNewText AS STRING) AS STRING STRICT 
		// Setting the text to an empty date failed.
		TRY
			SUPER:__SetText(cNewText)
		CATCH
			// Do nothing
		END TRY
		RETURN cNewText

	ASSIGN __Value(uValue AS USUAL)  STRICT 
		IF ! _lNoAssign

			SUPER:__Value := uValue

			IF IsDate(uValue)
				SELF:SelectedDate := uValue
			ELSEIF IsString(uValue)
				SELF:SelectedTime := uValue
			ENDIF
		ENDIF
		RETURN 

	ACCESS DateRange AS DateRange
		LOCAL dMin, dMax AS DATE
		dMin := (DATE) __DateTimePicker:MinDate 
		dMax := (DATE) __DateTimePicker:MaxDate 
		RETURN DateRange{dMin, dMax }

	ASSIGN DateRange(oNewRange  AS DateRange) 
		__DateTimePicker:MinDate := oNewRange:Min
		__DateTimePicker:MaxDate := oNewRange:Max

	ACCESS Format AS STRING
		RETURN cFormat

	ASSIGN Format(sNewFormat AS STRING) 
		cFormat := sNewFormat
		__DateTimePicker:Format := System.Windows.Forms.DateTimePickerFormat.Custom
		__DateTimePicker:CustomFormat := sNewFormat

		RETURN 

	ACCESS MCBackgroundColor AS Color
		RETURN (Color) __DateTimePicker:BackColor

	ASSIGN MCBackgroundColor(oColor AS Color) 
		__DateTimePicker:BackColor := oColor

	ASSIGN MCFont(oNewFont as XSharp.VO.Font) 
		__DateTimePicker:CalendarFont := oNewFont
		RETURN 

	ACCESS MCMonthBackgroundColor  AS Color
		RETURN  __DateTimePicker:CalendarMonthBackground

	ASSIGN MCMonthBackgroundColor(oColor AS Color) 
		__DateTimePicker:CalendarMonthBackground := oColor

	ACCESS MCTextColor  AS Color
		RETURN __DateTimePicker:CalendarForeColor

	ASSIGN MCTextColor(oColor AS Color) 
		__DateTimePicker:CalendarForeColor := oColor

	ACCESS MCTitleBackgroundColor  AS Color
		RETURN __DateTimePicker:CalendarTitleBackColor

	ASSIGN MCTitleBackgroundColor(oColor AS Color) 
		__DateTimePicker:CalendarTitleBackColor := oColor

	ACCESS MCTitleTextColor  AS Color
		RETURN __DateTimePicker:CalendarTitleForeColor

	ASSIGN MCTitleTextColor(oColor AS Color) 
		__DateTimePicker:CalendarTitleForeColor := oColor

	ACCESS MCTrailingTextColor  AS Color
		RETURN __DateTimePicker:CalendarTrailingForeColor

	ASSIGN MCTrailingTextColor(oColor AS Color) 
		__DateTimePicker:CalendarTrailingForeColor := oColor

	ACCESS NullFormat AS LOGIC
		RETURN SELF:__DateTimePicker:CustomFormat == "''" 
		
	//METHOD ParentNotify(nCode, lParam) 
	//	//SE-040929 For a correct focus after closing the calender

	//	IF nCode = DTN_CLOSEUP
	//		IF GetFocus() != SELF:Handle()
	//			SetFocus(SELF:Handle())
	//		ENDIF
	//		keybd_event(VK_RIGHT, 0, 0, 0)
	//	ENDIF

	//	RETURN 0l


	METHOD PerformValidations() CLIPPER
		LOCAL lSuccess		AS LOGIC
		LOCAL uOldValue		AS USUAL

		// DHer: 18/12/2008
		uOldValue := SELF:uValue
		IF _AND(Win32.GetWindowLong(SELF:Handle(),GWL_STYLE),DTS_TIMEFORMAT)>0
			SELF:uValue := SELF:SelectedTime
		ELSE	
			SELF:uValue := SELF:SelectedDate
		ENDIF
		lSuccess := SUPER:PerformValidations()
		SELF:uValue := uOldValue

		RETURN lSuccess

	ACCESS SelectedDate AS DATE
		IF __DateTimePicker:Checked
			RETURN (DATE) __DateTimePicker:Value
		ENDIF
		RETURN NULL_DATE

	ASSIGN SelectedDate(dNewDate AS DATE) 
		// Suggestion from Håkon Clausen 
		SELF:SetDateTime(dNewDate, NULL_STRING)
		RETURN 

	ACCESS SelectedTime AS STRING
		LOCAL dSelected AS DateTime
		IF __DateTimePicker:Checked
			dSelected := __DateTimePicker:Value
			RETURN ConTime((DWORD)dSelected:Hour, (DWORD)dSelected:Minute, (DWORD)dSelected:Second)
		ENDIF
		RETURN NULL_STRING
	
	ASSIGN SelectedTime(sNewTime AS STRING) 
		// Suggestion from Håkon Clausen 
		SELF:SetDateTime(NULL_DATE, sNewTime)
		RETURN 

	METHOD SetDateTime(dNewDate AS DATE, sNewTime AS STRING) AS VOID STRICT
		// Suggestion from Håkon Clausen and Dirk Herijgers 
		LOCAL dwFlag AS DWORD
		LOCAL sDate AS DateTime
		LOCAL nDay, nMonth, nYear AS LONG
		LOCAL nHour, nMins, nSecs AS LONG
		// if no date or time given. Reset to default values and uncheck
		IF dNewDate == NULL_DATE .AND. SLen(sNewTime) == 0
			// clear the control 
			dwFlag := GDT_NONE
		ELSE
			dwFlag := GDT_VALID
			IF dNewDate == NULL_DATE
				dNewDate := (DATE) DateTime.Now
			ENDIF
			nDay		 := dNewDate:Day
			nMonth		 := dNewDate:Month
			nYear       := dNewDate:Year

			IF SLen(sNewTime) > 0
				nHour    := Val(Left(sNewTime, 2))
				nMins    := Val(SubStr(sNewTime, 4,2))
				nSecs    := Val(SubStr(sNewTime, 7,2))
			ENDIF
			sDate := DateTime{nYear, nMonth, nDay, nHour, nMins, nSecs}
		ENDIF
		IF dwFlag != GDT_NONE
			try
				__DateTimePicker:Value			:= sDate
				__DateTimePicker:CustomFormat	:= cFormat
			catch
			end try
		ELSE
			__DateTimePicker:CustomFormat	:= "''" 
		ENDIF		
		__DateTimePicker:Invalidate()
		RETURN

	ACCESS TextValue  AS STRING
		IF (_AND(Win32.GetWindowLong(SELF:Handle(), GWL_STYLE), DTS_TIMEFORMAT) > 0)
			RETURN SELF:SelectedTime
		ENDIF
		RETURN AsString(SELF:SelectedDate)

	ACCESS Value as USUAL
		IF _AND(Win32.GetWindowLong(SELF:Handle(),GWL_STYLE),DTS_TIMEFORMAT)>0
			SELF:uValue := SELF:SelectedTime
		ELSE	
			SELF:uValue := SELF:SelectedDate
		ENDIF

		RETURN SUPER:Value
        
	ACCESS IsNone  AS LOGIC
        RETURN SELF:__DateTimePicker:Value == DateTime.MinValue

	ACCESS IsTimePicker AS LOGIC
        LOCAL nStyle AS LONG
        nStyle := Win32.GetWindowStyle(SELF:__Handle)
        RETURN _AND(nStyle, _OR(DTS_TIMEFORMAT,DTS_UPDOWN) ) > 0

	
END CLASS

