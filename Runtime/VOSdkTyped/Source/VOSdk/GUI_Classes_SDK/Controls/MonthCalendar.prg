

CLASS DateRange INHERIT VObject
	PROTECT dStartDate AS DATE
	PROTECT dEndDate AS DATE

	CONSTRUCTOR(dStart AS DATE, dEnd AS DATE) 
		SUPER()
		dStartDate := dStart
		dEndDate := dEnd
		RETURN 

	PROPERTY Max AS DATE GET dEndDate	SET dEndDate := Value
	PROPERTY Min AS DATE GET dStartDate SET dStartDate := Value

END CLASS

CLASS MonthCalendar INHERIT TextControl

	
	ACCESS __Calendar AS VOMonthCalendar
		RETURN (VOMonthCalendar) oCtrl

    PROPERTY ControlType AS ControlType GET ControlType.MonthCalendar

	
	METHOD __GetColor(dwColorID AS DWORD) AS Color STRICT 
		LOCAL cr AS LONG
		cr := GuiWin32.SendMessage(oCtrl:Handle, MCM_GETCOLOR, dwColorID, 0L)
		RETURN Color.FromColorRef((DWORD) cr)

	METHOD __SetColor(oColor AS Color, dwColorID AS DWORD) AS Color STRICT 
		GuiWin32.SendMessage(oCtrl:Handle, MCM_SETCOLOR, dwColorID, (LONG) oColor:ColorRef)
		RETURN oColor

	ASSIGN __Value(dValue AS USUAL)  STRICT 
		
		SUPER:__Value := dValue
		IF IsDate(uValue)
			SELF:Selection := uValue
		ENDIF
		RETURN 

	ACCESS BackgroundColor  AS Color
		IF SELF:ValidateControl()

			RETURN (Color) __Calendar:BackColor
		ENDIF
		RETURN NULL_OBJECT
	ASSIGN BackgroundColor(oColor AS Color) 
		IF SELF:ValidateControl()
			__Calendar:BackColor := oColor
		ENDIF
	//METHOD Dispatch (oEvent) 
	//	LOCAL oEvt := oEvent AS @@Event
	//	//PP-030319 WM_GETDLGCODE used to support arrow keys in the control. Thanks to S Ebert

	//	IF oEvt:message == WM_GetDlgCode
	//		SELF:EventReturnValue := DLGC_WANTARROWS
	//		RETURN 1l
	//	ENDIF

	//	RETURN SUPER:Dispatch(oEvt)

	ACCESS FirstDayOfWeek AS LONG
		IF SELF:ValidateControl()
			RETURN (LONG) __Calendar:FirstDayOfWeek
		ENDIF
		RETURN 0
	ASSIGN FirstDayOfWeek(iNewVal AS LONG) 
		IF SELF:ValidateControl()
			__Calendar:FirstDayOfWeek	:= (System.Windows.Forms.Day) iNewVal	
		ENDIF
		RETURN 

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware) 
		DEFAULT(@lDataAware, TRUE)
		SUPER(oOwner, xID, oPoint, oDimension, "SysMonthCal32", dwStyle, lDataAware)
		RETURN 

	ACCESS MaxSelCount AS LONG
		IF SELF:ValidateControl()
			RETURN __Calendar:MaxSelectionCount
		ENDIF
		RETURN 0
	ASSIGN MaxSelCount(iNewVal AS LONG) 
		IF SELF:ValidateControl()
			__Calendar:MaxSelectionCount := iNewVal
		ENDIF
		RETURN 


	ACCESS MonthBackgroundColor  AS Color
		RETURN SELF:__GetColor(MCSC_MONTHBK)

	ASSIGN MonthBackgroundColor(oColor AS Color) 
		SELF:__SetColor(oColor, MCSC_MONTHBK)

	ACCESS MonthDelta 
		RETURN GuiWin32.SendMessage(SELF:Handle(), MCM_GETMONTHDELTA, 0, 0)

	ASSIGN MonthDelta(iNewVal) 
		GuiWin32.SendMessage(SELF:Handle(), MCM_SETMONTHDELTA, DWORD(_CAST, iNewVal), 0)
		RETURN 

	ACCESS Range AS DateRange
		LOCAL dStart, dEnd AS DATE
		IF SELF:ValidateControl()
			dStart := (DATE) __Calendar:SelectionStart
			dEnd   := (DATE) __Calendar:SelectionEnd
			RETURN DateRange{dStart, dEnd }
		ENDIF
		RETURN DateRange{Today(), Today()}
	ASSIGN Range(oNewRange AS DateRange) 
		IF SELF:ValidateControl()
			__Calendar:SelectionStart	:= oNewRange:Min
			__Calendar:SelectionEnd		:= oNewRange:Max
		ENDIF
		RETURN 

	ACCESS Selection AS DATE
		IF SELF:ValidateControl()
			RETURN (DATE) __Calendar:SelectionStart
		ENDIF
		RETURN NULL_DATE

	ASSIGN Selection(dNewVal AS DATE) 
		IF SELF:ValidateControl()
			__Calendar:SelectionStart := dNewVal
		ENDIF
		RETURN 

	ACCESS SelectionRange AS DateRange
		LOCAL dStart AS DATE
		LOCAL dEnd AS DATE
		IF SELF:ValidateControl()
			dStart := (DATE) __Calendar:MinDate
			dEnd   := (DATE) __Calendar:MaxDate
			RETURN DateRange{dStart, dEnd }
		ENDIF
		RETURN DateRange{Today(), Today()}
		
	ASSIGN SelectionRange(oNewRange AS DateRange) 
		IF SELF:ValidateControl()
			__Calendar:MinDate := oNewRange:Min
			__Calendar:MaxDate := oNewRange:Max
		ENDIF
		RETURN 

	ACCESS TextColor AS Color
		//RETURN SELF:__GetColor(MCSC_TEXT)
		IF SELF:ValidateControl()
			RETURN (Color) __Calendar:ForeColor
		ENDIF
		RETURN NULL_OBJECT
	ASSIGN TextColor(oColor AS Color) 
		IF SELF:ValidateControl()
			__Calendar:ForeColor := oColor		
		ENDIF
	ACCESS TextValue AS STRING
		RETURN AsString(SELF:Selection)

	ACCESS TitleBackgroundColor AS Color
		IF SELF:ValidateControl()
			RETURN (Color) __Calendar:TitleBackColor
		ENDIF
		RETURN NULL_OBJECT	

	ASSIGN TitleBackgroundColor(oColor AS Color) 
		IF SELF:ValidateControl()
			__Calendar:TitleBackColor := oColor
		ENDIF
		
	ACCESS TitleTextColor AS Color
		IF SELF:ValidateControl()
			RETURN (Color) __Calendar:TitleForeColor
		ENDIF
		RETURN NULL_OBJECT
		
	ASSIGN TitleTextColor(oColor AS Color) 
		IF SELF:ValidateControl()
			__Calendar:TitleForeColor := oColor
		ENDIF
		
	ACCESS Today AS DATE
		IF SELF:ValidateControl()
			RETURN (DATE) __Calendar:TodayDate
		ENDIF
		RETURN Today()
	ASSIGN Today(dNewVal AS DATE) 
		IF SELF:ValidateControl()
			__Calendar:TodayDate := (DateTime) dNewVal
		ENDIF
		RETURN 

	ACCESS TrailingTextColor AS Color
		IF SELF:ValidateControl()
			RETURN (Color) __Calendar:TrailingForeColor
		ENDIF
		RETURN NULL_OBJECT
	ASSIGN TrailingTextColor(oColor AS Color) 
		IF SELF:ValidateControl()
			__Calendar:TrailingForeColor := oColor
		ENDIF
	ACCESS Value AS USUAL
		uValue := SELF:Selection
		RETURN uValue
	
END CLASS

