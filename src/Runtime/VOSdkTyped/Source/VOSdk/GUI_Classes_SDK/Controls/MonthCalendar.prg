//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="Gui.xml" path="doc/DateRange/*" />

CLASS DateRange INHERIT VObject
	PROTECT dStartDate AS DATE
	PROTECT dEndDate AS DATE

	CONSTRUCTOR(dStart AS DATE, dEnd AS DATE)
		SUPER()
		dStartDate := dStart
		dEndDate := dEnd
		RETURN

/// <include file="Gui.xml" path="doc/DateRange.Max/*" />
	PROPERTY Max AS DATE GET dEndDate	SET dEndDate := Value
	/// <include file="Gui.xml" path="doc/DateRange.Min/*" />
	PROPERTY Min AS DATE GET dStartDate SET dStartDate := Value

END CLASS

/// <include file="Gui.xml" path="doc/MonthCalendar/*" />
CLASS MonthCalendar INHERIT TextControl

	ACCESS __Calendar AS VOMonthCalendar
		RETURN (VOMonthCalendar) oCtrl
    /// <inheritdoc />
    PROPERTY ControlType AS ControlType GET ControlType.MonthCalendar

	METHOD __GetColor(dwColorID AS DWORD) AS Color STRICT
		LOCAL cr AS LONG
		cr := GuiWin32.SendMessage(oCtrl:Handle, MCM_GETCOLOR, dwColorID, 0L)
		RETURN (Color) cr

 /// <exclude />
	METHOD __SetColor(oColor AS Color, dwColorID AS DWORD) AS Color STRICT
		GuiWin32.SendMessage(oCtrl:Handle, MCM_SETCOLOR, dwColorID, (LONG) oColor:ColorRef)
		RETURN oColor

 /// <exclude />
	ASSIGN __Value(dValue AS USUAL)  STRICT

		SUPER:__Value := dValue
		IF IsDate(uValue)
			SELF:Selection := uValue
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/MonthCalendar.BackgroundColor/*" />
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

	/// <include file="Gui.xml" path="doc/MonthCalendar.FirstDayOfWeek/*" />
	ACCESS FirstDayOfWeek AS LONG
		IF SELF:ValidateControl()
			RETURN (LONG) __Calendar:FirstDayOfWeek
		ENDIF
		RETURN 0
/// <include file="Gui.xml" path="doc/MonthCalendar.FirstDayOfWeek/*" />
	ASSIGN FirstDayOfWeek(iNewVal AS LONG)
		IF SELF:ValidateControl()
			__Calendar:FirstDayOfWeek	:= (System.Windows.Forms.Day) iNewVal
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/MonthCalendar.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware)
		DEFAULT(REF lDataAware, TRUE)
		SUPER(oOwner, xID, oPoint, oDimension, "SysMonthCal32", dwStyle, lDataAware)
		RETURN

/// <include file="Gui.xml" path="doc/MonthCalendar.MaxSelCount/*" />
	ACCESS MaxSelCount AS LONG
		IF SELF:ValidateControl()
			RETURN __Calendar:MaxSelectionCount
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/MonthCalendar.MaxSelCount/*" />
	ASSIGN MaxSelCount(iNewVal AS LONG)
		IF SELF:ValidateControl()
			__Calendar:MaxSelectionCount := iNewVal
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/MonthCalendar.MonthBackgroundColor/*" />
	ACCESS MonthBackgroundColor  AS Color
		IF SELF:ValidateControl()
			RETURN __Calendar:BackColor
        ENDIF
        RETURN System.Drawing.Color.White

/// <include file="Gui.xml" path="doc/MonthCalendar.MonthBackgroundColor/*" />
	ASSIGN MonthBackgroundColor(oColor AS Color)
		IF SELF:ValidateControl()
			__Calendar:BackColor := oColor
		ENDIF

/// <include file="Gui.xml" path="doc/MonthCalendar.MonthDelta/*" />
	ACCESS MonthDelta
		RETURN GuiWin32.SendMessage(SELF:Handle(), MCM_GETMONTHDELTA, 0, 0)

/// <include file="Gui.xml" path="doc/MonthCalendar.MonthDelta/*" />
	ASSIGN MonthDelta(iNewVal)
		GuiWin32.SendMessage(SELF:Handle(), MCM_SETMONTHDELTA, DWORD(_CAST, iNewVal), 0)
		RETURN

/// <include file="Gui.xml" path="doc/MonthCalendar.Range/*" />
	ACCESS Range AS DateRange
		LOCAL dStart, dEnd AS DATE
		IF SELF:ValidateControl()
			dStart := (DATE) __Calendar:SelectionStart
			dEnd   := (DATE) __Calendar:SelectionEnd
			RETURN DateRange{dStart, dEnd }
		ENDIF
		RETURN DateRange{Today(), Today()}
/// <include file="Gui.xml" path="doc/MonthCalendar.Range/*" />
	ASSIGN Range(oNewRange AS DateRange)
		IF SELF:ValidateControl()
			__Calendar:SelectionStart	:= oNewRange:Min
			__Calendar:SelectionEnd		:= oNewRange:Max
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/MonthCalendar.Selection/*" />
	ACCESS Selection AS DATE
		IF SELF:ValidateControl()
			RETURN (DATE) __Calendar:SelectionStart
		ENDIF
		RETURN NULL_DATE

/// <include file="Gui.xml" path="doc/MonthCalendar.Selection/*" />
	ASSIGN Selection(dNewVal AS DATE)
		IF SELF:ValidateControl()
			__Calendar:SelectionStart := dNewVal
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/MonthCalendar.SelectionRange/*" />
	ACCESS SelectionRange AS DateRange
		LOCAL dStart AS DATE
		LOCAL dEnd AS DATE
		IF SELF:ValidateControl()
			dStart := (DATE) __Calendar:MinDate
			dEnd   := (DATE) __Calendar:MaxDate
			RETURN DateRange{dStart, dEnd }
		ENDIF
		RETURN DateRange{Today(), Today()}

/// <include file="Gui.xml" path="doc/MonthCalendar.SelectionRange/*" />
	ASSIGN SelectionRange(oNewRange AS DateRange)
		IF SELF:ValidateControl()
			__Calendar:MinDate := oNewRange:Min
			__Calendar:MaxDate := oNewRange:Max
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/MonthCalendar.TextColor/*" />
	ACCESS TextColor AS Color
		//RETURN SELF:__GetColor(MCSC_TEXT)
		IF SELF:ValidateControl()
			RETURN (Color) __Calendar:ForeColor
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/MonthCalendar.TextColor/*" />
	ASSIGN TextColor(oColor AS Color)
		IF SELF:ValidateControl()
			__Calendar:ForeColor := oColor
		ENDIF

/// <include file="Gui.xml" path="doc/MonthCalendar.TextValue/*" />
	ACCESS TextValue AS STRING
		RETURN AsString(SELF:Selection)


/// <include file="Gui.xml" path="doc/MonthCalendar.TitleBackgroundColor/*" />
	ACCESS TitleBackgroundColor AS Color
		IF SELF:ValidateControl()
			RETURN (Color) __Calendar:TitleBackColor
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/MonthCalendar.TitleBackgroundColor/*" />
	ASSIGN TitleBackgroundColor(oColor AS Color)
		IF SELF:ValidateControl()
			__Calendar:TitleBackColor := oColor
		ENDIF

/// <include file="Gui.xml" path="doc/MonthCalendar.TitleTextColor/*" />
	ACCESS TitleTextColor AS Color
		IF SELF:ValidateControl()
			RETURN (Color) __Calendar:TitleForeColor
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/MonthCalendar.TitleTextColor/*" />
	ASSIGN TitleTextColor(oColor AS Color)
		IF SELF:ValidateControl()
			__Calendar:TitleForeColor := oColor
		ENDIF

/// <include file="Gui.xml" path="doc/MonthCalendar.Today/*" />
	ACCESS Today AS DATE
		IF SELF:ValidateControl()
			RETURN (DATE) __Calendar:TodayDate
		ENDIF
		RETURN Today()
/// <include file="Gui.xml" path="doc/MonthCalendar.Today/*" />
	ASSIGN Today(dNewVal AS DATE)
		IF SELF:ValidateControl()
			__Calendar:TodayDate := (DateTime) dNewVal
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/MonthCalendar.TrailingTextColor/*" />
	ACCESS TrailingTextColor AS Color
		IF SELF:ValidateControl()
			RETURN (Color) __Calendar:TrailingForeColor
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/MonthCalendar.TrailingTextColor/*" />
	ASSIGN TrailingTextColor(oColor AS Color)
		IF SELF:ValidateControl()
			__Calendar:TrailingForeColor := oColor
		ENDIF
/// <include file="Gui.xml" path="doc/MonthCalendar.Value/*" />
	ACCESS Value AS USUAL
		uValue := SELF:Selection
		RETURN uValue

END CLASS

