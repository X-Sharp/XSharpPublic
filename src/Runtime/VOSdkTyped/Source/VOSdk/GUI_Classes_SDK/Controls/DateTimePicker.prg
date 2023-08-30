//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Windows.Forms
USING System.Runtime.InteropServices
USING VOSDK := XSharp.VO.SDK

CLASS DateTimePicker INHERIT TextControl
    HIDDEN _lNoAssign AS LOGIC
    PROTECT cFormat        AS STRING

    /// <exclude />
    PROPERTY ControlType AS ControlType GET ControlType.DateTimePicker

    CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware)
        DEFAULT( REF lDataAware, TRUE)
        //SELF:cFormat := Vulcan.Runtime.State.DateFormat_Internal
        SUPER(oOwner, xID, oPoint, oDimension, "SysDateTimePick32", dwStyle, lDataAware)
        RETURN
    /// <exclude />
    METHOD OnControlCreated(oC AS IVOControl) AS VOID
        VAR oDT := (VODateTimePicker) oC
        oDT:ShowCheckBox:= TRUE
        oDT:MinDate     := DateTime.MinValue
        oDT:MaxDate     := DateTime.MaxValue
        oDT:Checked     := FALSE
        RETURN


    /// <exclude />
    PROPERTY __DateTimePicker AS VODateTimePicker GET (VODateTimePicker) oCtrl

    /// <exclude />
    METHOD __Gather() AS LOGIC STRICT
        LOCAL lReturn AS LOGIC

        _lNoAssign := TRUE
        lReturn := SUPER:__Gather()
        _lNoAssign := FALSE

        RETURN lReturn

    /// <exclude />
    [Obsolete];
    METHOD __GetMCColor(dwColorID AS DWORD) AS Color STRICT
        // Helper method from VO Dummy now
        RETURN Color{0}

    /// <exclude />
    [Obsolete];
    METHOD __SetMCColor(oColor AS Color, dwColorID AS DWORD) AS Color STRICT
        RETURN oColor

    /// <exclude />
    METHOD __SetText(cNewText AS STRING) AS STRING STRICT
        // Setting the text to an empty date failed.
        TRY
            SUPER:__SetText(cNewText)
        CATCH
            // Do nothing
            NOP
        END TRY
        RETURN cNewText


    /// <exclude />
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

    /// <include file="Gui.xml" path="doc/DateTimePicker.DateRange/*" />
    PROPERTY DateRange AS DateRange
        GET
            RETURN DateRange{ __DateTimePicker:MinDate, __DateTimePicker:MaxDate}
        END GET
        SET
            __DateTimePicker:MinDate := DateTime{value:Min:Year, value:Min:Month, value:Min:Day}
            __DateTimePicker:MaxDate := DateTime{value:Max:Year, value:Max:Month, value:Max:Day,23,59,59}
        END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/DateTimePicker.Format/*" />
    PROPERTY Format AS STRING
        GET
            RETURN cFormat
        END GET
        SET
            cFormat := value
            __DateTimePicker:Format := System.Windows.Forms.DateTimePickerFormat.Custom
            __DateTimePicker:CustomFormat := value

        END SET
    END PROPERTY


    /// <include file="Gui.xml" path="doc/DateTimePicker.MCBackgroundColor/*" />
    PROPERTY MCBackgroundColor AS Color ;
        GET __DateTimePicker:BackColor ;
        SET __DateTimePicker:BackColor := value

    /// <include file="Gui.xml" path="doc/DateTimePicker.MCFont/*" />
    ASSIGN MCFont(oNewFont AS VOSDK.Font)
        __DateTimePicker:CalendarFont := oNewFont
        RETURN

    /// <include file="Gui.xml" path="doc/DateTimePicker.MCMonthBackgroundColor/*" />
    PROPERTY MCMonthBackgroundColor  AS Color ;
    GET __DateTimePicker:CalendarMonthBackground ;
    SET __DateTimePicker:CalendarMonthBackground := value

    /// <include file="Gui.xml" path="doc/DateTimePicker.MCTextColor/*" />
    PROPERTY MCTextColor  AS Color ;
    GET __DateTimePicker:CalendarForeColor ;
    SET __DateTimePicker:CalendarForeColor := value

    /// <include file="Gui.xml" path="doc/DateTimePicker.MCTitleBackgroundColor/*" />
    PROPERTY MCTitleBackgroundColor  AS Color ;
    GET __DateTimePicker:CalendarTitleBackColor ;
    SET __DateTimePicker:CalendarTitleBackColor := value

    /// <include file="Gui.xml" path="doc/DateTimePicker.MCTitleTextColor/*" />
    PROPERTY MCTitleTextColor  AS Color ;
    GET __DateTimePicker:CalendarTitleForeColor ;
    SET __DateTimePicker:CalendarTitleForeColor := value

    /// <include file="Gui.xml" path="doc/DateTimePicker.MCTrailingTextColor/*" />
    PROPERTY MCTrailingTextColor  AS Color ;
    GET __DateTimePicker:CalendarTrailingForeColor;
    SET __DateTimePicker:CalendarTrailingForeColor := value

    /// <include file="Gui.xml" path="doc/DateTimePicker.NullFormat/*" />
    PROPERTY NullFormat AS LOGIC GET SELF:__DateTimePicker:CustomFormat == "''"

    // Todo DateTimePicker.ParentNotify
    //	//SE-040929 For a correct focus after closing the calender

    //	IF nCode = DTN_CLOSEUP
    //		IF GetFocus() != SELF:Handle()
    //			SetFocus(SELF:Handle())
    //		ENDIF
    //		keybd_event(VK_RIGHT, 0, 0, 0)
    //	ENDIF

    //	RETURN 0l

/// <include file="Gui.xml" path="doc/DateTimePicker.PerformValidations/*" />
METHOD PerformValidations() CLIPPER
    LOCAL lSuccess		AS LOGIC
    LOCAL uOldValue		AS USUAL

    // DHer: 18/12/2008
    uOldValue := SELF:uValue
    IF _AND(GuiWin32.GetWindowStyle(SELF:Handle()),DTS_TIMEFORMAT)>0
        SELF:uValue := SELF:SelectedTime
    ELSE
        SELF:uValue := SELF:SelectedDate
    ENDIF
    lSuccess := SUPER:PerformValidations()
    SELF:uValue := uOldValue

    RETURN lSuccess

    /// <include file="Gui.xml" path="doc/DateTimePicker.SelectedDate/*" />
    PROPERTY SelectedDate AS DATE
        GET
            IF __DateTimePicker:Checked
                RETURN (DATE) __DateTimePicker:Value
            ENDIF
            RETURN NULL_DATE
        END GET
        SET
            // Suggestion from Håkon Clausen
            SELF:SetDateTime(value, NULL_STRING)
        END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/DateTimePicker.SelectedTime/*" />
    PROPERTY SelectedTime AS STRING
        GET
            LOCAL dSelected AS DateTime
            IF __DateTimePicker:Checked
                dSelected := __DateTimePicker:Value
                RETURN ConTime((DWORD)dSelected:Hour, (DWORD)dSelected:Minute, (DWORD)dSelected:Second)
            ENDIF
            RETURN NULL_STRING
        END GET
        SET
            // Suggestion from Håkon Clausen
            SELF:SetDateTime(NULL_DATE, value)
        END SET
    END PROPERTY

    /// <include file="Gui.xml" path="doc/DateTimePicker.SetDateTime/*" />
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
                NOP
            end try
        ELSE
            __DateTimePicker:CustomFormat	:= "''"
        ENDIF
        __DateTimePicker:Invalidate()
        RETURN

    /// <include file="Gui.xml" path="doc/DateTimePicker.TextValue/*" />
    ACCESS TextValue  AS STRING
        IF (_AND(GuiWin32.GetWindowStyle(SELF:Handle()), DTS_TIMEFORMAT) > 0)
            RETURN SELF:SelectedTime
        ENDIF
        RETURN AsString(SELF:SelectedDate)

    /// <include file="Gui.xml" path="doc/DateTimePicker.Value/*" />
    ACCESS Value as USUAL
        IF _AND(GuiWin32.GetWindowStyle(SELF:Handle()),DTS_TIMEFORMAT)>0
            SELF:uValue := SELF:SelectedTime
        ELSE
            SELF:uValue := SELF:SelectedDate
        ENDIF

        RETURN SUPER:Value
    /// <include file="Gui.xml" path="doc/DateTimePicker.IsNone/*" />
    PROPERTY IsNone  AS LOGIC GET SELF:__DateTimePicker:Value == DateTime.MinValue

    /// <include file="Gui.xml" path="doc/DateTimePicker.IsTimePicker/*" />
    ACCESS IsTimePicker AS LOGIC
        LOCAL nStyle AS LONG
        nStyle := GuiWin32.GetWindowStyle(SELF:__Handle)
        RETURN _AND(nStyle, _OR(DTS_TIMEFORMAT,DTS_UPDOWN) ) > 0


END CLASS

