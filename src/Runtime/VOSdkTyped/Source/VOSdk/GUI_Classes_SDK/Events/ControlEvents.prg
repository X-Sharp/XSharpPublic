//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
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
            IF Control != NULL_OBJECT .AND. Control:HyperLabel != NULL_OBJECT
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
    PROPERTY __DtPicker as VODateTimePicker GET (VODateTimePicker) SELF:Control:__Control


    ACCESS SelectedDate AS DATE STRICT
        LOCAL oDT AS VODateTimePicker
        oDT := SELF:__DtPicker
        RETURN (DATE) oDT:Value

    ACCESS SelectedTime AS STRING STRICT
        LOCAL dt AS DateTime
        LOCAL oDT AS VODateTimePicker
        LOCAL sReturn AS STRING
        oDT := SELF:__DtPicker
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
    CONSTRUCTOR(loControl AS VOSDK.Control, lExplicit AS LOGIC)
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
    //Todo RichEditProtectEvent
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
    //Todo RichEditSelectionEvent
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






CLASS SliderEvent INHERIT @@Event
    PROPERTY Slider AS Slider AUTO
    PROPERTY Position AS LONG AUTO

    CONSTRUCTOR(oC AS Slider) STRICT
        SUPER()
        SELF:Slider := oC
        SELF:Position := oC:__TrackBar:Value
    END CLASS

CLASS ScrollEvent INHERIT @@Event
    PROPERTY ScrollBar AS ScrollBar AUTO
    PROPERTY Position AS LONG AUTO
    PROPERTY IsWindowScroll AS LOGIC GET SELF:ScrollBar IS WindowScrollBar
    CONSTRUCTOR (m REF System.Windows.Forms.Message)
        SUPER(REF m)
    CONSTRUCTOR(oC AS Scrollbar) STRICT
        SUPER()
        SELF:ScrollBar  := oC
        SELF:Position   := oC:__ScrollBar:Value

    END CLASS

CLASS SpinnerEvent INHERIT Event
    HIDDEN oSpinner AS Spinner
    PROPERTY SpinnerID  AS LONG     GET Spinner:ControlID
    PROPERTY Spinner    AS Spinner  GET oSpinner
    CONSTRUCTOR (oC AS Spinner)
        SELF:oSpinner := oC

    ACCESS OldPosition AS LONGINT STRICT
        RETURN oSpinner:Position

    ACCESS OldValue AS LONGINT STRICT
        RETURN SELF:OldPosition

    ACCESS Position AS LONGINT STRICT
        RETURN SELF:Spinner:Position


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

    END CLASS


CLASS SysLinkSelectEvent INHERIT ControlNotifyEvent
    //Todo SysLinkSelectEvent
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


