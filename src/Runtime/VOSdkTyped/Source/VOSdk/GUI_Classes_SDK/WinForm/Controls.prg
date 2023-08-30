//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Controls.prg
// This file contains subclasses Windows.Forms controls that are used in the VO Compatible
// Unicode GUI Classes
// Each control has a reference to the VO control and a VOControlProperties object
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control

USING System.Windows.Forms
USING SWF := System.Windows.Forms
USING System.Drawing
USING System.Collections.Generic
USING VOSDK := XSharp.VO.SDK

class VOButton inherit SWF.Button implements IVoControlProperties
    #include "PropControlStyle.xh"

#region Properties
    PROPERTY Button AS VOSDK.Button GET (VOSDK.Button) SELF:Control
#endregion

	METHOD Initialize AS VOID STRICT
		SELF:Margin := Padding{0}
		SELF:AutoSize := FALSE
		SELF:FlatStyle := FlatStyle.System
        SELF:UseCompatibleTextRendering := FALSE

		RETURN

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		oProperties:NotStyle := WS_CLIPCHILDREN | WS_CLIPSIBLINGS
		oProperties:NotExStyle := WS_EX_NOPARENTNOTIFY
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()





	PROPERTY DefaultButton AS LOGIC
	GET
		IF oProperties != NULL_OBJECT
			RETURN  _AND(SELF:oProperties:Style , BS_DEFPUSHBUTTON) == BS_DEFPUSHBUTTON
		ENDIF
		RETURN FALSE
	END GET
	END PROPERTY
END CLASS

class VOCheckBox inherit SWF.CheckBox  implements IVoControlProperties
    #include "PropControl.xh"


	METHOD Initialize() AS VOID STRICT
			SELF:UseCompatibleTextRendering := FALSE
			SELF:Margin     := Padding{0}
			SELF:FlatStyle  := FlatStyle.System
		RETURN

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()
		SELF:ForeColor := System.Drawing.Color.Black

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
			LOCAL dwStyle AS LONG
			dwStyle := _AND(oProperties:Style , _NOT(oProperties:NotStyle))
			SELF:AutoCheck := _AND(dwStyle, BS_AUTOCHECKBOX) == BS_AUTOCHECKBOX
			IF _AND(dwStyle, BS_LEFTTEXT) == BS_LEFTTEXT
				SELF:CheckAlign := ContentAlignment.MiddleRight
			ELSE
				SELF:CheckAlign := ContentAlignment.MiddleLeft
			ENDIF
			SELF:TextAlign := oProperties:TextAlignment
		ENDIF

	OVERRIDE PROTECTED PROPERTY CreateParams AS SWF.CreateParams
		GET
			LOCAL IMPLIED result := SUPER:CreateParams
			result:style ~= (LONG)BS_MULTILINE
			RETURN result
		END GET
	END PROPERTY

END CLASS

class VORadioButton inherit SWF.RadioButton  implements IVoControlProperties
    #include "PropControl.xh"

	PROPERTY SuppressCheckedChanged AS LOGIC AUTO := FALSE

	METHOD Initialize() AS VOID STRICT
		SELF:FlatStyle := FlatStyle.System
		SELF:UseCompatibleTextRendering := TRUE
		SELF:Margin := Padding{0}
		SELF:SetStyle(ControlStyles.StandardClick, TRUE)
		RETURN

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()
		SELF:ForeColor := System.Drawing.Color.Black

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
			LOCAL dwStyle AS LONG
			dwStyle := _AND(oProperties:Style , _NOT(oProperties:NotStyle))
			SELF:AutoCheck := _AND(dwStyle, BS_AUTORADIOBUTTON) == BS_AUTORADIOBUTTON
			IF _AND(dwStyle, BS_LEFTTEXT) == BS_LEFTTEXT
				SELF:CheckAlign := ContentAlignment.MiddleRight
			ELSE
				SELF:CheckAlign := ContentAlignment.MiddleLeft
			ENDIF
			SELF:TextAlign := oProperties:TextAlignment
		ENDIF
		SELF:AutoSize := FALSE

END CLASS

class VOGroupBox inherit SWF.GroupBox  implements IVoControlProperties
    #include "PropControlStyle.xh"
	PROPERTY IsRadioGroup AS LOGIC AUTO
	PROTECTED lFound AS LOGIC

    METHOD SetChildIndex(oCtrl AS IVOControl, nIndex AS LONG) AS VOID
        IF oCtrl IS SWF.Control VAR oC
            SELF:Controls:SetChildIndex(oC,nIndex)
        ENDIF

    METHOD AddControl(oCtrl AS IVOControl) AS VOID
        IF oCtrl IS SWF.Control VAR oC
            SELF:Controls:Add(oC)
        ENDIF

	METHOD Initialize AS VOID STRICT
		SELF:Margin := Padding{0}
		SELF:FlatStyle := FlatStyle.System
		SELF:UseCompatibleTextRendering := FALSE
        SELF:oProperties:OnWndProc += OnWndProc

		RETURN

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()

	method OnWndProc(m ref Message) as void
		IF (m:Msg == WM_NCHITTEST)
			m:Result := (IntPtr)HTTRANSPARENT
		ENDIF


	 METHOD MoveNestedControl(oChild AS IVOControl) AS LOGIC
		LOCAL oLocParent AS System.Drawing.Point
		LOCAL oLocChild  AS System.Drawing.Point

		IF oChild == SELF .or. oChild:Parent != SELF:Parent
			RETURN FALSE
		ENDIF
		oLocParent := SELF:Location
		oLocChild  := oChild:Location
		IF oLocChild:X >= oLocParent:X .and. oLocChild:Y >= oLocParent:Y ;
				.and. oChild:Width+oLocChild:X <= oLocParent:X+SELF:Width .and. oLocChild:Y + oChild:Height <= oLocParent:Y+SELF:Height
			IF oChild:GetType() == typeof(VOGroupBox)
				IF oChild:Location == SELF:Location .and. oChild:Width == SELF:Width .and. oChild:Height = SELF:Height
					RETURN FALSE
				ENDIF
			ENDIF
			IF SELF:TabIndex > oChild:TabIndex
				SELF:TabIndex := oChild:TabIndex
			ENDIF
			oChild:Parent := SELF
			oLocChild:X -= oLocParent:X
			oLocChild:Y -= oLocParent:Y
			oChild:Location := oLocChild
			RETURN TRUE
		ENDIF
		RETURN FALSE

	PROTECTED lMoved := FALSE AS LOGIC
	METHOD MoveChildren(nOffset AS DWORD) AS VOID
		LOCAL oLocParent AS System.Drawing.Point
		LOCAL oLocChild  AS System.Drawing.Point
		LOCAL lWasMoved AS LOGIC

		IF !lMoved
			IF SELF:Parent != NULL_OBJECT .and. SELF:Parent:Controls:Count > 0 .and. SELF:Text != NULL .AND. SELF:Text:Length > 0
				FOREACH oC AS SWF.Control IN SELF:Parent:Controls
					IF oC != SELF .AND. oC:Parent == SELF:Parent
						oLocParent := SELF:Location
						oLocChild  := oC:Location
						IF oLocChild:X >= oLocParent:X .and. oLocChild:Y >= oLocParent:Y .and. oC:Width+oLocChild:X <= oLocParent:X+SELF:Width .and. oLocChild:Y + oC:Height <= oLocParent:Y+SELF:Height
							oC:Location := System.Drawing.Point{oLocChild:X, oLocChild:Y + 5 + nOffset}
                            lWasMoved := true
                            if oC is IVOControlProperties var oVOControl .and. IsAssign(oVOControl, #WasMoved)
								IVarPut(oVOControl:Control,#WasMoved, true)
							ENDIF
						ENDIF
					ENDIF
				NEXT
			ENDIF
			SELF:lMoved := lWasMoved
		ENDIF
		RETURN


	 METHOD FindChildren() AS VOID STRICT
		LOCAL aControls AS IVOControl[]
		IF !lFound .and. SELF:Parent != NULL_OBJECT .and. SELF:Parent:Controls:Count > 0
			aControls := IVOControl[]{SELF:Parent:Controls:Count}
			SELF:Parent:Controls:CopyTo(aControls, 0)
			SELF:FindChildren(aControls)
        ENDIF
        RETURN

	METHOD FindChildren(aControls AS IVOControl[]) AS VOID STRICT
		LOCAL lAdded AS LOGIC
        LOCAL oLB := NULL AS VOListBox
		IF ! lFound
			LOCAL aNestedGroups AS List<VOGroupBox>
			aNestedGroups := List<VOGroupBox>{}
			FOREACH oChild AS IVOControl IN aControls
				IF oChild != SELF
					IF ! (oChild IS VOFramePanel)
						IF oChild IS VOListBox VAR oList
                            oLB := oList
							IF oLB:Items:Count == 0
								oLB:Items:Add(ListBoxItemValue{"",0})
								lAdded  := TRUE
							ELSE
								lAdded := FALSE
							ENDIF
						ENDIF
						IF SELF:MoveNestedControl(oChild)
							IF oChild IS VOGroupBox VAR oGroup
								aNestedGroups:Add(oGroup )
							ENDIF
						ENDIF
						IF lAdded .and. oLB != NULL_OBJECT
							oLB:Items:Clear()
						ENDIF
						oLB := NULL_OBJECT
					ENDIF
				ENDIF
			NEXT
			FOREACH oC AS VOGroupBox IN aNestedGroups
				oC:FindChildren()
			NEXT
			SELF:lFound := TRUE
		ENDIF
		RETURN

	METHOD GetAllChildren(aChildren AS IList<IVOControl>) AS IList<IVOControl>

		IF aChildren =NULL
			aChildren := System.Collections.Generic.List<IVOControl>{}
		ENDIF
		FOREACH oC AS SWF.Control IN SELF:Controls
			IF oC IS IVoControl VAR oVOC
				aChildren:add(oVOC)
			ENDIF
			IF oC IS VOGroupBox VAR oGroup
				aChildren := oGroup:GetAllChildren(aChildren)
			ENDIF
		NEXT
		RETURN aChildren

END CLASS

class VOLabel inherit SWF.Label  implements  IVOControlProperties
    #include "PropControlStyle.xh"

	property FixedText as VOSDK.FixedText get (VOSDK.FixedText) oProperties:Control

	METHOD Initialize() AS VOID STRICT
		SELF:Margin := Padding{0}
		SELF:UseCompatibleTextRendering := FALSE
        	SELF:ResizeRedraw := TRUE

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()
		SELF:ForeColor := System.Drawing.Color.Black
end class

class VOOwnerDrawnLabel inherit VOLabel  implements  IVOControlProperties, IVOControl
	PROPERTY FixedText AS VOSDK.FixedText GET (VOSDK.FixedText) oProperties:Control
	// No need to include because inherits from VOLabel

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER(Owner, dwStyle, dwExStyle)

//	#define WM_NCHITTEST 0X0084
//	#define HTTRANSPARENT (-1)

	VIRTUAL PROTECT METHOD WndProc(m REF Message) AS VOID
		IF (m:Msg == WM_NCHITTEST)
			m:Result := (IntPtr)HTTRANSPARENT
		ELSE
			SUPER:WndProc( REF m)
		ENDIF


	VIRTUAL PROTECT METHOD OnPaint(e AS PaintEventArgs) AS VOID
		IF SELF:oProperties == NULL_OBJECT
			SUPER:OnPaint(e)
		ELSEIF ! SELF:FixedText:OnPaint(e)
			SUPER:OnPaint(e)
		ENDIF


END CLASS

class VOImageLabel inherit VOLabel  implements  IVOControlProperties

	METHOD Initialize() AS VOID STRICT
		SELF:UseCompatibleTextRendering := FALSE
		RETURN

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()

END CLASS

class VOLinkLabel inherit SWF.LinkLabel  implements  IVOControlProperties
        #include "PropControlStyle.xh"

	method Initialize() as void strict
		SELF:UseCompatibleTextRendering := FALSE
		RETURN

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()



END CLASS


class VOProgressBar inherit SWF.ProgressBar  implements  IVOControlProperties
        #include "PropControlStyle.xh"

	constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		super()
		SELF:SetVisualStyle()


END CLASS



class VOHScrollBar inherit SWF.HScrollBar  implements  IVOControlProperties
    PROPERTY ScrollBar AS ScrollBar GET (ScrollBar) SELF:Control
    #include "PropControlStyle.xh"

	constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()

    PROTECTED METHOD OnValueChanged (e AS EventArgs) AS VOID
	    LOCAL oWindow AS Window
		LOCAL oEvent AS ScrollEvent
		//Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnValueChanged(e)
		IF oProperties != NULL_OBJECT
			oEvent := ScrollEvent{SELF:Scrollbar}
			IF oProperties:Window != NULL_OBJECT
                oProperties:Window:HorizontalScroll(oEvent)
    		ENDIF
		ENDIF
		RETURN
END CLASS

class VOHSpinner inherit SWF.HScrollBar  implements  IVOControlProperties
    PROPERTY Spinner AS Spinner GET (Spinner) SELF:Control
    #include "PropControlStyle.xh"

    constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()

    PROTECTED METHOD OnValueChanged (e AS EventArgs) AS VOID
	    LOCAL oWindow AS Window
		LOCAL oEvent AS SpinnerEvent
		//Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnValueChanged(e)
		IF oProperties != NULL_OBJECT
			oEvent := SpinnerEvent{SELF:Spinner}
			IF oProperties:Window != NULL_OBJECT
                oProperties:Window:HorizontalSpin(oEvent)
    		ENDIF
		ENDIF
		RETURN

END CLASS

class VOVScrollBar inherit SWF.VScrollBar  implements  IVOControlProperties
        #include "PropControlStyle.xh"

    property ScrollBar as ScrollBar get (ScrollBar) self:Control

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()

    protected method OnValueChanged (e as EventArgs) as void
	    LOCAL oWindow AS Window
		LOCAL oEvent AS ScrollEvent
		//Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnValueChanged(e)
		IF oProperties != NULL_OBJECT
			oEvent := ScrollEvent{SELF:Scrollbar}
			IF oProperties:Window != NULL_OBJECT
                oProperties:Window:VerticalScroll(oEvent)
    		ENDIF
		ENDIF
		RETURN

END CLASS

class VOVSpinner inherit SWF.NumericUpDown  implements  IVOControlProperties
        #include "PropControlStyle.xh"

    property Spinner as Spinner get (Spinner) self:Control

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()


    PROTECTED METHOD OnValueChanged (e AS EventArgs) AS VOID
	    LOCAL oWindow AS Window
		LOCAL oEvent AS SpinnerEvent
		//Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnValueChanged(e)
		IF oProperties != NULL_OBJECT
			oEvent := SpinnerEvent{SELF:Spinner}
			IF oProperties:Window != NULL_OBJECT
                oProperties:Window:VerticalSpin(oEvent)
    		ENDIF
		ENDIF
		RETURN
END CLASS


class VOSlider inherit SWF.TrackBar  implements IVOControlProperties
        #include "PropControlStyle.xh"

    property Slider as Slider get (Slider) self:Control

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()

    protected method OnValueChanged (e as EventArgs) as void
	    LOCAL oWindow AS Window
		LOCAL oEvent AS SliderEvent
		//Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnValueChanged(e)
		IF oProperties != NULL_OBJECT
			oEvent := SliderEvent{SELF:Slider}
			IF oProperties:Window != NULL_OBJECT
                IF SELF:Orientation == Orientation.Horizontal
				    oProperties:Window:HorizontalSlide(oEvent)
                ELSE
                    oProperties:Window:VerticalSlide(oEvent)
                ENDIF
			ENDIF
		ENDIF
		RETURN

END CLASS

class VOStatusBar inherit SWF.StatusStrip  implements IVOControlProperties
	PRIVATE oTm AS SWF.Timer
    #include "PropControlStyle.xh"
    constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		oTm := SWF.Timer{}
		oTm:Tick += Timer_Tick
		oTm:Interval := 1000
		oTm:Enabled := TRUE
		SUPER()
		SELF:SetVisualStyle()


	VIRTUAL METHOD Timer_Tick(sender AS OBJECT, e AS System.EventArgs)  AS VOID
		LOCAL oSb AS VOSDK.StatusBar
		oSb := (VOSDK.StatusBar) Control
		oSb:Timer()

	VIRTUAL PROTECTED METHOD OnItemClicked(e AS ToolStripItemClickedEventArgs) AS VOID
		LOCAL oSb AS VOSDK.StatusBar
		oSb := (VOSDK.StatusBar) Control
		oSb:OnItemClicked(e:ClickedItem)

END CLASS

class VODateTimePicker  inherit SWF.DateTimePicker implements IVOControlProperties
    #include "PropControlStyle.xh"
    constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()


	PROTECTED METHOD OnValueChanged (e AS EventArgs) AS VOID
	    SUPER:OnValueChanged(e)
        //	LOCAL oWindow AS Window
        //	oWindow := (Window) SELF:Control:Owner
		//oWindow:DateTimeSelectionChanged(DateTimeSelectionEvent{SELF:Control})
		RETURN

	PROTECTED METHOD OnFormatChanged (e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnFormatChanged(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:DateTimeSelectionChanged(DateTimeSelectionEvent{SELF:Control})
		RETURN


END CLASS

class VOMonthCalendar  inherit SWF.MonthCalendar implements IVOControlProperties
    #include "PropControlStyle.xh"
	constructor(Owner as VOSDK.Control, dwStyle as long, dwExStyle as long)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()

		SELF:TitleBackColor := System.Drawing.Color.DimGray
		SELF:TrailingForeColor := System.Drawing.Color.LightGray
		SELF:TitleForeColor := System.Drawing.Color.White


	PROTECTED METHOD OnDateChanged(e AS SWF.DateRangeEventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnDateChanged(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:MonthCalSelectionChanged(MonthCalSelectionEvent{SELF:Control, FALSE})
		RETURN

	PROTECTED METHOD OnDateSelected(e AS SWF.DateRangeEventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnDateSelected(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:MonthCalSelectionChanged(MonthCalSelectionEvent{SELF:Control, TRUE})
		RETURN



END CLASS




