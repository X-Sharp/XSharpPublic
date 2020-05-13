// Controls.prg
// This file contains subclasses Windows.Forms controls that are used in the VO Compatible
// Unicode GUI Classes
// Each control has a reference to the VO control and a VOControlProperties object
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control

#USING System.Windows.Forms
#using System.Drawing
#using System.Collections.Generic

CLASS VOButton INHERIT System.Windows.Forms.Button IMPLEMENTS IVOControl, IVOControlInitialize

	#include "PropControl.vh"

    
#region Properties
    PROPERTY Button AS XSharp.VO.Button GET (XSharp.VO.Button) SELF:Control
#endregion        

	METHOD Initialize AS VOID STRICT
		SELF:Margin := Padding{0,0,0,0}
		SELF:AutoSize := FALSE
		SELF:FlatStyle := FlatStyle.System
        SELF:UseCompatibleTextRendering := FALSE

		RETURN
	
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		oProperties:NotStyle := WS_CLIPCHILDREN | WS_CLIPSIBLINGS
		oProperties:NotExStyle := WS_EX_NOPARENTNOTIFY
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()


	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF


	

	PROPERTY DefaultButton AS LOGIC
	GET
		IF oProperties != NULL_OBJECT
			RETURN  _AND(SELF:oProperties:Style , BS_DEFPUSHBUTTON) == BS_DEFPUSHBUTTON
		ENDIF
		RETURN FALSE
	END GET
	END PROPERTY
END CLASS

CLASS VOCheckBox INHERIT System.Windows.Forms.CheckBox IMPLEMENTS IVOControl, IVOControlInitialize
	#include "PropControl.vh"
	
	METHOD Initialize() AS VOID STRICT
			SELF:UseCompatibleTextRendering := FALSE
			SELF:Margin := Padding{0,0,0,0}
			SELF:FlatStyle := FlatStyle.System
		RETURN
		
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
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

	VIRTUAL PROTECTED PROPERTY CreateParams AS System.Windows.Forms.CreateParams 
		GET
			LOCAL IMPLIED result := SUPER:CreateParams
			result:style ~= (LONG)BS_MULTILINE
			RETURN result
		END GET
	END PROPERTY

END CLASS
	
CLASS VORadioButton INHERIT System.Windows.Forms.RadioButton IMPLEMENTS IVOControl, IVOControlInitialize
	
	#include "PropControl.vh"
	PUBLIC lBlockCheckedChanged AS LOGIC
	
	METHOD Initialize() AS VOID STRICT
			SELF:FlatStyle := FlatStyle.System
			SELF:UseCompatibleTextRendering := TRUE
			SELF:Margin := Padding{0,0,0,0}
			SELF:SetStyle(ControlStyles.StandardClick, TRUE)
		RETURN
	
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()
		SELF:ForeColor := System.Drawing.Color.Black

	PUBLIC METHOD myCheckedChanged(sender AS OBJECT, e AS EventArgs) AS VOID
		IF SELF:oProperties != NULL .AND. !lBlockCheckedChanged
			SELF:oProperties:OnClick(sender, e)
		ENDIF
		RETURN


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

CLASS VOGroupBox INHERIT System.Windows.Forms.GroupBox IMPLEMENTS IVOControl, IVOControlInitialize
	#include "PropControl.vh"
	PROPERTY IsRadioGroup AS LOGIC AUTO
	PROTECTED lFound AS LOGIC

	METHOD Initialize AS VOID STRICT
		SELF:Margin := Padding{0}		
		SELF:FlatStyle := FlatStyle.System
		SELF:UseCompatibleTextRendering := FALSE
        SELF:oProperties:OnWndProc += OnWndProc

		RETURN
		
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()
		
	OVERRIDE METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF

	METHOD OnWndProc(m REF Message) AS VOID
		IF (m:Msg == WM_NCHITTEST)
			m:Result := (IntPtr)HTTRANSPARENT
		ENDIF

		
	 METHOD MoveNestedControl(oChild AS System.Windows.Forms.Control) AS LOGIC
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
				FOREACH oC AS System.Windows.Forms.Control IN SELF:Parent:Controls
					IF oC != SELF .AND. oC:Parent == SELF:Parent
						oLocParent := SELF:Location
						oLocChild  := oC:Location
						IF oLocChild:X >= oLocParent:X .and. oLocChild:Y >= oLocParent:Y .and. oC:Width+oLocChild:X <= oLocParent:X+SELF:Width .and. oLocChild:Y + oC:Height <= oLocParent:Y+SELF:Height
							oC:Location := System.Drawing.Point{oLocChild:X, oLocChild:Y + 5 + nOffset}
							lWasMoved := TRUE
							IF IsAccess(((OBJECT)oC), #Control) .AND. IsAssign(((OBJECT)oC):Control, #WasMoved)
								((OBJECT)oC):Control:WasMoved := TRUE
							ENDIF
						ENDIF
					ENDIF
				NEXT
			ENDIF
			SELF:lMoved := lWasMoved
		ENDIF
		RETURN


	 METHOD FindChildren() AS VOID STRICT
		LOCAL aControls AS System.Windows.Forms.Control[]
		IF !lFound .and. SELF:Parent != NULL_OBJECT .and. SELF:Parent:Controls:Count > 0
			aControls := System.Windows.Forms.Control[]{SELF:Parent:Controls:Count}
			SELF:Parent:Controls:CopyTo(aControls, 0)
			SELF:FindChildren(aControls)
        ENDIF
        RETURN
		
	METHOD FindChildren(aControls AS System.Windows.Forms.Control[]) AS VOID STRICT
		LOCAL lAdded AS LOGIC
        LOCAL oLB := NULL AS VOListBox
		IF ! lFound
			LOCAL aNestedGroups AS List<VOGroupBox>
			aNestedGroups := List<VOGroupBox>{}
			FOREACH oChild AS System.Windows.Forms.Control IN aControls
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

	METHOD getAllChildren(aChildren AS System.Collections.Generic.List<IVOControl>) AS System.Collections.Generic.List<IVOControl>
		
		IF aChildren=NULL
			aChildren := System.Collections.Generic.List<IVOControl>{}
		ENDIF
		FOREACH oC AS System.Windows.Forms.Control IN SELF:Controls
			IF oC IS IVoControl VAR oVOC
				aChildren:add(oVOC)
			ENDIF
			IF oC IS VOGroupBox VAR oGroup
				aChildren := oGroup:getAllChildren(aChildren)
			ENDIF
		NEXT
		RETURN aChildren

END CLASS

CLASS VOLabel INHERIT System.Windows.Forms.Label IMPLEMENTS IVOControl, IVOControlInitialize

	#include "PropControl.vh"
	PROPERTY FixedText AS XSharp.VO.FixedText GET (XSharp.VO.FixedText) oProperties:Control

	METHOD Initialize() AS VOID STRICT
		SELF:Margin := Padding{0,0,0,0}
		SELF:UseCompatibleTextRendering := FALSE
        	SELF:ResizeRedraw := TRUE
		
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()
		SELF:ForeColor := System.Drawing.Color.Black

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF
		
		
END CLASS

CLASS VOOwnerDrawnLabel INHERIT VOLabel IMPLEMENTS IVOControl
	PROPERTY FixedText AS XSharp.VO.FixedText GET (XSharp.VO.FixedText) oProperties:Control
	// No need to include because inherits from VOLabel
	
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
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

CLASS VOImageLabel INHERIT System.Windows.Forms.Label IMPLEMENTS IVOControl, IVOControlInitialize
	#include "PropControl.vh"
	
	METHOD Initialize() AS VOID STRICT
		SELF:UseCompatibleTextRendering := FALSE
		RETURN
	
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF		

	
END CLASS

CLASS VOLinkLabel INHERIT System.Windows.Forms.LinkLabel IMPLEMENTS IVOControl, IVOControlInitialize
	#include "PropControl.vh"

	METHOD Initialize() AS VOID STRICT
		SELF:UseCompatibleTextRendering := FALSE
		RETURN

	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF		


END CLASS


CLASS VOProgressBar INHERIT System.Windows.Forms.ProgressBar IMPLEMENTS IVOControl

	#include "PropControl.vh"

	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF		


END CLASS



CLASS VOHScrollBar INHERIT System.Windows.Forms.HScrollBar IMPLEMENTS IVOControl

	#include "PropControl.vh"
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF		


END CLASS

CLASS VOVScrollBar INHERIT System.Windows.Forms.VScrollBar IMPLEMENTS IVOControl
	#include "PropControl.vh"
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()
		
	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF		

END CLASS

CLASS VOSlider INHERIT System.Windows.Forms.TrackBar IMPLEMENTS IVOControl
	#include "PropControl.vh"
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF		

END CLASS

CLASS VOStatusStrip INHERIT System.Windows.Forms.StatusStrip IMPLEMENTS IVOControl
	PRIVATE oTm AS System.Windows.Forms.Timer
	#include "PropControl.vh"
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		oTm := System.Windows.Forms.Timer{}
		oTm:Tick += Timer_Tick
		oTm:Interval := 1000
		oTm:Enabled := TRUE
		SUPER()
		SELF:SetVisualStyle()


	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF		

		

	VIRTUAL METHOD Timer_Tick(sender AS OBJECT, e AS System.EventArgs)  AS VOID
		LOCAL oSb AS XSharp.VO.StatusBar
		oSb := (XSharp.VO.StatusBar) Control
		oSb:Timer()
	
	VIRTUAL PROTECTED METHOD OnItemClicked(e AS ToolStripItemClickedEventArgs) AS VOID
		LOCAL oSb AS XSharp.VO.StatusBar
		oSb := (XSharp.VO.StatusBar) Control
		oSb:OnItemClicked(e:ClickedItem)
	
END CLASS

CLASS VODateTimePicker  INHERIT System.Windows.Forms.DateTimePicker IMPLEMENTS IVOControl
	#include "PropControl.vh"
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()
		

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF		



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

CLASS VOMonthCalendar  INHERIT System.Windows.Forms.MonthCalendar IMPLEMENTS IVOControl
	#include "PropControl.vh"
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()
	 
		SELF:TitleBackColor := System.Drawing.Color.DimGray
		SELF:TrailingForeColor := System.Drawing.Color.LightGray
		SELF:TitleForeColor := System.Drawing.Color.White

	METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF		

	
	PROTECTED METHOD OnDateChanged(e AS System.Windows.Forms.DateRangeEventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnDateChanged(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:MonthCalSelectionChanged(MonthCalSelectionEvent{SELF:Control, FALSE})
		RETURN

	PROTECTED METHOD OnDateSelected(e AS System.Windows.Forms.DateRangeEventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnDateSelected(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:MonthCalSelectionChanged(MonthCalSelectionEvent{SELF:Control, TRUE})
		RETURN
		

	 
END CLASS




