//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Properties.prg


USING System.Windows.Forms
USING System.ComponentModel
USING System.Drawing
USING VOSDK := XSharp.VO.SDK

CLASS VOProperties
	DELEGATE StyleChanged_Delegate() AS VOID STRICT
	property Style as long      auto
	property ExStyle as  long   auto
	property NotStyle as long   auto
	property NotExStyle as long auto
    EVENT StyleChanged AS StyleChanged_Delegate
	EVENT ExStyleChanged AS StyleChanged_Delegate

	CONSTRUCTOR() STRICT

	CONSTRUCTOR(liStyle AS LONG, dwExStyle AS LONG)
		Style	:= liStyle
		ExStyle := dwExStyle

	METHOD SetStyles(CreateParams AS System.Windows.Forms.CreateParams ) AS VOID
		CreateParams:Style |= Style
		CreateParams:Style ~= NotStyle
		CreateParams:ExStyle |= ExStyle
		CreateParams:ExStyle ~= NotExStyle
		RETURN


	METHOD SetStyle(kStyle AS LONG, lEnable AS LOGIC) AS VOID
		IF lEnable
			SELF:Style |= kStyle
		ELSE
			SELF:NotStyle |= kStyle
		ENDIF
		IF StyleChanged != NULL
			SELF:StyleChanged()
		ENDIF


	METHOD SetExStyle(kStyle AS LONG, lEnable AS LOGIC) AS VOID
		IF lEnable
			SELF:ExStyle |= kStyle
		ELSE
			SELF:NotExStyle |= kStyle
		ENDIF
		IF ExStyleChanged != NULL
			SELF:ExStyleChanged()
		ENDIF

	ACCESS TextAlignment AS ContentAlignment
		LOCAL liStyle AS LONG
		LOCAL liAlign AS ContentAlignment
		liAlign := ContentAlignment.MiddleLeft
		liStyle := _AND(Style , _NOT(NotStyle))
		IF _AND(liStyle, BS_CENTER) == BS_CENTER
			IF _AND(liStyle, BS_TOP) == BS_TOP
				liAlign := ContentAlignment.TopCenter
			ELSEIF _AND(liStyle, BS_VCENTER) == BS_VCENTER
				liAlign := ContentAlignment.MiddleCenter
			ELSE
				liAlign := ContentAlignment.BottomCenter
			ENDIF
		ELSEIF _AND(liStyle, BS_RIGHT) == BS_RIGHT
			IF _AND(liStyle, BS_TOP) == BS_TOP
				liAlign := ContentAlignment.TopRight
			ELSEIF _AND(liStyle, BS_VCENTER) == BS_VCENTER
				liAlign := ContentAlignment.MiddleRight
			ELSE
				liAlign := ContentAlignment.BottomRight
			ENDIF
		ELSE
			IF _AND(liStyle, BS_TOP) == BS_TOP
				liAlign := ContentAlignment.TopLeft
			ELSEIF _AND(liStyle, BS_VCENTER) == BS_VCENTER
				liAlign := ContentAlignment.MiddleLeft
			ELSE
				liAlign := ContentAlignment.BottomLeft
			ENDIF

		ENDIF
		return liAlign


END CLASS


DELEGATE WndProc(msg REF Message) AS VOID

CLASS VOControlProperties INHERIT VOProperties
	PROPERTY oWFC AS System.Windows.Forms.Control  AUTO GET PRIVATE SET
	PROPERTY Control AS VOSDK.Control AUTO GET PRIVATE SET
	PROPERTY Window AS VOSDK.Window AUTO GET PRIVATE SET
	PROTECT _lHandleDoubleClickThroughMouseUp AS LOGIC

    PUBLIC EVENT OnWndProc AS WndProc


    METHOD Dispatch(m REF Message) AS VOID
        IF OnWndProc != NULL
            SELF:OnWndProc( REF m)
        ENDIF
        VAR oEvent := Event{REF m}
        SELF:Control:Dispatch( oEvent)
        SELF:Window:Dispatch( oEvent)


	ACCESS ModifierKeys AS Keys
		RETURN System.Windows.Forms.Control.ModifierKeys

	METHOD LinkTo(oOwner AS VOSDK.Control) AS VOID STRICT
		Control := oOwner
		Window := oOwner:Owner
		IF oWFC IS IVOControlProperties VAR oVOC
			SELF:StyleChanged += oVOC:SetVisualStyle
		ENDIF



	CONSTRUCTOR(oControl AS System.Windows.Forms.Control, oOwner AS VOSDK.Control)
		SELF(oControl, oOwner, 0, 0)


	CONSTRUCTOR(oControl AS System.Windows.Forms.Control, oOwner AS VOSDK.Control, liStyle AS LONG, dwExStyle AS LONG)
		LOCAL lFlagDoubleClick AS LOGIC
		SUPER(liStyle, dwExStyle)
		oWFC := oControl
		oWFC:Enter   += OnGotFocus
		oWFC:Leave  += OnLostFocus
		oWFC:KeyDown	+= OnKeyDown
		oWFC:KeyUp		+= OnKeyUp
		oWFC:MouseDown	+= OnMouseDown
		oWFC:MouseUp	+= OnMouseUp
		oWFC:MouseMove  += OnMouseMove
		oWFC:HelpRequested += OnHelpRequested


		// Handle Treeview and Listbox Doubleclicks through the actual doubleclick-event (and SinglelineEdits)
		// Because DoubleClicks on Treeviews (allg. Voreinstellungen) and Listviews (Multifelder) weren't responding
		lFlagDoubleClick := oOwner IS VOSDK.TreeView .OR. oOwner IS VOSDK.ListBox
		SELF:_lHandleDoubleClickThroughMouseUp := !lFlagDoubleClick
		IF lFlagDoubleClick
			oWFC:MouseDoubleClick += OnMouseDoubleClick
		ELSEIF oOwner IS VOSDK.SingleLineEdit
			oWFC:MouseDoubleClick += OnMouseDoubleClick
		ELSEIF oOwner IS VOSDK.Button
			oWFC:Click			+= OnClick
			oWFC:DoubleClick	+= OnDoubleClick
		ENDIF
		self:LinkTo(oOwner)

	PUBLIC METHOD OnClick(sender AS OBJECT, e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlEvent
		IF SELF:Control != NULL_OBJECT
			oEvent := ControlEvent{Control}
			oWindow := (Window) SELF:Control:Owner
			IF oWindow != NULL_OBJECT
				// Wenn es ein Datawindow ist, muss hier PreventComboGather abgefragt werden, da durch einen Skip oder ein GoBottom kein Click-Event ausgelöst werden darf
				// Dies würde z.b. passieren, wenn durch einen Skip eine datengebundene Checkbox gesetzt wird
				IF !oWindow:__CommandFromEvent(oEvent)
					oWindow:ButtonClick(oEvent)
				ELSE
					oWindow:EventReturnValue := 1L
				ENDIF
			ENDIF
		ENDIF
		RETURN

	PROTECTED METHOD OnDoubleClick(sender AS OBJECT, e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlEvent
		IF SELF:Control != NULL_OBJECT
			oEvent := ControlEvent{Control}
			oWindow := (Window) SELF:Control:Owner
			IF oWindow != NULL_OBJECT
				oWindow:ButtonDoubleClick(oEvent)
			ENDIF
		ENDIF
		RETURN


	method OnKeyDown(s as object, e as KeyEventArgs) as void
		IF SELF:Control != NULL_OBJECT
			LOCAL k AS KeyEvent
			k := KeyEvent{e}
			SELF:Control:EventReturnValue := 0
			SELF:Control:KeyDown(k)
			IF SELF:Window != NULL_OBJECT .AND. SELF:Control:EventReturnValue == 0
				self:Window:KeyDown(k)
			ENDIF
		ENDIF
		return

	method OnKeyUp(s as object, e as KeyEventArgs) as void
		IF SELF:Control != NULL_OBJECT
			LOCAL k AS KeyEvent
			k := KeyEvent{e}
			SELF:Control:EventReturnValue := 0
			SELF:Control:KeyUp(k)
			IF SELF:Window != NULL_OBJECT .AND. SELF:Control:EventReturnValue == 0
				self:Window:KeyUp(k)
			ENDIF
			IF SELF:COntrol IS VOSDK.Combobox
				SELF:Control:__Update()
			ENDIF
		ENDIF
		RETURN

	METHOD AdjustMouseEventPosition(e AS MouseEventArgs) AS MouseEventArgs
		LOCAL oDeltas AS Tuple<LONG,LONG>
		oDeltas := SELF:getPositionDelta()
		RETURN MouseEventArgs{e:Button, e:clicks, e:X+oDeltas:Item1, e:y+oDeltas:Item2, e:delta}

	METHOD RevertEventPosition(p AS System.Drawing.Point) AS System.Drawing.Point
		LOCAL oDeltas AS Tuple<LONG,LONG>
		oDeltas := SELF:getPositionDelta()
		RETURN System.Drawing.Point{p:X-oDeltas:Item1, p:Y-oDeltas:Item2}

	METHOD getPositionDelta() AS Tuple<LONG,LONG>
		LOCAL nDeltaX, nDeltaY AS LONG
		LOCAL oParent AS System.Windows.Forms.Control
		oParent := SELF:oWFC
		DO WHILE oParent != NULL
			IF oParent is System.Windows.Forms.Form
				// We are at the form level, so exit
				EXIT
			ENDIF
			nDeltaX += oParent:Location:X
			nDeltaY += oParent:Location:Y
			oParent := oParent:Parent
		ENDDO
		RETURN Tuple<LONG,LONG>{nDeltaX, nDeltaY}


	method OnMouseDown(s as object, e as MouseEventArgs) as void
		IF SELF:Control != NULL_OBJECT
			LOCAL m AS MouseEvent
			e := SELF:AdjustMouseEventPosition(e)
			m := MouseEvent{e, SELF:ModifierKeys}
			SELF:Control:EventReturnValue := 0
			SELF:Control:MouseButtonDown(m)
			//IF SELF:Window != NULL_OBJECT .AND. SELF:Control:EventReturnValue == 0
			//	SELF:Window:MouseButtonDown(m)
			//ENDIF
		ENDIF
		RETURN

	method OnMouseUp(s as object, e as MouseEventArgs) as void
		IF SELF:Control != NULL_OBJECT
			e := SELF:AdjustMouseEventPosition(e)
			IF SELF:_lHandleDoubleClickThroughMouseUp .AND. e:Clicks == 2
				SELF:OnMouseDoubleClick (s, e)
			ELSE
				LOCAL m AS MouseEvent
				m := MouseEvent{e, SELF:ModifierKeys}
				SELF:Control:EventReturnValue := 0
				SELF:Control:MouseButtonUp(m)
				//IF SELF:Window != NULL_OBJECT .AND. SELF:Control:EventReturnValue == 0
				//	SELF:Window:MouseButtonUp(m)
				//ENDIF
			ENDIF
		ENDIF
		RETURN


	method OnMouseDoubleClick(s as object, e as MouseEventArgs) as void
		IF SELF:Control != NULL_OBJECT
			LOCAL m AS MouseEvent
			e := SELF:AdjustMouseEventPosition(e)
			m := MouseEvent{e, SELF:ModifierKeys}
			SELF:Control:EventReturnValue := 0
			SELF:Control:MouseButtonDoubleClick(m)
			//IF SELF:Window != NULL_OBJECT .AND. SELF:Control:EventReturnValue == 0
			//	SELF:Window:MouseButtonDoubleClick(m)
			//ENDIF
		ENDIF
		RETURN

	method OnMouseMove(s as object, e as MouseEventArgs) as void
		IF SELF:Control != NULL_OBJECT
			LOCAL m AS MouseEvent
			e := SELF:AdjustMouseEventPosition(e)
			m := MouseEvent{e, SELF:ModifierKeys}
			IF e:Button == MouseButtons.None
				SELF:Control:MouseMove(m)
			ELSE
				SELF:Control:MouseDrag(m)
			ENDIF
			SELF:Control:ShowToolTip()
		ENDIF
		RETURN

	method OnGotFocus(s as object, e as EventArgs) as void
		//Debout("ControlProperties:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		IF SELF:Control != NULL_OBJECT
			SELF:Control:FocusChange(FocusChangeEvent{TRUE})
		ENDIF
		return

	method OnLostFocus(s as object, e as EventArgs) as void
		//Debout("ControlProperties:OnLostFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		IF SELF:Control != NULL_OBJECT
			SELF:Control:FocusChange(FocusChangeEvent{FALSE})
		ENDIF
		return

	method OnHelpRequested(s as object, e as HelpEventArgs ) as void
		IF SELF:Window != NULL_OBJECT
			SELF:Window:HelpRequest(HelpRequestEvent{e, s})
		ENDIF
		return
END CLASS


CLASS VOFormProperties INHERIT VOProperties
	PROPERTY Form AS System.Windows.Forms.Form  AUTO
	PROPERTY Window AS VOSDK.Window         AUTO

	CONSTRUCTOR(oForm AS System.Windows.Forms.Form, oWindow AS VOSDK.Window)
		SUPER()
		Window := oWindow
		Form  := oForm
		oForm:Enter   += OnGotFocus
		oForm:Leave  += OnLostFocus

		oForm:MenuStart += OnMenuStart
		oForm:MenuComplete += OnMenuComplete
		// oForm:HelpRequested += OnHelpRequested
		oForm:HelpButtonClicked += OnHelpButtonClicked
		oForm:Activated += OnActivated
		oForm:Deactivate += OnDeactivate


	method OnGotFocus(s as object, e as EventArgs) as void
		IF SELF:Window != NULL_OBJECT
			SELF:Window:FocusChange(FocusChangeEvent{TRUE})
		ENDIF
		return

	method OnLostFocus(s as object, e as EventArgs) as void
		IF SELF:Window != NULL_OBJECT
			SELF:Window:FocusChange(FocusChangeEvent{FALSE})
		ENDIF
		return

	method OnMenuComplete(s as object, e as EventArgs) as void
		//	SELF:Window:MenuComplete(MenuSelectEvent{})
		return

	method OnMenuStart(s as object, e as EventArgs) as void
		//	SELF:Window:MenuInit(MenuInitEvent{})
		return

	method OnHelpRequested(s as object, e as HelpEventArgs ) as void
		IF SELF:Window != NULL_OBJECT
			SELF:Window:HelpRequest(HelpRequestEvent{e, s})
		ENDIF
		return

	method OnHelpButtonClicked(s as object, e as CancelEventArgs ) as void
		//	SELF:Window:MenuInit(MenuInitEvent{})
		return
	METHOD OnActivated(s AS OBJECT, e AS EventArgs) AS VOID
		IF SELF:Window != NULL_OBJECT
			SELF:Window:Activate(@@Event{})
		ENDIF

	METHOD OnDeactivate(s AS OBJECT, e AS EventArgs) AS VOID
		IF SELF:Window != NULL_OBJECT
			SELF:Window:DeActivate(@@Event{})
		ENDIF

END CLASS
