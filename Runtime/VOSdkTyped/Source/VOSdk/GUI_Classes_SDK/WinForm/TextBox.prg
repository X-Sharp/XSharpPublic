// textBox.prg
// This file contains subclasses Windows.Forms controls that are used in the VO Compatible
// Unicode GUI Classes, in particular several TextBox subclasses
// 
// Also some On..() methods have been implemented that call the event handlers on the VO Window
// class that owns the control
#INCLUDE "VOWin32APILibrary.vh"
#USING System.Windows.Forms

/*

class VOAutoComplete inherit AutoComplete IMPLEMENTS IVOControl, IVOControlInitialize
	PRIVATE dwStyleV7 AS LONG
	PRIVATE dwExStyleV7 AS LONG
	//PRIVATE lBusy		AS LOGIC
	PROPERTY oEdit		AS XSharp.VO.Edit GET (XSharp.VO.Edit) SELF:Control

	#include "PropControl.vh"

	METHOD Initialize() AS VOID STRICT
		SELF:AutoSize			:= FALSE
		RETURN
		
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER()
		oProperties := VOControlProperties{SELF, Owner, 0, 0}
		dwStyleV7 := dwStyle
		dwExStyleV7 := dwExStyle
		SELF:Initialize()
		SELF:SetVisualStyle()


	METHOD SetVisualStyle AS VOID STRICT
		IF SELF:oProperties != NULL_OBJECT
				SELF:TabStop			:= _AND(dwStyleV7, WS_TABSTOP) == WS_TABSTOP
				SELF:Properties:UseSystemPasswordChar	:= _AND(dwStyleV7, ES_PASSWORD) == ES_PASSWORD
				SELF:Height := 18
		ENDIF		
		
	VIRTUAL PROPERTY Text AS STRING GET SUPER:Text SET SUPER:Text := Value		

	
	#region Event Handlers
	VIRTUAL PROTECT METHOD OnTextChanged(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlEvent
		SUPER:OnTextChanged(e)
		IF oProperties != NULL_OBJECT .and. SELF:oEdit != NULL_OBJECT
			oEvent := ControlEvent{SELF:oEdit}
			oWindow := (Window) SELF:oEdit:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditChange(oEvent)
			ENDIF
		ENDIF
		RETURN
	
	VIRTUAL PROTECT METHOD OnLeave(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS EditFocusChangeEvent
		//Debout("TextBox:OnLostFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnLeave(e)
		IF oProperties != NULL_OBJECT .and. SELF:oEdit != NULL_OBJECT
			oEvent := EditFocusChangeEvent{SELF:oEdit, FALSE}
			oWindow := (Window) SELF:oEdit:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditFocusChange(oEvent)
			ENDIF
		ENDIF
		RETURN
	

	VIRTUAL PROTECT METHOD OnEnter(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS EditFocusChangeEvent
		//Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnEnter(e)
		IF oProperties != NULL_OBJECT .and. SELF:oEdit != NULL_OBJECT
			oEvent := EditFocusChangeEvent{SELF:oEdit, TRUE}
			oWindow := (Window) SELF:oEdit:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditFocusChange(oEvent)
			ENDIF
		ENDIF
		RETURN

	PROTECT _lInPaint AS LOGIC
	
	VIRTUAL PROTECT METHOD WndProc(msg REF Message) AS VOID
		LOCAL lDone := FALSE AS LOGIC
		//IF oProperties == NULL_OBJECT
		//	lDone := TRUE
		//	SUPER:WndProc(REF msg) 
		// ELSE
		IF msg:Msg == WM_PASTE .and. SELF:oEdit != NULL_OBJECT
			IF IsInstanceOf(oEdit, #SingleLineEdit)
				LOCAL oSle AS SingleLineEdit
				oSle := (SingleLineEdit) oEdit
				IF oSle:__EditString != NULL_OBJECT
					oSle:Paste()
					lDone := TRUE
				ENDIF
			ENDIF
		ENDIF
		IF ! lDone //.and. ! oProperties:WndProc(msg)
			SUPER:WndProc(msg) 
		ENDIF
		RETURN
	#endregion	
END CLASS

*/


	CLASS VOTextBox INHERIT System.Windows.Forms.TextBox IMPLEMENTS IVOControl, IVOControlInitialize
	//PRIVATE lBusy		AS LOGIC
	PROPERTY oEdit		AS XSharp.VO.Edit GET (XSharp.VO.Edit) SELF:Control

	#include "PropControl.vh"

	METHOD Initialize() AS VOID STRICT
		SELF:AutoSize			:= FALSE
		RETURN
	
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER()
			oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SELF:Initialize()
		SELF:SetVisualStyle()


	METHOD SetVisualStyle AS VOID STRICT
		IF SELF:oProperties != NULL_OBJECT
				LOCAL dwStyle AS LONG
				dwStyle					:= _AND(oProperties:Style , _NOT(oProperties:NotStyle))
				SELF:TabStop			:= _AND(dwStyle, WS_TABSTOP) == WS_TABSTOP
				SELF:UseSystemPasswordChar	:= _AND(dwStyle, ES_PASSWORD) == ES_PASSWORD
				SELF:AcceptsReturn		:= _AND(dwStyle, ES_WANTRETURN) == ES_WANTRETURN
				SELF:Multiline			:= _AND(dwStyle, ES_MULTILINE) == ES_MULTILINE 
		ENDIF		
		
	VIRTUAL PROPERTY Text AS STRING GET SUPER:Text SET SUPER:Text := Value		
	
	#region Event Handlers
	VIRTUAL PROTECT METHOD OnTextChanged(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlEvent
		SUPER:OnTextChanged(e)
		IF oProperties != NULL_OBJECT .and. SELF:oEdit != NULL_OBJECT
			oEvent := ControlEvent{SELF:oEdit}
			oWindow := (Window) SELF:oEdit:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditChange(oEvent)
			ENDIF
		ENDIF
		RETURN
	
	VIRTUAL PROTECT METHOD OnLeave(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS EditFocusChangeEvent
		//Debout("TextBox:OnLostFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnLeave(e)
		IF oProperties != NULL_OBJECT .and. SELF:oEdit != NULL_OBJECT
			oEvent := EditFocusChangeEvent{SELF:oEdit, FALSE}
			oWindow := (Window) SELF:oEdit:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditFocusChange(oEvent)
			ENDIF
		ENDIF
		RETURN
	

	VIRTUAL PROTECT METHOD OnEnter(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS EditFocusChangeEvent
		//Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnEnter(e)
		IF oProperties != NULL_OBJECT .and. SELF:oEdit != NULL_OBJECT
			oEvent := EditFocusChangeEvent{SELF:oEdit, TRUE}
			oWindow := (Window) SELF:oEdit:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditFocusChange(oEvent)
			ENDIF
		ENDIF
		RETURN

	PROTECT _lInPaint AS LOGIC
	
	VIRTUAL PROTECT METHOD WndProc(msg REF Message) AS VOID
		LOCAL lDone := FALSE AS LOGIC
		//IF oProperties == NULL_OBJECT
		//	lDone := TRUE
		//	SUPER:WndProc(msg) 
		// ELSE
		IF msg:Msg == WM_PASTE .and. SELF:oEdit != NULL_OBJECT
			IF IsInstanceOf(oEdit, #SingleLineEdit)
				LOCAL oSle AS SingleLineEdit
				oSle := (SingleLineEdit) oEdit
				IF oSle:__EditString != NULL_OBJECT
					oSle:Paste()
					lDone := TRUE
				ENDIF
			ENDIF
		ENDIF
		IF ! lDone //.and. ! oProperties:WndProc(REF msg)
			SUPER:WndProc(REF msg) 
		ENDIF
		RETURN
	#endregion	

	// In V7 gibt es in der Windowprocedure kein WM_PASTE mehr, da das Control (TextEdit) ein Wrapper ist. Deswegen muss Paste hier über ein Keydown gehandelt werden
	METHOD handleKeyDown(sender AS OBJECT, args AS System.Windows.Forms.KeyEventArgs) AS VOID
		IF args:KeyCode == Keys.V && args:Control
			IF IsInstanceOf(oEdit, #SingleLineEdit)
				LOCAL oSle AS SingleLineEdit
				oSle := (SingleLineEdit) oEdit
				IF oSle:__EditString != NULL_OBJECT
					oSle:Paste()
					args:SuppressKeyPress := TRUE
				ENDIF
			ENDIF
		ENDIF
	RETURN

END CLASS

CLASS VOHotKeyTextBox INHERIT VOTextBox IMPLEMENTS IVOControl
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER(Owner, dwStyle, dwExStyle)

	VIRTUAL PROTECTED PROPERTY CreateParams AS System.Windows.Forms.CreateParams 
		GET
			LOCAL IMPLIED result := SUPER:CreateParams
			result:ClassName := HOTKEY_CLASS
			RETURN result
		END GET
	END PROPERTY
END CLASS

CLASS VOMLETextBox INHERIT VOTextBox
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER(Owner,dwStyle,dwExStyle )
		SELF:Multiline := TRUE
	
	VIRTUAL PROTECTED PROPERTY CreateParams AS System.Windows.Forms.CreateParams 
		GET
			LOCAL IMPLIED result := SUPER:CreateParams
			result:style |= (LONG)WS_VSCROLL
			RETURN result
		END GET
	END PROPERTY

	


	PROTECTED METHOD OnKeyDown (e AS System.Windows.Forms.KeyEventArgs) AS VOID STRICT
		// Suppress Escape. Was in VO in MultiLineEdit:Dispatch()
		IF e:KeyCode != System.Windows.Forms.Keys.Escape
			SUPER:OnKeyDown(e)
		ENDIF

END CLASS



CLASS VOIPAddressTextBox INHERIT VOTextBox
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER(Owner,dwStyle,dwExStyle )
	
	VIRTUAL PROTECTED PROPERTY CreateParams AS System.Windows.Forms.CreateParams 
		GET
			LOCAL IMPLIED result := SUPER:CreateParams
			result:ClassName := "SysIPAddress32"
			RETURN result
		END GET
	END PROPERTY

END CLASS


CLASS VORichTextBox INHERIT System.Windows.Forms.RichTextBox IMPLEMENTS IVOControl, IVOControlInitialize
	#include "PropControl.vh"

	METHOD Initialize() AS VOID STRICT
		SELF:AutoSize			:= FALSE
		RETURN


	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()

	METHOD SetVisualStyle AS VOID STRICT
		IF SELF:oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF
	

END CLASS


CLASS VOSpinnerTextBox INHERIT System.Windows.Forms.NumericUpDown IMPLEMENTS IVOControl
	PROPERTY oEdit		AS XSharp.VO.SpinnerEdit GET (XSharp.VO.SpinnerEdit) SELF:Control
	#include "PropControl.vh"

	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Minimum := 0
		SELF:Maximum := System.Int32.MaxValue
		SELF:SetVisualStyle()

	METHOD SetVisualStyle AS VOID STRICT
		IF SELF:oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF

	VIRTUAL PROTECT METHOD OnTextChanged(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlEvent
		SUPER:OnTextChanged(e)
		IF oProperties != NULL_OBJECT .and. SELF:oEdit != NULL_OBJECT
			oEvent := ControlEvent{SELF:oEdit}
			oWindow := (Window) SELF:oEdit:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditChange(oEvent)
			ENDIF
		ENDIF

	VIRTUAL PROTECT METHOD OnEnter(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS EditFocusChangeEvent
		//Debout("TextBox:OnLostFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnEnter(e)
		IF oProperties != NULL_OBJECT
			oEvent := EditFocusChangeEvent{SELF:oEdit, FALSE}
			oWindow := (Window) SELF:oEdit:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditFocusChange(oEvent)
				// Achtung, dies ist ein schneller Workaround für  #5489
				// Dies sollte bei der Bearbeitung von Ticket #5566 reviewed werden
				//oWindow:__form:refresh()
			ENDIF
		ENDIF
		RETURN
	

	VIRTUAL PROTECT METHOD OnLeave(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS EditFocusChangeEvent
		//Debout("TextBox:OnGotFocus", SELF:Control:NameSym,SELF:Control:ControlID, CRLF)
		SUPER:OnLeave(e)
		IF oProperties != NULL_OBJECT
			oEvent := EditFocusChangeEvent{SELF:oEdit, TRUE}
			oWindow := (Window) SELF:oEdit:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditFocusChange(oEvent)
			ENDIF
		ENDIF
		RETURN
	PROPERTY Text AS STRING
		GET 
			RETURN SUPER:Text
		END GET
		SET
			IF STRING.IsNullOrWhiteSpace(VALUE)
				Value := SELF:Minimum:ToString()
			ENDIF
			SUPER:Text := VALUE
		END SET
	END PROPERTY

END CLASS


