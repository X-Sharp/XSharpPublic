//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// ListBox.prg
// This file contains subclasses Windows.Forms controls that are used in the VO Compatible
// Unicode GUI Classes, in particular several TextBox subclasses
//
// Also some On..() methods have been implemented that call the event handlers on the VO Window
// class that owns the control

USING SWF := System.Windows.Forms
USING VOSDK := XSharp.VO.SDK
USING System.Collections

CLASS VOListBox INHERIT SWF.ListBox IMPLEMENTS IVOControlProperties, IBaseListBox

#region fields
	PROTECTED lBusy AS LOGIC
	PROTECTED _lNoVerticalScrollBar:= FALSE AS LOGIC
	PROTECTED searchString := STRING.Empty AS STRING
	PROTECTED lastKeyPressTime := DateTime.MinValue AS DateTime
#endregion

#region properties
	PROPERTY ListBox AS VOSDK.ListBox
		GET
    		RETURN (VOSDK.ListBox) SELF:Control
		END GET
    END PROPERTY

    NEW PROPERTY Items           AS IList GET SUPER:Items

#endregion
	#include "PropControl.xh"


#region Helper methods
	METHOD Initialize AS VOID STRICT
		self:DisplayMember         := "DisplayValue"
		self:ValueMember           := "Value"
        self:oProperties:OnWndProc += OnWndProc
		return

	METHOD IncrementalSearch( ch AS CHAR) AS LOGIC
		LOCAL nItem AS INT
		IF (DateTime.Now - lastKeyPressTime > TimeSpan{0, 0, 1})
			searchString := ch:ToString()
		ELSE
			searchString += ch:ToString()
		ENDIF
		lastKeyPressTime := DateTime.Now
		nItem := SELF:FindString(searchString,0)
		IF nItem >= 0
			IF nItem != SELF:SelectedIndex
				SELF:ClearSelected()
				SELF:SelectedIndex := nItem
				SELF:Update()
			ENDIF
			RETURN TRUE
		ELSE
			searchString := STRING.Empty
			lastKeyPressTime := DateTime.MinValue
			RETURN FALSE
		ENDIF


	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SELF(Owner, dwStyle, dwExStyle, FALSE)

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG, lHideVerticalScrollBars AS LOGIC)
		LOCAL lSorted AS LOGIC
		IF _AND(dwStyle, LBS_SORT) == LBS_SORT
			lSorted := TRUE
			dwStyle := (LONG) _AND(dwStyle, _NOT(LBS_SORT))
		ENDIF
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:_lNoVerticalScrollBar := lHideVerticalScrollBars
		SELF:Sorted:= lSorted
		SELF:Initialize()
		SELF:SetVisualStyle()

	VIRTUAL METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			LOCAL dwStyle AS LONG
			dwStyle := oProperties:Style
			SELF:TabStop	 := _AND(dwStyle, WS_TABSTOP) == WS_TABSTOP
			SELF:MultiColumn := _AND(dwStyle, LBS_MULTICOLUMN) == LBS_MULTICOLUMN
			SELF:UseTabStops := _AND(dwStyle, LBS_USETABSTOPS) == LBS_USETABSTOPS
			IF _AND(dwStyle, LBS_MULTIPLESEL|LBS_EXTENDEDSEL) != 0
				IF _AND(dwStyle, LBS_EXTENDEDSEL) == LBS_EXTENDEDSEL
					SELF:SelectionMode := SWF.SelectionMode.MultiExtended
				ELSE
					SELF:SelectionMode := SWF.SelectionMode.MultiSimple
				ENDIF
			ELSEIF _AND(dwStyle, LBS_NOSEL) != 0
				SELF:SelectionMode := SWF.SelectionMode.None
			ELSE
				SELF:SelectionMode := SWF.SelectionMode.One
			ENDIF
		ENDIF


#endregion

#region Windows Forms Method and Property overrides

    OVERRIDE PROTECTED PROPERTY CreateParams AS SWF.CreateParams
		GET
			LOCAL IMPLIED result := SUPER:CreateParams

			IF SELF:_lNoVerticalScrollBar
				result:Style := (LONG) _AND(result:Style, _NOT(WS_VSCROLL))
			ENDIF

			RETURN result
		END GET
	END PROPERTY


    // Override Text property to handle empty listox
	VIRTUAL PROPERTY Text AS STRING
	GET
		LOCAL cRetVal AS STRING
		TRY
			IF SELF:SelectedIndices:Count == 0 .OR. SELF:SelectedItems:Count == 0 .OR. SELF:Items:Count == 0
				cRetVal := String.Empty
			ENDIF
			cRetVal := SUPER:Text
		CATCH
			RETURN String.Empty
		END TRY
		RETURN cRetVal
	END GET
	SET
		SUPER:Text := Value
	END SET
	END PROPERTY


	VIRTUAL PROTECTED METHOD OnKeyPress(e AS SWF.KeyPressEventArgs) AS VOID
		SUPER:OnKeyPress(e)
		e:handled := SELF:IncrementalSearch(e:KeyChar)
		RETURN


	PROTECT METHOD OnMouseDoubleClick(e AS SWF.MouseEventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlEvent
		SUPER:OnMouseDoubleClick(e)
		IF SELF:Control != NULL_OBJECT
			oWindow := (Window) SELF:Control:Owner
			IF oWindow != NULL_OBJECT
				oEvent := ControlEvent{SELF:Control}
				oWindow:ListBoxClick(oEvent)
			ENDIF
		ENDIF
		RETURN

	VIRTUAL PROTECT METHOD OnSelectedIndexChanged (e AS EventArgs) AS VOID STRICT
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlEvent
		IF SELF:Control != NULL_OBJECT
			IF ! SELF:lBusy .and. ! SELF:ListBox:IsBusy
				// Prevent recursion
				SELF:lBusy := TRUE
				TRY
					oWindow := (Window) SELF:Control:Owner
					IF oWindow != NULL_OBJECT
						oEvent := ControlEvent{SELF:Control}
						oWindow:ListBoxSelect(oEvent)
					ENDIF
				FINALLY
					SELF:lBusy := FALSE
				END TRY
			ENDIF
		ENDIF
		SUPER:OnSelectedIndexChanged(e)	// This also triggers the Event Handlers
		RETURN

#endregion


    // This gets called from the WndProc event handler
	VIRTUAL METHOD OnWndProc(msg REF SWF.Message) AS VOID
		// Windows forms does not raise a mouse double click event for the right mouse button
		IF SELF:Control == NULL_OBJECT
			// do nothing
            NOP
		ELSEIF (msg:Msg == WM_RBUTTONDBLCLK)
			var me := SWF.MouseEventArgs{SWF.MouseButtons.Right, 2, LOWORD((DWORD) msg:LParam:ToInt32()), HIWORD((DWORD) msg:LParam:ToInt32()), 0}
			SELF:Control:MouseButtonDoubleClick(MouseEvent{me, SWF.Control.ModifierKeys})
		ELSEIF (msg:Msg == WM_MBUTTONDBLCLK)
			var me := SWF.MouseEventArgs{SWF.MouseButtons.Middle, 2, LOWORD((DWORD) msg:LParam:ToInt32()), HIWORD((DWORD) msg:LParam:ToInt32()), 0}
			SELF:Control:MouseButtonDoubleClick(MouseEvent{me, SWF.Control.ModifierKeys})
		ELSEIF (msg:Msg == WM_XBUTTONDBLCLK)
			var me := SWF.MouseEventArgs{SWF.MouseButtons.XButton1, 2, LOWORD((DWORD) msg:LParam:ToInt32()), HIWORD((DWORD) msg:LParam:ToInt32()), 0}
			SELF:Control:MouseButtonDoubleClick(MouseEvent{me, SWF.Control.ModifierKeys})
		ENDIF
		RETURN



END CLASS

CLASS VOComboBox INHERIT SWF.ComboBox IMPLEMENTS IVOControlProperties, IBaseListBox
	PROPERTY ComboBox AS VOSDK.ComboBox GET (VOSDK.ComboBox) SELF:Control
	PROTECTED searchString := STRING.Empty AS STRING
	PROTECTED lastKeyPressTime := DateTime.MinValue AS DateTime
	PROTECTED lBusy AS LOGIC
    protected cSavedString as string
	#include "PropControlStyle.xh"

	METHOD Initialize AS VOID STRICT
		self:DisplayMember	:= "DisplayValue"
		self:ValueMember	:= "Value"
		SELF:FlatStyle		:= SWF.FlatStyle.System
        self:Margin			:= SWF.Padding{0}
		return

    NEW PROPERTY AutoCompleteSource AS DWORD GET (DWORD) SUPER:AutoCompleteSource SET SUPER:AutoCompleteSource := (SWF.AutoCompleteSource) VALUE
    NEW Property Items as IList GET SuPER:Items
	PROPERTY Text AS STRING
		GET
			IF SELF:IsDisposed
				RETURN cSavedString
			ELSE
				// During closing of a window the items list may be empty causing a crash
				LOCAL cResult AS STRING
				TRY
					IF SELF:DropDownStyle == SWF.ComboBoxStyle.DropDownList
						IF SELF:SelectedIndex >= 0 .and. SELF:Items:Count > 0
							cResult := SUPER:Text
						ELSE
							cResult := STRING.Empty
						ENDIF
					ELSE
						cResult := SUPER:Text
					ENDIF
				CATCH
					cResult := STRING.Empty
				END TRY
				RETURN cResult

			ENDIF
		END GET
		SET
			SUPER:Text := value
		END SET
	END PROPERTY

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		LOCAL lSorted AS LOGIC
		IF _AND(dwStyle, CBS_SORT) == CBS_SORT
			lSorted := TRUE
			dwStyle := (LONG) _AND(dwStyle, _NOT(CBS_SORT))
		ENDIF
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Sorted:= lSorted
		SELF:Initialize()
		SELF:SetVisualStyle()

	method IncrementalSearch( ch as char) as logic
		LOCAL nItem AS INT
		IF (DateTime.Now - lastKeyPressTime > TimeSpan{0, 0, 1})
			searchString := ch:ToString()
		ELSE
			searchString += ch:ToString()
		ENDIF
		lastKeyPressTime := DateTime.Now
		nItem := SELF:FindString(searchString,0)
		IF nItem >= 0
			IF nItem != SELF:SelectedIndex
				SELF:SelectedIndex := nItem
				SELF:Update()
			ENDIF
			RETURN TRUE
		ELSEIF searchString == " "
			FOREACH oItem AS ListBoxItemValue IN SELF:Items
				IF oItem:DisplayValue = ""
					SELF:SelectedIndex := SELF:Items:IndexOf(oItem)
					SELF:Update()
					RETURN TRUE
				ENDIF
			NEXT
			searchString := STRING.Empty
			lastKeyPressTime := DateTime.MinValue
			RETURN FALSE
		ELSE
			searchString := STRING.Empty
			lastKeyPressTime := DateTime.MinValue
			RETURN FALSE
		ENDIF


	VIRTUAL PROTECTED METHOD DestroyHandle AS VOID STRICT
		SELF:cSavedString := SELF:Text
		SUPER:DestroyHandle()
		RETURN

	VIRTUAL PROTECTED METHOD OnKeyPress(e AS SWF.KeyPressEventArgs) AS VOID
		SUPER:OnKeyPress(e)
		IF SELF:DropDownStyle == SWF.ComboBoxStyle.DropDownList
			e:handled := SELF:IncrementalSearch(e:KeyChar)
		ENDIF
		RETURN

	VIRTUAL PROTECT METHOD OnLostFocus(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS EditFocusChangeEvent
		SUPER:OnLostFocus(e)
		IF SELF:oProperties != NULL_OBJECT
			oEvent := EditFocusChangeEvent{SELF:ComboBox, FALSE}
			oWindow := (Window) SELF:ComboBox:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditFocusChange(oEvent)
			ENDIF
		ENDIF
		RETURN

	VIRTUAL PROTECT METHOD OnGotFocus(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS EditFocusChangeEvent
		SUPER:OnGotFocus(e)
		IF SELF:oProperties != NULL_OBJECT
			oEvent := EditFocusChangeEvent{SELF:ComboBox, TRUE}
			oWindow := (Window) SELF:ComboBox:Owner
			IF oWindow != NULL_OBJECT
				oWindow:EditFocusChange(oEvent)
			ENDIF
		ENDIF
		RETURN

	VIRTUAL PROTECT METHOD OnSelectedIndexChanged (e AS EventArgs) AS VOID STRICT
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlEvent
		IF ! SELF:lBusy .and. SELF:ComboBox != NULL_OBJECT .and. !SELF:ComboBox:IsBusy
			SELF:lBusy := TRUE
			TRY
				oWindow := (Window) SELF:Control:Owner
				IF oWindow != NULL_OBJECT
					oEvent := ControlEvent{SELF:Control}
					oWindow:ListBoxSelect(oEvent)
				ENDIF
			FINALLY
				SELF:lBusy := FALSE
			END TRY
		ENDIF
			SUPER:OnSelectedIndexChanged(e)	// This also triggers the Event Handlers
		RETURN


	VIRTUAL PROTECT METHOD OnMouseDoubleClick(e AS SWF.MouseEventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlEvent
		SUPER:OnMouseDoubleClick(e)
		oWindow := (Window) SELF:Control:Owner
		IF oWindow != NULL_OBJECT
			oEvent := ControlEvent{SELF:Control}
			oWindow:ListBoxClick(oEvent)
		ENDIF
		RETURN

	VIRTUAL PROTECT METHOD OnTextChanged(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnTextChanged(e)
		IF ! SELF:lBusy .and. oProperties != NULL_OBJECT .and. SELF:ComboBox != NULL_OBJECT .and. !SELF:ComboBox:IsBusy
			SELF:lBusy := TRUE
			TRY
				SELF:ComboBox:__EditChange()
				oWindow := (Window) SELF:Control:Owner

				IF oWindow != NULL_OBJECT
					oWindow:EditChange(ControlEvent{SELF:Control})
				ENDIF
			FINALLY
				SELF:lBusy := FALSE
			END TRY
		ENDIF

END CLASS
