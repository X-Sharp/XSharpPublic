// ListBox.prg
// This file contains subclasses Windows.Forms controls that are used in the VO Compatible
// Unicode GUI Classes, in particular several TextBox subclasses
//  
// Also some On..() methods have been implemented that call the event handlers on the VO Window
// class that owns the control

#USING System.Windows.Forms


CLASS VOListBox INHERIT System.Windows.Forms.ListBox IMPLEMENTS IVOControl, IVOControlInitialize

#region fields    
	PROTECTED lBusy AS LOGIC
	PROTECTED _lNoVerticalScrollBar:= FALSE AS LOGIC
	PROTECTED searchString := STRING.Empty AS STRING
	PROTECTED lastKeyPressTime := DateTime.MinValue AS DateTime
#endregion

#region properties
	PROPERTY ListBox AS XSharp.VO.ListBox 
		GET 
    		RETURN (XSharp.VO.ListBox) SELF:Control 
		END GET
    END PROPERTY


#endregion
	#include "PropControl.vh"


#region Helper methods
	METHOD Initialize AS VOID STRICT
		SELF:DisplayMember         := "DisplayValue"
		SELF:ValueMember           := "Value"
		SELF:DrawMode              := DrawMode.OwnerDrawFixed
		//SELF:DrawItem += SupportFunctions.listBox_DrawItem
        SELF:oProperties:OnWndProc += OnWndProc

		RETURN
		
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


	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SELF(Owner, dwStyle, dwExStyle, FALSE)
				
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG, lHideVerticalScrollBars AS LOGIC)
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
		
	OVERRIDE METHOD SetVisualStyle AS VOID STRICT
		IF oProperties != NULL_OBJECT
			LOCAL dwStyle AS LONG
			dwStyle := oProperties:Style
			SELF:TabStop	 := _AND(dwStyle, WS_TABSTOP) == WS_TABSTOP
			SELF:MultiColumn := _AND(dwStyle, LBS_MULTICOLUMN) == LBS_MULTICOLUMN
			SELF:UseTabStops := _AND(dwStyle, LBS_USETABSTOPS) == LBS_USETABSTOPS
			IF _AND(dwStyle, LBS_MULTIPLESEL|LBS_EXTENDEDSEL) != 0
				IF _AND(dwStyle, LBS_EXTENDEDSEL) == LBS_EXTENDEDSEL
					SELF:SelectionMode := SelectionMode.MultiExtended
				ELSE
					SELF:SelectionMode := SelectionMode.MultiSimple
				ENDIF
			ELSEIF _AND(dwStyle, LBS_NOSEL) != 0
				SELF:SelectionMode := SelectionMode.None
			ELSE
				SELF:SelectionMode := SelectionMode.One
			ENDIF
		ENDIF


#endregion

#region Windows Forms Method and Property overrides    

    VIRTUAL PROTECTED PROPERTY CreateParams AS System.Windows.Forms.CreateParams 
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


	VIRTUAL PROTECTED METHOD OnKeyPress(e AS KeyPressEventArgs) AS VOID
		SUPER:OnKeyPress(e)
		e:handled := SELF:IncrementalSearch(e:KeyChar)
		RETURN


	PROTECT METHOD OnMouseDoubleClick(e AS MouseEventArgs) AS VOID	
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
	VIRTUAL METHOD OnWndProc(msg REF Message) AS VOID	
		// Windows forms does not raise a mouse double click event for the right mouse button
		IF SELF:Control == NULL_OBJECT
			// do nothing
		ELSEIF (msg:Msg == WM_RBUTTONDBLCLK) 
			LOCAL me AS MouseEventArgs
			me := MouseEventArgs{MouseButtons.Right, 2, LOWORD((DWORD) msg:LParam:ToInt32()), HIWORD((DWORD) msg:LParam:ToInt32()), 0}
			SELF:Control:MouseButtonDoubleClick(MouseEvent{me, System.Windows.Forms.Control.ModifierKeys})				
		ELSEIF (msg:Msg == WM_MBUTTONDBLCLK) 
			LOCAL me AS MouseEventArgs
			me := MouseEventArgs{MouseButtons.Middle, 2, LOWORD((DWORD) msg:LParam:ToInt32()), HIWORD((DWORD) msg:LParam:ToInt32()), 0}
			SELF:Control:MouseButtonDoubleClick(MouseEvent{me, System.Windows.Forms.Control.ModifierKeys})				
		ELSEIF (msg:Msg == WM_XBUTTONDBLCLK) 
			LOCAL me AS MouseEventArgs
			me := MouseEventArgs{MouseButtons.XButton1, 2, LOWORD((DWORD) msg:LParam:ToInt32()), HIWORD((DWORD) msg:LParam:ToInt32()), 0}
			SELF:Control:MouseButtonDoubleClick(MouseEvent{me, System.Windows.Forms.Control.ModifierKeys})				
		ENDIF
		RETURN
	

	
END CLASS

CLASS VOComboBox INHERIT System.Windows.Forms.ComboBox IMPLEMENTS IVOControl, IVOControlInitialize
	PROPERTY ComboBox AS XSharp.VO.ComboBox GET (XSharp.VO.ComboBox) SELF:Control
	PROTECTED searchString := STRING.Empty AS STRING
	PROTECTED lastKeyPressTime := DateTime.MinValue AS DateTime
	PROTECTED lBusy AS LOGIC
	PROTECTED cSavedString AS STRING
	#include "PropControl.vh"

	METHOD Initialize AS VOID STRICT
		SELF:DisplayMember	:= "DisplayValue"
		SELF:ValueMember	:= "Value"
		SELF:FlatStyle		:= FlatStyle.System
		SELF:Margin			:= Padding{0,0,0,0}
		RETURN


	PROPERTY Text AS STRING
		GET
			IF SELF:IsDisposed
				RETURN cSavedString
			ELSE
				// During closing of a window the items list may be empty causing a crash
				LOCAL cResult AS STRING
				TRY
					IF SELF:DropDownStyle == ComboBoxStyle.DropDownList
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

	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
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
		SELF:DrawMode := DrawMode.OwnerDrawFixed
		//SELF:DrawItem += SupportFunctions.comboBox_DrawItem

	
	METHOD SetVisualStyle	 AS VOID STRICT
		IF SELF:oProperties != NULL_OBJECT
			SELF:TabStop := (_AND(oProperties:Style, WS_TABSTOP) == WS_TABSTOP)
		ENDIF
	



	METHOD IncrementalSearch( ch AS Char) AS LOGIC
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

	VIRTUAL PROTECTED METHOD OnKeyPress(e AS KeyPressEventArgs) AS VOID
		SUPER:OnKeyPress(e)
		IF SELF:DropDownStyle == ComboBoxStyle.DropDownList
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
	
	
	VIRTUAL PROTECT METHOD OnMouseDoubleClick(e AS MouseEventArgs) AS VOID	
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
