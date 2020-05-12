// TabControl.prg
// This file contains subclasses Windows.Forms controls that are used in the VO Compatible
// Unicode GUI Classes
// Each control has a reference to the VO control and a VOControlProperties object
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control

#USING System.Windows.Forms

CLASS VOTabControl INHERIT System.Windows.Forms.TabControl IMPLEMENTS IVOControl, IVOControlInitialize
	#include "PropControl.vh"
	EXPORT BounceFocus:= FALSE AS LOGIC
    PROPERTY VOTab AS XSharp.VO.TabControl GET (XSharp.VO.TabControl) SELF:Control

	METHOD Initialize AS VOID STRICT
		SELF:ShowToolTips := TRUE
		SELF:HotTrack	  := TRUE
		SELF:Appearance   := TabAppearance.Normal

	PROTECTED METHOD OnGotFocus(e AS EventArgs) AS VOID
		IF BounceFocus
			SELF:Parent:Focus()
		ELSE
			SUPER:OnGotFocus(e)
		ENDIF
	
	CONSTRUCTOR(Owner AS XSharp.VO.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()
		SELF:BackColor := System.Drawing.Color.White

	METHOD SetVisualStyle AS VOID STRICT
		// Empty but required

	VIRTUAL PROTECTED PROPERTY CreateParams AS System.Windows.Forms.CreateParams 
		GET
			LOCAL result := SUPER:CreateParams AS System.Windows.Forms.CreateParams 
			IF _AND(result:Style , (INT) 0X00800000) != 0		
				// Remove WS_BORDER
				result:Style -= (INT) 0X00800000
			ENDIF
			RETURN result
		END GET
	END PROPERTY
	
	VIRTUAL PROTECTED METHOD OnKeyDown(ke AS KeyEventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlNotifyEvent
		SUPER:OnKeyDown(ke)
			oEvent := ControlNotifyEvent{Control}
			oWindow := (Window) SELF:Control:Owner
			oWindow:TabKeyDown(oEvent)
		RETURN
	
	VIRTUAL PROTECTED METHOD OnSelected(e AS TabControlEventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlNotifyEvent
		SUPER:OnSelected(e)
		VOTab:__FocusPage( SELF:SelectedIndex)		
		oEvent := ControlNotifyEvent{Control}
		oEvent:NotifyCode := TCN_SELCHANGE
		oWindow := (Window) SELF:Control:Owner
		oWindow:TabSelect(oEvent)
		
		RETURN

	VIRTUAL PROTECTED METHOD OnSelecting(e AS TabControlCancelEventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlNotifyEvent
		SUPER:OnSelecting(e)
		oEvent := ControlNotifyEvent{Control}
		oEvent:NotifyCode := TCN_SELCHANGING
		oWindow := (Window) SELF:Control:Owner
		oWindow:TabSelectionChanging(oEvent)
		RETURN
	VIRTUAL PROTECT METHOD OnVisibleChanged(e AS EventArgs) AS VOID
		SUPER:OnVisibleChanged(e)
//		IF SELF:Visible
//			//FOREACH oTabPage AS System.Windows.Forms.TabPage IN SELF:TabPages
//			//	oTabPage:BackColor := VOPanel.DefBackColor
//			//NEXT
//		ENDIF
		
	VIRTUAL PROTECT METHOD OnTabIndexChanged(e AS EventArgs) AS VOID
		SUPER:OnTabIndexChanged(e)
		SELF:ResizeCurrentPage()
		
	VIRTUAL PROTECT METHOD OnSizeChanged(e AS EventArgs) AS VOID
		SUPER:OnSizeChanged(e)			
		SELF:ResizeCurrentPage()
		
	METHOD ResizeCurrentPage AS VOID
		LOCAL oPage AS TabPage
		LOCAL oClient AS System.Windows.Forms.Control
		oPage := SUPER:SelectedTab
		IF oPage != NULL .and. oPage:Controls:Count > 0
			oClient := oPage:Controls[0]
			IF oClient:Size:Width < oPage:ClientRectangle:Width .and. oClient:Size:Height < oPage:ClientRectangle:Height
				oClient:Size := oPage:ClientRectangle:Size
			ENDIF
		ENDIF
				

END CLASS

