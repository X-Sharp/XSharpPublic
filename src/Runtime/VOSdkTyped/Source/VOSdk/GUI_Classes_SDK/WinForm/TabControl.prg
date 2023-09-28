//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// TabControl.prg
// This file contains subclasses Windows.Forms controls that are used in the VO Compatible
// Unicode GUI Classes
// Each control has a reference to the VO control and a VOControlProperties object
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control
USING SWF := System.Windows.Forms
USING System.Windows.Forms
USING VOSDK := XSharp.VO.SDK

class VOTabControl inherit SWF.TabControl implements IVOControlProperties,IVOControlInitialize
	#include "PropControlStyle.xh"
	PROPERTY BounceFocus AS LOGIC AUTO := FALSE
    PROPERTY VOTab AS VOSDK.TabControl GET (VOSDK.TabControl) SELF:Control
 
	METHOD Initialize AS VOID STRICT
		SELF:ShowToolTips := TRUE
		SELF:HotTrack	  := TRUE
		SELF:Appearance   := SWF.TabAppearance.Normal

	PROTECTED METHOD OnGotFocus(e AS EventArgs) AS VOID
		IF BounceFocus
			SELF:Parent:Focus()
		ELSE
			SUPER:OnGotFocus(e)
		ENDIF

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()
		SELF:BackColor := System.Drawing.Color.White


	OVERRIDE PROTECTED PROPERTY CreateParams AS SWF.CreateParams
		GET
			LOCAL result := SUPER:CreateParams AS SWF.CreateParams
			IF _AND(result:Style , (INT) 0X00800000) != 0
				// Remove WS_BORDER
				result:Style -= (INT) 0X00800000
			ENDIF
			RETURN result
		END GET
	END PROPERTY

	VIRTUAL PROTECTED METHOD OnKeyDown(ke AS SWF.KeyEventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlNotifyEvent
		SUPER:OnKeyDown(ke)
			oEvent := ControlNotifyEvent{Control}
			oWindow := (Window) SELF:Control:Owner
			oWindow:TabKeyDown(oEvent)
		RETURN

	VIRTUAL PROTECTED METHOD OnSelected(e AS SWF.TabControlEventArgs) AS VOID
		LOCAL oWindow AS Window
		LOCAL oEvent AS ControlNotifyEvent
		SUPER:OnSelected(e)
		VOTab:__FocusPage( SELF:SelectedIndex)
		oEvent := ControlNotifyEvent{Control}
		oEvent:NotifyCode := TCN_SELCHANGE
		oWindow := (Window) SELF:Control:Owner
		oWindow:TabSelect(oEvent)

		RETURN

	VIRTUAL PROTECTED METHOD OnSelecting(e AS SWF.TabControlCancelEventArgs) AS VOID
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
//			//FOREACH oTabPage AS SWF.TabPage IN SELF:TabPages
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
		LOCAL oClient AS SWF.Control
		VAR oPage := SUPER:SelectedTab
		IF oPage != NULL .and. oPage:Controls:Count > 0
			oClient := oPage:Controls[0]
			IF oClient:Size:Width < oPage:ClientRectangle:Width .and. oClient:Size:Height < oPage:ClientRectangle:Height
				oClient:Size := oPage:ClientRectangle:Size
			ENDIF
		ENDIF


END CLASS

