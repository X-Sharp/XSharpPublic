//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// MenuToolbar.prg
// This file contains subclasses for the Menu and Toolbar Controls
// Unicode GUI Classes
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control

USING SWF := System.Windows.Forms

USING System.Windows.Forms
USING System.Reflection
USING System.Collections.Generic
USING VOSDK := XSharp.VO.SDK
CLASS VOMenu INHERIT SWF.MainMenu

	CONSTRUCTOR() STRICT
		SUPER()

	METHOD GetItemByID(nItemID as LONG) AS VOMenuItem
		RETURN VOMenu.GetMenuItem(SELF, nItemID)

	STATIC METHOD GetMenuItem(oMenu as SWF.Menu, nItemID as Long) as VOMenuItem
		FOREACH oItem as VOMenuItem in oMenu:MenuItems
			IF oItem:MenuItemID == nItemID
				RETURN oItem
			ENDIF
			IF oItem:IsParent
				LOCAL oResult as VOMenuItem
				oResult := GetMenuItem(oItem, nItemID)
				IF oResult != NULL_OBJECT
					RETURN oResult
				ENDIF
			ENDIF
		NEXT
		RETURN NULL_OBJECT

	ACCESS MenuItemArray AS SWF.MenuItem[]
		LOCAL oItems AS List<SWF.MenuItem>
		oItems := List<SWF.MenuItem>{}
		IF SELF:MenuItems:Count == 1
			// First Item is 'Dummy'
			// Rest of the items are the real thing
			LOCAL oFirst AS SWF.MenuItem
			oFirst := SELF:MenuItems[0]
			foreach oItem as SWF.MenuItem in oFirst:MenuItems
				oItems:Add(oItem)
			NEXT

		ELSE
			foreach oItem as SWF.MenuItem in self:MenuItems
				oItems:Add(oItem)
			NEXT
		ENDIF
		return oItems:ToArray()

	METHOD AsContextMenu AS SWF.ContextMenu STRICT
		RETURN SWF.ContextMenu{SELF:MenuItemArray}

#ifdef DEBUG
    PROTECTED METHOD ProcessCmdKey (msg REF SWF.Message , keyData AS SWF.Keys ) AS LOGIC
        System.Diagnostics.Debug.WriteLine(keyData:ToString())
        RETURN SUPER:ProcessCmdKey(REF msg, keyData)
#endif
END CLASS

CLASS VOMenuItem INHERIT SWF.MenuItem

	INTERNAL MenuItemID as LONG
	CONSTRUCTOR() STRICT
		SUPER()

	CONSTRUCTOR(cText as STRING) STRICT
		SUPER(cText)

	METHOD SetShortCut(nValue AS LONG) AS VOID
		LOCAL oData AS OBJECT
		GetShortCutInfo()
		IF dataFieldInfo != NULL_OBJECT .and. shortCutInfo != NULL_OBJECT
			oData := dataFieldInfo:GetValue(SELF)
			IF oData != NULL_OBJECT
				shortCutInfo:SetValue(oData, nValue)
			ENDIF
		ENDIF

	METHOD CloneMenu() AS SWF.MenuItem STRICT
	    LOCAL item AS VOMenuItem
		item := VOMenuItem{}
		item:CloneMenu(SELF)
		item:MenuItemID := SELF:MenuItemID
        item:Click += ItemClick
		RETURN item
    METHOD ItemClick (sender AS OBJECT, e AS EventArgs) AS VOID
        RETURN

	ACCESS MenuItemArray AS SWF.MenuItem[]
		LOCAL oItems AS List<SWF.MenuItem>
		oItems := List<SWF.MenuItem>{}
		IF SELF:MenuItems:Count == 1
			// First Item is 'Dummy'
			// Rest of the items are the real thing
			VAR oFirst := SELF:MenuItems[0]
			foreach oItem as SWF.MenuItem in oFirst:MenuItems
				oItems:Add(oItem)
			NEXT

		ELSE
			FOREACH oItem AS SWF.MenuItem IN SELF:MenuItems
				oItems:Add(oItem)
			NEXT
		ENDIF
		RETURN oItems:ToArray()

	#region Static Methods
	STATIC PROTECTED dataFieldInfo as FieldInfo
	STATIC PROTECTED shortCutInfo  as FieldInfo

	STATIC PROTECTED METHOD GetShortCutInfo AS VOID STRICT
		IF dataFieldInfo == NULL_OBJECT
			LOCAL oType as System.Type
			oType := typeof(SWF.MenuItem)
			dataFieldInfo := oType:GetField("data", BindingFlags.Instance|BindingFlags.NonPublic)
			IF dataFieldInfo != NULL_OBJECT
				oType := dataFieldInfo:FieldType
				shortCutInfo := oType:GetField("shortcut", BindingFlags.Instance|BindingFlags.NonPublic)
			endif
		endif
	#endregion
END CLASS

CLASS VOMainMenu INHERIT SWF.MainMenu
	CONSTRUCTOR() STRICT
		SUPER()

END CLASS

CLASS VOToolStrip INHERIT SWF.ToolStrip
	CONSTRUCTOR() STRICT
		SUPER()

END CLASS



CLASS VOSeparator INHERIT SWF.ToolStripSeparator
	CONSTRUCTOR() STRICT
		SUPER()

END CLASS

CLASS VOToolStripButton INHERIT SWF.ToolStripButton
	CONSTRUCTOR() STRICT
		SUPER()

END CLASS


class VOToolBar inherit SWF.ToolBar implements IVOControlProperties
    #include "PropControlStyle.xh"

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:ButtonSize := System.Drawing.Size{20,20}


	METHOD GetButton(nID AS LONG, lMenuId AS LOGIC) AS SWF.ToolBarButton
		if lMenuId
			FOREACH oButton as VOToolBarButton in Buttons
				if oButton:MenuID == nID
					RETURN oButton
				ENDIF
			NEXT
		ELSE
			if nID <= SELF:Buttons:Count
				LOCAL IMPLIED oButton := SELF:Buttons[nID-1]
				RETURN oButton
			ENDIF
		ENDIF
		RETURN NULL_OBJECT


    PROTECTED VIRTUAL METHOD OnMouseMove(e as SWF.MouseEventArgs) AS VOID
        SUPER:OnMouseMove(e)
        if SELF:Bounds:Contains(Point{e:X, e:Y})
            SELF:ShowStatusMessage(e:X, e:Y)
        ENDIF
        RETURN

    PRIVATE METHOD ShowStatusMessage(X as LONG, Y as LONG) AS VOID
        LOCAL selectedButton := NULL_OBJECT as VOToolBarButton
        FOREACH button as VOToolBarButton in SELF:Buttons
            local rect := button:Rectangle as System.Drawing.Rectangle
            if rect:Left <= X .and. rect:Right >= X
                if rect:Top <= Y .and. rect:Bottom >= Y
                    selectedButton := button
                    EXIT
                ENDIF
            ENDIF
        NEXT
        IF selectedButton != NULL_OBJECT
            LOCAL nMenuId := 0 as LONG
            LOCAL oWindow as Window
            LOCAL oMenu   as Menu
            oWindow := self:Control:Owner
            oMenu   := oWindow:Menu
            nMenuId := selectedButton:MenuID
           oWindow:MenuSelect(MenuSelectEvent{oMenu, oWindow, nMenuId})
        ENDIF


	METHOD PressButton(nID as LONG, lPressed as LOGIC) AS LOGIC
		LOCAL oButton as SWF.ToolBarButton
		oButton := SELF:GetButton(nID, TRUE)
		IF oButton != NULL_OBJECT
			oButton:Pushed := lPressed
			RETURN TRUE
		ENDIF
		RETURN FALSE

	PROTECTED VIRTUAL METHOD OnResize(e AS EventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnResize(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ToolBarHeightChanged(ControlNotifyEvent{SELF:Control})
		RETURN

	PROTECTED VIRTUAL METHOD OnHandleCreated( e AS EventArgs) AS VOID
		SUPER:OnHandleCreated(e)
		RETURN
END CLASS

CLASS VOToolBarButton INHERIT SWF.ToolBarButton
	INTERNAL MenuID := 0 AS LONG

	CONSTRUCTOR() STRICT
		SUPER()
		RETURN

END CLASS


CLASS VOToolTip INHERIT SWF.ToolTip

	CONSTRUCTOR()
		SUPER()
		SELF:StripAmpersands := TRUE
		SELF:BackColor := System.Drawing.Color.LightYellow
		SELF:OwnerDraw := TRUE
		SELF:Draw	+= OnMyDraw

	PRIVATE METHOD OnMyDraw(sender AS OBJECT, e AS SWF.DrawToolTipEventArgs ) AS VOID
		e:DrawBackground()
        e:DrawBorder()
        e:DrawText()

	METHOD Stop() AS VOID STRICT
		SELF:Active := FALSE
		SELF:StopTimer()
		SELF:RemoveAll()

END CLASS
