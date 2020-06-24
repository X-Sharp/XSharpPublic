// MenuToolbar.prg
// This file contains subclasses for the Menu and Toolbar Controls
// Unicode GUI Classes
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control

USING System.Windows.Forms
USING System.Reflection
USING System.Collections.Generic
USING VOSDK := XSharp.VO.SDK
CLASS VOMenu INHERIT System.Windows.Forms.MainMenu

	CONSTRUCTOR() STRICT
		SUPER()

	METHOD GetItemByID(nItemID as LONG) AS VOMenuItem
		RETURN VOMenu.GetMenuItem(SELF, nItemID)
	
	STATIC METHOD GetMenuItem(oMenu as System.Windows.Forms.Menu, nItemID as Long) as VOMenuItem
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
		
	ACCESS MenuItemArray AS MenuItem[]
		LOCAL oItems AS List<MenuItem>
		oItems := List<MenuItem>{}
		IF SELF:MenuItems:Count == 1
			// First Item is 'Dummy' 
			// Rest of the items are the real thing
			LOCAL oFirst AS MenuItem
			oFirst := SELF:MenuItems[0]
			FOREACH IMPLIED oItem IN oFirst:MenuItems
				oItems:Add(oItem)
			NEXT
			
		ELSE			
			FOREACH IMPLIED oItem IN SELF:MenuItems
				oItems:Add(oItem)
			NEXT
		ENDIF
		RETURN oItems:ToArray()
	
	METHOD AsContextMenu AS ContextMenu STRICT
		RETURN ContextMenu{SELF:MenuItemArray}
	

    PROTECTED METHOD ProcessCmdKey (msg REF System.Windows.Forms.Message , keyData AS System.Windows.Forms.Keys ) AS LOGIC
        System.Diagnostics.Debug.WriteLine(keyData:ToString())
        RETURN SUPER:ProcessCmdKey(REF msg, keyData)

END CLASS

CLASS VOMenuItem INHERIT MenuItem
	
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
	
	METHOD CloneMenu() AS System.Windows.Forms.MenuItem STRICT
	    LOCAL item AS VOMenuItem
		item := VOMenuItem{}
		item:CloneMenu(SELF)
		item:MenuItemID := SELF:MenuItemID
        item:Click += ItemClick
		RETURN item
    METHOD ItemClick (sender AS OBJECT, e AS EventArgs) AS VOID
        RETURN

	ACCESS MenuItemArray AS MenuItem[]
		LOCAL oItems AS List<MenuItem>
		oItems := List<MenuItem>{}
		IF SELF:MenuItems:Count == 1
			// First Item is 'Dummy' 
			// Rest of the items are the real thing
			LOCAL oFirst AS MenuItem
			oFirst := SELF:MenuItems[0]
			FOREACH IMPLIED oItem IN oFirst:MenuItems
				oItems:Add(oItem)
			NEXT
			
		ELSE			
			FOREACH IMPLIED oItem IN SELF:MenuItems
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
			oType := typeof(System.Windows.Forms.MenuItem)
			dataFieldInfo := oType:GetField("data", BindingFlags.Instance|BindingFlags.NonPublic)
			IF dataFieldInfo != NULL_OBJECT
				oType := dataFieldInfo:FieldType
				shortCutInfo := oType:GetField("shortcut", BindingFlags.Instance|BindingFlags.NonPublic)
			endif
		endif
	#endregion
END CLASS

CLASS VOMainMenu INHERIT MainMenu
	CONSTRUCTOR() STRICT
		SUPER()

END CLASS

CLASS VOToolStrip INHERIT ToolStrip
	CONSTRUCTOR() STRICT
		SUPER()

END CLASS



CLASS VOSeparator INHERIT ToolStripSeparator
	CONSTRUCTOR() STRICT
		SUPER()

END CLASS

CLASS VOToolStripButton INHERIT ToolStripButton
	CONSTRUCTOR() STRICT
		SUPER()

END CLASS


CLASS VOToolBar INHERIT System.Windows.Forms.ToolBar IMPLEMENTS IVOControl
	#include "PropControl.vh"

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:ButtonSize := System.Drawing.Size{20,20}

	METHOD SetVisualStyle as VOID STRICT
		// Empty but required
		
	METHOD GetButton(nID AS LONG, lMenuId AS LOGIC) AS ToolBarButton
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
	
	
	METHOD PressButton(nID as LONG, lPressed as LOGIC) AS LOGIC
		LOCAL oButton as ToolBarButton
		oButton := GetButton(nID, TRUE)
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

CLASS VOToolBarButton INHERIT ToolBarButton
	INTERNAL MenuID := 0 AS LONG 
	
	CONSTRUCTOR() STRICT
		SUPER()
		RETURN
		
END CLASS


CLASS VOToolTip INHERIT ToolTip
	
	CONSTRUCTOR()
		SUPER()
		SELF:StripAmpersands := TRUE
		SELF:BackColor := System.Drawing.Color.LightYellow
		SELF:OwnerDraw := TRUE
		SELF:Draw	+= OnMyDraw

	PRIVATE METHOD OnMyDraw(sender AS OBJECT, e AS System.Windows.Forms.DrawToolTipEventArgs ) AS VOID
		e:DrawBackground()
        e:DrawBorder()
        e:DrawText()
	
	METHOD Stop() AS VOID STRICT
		SELF:Active := FALSE
		SELF:StopTimer()
		SELF:RemoveAll()
		
END CLASS
