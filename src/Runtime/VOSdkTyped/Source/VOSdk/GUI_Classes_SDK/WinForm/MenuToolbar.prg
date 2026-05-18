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
//
// Updated to use modern Windows Forms controls:
//   MainMenu  -> MenuStrip
//   MenuItem  -> ToolStripMenuItem
//   ContextMenu -> ContextMenuStrip
//   ToolBar   -> ToolStrip
//   ToolBarButton -> ToolStripButton
#pragma options("az", on)
USING SWF := System.Windows.Forms
USING System.Windows.Forms
USING System.Collections.Generic
USING System.Linq
USING VOSDK := XSharp.VO.SDK

/// <summary>
/// Replacement for System.Windows.Forms.MainMenu.
/// Inherits MenuStrip and exposes backward-compatible helpers.
/// </summary>
CLASS VOMenu INHERIT SWF.MenuStrip

    CONSTRUCTOR() STRICT
        SUPER()
        SELF:Dock := SWF.DockStyle.Top

    METHOD GetItemByID(nItemID AS LONG) AS VOMenuItem
        RETURN VOMenu.GetMenuItem(SELF:Items, nItemID)

    STATIC METHOD GetMenuItem(oItems AS SWF.ToolStripItemCollection, nItemID AS LONG) AS VOMenuItem
        FOREACH oItem AS VOMenuItem IN oItems:OfType<VOMenuItem>()
            IF oItem:MenuItemID == nItemID
                RETURN oItem
            ENDIF
            IF oItem:HasDropDownItems
                LOCAL oResult AS VOMenuItem
                oResult := GetMenuItem(oItem:DropDownItems, nItemID)
                IF oResult != NULL_OBJECT
                    RETURN oResult
                ENDIF
            ENDIF
        NEXT
        RETURN NULL_OBJECT

    ACCESS MenuItemArray AS VOMenuItem[]
        RETURN SELF:Items:OfType<VOMenuItem>():ToArray()

    METHOD AsContextMenuStrip() AS SWF.ContextMenuStrip STRICT
        LOCAL oContextMenu AS SWF.ContextMenuStrip
        oContextMenu := SWF.ContextMenuStrip{}
        SELF:__FillContextMenuStrip(oContextMenu:Items, SELF:Items)
        RETURN oContextMenu

    // Backward-compatible alias for AsContextMenuStrip
    METHOD AsContextMenu() AS SWF.ContextMenuStrip STRICT
        RETURN SELF:AsContextMenuStrip()

    PRIVATE METHOD __FillContextMenuStrip(oTarget AS SWF.ToolStripItemCollection, oSource AS SWF.ToolStripItemCollection) AS VOID
        FOREACH oItem AS VOMenuItem IN oSource:OfType<VOMenuItem>()
            LOCAL oClone AS VOMenuItem
            oClone := oItem:CloneMenu()
            oTarget:Add(oClone)
        NEXT


END CLASS

/// <summary>
/// Replacement for System.Windows.Forms.MenuItem.
/// Inherits ToolStripMenuItem and exposes backward-compatible helpers.
/// </summary>
CLASS VOMenuItem INHERIT SWF.ToolStripMenuItem

    INTERNAL MenuItemID         AS LONG
    // Store handler delegates so they can be copied when cloning
    INTERNAL OnClickHandler     AS System.EventHandler
    INTERNAL OnSelectHandler    AS System.EventHandler
    INTERNAL OnPopupHandler     AS System.EventHandler

    CONSTRUCTOR() STRICT
        SUPER()
        SELF:DisplayStyle := SWF.ToolStripItemDisplayStyle.Text

    CONSTRUCTOR(cText AS STRING) STRICT
        SUPER(cText)
        SELF:DisplayStyle := SWF.ToolStripItemDisplayStyle.Text

    /// <summary>Sets the keyboard shortcut using a Keys enum value.</summary>
    METHOD SetShortCut(nValue AS LONG) AS VOID
        SELF:ShortcutKeys := (SWF.Keys) nValue

    /// <summary>Creates a deep clone of this menu item including all children.</summary>
    METHOD CloneMenu() AS VOMenuItem STRICT
        LOCAL item AS VOMenuItem
        item := VOMenuItem{}
        item:Text            := SELF:Text
        item:Enabled         := SELF:Enabled
        item:Checked         := SELF:Checked
        item:MenuItemID      := SELF:MenuItemID
        item:ShortcutKeys    := SELF:ShortcutKeys
        item:ShowShortcutKeys := SELF:ShowShortcutKeys
        // Copy stored handler delegates
        IF SELF:OnClickHandler != NULL_OBJECT
            item:OnClickHandler := SELF:OnClickHandler
            item:Click          += item:OnClickHandler
        ENDIF
        IF SELF:OnSelectHandler != NULL_OBJECT
            item:OnSelectHandler := SELF:OnSelectHandler
            item:MouseEnter      += item:OnSelectHandler
        ENDIF
        IF SELF:OnPopupHandler != NULL_OBJECT
            item:OnPopupHandler  := SELF:OnPopupHandler
            item:DropDownOpened  += item:OnPopupHandler
        ENDIF
        // Recursively clone children
        FOREACH oChild AS VOMenuItem IN SELF:DropDownItems:OfType<VOMenuItem>()
            item:DropDownItems:Add(oChild:CloneMenu())
        NEXT
        RETURN item

    /// <summary>Removes this item from its parent collection.</summary>
    METHOD RemoveFromParent() AS VOID
        IF SELF:OwnerItem IS VOMenuItem VAR oParent
            oParent:DropDownItems:Remove(SELF)
        ELSEIF SELF:Owner IS VOMenu VAR oMenu
            oMenu:Items:Remove(SELF)
        ENDIF

    ACCESS MenuItemArray AS VOMenuItem[]
        RETURN SELF:DropDownItems:OfType<VOMenuItem>():ToArray()

    // Backward compat: was MenuItem.IsParent
    PROPERTY IsParent AS LOGIC GET SELF:HasDropDownItems
    OVERRIDE PROPERTY Text AS STRING
        GET
            RETURN SUPER:Text
        END GET
        SET
            // Strip Shortkey after tab character if present (e.g. "E&xit\tCtrl+X" -> "E&xit")
            var capt := value
            SELF:ShortcutKeyDisplayString  := ""
            if capt:Contains(e"\t")
                var parts := capt:Split(c'\t')
                capt := parts[0]
                SELF:ShortcutKeyDisplayString  := parts[1]
            endif
            SUPER:Text := capt
        END SET
    END PROPERTY
END CLASS

CLASS VOSeparator INHERIT SWF.ToolStripSeparator
    CONSTRUCTOR() STRICT
        SUPER()
END CLASS

/// <summary>
/// Replacement for System.Windows.Forms.ToolBar.
/// Inherits ToolStrip and exposes backward-compatible helpers.
/// </summary>
CLASS VOToolBar INHERIT SWF.ToolStrip IMPLEMENTS IVOControlProperties
    #include "PropControlStyle.xh"

    CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
        oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
        SUPER()
        SELF:ImageScalingSize := System.Drawing.Size{20, 20}
        SELF:GripStyle        := SWF.ToolStripGripStyle.Hidden

    // Backward compat: maps ButtonSize to ImageScalingSize
    PROPERTY ButtonSize AS System.Drawing.Size
        GET
            RETURN SELF:ImageScalingSize
        END GET
        SET
            SELF:ImageScalingSize := value
        END SET
    END PROPERTY

    METHOD GetButton(nID AS LONG, lMenuId AS LOGIC) AS VOToolBarButton
        IF lMenuId
            FOREACH oButton AS VOToolBarButton IN SELF:Items:OfType<VOToolBarButton>()
                IF oButton:MenuID == nID
                    RETURN oButton
                ENDIF
            NEXT
        ELSE
            IF nID <= SELF:Items:Count
                IF SELF:Items[nID - 1] IS VOToolBarButton VAR oBtn
                    RETURN oBtn
                ENDIF
            ENDIF
        ENDIF
        RETURN NULL_OBJECT

    PROTECTED VIRTUAL METHOD OnMouseMove(e AS SWF.MouseEventArgs) AS VOID
        SUPER:OnMouseMove(e)
        IF SELF:Bounds:Contains(Point{e:X, e:Y})
            SELF:ShowStatusMessage(e:X, e:Y)
        ENDIF
        RETURN

    PRIVATE METHOD ShowStatusMessage(X AS LONG, Y AS LONG) AS VOID
        LOCAL selectedButton := NULL_OBJECT AS VOToolBarButton
        FOREACH button AS VOToolBarButton IN SELF:Items:OfType<VOToolBarButton>()
            LOCAL rect := button:Bounds AS System.Drawing.Rectangle
            IF rect:Left <= X .AND. rect:Right >= X
                IF rect:Top <= Y .AND. rect:Bottom >= Y
                    selectedButton := button
                    EXIT
                ENDIF
            ENDIF
        NEXT
        IF selectedButton != NULL_OBJECT
            LOCAL nMenuId := 0 AS LONG
            LOCAL oWindow AS Window
            LOCAL oMenu   AS Menu
            oWindow := SELF:Control:Owner
            oMenu   := oWindow:Menu
            nMenuId := selectedButton:MenuID
            oWindow:MenuSelect(MenuSelectEvent{oMenu, oWindow, nMenuId})
        ENDIF

    METHOD PressButton(nID AS LONG, lPressed AS LOGIC) AS LOGIC
        LOCAL oButton AS VOToolBarButton
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

    PROTECTED VIRTUAL METHOD OnHandleCreated(e AS EventArgs) AS VOID
        SUPER:OnHandleCreated(e)
        RETURN

END CLASS

/// <summary>
/// Replacement for System.Windows.Forms.ToolBarButton.
/// Inherits ToolStripButton and exposes backward-compatible helpers.
/// </summary>
CLASS VOToolBarButton INHERIT SWF.ToolStripButton
    INTERNAL MenuID := 0 AS LONG

    CONSTRUCTOR() STRICT
        SUPER()
        RETURN

    // Backward compat: maps to Checked (pressed/depressed state)
    PROPERTY Pushed AS LOGIC
        GET
            RETURN SELF:Checked
        END GET
        SET
            SELF:Checked := value
        END SET
    END PROPERTY

    // Backward compat: PartialPush has no direct equivalent in ToolStrip; ignored
    PROPERTY PartialPush AS LOGIC AUTO

    // Backward compat: Rectangle was used in old ToolBarButton; Bounds is the equivalent
    ACCESS Rectangle AS System.Drawing.Rectangle
        RETURN SELF:Bounds

END CLASS


CLASS VOToolTip INHERIT SWF.ToolTip

    CONSTRUCTOR()
        SUPER()
        SELF:StripAmpersands := TRUE
        SELF:BackColor := System.Drawing.Color.LightYellow
        SELF:OwnerDraw := TRUE
        SELF:Draw += OnMyDraw

    PRIVATE METHOD OnMyDraw(sender AS OBJECT, e AS SWF.DrawToolTipEventArgs) AS VOID
        e:DrawBackground()
        e:DrawBorder()
        e:DrawText()

    METHOD Stop() AS VOID STRICT
        SELF:Active := FALSE
        SELF:StopTimer()
        SELF:RemoveAll()

END CLASS
