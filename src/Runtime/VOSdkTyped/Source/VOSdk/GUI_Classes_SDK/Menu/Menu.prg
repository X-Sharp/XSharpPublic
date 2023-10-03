//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



USING System.Reflection
USING VOSDK := XSharp.VO.SDK
/// <include file="Gui.xml" path="doc/Menu/*" />
CLASS Menu INHERIT VObject
    PROTECT oWindow			as Window
    PROTECT oParent 		AS Menu
    PROTECT oMenu 			AS VOMenu
    PROTECT aChildren		AS ARRAY
    PROTECT iAutoPosition	AS INT
    PROTECT oAccelerator	AS Accelerator
    PROTECT oToolBar 		AS ToolBar
    PROTECT aItem 			AS ARRAY

    /// <exclude />
    PROPERTY __Menu as VOMenu GET oMenu

    /// <exclude />
    ASSIGN __Owner(oWin as Window)
        oWindow := oWIn

    /// <exclude />
    METHOD __GetParent() AS Menu
        LOCAL oMenu as Menu
        oMenu := SELF

        DO WHILE oMenu:oParent != NULL_OBJECT
            oMenu := oMenu:oParent
        ENDDO
        RETURN oMenu

    METHOD __CreateMenuItem(cCaption AS STRING, nId AS LONG) AS VOMenuItem
        LOCAL oItem AS VOMenuItem
        oItem := VOMenuItem{}
        IF STRING.IsNullOrEmpty(cCaption)
            oItem:Text := "-"
        ELSE
            oItem:Text	:= cCaption
        ENDIF
        oItem:ShowShortcut := FALSE
        oItem:MenuItemID   := nId
        oItem:Click  += OnItemClick
        oItem:Select += OnItemSelect
        oItem:Popup  += OnItemPopup
        RETURN oItem

    /// <include file="Gui.xml" path="doc/Menu.Accelerator/*" />
    PROPERTY Accelerator AS Accelerator GET oAccelerator SET oAccelerator := Value

    /// <include file="Gui.xml" path="doc/Menu.Items/*" />
    PROPERTY Items AS ARRAY GET aItem

    /// <include file="Gui.xml" path="doc/Menu.AddChild/*" />
    METHOD AddChild(oMenu as Menu) AS VOID
        aadd(aChildren, oMenu)
        RETURN

    /// <include file="Gui.xml" path="doc/Menu.AppendItem/*" />
    METHOD AppendItem(nItemID , xNewItem )
        LOCAL cNewItem AS STRING
        LOCAL oSubMenu	AS Menu
        LOCAL oItem	    AS VOMenuItem
        LOCAL oHl		AS HyperLabel
        LOCAL aItems	AS VOMenuItem[]
        if nItemID is Menu var oMenu
            oSubMenu := oMenu
            oSubMenu:SetParent(SELF)
            SELF:AddChild(nItemID)
            cNewItem := xNewItem
            oItem := SELF:__CreateMenuItem(cNewItem, oSubMenu:GetHashCode())
            SELF:oMenu:MenuItems:Add(oItem)

            aItems := VOMenuItem[]{oSubMenu:__Menu:MenuItems:Count}
            oSubMenu:__Menu:MenuItems:CopyTo(aItems, 0)
            oItem:MenuItems:AddRange(aItems)

        ELSEIF IsNumeric(nItemID)
            IF nItemID == MENUSEPARATOR
                oItem := SELF:__CreateMenuItem("-", nItemID)
                SELF:oMenu:MenuItems:Add(oItem)
            ELSE
                if xNewItem is HyperLabel
                    oHl := xNewItem
                    cNewItem := oHl:Caption
                    oItem := SELF:__CreateMenuItem(cNewItem, nItemID)
                    SELF:oMenu:MenuItems:Add(oItem)
                    AAdd(aItem, {nItemID, oHl})
                ELSEIF IsString(xNewItem)
                    oItem := SELF:__CreateMenuItem(cNewItem, nItemID)
                    SELF:oMenu:MenuItems:Add(oItem)
                elseif xNewItem is Bitmap
                    // todo Menu Bitmaps
                    //lRetVal := AppendMenu(hMenu, _OR(MF_BYCOMMAND, MF_BITMAP, MF_ENABLED), nItemID, xNewItem:Handle())
                    NOP
                ENDIF
            ENDIF
        ENDIF

        RETURN oItem != NULL_OBJECT

    /// <include file="Gui.xml" path="doc/Menu.CheckItem/*" />
    METHOD CheckItem(nItemID )
        IF IsSymbol(nItemID)
            nItemID := SELF:GetMenuID((SYMBOL) nItemID)
        ENDIF
        LOCAL IMPLIED oItem := oMenu:GetItemByID(nItemID)
        IF oItem != NULL_OBJECT
            TRY
                oItem:Checked := TRUE
            CATCH
                // Do nothing
                NOP
            END TRY
        ENDIF

        RETURN oItem != NULL_OBJECT

    /// <include file="Gui.xml" path="doc/Menu.DeleteChild/*" />
    METHOD DeleteChild(oMenu as Menu)  AS LOGIC
        LOCAL dwI, dwCount AS DWORD

        dwCount := ALen(aChildren)
        FOR dwI := 1 UPTO dwCount
            IF aChildren[dwI] = oMenu
                ATrueDel(aChildren, dwI)
                RETURN TRUE
            ENDIF
        NEXT  // dwI
        RETURN FALSE

    /// <include file="Gui.xml" path="doc/Menu.DeleteItem/*" />
    METHOD DeleteItem(xItemIdOrMenu AS USUAL) AS LOGIC
        LOCAL retVal AS LOGIC
        LOCAL nItemID AS LONG
        retVal := FALSE
        if xItemIdOrMenu is Menu var oSubMenu
            nItemID := oSubMenu:GetHashCode()
            local implied oItem := self:oMenu:GetItemByID(nItemID)
            IF oItem != NULL_OBJECT
                SELF:DeleteChild(xItemIdOrMenu)
                IF oItem != NULL_OBJECT
                    oItem:Parent:MenuItems:Remove(oItem)
                    retVal := TRUE
                ENDIF
            ENDIF
        elseif IsNumeric(xItemIdOrMenu)
            nItemID := xItemIdOrMenu
            local implied oItem := self:oMenu:GetItemByID(nItemID)
            IF oItem != NULL_OBJECT
                oItem:Parent:MenuItems:Remove(oItem)
                retVal := TRUE
            ENDIF
            IF (oToolBar != NULL_OBJECT)
                oToolBar:DeleteItem(xItemIdOrMenu)
            ENDIF
        ENDIF
        RETURN retVal

    /// <include file="Gui.xml" path="doc/Menu.Destroy/*" />

    METHOD Destroy() AS USUAL CLIPPER
        oMenu			:= NULL_OBJECT
        aItem			:= NULL_ARRAY
        aChildren		:= NULL_ARRAY
        oWindow			:= NULL_OBJECT
        oAccelerator	:= NULL_OBJECT
        SUPER:Destroy()

        RETURN NIL

    /// <include file="Gui.xml" path="doc/Menu.DisableItem/*" />
    METHOD DisableItem(nItemID )
        IF IsSymbol(nItemID)
            nItemID := SELF:GetMenuID((SYMBOL) nItemID)
        ENDIF
        IF (oToolBar != NULL_OBJECT)
            oToolBar:DisableItem(nItemID)
        ENDIF
        LOCAL IMPLIED oItem := oMenu:GetItemByID(nItemID)
        IF oItem != NULL_OBJECT
            oItem:Enabled:= FALSE
        ENDIF

        RETURN oItem != NULL_OBJECT

    /// <include file="Gui.xml" path="doc/Menu.DisableAutoUpdate/*" />
    METHOD DisableAutoUpdate() AS VOID CLIPPER
        SELF:SetAutoUpdate(999)

        RETURN

    /// <include file="Gui.xml" path="doc/Menu.EnableItem/*" />
    METHOD EnableItem(nItemID)
        IF IsSymbol(nItemID)
            nItemID := SELF:GetMenuID((SYMBOL) nItemID)
        ENDIF
        IF (oToolBar != NULL_OBJECT)
            oToolBar:EnableItem(nItemID)
        ENDIF
        LOCAL IMPLIED oItem := oMenu:GetItemByID(nItemID)
        IF oItem != NULL_OBJECT
            oItem:Enabled:= TRUE
        ENDIF
        RETURN oItem != NULL_OBJECT

    /// <include file="Gui.xml" path="doc/Menu.GetAutoUpdate/*" />
    METHOD GetAutoUpdate() AS LONG CLIPPER
        RETURN iAutoPosition


    /// <include file="Gui.xml" path="doc/Menu.GetSubMenu/*" />
    METHOD GetSubMenu(nIndex AS DWORD)  AS Menu
        LOCAL oResult AS Menu
        LOCAL oSubMenu AS System.Windows.Forms.Menu
        oSubMenu := SELF:__Menu:MenuItems[(LONG) nIndex]:CloneMenu()
        oResult := Menu{oSubMenu}
        FOREACH aItem AS ARRAY IN SELF:aItem
            oResult:RegisterItem(aItem[1], aItem[2])		// ID, HyperLabel
        NEXT
        RETURN oResult


    /// <include file="Gui.xml" path="doc/Menu.Handle/*" />
    METHOD Handle() AS VOMenu STRICT
        RETURN oMenu

    /// <include file="Gui.xml" path="doc/Menu.HyperLabel/*" />
    METHOD HyperLabel(nItemID)
        LOCAL dwIndex AS DWORD
        LOCAL dwCount AS DWORD
        LOCAL oHyperLabel AS HyperLabel
        LOCAL oChildMenu AS VOSDK.Menu
        IF IsSymbol(nItemID)
            nItemID := SELF:GetMenuID((SYMBOL) nItemID)
        ENDIF

        dwCount := ALen(aItem)
        FOR dwIndex := 1 UPTO dwCount
            IF nItemID == aItem[dwIndex][ 1]
                RETURN aItem[dwIndex][2]
            ENDIF
        NEXT  // dwI

        dwCount := ALen(aChildren)
        FOR dwIndex := 1 UPTO ALen(aChildren)
            oChildMenu := aChildren[dwIndex]
            oHyperLabel := oChildMenu:HyperLabel(nItemID)
            IF oHyperLabel != NULL_OBJECT
                RETURN oHyperLabel
            ENDIF
        NEXT  // dwIndex

        RETURN NULL_OBJECT

    /// <include file="Gui.xml" path="doc/Menu.ctor/*" />
    CONSTRUCTOR(xResourceID)
        LOCAL oResMenu AS ResourceMenu
        LOCAL oResourceID AS ResourceID
        SUPER()
        aChildren := {}
        aItem := {}
        if xResourceID is VoMenu
            oMenu := xResourceID
        elseif xResourceID is VoMenuItem
            LOCAL oItem AS VOMenuItem
            oItem := xResourceID
            oMenu := VOMenu{}
            FOREACH IMPLIED oSubItem IN oItem:MenuItemArray
                oMenu:MenuItems:Add(oSubItem:CloneMenu())
            NEXT

        ELSE
            oMenu := VOMenu{}
            IF IsNil(xResourceID)
                // Do nothing
            NOP

            ELSE
                IF IsNumeric(xResourceID) .OR. IsPtr(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
                    oResourceID := ResourceID{xResourceID}
                elseif !(xResourceID is ResourceID)
                    WCError{#Init, #Menu, __WCSTypeError}:Throw()
                ELSE
                    oResourceID := xResourceID
                ENDIF
                oResMenu := ResourceMenu{oResourceID:Handle(), oResourceID:Name}
                oResMenu:AddItemsTo(SELF)
            ENDIF
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/Menu.InsertItem/*" />
    METHOD InsertItem(nItemID AS USUAL, xNewItem AS USUAL, nBeforeID AS INT) AS LOGIC
        LOCAL retVal AS LOGIC
        LOCAL cNewItem AS STRING
        LOCAL oSubMenu AS Menu
        LOCAL oItem	    as VOMenuItem
        LOCAL aItems	as VOMenuItem[]
        LOCAL oHl		as HyperLabel

        if nItemID is Menu
            oSubMenu := nItemID
            oSubMenu:SetParent(SELF)
            SELF:AddChild(oSubMenu)
            cNewItem := xNewItem
            oItem := SELF:__CreateMenuItem(cNewItem, oSubMenu:GetHashCode())
            oItem := VOMenuItem{cNewItem}
            SELF:oMenu:MenuItems:Add(nBeforeID, oItem)
            aItems := VOMenuItem[]{oSubMenu:__Menu:MenuItems:Count}
            oSubMenu:__Menu:MenuItems:CopyTo(aItems, 0)
            oItem:MenuItems:AddRange(aItems)

        ELSEIF IsNumeric(nItemID)
            IF (nItemID == MENUSEPARATOR)
                oItem := SELF:__CreateMenuItem("-", nItemID)
                SELF:oMenu:MenuItems:Add(nBeforeID, oItem )

            ELSE
                if xNewItem is HyperLabel
                    oHl := xNewItem
                    cNewItem := oHl:Caption
                    oItem := SELF:__CreateMenuItem(cNewItem, nItemID)
                    SELF:oMenu:MenuItems:Add(nBeforeID, oItem)
                ELSEIF IsString(xNewItem)
                    cNewItem := xNewItem
                    oItem := SELF:__CreateMenuItem(cNewItem, nItemID)
                    SELF:oMenu:MenuItems:Add(nBeforeID, oItem)
                elseif xNewItem is Bitmap
                    //retVal := InsertMenu(hMenu, nBeforeID, _OR(_OR(MF_BYCOMMAND, MF_BITMAP), MF_ENABLED), nItemID, PSZ(_CAST, xNewItem:Handle()))
                    NOP
                ENDIF
            ENDIF
        ENDIF
        RETURN retVal


    /// <include file="Gui.xml" path="doc/Menu.MakeMenuRtol/*" />
    METHOD MakeMenuRtol(lRToL AS LOGIC)
        //Todo MakeMenuRtol
        RETURN SELF

    /// <include file="Gui.xml" path="doc/Menu.Name/*" />
    METHOD Name(nItemID AS LONG)  AS STRING
        LOCAL oHL AS HyperLabel

        oHL := SELF:HyperLabel(nItemID)
        IF (oHL != NULL_OBJECT)
            RETURN oHL:Name
        ENDIF

        RETURN NULL_STRING

    /// <include file="Gui.xml" path="doc/Menu.NameSym/*" />
    METHOD NameSym(nItemID AS LONG) AS SYMBOL
        LOCAL oHL AS HyperLabel

        oHL:=SELF:HyperLabel(nItemID)

        IF (oHL != NULL_OBJECT)
            RETURN oHL:NameSym
        ENDIF

        RETURN NULL_SYMBOL

    /// <include file="Gui.xml" path="doc/Menu.PostInit/*" />
    METHOD PostInit()
        RETURN NIL

    /// <include file="Gui.xml" path="doc/Menu.PreInit/*" />
    METHOD PreInit()
        RETURN NIL

    /// <include file="Gui.xml" path="doc/Menu.RegisterItem/*" />
    method RegisterItem(nItemID as usual, oHyperLabel := null as HyperLabel,  hParentMenu := null as VOMenu , nPosition := -1 as long) as logic
        // hParentMenu and nPosition are now ignored
        LOCAL lResult AS LOGIC
        LOCAL oItem	AS VOMenuItem
        IF IsSymbol(nItemID)
            nItemID := SELF:GetMenuID((SYMBOL) nItemID)
        ENDIF
        IF IsLong(nItemID)
            oItem := oMenu:GetItemByID(nItemID)
            IF oItem != NULL_OBJECT
                oItem:Text := oHyperLabel:Caption
                // Poor Man's ShortCuts
                //if oItem:Text:Contains(e"\t")
                //	LOCAL cShortCut as STRING
                //	cShortCut := oItem:Text:Substring(oItem:Text:IndexOf(e"\t")+1)
                //	try
                //		oItem:Shortcut := (System.Windows.Forms.Shortcut) Enum.Parse(typeof(System.Windows.Forms.Shortcut), cShortCut:Replace("+",""))
                //		oItem:Text := oItem:Text:Substring(0, oItem:Text:IndexOf(e"\t"))
                //		oItem:ShowShortcut := TRUE
                //	catch e as Exception

                //	end try
                //ENDIF
            ENDIF

        ENDIF
        AAdd(aItem, {nItemID, oHyperLabel})
        lResult := TRUE
        RETURN lResult

    /// <include file="Gui.xml" path="doc/Menu.SetAutoUpdate/*" />
    METHOD SetAutoUpdate(nMenuNumber AS LONG) AS VOID
        iAutoPosition := nMenuNumber
        RETURN

    /// <include file="Gui.xml" path="doc/Menu.SetParent/*" />
    METHOD SetParent(oMenu AS Menu) AS VOID
        oParent := oMenu
        RETURN


    /// <include file="Gui.xml" path="doc/Menu.SetShortCuts/*" />
    METHOD SetShortCuts(oAccelerator AS Accelerator) AS VOID
        SELF:__ClearShortCuts(SELF:__Menu)
        IF oAccelerator != NULL_OBJECT
            FOREACH IMPLIED oKey IN oAccelerator:Keys
                LOCAL IMPLIED oItem := SELF:__Menu:GetItemByID(oKey:ID)
                if oItem != null_object
                    // Some shortcuts require special handling
                    oItem:SetShortCut(oKey:Shortcut)
                ENDIF
            NEXT
        ENDIF


    METHOD __ClearShortCuts(oMenu as System.Windows.Forms.Menu) as VOID
        FOREACH oItem AS System.Windows.Forms.MenuItem in oMenu:MenuItems
            oItem:Shortcut := System.Windows.Forms.Shortcut.None
            SELF:__ClearShortCuts(oItem)
        NEXT
        RETURN

    /// <include file="Gui.xml" path="doc/Menu.ShowAsPopup/*" />
    METHOD ShowAsPopup(oOwner, oPoint, kButton, kAlignment, oNotOverlap)
        //Todo ShowAsPopup
        /*
        LOCAL hPopUpMenu  AS PTR
        LOCAL strucPoint  IS _winPoint
        LOCAL strucTPM    IS _winTPMParams
        LOCAL pTPM        AS _winTPMParams
        LOCAL sRect       IS _winRect
        LOCAL sRectItem   IS _winRect
        LOCAL liItem      AS LONGINT
        LOCAL hWnd        AS PTR
        LOCAL lRet := FALSE AS LOGIC
        //PP-030319 new parameter oNotOverlap, courtesy S. Ebert
        //PP-041001 Update from S. Ebert

        IF !IsInstanceOfUsual(oOwner, #Window) .AND. !IsInstanceOfUsual(oOwner, #Control)
        WCError{#ShowPopup, #Menu, __WCSTypeError, oOwner, 1}:Throw()
        ENDIF

        DEFAULT( REF kButton, PM_RIGHTBUTTON)
        DEFAULT( REF kAlignment, PM_ALIGNLEFT)

        IF IsInstanceOfUsual(oPoint, #Point)
        strucPoint:x := oPoint:X
        IF strucPoint:x > GetSystemMetrics(SM_CXScreen)
        strucPoint:x := GetSystemMetrics(SM_CXScreen)
        ELSEIF strucPoint:x < 0
        strucPoint:x := 0
        ENDIF
        strucPoint:y := oPoint:Y
        IF WCGetCoordinateSystem()
        strucPoint:y := GetSystemMetrics(SM_CYSCREEN) - strucPoint:y
        ENDIF
        IF strucPoint:y > GetSystemMetrics(SM_CYScreen)
        strucPoint:y := GetSystemMetrics(SM_CYScreen)
        ELSEIF strucPoint:y < 0
        strucPoint:y := 0
        ENDIF
        ELSEIF IsLong(oPoint)
        strucPoint:x := SHORTINT(_CAST, LoWord(DWORD(_CAST, oPoint)))
        strucPoint:y := SHORTINT(_CAST, HiWord(DWORD(_CAST, oPoint)))
        ELSE
        GetCursorPos(@strucPoint)
        ENDIF

        IF strucPoint:x = -1 .AND. strucPoint:y = -1 //Keyboard call
        hWnd := oOwner:Handle()
        GetCursorPos(@strucPoint)
        GetWindowRect(hWnd, @sRect)
        DO CASE
        CASE IsInstanceOfUsual(oOwner, #ListView)
        liItem := ListView_GetNextItem(hWnd, -1, LV_GNIBYITEM+LVNI_FOCUSED)
        IF liItem >= 0 .AND. liItem >= ListView_GetTopIndex(hWnd)
        sRectItem:left := LVIR_LABEL
        IF LOGIC(_CAST, SendMessage(hWnd, LVM_GETITEMRECT, DWORD(liItem), LONGINT(_CAST,@sRectItem)))
        strucPoint:x := sRectItem:Left
        strucPoint:y := sRectItem:Bottom
        ScreenToClient(hWnd, @strucPoint)
        ENDIF
        ENDIF
        CASE IsInstanceOfUsual(oOwner, #TreeView)
        liItem := LONGINT(_CAST, TreeView_GetNextItem(hWnd, -1, TVGN_CARET))
        IF liItem >= 0
        sRectItem:left := liItem
        IF LOGIC(_CAST, SendMessage(hWnd, TVM_GETITEMRECT, 1, LONGINT(_CAST,@sRectItem)))
        strucPoint:x := sRectItem:Left
        strucPoint:y := sRectItem:Bottom
        ScreenToClient(hWnd, @strucPoint)
        ENDIF
        ENDIF
        ENDCASE
        IF ! PtInRect(@sRect, strucPoint)
        strucPoint:x := (sRect:left + sRect:right)  / 2
        strucPoint:y := (sRect:top  + sRect:bottom) / 2
        ENDIF
        ENDIF

        IF IsInstanceOfUsual(oNotOverlap, #Control) .OR. IsInstanceOfUsual(oNotOverlap, #Window)
        strucTPM:cbSize := _SIZEOF(_winTPMParams)
        GetWindowRect(oNotOverlap:Handle(), @strucTPM:rcExclude)
        pTPM := @strucTPM
        ELSEIF IsInstanceOfUsual(oNotOverlap, #BoundingBox)
        strucTPM:cbSize := _SIZEOF(_winTPMParams)
        SetRect(@strucTPM:rcExclude,oNotOverlap:Left,oNotOverlap:Top,oNotOverlap:Right,oNotOverlap:Bottom)
        IF WCGetCoordinateSystem()
        strucTPM:rcExclude:Top    := GetSystemMetrics(SM_CYSCREEN) - strucTPM:rcExclude:Top
        strucTPM:rcExclude:Bottom := GetSystemMetrics(SM_CYSCREEN) - strucTPM:rcExclude:Bottom
        ENDIF
        pTPM := @strucTPM
        ELSEIF IsPtr(oNotOverlap)
        strucTPM:cbSize := _SIZEOF(_winTPMParams)
        CopyRect(@strucTPM:rcExclude, oNotOverlap)
        pTPM := @strucTPM
        ENDIF

        hPopUpMenu := GetSubMenu(SELF:Handle(), 0)
        IF hPopUpMenu != NULL_PTR
        lRet := TrackPopupMenuEx(hPopUpMenu, _OR(DWORD(kButton), DWORD(kAlignment)),;
        strucPoint:x, strucPoint:y, oOwner:Handle(), pTPM)
        ENDIF

        RETURN lRet
        */
        RETURN FALSE

    /// <include file="Gui.xml" path="doc/Menu.ToolBar/*" />
    PROPERTY ToolBar as ToolBar GET oToolBar SET oToolBar := Value

    /// <include file="Gui.xml" path="doc/Menu.UncheckItem/*" />
    METHOD UncheckItem(nItemID)
        IF IsSymbol(nItemID)
            nItemID := SELF:GetMenuID((SYMBOL) nItemID)
        ENDIF
        LOCAL IMPLIED oItem := oMenu:GetItemByID(nItemID)
        IF oItem != NULL_OBJECT
            oItem:Checked := FALSE
        ENDIF
        RETURN oItem != NULL_OBJECT

    /// <include file="Gui.xml" path="doc/Menu.UnregisterItem/*" />
    METHOD UnregisterItem(nItemID )
        LOCAL dwIndex AS DWORD
        LOCAL dwCount AS DWORD
        IF IsSymbol(nItemID)
            nItemID := SELF:GetMenuID((SYMBOL) nItemID)
        ENDIF
        dwCount := ALen(aItem)
        FOR dwIndex := 1 UPTO dwCount
            IF nItemID == aItem[dwIndex][ 1]
                ATrueDel(aItem,dwIndex)
                EXIT
            ENDIF
        NEXT  // dwIndex
        RETURN TRUE

#region Events
    /// <exclude />
    METHOD OnItemClick(Sender AS OBJECT, e AS EventArgs) AS VOID
        LOCAL oEvt AS MenuCommandEvent
        LOCAL oMenu AS Menu
        LOCAL oMenuItem AS VOMenuItem
        oMenu := SELF:__GetParent()

        IF oMenu:oWindow != NULL_OBJECT
            oMenuItem := (VOMenuItem) Sender
            oEvt := MenuCommandEvent{SELF,oMenu:oWindow, oMenuItem:MenuItemID}
            oMenu:oWindow:__PreMenuCommand(oEvt)
        ENDIF
        RETURN

    /// <exclude />
    METHOD OnItemSelect(Sender AS OBJECT, e AS EventArgs) AS VOID
        LOCAL oMenu AS Menu
        LOCAL oMenuItem AS VOMenuItem

        oMenu := SELF:__GetParent()

        IF oMenu:oWindow != NULL_OBJECT
            oMenuItem := (VOMenuItem) Sender
            oMenu:oWindow:MenuSelect(MenuSelectEvent{SELF,oMenu:oWindow, oMenuItem:MenuItemID})
        ENDIF
        RETURN

    /// <exclude />
    METHOD OnItemPopup(Sender as OBJECT, e as EventArgs) AS VOid
        LOCAL oMenu as Menu
        LOCAL oMenuItem as VOMenuItem
        oMenu := SELF:__GetParent()

        if oMenu:oWindow != NULL_OBJECT
            oMenuItem := (VOMenuItem) Sender
            oMenu:oWindow:MenuInit(MenuInitEvent{SELF,oMenu:oWindow, oMenuItem:MenuItemID})
        ENDIF
        RETURN

#endregion


#region Extensions

    /// <include file="Gui.xml" path="doc/Menu.GetMenuID/*" />
    METHOD GetMenuID(symItem AS SYMBOL) AS LONG
        LOCAL nItem, nCount AS DWORD
        LOCAL oHL			AS HyperLabel
        nCount := Alen(aItem)
        FOR nItem := 1 TO nCount
            oHL := aItem[nItem][2]
            IF oHL:NameSym == symItem
                RETURN aItem[nItem][1]
            ENDIF
        NEXT
        RETURN 0


    /// <include file="Gui.xml" path="doc/Menu.SetAble/*" />
    METHOD SetAble(nID , lEnable ) AS VOID
        DEFAULT(ref lEnable, true)
        IF IsSymbol (nID)
            nID := SELF:GetMenuID(nID)
        ENDIF
        IF IsNumeric(nID)
            IF lEnable
                SELF:EnableItem(nID)
            ELSE
                SELF:DisableItem(nID)
            ENDIF
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/Menu.SetCheck/*" />
    METHOD SetCheck( nID , lCheck) AS VOID
        DEFAULT(ref lCheck, true)
        IF IsSymbol (nID)
            nID := SELF:GetMenuID(nID)
        ENDIF
        IF IsNumeric(nID)
            IF lCheck
                SELF:CheckItem(nID)
            ELSE
                SELF:UnCheckItem(nID)
            ENDIF
        ENDIF
#endregion

END CLASS

//CLASS SystemMenu INHERIT Menu

//	METHOD Destroy()  AS USUAL CLIPPER

//		SUPER:Destroy()
//		RETURN SELF


//	CONSTRUCTOR(oOwner)
//		//Todo SystemMenu class
//		//SUPER(GetSystemMenu(((Window)oOwner):Handle(), FALSE))
//		SUPER()
//		RETURN
//END CLASS

FUNCTION GetMenuItemCount(oMenu AS VOMenu) AS LONG
    RETURN oMenu:MenuItems:Count

FUNCTION GetSubMenu(oMenu AS VOMenu, nItem AS LONG) AS VOMenuItem
    LOCAL nCurrent AS LONG
    nCurrent := 0
    FOREACH oItem AS VOMenuItem IN oMenu:MenuItems
        IF oItem:MenuItems:Count > 0
            // This is a SubMenu
            IF nCurrent == nItem
                RETURN oItem
            ENDIF
            ++nCurrent
        ENDIF
    NEXT
    RETURN NULL_OBJECT

