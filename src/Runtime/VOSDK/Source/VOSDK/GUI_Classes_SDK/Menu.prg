#pragma options ("enforceself", on)
/// <include file="Gui.xml" path="doc/Menu/*" />
CLASS Menu INHERIT VObject
	PROTECT oParent 	AS Menu
	PROTECT hMenu 		AS PTR
	PROTECT aChildren AS ARRAY
	PROTECT iAutoPosition AS INT
	PROTECT oAccelerator AS Accelerator
	PROTECT oToolBar 	AS ToolBar
	PROTECT aItem 		AS ARRAY


/// <include file="Gui.xml" path="doc/Menu.Accelerator/*" />
access Accelerator
	RETURN oAccelerator


/// <include file="Gui.xml" path="doc/Menu.Accelerator/*" />
assign Accelerator(oNewAccelerator)
	oAccelerator := oNewAccelerator


	return


/// <include file="Gui.xml" path="doc/Menu.Items/*" />
ACCESS Items
    RETURN aItem


/// <include file="Gui.xml" path="doc/Menu.AddChild/*" />
method AddChild(oMenu)
	AAdd(aChildren, oMenu)


	RETURN SELF


/// <include file="Gui.xml" path="doc/Menu.AppendItem/*" />
method AppendItem(nItemID, xNewItem)
	local lRetVal as logic
	LOCAL cNewItem AS STRING


	if nItemID is Menu var oItem
		oItem:SetParent(self)
		self:AddChild(oItem)
		cNewItem := xNewItem
		lRetVal := AppendMenu(hMenu, _or(MF_BYCOMMAND, MF_POPUP, MF_ENABLED), dword(_cast, oItem:Handle()), String2Psz(cNewItem))
	ELSEIF IsNumeric(nItemID)
		IF nItemID == MENUSEPARATOR
			lRetVal := AppendMenu(hMenu, _OR(MF_BYCOMMAND, MF_SEPARATOR), 0, NULL_PSZ)
		ELSE
			if xNewItem is HyperLabel var oHL
				cNewItem := oHL:Caption
				lRetVal := AppendMenu(hMenu, _OR(MF_BYCOMMAND, MF_STRING, MF_ENABLED), nItemID, String2Psz(cNewItem))
				AAdd(aItem, {nItemID, xNewItem})
			elseif xNewItem is string var strItem
				cNewItem := strItem
				lRetVal := AppendMenu(hMenu, _OR(MF_BYCOMMAND, MF_STRING, MF_ENABLED), nItemID, String2Psz(cNewItem))
			elseif xNewItem is Bitmap var oBmp
				lRetVal := AppendMenu(hMenu, _or(MF_BYCOMMAND, MF_BITMAP, MF_ENABLED), nItemID, oBmp:Handle())
			ENDIF
		ENDIF
	ENDIF


	RETURN lRetVal


/// <include file="Gui.xml" path="doc/Menu.CheckItem/*" />
method CheckItem(nItemID)




	RETURN CheckMenuItem(hMenu, nItemID, _OR(MF_CHECKED, MF_BYCOMMAND))


/// <include file="Gui.xml" path="doc/Menu.Children/*" />
ACCESS Children() AS ARRAY
	Return aChildren


/// <include file="Gui.xml" path="doc/Menu.DeleteChild/*" />
method DeleteChild(oMenu)
	//SE-060526
	LOCAL dwI, dwCount AS DWORD


	dwCount := ALen(aChildren)
	FOR dwI := 1 UPTO dwCount
	   IF aChildren[dwI] = oMenu
	   	ADel(aChildren, dwI)
		   ASize(aChildren, dwCount - 1)
		   EXIT
	   ENDIF
	NEXT  // dwI


	RETURN NIL


/// <include file="Gui.xml" path="doc/Menu.DeleteItem/*" />
method DeleteItem(xItemIdOrMenu)
	LOCAL retVal AS LOGIC
	LOCAL i AS DWORD
	LOCAL hTmp AS PTR
	LOCAL iItemCount AS DWORD


	IF IsInstanceOfUsual(xItemIdOrMenu, #Menu)
		iItemCount := DWORD(GetMenuItemCount(hMenu))
		IF iItemCount > 0
			FOR i := 0 TO iItemCount-1
				hTmp := GetSubMenu(hMenu, INT(i))
				IF hTmp == xItemIdOrMenu:Handle()
					xItemIdOrMenu:SetParent(NULL_OBJECT)
					SELF:DeleteChild(xItemIdOrMenu)
					retVal := RemoveMenu(hMenu, i, MF_BYPOSITION)
					EXIT
				ENDIF
			NEXT
		ENDIF
	ELSE
		retVal := RemoveMenu(hMenu, xItemIdOrMenu, MF_BYCOMMAND)
		IF (oToolBar != NULL_OBJECT)
			oToolBar:DeleteItem(xItemIdOrMenu)
		ENDIF
	ENDIF


	RETURN retVal
/// <include file="Gui.xml" path="doc/Menu.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	LOCAL wIndex AS DWORD
	LOCAL wLen AS DWORD
	LOCAL oSubMenu AS Menu


	IF hMenu != 0
		DestroyMenu(hMenu)
	ENDIF


	__WCUnregisterMenu(SELF)


	IF !InCollect()
		IF oParent!=NULL_OBJECT
			oParent:DeleteChild(SELF)
			oParent:=NULL_OBJECT
		ENDIF
		wLen := ALen(aChildren)
		FOR wIndex := 1 UPTO wLen
		    oSubMenu := aChildren[wIndex]
			oSubMenu:SetParent(NULL_OBJECT)
			oSubMenu:Destroy()
		NEXT  // wIndex
		aChildren := NULL_ARRAY
		aItem 	:= NULL_ARRAY
		hMenu 	:= NULL_PTR
		UnregisterAxit(SELF)
	ENDIF


	SUPER:Destroy()


	RETURN NIL


/// <include file="Gui.xml" path="doc/Menu.DisableItem/*" />
method DisableItem(nItemID)




	IF (oToolBar != NULL_OBJECT)
		oToolBar:DisableItem(nItemID)
	ENDIF
	RETURN EnableMenuItem(hMenu, nItemID, _OR(MF_DISABLED, MF_GRAYED, MF_BYCOMMAND))


/// <include file="Gui.xml" path="doc/Menu.DisableAutoUpdate/*" />
method DisableAutoUpdate()


	// DHer: 18/12/2008
	SELF:SetAutoUpdate(999)


RETURN NIL


/// <include file="Gui.xml" path="doc/Menu.EnableItem/*" />
method EnableItem(nItemID)




	IF (oToolBar != NULL_OBJECT)
		oToolBar:EnableItem(nItemID)
	ENDIF


	RETURN EnableMenuItem(hMenu, nItemID, _OR(MF_ENABLED , MF_BYCOMMAND))


/// <include file="Gui.xml" path="doc/Menu.GetAutoUpdate/*" />
method GetAutoUpdate()




	RETURN iAutoPosition


/// <include file="Gui.xml" path="doc/Menu.GetSubMenu/*" />
method GetSubMenu(nIndex)




	RETURN GetSubMenu(hMenu,nIndex)


/// <include file="Gui.xml" path="doc/Menu.Handle/*" />
METHOD Handle() AS PTR




	RETURN hMenu


/// <include file="Gui.xml" path="doc/Menu.HyperLabel/*" />
method HyperLabel(nItemID)
	//SE-060526
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL oHyperLabel AS HyperLabel
	LOCAL oChildMenu AS Menu






	dwCount := ALen(aItem)
	FOR dwIndex := 1 UPTO dwCount
	   IF nItemID == aItem[dwIndex, 1]
	   	RETURN aItem[dwIndex][2]
	   ENDIF
	NEXT  // dwI


	dwCount := ALen(aChildren)
	FOR dwIndex := 1 UPTO ALen(aChildren)
		oChildMenu := aChildren[dwIndex]
		oHyperlabel := oChildMenu:Hyperlabel(nItemID)
		IF oHyperlabel != NULL_OBJECT
			RETURN oHyperlabel
		ENDIF
	NEXT  // dwIndex


	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/Menu.ctor/*" />
constructor(xResourceID)
	LOCAL hInst AS PTR
	LOCAL lpszMenu AS PTR




	SUPER()


	IF IsNil(xResourceID)
		hMenu := CreateMenu()
	ELSEIF IsPtr(xResourceID) .AND. IsMenu(xResourceID) 		// == FALSE
		hMenu := xResourceID
	ELSE
		IF IsNumeric(xResourceID) .OR. IsPtr(xResourceID) .OR. IsSymbol(xResourceID) .OR. IsString(xResourceID)
			xResourceID := ResourceID{xResourceID}
		ELSEIF !IsInstanceOfUsual(xResourceID, #ResourceID)
			WCError{#Init, #Menu, __WCSTypeError}:Throw()
		ENDIF


		hInst := xResourceID:Handle()
		lpszMenu := xResourceID:Address()


		hMenu := LoadMenu(hInst, lpszMenu)
	ENDIF


	aChildren := {}
	aItem := {}


	__WCRegisterMenu(SELF, hMenu)




	return


/// <include file="Gui.xml" path="doc/Menu.InsertItem/*" />
method InsertItem(nItemID, xNewItem, nBeforeID)
	LOCAL retVal AS LOGIC
	LOCAL cNewItem AS STRING




	if nItemID is Menu var oItem
		oItem:SetParent(self)
		self:AddChild(oItem)
		cNewItem := xNewItem
		retVal := InsertMenu(hMenu, nBeforeID, _or(_or(MF_BYCOMMAND, MF_POPUP), MF_ENABLED), dword(_cast, oItem:Handle()), String2Psz(cNewItem))
	ELSEIF IsNumeric(nItemID)
		IF (nItemID == MENUSEPARATOR)
			retVal := InsertMenu(hMenu, nBeforeID, _OR(MF_BYCOMMAND, MF_SEPARATOR), 0, NULL_PSZ)
		ELSE
			if xNewItem is HyperLabel var oHL
				cNewItem := oHL:Caption
				retVal := InsertMenu(hMenu, nBeforeID, _OR(_OR(MF_BYCOMMAND, MF_STRING), MF_ENABLED), nItemID, String2Psz(cNewItem))
			elseif xNewItem is string var StrItem
				cNewItem := StrItem
				retVal := InsertMenu(hMenu, nBeforeID, _OR(_OR(MF_BYCOMMAND, MF_STRING), MF_ENABLED), nItemID, String2Psz(cNewItem))
			elseif xNewItem is Bitmap var oBMp
				retVal := InsertMenu(hMenu, nBeforeID, _or(_or(MF_BYCOMMAND, MF_BITMAP), MF_ENABLED), nItemID, psz(_cast, oBMp:Handle()))
			ENDIF
		ENDIF
	ENDIF


	RETURN retVal


/// <include file="Gui.xml" path="doc/Menu.MakeMenuRtol/*" />
method MakeMenuRtol(lRToL)
	LOCAL strcMII IS _winMENUITEMINFO
	LOCAL pszBuffer AS PSZ


	Default(@lRToL, TRUE)


	pszBuffer := MemAlloc(128)


	strcmii:cbSize := _SIZEOF(_winMENUITEMINFO)
	strcmii:fMask := MIIM_TYPE
	strcmii:dwTypeData := pszBuffer
	strcmii:cch := 128


	GetMenuItemInfo(hMenu, 0, TRUE, @strcMII)


	strcmii:fMask := MIIM_TYPE


	IF lRToL
		strcmii:fType := _OR(strcmii:fType, DWORD(_CAST, MFT_RIGHTJUSTIFY), DWORD(_CAST, MFT_RIGHTORDER))
	ELSE
		strcmii:fType := _AND(strcmii:fType, DWORD(_CAST, _NOT(_OR(MFT_RIGHTJUSTIFY , MFT_RIGHTORDER))))
	ENDIF


	// Right justify the specified item and all those following it
	SetMenuItemInfo(hMenu, 0, TRUE, @strcMII)
	MemFree(pszbuffer)


	RETURN SELF


/// <include file="Gui.xml" path="doc/Menu.MenuItems/*" />
ACCESS MenuItems AS ARRAY
	RETURN aItem




/// <include file="Gui.xml" path="doc/Menu.Name/*" />
method Name(nItemID)
	LOCAL oHL AS HyperLabel






	oHL := SELF:HyperLabel(nItemID)
	IF (oHL != NULL_OBJECT)
		RETURN oHL:Name
	ENDIF


	RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/Menu.NameSym/*" />
method NameSym(nItemID)
	LOCAL oHL AS HyperLabel






	oHL:=SELF:HyperLabel(nItemID)


	IF (oHL != NULL_OBJECT)
		RETURN oHL:NameSym
	ENDIF


	RETURN NULL_SYMBOL


/// <include file="Gui.xml" path="doc/Menu.PostInit/*" />
method PostInit()
     RETURN NIL


/// <include file="Gui.xml" path="doc/Menu.PreInit/*" />
method PreInit()
     RETURN NIL


/// <include file="Gui.xml" path="doc/Menu.RegisterItem/*" />
method RegisterItem(nItemID, oHyperLabel, hParentMenu, nPosition)
	LOCAL hMenu AS PTR
	LOCAL lResult AS LOGIC
	LOCAL cCaption AS STRING
	//PP-040110 return logic value from ModifyMenu






	IF IsLong(nItemID)
		// RvdH 070206 Changed to NOT use PCount() but check parameter types
		IF IsNumeric(nItemID) .AND. IsAccess(oHyperLabel,#Caption)
			cCaption := oHyperLabel:Caption
			IF IsPtr(hParentMenu) .AND. IsLong(nPosition)
				hMenu := GetSubMenu(hParentMenu, nPosition)
				__WCRegisterMenu(SELF, hMenu)
				lResult := ModifyMenu(hParentMenu, nPosition, _OR(MF_BYPOSITION, _OR(MF_POPUP, MF_STRING)), DWORD(_CAST, hMenu), String2Psz(cCaption))
			ELSE
				lResult := ModifyMenu(SELF:Handle(), nItemID, _OR(MF_BYCOMMAND, MF_STRING), nItemID, String2Psz(cCaption))
			ENDIF
			AAdd(aItem, {nItemID, oHyperLabel})
		ELSE
			WCError{#RegisterItem, #Menu, __WCSTypeError, nItemID, 1}:Throw()
		ENDIF
	ELSE
		WCError{#RegisterItem, #Menu, __WCSTypeError, nItemID, 1}:Throw()
	ENDIF


	RETURN lResult


/// <include file="Gui.xml" path="doc/Menu.SetAutoUpdate/*" />
method SetAutoUpdate(nMenuNumber)


	iAutoPosition := nMenuNumber
	RETURN iAutoPosition


/// <include file="Gui.xml" path="doc/Menu.SetParent/*" />
method SetParent(oMenu)




	oParent := oMenu


	RETURN SELF


/// <include file="Gui.xml" path="doc/Menu.ShowAsPopup/*" />
method ShowAsPopup(oOwner, oPoint, kButton, kAlignment, oNotOverlap)
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


	if !(oOwner is Window) .and. !(oOwner is Control)
		WCError{#ShowPopup, #Menu, __WCSTypeError, oOwner, 1}:Throw()
	ENDIF


	Default(@kButton, PM_RIGHTBUTTON)
	Default(@kAlignment, PM_ALIGNLEFT)


	if oPoint is Point var oPT
		strucPoint:x := oPT:X
		IF strucPoint:x > GetSystemMetrics(SM_CXScreen)
			strucPoint:x := GetSystemMetrics(SM_CXScreen)
		ELSEIF strucPoint:x < 0
			strucPoint:x := 0
		ENDIF
		strucPoint:y := oPT:Y
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
		GetCursorPos(@strucPoint)
		GetWindowRect(hWnd, @sRect)
		DO CASE
        case oOwner is ListView var oLV
            hWnd := oLV:Handle()
			liItem := ListView_GetNextItem(hWnd, -1, LV_GNIBYITEM+LVNI_FOCUSED)
			IF liItem >= 0 .AND. liItem >= ListView_GetTopIndex(hWnd)
				sRectItem:left := LVIR_LABEL
				IF LOGIC(_CAST, SendMessage(hWnd, LVM_GETITEMRECT, DWORD(liItem), LONGINT(_CAST,@sRectItem)))
					strucPoint:x := sRectItem:Left
					strucPoint:y := sRectItem:Bottom
					ScreenToClient(hWnd, @strucPoint)
				ENDIF
			ENDIF
        case oOwner is TreeView var oTV
            hWnd := oTV:Handle()
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


	if oNotOverlap is Control .or. oNotOverlap is Window
		strucTPM:cbSize := _SIZEOF(_winTPMParams)
		GetWindowRect(oNotOverlap:Handle(), @strucTPM:rcExclude)
		pTPM := @strucTPM
	elseif oNotOverlap is BoundingBox var oBB
		strucTPM:cbSize := _SIZEOF(_winTPMParams)
		SetRect(@strucTPM:rcExclude,oBB:Left,oBB:Top,oBB:Right,oBB:Bottom)
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


/// <include file="Gui.xml" path="doc/Menu.ToolBar/*" />
access ToolBar




	RETURN oToolBar


/// <include file="Gui.xml" path="doc/Menu.ToolBar/*" />
assign ToolBar(oNewToolBar)


	RETURN oToolBar := oNewToolBar


/// <include file="Gui.xml" path="doc/Menu.UncheckItem/*" />
method UncheckItem(nItemID)


	RETURN CheckMenuItem(hMenu, nItemID, _OR(MF_UNCHECKED, MF_BYCOMMAND))


/// <include file="Gui.xml" path="doc/Menu.UnregisterItem/*" />
method UnregisterItem(nItemID)
	//SE-060526
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD
	// !!! Temp
	// Sabo, 09/26/95
	// Workaround for export locals not working yet
	STATIC LOCAL nItemIDKludge AS DWORD


	nItemIDKludge:=nItemID


	IF IsLong(nItemID)
		dwCount := ALen(aItem)
		FOR dwIndex := 1 UPTO dwCount
		   IF nItemIDKludge == aItem[dwIndex, 1]
		   	ADel(aItem, dwIndex)
			   ASize(aItem, dwCount - 1)
			   EXIT
		   ENDIF
		NEXT  // dwIndex
	ELSE
		WCError{#UnregisterItem, #Menu, __WCSTypeError, nItemID, 1}:Throw()
	ENDIF


	RETURN NIL


END CLASS


/// <include file="Gui.xml" path="doc/SystemMenu/*" />
CLASS SystemMenu INHERIT Menu


/// <include file="Gui.xml" path="doc/SystemMenu.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF !InCollect()
		hMenu := 0
	ENDIF
	SUPER:Destroy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/SystemMenu.ctor/*" />
constructor(oOwner)




	SUPER(GetSystemMenu(oOwner:Handle(), FALSE))


	return
END CLASS


