STATIC DEFINE TAB_SYMBOL := 1
STATIC DEFINE TAB_INDEX  := 2
STATIC DEFINE TAB_PAGE   := 3
STATIC DEFINE TIP_SYMBOL := 1
STATIC DEFINE TIP_TEXT   := 2

/*
TODO: OPTIMIZATION
PARTIAL CLASS TabControlEntry
   EXPORT SymName    as SYMBOL
   EXPORT Index      AS LONG
   EXPORT Page       as Object
   EXPORT TipText    AS STRING
*/


PARTIAL CLASS TabControl INHERIT TextControl
	PROTECT oImageList		AS ImageList
	PROTECT aPages			 AS ARRAY
	PROTECT aTipsText		 AS ARRAY
	PROTECT oCurrentPage	AS Window
	PROTECT nMaxHeight		AS INT
	PROTECT nMaxWidth		 AS INT
	PROTECT lAutoSize AS LOGIC

	//PP-030828 Strong typing
	METHOD __AdjustPage() AS VOID STRICT 
	//PP-040614 Method from S Ebert
   // Added check for __SubForm
	LOCAL sRect IS _WINRect
	LOCAL oForm AS __FormFrame

	IF (oCurrentPage != NULL_OBJECT)
		GetWindowRect(hWnd, @sRect)
		TabCtrl_AdjustRect(hWnd, FALSE,  @sRect)

#ifdef __VULCAN__
		MapWindowPoints(HWND_DESKTOP, oFormSurface:Handle(), (_winPOINT PTR) @sRect, 2)
#else		
		MapWindowPoints(HWND_DESKTOP, oFormSurface:Handle(), @sRect, 2)
#endif		

		IF IsInstanceOf(oCurrentPage, #DataWindow) .AND. IVarGet(oCurrentPage, #__SubForm)
			oForm := IVarGet(oCurrentPage, #__FormWindow)
			IF oForm != NULL_OBJECT
				oForm:ChangeFormSize(Point{sRect:left - 1, sRect:top}, Dimension{sRect:right - sRect:left, sRect:bottom - sRect:top - 1}, TRUE)
			ENDIF
		ELSE
			SetWindowPos(oCurrentPage:Handle(), NULL, sRect:left - 1, sRect:top, sRect:right - sRect:left, sRect:bottom - sRect:top - 1, _OR(SWP_NOACTIVATE, SWP_NOZORDER))
		ENDIF
	ENDIF
	RETURN

METHOD __CalcNewDimension(oNewPage AS OBJECT) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL oOrigin AS Point
	LOCAL strucPageRect IS _WINRECT
	LOCAL strucTabRect IS _WINRECT

	oOrigin := SELF:Origin

	// Get the area of both the tab page and the tab control
	GetWindowRect(oNewPage:Handle(), @strucPageRect)
	GetWindowRect(SELF:Handle(), @strucTabRect)

	// Calculate the absolute maximum size to encompass the largest dimensions of any page
	IF nMaxHeight < strucPageRect:bottom - strucPageRect:top
		nMaxHeight := strucPageRect:bottom - strucPageRect:top
	ENDIF
	IF nMaxWidth < strucPageRect:right - strucPageRect:left
		nMaxWidth := strucPageRect:right - strucPageRect:left
	ENDIF

	strucTabRect:bottom := strucTabRect:top + nMaxHeight
	strucTabRect:right := strucTabRect:left + nMaxWidth

	// Apply the new dimensions to all the pages in the tab control
	// ??? really needed ??? it is done in __SetFocusPage anyway
	//SetWindowPos(xPage:Handle(), NULL_PTR, strucTabRect.left, strucTabRect.top, nMaxWidth, nMaxHeight, SWP_NOZORDER)
	//for dwCount := 1 upto ALen(aPages)
	//	SetWindowPos(aPages[dwCount][3]:Handle(), NULL_PTR, strucTabRect.left + __TABBORDER, strucTabRect.top + __TABBORDER, nMaxWidth, nMaxHeight, _Or(SWP_NOREDRAW, SWP_NOZORDER))
	//next dwCount

	// Adjust the client area of the tab control to fit the new size of the pages
	TabCtrl_AdjustRect(SELF:Handle(), TRUE, @strucTabRect)
	SetWindowPos(SELF:Handle(), NULL_PTR, 0, 0,;
		strucTabRect:right-strucTabRect:left, strucTabRect:bottom-strucTabRect:top,;
		_OR(SWP_NOMOVE, SWP_NOZORDER))

	SELF:origin := oOrigin
	RETURN

METHOD __FocusPage(nIndex AS INT) AS VOID STRICT 
	//PP-031129 Revised method from S Ebert-correct Z order after page change, keyboard handling, positioning
	//PP-030828 Strong typing
	//PP-040417 Update from S Ebert
	LOCAL oOldPage        AS Window
	LOCAL rectTab         IS _winRect
	LOCAL lSelfFocus      AS LOGIC
	LOCAL hWndInsertAfter AS PTR
	lSelfFocus := (hWnd = GetFocus())

	oOldPage := SELF:CurrentPage

	oCurrentPage := SELF:__GetPageFromIndex(nIndex)

	IF (oCurrentPage != NULL_OBJECT)
		IF oOldPage != NULL_OBJECT
			oOldPage:Hide()
			SetWindowPos(oOldPage:Handle(), HWND_BOTTOM, 0, 0, 0, 0, _OR(SWP_NOSIZE, SWP_NOMOVE))
		ENDIF

		GetWindowRect(hWnd, @rectTab)
		TabCtrl_AdjustRect(hWnd, FALSE, @rectTab)

		IF ! lAutoSize
			oCurrentPage:Size := Dimension{rectTab:right - rectTab:left, rectTab:bottom - rectTab:top - 1}
		ENDIF

#ifdef __VULCAN__
		MapWindowPoints(HWND_DESKTOP, oFormSurface:Handle(), (_winPOINT PTR) @rectTab, 2)
#else		
		MapWindowPoints(HWND_DESKTOP, oFormSurface:Handle(), @rectTab, 2)
#endif		

		SetWindowPos(oCurrentPage:Handle(), NULL, rectTab:left - 1, rectTab:top, 0, 0, _OR(SWP_NOSIZE, SWP_NOZORDER, SWP_NOREDRAW))

		hWndInsertAfter := GetNextWindow(hwnd, GW_HWNDNEXT)
		IF hWndInsertAfter != NULL_PTR .AND. _AND(GetWindowLong(hWndInsertAfter, GWL_STYLE), WS_VISIBLE) =  WS_VISIBLE
			IF (hWndInsertAfter := GetNextWindow(hwnd, GW_HWNDPREV)) = NULL_PTR
				hWndInsertAfter := HWND_TOP
			ENDIF
		ELSE
			hWndInsertAfter := HWND_TOP
		ENDIF

		SetWindowPos(oCurrentPage:Handle(), hWndInsertAfter, 0, 0, 0, 0, _OR(SWP_NOSIZE, SWP_NOMOVE, SWP_NOREDRAW))

		IF SELF:IsVisible()
			oCurrentPage:Show()
			IF lSelfFocus
				SetFocus(hwnd)
			ELSE
				oCurrentPage:SetFocus()
			ENDIF
		ENDIF
	ENDIF
	RETURN

METHOD __GetIndexFromSymbol(symTabName AS SYMBOL) AS INT STRICT 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD
 	dwCount := ALen(aPages)
	FOR dwI := 1 UPTO dwCount
		IF aPages[dwI, 1] == symTabName
			RETURN aPages[dwI][2]
		ENDIF
	NEXT  // dwI

	RETURN -1
	
METHOD __GetIndexFromPage(oTab) 
	// DHer: 18/12/2008
	LOCAL dwI, dwCount AS DWORD
 	dwCount := ALen(aPages)
	FOR dwI := 1 UPTO dwCount
		IF aPages[dwI, TAB_PAGE] == oTab
			RETURN aPages[dwI][2]
		ENDIF
	NEXT  // dwI

RETURN -1

METHOD __GetPageFromIndex(nTabIndex AS LONG) AS OBJECT STRICT 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD
	LOCAL xPage AS USUAL

	dwCount := ALen(aPages)
	FOR dwI := 1 UPTO dwCount
		IF aPages[dwI, TAB_INDEX] == nTabIndex
			xPage := aPages[dwI][TAB_PAGE]
			IF IsSymbol(xPage)
				xPage := SELF:CreatePageInstance(xPage, aPages[dwI, 1])
				IF xPage != NULL_OBJECT
					aPages[dwI, TAB_PAGE] := xPage
				ENDIF
			ENDIF
			RETURN xPage
		ENDIF
	NEXT  // dwI
	RETURN NULL_OBJECT

METHOD __GetPageFromSymbol(symTabName) 
	// DHer: 18/12/2008
	LOCAL dwI, dwCount AS DWORD
 	dwCount := ALen(aPages)
	FOR dwI := 1 UPTO dwCount
		IF aPages[dwI, TAB_SYMBOL] == symTabName
			RETURN aPages[dwI][TAB_PAGE]
		ENDIF
	NEXT  // dwI

RETURN NULL_OBJECT

METHOD __GetSymbolFromIndex(nTabIndex AS LONG) AS SYMBOL STRICT 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD

	dwCount := ALen(aPages)
	FOR dwI := 1 UPTO dwCount
		IF aPages[dwI, TAB_INDEX] == nTabIndex
			RETURN aPages[dwI][TAB_SYMBOL]
		ENDIF
	NEXT  // dwI

	RETURN NULL_SYMBOL


METHOD __GetSymbolFromPage(oTab) 
	// DHer: 18/12/2008
	LOCAL dwI, dwCount AS DWORD

	dwCount := ALen(aPages)
	FOR dwI := 1 UPTO dwCount
		IF aPages[dwI, TAB_PAGE] == oTab
			RETURN aPages[dwI][TAB_SYMBOL]
		ENDIF
	NEXT  // dwI


RETURN NULL_SYMBOL

METHOD AddTipText(symTabName, cText) 
	//SE-060526
	
	IF ! SELF:ChangeTipText(symTabName, cText)
		AAdd(aTipsText, {symTabName, cText})
	ENDIF
	RETURN NIL

METHOD AppendTab(symTabName, cCaption, xPage, nImage) 
	LOCAL strucTabItem	IS _winTC_Item
	LOCAL pszCaption	AS PSZ
	LOCAL nIndex		AS INT
	LOCAL lReturnValue	AS LOGIC
	LOCAL hFirst, hBefore AS PTR

	//RvdH 070503         
   Default(@nImage, 0)


	// Fill out the tab structure with the arguments passed in
	strucTabItem:mask := _OR(TCIF_TEXT, TCIF_IMAGE)
	strucTabItem:iImage := nImage - 1
	strucTabItem:cchTextMax := 128

	//SE-070430     
	IF IsInstanceOfUsual(xPage,#Window) 
		//PP-030909 XP theme background on tab page
		xPage:EnableThemeDialogTexture(ETDT_ENABLETAB)
		
		IF Empty(cCaption) .AND. (xPage:Hyperlabel != NULL_OBJECT)
			cCaption := xPage:Hyperlabel:Caption
		ENDIF
	ELSEIF ! IsSymbol(xPage)
		xPage := symTabName
	ENDIF

	IF !IsNil(cCaption) .AND. (NULL_STRING != cCaption)
		pszCaption := StringAlloc(cCaption)
	ENDIF
	strucTabItem:pszText := pszCaption

	// Append the new tab and add its page to the list of pages
	nIndex := TabCtrl_InsertItem(SELF:Handle(), INT(_CAST, ALen(aPages)), @strucTabItem)
	IF nIndex != -1
		AAdd(aPages, {symTabName, nIndex, xPage})
		lReturnValue := TRUE
	ENDIF

	IF (PTR(_CAST, pszCaption) != NULL_PTR)
		MemFree(pszCaption)
	ENDIF

	IF lAutoSize .AND. ! IsSymbol(xPage)
		SELF:__CalcNewDimension(xPage)
	ENDIF

	hFirst := GetWindow(SELF:Handle(), GW_HWNDFIRST)
	IF (hFirst == SELF:Handle())
		hBefore := HWND_TOP
	ELSE
		hBefore := GetWindow(SELF:Handle(), GW_HWNDPREV)
	ENDIF
	IF IsInstanceOfUsual(xPage,#Window)
		SetWindowPos(xPage:Handle(), hBefore, 0, 0, 0, 0, _OR(SWP_NOSIZE, SWP_NOMOVE))
	ENDIF
	RETURN lReturnValue

ACCESS AutoSize 
	

	RETURN lAutoSize

ASSIGN AutoSize(lNewValue) 
	

	lAutoSize := lNewValue
	RETURN 

METHOD ChangeTipText(symTabName, cText) 
	//SE-060526
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL symName AS SYMBOL

	

	IF IsLong(symTabName)
		symName := SELF:__GetSymbolFromIndex(symTabName)
	ELSE
		symName := symTabName
	ENDIF

	dwCount := ALen(aTipsText)
	FOR dwIndex := 1 UPTO dwCount
		IF aTipsText[dwIndex, TIP_SYMBOL] == symName
			aTipsText[dwIndex, TIP_TEXT] := cText
			RETURN TRUE
		ENDIF
	NEXT  // dwIndex

	RETURN FALSE

METHOD CreatePageInstance(symPageClass, symTabName) 
	//SE-060526
	LOCAL oPage AS OBJECT

	oPage := CreateInstance(symPageClass, SELF:Owner)
	IF IsInstanceOfUsual(oPage, #Window)
		oPage:EnableThemeDialogTexture(ETDT_ENABLETAB)
	ENDIF

	IF lAutoSize
		SELF:__CalcNewDimension(oPage)
	ENDIF

	IF IsMethod(oPage, #TabInit)
		Send(oPage, #TabInit, symTabName)
	ENDIF

	RETURN oPage

ACCESS CurrentPage 
	

	RETURN oCurrentPage

METHOD DeleteAllTabs() 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD

	

	dwCount := ALen(aPages)
	FOR dwI := 1 UPTO dwCount
		IF IsObject(aPages[dwI, TAB_PAGE])
			Send(aPages[dwI, TAB_PAGE], #Destroy)
		ENDIF
	NEXT  // dwI

	aPages := {}
	aTipsText := {}
	oCurrentPage := NULL_OBJECT

	RETURN LOGIC(_CAST, TabCtrl_DeleteAllItems(SELF:Handle()))

METHOD DeleteTab(symTabName) 
	//SE-060526
	LOCAL dwI, dwCount AS DWORD
	LOCAL iTabIdx AS INT
	LOCAL lRet AS LOGIC

	

	iTabIdx := SELF:__GetIndexFromSymbol(symTabName)

	IF (iTabIdx != -1)
		lRet := LOGIC(_CAST, TabCtrl_DeleteItem(SELF:Handle(), iTabIdx))

		IF lRet
			dwCount := ALen(aPages)
			FOR dwI := 1 UPTO dwCount
				IF aPages[dwI, TAB_SYMBOL] == symTabName
					EXIT
				ENDIF
			NEXT  // dwI
			IF dwI <= dwCount
				IF IsObject(aPages[dwI][3])
					Send(aPages[dwI][3], #Destroy)
				ENDIF
				SELF:RemoveTipText(symTabName)
				ADel(aPages, dwI)
				dwCount -= 1
				ASize(aPages, dwCount)

				// adjust indizes
				FOR dwI:= 1 UPTO dwCount
					IF (aPages[dwI][TAB_INDEX] > iTabIdx)
						aPages[dwI][TAB_INDEX] := aPages[dwI][TAB_INDEX] - 1
					ENDIF
				NEXT  // dwI

				oCurrentPage := NULL_OBJECT
				IF SELF:TabCount > 0
					TabCtrl_SetCurSel(hWnd, 0)
					SELF:__FocusPage(0)
				ENDIF
			ENDIF
		ENDIF
	ENDIF

	RETURN lRet

METHOD Destroy() 
	

	IF !InCollect()
		aPages := NULL_ARRAY
		aTipsText := NULL_ARRAY
		oCurrentPage := NULL_OBJECT
		//oImageList := NULL_OBJECT
	ENDIF

	SUPER:Destroy()

	RETURN NIL


METHOD GetTabBoundingBox(symTabName) 
	LOCAL strucRect IS _winRect
	LOCAL oOrigin	AS Point
	LOCAL oSize		AS Dimension

	

	IF LOGIC(_CAST, TabCtrl_GetItemRect(SELF:Handle(), SELF:__GetIndexFromSymbol(symTabName),  @strucRect))
		oOrigin := Point{strucRect:top, strucRect:left}
		oOrigin := __WCConvertPoint(SELF, oOrigin)
		oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}

		RETURN BoundingBox{oOrigin, oSize}
	ENDIF

	RETURN NULL_OBJECT

METHOD GetCaption(symTabName) 
	//RvdH 070124
	LOCAL strucTabItem IS _winTC_Item
	LOCAL i AS INT
	LOCAL pszCaption AS PSZ
	LOCAL lRet AS LOGIC
	LOCAL cReturn AS STRING

	i := SELF:__GetIndexFromSymbol(symTabName)

	IF (i != -1)
		pszCaption := StringAlloc(Space(128))
		strucTabItem:mask := TCIF_TEXT
		strucTabItem:cchTextMax := 128
		strucTabItem:pszText := pszCaption
		lRet := TabCtrl_GetItem(SELF:Handle(), i, @strucTabItem)
		cReturn := Psz2String( pszCaption )

		MemFree(PTR(_CAST, pszCaption))
	ENDIF

	RETURN cReturn
METHOD GetTabImage (symTabName) 
	//RvdH 070124
	LOCAL nIndex AS INT
	LOCAL strucTabItem IS _winTC_Item
	LOCAL nImageIndex	AS INT
	
	nIndex := SELF:__GetIndexFromSymbol(symTabName)
	IF (nIndex != -1)
		strucTabItem:mask := TCIF_IMAGE
		strucTabItem:iImage := nImageIndex -1
		TabCtrl_GetItem(SELF:Handle(), nIndex, @strucTabItem)
		nImageIndex := strucTabItem:iImage +1
	ENDIF

	RETURN nImageIndex


METHOD GetTabPage(xSymbolOrPosition) 
	IF IsSymbol(xSymbolOrPosition)
		RETURN SELF:__GetPageFromIndex(SELF:__GetIndexFromSymbol(xSymbolOrPosition))
	ELSEIF IsNumeric(xSymbolOrPosition)
		RETURN SELF:__GetPageFromIndex(xSymbolOrPosition-1)
	ENDIF

	RETURN NULL_OBJECT

METHOD GetTipText(symTabName) 
	//SE-060526
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL symName AS SYMBOL

	

	IF IsLong(symTabName)
		symName := SELF:__GetSymbolFromIndex(symTabName)
	ELSE
		symName := symTabName
	ENDIF

	dwCount := ALen(aTipsText)
	FOR dwIndex := 1 UPTO dwCount
		IF aTipsText[dwIndex, TIP_SYMBOL] == symName
			RETURN aTipsText[dwIndex, TIP_TEXT]
		ENDIF
	NEXT  // dwIndex

	RETURN NULL_STRING

METHOD Hide() 
	

	SUPER:Hide()
	IF (oCurrentPage != NULL_OBJECT)
		oCurrentPage:Hide()
	ENDIF

	RETURN NIL

ACCESS ImageList 
	

	RETURN oImageList

ASSIGN ImageList(oNewImageList) 
	

	IF IsInstanceOfUsual(oNewImageList, #ImageList)
		TabCtrl_SetImageList(SELF:Handle(), oNewImageList:Handle())
		oImageList := oNewImageList
	ENDIF

	RETURN 

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	

	IF IsInstanceOfUsual(xID, #ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, , kStyle, FALSE)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, WC_TABCONTROL, kStyle, FALSE)
	ENDIF

	// self:SetStyle(_Or(WS_CHILD, WS_VISIBLE))
	// self:SetStyle(WS_BORDER, false)
	lAutoSize := FALSE
	aPages := {}
	aTipsText := {}

	RETURN 

METHOD InsertTab(nPosition, symTabName, cCaption, xPage, nImage) 
	LOCAL strucTabItem	IS _winTC_Item
	LOCAL pszCaption	AS PSZ
	LOCAL nIndex		AS INT
	LOCAL lReturnValue	AS LOGIC
	LOCAL iLen, i AS INT

	

	//PP-030909 XP theme background on tab page
	IF IsInstanceOfUsual(xPage,#Window)
		xPage:EnableThemeDialogTexture(ETDT_ENABLETAB)
	ENDIF

	// Fill out the tab structure with the arguments passed in
	strucTabItem:mask := _OR(TCIF_TEXT, TCIF_IMAGE)
	strucTabItem:iImage := nImage - 1
	strucTabItem:cchTextMax := 128
	IF !IsNil(cCaption) .AND. !(cCaption == NULL_STRING)
		pszCaption := StringAlloc(cCaption)
	ENDIF
	strucTabItem:pszText := pszCaption

	// Insert the new tab and add its page to the list of pages
	nIndex := TabCtrl_InsertItem(SELF:Handle(), nPosition - 1, @strucTabItem)
	IF nIndex != -1
		// Bug in 2.0a !!!!
		iLen := INT(_CAST, ALen(aPages))
		FOR i := 1 TO iLen
			IF aPages[i][2] >= nIndex
				aPages[i][2] := aPages[i][2] + 1
			ENDIF
		NEXT
		AAdd(aPages, {symTabName, nIndex, xPage})
		lReturnValue := TRUE
	ENDIF

	IF (PTR(_CAST, pszCaption) != NULL_PTR)
		MemFree(PTR(_CAST, pszCaption))
	ENDIF

	IF lAutoSize .AND. ! IsSymbol(xPage) //SE-060526
		SELF:__CalcNewDimension(xPage)
	ENDIF

	RETURN lReturnValue

METHOD IsTabPage(xSymbolOrPosition) 
    //SE-070430
    LOCAL dwTabIndex   AS DWORD
    LOCAL symTabName   AS SYMBOL
    LOCAL dwI, dwCount AS DWORD
    LOCAL cbBlock      AS CODEBLOCK 
	
    IF IsSymbol(xSymbolOrPosition)
        symTabName := xSymbolOrPosition 
        cbBlock    := {|x|x[TAB_SYMBOL]==symTabName}
    ELSEIF IsNumeric(xSymbolOrPosition)
        dwTabIndex := xSymbolOrPosition-1
        cbBlock    := {|x|x[TAB_INDEX]==dwTabIndex}
    ELSE
        RETURN FALSE
    ENDIF

    dwCount := ALen(aPages)
    FOR dwI := 1 UPTO dwCount 
        IF Eval(cbBlock, aPages[dwI])
            IF IsInstanceOfUsual(aPages[dwI, TAB_PAGE], #Window)
                RETURN TRUE
            ENDIF    
            EXIT
        ENDIF
    NEXT

    RETURN FALSE


METHOD Move(oMoveEvent) 
	//PP-040614 Method from S Ebert
	SELF:__AdjustPage()
	RETURN NIL

METHOD PadTabs(dwWidth, dwHeight) 
	

	TabCtrl_SetPadding(SELF:Handle(), dwWidth, dwHeight)

	RETURN NIL

METHOD RemoveTabImage(nImageIndex) 

	
	TabCtrl_RemoveImage(SELF:Handle(), nImageIndex -1)
	RETURN TRUE

METHOD RemoveTipText(symTabName) 
	//SE-060526
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL symName AS SYMBOL

	

	IF IsLong(symTabName)
		symName := SELF:__GetSymbolFromIndex(symTabName)
	ELSE
		symName := symTabName
	ENDIF

	dwCount := ALen(aTipsText)
	FOR dwIndex := 1 UPTO dwCount
		IF aTipsText[dwIndex, TIP_SYMBOL] == symName
			ADel(aTipsText, dwIndex)
			ASize(aTipsText, dwCount - 1)
			RETURN TRUE
		ENDIF
	NEXT  // dwIndex

	RETURN FALSE

METHOD Resize(oResizeEvent) 
    //PP-040322 Method from S Ebert
    SELF:__AdjustPage()
    RETURN SELF

ACCESS RowCount 
	

	IF (_AND(GetWindowLong(SELF:Handle(), GWL_STYLE), TCS_MULTILINE) > 0)
		RETURN TabCtrl_GetRowCount(SELF:Handle())
	ENDIF

	RETURN 1

ACCESS SelectedTab 
	RETURN SELF:__GetSymbolFromIndex(TabCtrl_GetCurSel(SELF:Handle()))

ACCESS SelectedTabPage 
	LOCAL sTabPage AS SYMBOL

	sTabPage := SELF:SelectedTab

	IF (sTabPage == NULL_SYMBOL)
		RETURN NULL_OBJECT
	ENDIF
	RETURN SELF:GetTabPage(sTabPage)

METHOD SelectTab(symTabName) 
	LOCAL nIndex AS INT

	nIndex := SELF:__GetIndexFromSymbol(symTabName)
	IF nIndex != -1
		TabCtrl_SetCurSel(SELF:Handle(), DWORD(nIndex))
		SELF:__FocusPage(DWORD(nIndex))
	ENDIF

	RETURN NIL

METHOD SetCaption(symTabName, cCaption) 
	//RvdH 070124
	LOCAL strucTabItem IS _winTC_Item
	LOCAL i AS INT
	LOCAL pszCaption AS PSZ
	LOCAL lRet AS LOGIC

	i := SELF:__GetIndexFromSymbol(symTabName)

	IF (i != -1) .AND. !IsNil(cCaption) .AND. (NULL_STRING != cCaption)
		pszCaption := StringAlloc(cCaption)
		strucTabItem:mask := TCIF_TEXT
		strucTabItem:cchTextMax := 128
		strucTabItem:pszText := pszCaption
		lRet := TabCtrl_SetItem(SELF:Handle(), i,  @strucTabItem)

		MemFree(PTR(_CAST, pszCaption))
	ENDIF

	RETURN lRet

METHOD SetTabImage(symTabName, nImageIndex) 
	LOCAL nIndex AS INT
	LOCAL strucTabItem IS _winTC_ITEM

	nIndex := SELF:__GetIndexFromSymbol(symTabName)
	IF (nIndex != -1)
		strucTabItem:mask := TCIF_IMAGE
		strucTabItem:iImage := nImageIndex -1
		TabCtrl_SetItem(SELF:Handle(), nIndex, @strucTabItem)
	ENDIF

	RETURN nImageIndex

METHOD SetTipText(symTabName, cText) 
	//RvdH 070124 	
	RETURN SELF:AddTipText(symTabName, cText)

METHOD Show() 
	SUPER:Show()

	IF (oCurrentPage != NULL_OBJECT)
		oCurrentPage:Show()
	ENDIF

	RETURN NIL

ACCESS TabCaption (symTabName) 
	RETURN SELF:GetCaption(symTabName)
	

#warning changed indexed assign TabCaption
//ASSIGN TabCaption (symTabName,cCaption)
ASSIGN TabCaption (cCaption,symTabName) 
	SELF:SetCaption(symTabName, cCaption)
	RETURN 
	

ACCESS TabCount 
	RETURN TabCtrl_GetItemCount(SELF:Handle())
END CLASS



#region defines
DEFINE iPageBorder := 2
#endregion
