CLASS __VOToolBarChild  INHERIT VObject
	//RvdH 0702056 Added to replace Child Toolbar Subarray items
	EXPORT NameSym 				AS SYMBOL
	EXPORT Handle					AS PTR
	EXPORT ImageList 				AS OBJECT
	EXPORT HotImageList 			AS OBJECT
	EXPORT DisabledImageList 	AS OBJECT

CONSTRUCTOR() 
    
    SUPER()


RETURN 
END CLASS

CLASS __VOToolBarExtraBitmap INHERIT VObject                     
	//RvdH 0702056 Added to replace Toolbar ExtraBitmap subarray items
	EXPORT Bitmap				AS OBJECT
	EXPORT ImageCount			AS LONGINT
	EXPORT FirstImageIndex	AS LONGINT
	EXPORT NameSym				AS SYMBOL

CONSTRUCTOR() 
    
    SUPER()


RETURN 
END CLASS

CLASS __VOToolBarTipText	INHERIT VObject                                  
	//RvdH 0702056 Added to replace Toolbar Tiptext subarray Items
	EXPORT ButtonID 	AS LONGINT
	EXPORT MenuItemID	AS LONGINT
	EXPORT TipText		AS STRING


CONSTRUCTOR() 
    
    SUPER()


RETURN 
END CLASS

CLASS __VOToolBarUpdate  INHERIT VObject 
	//RvdH 0702056 Added to replace Toolbar ptrUpdate structure elements
	EXPORT symAction	 	AS SYMBOL
	EXPORT nButtonID	 	AS LONGINT
	EXPORT nMenuItemID 	AS LONGINT
	EXPORT nBeforeID	 	AS LONGINT
	EXPORT oBitmap		 	AS OBJECT
	EXPORT nPosition	 	AS DWORD
	EXPORT cButtonText 	AS STRING
	EXPORT nImageCount 	AS DWORD
	EXPORT bState		 	AS BYTE
	EXPORT bStyle		 	AS BYTE
	EXPORT symToolBar 	AS SYMBOL
	EXPORT lFlat         AS LOGIC

CONSTRUCTOR() 
    
    SUPER()


RETURN 
END CLASS

CLASS ToolBar INHERIT Control
	PROTECT oBitmap 			AS Bitmap
	PROTECT aExtraBitmaps 	AS ARRAY
	PROTECT nButtonStyle 	AS DWORD
	PROTECT nImageCount 		AS INT
	PROTECT oButtonSize 		AS Dimension
	//PROTECT ptrUpdateArray 	AS PTR		//RvdH 070206 Changed to use regular Array
	//PROTECT dwUpdateCount 	AS DWORD		//RvdH 070206 Changed to use regular Array
	PROTECT aUpdates 			AS ARRAY			//RvdH 070206 Added to replace ptrUpdateArray
	PROTECT dwBufferSize 	AS DWORD      //SE-070427 unused, can be deleted in the future
	PROTECT aTipsText 		AS ARRAY
	PROTECT lFlat 				AS LOGIC
	PROTECT hwndMainTB 		AS PTR
	PROTECT aChildren 		AS ARRAY
	PROTECT oBandImageList 	AS ImageList
	PROTECT lOldStyle 		AS LOGIC
	PROTECT aBackBitmaps 	AS ARRAY
	//PP-030910
	EXPORT Divider 			AS LOGIC

	//PP-030828 Strong typing
	ACCESS __ButtonStyle AS DWORD STRICT 
	//PP-030828 Strong typing
	

	RETURN nButtonStyle

METHOD __CreateToolBar(symTB AS SYMBOL, hwndParent AS PTR, dwID AS DWORD, dwTBStyle AS DWORD) AS PTR STRICT 
	//PP-040914 from S Ebert
	//SE-050729
	//RvdH 0702056 Changed to use __VOToolBarChild
	LOCAL oImagelist   AS ImageList
	LOCAL hWndTB       AS PTR
	LOCAL oTB          AS __VOToolBarChild
	LOCAL sTBAddBitmap IS _winTBAddBitmap          
	LOCAL liTbStyle		 AS LONGINT

	hWndTB := CreateWindowEx(0, String2Psz(TOOLBARCLASSNAME), NULL_PSZ, dwTBStyle,;
		0, 0, 0, 0, hwndParent, wID, _GetInst(), 0)

	IF hWndTB != NULL_PTR
		liTbStyle := LONGINT(_CAST, dwTBStyle)

		IF _AND(liTbStyle, TBSTYLE_TRANSPARENT) = 0
			liTbStyle := GetWindowLong(hWndTB, GWL_STYLE)
			IF _AND(liTbStyle, TBSTYLE_TRANSPARENT) > 0
				SetWindowLong(hWndTB, GWL_STYLE, _AND(liTbStyle, _NOT(TBSTYLE_TRANSPARENT)))
			ENDIF
		ENDIF

		oTB := SELF:__FindToolBar(symTB)
		IF oTB == NULL_OBJECT
			oTB			:= __VOToolBarChild{}
			oTB:NameSym := symTB
			oTB:Handle 	:= hWndTB 
			AAdd(aChildren, oTB)
		ELSE
			oTB:Handle:= hWndTB
		ENDIF

		hwndParent := hWnd
		hWnd       := hWndTB
		__WCRegisterControl(SELF)
		hWnd       := hwndParent

		SendMessage(hWndTB, TB_BUTTONSTRUCTSIZE, _SIZEOF(_winTBBUTTON), 0l)
		SendMessage(hWndTB, TB_SETBITMAPSIZE, 0u, MakeLong(LoWord(DWORD(oButtonSize:Width)), LoWord(DWORD(oButtonSize:Height))))

		oImagelist := oTb:ImageList
		IF oImageList != NULL_OBJECT
			SendMessage(hWndTB, TB_SETIMAGELIST, 0u, LONGINT(_CAST, oImageList:Handle()))

			oImagelist := oTB:HotImageList
			IF oImageList != NULL_OBJECT
				SendMessage(hWndTB, TB_SETHOTIMAGELIST, 0u, LONGINT(_CAST, oImageList:Handle()))
			ENDIF
			oImagelist := oTB:DisabledImageList
			IF oImageList != NULL_OBJECT
				SendMessage(hWndTB, TB_SETDISABLEDIMAGELIST, 0u, LONGINT(_CAST, oImageList:Handle()))
			ENDIF
		ELSEIF oBitmap != NULL_OBJECT //SE-050729
			sTBAddBitmap:hInst := NULL_PTR
			sTBAddBitmap:nID   := DWORD(_CAST,oBitmap:Handle())
			SendMessage(hWndTB, TB_ADDBITMAP , DWORD(_CAST,nImageCount), LONGINT(_CAST,@sTBAddBitmap))
		ENDIF

	ENDIF

	RETURN hWndTB             

METHOD __FindExtraBitMap(oBmp AS OBJECT, symTB AS SYMBOL)  AS DWORD STRICT 
	//RvdH 070206 Added to centralize location of Extra Bitmaps
	LOCAL dwIndex, dwCount AS DWORD
	LOCAL dwResult	AS DWORD
	LOCAL oExtraBitMap AS __VoToolBarExtraBitmap
	dwCount := ALen(aExtraBitmaps)
	FOR dwIndex := 1 UPTO dwCount
		oExtraBitMap	:= aExtraBitmaps[dwIndex]
		IF oExtraBitMap:Bitmap == oBmp .AND. oExtraBitMap:NameSym == symTB 
			dwResult := dwIndex
			EXIT	   	
		ENDIF
	NEXT //dwIndex
	RETURN dwResult	

METHOD __FindTipText(nID AS LONGINT, symLookUp AS SYMBOL)  AS DWORD STRICT 
	//RvdH 070206 Added to centralize location of TipTexts
	LOCAL dwIndex, dwCount AS DWORD
	LOCAL dwResult	AS DWORD
	LOCAL oText AS __VOToolBarTipText
	dwCount := ALen(aTipsText)
	IF (symLookUp == #ButtonID)
		FOR dwIndex := 1 UPTO dwCount
			oText	:= aTipsText[dwIndex]
			IF oText:ButtonID == nID
				dwResult := dwIndex
				EXIT	   	
			ENDIF
		NEXT //dwIndex
	ELSE
		FOR dwIndex := 1 UPTO dwCount
			oText	:= aTipsText[dwIndex]
			IF oText:MenuItemID == nID
				dwResult := dwIndex
				EXIT	   	
			ENDIF
		NEXT //dwIndex
	ENDIF		
	RETURN dwResult

METHOD __FindToolBar(symTB AS SYMBOL) AS OBJECT STRICT 
	//PP-040417 from S Ebert
	//RvdH 0702056 Changed to use __VOToolBarChild
	LOCAL idx, iLen AS DWORD
	LOCAL oChild	AS __VOToolBarChild
	

	iLen := ALen(aChildren)
	FOR idx := 1 UPTO iLen        
		oChild := aChildren[idx]
		IF oChild:NameSym == symTB
			RETURN oChild
		ENDIF
	NEXT // idx

	RETURN NULL_OBJECT

METHOD __FindToolBarHandle(symTB AS SYMBOL) AS PTR STRICT 
	//PP-030828 Strong typing
	//PP-040417 Update from S Ebert
	//RvdH 0702056 Changed to use __VOToolBarChild
	LOCAL oTB AS __VOToolBarChild
	oTB := SELF:__FindToolBar(symTB)

	//SE-070427
	IF oTB != NULL_OBJECT .AND. oTB:oCargo == NULL_OBJECT
		RETURN oTB:Handle
	ENDIF

	RETURN NULL_PTR

ACCESS __IsRebar AS LOGIC STRICT 
	//PP-030828 Strong typing
	

	RETURN (gpfnInitCommonControlsEx != NULL_PTR) .AND. !lOldStyle

ACCESS __IsTopAligned AS LOGIC STRICT 
	//PP-030828 Strong typing
	

	IF (hWnd != NULL_PTR)
		IF _AND(GetWindowLong(hwnd, GWL_STYLE), CCS_TOP) == 1
			RETURN TRUE
		ENDIF
	ENDIF

	RETURN FALSE

METHOD __SetParent(oObject AS OBJECT) AS VOID STRICT 
	//PP-030828 Strong typing
	

	IF (oObject != NULL_OBJECT)
		IF (hWnd != NULL_PTR)
			// SendMessage(self:Handle(), WM_SIZE, oResEvt:wParam, oResEvt:lParam) // !!! old code !!!

			//SendMessage(hWndTB, TB_SETPARENT, dword(_cast, oObject:Handle()), 0) // doesn't work if TB is ReBar, needed ???
		ENDIF
		oParent := oObject
		oFormSurface := oObject
	ENDIF
	RETURN


METHOD __TryDeferAction(symAction AS SYMBOL, nMenuItemID AS LONGINT, symTB AS SYMBOL) AS __VOToolBarUpdate STRICT 
	//PP-030828 Strong typing
	//PP-040421 Update from S Ebert
	//RvdH 070206 Changed to use __VOToolBarUpdate class
	LOCAL oUpdate AS __VOToolBarUpdate

	IF (aUpdates == NULL_ARRAY)
		RETURN NULL_OBJECT
	ENDIF

	// If the toolbar has not yet been created, fill out the buffer information
	// necessary to apply this operation when it is created

	oUpdate 		:= __VOToolBarUpdate{}
	AAdd(aUpdates, oUpdate)

	oUpdate:symAction 	:= symAction
	oUpdate:nMenuItemID 	:= nMenuItemID
	oUpdate:symToolBar 	:= symTB

	RETURN oUpdate


METHOD AddBand(sBandName, oControl, iPos, iMinWidth, iMinHeight, sText, oForeColor, ;
		oBackColor, iImageIndex, oBackBitmap) 
	//PP-040511 Update from S Ebert
	LOCAL rbBand 			IS _winREBARBANDINFO
	LOCAL lRet := FALSE 	AS LOGIC
	LOCAL liPos 			AS LONGINT
	LOCAL oChild			AS __VOToolBarChild
	IF !SELF:__IsRebar
		RETURN FALSE
	ENDIF

	IF IsNumeric(iPos)
		liPos := iPos
	ELSE
		liPos := -1l
	ENDIF

	MemClear(@rbBand, _SIZEOF(_winREBARBANDINFO))
	rbBand:cbSize := _SIZEOF(_winREBARBANDINFO) //COMPAT_REBARBANDINFO_SIZE
	rbBand:fMask  := _OR(RBBIM_CHILD, RBBIM_STYLE, RBBIM_COLORS, RBBIM_ID)

	IF IsLong(iMinHeight) .AND. IsLong(iMinWidth)
		rbBand:fMask := _OR(rbBand:fMask, RBBIM_CHILDSIZE)
		rbBand:cxMinChild := iMinWidth
		rbBand:cyMinChild := iMinHeight
	ENDIF

	IF !Empty(sText)
		rbBand:fMask := _OR(rbBand:fMask, RBBIM_TEXT)
		rbBand:lpText := StringAlloc(sText)
		rbBand:cch := SLen(sText)
	ENDIF

	rbBand:clrFore := GetSysColor(COLOR_BTNTEXT)
	rbBand:clrBack := GetSysColor(COLOR_BTNFACE)
	IF IsInstanceOf(oForeColor, #Color)
		rbBand:clrFore := oForeColor:ColorRef
	ENDIF
	IF IsInstanceOf(oBackColor, #Color)
		rbBand:clrBack := oBackColor:ColorRef
	ENDIF

	IF IsInstanceOf(oBackBitmap, #Bitmap)
		rbBand:fMask := _OR(rbBand:fMask, RBBIM_BACKGROUND)
		rbBand:hbmBack := oBackBitmap:Handle()
		AAdd(aBackBitmaps, oBackBitmap)
	ENDIF

	IF IsLong(iImageIndex)
		rbBand:fMask := _OR(rbBand:fMask, RBBIM_IMAGE)
		rbBand:iImage := iImageIndex
	ENDIF

	rbBand:fStyle := _OR(RBBS_CHILDEDGE, RBBS_GRIPPERALWAYS)
	//RvdH 070427 	oControl MUST be a Control (according to docs)
	//					but we also accept other objects that has a handle (a window)
	rbBand:hwndChild := oControl:Handle()
	IF IsAccess(oControl, #ControlID) 
		rbBand:wID := oControl:ControlID
	ELSE
		rbBand:wID := DWORD(100+SendMessage(hWnd, RB_GETBANDCOUNT, 0, 0))
	ENDIF

	//PP-031115 Use ilPos to determine where band is inserted
	IF (SendMessage(hWnd, RB_INSERTBAND, DWORD(_CAST, liPos), LONGINT(_CAST, @rbBand)) != 0)
		oChild := __VOToolBarChild{}
		oChild:NameSym := sBandName
		//SE-070427
		oChild:oCargo := oControl
		oChild:Handle := oControl:Handle()

		//AAdd(aChildren, {sBandName, oChild, Null_OBJECT, Null_OBJECT, Null_OBJECT})
		AAdd(aChildren, oChild)
		lRet := TRUE
	ENDIF

	IF (PTR(_CAST, rbBand:lpText) != NULL_PTR)
		MemFree(rbBand:lpText)
	ENDIF

	RETURN lRet

METHOD AddSubToolBarBand(symToolBar, iPos, iMinWidth, lFlat_dwStyle) 
	//PP-040505 Update from S Ebert
	//RvdH 070206 Changed to use __VOToolBarUpdate class
	LOCAL hwndNewTB   AS PTR
	LOCAL rbBand      IS _winREBARBANDINFO
	LOCAL dwSubStyle  AS DWORD
	LOCAL oUpdate		AS __VOToolBarUpdate
	LOCAL dwBandCount AS DWORD
	LOCAL sSize       IS _WinSize
	LOCAL hDC         AS PTR
	LOCAL hFont       AS PTR

	

	IF !SELF:__IsRebar
		RETURN FALSE
	ENDIF

	Default(@iPos, -1)
	Default(@iMinWidth, 100)
	Default(@lFlat_dwStyle, lFlat)

	IF (hWnd != NULL_PTR)
		IF IsNumeric(lFlat_dwStyle)
			dwSubStyle := lFlat_dwStyle
			dwSubStyle := _OR(LONGINT(dwSubStyle), WS_CHILD, WS_CLIPSIBLINGS, WS_VISIBLE, CCS_NORESIZE, CCS_NOPARENTALIGN, CCS_NODIVIDER)
			IF IsThemeEnabled()
				dwSubStyle := _OR(dwSubStyle, TBSTYLE_TRANSPARENT)
			ENDIF
		ELSE
			dwSubStyle := dwStyle

			IF lFlat_dwStyle
				dwSubStyle := _OR(dwSubStyle, TBSTYLE_FLAT)
			ELSE
				dwSubStyle := _AND(dwSubStyle, _NOT(TBSTYLE_FLAT))
			ENDIF
		ENDIF

		dwBandCount := DWORD(_CAST, SendMessage(hWnd, RB_GETBANDCOUNT, 0, 0))

		hWndNewTB := SELF:__CreateToolBar(symToolBar, hwnd, 1000U+dwBandCount, dwSubStyle)

		IF (hWndNewTB == NULL_PTR)
			RETURN FALSE
		ENDIF

		MemSet(@rbBand, 0, _SIZEOF(_winREBARBANDINFO))
		rbBand:cbSize     := _SIZEOF(_winREBARBANDINFO) // COMPAT_REBARBANDINFO_SIZE
		rbBand:fMask      := _OR(RBBIM_CHILD, RBBIM_SIZE, RBBIM_CHILDSIZE, RBBIM_STYLE, RBBIM_COLORS, RBBIM_ID)
		rbBand:clrFore    := GetSysColor(COLOR_BTNTEXT)
		rbBand:clrBack    := GetSysColor(COLOR_BTNFACE)
		rbBand:fStyle     := _OR(RBBS_NOVERT, RBBS_CHILDEDGE, RBBS_CHILDEDGE)
		//rbBand.hbmBack  := LoadBitmap(hInst, MAKEINTRESOURCE(IDB_BACK));
			rbBand:hwndChild  := hWndNewTB
		rbBand:wID        := 1042U + dwBandCount
		rbBand:cx         := iMinWidth
		rbBand:cxMinChild := iMinWidth
		rbBand:cyMinChild := HiWord(DWORD(_CAST, SendMessage(hWndNewTB, TB_GETBUTTONSIZE, 0, 0))) + IIF(_AND(dwSubStyle, TBSTYLE_FLAT) = TBSTYLE_FLAT,2, 6)

		IF SELF:__ButtonStyle != TB_ICONONLY
			hDC := GetDC(hWndNewTB)
			IF (hFont := SendMessage(hWndNewTB, WM_GETFONT, 0l, 0l)) != NULL_PTR
				SelectObject(hdc, hFont)
			ENDIF
			GetTextExtentPoint(hDC, String2Psz( "M"), 1, @sSize)
			ReleaseDC(hWndNewTB, hDC)
			IF SELF:__ButtonStyle = TB_TEXTANDICON .AND. _AND(dwSubStyle, TBSTYLE_LIST) = 0
				rbBand:cyMinChild += DWORD(sSize:cy)
			ELSEIF rbBand:cyMinChild < DWORD(sSize:cy + 8)
				rbBand:cyMinChild := DWORD(sSize:cy + 8)
			ENDIF
		ENDIF

		// Insert band into rebar
		SendMessage(hWnd, RB_INSERTBAND, DWORD(_CAST, iPos), LONGINT(_CAST, @rbBand))
	ELSEIF (aUpdates != NULL_ARRAY)
		oUpdate := SELF:__TryDeferAction(#AddSubToolBarBand, iPos, symToolBar)
		// "mis-use" entries to store symToolBar, iPos, iMinWidth, lFlat)
		oUpdate:nButtonID := iMinWidth
		IF IsLogic(lFlat_dwStyle)
			oUpdate:nPosition := DWORD(_CAST, lFlat_dwStyle)
			oUpdate:bState    := 0
		ELSEIF IsNumeric(lFlat_dwStyle)
			oUpdate:nPosition := DWORD(lFlat_dwStyle)
			oUpdate:bState    := 1
		ENDIF
	ENDIF
	RETURN SELF

METHOD AddTipText(nButtonID, nMenuItemID, cText) 
	//RvdH 070206 Changerd to use new __VOToolBarTipText objects
#ifndef __VULCAN__	
	LOCAL pszText AS PSZ 
#endif	
	LOCAL oTipText AS __VOToolBarTipText

	
	EnforceNumeric(@nButtonID)
	EnforceNumeric(@nMenuItemID)
	EnforceType(@cText,STRING)
	IF cText == TB_NOSTRING
		cText := NULL_STRING
	ENDIF
	IF cText == TB_DEFSTRING
		cText := NULL_STRING
		IF nButtonID <= 132
#ifdef __VULCAN__
         cText := __CavoStr( __WCToolTipOffset + nButtonID )
#else   
			pszText := PSZ(_CAST, MemAlloc(80))
			IF LoadString(GetNatDllHandle(), __WCToolTipOffset + nButtonID, pszText, 80) != 0
				cText := Psz2String(pszText)
			ENDIF
			MemFree(PTR(_CAST, pszText))
#endif			
		ENDIF
	ENDIF                               
	oTipText 				:= __VOToolBarTipText{}
	oTipText:ButtonID 	:= nButtonID
	oTipText:MenuItemID 	:= nMenuItemID 
	oTipText:TipText		:= cText
	AAdd(aTipsText, oTipText)

	RETURN NIL

METHOD AppendItem(nButtonID, nMenuItemID, oBmp, nPosition, cTitle, nImgCount, bState, bStyle, symTB) 
	//PP-040505 Update from S Ebert
	//SE-050929           
	//RvdH 070206 Changed to use new __VoToolBarExtraBitmap objects
	//RvdH 070206 Changed to use __VOToolBarUpdate class
	LOCAL strucButton 	IS _winTBBUTTON
	LOCAL strucAddBitmap IS _winTBADDBITMAP
	LOCAL oUpdate		 	AS __VOToolBarUpdate
	LOCAL pszTitle AS PSZ
	LOCAL dwIndex 	AS DWORD
	LOCAL oExtraBitmap AS __VoToolBarExtraBitmap
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF hWnd != NULL_PTR .AND. hwndTB = NULL_PTR .AND. symTB = #MAINTOOLBAR
		//for back compatibility
		SELF:AddSubToolBarBand(symTB)
		hwndTB := hwndMainTB := SELF:__FindToolBarHandle(symTB)
	ENDIF

	IF hWndTB != NULL_PTR
		IF IsLong(nButtonID) .AND. nButtonID >= I_IMAGENONE
			IF nButtonID == IDT_SEPARATOR
				// Separator
				IF IsLong(nMenuItemID)
					strucButton:iBitmap := nMenuItemID
				ELSE
					strucButton:iBitmap := 0
				ENDIF
				strucButton:idCommand := 0
				strucButton:fsStyle := TBSTYLE_SEP
			ELSEIF IsLong(nMenuItemID)
				// Button
				strucButton:idCommand := nMenuItemID
				strucButton:fsState := TBSTATE_ENABLED
				strucButton:fsStyle := TBSTYLE_BUTTON
				//SE-050729
				IF oBitmap != NULL_OBJECT .AND. IsObject(oBmp) .AND. oBmp != NULL_OBJECT .AND. SELF:__ButtonStyle != TB_TEXTONLY
					Default(@nImgCount, 1)
					//SE-060526
					dwIndex := SELF:__FindExtraBitMap(oBmp, symTB)
					IF dwIndex == 0
						oExtraBitMap 				:= __VoToolBarExtraBitmap{}
						oExtraBitMap:Bitmap 		:= oBmp
						oExtraBitMap:ImageCount := nPosition
						AAdd(aExtraBitmaps, oExtraBitMap)
					ELSE
						oExtraBitMap := aExtraBitmaps[dwIndex]               	
					ENDIF                        
					IF oExtraBitMap:NameSym == NULL_SYMBOL
						strucAddBitmap:hInst 	:= NULL_PTR
						strucAddBitmap:nID 		:= DWORD(_CAST, oBmp:Handle())
						nImgCount := Max(nImgCount, oExtraBitMap:ImageCount)
						oExtraBitMap:FirstImageIndex:= SendMessage(hWndTB, TB_ADDBITMAP, nImgCount, LONGINT(_CAST, @strucAddBitmap))
						oExtraBitMap:NameSym	:= symTB      
					ENDIF

				ENDIF

				IF SELF:__ButtonStyle != TB_TEXTONLY
					//SE-050905
					IF oBitmap != NULL_OBJECT
						IF nButtonID < 0
							strucButton:iBitmap := nButtonID
						ELSEIF nButtonID <= IDT_CUSTOMBITMAP
							strucButton:iBitmap := nButtonID - 1l
						ELSEIF IsLong(nPosition)
							strucButton:iBitmap := oExtraBitMap:FirstImageIndex + nPosition - 1l
						ELSE
							strucButton:iBitmap := 1l
						ENDIF
					ELSE
						strucButton:iBitmap := nButtonID - 1
					ENDIF
				ELSE
					strucButton:iBitmap := -1l
				ENDIF

				strucButton:iString := -1l
				IF SELF:__ButtonStyle != TB_ICONONLY
					IF IsString(cTitle)
						IF ! cTitle == NULL_STRING
							pszTitle := StringAlloc(cTitle+Chr(0))
							strucButton:iString := SendMessage(hWndTB, TB_ADDSTRING, 0, LONGINT(_CAST, pszTitle))
							MemFree(pszTitle)
						ENDIF
					ELSEIF nButtonID > 0 .AND. nButtonID <= IDT_CUSTOMBITMAP .AND. oBitmap != NULL_OBJECT
#ifdef __VULCAN__
                  LOCAL cText AS STRING
                  cText := __CavoStr( __WCToolBarOffset + nButtonID )
						strucButton:iString := SendMessage(hWndTB, TB_ADDSTRING, 0, LONGINT(_CAST, String2Psz( cText ) ) )
#else   
						pszTitle := PSZ(_CAST, MemAlloc(80))
						MemSet(pszTitle, 0, 80)
						IF LoadString(GetNatDllHandle(), __WCToolBarOffset + nButtonID, pszTitle, 80) != 0
							strucButton:iString := SendMessage(hWndTB, TB_ADDSTRING, 0, LONGINT(_CAST, pszTitle))
						ENDIF
						MemFree(pszTitle)
#endif						
					ENDIF
				ENDIF

				IF IsNumeric(bState)
					strucButton:fsState := bState
				ENDIF
				IF IsNumeric(bStyle)
					strucButton:fsStyle := bStyle
				ENDIF
			ELSE
				//SE-050729
				RETURN FALSE
			ENDIF
			//SE-050729
			RETURN LOGIC(_CAST, SendMessage(hWndTB, TB_ADDBUTTONS, 1, LONGINT(_CAST, @strucButton)))
		ENDIF
		RETURN FALSE
	ELSEIF SELF:aUpdates != NULL_ARRAY
		// If the toolbar has not yet been created, fill out the buffer information
		// necessary to apply this operation when it is created
		IF ! IsNumeric(nMenuItemID)
			nMenuItemID := 0
		ENDIF
		oUpdate := SELF:__TryDeferAction(#AppendItem, nMenuItemID, symTB)

		IF IsObject(oBmp)
			//SE-060526
			dwIndex := SELF:__FindExtraBitMap(oBmp, symTB)
			IF dwIndex == 0
				oExtraBitMap := __VoToolBarExtraBitmap{}
				oExtraBitMap:Bitmap 		:= oBmp
				oExtraBitMap:ImageCount := nPosition
				AAdd(aExtraBitmaps, oExtraBitMap)
			ELSE // update ImageCount
				oExtraBitMap := aExtraBitmaps[dwIndex]
				oExtraBitMap:FirstImageIndex := Max(oExtraBitMap:FirstImageIndex, nPosition)
			ENDIF
			oUpdate:oBitmap := oBmp
		ENDIF
		IF IsNumeric(nPosition)
			oUpdate:nPosition := nPosition
		ENDIF
		IF IsString(cTitle)
			oUpdate:nBeforeID     := 1
			oUpdate:cButtonText 	 := cTitle
		ELSE
			oUpdate:nBeforeID     := 0
			oUpdate:cButtonText := NULL_STRING
		ENDIF
		IF IsNumeric(nImgCount)
			oUpdate:nImageCount := nImgCount
		ENDIF

		Default(@bState, TBSTATE_ENABLED)
		Default(@bStyle, TBSTYLE_BUTTON)

		oUpdate:nButtonID   := nButtonID
		oUpdate:bState      := bState
		oUpdate:bStyle      := bStyle
	ENDIF

	RETURN TRUE

METHOD AppendSubItem(symTB, nButtonID, nMenuItemID, oBmp, nPosition, cTitle, nImgCount, bState, bStyle) 
	
	// This is only a shortcut for AppendItem
	RETURN SELF:AppendItem(nButtonID, nMenuItemID, oBmp, nPosition, cTitle, nImgCount, bState, bStyle, symTB)

ACCESS BandCount 
	

	IF !SELF:__IsRebar
		RETURN 0
	ENDIF

	RETURN SendMessage(hwnd, RB_GETBANDCOUNT, 0, 0)

ACCESS BandImageList 
	

	RETURN oBandImageList

ASSIGN BandImageList(oImageList) 
	LOCAL rbi IS _winREBARINFO

	

	IF SELF:__IsRebar
		oBandImageList := oImageList

		IF (hwnd != NULL)
			rbi:cbSize := _SIZEOF(_winREBARINFO)
			rbi:fMask := RBIM_IMAGELIST
			rbi:himl := IIF(IsInstanceOf(oBandImageList, #ImageList), oBandImageList:Handle(), NULL_PTR)

			SendMessage(hwnd, RB_SETBARINFO, 0, LONGINT(_CAST, @rbi))
		ENDIF
	ENDIF

	RETURN 

ACCESS Bitmap 
	

	RETURN oBitmap

ASSIGN Bitmap(oNewBitmap) 
	//SE-050929
	LOCAL oBMPSize AS Dimension

	

	// Only allow the assign if the control has not yet been created
	// and if it is not in imagelist mode.
	IF hWnd = NULL_PTR
		IF nImageCount != 0
			oBitmap  := oNewBitmap
			IF oBitmap = NULL_OBJECT //Enables imagelist mode
				nImageCount := 0
			ELSE
				oBMPSize := oBitmap:Size
				IF oButtonSize:Width = 0
					nImageCount := oBMPSize:Width / oBMPSize:Height
				ELSE
					nImageCount := oBMPSize:Width / oButtonSize:Width
				ENDIF
			ENDIF
		ELSE
			WCError{#Bitmap,#ToolBar,"Can not assign a Bitmap in imagelist mode!",oNewBitmap,1}:@@Throw()
		ENDIF
	ENDIF

	RETURN 


ASSIGN BorderStyle(kBorderStyle) 
	// For CA-Visual Objects 1.0 compatibility only
	

	RETURN 

ACCESS BoundingBox 
	LOCAL oBoundingBox AS BoundingBox

	

	IF hWnd != NULL_PTR
		oBoundingBox := BoundingBox{SELF:Origin, SELF:Size}
	ENDIF

	RETURN oBoundingBox
ACCESS ButtonCount            // dcaton 070215 changed from ACCESS ButtonCount(symTB)
   RETURN GetButtonCount()

METHOD GetButtonCount(symTB)  // dcaton 070215 changed from ACCESS to METHOD, Vulcan doesn't support CLIPPER-calling convention properties
	LOCAL hwndTB AS PTR
	LOCAL symToolBar AS SYMBOL

	

	IF (PCount() != 1) .OR. !IsSymbol(symTB)
		symToolBar := #MAINTOOLBAR
	ELSE
		symToolBar := symTB
	ENDIF

	hwndTB := SELF:__FindToolBarHandle(symToolBar)

	IF (hWndTB != NULL_PTR)
		RETURN SendMessage(hWndTB, TB_BUTTONCOUNT, 0, 0)
	ENDIF

	RETURN 0

ACCESS ButtonSize 
	

	RETURN oButtonSize

ASSIGN ButtonSize(oNewButtonSize) 
	

	// Only allow the assign if the control has not yet been created
	IF IsInstanceOfUsual(oNewButtonSize, #Dimension)
		IF hWnd = NULL_PTR .AND. ALen(SELF:aUpdates) > 0
			SELF:__TryDeferAction(#ButtonSize, MakeLong(oNewButtonSize:Width, oNewButtonSize:Height), NULL_SYMBOL)
		ELSE
			RETURN oButtonSize := oNewButtonSize
		ENDIF
	ENDIF

	RETURN 


ASSIGN ButtonStyle(kButtonStyle) 

	

	IF (kButtonStyle == TB_ICONONLY) .OR. (kButtonStyle == TB_TEXTONLY) .OR. (kButtonStyle == TB_TEXTANDICON)
		IF hWnd = NULL_PTR .AND. ALen(SELF:aUpdates) > 0
			SELF:__TryDeferAction(#ButtonStyle, kButtonStyle, NULL_SYMBOL)
		ELSE
			nButtonStyle := kButtonStyle
			IF (nButtonStyle == TB_TEXTONLY)
				IF oButtonSize:Height > 1
					oButtonSize:Width  := 20
					oButtonSize:Height := 1
				ENDIF
			ENDIF
		ENDIF
	ENDIF

	RETURN 

METHOD ChangeTipText(nID, cText, symLookUp) 
	//SE-060526       
	//RvdH 070206 Changerd to use new __VOToolBarTipText objects
	LOCAL dwIndex 	AS DWORD
	LOCAL oTipText AS __VOToolBarTipText
	

	Default(@symLookUp, #ButtonID)

	dwIndex := SELF:__FindTipText(nID, symLookUp)
	IF dwIndex > 0    
		oTipText := aTipsText[dwIndex] 
		oTipText:TipText := cText
	ENDIF
	RETURN dwIndex > 0

METHOD ClickItem(nMenuItemID, symTB) 
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF (hWndTB != NULL_PTR)
		SendMessage(hWndTB, TB_CHECKBUTTON, nMenuItemID, LONGINT(_CAST, TRUE))
	ELSE
		SELF:__TryDeferAction(#ClickItem, nMenuItemID, symTB)
	ENDIF

	RETURN NIL

ACCESS ClientArea 
	LOCAL oOwnerArea AS BoundingBox
	LOCAL oToolBarArea AS BoundingBox

	

	IF (hWnd != NULL_PTR) .AND. SELF:IsVisible()
		IF (SELF:Owner != NULL_OBJECT)
			oOwnerArea := SELF:Owner:CanvasArea
			oToolBarArea := SELF:BoundingBox
			IF (WCGetCoordinateSystem() == WCCartesianCoordinates)
				oOwnerArea:Height -= oToolBarArea:Height
			ELSE
				oOwnerArea:Top += oToolBarArea:Height
			ENDIF
		ENDIF
		RETURN oOwnerArea
	ENDIF

	RETURN SELF:Owner:CanvasArea

METHOD Configure() 

	

	/*
	// !!! we need more info to support configure !!!
	if (hWndTB != NULL_PTR)
	SendMessage(hWndTB, TB_CUSTOMIZE, 0, 0)
	endif
	*/

	RETURN NIL

METHOD Create() 
	//PP-040505 Update from S Ebert
	//SE-050929
	//RvdH 070206 Changed to use __VOToolBarUpdate class
	LOCAL dwCount 		AS DWORD
	LOCAL dwUpdateCount 	AS DWORD
	LOCAL oUpdate		AS __VOToolBarUpdate
	LOCAL rbBand 		IS _winREBARBANDINFO
	LOCAL sSize  		IS _WinSize
	LOCAL dwRebarStyle AS DWORD

	IF (hWnd == NULL_PTR)
		IF (WCGetCoordinateSystem() == WCCartesianCoordinates)
			SELF:Origin:Y := SELF:Origin:Y + SELF:Size:Height
		ENDIF

		SELF:SetStyle(TBSTYLE_FLAT, lFlat)

		//SE-070427
		IF (dwUpdateCount := ALen(SELF:aUpdates)) > 0  //only if MED menu exists
			IF nImageCount = -1l //Undefined mode
				//Setting of bitmap mode and assigning the default bitmap
				oBitmap := Bitmap{ResourceID{"IDB_DEFTOOLBAR", _GetInst()}, BMP_3DTRANSPARENT}
				nImageCount := 132
			ENDIF
		ENDIF
		
		IF SELF:__IsRebar
			SELF:SetStyle(_OR(CCS_NORESIZE, CCS_NOPARENTALIGN, CCS_NODIVIDER))

			IF IsThemeEnabled()
				SELF:SetStyle(TBSTYLE_TRANSPARENT)
			ENDIF

			dwRebarStyle := _OR(WS_VISIBLE, WS_CHILD, WS_CLIPCHILDREN, WS_CLIPSIBLINGS, RBS_VARHEIGHT, RBS_BANDBORDERS)
			IF ! SELF:Divider
				dwRebarStyle := _OR(LONGINT(dwRebarStyle), CCS_NODIVIDER)
			ENDIF

			hWnd := CreateWindowEx(WS_EX_TOOLWINDOW, PSZ(_CAST, "ReBarWindow32"), PSZ(_CAST, "Toolbar"),;
				dwRebarStyle, 0, 0, 400, 30, SELF:Owner:Handle(), PTR(_CAST, 13001), _GetInst(), 0)

			IF hWnd != NULL_PTR
				__WCRegisterControl(SELF) //register after we get the handle
				//SE-050729
				IF ALen(SELF:aUpdates) > 0
					hwndMainTB := SELF:__CreateToolBar(#MAINTOOLBAR, hWnd, DWORD(wID), dwStyle)
				ENDIF
			ENDIF
		ELSE
			SELF:SetStyle(CCS_NODIVIDER, ! SELF:Divider)
			hwndMainTB := SELF:__CreateToolBar(#MAINTOOLBAR, SELF:Owner:Handle(), DWORD(wID), dwStyle)
			//PP-031115
			hWnd       := hWndMainTB
		ENDIF

		//SendMessage(hwndMainTB, TB_SETBUTTONSIZE, 0, MAKELONG(word(oButtonSize:Width), word(oButtonSize:Height)+100))
		//SendMessage(hwndMainTB, TB_SETBITMAPSIZE, 0, MAKELONG(word(oButtonSize:Width), word(oButtonSize:Height)+100))
		IF hwnd != NULL_PTR
			__lpfnDefaultProc := GetWindowLong(hWnd, GWL_WNDPROC)
			SetWindowLong(hWnd, GWL_WNDPROC, LONGINT(_CAST, Get__WCControlProcPtr()))   // dcaton 070319 use helper to get ptr

			oSize   := NULL_OBJECT
			oOrigin := NULL_OBJECT

			IF SELF:__IsRebar
				IF (oBandImageList != NULL_OBJECT)
					// this does the SendMessage since now we have a handle
					SELF:BandImageList := oBandImageList
				ENDIF
				IF hWndMainTB != NULL_PTR
					MemSet(@rbBand, 0, _SIZEOF(_winREBARBANDINFO))
					rbBand:cbSize    := _SIZEOF(_winREBARBANDINFO) //COMPAT_REBARBANDINFO_SIZE
					rbBand:fMask     := _OR(RBBIM_CHILD, RBBIM_STYLE, RBBIM_COLORS, RBBIM_ID)
					rbBand:clrFore   := GetSysColor(COLOR_BTNTEXT)
					rbBand:clrBack   := GetSysColor(COLOR_BTNFACE)
					rbBand:fStyle    := _OR(RBBS_NOVERT, RBBS_CHILDEDGE, RBBS_CHILDEDGE)
					rbBand:hwndChild := hWndMainTB
					rbBand:wID       := 1042

					// Insert band into rebar
					SendMessage(hWnd, RB_INSERTBAND, DWORD(_CAST, -1L), LONGINT(_CAST, @rbBand))
				ENDIF
			ENDIF

				IF dwUpdateCount > 0
					
					// Apply all buffered changes and reset dwUpdateCount
					FOR dwCount := 1 UPTO dwUpdateCount 
						oUpdate := SELF:aUpdates[dwCount]
						DO CASE
						CASE oUpdate:symAction == #AppendItem
							// RvdH 031015 Changed oBitMap to ptrBitMap
							//Send(SELF, #AppendItem, oUpdate:nButtonID, oUpdate:nMenuItemID, oUpdate:oBitmap, oUpdate:nPosition, Psz2String(oUpdate:pszButtonText), oUpdate:nImageCount, oUpdate:bState, oUpdate:bStyle, oUpdate:symToolBar)
							SELF:AppendItem(oUpdate:nButtonID, oUpdate:nMenuItemID, oUpdate:oBitmap, oUpdate:nPosition, ;
								IIF(oUpdate:nBeforeID = 1, oUpdate:cButtonText, NIL), oUpdate:nImageCount, oUpdate:bState, ;
								oUpdate:bStyle, oUpdate:symToolBar)

						CASE oUpdate:symAction == #InsertItem
							SELF:InsertItem(oUpdate:nButtonID, oUpdate:nMenuItemID, oUpdate:nBeforeID,;
								oUpdate:bState, oUpdate:bStyle, oUpdate:symToolBar)

						CASE oUpdate:symAction == #Rows
							// SELF:Rows[ oUpdate:symToolBar] := oUpdate:nMenuItemID   dcaton 070316 changed to call new SetRows() method 
							SELF:SetRows( oUpdate:nMenuItemID, oUpdate:symToolBar )

						CASE oUpdate:symAction == #AddSubToolBarBand
							SELF:AddSubToolBarBand( oUpdate:symToolBar, oUpdate:nMenuItemID, oUpdate:nButtonID, ;
								IIF(oUpdate:bState = 0, LOGIC(_CAST, oUpdate:nPosition), oUpdate:nPosition))

						CASE oUpdate:symAction == #ButtonSize
							oButtonSize:Width  := LoWord(DWORD(oUpdate:nMenuItemID))
							oButtonSize:Height := HiWord(DWORD(oUpdate:nMenuItemID))

						CASE oUpdate:symAction == #ButtonStyle
							SELF:ButtonStyle := oUpdate:nMenuItemID

						OTHERWISE
							IF IsMethod(SELF, oUpdate:symAction)
								Send(SELF, oUpdate:symAction, oUpdate:nMenuItemID, oUpdate:symToolBar)
							ELSEIF IsMethod(SELF, #DoAction)
								Send(SELF, #DoAction, oUpdate)
							ENDIF
						END CASE
				NEXT

				// Free the Array
				SELF:aUpdates := NULL_ARRAY
			ENDIF

			IF SELF:__IsRebar
				dwcount := DWORD(_CAST,SendMessage(hWnd, RB_GETBANDCOUNT, 0, 0))

				IF hWndMainTB != NULL_PTR
					rbBand:fMask      := RBBIM_CHILDSIZE
					SendMessage(hwndMainTB, TB_GETMAXSIZE, 0l, LONGINT(_CAST,@sSize))
					rbBand:cxMinChild := DWORD(sSize:cx)
					rbBand:cyMinChild := DWORD(sSize:cy + IIF(lFlat,2, 6))

					//rbBand.cxMinChild := LoWord(DWORD(_CAST, SendMessage(hwndMainTB, TB_GETBUTTONSIZE, 0, 0))) * SELF:ButtonCount + 26
					//rbBand.cyMinChild := HiWord(DWORD(_CAST, SendMessage(hwndMainTB, TB_GETBUTTONSIZE, 0, 0))) + 6

					// ??? needed ???
					//if (nButtonStyle == TB_TEXTANDICON)
					// rbBand.cyMinChild += 10
					//endif
					SendMessage(hWnd, RB_SETBANDINFO, 0U, LONGINT(_CAST, @rbBand))

					//delete the following 3 code lines, if MED band order is corrected.
					IF dwCount > 1
						SendMessage(hWnd, RB_MOVEBAND, 0, LONGINT(_CAST, dwCount) - 1l)
					ENDIF
				ENDIF

				IF dwCount > 1
					SendMessage(hWnd, RB_MAXIMIZEBAND, dwCount-1, 0)
				ENDIF
			ENDIF
		ENDIF
	ENDIF

	RETURN hWnd

METHOD DeleteItem(nMenuItemID, symTB) 
	LOCAL liIndex AS LONGINT
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF hWndTB != NULL_PTR
		// Get the index number from nMenuItemID
		//PP-030910 Bug 178
		IF nMenuItemID <= 0 //If nMenuItemID is <= 0 nMenuItemID means the zero based buttonindex
			liIndex := -1 * nMenuItemID
		ELSE
			liIndex := SendMessage(hWndTB, TB_COMMANDTOINDEX, DWORD(_CAST, nMenuItemID), 0)
		ENDIF
		IF liIndex >= 0
			SendMessage(hWndTB, TB_DELETEBUTTON, DWORD(liIndex), 0)
		ENDIF
	ELSE
		//PP-030909 Bug BF 030130. Symbol spelled incorrectly.
		SELF:__TryDeferAction(#DeleteItem, nMenuItemID, symTB)
	ENDIF

	RETURN NIL

METHOD Destroy() 
	//PP-040417 from S Ebert
	//SE-050729
	//RvdH 0702056 Changed to use __VOToolBarChild
	LOCAL dwI, dwCount AS DWORD
	LOCAL hWndTB 		AS PTR
	LOCAL oChild		AS __VOToolBarChild

	
	// RvdH-030323 Add check for empty toolbar

	//PP-031115
	IF SELF:__IsRebar
		//PP-040417 from S Ebert
		dwCount := ALen(aChildren)
		FOR dwI := 1 UPTO dwCount
			oChild := aChildren[dwI]
			hWndTB := oChild:Handle
			//SE-070427
			IF oChild:oCargo = NULL_OBJECT .AND. hWndTB != NULL_PTR
				__WCUnRegisterControl(hWndTB)
			ENDIF
		NEXT //dwI
	ENDIF

	IF !InCollect()
		oBitmap 			:= NULL_OBJECT
		aExtraBitmaps 	:= NULL_ARRAY
		aTipsText 		:= NULL_ARRAY
		aUpdates			:= NULL_ARRAY
		aChildren 		:= NULL_ARRAY
		oBandImageList := NULL_OBJECT
	ENDIF

	SUPER:Destroy()

	RETURN NIL

METHOD DimItem(nMenuItemID, symTB) 
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF hWndTB != NULL_PTR
		SendMessage(hWndTB, TB_INDETERMINATE, nMenuItemID, LONGINT(_CAST, TRUE))
	ELSE
		SELF:__TryDeferAction(#DimItem, nMenuItemID, symTB)
	ENDIF

	RETURN NIL

METHOD DisableItem(nMenuItemID, symTB) 
	//PP-040421 Update from S Ebert
	//SE-060520
	//RvdH 0702056 Changed to use __VOToolBarChild
	LOCAL hWndTB  AS PTR
	LOCAL i, dwLen AS DWORD
	LOCAL oChild	AS __VOToolBarChild

	

	IF IsSymbol(symTB) .AND. symTB != NULL_SYMBOL
		hWndTB := SELF:__FindToolBarHandle(symTB)

		IF hWndTB != NULL_PTR
			SendMessage(hWndTB, TB_ENABLEBUTTON, nMenuItemID, 0)
		ELSE
			SELF:__TryDeferAction(#DisableItem, nMenuItemID, symTB)
		ENDIF
	ELSE
		IF hWnd != NULL_PTR
			dwLen := ALen(aChildren)
			FOR i := 1 UPTO dwLen  
				oChild :=aChildren[i] 
				//SE-070427
				IF oChild:Handle != NULL_PTR .AND. oChild:oCargo = NULL_OBJECT
					SendMessage(oChild:Handle, TB_ENABLEBUTTON, nMenuItemID, 0)
				ENDIF
			NEXT
		ELSE
			SELF:__TryDeferAction(#DisableItem, nMenuItemID, NULL_SYMBOL)
		ENDIF
	ENDIF

	RETURN NIL

METHOD EnableBands(lEnable) 

	Default(@lEnable, TRUE)
	lOldStyle := !lEnable

	RETURN lEnable


METHOD EnableDrag(lEnable) 
	// Defer while feature is unavailable
	

	RETURN NIL

METHOD EnableItem(nMenuItemID, symTB) 
	//PP-040421 Update from S Ebert
	//SE-060520
	//RvdH 0702056 Changed to use __VOToolBarChild
	LOCAL hWndTB  AS PTR
	LOCAL i, dwLen AS DWORD
	LOCAL oChild	AS __VOToolBarChild

	IF IsSymbol(symTB) .AND. symTB != NULL_SYMBOL
		hWndTB := SELF:__FindToolBarHandle(symTB)

		IF hWndTB != NULL_PTR
			SendMessage(hWndTB, TB_ENABLEBUTTON, nMenuItemID, 0xFFFFFFFFL)
		ELSE
			SELF:__TryDeferAction(#EnableItem, nMenuItemID, symTB)
		ENDIF
	ELSE
		IF hWnd != NULL_PTR
			dwLen :=  ALen(aChildren)
			FOR i := 1 UPTO dwLen
				oChild := aChildren[i]
				//SE-070427
				IF oChild:Handle != NULL_PTR .AND. oChild:oCargo = NULL_OBJECT
					SendMessage(oChild:Handle, TB_ENABLEBUTTON, nMenuItemID,0xFFFFFFFFL)
				ENDIF
			NEXT
		ELSE
			SELF:__TryDeferAction(#EnableItem, nMenuItemID, NULL_SYMBOL)
		ENDIF
	ENDIF

	RETURN NIL

ACCESS Flat 
	

	RETURN lFlat

ASSIGN Flat(lNewVal) 
	

	lFlat := lNewVal

	IF IsWindow(hWndMainTB)
		IF lFlat
			//SetClassLong(GetParent(hWndMainTB), GCL_HBRBACKGROUND, COLOR_3DFACE+1)
			SetWindowLong(hWndMainTB, GWL_STYLE, _OR(GetWindowLong(hWndMainTB, GWL_STYLE), TBSTYLE_FLAT))
		ELSE
			SetWindowLong(hwndMainTB, GWL_STYLE, _AND(GetWindowLong(hWndMainTB, GWL_STYLE), _NOT(LONGINT(TBSTYLE_FLAT))))
		ENDIF
	ENDIF

	RETURN 

ASSIGN GapSize(nGapSize) 
	// For CA-Visual Objects 1.0 compatibility only
	

	RETURN 0

METHOD GetButtonDescription(nButtonID, symTB) 
	//SE-041015 Fix from S Ebert
	LOCAL pszText AS PSZ
	LOCAL cDescription AS STRING
	LOCAL hwndTB AS PTR
	LOCAL liLength AS LONGINT

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF (hWndTB != NULL_PTR)
		liLength := SendMessage(hWndTB, TB_GETBUTTONTEXT, DWORD(nButtonID), 0l)
		IF liLength > 0
			pszText := MemAlloc(DWORD(liLength + 1))
			SendMessage(hWndTB, TB_GETBUTTONTEXT, DWORD(nButtonID), LONGINT(_CAST, pszText))
			cDescription := Psz2String(pszText)
			MemFree(pszText)
		ENDIF
	ENDIF

	RETURN AllTrim(cDescription)

METHOD GetImageList(symType, symTB) 
	//PP-040417 from S Ebert
	//RvdH 0702056 Changed to use __VOToolBarChild
	LOCAL oTB	  	  AS __VOToolBarChild
	LOCAL oImageList AS ImageList

	Default(@symTB, #MAINTOOLBAR)
	oTb := SELF:__FindToolBar(symTB)
	IF oTB != NULL_OBJECT
		oImagelist	:= oTB:ImageList
		IF IsSymbol(symType)
			IF symType = #HOTIMAGELIST
				oImagelist	:= oTB:HotImageList
			ELSEIF symType = #DISABLEDIMAGELIST
				oImagelist := oTB:DisabledImageList
			ENDIF
		ENDIF
	ENDIF

	RETURN oImageList

METHOD GetState(nMenuItemID, symTB) 
	//SE-050701
	LOCAL hwndTB AS PTR
	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF hWndTB != NULL_PTR
		RETURN SendMessage(hWndTB, TB_GETSTATE, nMenuItemID, 0)
	ENDIF

	RETURN -1l


METHOD GetTipText(nButtonID, symLookUp) 
	//SE-060526       
	//RvdH 070206 Changerd to use new __VOToolBarTipText objects
	LOCAL dwIndex 	AS DWORD
	LOCAL oTipText AS __VOToolBarTipText
	LOCAL cResult	AS STRING
	

	Default(@symLookUp, #ButtonID)

	dwIndex := SELF:__FindTipText(nButtonID, symLookUp)
	IF dwIndex > 0    
		oTipText := aTipsText[dwIndex] 
		cResult := oTipText:TipText 
	ENDIF

	RETURN cResult

METHOD Hide() 

	

	SUPER:Hide()

	IF IsInstanceOf(SELF:Owner, #ShellWindow)
		SELF:Owner:__AdjustClient()
	ELSEIF IsInstanceOf(SELF:Owner, #DataWindow)
		SELF:Owner:__AdjustForm()
	ENDIF

	RETURN NIL

METHOD HideItem(nMenuItemID, symTB) 
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF hWndTB != NULL_PTR
		SendMessage(hWndTB, TB_HIDEBUTTON, nMenuItemID, LONGINT(_CAST, TRUE))
	ELSE
		SELF:__TryDeferAction(#HideItem, nMenuItemID, symTB)
	ENDIF

	RETURN NIL

ACCESS ImageCount 
	//SE-050729
	

	IF nImageCount < 0
		RETURN 0
	ENDIF

	RETURN nImageCount


ASSIGN ImageCount(nNewImageCount) 
	//SE-050729
	

	// Only allow the assign if the control has not yet been created
	IF hWnd = NULL_PTR .AND. nNewImageCount > 0 .AND. nImageCount != 0
		RETURN nImageCount := nNewImageCount
	ENDIF

	RETURN 

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lEnableBands) 
	//SE-050729
	Default(@xID, 0)
	Default(@oPoint, Point{})
	Default(@oDimension, Dimension{})
	Default(@lEnableBands, TRUE)

	IF IsNil(oOwner)
		// ToolBar is being created without a parent; don't call super:Init()
		RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition 
		oOrigin 	:= Point{oPoint:X, oPoint:Y}
		oSize 	:= Dimension{oDimension:Width, oDimension:Height}
		SELF:__ClassName := TOOLBARCLASSNAME
	ELSE
		// ToolBar is being created with a parent, calling super:Init() is safe
		SUPER(oOwner, xID, oPoint, oDimension, TOOLBARCLASSNAME)
	ENDIF

	SELF:SetStyle(_OR(WS_CHILD, WS_CLIPSIBLINGS, WS_VISIBLE, TBSTYLE_WRAPABLE, TBSTYLE_TOOLTIPS))

	// Set up default configuration
	oButtonSize := Dimension{16, 16}
	//Moved to Create() SE-050729
	//oBitmap := Bitmap{ResourceID{"IDB_DEFTOOLBAR", _GetInst()}, BMP_3DTRANSPARENT}
	//nImageCount := 132
	nImageCount := -1l //Unintialized mode
	// Three image modes exist
	// nImageCount = -1l Unintialized.
	// nImageCount =  0l Toolbar uses only Imagelists, using of Bitmaps is impossible.
	// nImageCount >  0l Toolbar uses only Bitmaps, using of Imagelists is impossible.

	//PP-030910
	//PP-040515
	SELF:Divider := ! lEnableBands

	// Initialize update structure array
	//ptrUpdateArray := MemCAlloc(16, _SizeOf(__WCToolBarUpdate))
	//dwUpdateCount := 0
	aUpdates	:= {}
	lOldStyle := !lEnableBands

	SELF:ButtonStyle := TB_ICONONLY
	aTipsText := {}
	aExtraBitmaps := {}
	aChildren := {}
	aBackBitmaps := {}

	RETURN 

METHOD InsertItem(nButtonID, nMenuItemID, nBeforeID, bState, bStyle, symTB) 
	//PP-040421 Update from S Ebert
	//RvdH 070206 Changed to use __VOToolBarUpdate class
	LOCAL strucButton IS _winTBBUTTON
	LOCAL oUpdate 		AS __VOToolBarUpdate
	LOCAL hWndTB 		AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	Default(@nBeforeID, -1)

	IF (hWndTB != NULL_PTR)
		IF IsLong(nButtonID)
			// Convert to zero-base
			IF (nBeforeID != -1)
				nBeforeID -= 1
			ENDIF
			IF (nButtonID == IDT_SEPARATOR)
				// Separator
				strucButton:iBitmap := 0
				strucButton:idCommand := 0
				strucButton:fsStyle := TBSTYLE_SEP
			ELSEIF IsLong(nMenuItemID)
				// Button
				strucButton:iBitmap := nButtonID - 1
				strucButton:idCommand := nMenuItemID
				strucButton:fsState := TBSTATE_ENABLED
				strucButton:fsStyle := TBSTYLE_BUTTON

				IF IsNumeric(bState)
					strucButton:fsState := bState
				ENDIF
				IF IsNumeric(bStyle)
					strucButton:fsStyle := bStyle
				ENDIF
			ENDIF
			strucButton:dwData := 0
			strucButton:iString := 0
		ENDIF

		RETURN LOGIC(_CAST, SendMessage(hWndTB, TB_INSERTBUTTON, nBeforeID, LONGINT(_CAST, @strucButton)))
	ELSEIF (SELF:aUpdates != NULL_ARRAY)
		// If the toolbar has not yet been created, fill out the buffer information
		// necessary to apply this operation when it is created
		IF ! IsNumeric(nMenuItemID)
			nMenuItemID := 0
		ENDIF
		oUpdate := SELF:__TryDeferAction(#InsertItem, nMenuItemID, symTB)

		Default(@bState, TBSTATE_ENABLED)
		Default(@bStyle, TBSTYLE_BUTTON)

		oUpdate:nButtonID := nButtonID
		oUpdate:nBeforeID := nBeforeID
		oUpdate:bState    := bState
		oUpdate:bStyle    := bStyle
	ENDIF

	RETURN TRUE

METHOD IsClicked(nID, symIDType, symTB) 
	LOCAL strucButton IS _winTBButton
	LOCAL dwID AS DWORD
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)
	Default(@symIDType, #ButtonID)

	IF (hWndTB != NULL_PTR)
		IF (symIDType == #MenuItemID)
			// Use the menu item ID
			dwID := nID
		ELSE
			// Get the menu item ID from the button ID
			SendMessage(hWndTB, TB_GETBUTTON, nID - 1, LONGINT(_CAST, @strucButton))
			dwID := DWORD(strucButton:idCommand)
		ENDIF
		RETURN LOGIC(_CAST, SendMessage(hWndTB, TB_ISBUTTONCHECKED, dwID, 0))
	ENDIF

	RETURN FALSE

METHOD IsDimmed(nID, symIDType, symTB) 
	LOCAL strucButton IS _winTBButton
	LOCAL dwID AS DWORD
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)
	Default(@symIDType, #ButtonID)

	IF (hWndTB != NULL_PTR)
		IF (symIDType == #MenuItemID)
			// Use the menu item ID
			dwID := nID
		ELSE
			// Get the menu item ID from the button ID
			SendMessage(hWndTB, TB_GETBUTTON, nID - 1, LONGINT(_CAST, @strucButton))
			dwID := DWORD(strucButton:idCommand)
		ENDIF
		RETURN LOGIC(_CAST, SendMessage(hWndTB, TB_ISBUTTONINDETERMINATE, dwID, 0))
	ENDIF

	RETURN FALSE

METHOD IsEnabled(nID, symIDType, symTB) 
	LOCAL strucButton IS _winTBButton
	LOCAL dwID AS DWORD
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)
	Default(@symIDType, #ButtonID)

	IF (hWndTB != NULL_PTR)
		IF (symIDType == #MenuItemID)
			// Use the menu item ID
			dwID := nID
		ELSE
			// Get the menu item ID from the button ID
			SendMessage(hWndTB, TB_GETBUTTON, nID - 1, LONGINT(_CAST, @strucButton))
			dwID := DWORD(strucButton:idCommand)
		ENDIF
		RETURN LOGIC(_CAST, SendMessage(hWndTB, TB_ISBUTTONENABLED, dwID, 0))
	ENDIF

	RETURN FALSE

METHOD IsHidden(nID, symIDType, symTB) 
	LOCAL strucButton IS _winTBButton
	LOCAL dwID AS DWORD
	LOCAL hwndTB AS PTR

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)
	Default(@symIDType, #ButtonID)

	IF (hWndTB != NULL_PTR)
		IF (symIDType == #MenuItemID)
			// Use the menu item ID
			dwID := nID
		ELSE
			// Get the menu item ID from the button ID
			SendMessage(hWndTB, TB_GETBUTTON, nID - 1, LONGINT(_CAST, @strucButton))
			dwID := DWORD(strucButton:idCommand)
		ENDIF
		RETURN LOGIC(_CAST, SendMessage(hWndTB, TB_ISBUTTONHIDDEN, dwID, 0))
	ENDIF

	RETURN FALSE

METHOD IsPressed(nID, symIDType, symTB) 
	LOCAL strucButton IS _winTBButton
	LOCAL dwID AS DWORD
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)
	Default(@symIDType, #ButtonID)

	IF (hWndTB != NULL_PTR)
		IF (symIDType == #MenuItemID)
			// Use the menu item ID
			dwID := nID
		ELSE
			// Get the menu item ID from the button ID
			SendMessage(hWndTB, TB_GETBUTTON, nID - 1, LONGINT(_CAST, @strucButton))
			dwID := DWORD(strucButton:idCommand)
		ENDIF
		RETURN LOGIC(_CAST, SendMessage(hWndTB, TB_ISBUTTONPRESSED, dwID, 0))
	ENDIF

	RETURN FALSE

ASSIGN Location(kLocation) 
	// Defer while feature is unavailable
	

	RETURN 

METHOD PressItem(nMenuItemID, symTB) 
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF (hWndTB != NULL_PTR)
		SendMessage(hWndTB, TB_PRESSBUTTON, nMenuItemID, LONGINT(_CAST, TRUE))
	ELSE
		SELF:__TryDeferAction(#PressItem, nMenuItemID, symTB)
	ENDIF

	RETURN NIL

METHOD RemoveTipText(nButtonID, symLookUp) 
	//SE-060526      
	LOCAL dwIndex 	AS DWORD
	

	Default(@symLookUp, #ButtonID)

	dwIndex := SELF:__FindTipText(nButtonID, symLookUp)
	IF dwIndex > 0    
		ADel(aTipsText, dwIndex)
		ASize(aTipsText, ALen(aTipsText) - 1)
	ENDIF

	RETURN dwIndex > 0
ACCESS Rows             // dcaton 070215 was ACCESS Rows(nRows)
   RETURN GetRows()	

METHOD GetRows(symTB)   // dcaton changed from ACCESS to METHOD, Vulcan doesn't support CLIPPER-calling convention properties
	LOCAL hwndTB AS PTR
	LOCAL symToolBar AS SYMBOL

	

	IF (PCount() != 1) .OR. !IsSymbol(symTB)
		symToolBar := #MAINTOOLBAR
	ELSE
		symToolBar := symTB
	ENDIF

	hwndTB := SELF:__FindToolBarHandle(symToolBar)

	IF (hWndTB != NULL_PTR)
		RETURN SendMessage(hWndTB, TB_GETROWS, 0, 0)
	ENDIF

	RETURN 0

ASSIGN Rows( nRows )	         // dcaton 070215 was ASSIGN Rows(nRows,symTB)
   RETURN SetRows( nRows )

METHOD SetRows(nRows, symTB)   // dcaton 070215 changed from ASSIGN to METHOD, Vulcan doesn't support CLIPPER-calling convention properties
	//PP-040421 Update from S Ebert
	LOCAL strucRect IS _winRect
	LOCAL hwndTB AS PTR
	LOCAL symToolBar AS SYMBOL

	

	IF (PCount() != 2) .OR. !IsSymbol(symTB)
		symToolBar := #MAINTOOLBAR
	ELSE
		symToolBar := symTB
	ENDIF

	hwndTB := SELF:__FindToolBarHandle(symToolBar)

	IF (hWndTB != NULL_PTR)
		SendMessage(hWndTB, TB_SETROWS, MAKEWPARAM(WORD(_CAST, nRows), WORD(_CAST, TRUE)), LONGINT(_CAST, @strucRect))
	ELSE
		SELF:__TryDeferAction(#Rows, 0, symToolBar)
	ENDIF

	RETURN nRows


ASSIGN SeparatorSize(nSeparatorSize) 
	// For CA-Visual Objects 1.0 compatibility only
	

	RETURN 0

METHOD SetImageList(uImageList, symType, symTB) 
	//PP-040417 from S Ebert
	//SE-050729
	//RvdH 0702056 Changed to use __VOToolBarChild
	LOCAL oTB     	  AS __VOToolBarChild
	LOCAL dwMsg      AS DWORD
	LOCAL oImageList AS ImageList
	LOCAL oBmp       AS Bitmap
	LOCAL oBMPSize   AS Dimension
	LOCAL dwCount    AS DWORD

	//SE-050729
	IF nImageCount < 1

		nImageCount := 0

		IF IsInstanceOfUsual(uImageList, #Bitmap)
			oBmp     := uImageList
			oBMPSize := oBmp:Size
			dwCount  := DWORD(oBMPSize:Width)
			IF oButtonSize:Width = 0
				oBMPSize:Width := oBMPSize:Height
			ELSE
				oBMPSize:Width := oButtonSize:Width
			ENDIF
			dwCount /= DWORD(oBMPSize:Width)

			oImageList := ImageList{dwCount, oButtonSize, NIL, _OR(ILC_COLOR24, ILC_MASK), 1}
			IF oImageList:Handle() != NULL_PTR
				oImageList:AddMask(oBmp, Color{192, 192, 192})
			ELSE
				oImageList := NULL_OBJECT
			ENDIF
		ELSE
			oImageList := uImageList
		ENDIF

		Default(@symTB, #MAINTOOLBAR)
		oTB := SELF:__FindToolBar(symTB)
		IF oTB == NULL_OBJECT  
			oTB	:= __VOToolBarChild{}
			oTB:NameSym := symTB
			AAdd(aChildren, oTB)
		ENDIF

		dwMsg  := TB_SETIMAGELIST
		IF IsSymbol(symType)
			IF symType = #HOTIMAGELIST
				oTB:HotImageList := oImagelist
				dwMsg  := TB_SETHOTIMAGELIST
			ELSEIF symType = #DisabledImageList
				oTB:DisabledImageList := oImagelist
				dwMsg  := TB_SETDISABLEDIMAGELIST
			ELSE
				oTB:ImageList := oImagelist				
			ENDIF
		ELSE
			oTB:ImageList := oImagelist							
		ENDIF

		IF oTB:Handle != NULL_PTR .AND. oImagelist != NULL_OBJECT
			SendMessage(oTB:Handle, dwMsg, 0u, LONGINT(_CAST, oImagelist:Handle()))
		ENDIF
	ELSE
		WCError{#SetImageList,#ToolBar,"Can not assign an ImageList in bitmap mode!",uImageList,1}:@@Throw()
	ENDIF

	RETURN oImageList


METHOD SetState(nMenuItemID, nState, symTB) 
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF (hWndTB != NULL_PTR)
		SendMessage(hWndTB, TB_SETSTATE, nMenuItemID, nState)
	ENDIF

	RETURN NIL

METHOD Show() 
	

	SUPER:Show()

	IF IsInstanceOf(SELF:Owner, #ShellWindow)
		SELF:Owner:__AdjustClient()
	ELSEIF IsInstanceOf(SELF:Owner, #DATAWINDOW)
		SELF:Owner:__AdjustForm()
	ENDIF

	RETURN NIL

METHOD ShowBand(iPos, lShow) 
	

	IF !SELF:__IsRebar
		RETURN FALSE
	ENDIF

	Default(@lShow, TRUE)
	RETURN (SendMessage(hWnd, RB_SHOWBAND, DWORD(_CAST, iPos), LONGINT(_CAST, lShow)) > 0)

METHOD ShowItem(nMenuItemID, symTB) 
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF (hWndTB != NULL_PTR)
		SendMessage(hWndTB, TB_HIDEBUTTON, nMenuItemID, LONGINT(_CAST, FALSE))
	ELSE
		SELF:__TryDeferAction(#ShowItem, nMenuItemID, symTB)
	ENDIF

	RETURN NIL

METHOD UnClickItem(nMenuItemID, symTB) 
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF (hWndTB != NULL_PTR)
		SendMessage(hWndTB, TB_CHECKBUTTON, nMenuItemID, LONGINT(_CAST, FALSE))
	ELSE
		SELF:__TryDeferAction(#UnClickItem, nMenuItemID, symTB)
	ENDIF

	RETURN NIL

METHOD UnDimItem(nMenuItemID, symTB) 
	//PP-040421 Update from S Ebert
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF (hWndTB != NULL_PTR)
		SendMessage(hWndTB, TB_INDETERMINATE, nMenuItemID, LONGINT(_CAST, FALSE))
	ELSE
		SELF:__TryDeferAction(#UnDimItem, nMenuItemID, symTB)
	ENDIF

	RETURN NIL

METHOD UnPressItem(nMenuItemID, symTB) 
	LOCAL hwndTB AS PTR

	

	Default(@symTB, #MAINTOOLBAR)
	hwndTB := SELF:__FindToolBarHandle(symTB)

	IF (hWndTB != NULL_PTR)
		SendMessage(hWndTB, TB_PRESSBUTTON, nMenuItemID, LONGINT(_CAST, FALSE))
	ELSE
		SELF:__TryDeferAction(#UnClickItem, nMenuItemID, symTB)
	ENDIF

	RETURN NIL

METHOD Update() 
	// Dummy - for CA-Visual Objects 1.0 compatibility only
	

	RETURN NIL

END CLASS



#region defines
DEFINE I_IMAGENONE := (-2)
#endregion
