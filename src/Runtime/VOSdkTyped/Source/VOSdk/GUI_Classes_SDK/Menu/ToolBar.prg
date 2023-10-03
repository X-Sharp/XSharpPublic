//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



USING System.Collections.Generic
/// <include file="Gui.xml" path="doc/ToolBar/*" />
CLASS ToolBar INHERIT Control
    // Todo Implement CoolBar Toolbar
	PROTECT oBitmap 		AS Bitmap
	PROTECT aExtraBitmaps	AS List<ToolBarExtraBitmap>
	PROTECT nButtonStyle 	AS DWORD
	PROTECT nImageCount 	AS INT
	PROTECT oButtonSize 	AS Dimension
	PROTECT aUpdates 		AS List<ToolbarUpdate>
	//PROTECT dwBufferSize 	AS DWORD      //SE-070427 unused, can be deleted in the future
	PROTECT aTipsText		AS List<ToolbarTipText>
	PROTECT lFlat 			AS LOGIC
	PROTECT hwndMainTB 		AS VOToolBar
	PROTECT aChildren		AS List<ToolbarChild>
	PROTECT oBandImageList 	AS ImageList
	PROTECT lOldStyle 		AS LOGIC
	PROTECT aBackBitmaps[0]	AS ARRAY
	EXPORT Divider 			AS LOGIC

    /// <exclude />
	PROPERTY __HasUpdates AS LOGIC GET aUpdates != NULL .and. aUpdates:Count > 0

 /// <exclude />
	ACCESS __ButtonStyle AS DWORD STRICT
		RETURN nButtonStyle

    /// <exclude />
    PROPERTY ControlType  AS ControlType  GET ControlType.ToolBar

	#region New Methods for Windows.Forms
    /// <exclude />
	METHOD OnControlCreated(oC AS IVOControl) AS VOID
		VAR oControl := (VOToolBar) oC
		oControl:ShowToolTips := TRUE
		oControl:Wrappable := FALSE
		oControl:ButtonClick += ButtonClick
		RETURN


    /// <exclude />
	VIRTUAL METHOD ButtonClick (Sender AS OBJECT, e AS System.Windows.Forms.ToolBarButtonClickEventArgs) AS VOID
		LOCAL nID AS LONG
		LOCAL oWin AS Window
		LOCAL oEvt AS MenuCommandEvent
		IF e:Button != NULL_OBJECT .and. e:Button:GetType() == typeof(VOToolBarButton)
			nID := ((VOToolBarButton) e:Button):MenuID
			oWin := (Window) SELF:Owner
			IF oWin:Menu != NULL_OBJECT
				oEvt := MenuCommandEvent{oWin:Menu,oWin, nID}
				oWin:__PreMenuCommand(oEvt)
			ENDIF
		endif
		RETURN

    /// <exclude />
	METHOD __CleanText(cText AS STRING) AS STRING
		IF cText:IndexOf('\t') > 0
			cText := Left(cText, (DWORD) cText:IndexOf('\t') )
		ENDIF
		IF cText:Contains("&")
			cText := cText:Replace("&","")
		ENDIF
		RETURN cText

    /// <exclude />
	METHOD __CreateButton(cCaption AS STRING, nID AS LONG) AS System.Windows.Forms.ToolBarButton
		LOCAL oButton AS VOToolBarButton
		LOCAL cText AS STRING
		oButton := VOToolBarButton{}
		oButton:MenuID := nID
		cText := SELF:GetTipText(nID,#MenuItemID)
		IF SELF:nButtonStyle != TB_ICONONLY
			oButton:Text := SELF:__CleanText(cCaption)
			IF String.IsNullOrEmpty(oButton:Text)
				oButton:Text := cText
			ENDIF
		ENDIF
		oButton:ToolTipText := cText

		RETURN oButton

    /// <exclude />
	ACCESS __ToolBar AS VOToolBar
		if oCtrl == NULL_OBJECT
			SELF:Create()
		ENDIF
		RETURN (VOToolBar) oCtrl

    /// <exclude />
	METHOD __GetButton(nID, symIDType, symTB) AS System.Windows.Forms.ToolBarButton
		LOCAL oTB AS VOToolBar
		Default(ref symTB, #MAINTOOLBAR)
		oTB := SELF:__FindToolBarHandle(symTB)
		Default(ref symIDType, #ButtonID)

		IF (oTB != NULL_OBJECT)
			IF (symIDType == #MenuItemID)
				// Use the menu item ID
				LOCAL IMPLIED oButton := oTB:GetButton((int) nID, TRUE)
				RETURN oButton
			ELSE
				// Get the menu item ID from the button ID
				LOCAL IMPLIED oButton := oTB:GetButton((int) nID, FALSE)
				RETURN oButton
			ENDIF
		ENDIF
		RETURN NULL_OBJECT

	#endregion

    /// <exclude />
	METHOD __CreateToolBar(symTB AS SYMBOL, oParent AS OBJECT, dwID AS DWORD, dwTBStyle AS DWORD) AS VOToolBar STRICT
		//PP-040914 from S Ebert
		//SE-050729
		//RvdH 0702056 Changed to use ToolBarChild
		LOCAL oImagelist	AS ImageList
		LOCAL oTb			AS VOToolBar
		LOCAL oTbC			AS ToolBarChild
		//LOCAL sTBAddBitmap	IS _winTBAddBitmap
		//LOCAL liTbStyle		AS LONGINT

		oCtrl := SELF:__CreateControl((INT) dwTBStyle, 0)
		oTb  := SELF:__ToolBar
		//oTb:BorderStyle := System.Windows.Forms.BorderStyle.Fixed3D
		//oTb:Divider := TRUE
		IF oTb != NULL_OBJECT
			SELF:Flat := lFlat
			//liTbStyle := LONGINT(_CAST, dwTBStyle)

			//IF _AND(liTbStyle, TBSTYLE_TRANSPARENT) = 0
			//	liTbStyle := GetWindowStyle(hWndTB)
			//	IF _AND(liTbStyle, TBSTYLE_TRANSPARENT) > 0
			//		SetWindowStyle(hWndTB, _AND(liTbStyle, _NOT(TBSTYLE_TRANSPARENT)))
			//	ENDIF
			//ENDIF

			oTbC := SELF:__FindToolBar(symTB)
			IF oTbC == NULL_OBJECT
				oTbC			:= ToolBarChild{}
				oTbC:NameSym := symTB
				oTbC:Handle 	:= oTb
				aChildren:Add(oTbC)
			ELSE
				oTbC:Handle:= oTb
			ENDIF

			//hwndParent := hWnd
			//hWnd       := hWndTB
			//WC.RegisterControl(SELF)
			//hWnd       := hwndParent

			//SendMessage(hWndTB, TB_BUTTONSTRUCTSIZE, _SIZEOF(_winTBBUTTON), 0l)
			IF oButtonSize != NULL_OBJECT
				oTb:ButtonSize := oButtonSize
			ENDIF
			//oTb:ImageSize := oButtonSize
			//SendMessage(hWndTB, TB_SETBITMAPSIZE, 0u, MakeLong(LoWord(DWORD(oButtonSize:Width)), LoWord(DWORD(oButtonSize:Height))))

			oImagelist := oBandImageList
			IF oImagelist != NULL_OBJECT
				oTb:ImageList := oImagelist
				//SendMessage(hWndTB, TB_SETIMAGELIST, 0u, LONGINT(_CAST, oImagelist:Handle()))

				//oImagelist := oTb:HotImageList
				//IF oImagelist != NULL_OBJECT
				//	SendMessage(hWndTB, TB_SETHOTIMAGELIST, 0u, LONGINT(_CAST, oImagelist:Handle()))
				//ENDIF
				//oImagelist := oTb:DisabledImageList
				//IF oImagelist != NULL_OBJECT
				//	SendMessage(hWndTB, TB_SETDISABLEDIMAGELIST, 0u, LONGINT(_CAST, oImagelist:Handle()))
				//ENDIF
			ELSEIF oBitmap != NULL_OBJECT
				//sTBAddBitmap:hInst := NULL_PTR
				//sTBAddBitmap:nID   := DWORD(_CAST,oBitmap:Handle())
				//SendMessage(hWndTB, TB_ADDBITMAP , DWORD(_CAST,nImageCount), LONGINT(_CAST,@sTBAddBitmap))
                    NOP
			ENDIF

		ENDIF

		RETURN oTb

 /// <exclude />
	METHOD __FindExtraBitMap(oBmp AS OBJECT, symTB AS SYMBOL)  AS ToolBarExtraBitmap STRICT
		//RvdH 070206 Added to centralize location of Extra Bitmaps
		FOREACH oExtraBitMap AS ToolBarExtraBitmap IN aExtraBitmaps
			IF oExtraBitMap:Bitmap == oBmp .AND. oExtraBitMap:NameSym == symTB
				RETURN oExtraBitmap
			ENDIF
		NEXT //
		RETURN NULL_OBJECT

 /// <exclude />
	METHOD __FindTipText(nID AS LONGINT, symLookUp AS SYMBOL)  AS ToolbarTipText STRICT
		//RvdH 070206 Added to centralize location of TipTexts
		IF (symLookUp == #ButtonID)
			FOREACH oText AS ToolbarTipText IN aTipsText
				IF oText:ButtonID == nID
					RETURN oText
				ENDIF
			NEXT //dwIndex
		ELSE
			FOREACH oText AS ToolbarTipText IN aTipsText
				IF oText:MenuItemID == nID
					RETURN oText
				ENDIF
			NEXT //dwIndex
		ENDIF
		RETURN NULL_OBJECT

 /// <exclude />
	METHOD __FindToolBar(symTB AS SYMBOL) AS ToolBarChild STRICT
		FOREACH oChild	AS ToolBarChild IN aChildren
			IF oChild:NameSym == symTB
				RETURN oChild
			ENDIF
		NEXT // idx

		RETURN NULL_OBJECT

 /// <exclude />
	METHOD __FindToolBarHandle(symTB AS SYMBOL) AS VOToolBar STRICT
		LOCAL oTB AS ToolBarChild
		oTB := SELF:__FindToolBar(symTB)

		IF oTB != NULL_OBJECT
			RETURN oTB:Handle
		ENDIF

		RETURN NULL_OBJECT

 /// <exclude />
	ACCESS __IsRebar AS LOGIC STRICT
		//PP-030828 Strong typing


		//RETURN (gpfnInitCommonControlsEx != NULL_PTR) .AND. !lOldStyle
		RETURN FALSE

 /// <exclude />
	ACCESS __IsTopAligned AS LOGIC STRICT
		//PP-030828 Strong typing

		//IF (hWnd != NULL_PTR)
		//	IF _AND(GetWindowStyle(hwnd), CCS_TOP) == 1
		//		RETURN TRUE
		//	ENDIF
		//ENDIF

		RETURN FALSE

 /// <exclude />
	METHOD __SetParent(oObject AS Window) AS VOID STRICT
		IF (oObject != NULL_OBJECT)
			oParent		 := oObject
			oFormSurface := oObject
		ENDIF
		RETURN


 /// <exclude />
	METHOD __TryDeferAction(symAction AS SYMBOL, nMenuItemID AS LONGINT, symTB AS SYMBOL) AS ToolBarUpdate STRICT
		LOCAL oUpdate AS ToolBarUpdate

		IF (aUpdates == NULL)
			RETURN NULL_OBJECT
		ENDIF

		// If the toolbar has not yet been created, fill out the buffer information
		// necessary to apply this operation when it is created

		oUpdate 		:= ToolBarUpdate{}
		aUpdates:Add( oUpdate)

		oUpdate:symAction 	:= symAction
		oUpdate:nMenuItemID := nMenuItemID
		oUpdate:symToolBar 	:= symTB

		RETURN oUpdate


/// <include file="Gui.xml" path="doc/ToolBar.AddBand/*" />
    METHOD AddBand(sBandName, oControl, iPos, iMinWidth, iMinHeight, sText, oForeColor, oBackColor, iImageIndex, oBackBitmap)
        // Todo: Toolbar.AddBand

		//PP-040511 Update from S Ebert
		//LOCAL rbBand 			IS _winREBARBANDINFO
		//LOCAL lRet := FALSE 	AS LOGIC
		//LOCAL liPos 			AS LONGINT
		//LOCAL oChild			AS ToolBarChild
		//IF !SELF:__IsRebar
		RETURN FALSE
	//ENDIF

	//IF IsNumeric(iPos)
	//	liPos := iPos
	//ELSE
	//	liPos := -1l
	//ENDIF

	//MemClear(@rbBand, _SIZEOF(_winREBARBANDINFO))
	//rbBand:cbSize := _SIZEOF(_winREBARBANDINFO) //COMPAT_REBARBANDINFO_SIZE
	//rbBand:fMask  := _OR(RBBIM_CHILD, RBBIM_STYLE, RBBIM_COLORS, RBBIM_ID)

	//IF IsLong(iMinHeight) .AND. IsLong(iMinWidth)
	//	rbBand:fMask := _OR(rbBand:fMask, RBBIM_CHILDSIZE)
	//	rbBand:cxMinChild := iMinWidth
	//	rbBand:cyMinChild := iMinHeight
	//ENDIF

	//IF !Empty(sText)
	//	rbBand:fMask := _OR(rbBand:fMask, RBBIM_TEXT)
	//	rbBand:lpText := StringAlloc(sText)
	//	rbBand:cch := SLen(sText)
	//ENDIF

	//rbBand:clrFore := GetSysColor(COLOR_BTNTEXT)
	//rbBand:clrBack := GetSysColor(COLOR_BTNFACE)
	//IF IsInstanceOf(oForeColor, #Color)
	//	rbBand:clrFore := oForeColor:ColorRef
	//ENDIF
	//IF IsInstanceOf(oBackColor, #Color)
	//	rbBand:clrBack := oBackColor:ColorRef
	//ENDIF

	//IF IsInstanceOf(oBackBitmap, #Bitmap)
	//	rbBand:fMask := _OR(rbBand:fMask, RBBIM_BACKGROUND)
	//	rbBand:hbmBack := oBackBitmap:Handle()
	//	AAdd(aBackBitmaps, oBackBitmap)
	//ENDIF

	//IF IsLong(iImageIndex)
	//	rbBand:fMask := _OR(rbBand:fMask, RBBIM_IMAGE)
	//	rbBand:iImage := iImageIndex
	//ENDIF

	//rbBand:fStyle := _OR(RBBS_CHILDEDGE, RBBS_GRIPPERALWAYS)
	////RvdH 070427 	oControl MUST be a Control (according to docs)
	////					but we also accept other objects that has a handle (a window)
	//rbBand:hwndChild := oControl:Handle()
	//IF IsAccess(oControl, #ControlID)
	//	rbBand:wID := oControl:ControlID
	//ELSE
	//	rbBand:wID := DWORD(100+SendMessage(hWnd, RB_GETBANDCOUNT, 0, 0))
	//ENDIF

	//IF (SendMessage(hWnd, RB_INSERTBAND, DWORD(_CAST, liPos), LONGINT(_CAST, @rbBand)) != 0)
	//	oChild := ToolBarChild{}
	//	oChild:NameSym := sBandName
	//	oChild:oCargo := oControl
	//	oChild:Handle := oControl:Handle()

	//	//AAdd(aChildren, {sBandName, oChild, Null_OBJECT, Null_OBJECT, Null_OBJECT})
	//	AAdd(aChildren, oChild)
	//	lRet := TRUE
	//ENDIF

	//IF (PTR(_CAST, rbBand:lpText) != NULL_PTR)
	//	MemFree(rbBand:lpText)
	//ENDIF

	//RETURN lRet

/// <include file="Gui.xml" path="doc/ToolBar.AddSubToolBarBand/*" />
	METHOD AddSubToolBarBand(symToolBar, iPos, iMinWidth, lFlat_dwStyle)
        // Todo: Toolbar.AddSubToolBarBand
		//PP-040505 Update from S Ebert
		//RvdH 070206 Changed to use ToolBarUpdate class
		//LOCAL hwndNewTB   AS PTR
		//LOCAL rbBand      IS _winREBARBANDINFO
		//LOCAL dwSubStyle  AS DWORD
		//LOCAL oUpdate		AS ToolBarUpdate
		//LOCAL dwBandCount AS DWORD
		//LOCAL sSize       IS _WinSize
		//LOCAL hDC         AS PTR
		//LOCAL hFont       AS PTR

		//IF !SELF:__IsRebar
		RETURN FALSE
	//ENDIF

	//DEFAULT( REF iPos, -1)
	//DEFAULT( REF iMinWidth, 100)
	//DEFAULT( REF lFlat_dwStyle, lFlat)

	//IF (hWnd != NULL_PTR)
	//	IF IsNumeric(lFlat_dwStyle)
	//		dwSubStyle := lFlat_dwStyle
	//		dwSubStyle := _OR(LONGINT(dwSubStyle), WS_CHILD, WS_CLIPSIBLINGS, WS_VISIBLE, CCS_NORESIZE, CCS_NOPARENTALIGN, CCS_NODIVIDER)
	//		IF IsThemeEnabled()
	//			dwSubStyle := _OR(dwSubStyle, TBSTYLE_TRANSPARENT)
	//		ENDIF
	//	ELSE
	//		dwSubStyle := dwStyle

	//		IF lFlat_dwStyle
	//			dwSubStyle := _OR(dwSubStyle, TBSTYLE_FLAT)
	//		ELSE
	//			dwSubStyle := _AND(dwSubStyle, _NOT(TBSTYLE_FLAT))
	//		ENDIF
	//	ENDIF

	//	dwBandCount := DWORD(_CAST, SendMessage(hWnd, RB_GETBANDCOUNT, 0, 0))

	//	hWndNewTB := SELF:__CreateToolBar(symToolBar, hwnd, 1000U+dwBandCount, dwSubStyle)

	//	IF (hWndNewTB == NULL_PTR)
	//		RETURN FALSE
	//	ENDIF

	//	MemSet(@rbBand, 0, _SIZEOF(_winREBARBANDINFO))
	//	rbBand:cbSize     := _SIZEOF(_winREBARBANDINFO) // COMPAT_REBARBANDINFO_SIZE
	//	rbBand:fMask      := _OR(RBBIM_CHILD, RBBIM_SIZE, RBBIM_CHILDSIZE, RBBIM_STYLE, RBBIM_COLORS, RBBIM_ID)
	//	rbBand:clrFore    := GetSysColor(COLOR_BTNTEXT)
	//	rbBand:clrBack    := GetSysColor(COLOR_BTNFACE)
	//	rbBand:fStyle     := _OR(RBBS_NOVERT, RBBS_CHILDEDGE, RBBS_CHILDEDGE)
	//	//rbBand.hbmBack  := LoadBitmap(hInst, MAKEINTRESOURCE(IDB_BACK));
	//	rbBand:hwndChild  := hWndNewTB
	//	rbBand:wID        := 1042U + dwBandCount
	//	rbBand:cx         := iMinWidth
	//	rbBand:cxMinChild := iMinWidth
	//	rbBand:cyMinChild := HiWord(DWORD(_CAST, SendMessage(hWndNewTB, TB_GETBUTTONSIZE, 0, 0))) + IIF(_AND(dwSubStyle, TBSTYLE_FLAT) = TBSTYLE_FLAT,2, 6)

	//	IF SELF:__ButtonStyle != TB_ICONONLY
	//		hDC := GetDC(hWndNewTB)
	//		IF (hFont := SendMessage(hWndNewTB, WM_GETFONT, 0l, 0l)) != NULL_PTR
	//			SelectObject(hdc, hFont)
	//		ENDIF
	//		GetTextExtentPoint(hDC, String2psz( "M"), 1, @sSize)
	//		ReleaseDC(hWndNewTB, hDC)
	//		IF SELF:__ButtonStyle = TB_TEXTANDICON .AND. _AND(dwSubStyle, TBSTYLE_LIST) = 0
	//			rbBand:cyMinChild += DWORD(sSize:cy)
	//		ELSEIF rbBand:cyMinChild < DWORD(sSize:cy + 8)
	//			rbBand:cyMinChild := DWORD(sSize:cy + 8)
	//		ENDIF
	//	ENDIF

	//	// Insert band into rebar
	//	SendMessage(hWnd, RB_INSERTBAND, DWORD(_CAST, iPos), LONGINT(_CAST, @rbBand))
	//ELSEIF (aUpdates != NULL_ARRAY)
	//	oUpdate := SELF:__TryDeferAction(#AddSubToolBarBand, iPos, symToolBar)
	//	// "mis-use" entries to store symToolBar, iPos, iMinWidth, lFlat)
	//	oUpdate:nButtonID := iMinWidth
	//	IF IsLogic(lFlat_dwStyle)
	//		oUpdate:nPosition := DWORD(_CAST, lFlat_dwStyle)
	//		oUpdate:bState    := 0
	//	ELSEIF IsNumeric(lFlat_dwStyle)
	//		oUpdate:nPosition := DWORD(lFlat_dwStyle)
	//		oUpdate:bState    := 1
	//	ENDIF
	//ENDIF
	//RETURN SELF

/// <include file="Gui.xml" path="doc/ToolBar.AddTipText/*" />
	METHOD AddTipText(nButtonID, nMenuItemID, cText)
		LOCAL oTipText AS ToolBarTipText

		EnforceNumeric(REF nButtonID)
		EnforceNumeric(REF nMenuItemID)
		EnforceType(REF cText,STRING)
		IF cText == TB_NOSTRING
			cText := NULL_STRING
		ENDIF
		IF cText == TB_DEFSTRING
			cText := NULL_STRING
			IF nButtonID <= 132
				cText := __CavoStr( __WCToolTipOffset + nButtonID )
			ENDIF
		ENDIF
		oTipText 				:= ToolBarTipText{}
		oTipText:ButtonID 		:= nButtonID
		oTipText:MenuItemID 	:= nMenuItemID
		oTipText:TipText		:= cText
		aTipsText:Add( oTipText)

		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.AppendItem/*" />
	METHOD AppendItem(nButtonID, nMenuItemID, oBmp, nPosition, cTitle, nImgCount, bState, bStyle, symTB)
		LOCAL oUpdate		 	AS ToolBarUpdate
		LOCAL oExtraBitmap		AS ToolBarExtraBitmap
		LOCAL oTB				AS VOToolBar
		LOCAL oButton			as System.Windows.Forms.ToolBarButton


		Default(ref symTB, #MAINTOOLBAR)
		Default(ref cTitle, "")
		Default(ref nButtonID, 0)
		oTB := SELF:__FindToolBarHandle(symTB)

		IF oCtrl != NULL_OBJECT .AND. oTB = NULL_OBJECT .AND. symTB = #MAINTOOLBAR
			//for back compatibility
			SELF:AddSubToolBarBand(symTB)
			oTB := hwndMainTB := SELF:__FindToolBarHandle(symTB)
		ENDIF

		IF oTB != NULL_OBJECT

			IF IsLong(nButtonID) .AND. nButtonID >= I_IMAGENONE
				oButton := SELF:__CreateButton(cTitle, nMenuItemID)
				oButton:ImageIndex := nButtonID-1
				IF nButtonID == IDT_SEPARATOR
					oButton:Style := System.Windows.Forms.ToolBarButtonStyle.Separator
					oButton:Pushed := TRUE
					oButton:PartialPush := TRUE
				ELSEIF IsLong(nMenuItemID)
					// Button
					//strucButton:idCommand := nMenuItemID
					//strucButton:fsState := TBSTATE_ENABLED
					//strucButton:fsStyle := TBSTYLE_BUTTON
					IF oBitmap != NULL_OBJECT .AND. IsObject(oBmp) .AND. oBmp != NULL_OBJECT .AND. SELF:__ButtonStyle != TB_TEXTONLY
						DEFAULT(ref nImgCount, 1)
						oExtraBitmap := SELF:__FindExtraBitMap(oBmp, symTB)
						IF oExtraBitmap == NULL_OBJECT
							oExtraBitmap 				:= ToolBarExtraBitmap{}
							oExtraBitmap:Bitmap 		:= oBmp
							oExtraBitmap:ImageCount		:= nPosition
							aExtraBitmaps:Add(oExtraBitmap)
						ENDIF
						IF oExtraBitmap:NameSym == NULL_SYMBOL
							//strucAddBitmap:hInst 	:= NULL_PTR
							//strucAddBitmap:nID 		:= DWORD(_CAST, oBmp:Handle())
							//nImgCount := Max(nImgCount, oExtraBitmap:ImageCount)
							//oExtraBitmap:FirstImageIndex:= SendMessage(hWndTB, TB_ADDBITMAP, nImgCount, LONGINT(_CAST, @strucAddBitmap))
							oExtraBitmap:NameSym	:= symTB
						ENDIF

					ENDIF

					IF SELF:__ButtonStyle != TB_TEXTONLY
						IF oBitmap != NULL_OBJECT
							//		IF nButtonID < 0
							//			strucButton:iBitmap := nButtonID
							//		ELSEIF nButtonID <= IDT_CUSTOMBITMAP
							//			strucButton:iBitmap := nButtonID - 1l
							//		ELSEIF IsLong(nPosition)
							//			strucButton:iBitmap := oExtraBitmap:FirstImageIndex + nPosition - 1l
							//		ELSE
							//			strucButton:iBitmap := 1l
							//		ENDIF
                            NOP

						ELSE
							//		strucButton:iBitmap := nButtonID - 1
                            NOP
						ENDIF
					ELSE
						//	strucButton:iBitmap := -1l
                        NOP
					ENDIF

					//strucButton:iString := -1l
					IF SELF:__ButtonStyle != TB_ICONONLY
						IF !String.IsNullOrEmpty(cTitle)
							//		IF ! cTitle == NULL_STRING
							//			pszTitle := StringAlloc(cTitle+CHR(0))
							//			strucButton:iString := SendMessage(hwndTB, TB_ADDSTRING, 0, LONGINT(_CAST, pszTitle))
							//			MemFree(pszTitle)
							//		ENDIF
                            NOP

						ELSEIF nButtonID > 0 .AND. nButtonID <= IDT_CUSTOMBITMAP .AND. oBitmap != NULL_OBJECT
							//		LOCAL cText AS STRING
							cTitle := __CavoStr( __WCToolBarOffset + nButtonID )
							//		strucButton:iString := SendMessage(hWndTB, TB_ADDSTRING, 0, LONGINT(_CAST, String2Psz( cText ) ) )
						ENDIF
						oButton:Text := cTitle
					ENDIF

					//IF IsNumeric(bState)
					//	strucButton:fsState := bState
					//ENDIF
					//IF IsNumeric(bStyle)
					//	strucButton:fsStyle := bStyle
					//ENDIF
				ELSE
					RETURN FALSE
				ENDIF
				oTB:Buttons:Add(oButton)
			ENDIF
			RETURN FALSE
		ELSEIF SELF:aUpdates != NULL
			// If the toolbar has not yet been created, fill out the buffer information
			// necessary to apply this operation when it is created
			IF ! IsNumeric(nMenuItemID)
				nMenuItemID := 0
			ENDIF
			oUpdate := SELF:__TryDeferAction(#AppendItem, nMenuItemID, symTB)

			IF IsObject(oBmp)
				oExtraBitmap := SELF:__FindExtraBitMap(oBmp, symTB)
				IF oExtraBitmap == NULL_OBJECT
					oExtraBitmap := ToolBarExtraBitmap{}
					oExtraBitmap:Bitmap 		:= oBmp
					oExtraBitmap:ImageCount := nPosition
					aExtraBitmaps:Add( oExtraBitmap)
				ELSE // update ImageCount
					oExtraBitmap:FirstImageIndex := Max(oExtraBitmap:FirstImageIndex, nPosition)
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

			Default(ref bState, TBSTATE_ENABLED)
			Default(ref bStyle, TBSTYLE_BUTTON)

			oUpdate:nButtonID   := nButtonID
			oUpdate:bState      := bState
			oUpdate:bStyle      := bStyle
		ENDIF

		RETURN TRUE

/// <include file="Gui.xml" path="doc/ToolBar.AppendSubItem/*" />
	METHOD AppendSubItem(symTB, nButtonID, nMenuItemID, oBmp, nPosition, cTitle, nImgCount, bState, bStyle)
		// This is only a shortcut for AppendItem
		RETURN SELF:AppendItem(nButtonID, nMenuItemID, oBmp, nPosition, cTitle, nImgCount, bState, bStyle, symTB)

/// <include file="Gui.xml" path="doc/ToolBar.BandCount/*" />
	ACCESS BandCount
		//IF !SELF:__IsRebar
		RETURN 0
	//ENDIF
	//RETURN SendMessage(hwnd, RB_GETBANDCOUNT, 0, 0)

/// <include file="Gui.xml" path="doc/ToolBar.BandImageList/*" />
	PROPERTY BandImageList as ImageList GET oBandImageList SET oBandImageList := value

/// <include file="Gui.xml" path="doc/ToolBar.Bitmap/*" />
	ACCESS Bitmap AS Bitmap
		RETURN oBitmap

/// <include file="Gui.xml" path="doc/ToolBar.Bitmap/*" />
	ASSIGN Bitmap(oNewBitmap AS Bitmap)
		LOCAL oBMPSize AS Dimension

		// Only allow the assign if the control has not yet been created
		// and if it is not in imagelist mode.
		IF oCtrl != NULL_OBJECT
			IF nImageCount != 0
				oBitmap  := oNewBitmap
				IF oBitmap = NULL_OBJECT //Enables imagelist mode
					nImageCount := 0
				ELSE
					oBMPSize := oBitmap:Size
					IF oButtonSize == NULL_OBJECT .or. oButtonSize:Width = 0
						nImageCount := oBMPSize:Width / oBMPSize:Height
					ELSE
						nImageCount := oBMPSize:Width / oButtonSize:Width
					ENDIF
				ENDIF
			ELSE
				WCError{#Bitmap,#ToolBar,"Can not assign a Bitmap in imagelist mode!",oNewBitmap,1}:Throw()
			ENDIF
		ENDIF

		RETURN


/// <include file="Gui.xml" path="doc/ToolBar.BorderStyle/*" />
	ASSIGN BorderStyle(kBorderStyle)
		// For CA-Visual Objects 1.0 compatibility only
		RETURN

/// <include file="Gui.xml" path="doc/ToolBar.BoundingBox/*" />
	ACCESS BoundingBox
		LOCAL oBoundingBox AS BoundingBox
		IF oCtrl != NULL_OBJECT
			oBoundingBox := BoundingBox{SELF:Origin, SELF:Size}
		ENDIF

		RETURN oBoundingBox
/// <include file="Gui.xml" path="doc/ToolBar.ButtonCount/*" />
	ACCESS ButtonCount            // dcaton 070215 changed from ACCESS ButtonCount(symTB)
		RETURN SELF:GetButtonCount()

/// <include file="Gui.xml" path="doc/ToolBar.GetButtonCount/*" />
	METHOD GetButtonCount(symTB)  // dcaton 070215 changed from ACCESS to METHOD, Vulcan doesn't support CLIPPER-calling convention properties
		LOCAL oTB	as VOToolBar
		LOCAL symToolBar AS SYMBOL

		IF (PCount() != 1) .OR. !IsSymbol(symTB)
			symToolBar := #MAINTOOLBAR
		ELSE
			symToolBar := symTB
		ENDIF
		oTB := SELF:__FindToolBarHandle(symToolBar)
		IF (oTB != NULL_OBJECT)
			RETURN oTB:Buttons:Count
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/ToolBar.ButtonSize/*" />
	ACCESS ButtonSize as Dimension
		RETURN oButtonSize

/// <include file="Gui.xml" path="doc/ToolBar.ButtonSize/*" />
	ASSIGN ButtonSize(oNewButtonSize as Dimension)

		// Only allow the assign if the control has not yet been created
		IF oCtrl = NULL_OBJECT .AND. SELF:__HasUpdates
			SELF:__TryDeferAction(#ButtonSize, MakeLong((WORD) oNewButtonSize:Width, (WORD) oNewButtonSize:Height), NULL_SYMBOL)
		ELSE
			oButtonSize := oNewButtonSize
		ENDIF
		RETURN


/// <include file="Gui.xml" path="doc/ToolBar.ButtonStyle/*" />
	ASSIGN ButtonStyle(kButtonStyle AS LONG)
		IF (kButtonStyle == TB_ICONONLY) .OR. (kButtonStyle == TB_TEXTONLY) .OR. (kButtonStyle == TB_TEXTANDICON)
			IF oCtrl = NULL_OBJECT .AND. SELF:__HasUpdates
				SELF:__TryDeferAction(#ButtonStyle, kButtonStyle, NULL_SYMBOL)
			ELSE
				nButtonStyle := (DWORD) kButtonStyle
				IF (nButtonStyle == TB_TEXTONLY)
					foreach oButton as VOToolBarButton in __ToolBar:Buttons
						oButton:ImageIndex := -1
					NEXT
					//IF oButtonSize:Height > 1
					//	oButtonSize:Width  := 20
					//	oButtonSize:Height := 1
					//ENDIF
				ENDIF
			ENDIF
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/ToolBar.ChangeTipText/*" />
	METHOD ChangeTipText(nID, cText, symLookUp)
		//SE-060526
		//RvdH 070206 Changed to use new ToolBarTipText objects
		LOCAL oTipText AS ToolBarTipText
		DEFAULT( ref symLookUp, #ButtonID)

		oTipText := SELF:__FindTipText(nID, symLookUp)
		IF oTipText != NULL_OBJECT
			oTipText:TipText := cText
		ENDIF
		RETURN oTipText != NULL_OBJECT

/// <include file="Gui.xml" path="doc/ToolBar.ClickItem/*" />
	METHOD ClickItem(nMenuItemID, symTB)
		RETURN SELF:PressItem(nMenuItemID, symTB)

/// <include file="Gui.xml" path="doc/ToolBar.ClientArea/*" />
	ACCESS ClientArea as BoundingBox
		LOCAL oOwnerArea AS BoundingBox
		LOCAL oToolBarArea AS BoundingBox

		IF (oCtrl != NULL_OBJECT) .AND. SELF:IsVisible()
			IF (SELF:Owner != NULL_OBJECT)
				oOwnerArea := ((Window) SELF:Owner):CanvasArea
				oToolBarArea := SELF:BoundingBox
				oOwnerArea:Top += oToolBarArea:Height
			ENDIF
			RETURN oOwnerArea
		ENDIF

		RETURN  ((Window) SELF:Owner):CanvasArea

/// <include file="Gui.xml" path="doc/ToolBar.Configure/*" />
	METHOD Configure()
		/*
		// !!! we need more info to support configure !!!
		if (hWndTB != NULL_PTR)
		SendMessage(hWndTB, TB_CUSTOMIZE, 0, 0)
		endif
		*/
		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.Create/*" />
	method Create() as System.Windows.Forms.Control strict
		//PP-040505 Update from S Ebert
		//SE-050929
		//RvdH 070206 Changed to use ToolBarUpdate class
		//LOCAL dwCount 		AS DWORD
		//LOCAL dwUpdateCount AS DWORD
		//LOCAL rbBand 		IS _winREBARBANDINFO
		//LOCAL sSize  		IS _WinSize
		//LOCAL dwRebarStyle AS DWORD

		IF (oCtrl == NULL_OBJECT)

			SELF:SetStyle(TBSTYLE_FLAT, lFlat)

			IF SELF:__hasUpdates  //only if MED menu exists
				IF nImageCount = -1l //Undefined mode
					//Setting of bitmap mode and assigning the default bitmap
					oBitmap := Bitmap{ResourceID{"IDB_DEFTOOLBAR", _GetInst()}, BMP_3DTRANSPARENT}
					oBandImageList := ImageList{132,oButtonSize}
					nImageCount := 132
					LOCAL i as DWORD

					LOCAL nWidth as LONG
					LOCAL nLeft  as LONG
					LOCAL nHeight as LONG
					nWidth  := oBitmap:__Bitmap:Width / 132
					nHeight := oBitmap:__Bitmap:Height
					FOR i:=  1 TO nImageCount
						LOCAL rect as System.Drawing.Rectangle
						LOCAL oBmp as System.Drawing.Bitmap
						rect := System.Drawing.Rectangle{nLeft,0,nWidth, nHeight}
						oBmp := oBitmap:__Bitmap:Clone(rect, oBitmap:__Bitmap:PixelFormat)
						oBandImageList:__ImageList:Images:Add(oBmp)
						nLeft += nWidth
					NEXT
				ENDIF
			ENDIF

			//IF SELF:__IsRebar
			//	SELF:SetStyle(_OR(CCS_NORESIZE, CCS_NOPARENTALIGN, CCS_NODIVIDER))

			//	IF IsThemeEnabled()
			//		SELF:SetStyle(TBSTYLE_TRANSPARENT)
			//	ENDIF

			//	dwRebarStyle := _OR(WS_VISIBLE, WS_CHILD, WS_CLIPCHILDREN, WS_CLIPSIBLINGS, RBS_VARHEIGHT, RBS_BANDBORDERS)
			//	IF ! SELF:Divider
			//		dwRebarStyle := _OR(LONGINT(dwRebarStyle), CCS_NODIVIDER)
			//	ENDIF

			//	hWnd := CreateWindowEx(WS_EX_TOOLWINDOW, PSZ(_CAST, "ReBarWindow32"), PSZ(_CAST, "Toolbar"),;
			//	dwRebarStyle, 0, 0, 400, 30, SELF:Owner:Handle(), PTR(_CAST, 13001), _GetInst(), 0)

			//	IF hWnd != NULL_PTR
			//		__WCRegisterControl(SELF) //register after we get the handle
			//		IF ALen(SELF:aUpdates) > 0
			//			hwndMainTB := SELF:__CreateToolBar(#MAINTOOLBAR, hWnd, DWORD(wID), dwStyle)
			//		ENDIF
			//	ENDIF
			//ELSE
			SELF:SetStyle(CCS_NODIVIDER, ! SELF:Divider)

			hwndMainTB := SELF:__CreateToolBar(#MAINTOOLBAR, SELF:Owner, DWORD(wId), (DWORD) dwStyle)

			oCtrl       := hwndMainTB
			//ENDIF
			IF oButtonSize != NULL_OBJECT
				hwndMainTB:ButtonSize := Dimension{oButtonSize:Width, oButtonSize:Height}
			ENDIF
			//hwndMainTB:ImageSize  := Dimension{oButtonSize:Width, oButtonSize:Height+100}
			//SendMessage(hwndMainTB, TB_SETBUTTONSIZE, 0, MAKELONG(word(oButtonSize:Width), word(oButtonSize:Height)+100))
			//SendMessage(hwndMainTB, TB_SETBITMAPSIZE, 0, MAKELONG(word(oButtonSize:Width), word(oButtonSize:Height)+100))
			IF oCtrl != NULL_OBJECT
				//__lpfnDefaultProc := GetWindowLong(hWnd, GWL_WNDPROC)
				//SetWindowLong(hWnd, GWL_WNDPROC, LONGINT(_CAST, Get__WCControlProcPtr()))   // dcaton 070319 use helper to get ptr

				oSize   := NULL_OBJECT
				oOrigin := NULL_OBJECT

				//IF SELF:__IsRebar
				//	IF (oBandImageList != NULL_OBJECT)
				//		// this does the SendMessage since now we have a handle
				//		SELF:BandImageList := oBandImageList
				//	ENDIF
				//	IF hwndMainTB != NULL_PTR
				//		MemSet(@rbBand, 0, _SIZEOF(_winREBARBANDINFO))
				//		rbBand:cbSize    := _SIZEOF(_winREBARBANDINFO) //COMPAT_REBARBANDINFO_SIZE
				//		rbBand:fMask     := _OR(RBBIM_CHILD, RBBIM_STYLE, RBBIM_COLORS, RBBIM_ID)
				//		rbBand:clrFore   := GetSysColor(COLOR_BTNTEXT)
				//		rbBand:clrBack   := GetSysColor(COLOR_BTNFACE)
				//		rbBand:fStyle    := _OR(RBBS_NOVERT, RBBS_CHILDEDGE, RBBS_CHILDEDGE)
				//		rbBand:hwndChild := hwndMainTB
				//		rbBand:wID       := 1042

				//		// Insert band into rebar
				//		SendMessage(hWnd, RB_INSERTBAND, DWORD(_CAST, -1L), LONGINT(_CAST, @rbBand))
				//	ENDIF
				//ENDIF


					// Apply all buffered changes and reset dwUpdateCount
					FOREACH oUpdate AS ToolbarUpdate IN  aUpdates
						DO CASE
						CASE oUpdate:symAction == #AppendItem
							SELF:AppendItem(oUpdate:nButtonID, oUpdate:nMenuItemID, oUpdate:oBitmap, oUpdate:nPosition, ;
							IIF(oUpdate:nBeforeID = 1, oUpdate:cButtonText, NIL), oUpdate:nImageCount, oUpdate:bState, ;
							oUpdate:bStyle, oUpdate:symToolBar)

						CASE oUpdate:symAction == #InsertItem
							SELF:InsertItem(oUpdate:nButtonID, oUpdate:nMenuItemID, oUpdate:nBeforeID,;
							oUpdate:bState, oUpdate:bStyle, oUpdate:symToolBar)

						CASE oUpdate:symAction == #Rows
							SELF:SetRows( oUpdate:nMenuItemID, oUpdate:symToolBar )

							//CASE oUpdate:symAction == #AddSubToolBarBand
							//	SELF:AddSubToolBarBand( oUpdate:symToolBar, oUpdate:nMenuItemID, oUpdate:nButtonID, ;
							//		IIF(oUpdate:bState = 0, LOGIC(_CAST, oUpdate:nPosition), oUpdate:nPosition))

						CASE oUpdate:symAction == #ButtonSize
							oButtonSize:Width  := LoWord(DWORD(oUpdate:nMenuItemID))
							oButtonSize:Height := HiWord(DWORD(oUpdate:nMenuItemID))

						CASE oUpdate:symAction == #ButtonStyle
							SELF:ButtonStyle := oUpdate:nMenuItemID

						OTHERWISE
							IF IsMethod(SELF, oUpdate:symAction)
								Send(SELF, oUpdate:symAction, oUpdate:nMenuItemID, oUpdate:symToolBar)
								ELSE	// RH DoAction is defined in our patches
								SELF:DoAction( oUpdate)
							ENDIF
						END CASE
					NEXT

					// Free the Array
					SELF:aUpdates := NULL

				//IF SELF:__IsRebar
				//	dwcount := DWORD(_CAST,SendMessage(hWnd, RB_GETBANDCOUNT, 0, 0))

				//	IF hWndMainTB != NULL_PTR
				//		rbBand:fMask      := RBBIM_CHILDSIZE
				//		SendMessage(hwndMainTB, TB_GETMAXSIZE, 0l, LONGINT(_CAST,@sSize))
				//		rbBand:cxMinChild := DWORD(sSize:cx)
				//		rbBand:cyMinChild := DWORD(sSize:cy + IIF(lFlat,2, 6))

				//		//rbBand.cxMinChild := LoWord(DWORD(_CAST, SendMessage(hwndMainTB, TB_GETBUTTONSIZE, 0, 0))) * SELF:ButtonCount + 26
				//		//rbBand.cyMinChild := HiWord(DWORD(_CAST, SendMessage(hwndMainTB, TB_GETBUTTONSIZE, 0, 0))) + 6

				//		// ??? needed ???
				//		//if (nButtonStyle == TB_TEXTANDICON)
				//		// rbBand.cyMinChild += 10
				//		//endif
				//		SendMessage(hWnd, RB_SETBANDINFO, 0U, LONGINT(_CAST, @rbBand))

				//		//delete the following 3 code lines, if MED band order is corrected.
				//		IF dwCount > 1
				//			SendMessage(hWnd, RB_MOVEBAND, 0, LONGINT(_CAST, dwCount) - 1l)
				//		ENDIF
				//	ENDIF

				//	IF dwCount > 1
				//		SendMessage(hWnd, RB_MAXIMIZEBAND, dwCount-1, 0)
				//	ENDIF
				//ENDIF
			ENDIF
		ENDIF
		LOCAL oOwner AS Window
		oOwner := (OBJECT) oParent
		IF oOwner != NULL_OBJECT .and. oOwner:__Form != NULL_OBJECT
            oOwner:__Form:AddControl(oCtrl)
            if oCtrl is IVOControl var oVC
                oOwner:__Form:SetChildIndex(oVC, 0)
            endif
		endif
		RETURN oCtrl

/// <include file="Gui.xml" path="doc/ToolBar.DeleteItem/*" />
	METHOD DeleteItem(nMenuItemID, symTB)
		LOCAL liIndex AS LONGINT
		LOCAL oTB AS VOToolBar

		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTB := SELF:__FindToolBarHandle(symTB)

		IF oTB != NULL_OBJECT
			// Get the index number from nMenuItemID
			IF nMenuItemID <= 0 //If nMenuItemID is <= 0 nMenuItemID means the zero based buttonindex
				liIndex := -1 * nMenuItemID
			ELSE
				LOCAL IMPLIED oButton := oTB:GetButton(nMenuItemID, TRUE)
				liIndex := oTB:Buttons:IndexOf(oButton)
			ENDIF
			IF liIndex >= 0
				oTB:Buttons:RemoveAt(liIndex)
			ENDIF
		ELSE
			SELF:__TryDeferAction(#DeleteItem, nMenuItemID, symTB)
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.Destroy/*" />
	METHOD Destroy() AS USUAL CLIPPER
		//PP-040417 from S Ebert
		//SE-050729
		//RvdH 0702056 Changed to use ToolBarChild
		// RvdH-030323 Add check for empty toolbar

		//IF SELF:__IsRebar
		//	dwCount := ALen(aChildren)
		//	FOR dwI := 1 UPTO dwCount
		//		oChild := aChildren[dwI]
		//		hWndTB := oChild:Handle
		//		IF oChild:oCargo = NULL_OBJECT .AND. hWndTB != NULL_PTR
		//			__WCUnRegisterControl(hWndTB)
		//		ENDIF
		//	NEXT //dwI
		//ENDIF

		//oBitmap 		:= NULL_OBJECT
		//aExtraBitmaps 	:= NULL_ARRAY
		//aTipsText 		:= NULL_ARRAY
		//aUpdates		:= NULL_ARRAY
		//aChildren 		:= NULL_ARRAY
		//oBandImageList := NULL_OBJECT

		SUPER:Destroy()

		RETURN NIL

	METHOD DimItem(nMenuItemID, symTB)
		LOCAL oTb AS VOToolBar
		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTb := SELF:__FindToolBarHandle(symTB)

		IF (oTb != NULL_OBJECT)
			LOCAL IMPLIED oButton := oTb:GetButton(nMenuItemID, TRUE)
			IF oButton != NULL_OBJECT
				oButton:PartialPush  := TRUE
			ENDIF
		ELSE
			SELF:__TryDeferAction(#DimItem, nMenuItemID, symTB)
		ENDIF

		RETURN NIL


/// <include file="Gui.xml" path="doc/ToolBar.DisableItem/*" />
	METHOD DisableItem(nMenuItemID, symTB)
		LOCAL oTB  AS VOToolBar

		IF IsSymbol(symTB) .AND. symTB != NULL_SYMBOL
			oTB := SELF:__FindToolBarHandle(symTB)

			IF oTB != NULL_OBJECT
				LOCAL IMPLIED oButton := oTB:GetButton(nMenuItemID, TRUE)
				IF oButton != NULL_OBJECT
					oButton:Enabled := FALSE
				ENDIF
			ELSE
				SELF:__TryDeferAction(#DisableItem, nMenuItemID, symTB)
			ENDIF
		ELSE
			IF oCtrl != NULL_OBJECT
				//dwLen := ALen(aChildren)
				//FOR i := 1 UPTO dwLen
				//	oChild :=aChildren[i]
				//	IF oChild:Handle != NULL_PTR .AND. oChild:oCargo = NULL_OBJECT
				//		SendMessage(oChild:Handle, TB_ENABLEBUTTON, nMenuItemID, 0)
				//	ENDIF
				//NEXT
                NOP

			ELSE
				SELF:__TryDeferAction(#DisableItem, nMenuItemID, NULL_SYMBOL)
			ENDIF
		ENDIF

		RETURN NIL

	METHOD DoAction(oUpdate)
		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.EnableBands/*" />
	METHOD EnableBands(lEnable)

		DEFAULT( ref lEnable, true)
		lOldStyle := !lEnable

		RETURN lEnable


/// <include file="Gui.xml" path="doc/ToolBar.EnableDrag/*" />
	METHOD EnableDrag(lEnable)
		// Defer while feature is unavailable
		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.EnableItem/*" />
	METHOD EnableItem(nMenuItemID, symTB)
		//PP-040421 Update from S Ebert
		//SE-060520
		//RvdH 0702056 Changed to use ToolBarChild
		LOCAL oTB  AS VOToolBar
		IF IsSymbol(symTB) .AND. symTB != NULL_SYMBOL
			oTB := SELF:__FindToolBarHandle(symTB)

			IF oTB != NULL_OBJECT
				LOCAL IMPLIED oButton := oTB:GetButton(nMenuItemID, TRUE)
				IF oButton != NULL_OBJECT
					oButton:Enabled := FALSE
				ENDIF
			ELSE
				SELF:__TryDeferAction(#EnableItem, nMenuItemID, symTB)
			ENDIF
		ELSE
			IF oCtrl != NULL_OBJECT
				//dwLen :=  ALen(aChildren)
				//FOR i := 1 UPTO dwLen
				//	oChild := aChildren[i]
				//	IF oChild:Handle != NULL_PTR .AND. oChild:oCargo = NULL_OBJECT
				//		SendMessage(oChild:Handle, TB_ENABLEBUTTON, nMenuItemID,0xFFFFFFFFL)
				//	ENDIF
				//NEXT
                NOP
			ELSE
				SELF:__TryDeferAction(#EnableItem, nMenuItemID, NULL_SYMBOL)
			ENDIF
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.Flat/*" />
	ACCESS Flat AS LOGIC
		RETURN lFlat

/// <include file="Gui.xml" path="doc/ToolBar.Flat/*" />
	ASSIGN Flat(lNewVal as LOGIC)
		lFlat := lNewVal
		IF oCtrl != NULL_OBJECT
			IF lFlat
				SELF:__ToolBar:Appearance := System.Windows.Forms.ToolBarAppearance.Flat
			ELSE
				SELF:__ToolBar:Appearance := System.Windows.Forms.ToolBarAppearance.Normal
			ENDIF
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/ToolBar.GapSize/*" />
	ASSIGN GapSize(nGapSize)
		// For CA-Visual Objects 1.0 compatibility only
		RETURN

/// <include file="Gui.xml" path="doc/ToolBar.GetButtonDescription/*" />
	METHOD GetButtonDescription(nButtonID, symTB)
		//SE-041015 Fix from S Ebert
		LOCAL cDescription AS STRING
		LOCAL oTB AS VOToolBar

		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTB := SELF:__FindToolBarHandle(symTB)

		IF (oTB != NULL_OBJECT)
			LOCAL IMPLIED oButton := oTB:GetButton(nButtonID, TRUE)
			IF oButton != NULL_OBJECT
				cDescription := oButton:Text
			ENDIF
		ENDIF

		RETURN AllTrim(cDescription)

/// <include file="Gui.xml" path="doc/ToolBar.GetImageList/*" />
	METHOD GetImageList(symType, symTB)
		//PP-040417 from S Ebert
		//RvdH 0702056 Changed to use ToolBarChild
		LOCAL oTB	  	  AS ToolBarChild
		LOCAL oImageList AS ImageList

		DEFAULT( ref symTB, #MAINTOOLBAR)
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

/// <include file="Gui.xml" path="doc/ToolBar.GetState/*" />
	METHOD GetState(nMenuItemID, symTB)
		//SE-050701
		LOCAL oTb AS VOToolBar


		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTb := SELF:__FindToolBarHandle(symTB)

		IF oTb != NULL_OBJECT
			//LOCAL IMPLIED oButton := oTb:GetButton(nMenuItemID, TRUE)
			RETURN 0 // oButton
		ENDIF

		RETURN -1l


/// <include file="Gui.xml" path="doc/ToolBar.GetTipText/*" />
	METHOD GetTipText(nButtonID, symLookUp)
		//SE-060526
		//RvdH 070206 Changerd to use new ToolBarTipText objects
		LOCAL oTipText AS ToolBarTipText
		LOCAL cResult	AS STRING


		DEFAULT( ref symLookUp, #ButtonID)

		oTipText := SELF:__FindTipText(nButtonID, symLookUp)
		IF oTipText != NULL_OBJECT
			cResult := oTipText:TipText
		ENDIF
		RETURN cResult

	//METHOD Hide()
	//	SUPER:Hide()
	//IF IsInstanceOf(SELF:Owner, #ShellWindow)
	//	((ShellWindow) SELF:Owner):__AdjustClient()
	//ELSEIF IsInstanceOf(SELF:Owner, #DataWindow)
	//	((DataWindow) SELF:Owner):__AdjustForm()
	//ENDIF

	//	RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.HideItem/*" />
	METHOD HideItem(nMenuItemID, symTB)
		LOCAL oTb AS VOToolBar

		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTb := SELF:__FindToolBarHandle(symTB)

		IF (oTb != NULL_OBJECT)
			LOCAL IMPLIED oButton := oTb:GetButton(nMenuItemID,TRUE)
			if oButton != NULL_OBJECT
				oButton:Visible := FALSE
			ENDIF
		ELSE
			SELF:__TryDeferAction(#HideItem, nMenuItemID, symTB)
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.ImageCount/*" />
	ACCESS ImageCount
		//SE-050729

		IF nImageCount < 0
			RETURN 0
		ENDIF

		RETURN nImageCount


/// <include file="Gui.xml" path="doc/ToolBar.ImageCount/*" />
	ASSIGN ImageCount(nNewImageCount)
		//SE-050729

		// Only allow the assign if the control has not yet been created
		IF oCtrl = NULL_OBJECT .AND. nNewImageCount > 0 .AND. nImageCount != 0
			nImageCount := nNewImageCount
		ENDIF

		RETURN

	METHOD __SetButtonSize(nSize AS LONG) AS VOID
		oButtonSize := Dimension{nSize, nSize}
		FOREACH oChild AS ToolbarChild IN aChildren
			LOCAL oTB  AS VOToolBar
			oTb := oChild:Handle
			oTb:ButtonSize := oButtonSize
		NEXT

/// <include file="Gui.xml" path="doc/ToolBar.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, lEnableBands, nButtonSize)
		DEFAULT( ref xID, 0)
		DEFAULT( ref oPoint, Point{})
		DEFAULT( ref oDimension, Dimension{})
		DEFAULT( ref lEnableBands, true)
		DEFAULT( ref nButtonSize, 16)
		SELF:cClassName := TOOLBARCLASSNAME
        local oPt := oPoint as Point
        local oDim := oDimension as Dimension

		aUpdates	:= LIst<ToolBarUpdate>{}
		aTipsText := List<ToolbarTipText>{}
		aExtraBitmaps := List<ToolbarExtraBitmap>{}
		aChildren := List<ToolbarChild>{}

		SELF:SetStyle(_OR(WS_CHILD, WS_CLIPSIBLINGS, WS_VISIBLE, TBSTYLE_WRAPABLE, TBSTYLE_TOOLTIPS))

		SELF:__SetButtonSize(nButtonSize)

		// Set up default configuration
		nImageCount := -1L //Unintialized mode
		// Three image modes exist
		// nImageCount = -1l Unintialized.
		// nImageCount =  0l Toolbar uses only Imagelists, using of Bitmaps is impossible.
		// nImageCount >  0l Toolbar uses only Bitmaps, using of Imagelists is impossible.

		SELF:Divider := ! lEnableBands

		// Initialize update structure array
		//ptrUpdateArray := MemCAlloc(16, _SizeOf(__WCToolBarUpdate))
		//dwUpdateCount := 0
		lOldStyle := !lEnableBands

		SELF:ButtonStyle := TB_ICONONLY
		aBackBitmaps := {}
		IF IsNil(oOwner)
			// ToolBar is being created without a parent; don't call super:Init()

			oOrigin 	:= Point{oPt:X, oPt:Y}
			oSize 		:= Dimension{oDim:Width, oDim:Height}
		ELSE
			// ToolBar is being created with a parent, calling super:Init() is safe
			SUPER(oOwner, xID, oPoint, oDimension, TOOLBARCLASSNAME)
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/ToolBar.InsertItem/*" />
	METHOD InsertItem(nButtonID, nMenuItemID, nBeforeID, bState, bStyle, symTB)
		//PP-040421 Update from S Ebert
		//RvdH 070206 Changed to use ToolBarUpdate class
		LOCAL oUpdate 		AS ToolBarUpdate
		LOCAL oTB 		AS VOToolBar
		LOCAL oButton			as System.Windows.Forms.ToolBarButton

		DEFAULT( ref symTB, #MAINTOOLBAR)
		DEFAULT( ref nButtonID, 0)
		oTB := SELF:__FindToolBarHandle(symTB)

		DEFAULT( ref nBeforeID, -1)

		IF (oTB != NULL_OBJECT)
			IF IsLong(nButtonID)
				oButton := SELF:__CreateButton("",nMenuItemID)
				oButton:ImageIndex := nButtonID-1
				// Convert to zero-base
				IF (nBeforeID != -1)
					nBeforeID -= 1
				ENDIF
				IF (nButtonID == IDT_SEPARATOR)
					// Separator
					oButton:Style := System.Windows.Forms.ToolBarButtonStyle.Separator
				ELSEIF IsLong(nMenuItemID)
					// Button
                    NOP

				ENDIF
				oTB:Buttons:Insert(nBeforeID, oButton)
			ENDIF


		ELSEIF (SELF:aUpdates != NULL)
			// If the toolbar has not yet been created, fill out the buffer information
			// necessary to apply this operation when it is created
			IF ! IsNumeric(nMenuItemID)
				nMenuItemID := 0
			ENDIF
			oUpdate := SELF:__TryDeferAction(#InsertItem, nMenuItemID, symTB)

			DEFAULT( ref bState, TBSTATE_ENABLED)
			DEFAULT( ref bStyle, TBSTYLE_BUTTON)

			oUpdate:nButtonID := nButtonID
			oUpdate:nBeforeID := nBeforeID
			oUpdate:bState    := bState
			oUpdate:bStyle    := bStyle
		ENDIF

		RETURN TRUE

/// <include file="Gui.xml" path="doc/ToolBar.IsClicked/*" />
	METHOD IsClicked(nID, symIDType, symTB)
		LOCAL IMPLIED oButton := SELF:__GetButton(nID, symIDType, symTB)
		IF oButton != NULL_OBJECT
			RETURN oButton:Pushed
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ToolBar.IsDimmed/*" />
	METHOD IsDimmed(nID, symIDType, symTB)
		LOCAL IMPLIED oButton := SELF:__GetButton(nID, symIDType, symTB)
		IF oButton != NULL_OBJECT
			RETURN !oButton:PartialPush
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ToolBar.IsEnabled/*" />
	METHOD IsEnabled(nID, symIDType, symTB) AS LOGIC CLIPPER
		LOCAL IMPLIED oButton := SELF:__GetButton(nID, symIDType, symTB)
		IF oButton != NULL_OBJECT
			RETURN oButton:Enabled
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ToolBar.IsHidden/*" />
	METHOD IsToolbarHidden(nID, symIDType, symTB)
		LOCAL IMPLIED oButton := SELF:__GetButton(nID, symIDType, symTB)
		IF oButton != NULL_OBJECT
			RETURN !oButton:Visible
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ToolBar.IsPressed/*" />
	METHOD IsPressed(nID, symIDType, symTB)
		LOCAL IMPLIED oButton := SELF:__GetButton(nID, symIDType, symTB)
		IF oButton != NULL_OBJECT
			RETURN oButton:Pushed
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ToolBar.Location/*" />
	ASSIGN Location(kLocation)
		// Defer while feature is unavailable
		RETURN

/// <include file="Gui.xml" path="doc/ToolBar.PressItem/*" />
	METHOD PressItem(nMenuItemID, symTB)
		LOCAL oTb AS VOToolBar
		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTb := SELF:__FindToolBarHandle(symTB)

		IF (oTb != NULL_OBJECT)
			LOCAL IMPLIED oButton := oTb:GetButton(nMenuItemID, TRUE)
			IF oButton != NULL_OBJECT
				oButton:Pushed := TRUE
			ENDIF
		ELSE
			SELF:__TryDeferAction(#PressItem, nMenuItemID, symTB)
		ENDIF

		RETURN NIL


/// <include file="Gui.xml" path="doc/ToolBar.RemoveTipText/*" />
	METHOD RemoveTipText(nButtonID, symLookUp)
		//SE-060526


		DEFAULT( ref symLookUp, #ButtonID)

		LOCAL IMPLIED oText  := SELF:__FindTipText(nButtonID, symLookUp)
		IF oText != NULL_OBJECT
			aTipsText:Remove(oText)
		ENDIF

		RETURN oText != NULL_OBJECT

/// <include file="Gui.xml" path="doc/ToolBar.Rows/*" />
	ACCESS Rows             // dcaton 070215 was ACCESS Rows(nRows)
		RETURN SELF:GetRows()

/// <include file="Gui.xml" path="doc/ToolBar.GetRows/*" />
	METHOD GetRows(symTB)   // dcaton changed from ACCESS to METHOD, Vulcan doesn't support CLIPPER-calling convention properties
		LOCAL oTB AS VOToolBar
		LOCAL symToolBar AS SYMBOL
		IF (PCount() != 1) .OR. !IsSymbol(symTB)
			symToolBar := #MAINTOOLBAR
		ELSE
			symToolBar := symTB
		ENDIF

		oTB := SELF:__FindToolBarHandle(symToolBar)

		IF (oTB != NULL_OBJECT)
			RETURN 1
		ENDIF

		RETURN 0

/// <include file="Gui.xml" path="doc/ToolBar.Rows/*" />
	ASSIGN Rows( nRows )	         // dcaton 070215 was ASSIGN Rows(nRows,symTB)
		SELF:SetRows( nRows )

/// <include file="Gui.xml" path="doc/ToolBar.SetRows/*" />
	METHOD SetRows(nRows, symTB)   // dcaton 070215 changed from ASSIGN to METHOD, Vulcan doesn't support CLIPPER-calling convention properties
		//PP-040421 Update from S Ebert
		//LOCAL strucRect IS _winRect
		LOCAL oTB AS VOToolBar
		LOCAL symToolBar AS SYMBOL

		IF (PCount() != 2) .OR. !IsSymbol(symTB)
			symToolBar := #MAINTOOLBAR
		ELSE
			symToolBar := symTB
		ENDIF

		oTB := SELF:__FindToolBarHandle(symToolBar)

		IF (oTB != NULL_OBJECT)
			//SendMessage(hwndTB, TB_SETROWS, MAKEWPARAM(WORD(_CAST, nRows), WORD(_CAST, TRUE)), LONGINT(_CAST, @strucRect))
            NOP
		ELSE
			SELF:__TryDeferAction(#Rows, 0, symToolBar)
		ENDIF

		RETURN nRows


/// <include file="Gui.xml" path="doc/ToolBar.SeparatorSize/*" />
	ASSIGN SeparatorSize(nSeparatorSize)
		// For CA-Visual Objects 1.0 compatibility only
		RETURN

/// <include file="Gui.xml" path="doc/ToolBar.SetImageList/*" />
	METHOD SetImageList(uImageList, symType, symTB)
		//PP-040417 from S Ebert
		//SE-050729
		//RvdH 0702056 Changed to use ToolBarChild
		//LOCAL oTB     	  AS ToolBarChild
		//LOCAL dwMsg      AS DWORD
		//LOCAL oImageList AS ImageList
		//LOCAL oBmp       AS Bitmap
		//LOCAL oBMPSize   AS Dimension
		//LOCAL dwCount    AS DWORD

		////SE-050729
		//IF nImageCount < 1

		//	nImageCount := 0

		//	IF IsInstanceOfUsual(uImageList, #Bitmap)
		//		oBmp     := uImageList
		//		oBMPSize := oBmp:Size
		//		dwCount  := DWORD(oBMPSize:Width)
		//		IF oButtonSize:Width = 0
		//			oBMPSize:Width := oBMPSize:Height
		//		ELSE
		//			oBMPSize:Width := oButtonSize:Width
		//		ENDIF
		//		dwCount /= DWORD(oBMPSize:Width)

		//		oImageList := ImageList{dwCount, oButtonSize, NIL, _OR(ILC_COLOR24, ILC_MASK), 1}
		//		IF oImageList:Handle() != NULL_PTR
		//			oImageList:AddMask(oBmp, Color{192, 192, 192})
		//		ELSE
		//			oImageList := NULL_OBJECT
		//		ENDIF
		//	ELSE
		//		oImageList := uImageList
		//	ENDIF

		//	DEFAULT( REF symTB, #MAINTOOLBAR)
		//	oTB := SELF:__FindToolBar(symTB)
		//	IF oTB == NULL_OBJECT
		//		oTB	:= ToolBarChild{}
		//		oTB:NameSym := symTB
		//		AAdd(aChildren, oTB)
		//	ENDIF

		//	dwMsg  := TB_SETIMAGELIST
		//	IF IsSymbol(symType)
		//		IF symType = #HOTIMAGELIST
		//			oTB:HotImageList := oImageList
		//			dwMsg  := TB_SETHOTIMAGELIST
		//		ELSEIF symType = #DisabledImageList
		//			oTB:DisabledImageList := oImageList
		//			dwMsg  := TB_SETDISABLEDIMAGELIST
		//		ELSE
		//			oTB:ImageList := oImageList
		//		ENDIF
		//	ELSE
		//		oTB:ImageList := oImageList
		//	ENDIF

		//	IF oTB:Handle != NULL_PTR .AND. oImageList != NULL_OBJECT
		//		SendMessage(oTB:Handle, dwMsg, 0u, LONGINT(_CAST, oImageList:Handle()))
		//	ENDIF
		//ELSE
		//	WCError{#SetImageList,#ToolBar,"Can not assign an ImageList in bitmap mode!",uImageList,1}:Throw()
		//ENDIF

		RETURN uImageList


/// <include file="Gui.xml" path="doc/ToolBar.SetState/*" />
	METHOD SetState(nMenuItemID, nState, symTB)
		LOCAL oTB AS VOToolBar

		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTB := SELF:__FindToolBarHandle(symTB)

		IF (oTB != NULL_OBJECT)
			//LOCAL IMPLIED oButton := oTB:GetButton(nMenuItemID, TRUE)
			//oButton:State := nState
            NOP
		ENDIF

		RETURN NIL

	//METHOD Show()
	//	SUPER:Show()

	//	//IF IsInstanceOf(SELF:Owner, #ShellWindow)
	//	//	((ShellWindow) SELF:Owner):__AdjustClient()
	//	//ELSEIF IsInstanceOf(SELF:Owner, #DATAWINDOW)
	//	//	((DataWindow) SELF:Owner):__AdjustForm()
	//	//ENDIF

	//	RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.ShowBand/*" />
	METHOD ShowBand(iPos, lShow)


		//IF !SELF:__IsRebar
		RETURN FALSE
	//ENDIF

	//DEFAULT( REF lShow, TRUE)
	//RETURN (SendMessage(hWnd, RB_SHOWBAND, DWORD(_CAST, iPos), LONGINT(_CAST, lShow)) > 0)

/// <include file="Gui.xml" path="doc/ToolBar.ShowItem/*" />
	METHOD ShowItem(nMenuItemID, symTB)
		LOCAL oTb AS VOToolBar

		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTb := SELF:__FindToolBarHandle(symTB)

		IF (oTb != NULL_OBJECT)
			LOCAL IMPLIED oButton := oTb:GetButton(nMenuItemID, TRUE)
			IF oButton != NULL_OBJECT
				oButton:Visible := TRUE
			ENDIF
		ELSE
			SELF:__TryDeferAction(#ShowItem, nMenuItemID, symTB)
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.UnClickItem/*" />
	METHOD UnClickItem(nMenuItemID, symTB)
		LOCAL oTb AS VOToolBar
		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTb := SELF:__FindToolBarHandle(symTB)

		IF (oTb != NULL_OBJECT)
			LOCAL IMPLIED oButton := oTb:GetButton(nMenuItemID, TRUE)
			IF oButton != NULL_OBJECT
				oButton:Pushed := FALSE
			ENDIF
		ELSE
			SELF:__TryDeferAction(#UnClickItem, nMenuItemID, symTB)
		ENDIF
		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.UnDimItem/*" />
	METHOD UnDimItem(nMenuItemID, symTB)
		//PP-040421 Update from S Ebert
		LOCAL oTb AS VOToolBar
		DEFAULT( ref symTB, #MAINTOOLBAR)
		oTb := SELF:__FindToolBarHandle(symTB)

		IF (oTb != NULL_OBJECT)
			LOCAL IMPLIED oButton := oTb:GetButton(nMenuItemID, TRUE)
			IF oButton != NULL_OBJECT
				oButton:PartialPush  := FALSE
			ENDIF
		ELSE
			SELF:__TryDeferAction(#UnDimItem, nMenuItemID, symTB)
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/ToolBar.UnPressItem/*" />
	METHOD UnPressItem(nMenuItemID, symTB)
		RETURN SELF:UnClickItem(nMenuItemID, symTB)


/// <include file="Gui.xml" path="doc/ToolBar.Update/*" />
	METHOD Update()
		// Dummy - for CA-Visual Objects 1.0 compatibility only
		RETURN NIL


	CLASS ToolBarChild
		PROPERTY NameSym 			AS SYMBOL       AUTO
		PROPERTY Handle				AS VOToolBar    AUTO
		PROPERTY ImageList 			AS OBJECT       AUTO
		PROPERTY HotImageList 		AS OBJECT       AUTO
		PROPERTY DisabledImageList 	AS OBJECT       AUTO
	END CLASS

	CLASS ToolBarExtraBitmap
		PROPERTY Bitmap				AS OBJECT       AUTO
		PROPERTY ImageCount			AS LONGINT      AUTO
		PROPERTY FirstImageIndex	    AS LONGINT      AUTO
		PROPERTY NameSym				AS SYMBOL       AUTO
	END CLASS

	CLASS ToolBarTipText
		PROPERTY ButtonID 	AS LONGINT   AUTO
		PROPERTY MenuItemID	AS LONGINT   AUTO
		PROPERTY TipText	AS STRING    AUTO
	END CLASS

	CLASS ToolBarUpdate
		PROPERTY symAction	 	AS SYMBOL       AUTO
		PROPERTY nButtonID	 	AS LONGINT      AUTO
		PROPERTY nMenuItemID 	AS LONGINT      AUTO
		PROPERTY nBeforeID	 	AS LONGINT      AUTO
		PROPERTY oBitmap		AS OBJECT       AUTO
		PROPERTY nPosition	 	AS DWORD        AUTO
		PROPERTY cButtonText 	AS STRING       AUTO
		PROPERTY nImageCount 	AS DWORD        AUTO
		PROPERTY bState		 	AS BYTE         AUTO
		PROPERTY bStyle		 	AS BYTE         AUTO
		PROPERTY symToolBar 	AS SYMBOL       AUTO
		PROPERTY lFlat          AS LOGIC        AUTO
	END CLASS

END CLASS




#region defines
DEFINE I_IMAGENONE := (-2)
#endregion

