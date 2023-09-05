//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#define TAB_SYMBOL 1
#define TAB_INDEX  2
#define TAB_PAGE   3
#define TIP_SYMBOL 1
#DEFINE TIP_TEXT   2


/// <include file="Gui.xml" path="doc/TabControl/*" />
CLASS TabControl INHERIT TextControl
	PROTECT oImageList		 AS ImageList
	protect aPages			 as array
	protect aTipsText		 as array
	PROTECT nMaxHeight		 AS INT
	PROTECT nMaxWidth		 AS INT
	PROTECT lAutoSize        AS LOGIC
	protect oCurrentPage	 as IGuiObject
    /// <inheritdoc />
    PROPERTY ControlType AS Controltype GET Controltype.TabControl

    /// <inheritdoc />
	METHOD OnControlCreated(oC AS IVOControl) AS VOID
		oC:Resize += OnReSize
		RETURN

    /// <inheritdoc />
	PROTECTED METHOD OnResize(o as object, e AS EventArgs) AS VOID
		SELF:Resize(ResizeEvent{})

	ACCESS __TabControl AS VOTabControl
		IF oCtrl == NULL_OBJECT
			SELF:Create()
		ENDIF
		RETURN (VOTabControl) oCtrl


	METHOD __AdjustIndices() AS VOID STRICT
		LOCAL dwI, dwCount AS LONG
		LOCAL nPos AS LONG
		dwCount := (long) Alen(aPages)
		for dwI := dwCount downto 1
			local aElement as array
			aElement := aPages[dwI]
			nPos := __TabControl:TabPages:IndexOfKey( (string) aElement[TAB_SYMBOL])
			if nPos >= 0
				aElement[TAB_INDEX] := nPos
			else
				if IsObject(aElement[TAB_PAGE])
					Send(aElement[TAB_PAGE], #Destroy)
				endif
				ADel(aPages, (dword) dwI)
				aSize(aPages, Alen(aPages)-1)
			endif
		next


	METHOD __AdjustPage() AS VOID STRICT
		// The FramePanel for a DataWindow sizes automatically
		// The SurfacePanel must be changed here, because we may want it to grow automatically
		// Todo: Fix this
		RETURN

 /// <exclude />
	METHOD __CalcNewDimension(oNewPage AS OBJECT) AS VOID STRICT
		// No need to resize, DockStyle.Fill
		RETURN

 /// <exclude />
	METHOD __FocusPage(nIndex AS INT) AS VOID STRICT

		LOCAL oOldPage        AS Window
		LOCAL lSelfFocus      AS LOGIC
		LOCAL oTabPage		  as System.Windows.Forms.TabPage
		LOCAL oPanel			AS IVOPanel
		IF ! SELF:__IsValid
			RETURN
		ENDIF
		oOldPage	 := SELF:CurrentPage
		oCurrentPage := SELF:__GetPageFromIndex(nIndex)
		lSelfFocus   := __TabControl:Focused
		IF (oCurrentPage != NULL_OBJECT)
			IF oOldPage != NULL_OBJECT
				oOldPage:Hide()
			ENDIF
			if oCurrentPage is DataWindow var oDW
				oPanel := oDw:__Frame
			elseif oCurrentPage is IControlParent var oPar
				oPanel := oPar:__Surface
			ENDIF
			oPanel:Visible := TRUE

			IF lSelfFocus
				__TabControl:Focus()
			ELSE
				oTabPage  := __TabControl:TabPages[nIndex]
				oTabPage:Focus()
				oCurrentPage:SetFocus()
			ENDIF
		ENDIF
		RETURN

 /// <exclude />
	METHOD __GetIndexFromSymbol(symTabName AS SYMBOL) AS INT STRICT
		FOREACH oPage AS System.Windows.Forms.TabPage IN __TabControl:TabPages
			IF oPage:Name == (STRING) symTabName
				RETURN __TabControl:TabPages:IndexOf(oPage)
			ENDIF
		NEXT
		RETURN -1

 /// <exclude />
	METHOD __GetIndexFromPage(oTab)
		LOCAL dwI, dwCount AS DWORD
		dwCount := ALen(aPages)
		for dwI := 1 upto dwCount
			if aPages[dwI][ TAB_PAGE] == oTab
				local symName as symbol
				symName := aPages[dwI][ TAB_SYMBOL]
				return __TabControl:TabPages:IndexOfKey((string) symName)
			ENDIF
		NEXT

		RETURN -1

 /// <exclude />
	METHOD __GetPageFromIndex(nTabIndex AS LONG) AS OBJECT STRICT
		LOCAL uPage AS USUAL
		LOCAL dwI AS LONG
		IF nTabIndex >= 0 .and. nTabIndex < __TabControl:TabPages:Count
			LOCAL symName AS SYMBOL
			LOCAL oPage AS System.Windows.Forms.TabPage
			LOCAL nIndex AS LONG
			// Retrieve Symbol from the TabControl:TabPages collection
			oPage   := __TabControl:TabPages[nTabIndex]
			symName := (SYMBOL) oPage:Name
			nIndex  := __TabControl:TabPages:IndexOf(oPage)
			// Find Page object in aPages
			FOR dwI := 1 TO alen(aPages)
				local aElement := aPages[dwI] as array
				if aElement[TAB_SYMBOL] == symName
					uPage := aElement[TAB_PAGE]
					IF IsSymbol(uPage)
						uPage := self:CreatePageInstance(uPage, aElement[TAB_SYMBOL])
						IF uPage != NULL_OBJECT
							aElement[TAB_PAGE] := uPage
							SELF:__SetTabOwner(nIndex, uPage)
						ENDIF
					ENDIF
					RETURN uPage
				ENDIF
			NEXT
		ENDIF
		RETURN NULL_OBJECT

 /// <exclude />
	METHOD __GetPageFromSymbol(symTabName)
		local dwI, dwCount as dword
		dwCount := ALen(aPages)
		for dwI := 1 upto dwCount
			if aPages[dwI][ TAB_SYMBOL] == symTabName
				return aPages[dwI][TAB_PAGE]
			ENDIF
		NEXT

		return null_object

 /// <exclude />
	METHOD __GetSymbolFromIndex(nTabIndex AS LONG) AS SYMBOL STRICT
		IF nTabIndex >= 0 .and. nTabIndex < __TabControl:TabPages:Count
			LOCAL oPage AS System.Windows.Forms.TabPage
			oPage := __TabControl:TabPages[nTabIndex]
			RETURN (SYMBOL) oPage:Name
		ENDIF
		RETURN NULL_SYMBOL


 /// <exclude />
	METHOD __GetSymbolFromPage(oTab)
		local dwI, dwCount as dword

		dwCount := ALen(aPages)
		for dwI := 1 upto dwCount
			if aPages[dwI][ TAB_PAGE] == oTab
				return aPages[dwI][TAB_SYMBOL]
			ENDIF
		NEXT
		RETURN NULL_SYMBOL

	METHOD __GetTabPage(symIndex as Symbol) as System.Windows.Forms.TabPage
		LOCAL i as LONG
		LOCAL oPage AS System.Windows.Forms.TabPage
		IF SELF:ValidateControl()
			i := SELF:__GetIndexFromSymbol(symIndex)
			IF i !=-1
				oPage := __TabControl:TabPages[i]
			ENDIF
		ENDIF
		RETURN oPage


	METHOD __SetTabOwner(nIndex AS INT, xPage AS USUAL) AS VOID
		LOCAL oTabPage AS System.Windows.Forms.TabPage
		LOCAL oWin AS Window
		LOCAL oPanel AS VOPanel
		IF SELF:ValidateControl() .and. IsObject(xPage)
			oTabPage := __TabControl:TabPages[nIndex]
            oTabPage:Margin := System.Windows.Forms.Padding{0}

			oWin := xPage
			if oWin is DataWindow var oDW
				oPanel := (VOPanel) oDw:__Frame
				oPanel:Dock := System.Windows.Forms.DockStyle.Fill
				oTabPage:Controls:Add(oPanel)
				oPanel := (VOPanel) (OBJECT) oDw:__Surface
			elseif oWin is DialogWindow var oDlg
				oPanel := (VOPanel) (object) oDlg:__Surface
				oPanel:Dock := System.Windows.Forms.DockStyle.Fill
				oTabPage:Controls:Add(oPanel)
			ELSE
				IF !oWin:__HasSurface
					oPanel		:= VOSurfacePanel{oWin}
					oPanel:Dock := System.Windows.Forms.DockStyle.Fill
					oPanel:Visible := TRUE
					foreach oC as System.Windows.Forms.Control in oWin:__Form:Controls
						oPanel:Controls:Add(oC)
					NEXT
				ELSE
					oPanel := (VOPanel) (OBJECT) oWin:__Surface
				ENDIF
				oTabPage:Controls:Add(oPanel)
			ENDIF
			oTabPage:AutoSize := TRUE
		ENDIF
		SELF:__AdjustIndices()
		RETURN

/// <include file="Gui.xml" path="doc/TabControl.AddTipText/*" />
	METHOD AddTipText(symTabName, cText)
		IF ! SELF:ChangeTipText(symTabName, cText)
			AAdd(aTipsText, {symTabName, cText})
		ENDIF
		RETURN NIL

/// <include file="Gui.xml" path="doc/TabControl.AppendTab/*" />
	METHOD AppendTab(symTabName, cCaption, xPage, nImage)
		LOCAL nIndex		AS INT
		LOCAL lReturnValue	AS LOGIC
		LOCAL cTooltip AS STRING
		//LOCAL hFirst, hBefore AS PTR

		DEFAULT( ref nImage, 0)


		// Fill out the tab structure with the arguments passed in
		if xPage is Window var oWin
			//PP-030909 XP theme background on tab page
			oWin:EnableThemeDialogTexture(ETDT_ENABLETAB)

			if Empty(cCaption) .and. oWin:HyperLabel != null_object
				cCaption := oWin:HyperLabel:Caption
			ENDIF
		ELSEIF ! IsSymbol(xPage)
			xPage := symTabName
		ENDIF

		cTooltip := SELF:GetTipText(symTabName)
		IF SELF:ValidateControl()
			// Append the new tab and add its page to the list of pages
			__TabControl:TabPages:Add((STRING)symTabName, (STRING)cCaption, (INT)nImage-1)
			nIndex := __TabControl:TabPages:IndexOfKey((STRING)symTabName)
			IF nIndex != -1
				AAdd(aPages, {symTabName, nIndex, xPage, __TabControl:TabPages[nIndex]})
				SELF:__SetTabOwner(nIndex, xPage)
				IF ! Empty(cTooltip)
					SELF:ChangeTipText(symTabName, cTooltip)
				ENDIF
				lReturnValue := TRUE
			ENDIF

			IF lAutoSize .AND. ! IsSymbol(xPage)
				SELF:__CalcNewDimension(xPage)
			ENDIF
		ENDIF
		SELF:__AdjustIndices()
		RETURN lReturnValue

/// <include file="Gui.xml" path="doc/TabControl.AutoSize/*" />
	ACCESS AutoSize AS LOGIC
		RETURN lAutoSize

/// <include file="Gui.xml" path="doc/TabControl.AutoSize/*" />
	ASSIGN AutoSize(lNewValue AS LOGIC)
		lAutoSize := lNewValue
		RETURN

/// <include file="Gui.xml" path="doc/TabControl.ChangeTipText/*" />
	METHOD ChangeTipText(symTabName, cText)
		LOCAL oPage AS System.Windows.Forms.TabPage
		LOCAL dwIndex AS DWORD
		LOCAL dwCount AS DWORD
		LOCAL symName AS SYMBOL
		IF IsLong(symTabName)
			symName := SELF:__GetSymbolFromIndex(symTabName)
		ELSE
			symName := symTabName
		ENDIF
		oPage := SELF:__GetTabPage(symTabName)

		IF oPage != NULL_OBJECT
			oPage:ToolTipText := cText
			RETURN TRUE
		ELSE
			dwCount := ALen(aTipsText)
			FOR dwIndex := 1 UPTO dwCount
				IF aTipsText[dwIndex][ TIP_SYMBOL] == symName
					aTipsText[dwIndex][ TIP_TEXT] := cText
					RETURN TRUE
				ENDIF
			NEXT
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/TabControl.CreatePageInstance/*" />
	METHOD CreatePageInstance(symPageClass, symTabName)
		LOCAL oPage AS OBJECT
		oPage := CreateInstance(symPageClass, (OBJECT) SELF:Owner)
		if oPage is Window var oWin
			oWin:EnableThemeDialogTexture(ETDT_ENABLETAB)
		ENDIF

		IF lAutoSize
			SELF:__CalcNewDimension(oPage)
		ENDIF

		IF IsMethod(oPage, #TabInit)
			Send(oPage, #TabInit, symTabName)
		ENDIF

		RETURN oPage

/// <include file="Gui.xml" path="doc/TabControl.CurrentPage/*" />
	ACCESS CurrentPage AS OBJECT
		LOCAL nCurrentIndex AS LONG
		IF oCurrentPage == NULL_OBJECT
			IF SELF:ValidateControl()
				nCurrentIndex := SELF:__TabControl:SelectedIndex +1
				oCurrentPage := SELF:__GetPageFromIndex(nCurrentIndex)
			ENDIF
		ENDIF
		RETURN oCurrentPage

/// <include file="Gui.xml" path="doc/TabControl.DeleteAllTabs/*" />
	METHOD DeleteAllTabs() AS LOGIC
		local dwI, dwCount as dword
		dwCount := ALen(aPages)
		for dwI := 1 upto dwCount
			if IsObject(aPages[dwI][ TAB_PAGE])
				((VObject) aPages[dwI][ TAB_PAGE]):Destroy()
			ENDIF
		NEXT

		aPages := {}
		aTipsText := {}
		IF SELF:ValidateControl()
			__TabControl:TabPages:Clear()
		ENDIF
		RETURN TRUE

/// <include file="Gui.xml" path="doc/TabControl.DeleteTab/*" />
	METHOD DeleteTab(symTabName AS SYMBOL)  AS LOGIC
		// LOCAL dwI, dwCount AS DWORD
		LOCAL iTabIdx AS DWORD
		LOCAL iFocus  as DWORD
		LOCAL lRet AS LOGIC
		LOCAL oTabPage		  AS System.Windows.Forms.TabPage
		IF SELF:ValidateControl()
			for iTabIdx := 1 to alen(self:aPages)
				if self:aPages[iTabIdx][ TAB_SYMBOL] == symTabName
					oTabPage := self:aPages[iTabIdx][ TAB_PAGE]
					SELF:__TabControl:TabPages:Remove(oTabPage)
					ADel(SELF:aPages, iTabIdx)
					Asize(SELF:aPages, Alen(aPages)-1)
					iFocus := iTabIdx-1
					EXIT
				ENDIF
			NEXT
			SELF:RemoveTipText(symTabName)
			oCurrentPage := NULL_OBJECT
			IF SELF:TabCount > 0
				iFocus := iFocus -1
				IF iFocus <  0 .or. iFocus >= SELF:TabCount
					iFocus := 0
				ENDIF
				__TabControl:SelectedIndex := (INT) iFocus
				SELF:__FocusPage((INT) iFocus)
			ENDIF
		ENDIF
		SELF:__AdjustIndices()
		RETURN lRet

/// <include file="Gui.xml" path="doc/TabControl.Destroy/*" />
	METHOD Destroy() AS USUAL clipper
		aPages := NULL_ARRAY
		aTipsText := NULL_ARRAY
		oCurrentPage := NULL_OBJECT
		SUPER:Destroy()
		RETURN NIL


/// <include file="Gui.xml" path="doc/TabControl.GetTabBoundingBox/*" />
	METHOD GetTabBoundingBox(symTabName AS SYMBOL) AS BoundingBox
		LOCAL oOrigin	AS Point
		LOCAL oSize		AS Dimension
		LOCAL oPage   AS System.Windows.Forms.TabPage
		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			oOrigin := oPage:Location
			oSize   := oPage:Size
			RETURN BoundingBox{oOrigin, oSize}
		ENDIF
		RETURN NULL_OBJECT

	METHOD GetCaption(symTabName AS SYMBOL) AS STRING
		LOCAL oPage   AS System.Windows.Forms.TabPage
		LOCAL cReturn AS STRING
		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			cReturn := oPage:Text
		ENDIF
		RETURN cReturn

/// <include file="Gui.xml" path="doc/TabControl.GetTabImage/*" />
	METHOD GetTabImage (symTabName AS SYMBOL)  AS INT
		LOCAL oPage         AS System.Windows.Forms.TabPage
		LOCAL nImageIndex	AS INT

		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			nImageIndex := oPage:ImageIndex
		ENDIF
		RETURN nImageIndex


/// <include file="Gui.xml" path="doc/TabControl.GetTabPage/*" />
	METHOD GetTabPage(xSymbolOrPosition AS USUAL)  AS OBJECT
		IF IsSymbol(xSymbolOrPosition)
			RETURN SELF:__GetPageFromIndex(SELF:__GetIndexFromSymbol(xSymbolOrPosition))
		ELSEIF IsNumeric(xSymbolOrPosition)
			RETURN SELF:__GetPageFromIndex(xSymbolOrPosition-1)
		ENDIF

		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TabControl.GetTipText/*" />
	METHOD GetTipText(symTabName AS USUAL)  AS STRING
		LOCAL dwIndex AS DWORD
		LOCAL dwCount AS DWORD
		LOCAL symName AS SYMBOL

		LOCAL oPage         AS System.Windows.Forms.TabPage

		IF IsLong(symTabName)
			symName := SELF:__GetSymbolFromIndex(symTabName)
		ELSE
			symName := symTabName
		ENDIF

		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			RETURN oPage:ToolTipText
		ENDIF

		dwCount := ALen(aTipsText)
		FOR dwIndex := 1 UPTO dwCount
			IF aTipsText[dwIndex][ TIP_SYMBOL] == symName
				RETURN aTipsText[dwIndex][ TIP_TEXT]
			ENDIF
		NEXT

		RETURN NULL_STRING

/// <include file="Gui.xml" path="doc/TabControl.Hide/*" />
	METHOD Hide()  AS VOID STRICT
		SUPER:Hide()
		IF (oCurrentPage != NULL_OBJECT)
			oCurrentPage:Hide()
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/TabControl.ImageList/*" />
	ACCESS ImageList AS ImageList
		RETURN oImageList

/// <include file="Gui.xml" path="doc/TabControl.ImageList/*" />
	ASSIGN ImageList(oNewImageList AS ImageList)
		IF SELF:ValidateControl()
			oImageList := oNewImageList
			__TabControl:ImageList := oImageList
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/TabControl.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)
		if xID is ResourceID
			SUPER(oOwner, xID, oPoint, oDimension, , kStyle, FALSE)
		ELSE
			SUPER(oOwner, xID, oPoint, oDimension, WC_TABCONTROL, kStyle, FALSE)
		ENDIF

		self:SetStyle(_Or(WS_CHILD, WS_VISIBLE))
		self:SetStyle(WS_BORDER, false)
		lAutoSize := FALSE
		aPages := {}
		aTipsText := {}

		RETURN

/// <include file="Gui.xml" path="doc/TabControl.InsertTab/*" />
	METHOD InsertTab(nPosition, symTabName, cCaption, xPage, nImage)
		LOCAL nIndex		AS INT
		LOCAL lReturnValue	AS LOGIC
		LOCAL iLen, i AS DWORD
		LOCAL cTooltip AS STRING

		DEFAULT( ref nImage, 0)
		IF SELF:ValidateControl()

			cTooltip := SELF:GetTipText(symTabName)

			// Insert the new tab and add its page to the list of pages
			IF nPosition > 0
				nPosition := nPosition -1
				IF nPosition > __TabControl:TabPages:Count
					nPosition := __TabControl:TabPages:Count
				ENDIF
			ELSE
				nPosition := 0
			ENDIF
			IF nImage > 0
				__TabControl:TabPages:Insert((INT) nPosition, (STRING) symTabName, (STRING) cCaption, (INT) nImage-1)
			ELSE
				__TabControl:TabPages:Insert((INT) nPosition, (STRING) symTabName, (STRING) cCaption )
			ENDIF

			nIndex := __TabControl:TabPages:IndexOfKey((STRING) symTabName)
			IF nIndex != -1
				iLen := ALen(aPages)
				FOR i := 1 TO iLen
					IF aPages[i][2] >= nIndex
						aPages[i][2] := aPages[i][2] + 1
					ENDIF
				NEXT
				AAdd(aPages, {symTabName, nIndex, xPage, __TabControl:TabPages[nIndex]})
				IF ! Empty(cTooltip)
					SELF:ChangeTipText(symTabName, cTooltip)
				ENDIF
				SELF:__SetTabOwner(nIndex, xPage)
				lReturnValue := TRUE
			ENDIF

			IF lAutoSize .AND. ! IsSymbol(xPage)
				SELF:__CalcNewDimension(xPage)
			ENDIF
		ENDIF
		SELF:__AdjustIndices()
		RETURN lReturnValue


/// <include file="Gui.xml" path="doc/TabControl.IsTabPage/*" />
	METHOD IsTabPage(iPos AS LONG) AS LOGIC
		RETURN iPos >= 0 .and. iPos < __TabControl:TabPages:Count


/// <include file="Gui.xml" path="doc/TabControl.IsTabPage/*" />
	METHOD IsTabPage(symPos AS SYMBOL) AS LOGIC
		local dwI, dwCount as dword
		dwCount := ALen(aPages)
		for dwI := 1 upto dwCount
			if aPages[dwI][ TAB_SYMBOL] == symPos
				if aPages[dwI][ TAB_PAGE] IS Window
					RETURN TRUE
				ENDIF
				EXIT
			ENDIF
		NEXT
		RETURN FALSE

/// <include file="Gui.xml" path="doc/TabControl.IsTabPage/*" />
	METHOD IsTabPage(xSymbolOrPosition AS USUAL) AS LOGIC
		IF IsSymbol(xSymbolOrPosition)
			RETURN SELF:IsTabPage((SYMBOL) xSymbolOrPosition)
		ELSEIF IsNumeric(xSymbolOrPosition)
			RETURN SELF:IsTabPage((LONG) xSymbolOrPosition)
		ELSE
			RETURN FALSE
		ENDIF

/// <include file="Gui.xml" path="doc/TabControl.Move/*" />
	METHOD Move(oMoveEvent as MoveEvent) AS VOID
		SELF:__AdjustPage()
		return

/// <include file="Gui.xml" path="doc/TabControl.PadTabs/*" />
	METHOD PadTabs(dwWidth AS INT, dwHeight AS INT) AS VOID
		IF SELF:ValidateControl()
			__TabControl:Padding := Point{dwWidth, dwHeight}
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/TabControl.RemoveTabImage/*" />
	METHOD RemoveTabImage(nImageIndex AS INT)  AS LOGIC
		IF SELF:oImageList != NULL_OBJECT .and. SELF:oImageList:ImageCount >= nImageIndex
			self:oImageList:__ImageList:Images[(int) nImageIndex -1] := (System.Drawing.Image)  System.Drawing.Bitmap{self:Size:Width, self:Size:Height}
			RETURN TRUE
		ENDIF
		RETURN  FALSE

/// <include file="Gui.xml" path="doc/TabControl.RemoveTipText/*" />
	METHOD RemoveTipText(symTabName AS USUAL)
		LOCAL dwIndex AS DWORD
		LOCAL dwCount AS DWORD
		LOCAL symName AS SYMBOL
		LOCAL oPage			AS System.Windows.Forms.TabPage

		IF IsLong(symTabName)
			symName := SELF:__GetSymbolFromIndex(symTabName)
		ELSE
			symName := symTabName
		ENDIF
		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			oPage:ToolTipText := NULL_STRING
		ENDIF

		dwCount := ALen(aTipsText)
		FOR dwIndex := 1 UPTO dwCount
			IF aTipsText[dwIndex][ TIP_SYMBOL] == symName
				AtrueDel(aTipsText, dwIndex)
				RETURN TRUE
			ENDIF
		NEXT

		RETURN FALSE

/// <include file="Gui.xml" path="doc/TabControl.Resize/*" />
	METHOD Resize(oResizeEvent as ResizeEvent) AS VOID
		SELF:__AdjustPage()
		return

/// <include file="Gui.xml" path="doc/TabControl.RowCount/*" />
	ACCESS RowCount AS INT
		IF SELF:__isValid
			RETURN __TabControl:RowCount
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/TabControl.SelectedTab/*" />
	ACCESS SelectedTab AS SYMBOL
		RETURN SELF:__GetSymbolFromIndex(__TabControl:SelectedIndex)

/// <include file="Gui.xml" path="doc/TabControl.SelectedTabPage/*" />
	ACCESS SelectedTabPage  AS OBJECT
		LOCAL sTabPage AS SYMBOL
		sTabPage := SELF:SelectedTab
		IF (sTabPage == NULL_SYMBOL)
			RETURN NULL_OBJECT
		ENDIF
		RETURN SELF:GetTabPage(sTabPage)

/// <include file="Gui.xml" path="doc/TabControl.SelectTab/*" />
	METHOD SelectTab(symTabName AS SYMBOL)
		LOCAL oPage		AS System.Windows.Forms.TabPage
		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			__TabControl:SelectedIndex := -1
			__TabControl:SelectedTab := oPage

		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/TabControl.SetCaption/*" />
	METHOD SetCaption(symTabName AS SYMBOL, cCaption AS STRING) AS LOGIC
		LOCAL lRet		AS LOGIC
		LOCAL oPage		AS System.Windows.Forms.TabPage
		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			oPage:Text := cCaption
			lRet := TRUE
		ENDIF

		RETURN lRet

/// <include file="Gui.xml" path="doc/TabControl.SetTabImage/*" />
	METHOD SetTabImage(symTabName AS SYMBOL, nImageIndex AS INT) AS INT
		LOCAL oPage		AS System.Windows.Forms.TabPage

		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			oPage:ImageIndex := nImageIndex
		ENDIF

		RETURN nImageIndex

/// <include file="Gui.xml" path="doc/TabControl.SetTipText/*" />
	METHOD SetTipText(symTabName AS SYMBOL, cText AS STRING)
		RETURN SELF:AddTipText(symTabName, cText)

/// <include file="Gui.xml" path="doc/TabControl.Show/*" />
	METHOD Show() AS VOID CLIPPER
		SUPER:Show()
		if oCurrentPage is Window var oWin
			oWin:Show()
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/TabControl.TabCaption/*" />

	PROPERTY TabCaption [symTabName AS SYMBOL] AS STRING GET SELF:GetCaption(symTabName) SET SELF:SetCaption(symTabName, Value)

/// <include file="Gui.xml" path="doc/TabControl.TabCount/*" />
	ACCESS TabCount  AS LONG
		IF SELF:__IsValid
			RETURN __TabControl:TabCount
		ENDIF
		RETURN 0
END CLASS

#region defines
DEFINE iPageBorder := 2
#endregion
