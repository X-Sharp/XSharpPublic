


STATIC DEFINE TAB_SYMBOL := 1
STATIC DEFINE TAB_INDEX  := 2
STATIC DEFINE TAB_PAGE   := 3
STATIC DEFINE TIP_SYMBOL := 1
STATIC DEFINE TIP_TEXT   := 2


CLASS TabControl INHERIT TextControl
	PROTECT oImageList		 AS ImageList
	PROTECT aPages			 AS ARRAY
	PROTECT aTipsText		 AS ARRAY
	PROTECT nMaxHeight		 AS INT
	PROTECT nMaxWidth		 AS INT
	PROTECT lAutoSize        AS LOGIC
	PROTECT oCurrentPage	 AS OBJECT

    PROPERTY ControlType AS Controltype GET Controltype.TabControl

	METHOD OnControlCreated(oC AS System.Windows.Forms.Control) AS VOID
		oC:Resize += OnReSize
		RETURN 

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
		dwCount := (LONG) Alen(aPages)
		FOR dwI := dwCount DOWNTO 1
			LOCAL aElement AS ARRAY
			aElement := aPages[dwI]
			nPos := __TabControl:TabPages:IndexOfKey( (STRING) aElement[TAB_SYMBOL])
			IF nPos >= 0
				aElement[TAB_INDEX] := nPos
			ELSE
				IF IsObject(aElement[TAB_PAGE])
					Send(aElement[TAB_PAGE], #Destroy)
				ENDIF
				ADel(aPages, (DWORD) dwI)
				aSize(aPages, Alen(aPages)-1)
			ENDIF
		NEXT
		

	METHOD __AdjustPage() AS VOID STRICT 
		// The FramePanel for a DataWindow sizes automatically
		// The SurfacePanel must be changed here, because we may want it to grow automatically
		// Todo: Fix this
		RETURN

	METHOD __CalcNewDimension(oNewPage AS OBJECT) AS VOID STRICT 
		// No need to resize, DockStyle.Fill
		RETURN

	METHOD __FocusPage(nIndex AS INT) AS VOID STRICT 
		
		LOCAL oOldPage        AS Window
		LOCAL lSelfFocus      AS LOGIC
		LOCAL oTabPage		  as System.Windows.Forms.TabPage
		LOCAL oPanel			AS VOPanel
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
			IF IsInstanceOf(oCurrentPage, #DataWindow)
				LOCAL oDw := oCurrentPage AS DataWindow
				oPanel := oDw:__Frame
			ELSE
				oPanel := oCurrentPage:__Surface
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

	METHOD __GetIndexFromSymbol(symTabName AS SYMBOL) AS INT STRICT 
		FOREACH oPage AS System.Windows.Forms.TabPage IN __TabControl:TabPages
			IF oPage:Name == (STRING) symTabName
				RETURN __TabControl:TabPages:IndexOf(oPage)
			ENDIF
		NEXT
		RETURN -1
	
	METHOD __GetIndexFromPage(oTab) 
		LOCAL dwI, dwCount AS DWORD
		dwCount := ALen(aPages)
		FOR dwI := 1 UPTO dwCount
			IF aPages[dwI][ TAB_PAGE] == oTab
				LOCAL symName AS SYMBOL
				symName := aPages[dwI][ TAB_SYMBOL]
				RETURN __TabControl:TabPages:IndexOfKey((STRING) symName)
			ENDIF
		NEXT  

		RETURN -1

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
				LOCAL aElement := aPages[dwI] AS ARRAY
				IF aElement[TAB_SYMBOL] == symName
					uPage := aElement[TAB_PAGE]
					IF IsSymbol(uPage)
						uPage := SELF:CreatePageInstance(uPage, aElement[TAB_SYMBOL])
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

	METHOD __GetPageFromSymbol(symTabName) 
		LOCAL dwI, dwCount AS DWORD
		dwCount := ALen(aPages)
		FOR dwI := 1 UPTO dwCount
			IF aPages[dwI][ TAB_SYMBOL] == symTabName
				RETURN aPages[dwI][TAB_PAGE]
			ENDIF
		NEXT  

		RETURN NULL_OBJECT

	METHOD __GetSymbolFromIndex(nTabIndex AS LONG) AS SYMBOL STRICT 
		IF nTabIndex >= 0 .and. nTabIndex < __TabControl:TabPages:Count
			LOCAL oPage AS System.Windows.Forms.TabPage
			oPage := __TabControl:TabPages[nTabIndex]
			RETURN (SYMBOL) oPage:Name
		ENDIF
		RETURN NULL_SYMBOL


	METHOD __GetSymbolFromPage(oTab) 
		LOCAL dwI, dwCount AS DWORD

		dwCount := ALen(aPages)
		FOR dwI := 1 UPTO dwCount
			IF aPages[dwI][ TAB_PAGE] == oTab
				RETURN aPages[dwI][TAB_SYMBOL]
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
			IF IsInstanceOf(oWin, #DataWindow)
				LOCAL oDw := (DataWindow) oWin AS DataWindow
				oPanel := (VOPanel) oDw:__Frame
				oPanel:Dock := System.Windows.Forms.DockStyle.Fill
				oTabPage:Controls:Add(oPanel)
				oPanel := (VOPanel) (OBJECT) oDw:__Surface
			ELSEIF IsInstanceOf(oWin, #DialogWindow)
				LOCAL oDlg := (DialogWindow) oWin AS DialogWindow
				oPanel := (VOPanel) (OBJECT) oDlg:__Surface
				oPanel:Dock := System.Windows.Forms.DockStyle.Fill
				oTabPage:Controls:Add(oPanel)
			ELSE
				IF !oWin:__HasSurface
					oPanel		:= VOSurfacePanel{oWin}
					oPanel:Dock := System.Windows.Forms.DockStyle.Fill
					oPanel:Visible := TRUE
					FOREACH IMPLIED oC IN oWin:__Form:Controls
						oPanel:Controls:AddRange(oC)
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
		
	METHOD AddTipText(symTabName, cText) 
		IF ! SELF:ChangeTipText(symTabName, cText)
			AAdd(aTipsText, {symTabName, cText})
		ENDIF
		RETURN NIL

	METHOD AppendTab(symTabName, cCaption, xPage, nImage) 
		LOCAL nIndex		AS INT
		LOCAL lReturnValue	AS LOGIC
		LOCAL cTooltip AS STRING
		//LOCAL hFirst, hBefore AS PTR

		DEFAULT(@nImage, 0)


		// Fill out the tab structure with the arguments passed in
		IF IsInstanceOfUsual(xPage,#Window) 
			//PP-030909 XP theme background on tab page
			((Window)xPage):EnableThemeDialogTexture(ETDT_ENABLETAB)
			
			IF Empty(cCaption) .AND. (((Window)xPage):HyperLabel != NULL_OBJECT)
				cCaption := ((Window)xPage):HyperLabel:Caption
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

	ACCESS AutoSize AS LOGIC
		RETURN lAutoSize

	ASSIGN AutoSize(lNewValue AS LOGIC) 
		lAutoSize := lNewValue
		RETURN 

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

	METHOD CreatePageInstance(symPageClass, symTabName) 
		LOCAL oPage AS OBJECT
		oPage := CreateInstance(symPageClass, (OBJECT) SELF:Owner)
		IF IsInstanceOfUsual(oPage, #Window)
			((Window)oPage):EnableThemeDialogTexture(ETDT_ENABLETAB)
		ENDIF

		IF lAutoSize
			SELF:__CalcNewDimension(oPage)
		ENDIF

		IF IsMethod(oPage, #TabInit)
			Send(oPage, #TabInit, symTabName)
		ENDIF

		RETURN oPage

	ACCESS CurrentPage AS OBJECT
		LOCAL nCurrentIndex AS LONG
		IF oCurrentPage == NULL_OBJECT
			IF SELF:ValidateControl()
				nCurrentIndex := SELF:__TabControl:SelectedIndex +1
				oCurrentPage := SELF:__GetPageFromIndex(nCurrentIndex)
			ENDIF
		ENDIF
		RETURN oCurrentPage

	METHOD DeleteAllTabs() AS LOGIC
		LOCAL dwI, dwCount AS DWORD
		dwCount := ALen(aPages)
		FOR dwI := 1 UPTO dwCount
			IF IsObject(aPages[dwI][ TAB_PAGE])
				((VObject) aPages[dwI][ TAB_PAGE]):Destroy()
			ENDIF
		NEXT  

		aPages := {}
		aTipsText := {}
		IF SELF:ValidateControl()
			__TabControl:TabPages:Clear()
		ENDIF
		RETURN TRUE

	METHOD DeleteTab(symTabName AS SYMBOL)  AS LOGIC
		// LOCAL dwI, dwCount AS DWORD
		LOCAL iTabIdx AS DWORD
		LOCAL iFocus  as DWORD
		LOCAL lRet AS LOGIC
		LOCAL oTabPage		  AS System.Windows.Forms.TabPage
		IF SELF:ValidateControl()
			FOR iTabIdx := 1 TO alen(SELF:aPages)
				IF SELF:aPages[iTabIdx][ TAB_SYMBOL] == symTabName
					oTabPage := SELF:aPages[iTabIdx][ TAB_PAGE]				
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

	METHOD Destroy() AS USUAL CLIPPER
		aPages := NULL_ARRAY
		aTipsText := NULL_ARRAY
		oCurrentPage := NULL_OBJECT
		SUPER:Destroy()
		RETURN NIL


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

	METHOD GetTabImage (symTabName AS SYMBOL)  AS INT
		LOCAL oPage         AS System.Windows.Forms.TabPage
		LOCAL nImageIndex	AS INT
		
		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			nImageIndex := oPage:ImageIndex
		ENDIF
		RETURN nImageIndex


	METHOD GetTabPage(xSymbolOrPosition AS USUAL)  AS OBJECT
		IF IsSymbol(xSymbolOrPosition)
			RETURN SELF:__GetPageFromIndex(SELF:__GetIndexFromSymbol(xSymbolOrPosition))
		ELSEIF IsNumeric(xSymbolOrPosition)
			RETURN SELF:__GetPageFromIndex(xSymbolOrPosition-1)
		ENDIF

		RETURN NULL_OBJECT

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

	METHOD Hide() 
		SUPER:Hide()
		IF (oCurrentPage != NULL_OBJECT)
			oCurrentPage:Hide()
		ENDIF
		RETURN NIL

	ACCESS ImageList AS ImageList
		RETURN oImageList

	ASSIGN ImageList(oNewImageList AS ImageList) 
		IF ValidateControl()
			oImageList := oNewImageList
			__TabControl:ImageList := oImageList:__ImageList
		ENDIF
		RETURN 

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
		IF IsInstanceOfUsual(xID, #ResourceID)
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

	METHOD InsertTab(nPosition, symTabName, cCaption, xPage, nImage) 
		LOCAL nIndex		AS INT
		LOCAL lReturnValue	AS LOGIC
		LOCAL iLen, i AS DWORD
		LOCAL cTooltip AS STRING

		DEFAULT(@nImage, 0)
		IF ValidateControl()
		
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


	METHOD IsTabPage(iPos AS LONG) AS LOGIC
		RETURN iPos >= 0 .and. iPos < __TabControl:TabPages:Count

	METHOD IsTabPage(symPos AS SYMBOL) AS LOGIC
		LOCAL dwI, dwCount AS DWORD
		dwCount := ALen(aPages)
		FOR dwI := 1 UPTO dwCount 
			IF aPages[dwI][ TAB_SYMBOL] == symPos
				IF IsInstanceOfUsual(aPages[dwI][ TAB_PAGE], #Window)
					RETURN TRUE
				ENDIF    
				EXIT
			ENDIF
		NEXT
		RETURN FALSE


	METHOD IsTabPage(xSymbolOrPosition AS USUAL) AS LOGIC
		IF IsSymbol(xSymbolOrPosition)
			RETURN SELF:IsTabPage((SYMBOL) xSymbolOrPosition)
		ELSEIF IsNumeric(xSymbolOrPosition)
			RETURN SELF:IsTabPage((LONG) xSymbolOrPosition)
		ELSE
			RETURN FALSE
		ENDIF

	METHOD Move(oMoveEvent) 
		SELF:__AdjustPage()
		RETURN NIL

	METHOD PadTabs(dwWidth AS INT, dwHeight AS INT) AS VOID
		IF ValidateControl()
			__TabControl:Padding := Point{dwWidth, dwHeight}
		ENDIF
		RETURN 

	METHOD RemoveTabImage(nImageIndex AS INT)  AS LOGIC
		IF SELF:oImageList != NULL_OBJECT .and. SELF:oImageList:ImageCount >= nImageIndex
			SELF:oImageList:__ImageList:Images[(INT) nImageIndex -1] := (OBJECT) System.Drawing.Bitmap{SELF:Size:Width, SELF:Size:Height}
			RETURN TRUE
		ENDIF
		RETURN  FALSE

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
		oPage := __GetTabPage(symTabName)
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

	METHOD Resize(oResizeEvent) 
		SELF:__AdjustPage()
		RETURN SELF

	ACCESS RowCount AS INT
		IF SELF:__isValid
			RETURN __TabControl:RowCount
		ENDIF
		RETURN 0

	ACCESS SelectedTab AS SYMBOL
		RETURN SELF:__GetSymbolFromIndex(__TabControl:SelectedIndex)

	ACCESS SelectedTabPage  AS OBJECT
		LOCAL sTabPage AS SYMBOL
		sTabPage := SELF:SelectedTab
		IF (sTabPage == NULL_SYMBOL)
			RETURN NULL_OBJECT
		ENDIF
		RETURN SELF:GetTabPage(sTabPage)

	METHOD SelectTab(symTabName AS SYMBOL) 
		LOCAL oPage		AS System.Windows.Forms.TabPage
		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			__TabControl:SelectedIndex := -1
			__TabControl:SelectedTab := oPage

		ENDIF
		RETURN SELF

	METHOD SetCaption(symTabName AS SYMBOL, cCaption AS STRING) AS LOGIC
		LOCAL lRet		AS LOGIC
		LOCAL oPage		AS System.Windows.Forms.TabPage
		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			oPage:Text := cCaption
			lRet := TRUE
		ENDIF

		RETURN lRet

	METHOD SetTabImage(symTabName AS SYMBOL, nImageIndex AS INT) AS INT
		LOCAL oPage		AS System.Windows.Forms.TabPage

		oPage := SELF:__GetTabPage(symTabName)
		IF oPage != NULL_OBJECT
			oPage:ImageIndex := nImageIndex
		ENDIF

		RETURN nImageIndex

	METHOD SetTipText(symTabName AS SYMBOL, cText AS STRING) 
		RETURN SELF:AddTipText(symTabName, cText)

	METHOD Show() AS VOID
		SUPER:Show()
		IF (oCurrentPage != NULL_OBJECT)
			oCurrentPage:Show()
		ENDIF
		RETURN 
		
	PROPERTY TabCaption [symTabName AS SYMBOL] AS STRING GET SELF:GetCaption(symTabName) SET SELF:SetCaption(symTabName, Value)

	ACCESS TabCount  AS LONG
		IF SELF:__IsValid
			RETURN __TabControl:TabCount
		ENDIF
		RETURN 0
END CLASS

#region defines
DEFINE iPageBorder := 2
#endregion
