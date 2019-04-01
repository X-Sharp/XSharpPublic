CLASS ListView INHERIT TextControl
	PROTECT aColumns AS ARRAY
	PROTECT aDeleted AS ARRAY
	PROTECT oLargeImageList AS ImageList
	PROTECT oSmallImageList AS ImageList
	PROTECT oStateImageList AS ImageList
	PROTECT oDragImageList AS ImageList
	PROTECT lDragDropEnabled AS LOGIC
	PROTECT symSortRoutineName AS SYMBOL

	//PP-030828 Strong typing
	METHOD __CreateDragImageList(nItem AS DWORD) AS OBJECT STRICT 
	//PP-030828 Strong typing
	LOCAL hImageList AS PTR
	LOCAL strucPoint IS _winPoint

	

	// create an ImageList handle from a ListView item
	hImageList := ListView_CreateDragImage(SELF:Handle(), INT(nItem - 1),  @strucPoint)

	// create an ImageList object from the ImageList handle
	IF (hImageList != NULL_PTR)
		RETURN ImageList{hImageList}
	ENDIF
	RETURN NULL_OBJECT

METHOD __GetColumnFromIndex(nColumn AS DWORD) AS ListViewColumn 
	//PP-030828 Strong typing
	RETURN aColumns[nColumn]

METHOD __GetColumnIndexFromSymbol(symColumnName AS SYMBOL) AS DWORD STRICT 
	//PP-030828 Strong typing
	LOCAL dwIndex, iLen AS DWORD
	LOCAL oColumn AS ListViewColumn

	// first attempt to find the entry and replace it
	// return AScan(aColumns, {|x| x:NameSym == symColumnName})
	iLen := ALen(aColumns)
	FOR dwIndex := 1 TO iLen
	    oColumn := aColumns[dwIndex]
		IF (oColumn:NameSym == symColumnName)
			RETURN dwIndex
		ENDIF
	NEXT  // dwIndex

	RETURN 0

METHOD __SetItem(strucItem AS _winLV_Item, oLVItem AS ListViewItem) AS VOID STRICT 
	//SE-060523
	LOCAL nIndex AS INT
	LOCAL dwIndex AS DWORD
	LOCAL oListViewColumn AS ListViewColumn
	LOCAL cText AS STRING
	LOCAL uVal AS USUAL
   LOCAL hHandle AS PTR
   LOCAL liSubImage AS LONGINT

	hHandle := SELF:Handle()

	nIndex := strucItem:iItem

	// set the text/value for each column
	FOR dwIndex := 1 TO SELF:ColumnCount
		oListViewColumn := SELF:__GetColumnFromIndex(dwIndex)
		cText := oLVItem:GetText(oListViewColumn:NameSym, @liSubImage)
		//RvdH 061217 changed
		IF dwIndex == 1
			liSubImage := oLVItem:ImageIndex
		ENDIF
		IF (cText == NULL_STRING)
			// if there is no text available, use the associated usual value
			uValue := oLVItem:GetValue(oListViewColumn:NameSym)
			IF !IsNil(uValue)
				cText := AllTrim(AsString(uValue))
			ENDIF
		ENDIF

	   strucItem:mask := _OR(LVIF_TEXT, LVIF_IMAGE)
	   //strucItem.iItem    := nIndex
		strucItem:iSubItem := LONGINT(_CAST, dwIndex) -1l
		IF liSubImage = 0
         IF strucItem:iSubItem > 0
         	strucItem:iImage := -1l
         ENDIF
		ELSE
		   strucItem:iImage := liSubImage - 1l
		ENDIF

		IF (cText == NULL_STRING)
			strucItem:pszText := NULL_PSZ
			ListView_SetItem(hHandle,  strucItem)
		ELSE
			strucItem:pszText := StringAlloc(cText)
			ListView_SetItem(hHandle,  strucItem)
			MemFree(PTR(_CAST, strucItem:pszText))
		ENDIF
      /*
		IF (cText == NULL_STRING)
			ListView_SetItemText(hHandle, nIndex, INT(_CAST, dwIndex - 1), NULL_PSZ)
		ELSE
			pszText := StringAlloc(cText)
			ListView_SetItemText(hHandle, nIndex, INT(_CAST, dwIndex - 1), pszText)
			MemFree(PTR(_CAST, pszText))
		ENDIF
      */

		//RvdH 060608 optimized
		//IF oLVItem:__lParam != 0 .and. !Empty(oListViewColumn:__ValueList)
		IF oLVItem:__lParam != 0 .AND. Alen(oListViewColumn:__ValueList) > 0
			uVal := oLVItem:GetValue(oListViewColumn:NameSym)
			IF !IsNil(uVal)
			   oListViewColumn:__ValueList[oLVItem:__lParam] := uVal
			ENDIF
		ENDIF
	NEXT  // dwIndex
   
   dwIndex := DWORD(nIndex)
	// set the state and overlay images
	ListView_SetItemState(hHandle, dwIndex, INDEXTOSTATEIMAGEMASK(oLVItem:StateImageIndex), LVIS_STATEIMAGEMASK)
	ListView_SetItemState(hHandle, dwIndex, INDEXTOOVERLAYMASK(oLVItem:OverlayImageIndex), LVIS_OVERLAYMASK)

	// set the visual state of the item
	ListView_SetItemState(hHandle, dwIndex, IIF(oLVItem:Disabled, LVIS_CUT, 0), LVIS_CUT)
	ListView_SetItemState(hHandle, dwIndex, IIF(oLVItem:DropTarget, LVIS_DROPHILITED, 0), LVIS_DROPHILITED)
	ListView_SetItemState(hHandle, dwIndex, IIF(oLVItem:Focused, LVIS_FOCUSED, 0), LVIS_FOCUSED)
	ListView_SetItemState(hHandle, dwIndex, IIF(oLVItem:Selected, LVIS_SELECTED, 0), LVIS_SELECTED)

	RETURN

ACCESS __SortRoutineName AS SYMBOL STRICT 
	//PP-030828 Strong typing
	

	RETURN symSortRoutineName

METHOD AddColumn(oListViewColumn) 
	

	RETURN SELF:InsertColumn(oListViewColumn)

METHOD AddGroup(iGroupId,cGroupName,dwAlign) 
	//PP-030909
	// Listview groups require XP visual styles
	LOCAL pLVGroup IS _WinLVGroup
	LOCAL i AS INT

	DEFAULT(@dwAlign,LVGA_HEADER_LEFT)
	DEFAULT(@iGroupId,-1)

	pLVGroup:cbSize := _SIZEOF(_winLVGroup)
	pLVGroup:mask := _OR(LVGF_HEADER,LVGF_GROUPID,LVGF_ALIGN)
	pLVGroup:pHeader := String2W(cGroupName)
	pLVGroup:iGroupId := iGroupId
	pLVGroup:uAlign := dwALign

 	i := SendMessage(SELF:handle(),LVM_INSERTGROUP,DWORD(_CAST,pLVGroup:iGroupId),LONGINT(_CAST,@pLVGroup))

	SysFreeString(pLVGroup:pHeader)

	RETURN i >= 0

METHOD AddItem(oListViewItem) 
	

	RETURN SELF:InsertItem(oListViewItem)

METHOD Arrange(kAlignment) 
	RETURN LOGIC(_CAST, SendMessage(SELF:Handle(), LVM_ARRANGE, DWORD(kAlignment), 0))

ACCESS BackgroundColor 
	LOCAL oColor AS Color
	
	//PP-031001: Fix color assignment
	oColor := Color{}
	oColor:ColorRef := ListView_GetBkColor(SELF:Handle())
	RETURN oColor

ASSIGN BackgroundColor(oBackgroundColor) 
	ListView_SetBkColor(SELF:Handle(), oBackgroundColor:ColorRef)
	RETURN 

ACCESS CheckBoxes 
	RETURN SELF:GetExLVStyle(LVS_EX_CHECKBOXES)

ASSIGN CheckBoxes(lNewVal) 
	RETURN SELF:SetExLVStyle(LVS_EX_CHECKBOXES, lNewVal)

ACCESS ColumnCount 
	RETURN ALen(aColumns)

ACCESS ColumnOrderArray 
	LOCAL iLen, i AS INT
	LOCAL _pi, pt AS INT PTR
	LOCAL aRet := {} AS ARRAY
	LOCAL oColumn   AS ListViewColumn

	

	iLen := INT(_CAST, ALen(aColumns))
	_pi := pt := MemAlloc(DWORD(iLen * _SIZEOF(INT)))

	IF (SendMessage(SELF:Handle(), LVM_GETCOLUMNORDERARRAY,DWORD(iLen), LONGINT(_CAST, _pi)) > 0)
		FOR i := 1 TO iLen
		    oColumn := aColumns[INT(_pi)+1]
			AAdd(aRet, oColumn:NameSym)
			_pi++
		NEXT
	ENDIF

	MemFree(pt)

	RETURN aRet

ASSIGN ColumnOrderArray(aNew) 
	//SE-060526
	LOCAL dwJ AS DWORD
	LOCAL _pi, pt AS INT PTR
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD

	

	dwCount := ALen(aColumns)

	_pi := pt := MemAlloc(dwCount * _SIZEOF(INT))

	FOR dwJ := 1 UPTO dwCount
		IF (dwIndex := SELF:__GetColumnIndexFromSymbol(aNew[dwJ])) = 0
			MemFree(pt)
			RETURN {}
		ENDIF
		INT(_pi) := INT(_CAST,dwIndex) - 1
		_pi++
	NEXT  // dwJ

	//PP-030910
	SendMessage(SELF:Handle(), LVM_SETCOLUMNORDERARRAY, dwCount, LONGINT(_CAST, pt))

	//PP-030910 Thanks to suggestion by S Ebert
	MemFree(pt)

	InvalidateRect(hWnd, NULL_PTR, TRUE)

	RETURN 

ACCESS CurrentView 
	LOCAL dwView AS DWORD
	LOCAL dwStyles AS DWORD

	

	// get the current style
	dwStyles := DWORD(_CAST, GetWindowLong(SELF:Handle(), GWL_STYLE))
	dwView := _AND(dwStyles, LVS_TYPEMASK)

	SWITCH dwView
	CASE LVS_ICON
		RETURN #IconView

	CASE LVS_SMALLICON
		RETURN #SmallIconView

	CASE LVS_LIST
		RETURN #ListView

	CASE LVS_REPORT
		RETURN #ReportView
	END SWITCH

	RETURN NULL_SYMBOL

METHOD DeleteAll() 
	LOCAL dwIndex AS DWORD

	

	// for each column, re-initialize the value array
	FOR dwIndex := 1 TO SELF:ColumnCount
		SELF:__GetColumnFromIndex(dwIndex):__ValueList := {}
	NEXT  // dwIndex

	aDeleted := {}

	RETURN ListView_DeleteAllItems(SELF:Handle())

METHOD DeleteAllColumns() 
	//SE-060526
	LOCAL lRet := TRUE AS LOGIC
	

	WHILE (ALen(aColumns) > 0)
		lRet := lRet .AND. SELF:DeleteColumn(1)
	END

	RETURN lRet

METHOD DeleteColumn(oListViewColumn) 
	//SE-060526
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD

	// RvdH 070312 Bug report Markus Feser 	
	dwCount := ALen(aColumns)

	// find the index of this column in the column list
	IF IsInstanceOfUsual(oListViewColumn, #ListViewColumn)
		FOR dwIndex := 1 UPTO dwCount
		   IF aColumns[dwIndex] = oListViewColumn
		   	EXIT
		   ENDIF
		NEXT  // dwI
	   IF dwIndex > dwCount
	   	dwIndex := 0
	   ENDIF
	ELSEIF IsSymbol(oListViewColumn)
		dwIndex := SELF:__GetColumnIndexFromSymbol(oListViewColumn)
	ELSEIF IsNumeric(oListViewColumn)
	   dwIndex := oListViewColumn
	ENDIF

	// remove the column from both the column list and the control
	IF dwIndex > 0
		ADel(aColumns, dwIndex)
		ASize(aColumns, dwCount - 1)
		RETURN ListView_DeleteColumn(SELF:Handle(), INT(_CAST, dwIndex - 1))
	ENDIF

	RETURN FALSE

METHOD DeleteItem(nItem) 
	LOCAL strucItem IS _winLV_Item
	LOCAL dwIndex AS DWORD
	LOCAL lparam AS LONGINT
	LOCAL oListViewColumn AS ListViewColumn

	

	strucItem:iItem := nItem - 1
	strucItem:mask := LVIF_PARAM
	IF ListView_GetItem(SELF:Handle(),  @strucItem)
		lparam := strucItem:lparam

		// for each column, delete the value entry associated with this item
		FOR dwIndex := 1 TO SELF:ColumnCount
			oListViewColumn := SELF:__GetColumnFromIndex(dwIndex)
			// dwLength := ALen(oListViewColumn:__ValueList)
			//RvdH 060608 optimized
			//IF !Empty(oListViewColumn:__ValueList)
			IF ALen(oListViewColumn:__ValueList) > 0
				oListViewColumn:__ValueList[lParam] := NIL
			ENDIF
			//ADel(oListViewColumn:__ValueList, lparam)
			//ASize(oListViewColumn:__ValueList, dwLength-1)
		NEXT  // dwIndex

		AAdd(aDeleted, lParam)

		// update the other listview items so they reference the correct value
		/*
		 iCount := self:ItemCount
		 for dwIndex := 0 to iCount-1
		 strucItem.iItem := dwIndex
		 strucItem.mask := LVIF_PARAM
		 if ListView_GetItem(self:Handle(), long(_cast, @strucItem))
		 if strucItem.lparam > lparam
		 strucItem.lparam := strucItem.lparam - 1
		 ListView_SetItem(self:Handle(), long(_cast, @strucItem))
		 endif
		 endif
		 next dwIndex
		*/
		// do the actual item delete
		RETURN ListView_DeleteItem(SELF:Handle(), nItem - 1)
	ENDIF

	RETURN FALSE

METHOD Destroy() 
	

	IF !InCollect()
		// if not in garbage collection, free column and deleted list
		aColumns := NULL_ARRAY
		aDeleted := NULL_ARRAY
		oLargeImageList := NULL_OBJECT
		oSmallImageList := NULL_OBJECT
		oStateImageList := NULL_OBJECT
		oDragImageList := NULL_OBJECT
	ENDIF
	SUPER:Destroy()

	RETURN NIL


ACCESS DragDropEnabled 
	

	RETURN lDragDropEnabled

ACCESS DragImageList 
	

	RETURN oDragImageList

ASSIGN DragImageList(oNewDragImageList) 
	

	RETURN oDragImageList := oNewDragImageList

METHOD EditItemLabel(nItem) 
	

	SELF:SetFocus()
	ListView_EditLabel(SELF:Handle(), INT(nItem - 1))
	RETURN NIL

METHOD EnableDragDrop(lEnable) 
	

	DEFAULT(@lEnable, TRUE)

	lDragDropEnabled := lEnable

	RETURN TRUE

METHOD EnableGroupView(lSetting) 
	//PP-030909
	// Listview groups require XP visual styles
	DEFAULT(@lSetting,TRUE)
	// LVM_ENABLEGROUPVIEW: 0=already enabled/disabled, 1=state changed, -1=failed
	RETURN SendMessage(SELF:handle(),LVM_ENABLEGROUPVIEW,DWORD(_CAST,lSetting),0) >= 0

METHOD EnableSort(symMethodName) 
	

	RETURN symSortRoutineName := symMethodName

METHOD EnsureVisible(nItem, lPartiallyVisible) 
	

	RETURN LOGIC(_CAST, ListView_EnsureVisible(SELF:Handle(), INT(nItem - 1), WORD(_CAST, lPartiallyVisible)))

ACCESS FullRowSelect 
	RETURN SELF:GetExLVStyle(LVS_EX_FULLROWSELECT)

ASSIGN FullRowSelect(lNewVal) 
	RETURN SELF:SetExLVStyle(LVS_EX_FULLROWSELECT, lNewVal)

METHOD GetAllItems( ) 
	LOCAL oLVI AS ListViewItem
	LOCAL aSelected AS ARRAY
	LOCAL i AS DWORD
	//PP-030319 added method

	aSelected := {}

	FOR i := 0 TO SELF:itemcount
		oLVI := SELF:GetNextItem(LV_GNIBYITEM,,,,,i)
		IF ! oLVI == NULL_OBJECT
			AAdd(aSelected,oLVI)
		ENDIF
	NEXT

	RETURN aSelected

METHOD GetAllSelectedItems( ) 
	LOCAL oLVI AS ListViewItem
	LOCAL aSelected AS ARRAY
	//PP-030319 added mmethod

	aSelected := {}

	IF ! ( oLVI := SELF:GetSelectedItem() ) == NULL_OBJECT
		AAdd(aSelected,oLVI)
		DO WHILE ! ( oLVI := SELF:GetNextItem(LV_GNIBELOW,FALSE,FALSE,FALSE,TRUE,oLVI:ItemIndex) ) == NULL_OBJECT
			AAdd(aSelected,oLVI)
		ENDDO
	ENDIF

	RETURN aSelected

METHOD GetColumn(xColumnID) 
	//SE-060526
	LOCAL dwIndex AS DWORD

	

	IF IsLong(xColumnID)
		RETURN aColumns[xColumnID]
	ELSEIF IsSymbol(xColumnID)
		IF (dwIndex := SELF:__GetColumnIndexFromSymbol(xColumnID)) > 0
		  	RETURN aColumns[dwIndex]
		ENDIF
	ENDIF

	RETURN NULL_OBJECT

METHOD GetExLVStyle(kExStyle) 
	//SE-060519
	// 2.5b renaming of method - was conflicting with Control:SetExStyle
	LOCAL dwExStyle AS DWORD

	dwExStyle := DWORD(_CAST, SendMessage(SELF:handle(), LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0))

	IF IsLong(kExStyle)
		RETURN (_AND(dwExStyle, DWORD(kExStyle)) > 0)
	ENDIF

   RETURN dwExStyle

METHOD GetGroupTextColor() 
	//PP-030909
	// Listview groups require XP visual styles
	LOCAL oColor AS Color
	LOCAL pLVGroupMetric IS _WINLVGROUPMETRICS

	pLVGroupMetric:cbSize := _SIZEOF(_winLVGroupMetrics)

	pLVGroupMetric:mask := LVGMF_TEXTCOLOR

	SendMessage(SELF:handle(),LVM_GETGROUPMETRICS,0,LONGINT(_CAST,@pLVGroupMetric))

	oColor := COLOR{}
	oColor:colorref := pLVGroupMetric:crHeader

	RETURN oColor

METHOD GetItemAtPosition(oPoint) 
	LOCAL strucHitTestInfo IS _winLV_HitTestInfo
	LOCAL nItem AS INT

	

	oPoint := __WCConvertPoint(SELF, oPoint)
	strucHitTestInfo:pt:x := oPoint:X
	strucHitTestInfo:pt:y := oPoint:Y
	nItem := ListView_HitTest(SELF:Handle(),  @strucHitTestInfo)
	IF (nItem != -1)
		RETURN SELF:GetItemAttributes(nItem + 1)
	ENDIF

	RETURN NULL_OBJECT

METHOD GetItemAttributes(nItem) 
	LOCAL strucItem IS _winLV_Item
	LOCAL oListViewItem AS ListViewItem
	LOCAL oListViewColumn AS ListViewColumn
	LOCAL dwState AS DWORD
	LOCAL dwIndex AS DWORD
	//LOCAL pszText AS PSZ
	LOCAL DIM aBuf[257] AS BYTE
	LOCAL aValueList AS ARRAY
	LOCAL hHandle AS PTR
	LOCAL liItemLParam AS LONGINT

	

	hHandle := SELF:Handle()

	// retrieve image index and lparam
	strucItem:iItem := nItem - 1
	strucItem:mask := _OR(LVIF_IMAGE, LVIF_PARAM, LVIF_INDENT)
	IF ListView_GetItem(hHandle,  @strucItem)
		// create the ListViewItem object and fill its properties
		oListViewItem := ListViewItem{}

		oListViewItem:ItemIndex := nItem
		oListViewItem:__lParam := strucItem:lParam
		oListViewItem:ImageIndex := strucItem:iImage + 1
		oListViewItem:Indent := strucItem:iIndent
		// set the image states
		dwState := ListView_GetItemState(hHandle, nItem - 1, LVIS_STATEIMAGEMASK)
		oListViewItem:StateImageIndex := dwState >> 12
		dwState := ListView_GetItemState(hHandle, nItem - 1, LVIS_OVERLAYMASK)
		oListViewItem:OverlayImageIndex := dwState >> 8

		// set the visual states
		IF ListView_GetItemState(hHandle, nItem - 1, LVIS_CUT) == LVIS_CUT
			oListViewItem:Disabled := TRUE
		ENDIF
		IF ListView_GetItemState(hHandle, nItem - 1, LVIS_DROPHILITED) == LVIS_DROPHILITED
			oListViewItem:DropTarget := TRUE
		ENDIF
		IF ListView_GetItemState(hHandle, nItem - 1, LVIS_FOCUSED) == LVIS_FOCUSED
			oListViewItem:Focused := TRUE
		ENDIF
		IF ListView_GetItemState(hHandle, nItem - 1, LVIS_SELECTED) == LVIS_SELECTED
			oListViewItem:Selected := TRUE
		ENDIF

		// get the text values and values for the object using lparam as
		// an index to the arrays
		liItemLParam := strucItem:lparam
		FOR dwIndex := 1 TO SELF:ColumnCount
			aBuf[1] := 0
			strucItem:mask := _OR(LVIF_TEXT, LVIF_IMAGE)
			strucItem:iSubItem := LONGINT(_CAST, dwIndex) - 1l
			strucItem:pszText := @aBuf[1]
			strucItem:cchTextMax := 256
			ListView_GetItem(hWnd, @strucItem)
			//pszText := @aBuf[1]
			//ListView_GetItemText(hHandle, DWORD(nItem - 1), INT(_CAST, dwIndex - 1), pszText, 256)
			oListViewColumn := SELF:__GetColumnFromIndex(dwIndex)
			oListViewItem:SetText(Psz2String(strucItem:pszText), oListViewColumn:NameSym, strucItem:iImage+1l)
			//RvdH 060608 optimized
			//IF !Empty(oListViewColumn:__ValueList)
			IF ALen(oListViewColumn:__ValueList) > 0
				//PP-030828 Strong typing
				aValueList := oListViewColumn:__ValueList
				oListViewItem:SetValue(aValueList[liItemLParam], oListViewColumn:NameSym)
			ENDIF
		NEXT  // dwIndex
	ENDIF

	RETURN oListViewItem

METHOD GetItemBoundingBox(nItem) 
	LOCAL strucRect IS _winRect
	LOCAL oOrigin AS Point
	LOCAL oSize AS Dimension

	

	IF LOGIC(_CAST, SendMessage(SELF:Handle(), LVM_GETITEMRECT, DWORD(nItem - 1), LONGINT(_CAST, @strucRect)))
		//PP-030910
		oOrigin := Point{strucRect:left, strucRect:bottom}
		oOrigin := __WCConvertPoint(SELF, oOrigin)
		oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}
		RETURN BoundingBox{oOrigin, oSize}
	ENDIF

	RETURN NULL_OBJECT

METHOD GetItemPosition(nItem) 
	LOCAL strucPoint IS _winPoint
	LOCAL oBoundingBox AS BoundingBox

	

	oBoundingBox := SELF:GetItemBoundingBox(nItem)
	ListView_GetItemPosition(SELF:Handle(), nItem - 1,  @strucPoint)

	RETURN __WCConvertPoint(SELF, Point{strucPoint:x, strucPoint:y + oBoundingBox:Height-1})

METHOD GetItemSpacing(symView) 

	

	DEFAULT(@symView, #IconView)

	// this only works for IconView and SmallIconView
	IF symView == #IconView
		RETURN ListView_GetItemSpacing(SELF:Handle(), DWORD(_CAST, FALSE))
	ELSEIF symView == #SmallIconView
		RETURN ListView_GetItemSpacing(SELF:Handle(), DWORD(_CAST, TRUE))
	ENDIF

	RETURN -1

METHOD GetNextItem(kRelationship, lDisabled, lDropTarget, lFocused, lSelected, nItemStart) 
	LOCAL dwState AS DWORD
	LOCAL nFoundItem AS INT
	LOCAL oListViewItem AS ListViewItem

	

	// handle default values
	DEFAULT(@lDisabled, FALSE)
	DEFAULT(@lDropTarget, FALSE)
	DEFAULT(@lFocused, FALSE)
	DEFAULT(@lSelected, FALSE)

	// create state argument
	IF lDisabled
		dwState := _OR(dwState, LVNI_CUT)
	ENDIF
	IF lDropTarget
		dwState := _OR(dwState, LVNI_DROPHILITED)
	ENDIF
	IF lFocused
		dwState := _OR(dwState, LVNI_FOCUSED)
	ENDIF
	IF lSelected
		dwState := _OR(dwState, LVNI_SELECTED)
	ENDIF

	IF !IsNil(nItemStart)
		IF !IsLong(nItemStart)
			WCError{#GetNextItem, #ListView, __WCSTypeError, nItemStart, 6}:@@Throw()
		ELSE
			kRelationship := LV_GNIBYITEM
			nItemStart--
		ENDIF
	ELSE
		nItemStart := -1
	ENDIF

	dwState := _OR(dwState, DWORD(kRelationship))
	nFoundItem := ListView_GetNextItem(SELF:Handle(), nItemStart, WORD(_CAST, dwState))
	IF (nFoundItem != -1)
		oListViewItem := SELF:GetItemAttributes(nFoundItem + 1)
	ENDIF

	RETURN oListViewItem

METHOD GetSelectedColumn() 
	//PP-030909
	// Requires XP visual styles
	RETURN SendMessage(SELF:handle(), LVM_GETSELECTEDCOLUMN, 0, 0) + 1


METHOD GetSelectedItem() 
	

	RETURN SELF:GetNextItem(LV_GNIBYITEM, FALSE, FALSE, FALSE, TRUE)

ACCESS GridLines 
	RETURN SELF:GetExLVStyle(LVS_EX_GRIDLINES)

ASSIGN GridLines(lNewVal) 
	RETURN SELF:SetExLVStyle(LVS_EX_GRIDLINES, lNewVal)

METHOD HasGroup(iGroupId) 
	//PP-030909
	// Listview groups require XP visual styles
	RETURN LOGIC(_CAST,SendMessage(SELF:handle(),LVM_HASGROUP,DWORD(_CAST,iGroupId),0))

ACCESS HeaderDragDrop 
	RETURN SELF:GetExLVStyle(LVS_EX_HEADERDRAGDROP)

ASSIGN HeaderDragDrop(lNewVal) 
	RETURN SELF:SetExLVStyle(LVS_EX_HEADERDRAGDROP, lNewVal)

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	LOCAL dwStyle AS DWORD

	

	IF IsNil(kStyle)
		dwStyle := WS_BORDER
	ELSE
		dwStyle := _OR(DWORD(kStyle), DWORD(_CAST, WS_BORDER))
	ENDIF

	IF IsInstanceOfUsual(xID, #ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, , dwStyle, TRUE)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, "SysListView32", dwStyle, TRUE)
	ENDIF
	SELF:SetStyle(LVS_SHAREIMAGELISTS)

	aColumns := {}
	// initialize array of deleted value indizes
	aDeleted := {}

	RETURN 

METHOD InsertColumn(oListViewColumn, nInsertAfter) 
	LOCAL strucListColumn IS _winLV_Column
	LOCAL pszCaption AS PSZ
	LOCAL pszStringSize AS PSZ
	LOCAL lReturnValue AS LONGINT

	

	DEFAULT(@nInsertAfter, SELF:ColumnCount)

	strucListColumn:mask := _OR(LVCF_FMT, LVCF_WIDTH, LVCF_TEXT, LVCF_SUBITEM)
	strucListColumn:fmt := oListViewColumn:Alignment
	strucListColumn:iSubItem := nInsertAfter
	IF (NULL_STRING != oListViewColumn:Caption)
		pszCaption := StringAlloc(oListViewColumn:Caption)
	ENDIF
	strucListColumn:pszText := pszCaption

	// calculate the pixel width of the column using the given character width
	IF (oListViewColumn:Width > 0)
		pszStringSize := StringAlloc(Replicate("M", oListViewColumn:Width))
		IF (PTR(_CAST, pszStringSize) != NULL_PTR)
			strucListColumn:cx := ListView_GetStringWidth(SELF:Handle(), pszStringSize)
			MemFree(pszStringSize)
		ENDIF
	ELSE
		strucListColumn:cx := oListViewColumn:Width
	ENDIF

	// convert the string size into pixel-width

	lReturnValue := ListView_InsertColumn(SELF:Handle(), nInsertAfter, @strucListColumn)

	IF (PTR(_CAST, pszCaption) != NULL_PTR)
		MemFree(pszCaption)
	ENDIF

	//AAdd(aColumns, oListViewColumn) // fix for 2.5b
	AAdd(aColumns, NIL)
	AIns(aColumns, nInsertAfter+1)
	aColumns[nInsertAfter+1] := oListViewColumn

	oListViewColumn:__Owner := SELF

	RETURN (lReturnValue != -1)

METHOD InsertItem(oListViewItem, nInsertAfter) 
	//SE-060523
	LOCAL strucListItem IS _winLV_Item
	LOCAL dwIndex AS DWORD
	LOCAL nIndex AS INT
	LOCAL aColumnValues AS ARRAY
	LOCAL oListViewColumn AS ListViewColumn
	LOCAL oLVItem AS ListViewItem
	LOCAL idx AS INT

	

	oLVItem := oListViewItem

	// by default, insert the item at the end
	DEFAULT(@nInsertAfter, SELF:ItemCount)

	// set the image index
	IF oLVItem:ImageIndex != 0
		strucListItem:iImage := oLVItem:ImageIndex - 1
	ENDIF

	// check if we can reuse an empty slot
	IF ALen(aDeleted) > 0
		idx := aDeleted[1]
		ADel(ADeleted, 1)
		ASize(aDeleted, ALen(aDeleted)-1)
	ENDIF

	// copy the usual values from the item to the column
	FOR dwIndex := 1 TO SELF:ColumnCount
		oListViewColumn := SELF:__GetColumnFromIndex(dwIndex)
		aColumnValues := oListViewColumn:__ValueList
		IF (idx > 0) //an empty slot found
			aColumnValues[idx] := oLVItem:GetValue(oListViewColumn:NameSym)
		ELSE
			AAdd(aColumnValues, oLVItem:GetValue(oListViewColumn:NameSym))
		ENDIF
	NEXT  // dwIndex

	// set the lparam, which will identify the index of the associated usual value
	strucListItem:lparam := IIF((idx == 0), ALen(aColumnValues), idx)

	// insert the item into the control
	strucListItem:mask := _OR(LVIF_IMAGE, LVIF_PARAM, LVIF_INDENT)
	strucListItem:iItem := nInsertAfter
	strucListItem:iIndent := oLVItem:Indent
	strucListItem:iItem := nIndex := ListView_InsertItem(SELF:Handle(),  @strucListItem)

	//SE-060523
   oLVItem:ItemIndex := nIndex + 1L

   IF (oLVItem:StateImageIndex == 0) .AND. SELF:GetExLVStyle(LVS_EX_CHECKBOXES)
		oLVItem:StateImageIndex := 1
	ENDIF

   idx :=strucListItem:lparam
   oListViewItem:__lParam := 0

   SELF:__SetItem(@strucListItem, oLVItem)

   oListViewItem:__lParam := idx

   RETURN (nIndex != -1)

ACCESS IsGroupViewEnabled 
	//PP-030909
	// Listview groups require XP visual styles
	RETURN LOGIC(_CAST,SendMessage(SELF:handle(),LVM_ISGROUPVIEWENABLED,0,0))

ACCESS ItemCount 
	

	RETURN ListView_GetItemCount(SELF:Handle())

ACCESS ItemsPerPage 
	

	RETURN SendMessage(SELF:Handle(), LVM_GETCOUNTPERPAGE, 0, 0)

ACCESS LargeImageList 
	

	RETURN oLargeImageList

ASSIGN LargeImageList(oNewImageList) 
	

	ListView_SetImageList(SELF:Handle(), oNewImageList:Handle(), LVSIL_NORMAL)
	oLargeImageList := oNewImageList

	RETURN 

METHOD RedrawRange(oRange) 
	

	RETURN LOGIC(_CAST, ListView_RedrawItems(SELF:Handle(), oRange:Min, oRange:Max))

METHOD RemoveAllGroups() 
	//PP-030909
	// Listview groups require XP visual styles
	SendMessage(SELF:handle(),LVM_REMOVEALLGROUPS,0,0)
	RETURN NIL

METHOD RemoveGroup(iGroupId) 
	//PP-030909
	// Listview groups require XP visual styles
	RETURN SendMessage(SELF:handle(),LVM_REMOVEGROUP,DWORD(_CAST,iGroupId),0) >= 0

METHOD Scroll(oDimension) 
	

	RETURN LOGIC(_CAST, ListView_Scroll(SELF:Handle(), oDimension:Width, -oDimension:Height))

ACCESS SearchString 
	LOCAL pszSearchString AS PSZ
	LOCAL cSearchString AS STRING
	LOCAL DIM aBuf[257] AS BYTE

	

	pszSearchString:= @aBuf[1]
	ListView_GetISearchString(SELF:Handle(), pszSearchString)
	cSearchString := Psz2String(pszSearchString)

	RETURN cSearchString

METHOD Seek(uValue, kSeekType, nStart, lWrap, lPartial) 
	//SE-060526
	LOCAL strucFindInfo IS _winLV_FindInfo
	LOCAL oPoint AS Point
	LOCAL pszText AS PSZ
	LOCAL nItem AS INT
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL aValList AS ARRAY

	

	// start the seek at the beginning by default
	IF IsNil(nStart)
		nStart := -1
	ELSE
		nStart--
	ENDIF

	// find the item closest to the given point
	IF IsInstanceOfUsual(uValue, #Point)
		// kSeekType is a usual
		DEFAULT(@kSeekType, LV_SEEKDOWN)

		IF kSeekType == LV_SEEKUP
			strucFindInfo:vkDirection := VK_UP
		ELSE
			strucFindInfo:vkDirection := VK_DOWN
		ENDIF

		// convert the point to a _winPoint
		oPoint := uValue
		oPoint := __WCConvertPoint(SELF, oPoint)
		strucFindInfo:pt:x := oPoint:X
		strucFindInfo:pt:y := oPoint:Y
		strucFindInfo:flags := LVFI_NEARESTXY

		// do the seek
		nItem := ListView_FindItem(SELF:Handle(), nStart,  @strucFindInfo)
	ELSEIF kSeekType == LV_SEEKTEXT
		// uValue is the item text, so a text seek is in order
		strucFindInfo:flags := LVFI_STRING

		// set wrap-around (off by default)
		DEFAULT(@lWrap, FALSE)
		IF lWrap
			strucFindInfo:flags := _OR(strucFindInfo:flags, LVFI_WRAP)
		ENDIF

		// set exact search (on by default)
		DEFAULT(@lPartial, FALSE)
		IF lPartial
			strucFindInfo:flags := _OR(strucFindInfo:flags, LVFI_PARTIAL)
		ENDIF

		// do the seek
		pszText := StringAlloc(AsString(uValue))
		strucFindInfo:_psz := pszText
		nItem := ListView_FindItem(SELF:Handle(), nStart,  @strucFindInfo)
		IF (PTR(_CAST, pszText) != NULL_PTR)
			MemFree(pszText)
		ENDIF
	ELSEIF kSeekType == LV_SEEKVALUE
		// uValue is the associated usual value, so a usual seek is in order
		strucFindInfo:flags := LVFI_PARAM

		// set wrap-around (off by default)
		DEFAULT(@lWrap, FALSE)
		IF lWrap
			strucFindInfo:flags := _OR(strucFindInfo:flags, LVFI_WRAP)
		ENDIF
		// find uValue in the first column's value list
		aValList := SELF:__GetColumnFromIndex(1):__ValueList
		dwCount := ALen(aValList)
		FOR dwIndex := 1 UPTO dwCount
		   IF aValList[dwIndex] = uValue
		   	strucFindInfo:lParam := LONGINT(_CAST, dwIndex)
		   	EXIT
		   ENDIF
		NEXT  // dwIndex
		nItem := ListView_FindItem(SELF:Handle(), nStart,  @strucFindInfo)
	ENDIF

	// return the ListViewItem object from nItem, if it exists
	IF (nItem != -1)
		ListView_EnsureVisible(hwnd, nItem, 1)
		RETURN SELF:GetItemAttributes(nItem + 1)
	ENDIF
	RETURN NULL_OBJECT

ACCESS SelectedCount 
	

	RETURN ListView_GetSelectedCount(SELF:Handle())

METHOD SelectItem(nItem, lSelect) 
	LOCAL oLVI AS ListViewItem

	DEFAULT(@lSelect, TRUE)
	oLVI := SELF:GetNextItem(LV_GNIBYITEM,,,,,nItem-1)
	IF (oLVI == NULL_OBJECT)
		RETURN FALSE
	ENDIF

	oLVI:Selected := lSelect
	SELF:SetItemAttributes(oLVI)
	RETURN TRUE

METHOD SelectNextItem() 
	LOCAL oLVI AS ListViewItem

	oLVI := SELF:GetSelectedItem()
	IF (oLVI == NULL_OBJECT)
		RETURN FALSE
	ENDIF

	oLVI := SELF:GetNextItem(LV_GNIBELOW,,,,,oLVI:ItemIndex)
	IF (oLVI == NULL_OBJECT)
		RETURN FALSE
	ENDIF

	oLVI:Selected := TRUE
	SELF:SetItemAttributes(oLVI)
	RETURN TRUE

METHOD SetBackgroundImage(uImage,dwFlags,xOffSet,yOffSet) 
	//PP-030909
	// Requires a call to CoInitialize() or the OLE library linked in. If CoInitialize() call CoUnitialize() later.
	LOCAL pLVBKI IS _WINLVBKIMAGE
	LOCAL hImage AS PTR
	LOCAL cURL AS STRING

	DO CASE
	CASE IsObject(uImage)
		hImage := uImage:handle()
		DEFAULT(@dwFlags,_OR(LVBKIF_SOURCE_HBITMAP,LVBKIF_STYLE_TILE))
	CASE IsPtr(uImage)
		hImage := uImage
		DEFAULT(@dwFlags,_OR(LVBKIF_SOURCE_HBITMAP,LVBKIF_STYLE_TILE))
	CASE IsString(uImage)
		cURL := uImage
		DEFAULT(@dwFlags,_OR(LVBKIF_SOURCE_URL,LVBKIF_STYLE_TILE))
	CASE IsNil(uImage)
		DEFAULT(@dwFlags,LVBKIF_SOURCE_NONE)
	ENDCASE

	DEFAULT(@xOffSet,0)
	DEFAULT(@yOffSet,0)

	pLVBKI:ulFlags := dwFlags
				// _or(LVBKIF_TYPE_WATERMARK,LVBKIF_STYLE_NORMAL,LVBKIF_SOURCE_NONE)
				// _or(LVBKIF_SOURCE_HBITMAP,LVBKIF_STYLE_TILE)
				// _or(LVBKIF_SOURCE_URL,LVBKIF_STYLE_TILE)
	//RvdH 060608 optimized
	//IF ! Empty(cURL)
	IF SLen(cURL) > 0
		pLVBKI:pszImage := String2Psz(cURL)
	ENDIF
	IF ! hImage == NULL_PTR
		pLVBKI:hbm := hImage
	ENDIF
	pLVBKI:xOffsetPercent := xOffSet
	pLVBKI:yOffsetPercent := yOffSet

	RETURN ! SendMessage(SELF:handle(),LVM_SETBKIMAGE,0,LONGINT(_CAST,@pLVBKI)) == 0

METHOD SetColumnFormat(nCol,dwFlag,nImage) 
	//PP-030909
	LOCAL pHeader AS PTR
	LOCAL pItem IS _WINHDITEM
	LOCAL dwFmt AS DWORD

	// HDF_CENTER/HDF_LEFT/HDF_RIGHT, HDF_BITMAP/HDF_BITMAP_ON_RIGHT, HDF_SORTDOWN/HDF_SORTUP
	// HDF_SORTDOWN/HDF_SORTUP require XP visual styles
	DEFAULT(@dwFlag,0)

	// image list index
	DEFAULT(@nImage,0)

	pHeader := PTR(_CAST,SendMessage(SELF:handle(), LVM_GETHEADER, 0,0))
	pItem:mask := _OR(HDI_IMAGE,HDI_FORMAT)

	dwFmt := _OR(HDF_STRING,DWORD(dwFlag))
	IF nImage > 0
		dwFmt := _OR(dwFmt,HDF_IMAGE)
		pItem:iImage := nImage-1
	ENDIF
	pItem:fmt := INT(dwFmt)

	RETURN ! SendMessage(pHeader, HDM_SETITEM, nCol-1, LONGINT(_CAST,@pItem)) == 0

METHOD SetExLVStyle(kExStyle, lEnable) 
	//SE-060519
	// 2.5b renaming of method - was conflicting with Control:SetExStyle

	

	IF !IsLong(kExStyle)
		WCError{#SetExLVStyle,#ListView,__WCSTypeError,kExStyle,}:@@Throw()
	ENDIF

	IF IsNil(lEnable) .OR. !IsLogic(lEnable)
		lEnable := TRUE
	ENDIF

	SendMessage(SELF:Handle(), LVM_SETEXTENDEDLISTVIEWSTYLE, kExStyle, IIF(lEnable, kExStyle, 0l))

	RETURN SELF

METHOD SetGroupName(iGroupId,cGroupName,dwAlign) 
	//PP-030909
	// Listview groups require XP visual styles
	LOCAL pLVGroup IS _WinLVGroup
	LOCAL i AS INT

	DEFAULT(@dwAlign,LVGA_HEADER_LEFT)

	pLVGroup:cbSize := _SIZEOF(_winLVGroup)
	pLVGroup:mask := _OR(LVGF_HEADER,LVGF_ALIGN)
	pLVGroup:pHeader := String2W(cGroupName)
	pLVGroup:uAlign := dwALign

 	i := SendMessage(SELF:handle(),LVM_SETGROUPINFO,DWORD(_CAST,iGroupId),LONGINT(_CAST,@pLVGroup))

	SysFreeString(pLVGroup:pHeader)

	InvalidateRect(SELF:handle(),NULL,TRUE)
	UpdateWindow(SELF:handle())


	RETURN i

METHOD SetGroupTextColor(oColor) 
	//PP-030909
	// Listview groups require XP visual styles
	LOCAL pLVGroupMetric IS _WINLVGROUPMETRICS

	pLVGroupMetric:cbSize := _SIZEOF(_winLVGroupMetrics)

	pLVGroupMetric:mask := LVGMF_TEXTCOLOR
	pLVGroupMetric:crHeader := oColor:colorRef

	SendMessage(SELF:handle(),LVM_SETGROUPMETRICS,0,LONGINT(_CAST,@pLVGroupMetric))
	RETURN SELF


METHOD SetItemAttributes(oListViewItem) 
	//SE-060523
	LOCAL strucItem IS _winLV_Item
	LOCAL nIndex AS INT
	//LOCAL dwIndex AS DWORD
	//LOCAL oListViewColumn AS ListViewColumn
	LOCAL oLVItem AS ListViewItem
	//LOCAL cText AS STRING
	//LOCAL uVal AS USUAL
   //LOCAL hHandle AS PTR
   //LOCAL liSubImage AS LONG

	

	oLVItem := oListViewItem

	//hHandle := SELF:Handle()

	// convert from 1-base to 0-base
	nIndex := oLVItem:ItemIndex - 1L

	// set the image index
	strucItem:iItem := nIndex
	strucItem:mask := _OR(LVIF_IMAGE, LVIF_INDENT)
	strucItem:iImage := oLVItem:ImageIndex - 1L
	strucItem:iIndent := oLVItem:Indent
	ListView_SetItem(hWnd,  @strucItem)

	SELF:__SetItem(@strucItem, oLVItem)

	RETURN NIL

METHOD SetItemGroupId(uLVI,nId) 
	//PP-030909
	// Listview groups require XP visual styles
	LOCAL pLVItem IS _winLVItem6
	LOCAL nItem AS INT

	IF IsObject(uLVI) .AND. ! uLVI == NULL_OBJECT
		nItem := uLVI:ItemIndex
	ELSEIF IsNumeric(uLVI)
		nItem := uLVI
	ENDIF

	IF nItem > 0
		pLVItem:mask := LVIF_GROUPID
		pLVItem:iItem := nItem-1
		pLVItem:iGroupId := nId

		SendMessage(SELF:handle(),LVM_SETITEM,0,LONGINT(_CAST,@pLVItem))
	ENDIF

	RETURN SELF

METHOD SetItemPosition(nItem, oPoint) 
	LOCAL oBoundingBox AS BoundingBox

	

	oBoundingBox := SELF:GetItemBoundingBox(nItem)
	oPoint:Y := oPoint:Y + oBoundingBox:Height
	oPoint := __WCConvertPoint(SELF, oPoint)

	RETURN LOGIC(_CAST, ListView_SetItemPosition(SELF:Handle(), nItem - 1, oPoint:X, oPoint:Y))

METHOD SetSelectedColumn(nIndex) 
	//PP-030909
	// Requires XP visual styles
	SendMessage(SELF:handle(), LVM_SETSELECTEDCOLUMN, nIndex-1, 0)

	InvalidateRect(SELF:handle(),NULL,TRUE)
	UpdateWindow(SELF:handle())
	RETURN SELF

ACCESS SmallImageList 
	

	RETURN oSmallImageList

ASSIGN SmallImageList(oNewImageList) 
	

	ListView_SetImageList(SELF:Handle(), oNewImageList:Handle(), LVSIL_SMALL)
	oSmallImageList := oNewImageList

	RETURN 

METHOD SortItems() 
	LOCAL hListView AS PTR

	// send the sort message, passing the ListView handle as sort data
	hListView := SELF:Handle()

	RETURN LOGIC(_CAST, SendMessage(hListView, LVM_SORTITEMS, DWORD(_CAST, hListView), LONGINT(_CAST, Get_ListView_ComparePtr())))

ACCESS StateImageList 
	

	RETURN oStateImageList

ASSIGN StateImageList(oNewImageList) 
	

	ListView_SetImageList(SELF:Handle(), oNewImageList:Handle(), LVSIL_STATE)
	oStateImageList := oNewImageList

	RETURN 

ACCESS TextBackgroundColor 
	LOCAL dwCR AS DWORD
	

	dwCR := ListView_GetTextBkColor(SELF:Handle())

	RETURN Color{_AND(dwCR, 0x000000FF), _AND((dwCR >> 8), 0x000000FF), _AND((dwCR >> 16), 0x000000FF)}

ASSIGN TextBackgroundColor(oTextBackgroundColor) 
	

	ListView_SetTextBkColor(SELF:Handle(), oTextBackgroundColor:ColorRef)

	RETURN 

ACCESS TextColor 
	LOCAL dwCR AS DWORD
	

	dwCR := ListView_GetTextColor(SELF:Handle())

	RETURN Color{_AND(dwCR, 0x000000FF), _AND((dwCR >> 8), 0x000000FF), _AND((dwCR >> 16), 0x000000FF)}

ASSIGN TextColor(oNewTextColor) 
	


	ListView_SetTextColor(SELF:Handle(), oNewTextColor:ColorRef)
	RETURN 

ACCESS TopItem 
	

	RETURN SendMessage(SELF:Handle(), LVM_GETTOPINDEX, 0, 0) + 1

ACCESS TrackSelection 
	RETURN SELF:GetExLVStyle(LVS_EX_TRACKSELECT)

ASSIGN TrackSelection(lNewVal) 
	RETURN SELF:SetExLVStyle(LVS_EX_TRACKSELECT, lNewVal)

METHOD Update(nItem) 
	

	RETURN LOGIC(_CAST, ListView_Update(SELF:Handle(), nItem - 1))

METHOD ViewAs(symView) 
	//PP-030909 #TileView
	LOCAL hListView AS PTR
	LOCAL dwView AS DWORD
	LOCAL dwStyles AS DWORD
	LOCAL dwType AS DWORD

	DO CASE
	CASE symView == #IconView
		dwView := LVS_ICON
		dwType := LV_VIEW_ICON

	CASE symView == #SmallIconView
		dwView := LVS_SMALLICON
		dwType := LV_VIEW_SMALLICON

	CASE symView == #ListView
		dwView := LVS_LIST
		dwType := LV_VIEW_LIST

	CASE symView == #ReportView
		dwView := LVS_REPORT
		dwType := LV_VIEW_DETAILS

	END CASE

	IF symView == #TileView
		SendMessage(SELF:handle(),LVM_SETVIEW,LV_VIEW_TILE,0)
	ELSE
		hListView := SELF:Handle()
		// get the current style
		dwStyles := DWORD(_CAST, GetWindowLong(hListView, GWL_STYLE))
		IF _AND(dwStyles, LVS_TYPEMASK) != dwView
			// remove the current style and add the desired style
			SetWindowLong(hListView, GWL_STYLE, LONGINT(_CAST, _OR((_AND(dwStyles, _NOT(LVS_TYPEMASK))), dwView)))
		ENDIF
		SendMessage(SELF:handle(),LVM_SETVIEW,dwType,0)
	ENDIF

	RETURN NIL

ACCESS ViewBoundingBox 
	LOCAL strucRect IS _winRect
	LOCAL oOrigin AS Point
	LOCAL oSize AS Dimension

	

	IF LOGIC(_CAST, ListView_GetViewRect(SELF:Handle(),  @strucRect))
		// get the bottom-left Windows coordinate of the rectangle and convert the point
		//PP-030910
		oOrigin := Point{strucRect:left, strucRect:bottom}
		oOrigin := __WCConvertPoint(SELF, oOrigin)
		oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}
		RETURN BoundingBox{oOrigin, oSize}
	ENDIF

	RETURN NULL_OBJECT

ACCESS ViewOrigin 
	LOCAL strucPoint IS _winPoint

	

	// this call is only meaningful if the current view is list or report
	IF ListView_GetOrigin(SELF:Handle(),  @strucPoint)
		RETURN __WCConvertPoint(SELF, Point{strucPoint:x, strucPoint:y})
	ENDIF

	RETURN Point{}
END CLASS

CLASS ListViewColumn INHERIT VObject
	PROTECT aValues AS ARRAY
	PROTECT nAlignment AS INT
	PROTECT nWidth AS INT
	PROTECT lExplicitFS AS LOGIC
	PROTECT oFieldSpec AS FieldSpec
	PROTECT oHyperLabel AS HyperLabel
	PROTECT oOwner AS ListView

	//PP-030828 Strong typing
	ASSIGN __Owner(oListView AS ListView)  STRICT 
	//PP-030828 Strong typing
	

	RETURN oOwner := oListView

ACCESS __ValueList AS ARRAY STRICT 
	//PP-030828 Strong typing
	

	RETURN aValues

ASSIGN __ValueList(aValueList AS ARRAY)  STRICT 
	//PP-030828 Strong typing
	

	RETURN aValues := aValueList

ACCESS Alignment 
	

	RETURN nAlignment

ASSIGN Alignment(nNewAlignment) 
	

	RETURN nAlignment := nNewAlignment

ACCESS Caption 
	

	RETURN oHyperLabel:Caption

ASSIGN Caption(cNewCaption) 
	LOCAL hOwner AS PTR
	LOCAL hChild AS PTR
	LOCAL pszCaption AS PSZ
	LOCAL pszClassName AS PSZ
	LOCAL cClassName AS STRING
	LOCAL dwIndex AS DWORD
	LOCAL strucHDItem IS _winHD_Item
	LOCAL oOldHL AS HyperLabel
	LOCAL DIM aBuf[257] AS BYTE

	

	// if this column has already been added to a control, attempt
	// to change update the caption visually
	IF oOwner != NULL_OBJECT
		pszClassName := @aBuf[1]
		hOwner := oOwner:Handle()
		hChild := GetWindow(hOwner, GW_CHILD)
		GetClassName(hChild, pszClassName, 256)
		cClassName := Psz2String(pszClassName)
		IF (cClassName != WC_HEADER)
			DO WHILE TRUE
				hChild := GetWindow(hChild, GW_HWNDNEXT)
				GetClassName(hChild, pszClassName, 256)
				cClassName := Psz2String(pszClassName)
				IF cClassName == WC_HEADER .OR. hChild == NULL_PTR
					EXIT
				ENDIF
			END DO
		ENDIF

		IF cClassName == WC_HEADER
			dwIndex := oOwner:__GetColumnIndexFromSymbol(SELF:NameSym) - 1
			pszCaption := StringAlloc(cNewCaption)
			strucHDItem:mask := HDI_TEXT
			strucHDItem:pszText := pszCaption
			strucHDItem:cchTextMax := LONGINT(_CAST, PszLen(pszCaption))
			Header_SetItem(hChild, LONGINT(_CAST, dwIndex),  @strucHDItem)
			IF (PTR(_CAST, pszCaption) != NULL_PTR)
				MemFree(pszCaption)
			ENDIF
		ENDIF
	ENDIF

	oOldHL := oHyperLabel
	oHyperLabel := HyperLabel{oOldHL:NameSym, cNewCaption, oOldHL:Description, oOldHL:HelpContext}
	RETURN 

METHOD Destroy() 
	

	IF !InCollect()
		// free the aValues array if not in garbage collection
		aValues := NULL_ARRAY
	ENDIF
	SUPER:Destroy()

	RETURN SELF

ACCESS FieldSpec 
	

	RETURN oFieldSpec

ASSIGN FieldSpec(oNewFieldSpec) 
	

	IF !IsInstanceOfUsual(oNewFieldSpec, #FieldSpec)
		WCError{#FieldSpec, #ListViewColumn, __WCSTypeError, oNewFieldSpec, 1}:@@Throw()
	ENDIF

	// set up FieldSpec information for the column
	oFieldSpec := oNewFieldSpec
	lExplicitFS := TRUE
	// nType := oFieldSpec:UsualType
	// DO CASE
	// CASE nType == FLOAT
	// iPictureType := PICTYPE_NUMERIC
	// CASE nType == DATE
	// cPicture := "@D"
	// iPictureType := PICTYPE_DATE
	// ENDCASE

	IF oFieldSpec:ValType == "N"
		IF SubStr(oFieldSpec:Picture, 1, 2) == "@B"
			SELF:Alignment := LVCFMT_LEFT
		ELSE
			SELF:Alignment := LVCFMT_RIGHT
		ENDIF
	ENDIF

	RETURN 

ACCESS HyperLabel 
	

	RETURN oHyperLabel

ASSIGN HyperLabel(oNewHyperLabel) 
	

	RETURN oHyperLabel := oNewHyperLabel

CONSTRUCTOR(nWidth, xColumnID, kAlignment) 
	

	SUPER()

	// set the width of the column
	IF IsInstanceOfUsual(nWidth, #FieldSpec)
		SELF:nWidth := __GetFSDefaultLength(nWidth)
		SELF:FieldSpec := nWidth
	ELSEIF IsNumeric(nWidth)
		SELF:nWidth := nWidth
	ELSE
		SELF:nWidth := 16
	ENDIF

	// set the HyperLabel object for the column
	IF IsInstanceOfUsual(xColumnID, #HyperLabel)
		oHyperLabel := xColumnID
	ELSEIF IsString(xColumnID)
		oHyperLabel := HyperLabel{String2Symbol(xColumnID), xColumnID}
	ELSEIF IsSymbol(xColumnID)
		oHyperLabel := HyperLabel{xColumnID, Symbol2String(xColumnID)}
	ELSE
		oHyperLabel := HyperLabel{NULL_SYMBOL, NULL_STRING, NULL_STRING, NULL_STRING}
	ENDIF

	IF !IsNil(kAlignment)
		nAlignment := kAlignment
	ENDIF

	// initalize the aValues array
	aValues := {}

	RETURN 

ACCESS NameSym 
	

	RETURN oHyperLabel:NameSym

ASSIGN NameSym(symNewNameSym) 
	LOCAL oOldHL AS HyperLabel

	

	oOldHL := oHyperLabel

	oHyperLabel := HyperLabel{symNewNameSym, oOldHL:Caption, oOldHL:Description, oOldHL:HelpContext}
	RETURN 

ACCESS Owner 
	// DHer: 18/12/2008
RETURN SELF:oOwner


ACCESS Width 
	LOCAL dwIndex AS DWORD
	LOCAL nPixelWidth AS INT
	LOCAL nCharWidth AS INT
	LOCAL pszChar AS PSZ

	

	// if this column is already attached to a ListView control, it may have been
	// resized by the user, so try to determine the updated width, otherwise just
	// return the stored width
	IF oOwner != NULL_OBJECT
		// get the current column width
		dwIndex := oOwner:__GetColumnIndexFromSymbol(SELF:NameSym) - 1
		nPixelWidth := ListView_GetColumnWidth(oOwner:Handle(), INT(_CAST, dwIndex))
		// get the maximum width of a single character
		pszChar := StringAlloc("M")
		nCharWidth := ListView_GetStringWidth(oOwner:Handle(), pszChar)
		MemFree(PTR(_CAST, pszChar))

		// the character width is the pixel width divided by the character width
		nWidth := nPixelWidth / nCharWidth
	ENDIF

	RETURN nWidth

ASSIGN Width(nNewWidth) 
	LOCAL dwIndex AS DWORD
	LOCAL cStringSize AS STRING
	LOCAL pszStringSize AS PSZ
	LOCAL nPixelWidth AS SHORTINT

	

	// if this column is already attached to a ListView control, and the view is report view,
	// the column must be updated visually
	IF (oOwner != NULL_OBJECT) .AND. (oOwner:CurrentView == #ReportView)
		// calculate the pixel width of the column using the given character width
		IF (nNewWidth > 0)
			cStringSize := Replicate("M", nNewWidth)
			IF (NULL_STRING != cStringSize)
				pszStringSize := StringAlloc(cStringSize)
			ENDIF

			// convert the string size into pixel-width
			nPixelWidth := SHORTINT(ListView_GetStringWidth(oOwner:Handle(), pszStringSize))
		ELSE
			nPixelWidth := nNewWidth
		ENDIF

		dwIndex := oOwner:__GetColumnIndexFromSymbol(SELF:NameSym) - 1
		ListView_SetColumnWidth(oOwner:Handle(), INT(_CAST, dwIndex), nPixelWidth)

		IF (PTR(_CAST, pszStringSize) != NULL_PTR)
			MemFree(pszStringSize)
		ENDIF
	ENDIF

	RETURN nWidth := nNewWidth

END CLASS

CLASS ListViewItem INHERIT VObject
	PROTECT nItem AS INT
	PROTECT nSubItem AS INT
	PROTECT nImageIndex AS INT
	PROTECT nStateImage AS INT
	PROTECT nOverlayImage AS INT
	PROTECT lDisabled AS LOGIC
	PROTECT lDropTarget AS LOGIC
	PROTECT lFocused AS LOGIC
	PROTECT lSelected AS LOGIC
	PROTECT dwState AS DWORD
	PROTECT dwStateMask AS DWORD
	PROTECT aColumnText AS ARRAY
	PROTECT aColumnValue AS ARRAY
	PROTECT lParam AS LONGINT
	PROTECT iIndent AS INT

	//PP-030828 Strong typing
	ACCESS __ColumnTextList AS ARRAY STRICT 
	//PP-030828 Strong typing
	

	RETURN aColumnText

ACCESS __ColumnValueList AS ARRAY STRICT 
	//PP-030828 Strong typing
	

	RETURN aColumnValue

ACCESS __lParam AS LONGINT STRICT 
	//PP-030828 Strong typing
	

	RETURN lParam

ASSIGN __lParam(uNewVal AS LONGINT)  STRICT 
	//PP-030828 Strong typing
	

	RETURN (lParam := uNewVal)

ACCESS Checked 
	

	RETURN ((nStateImage-1) > 0)

ASSIGN Checked(lNewVal) 
	

	nStateImage := IIF(lNewVal, 2, 1)
	RETURN 

METHOD Destroy() 
	

	IF !InCollect()
		// if not in garbage collection, free the arrays
		aColumnText := NULL_ARRAY
		aColumnValue := NULL_ARRAY
	ENDIF
	SUPER:Destroy()

	RETURN SELF

ACCESS Disabled 
	

	RETURN lDisabled

ASSIGN Disabled(lEnabled) 
	

	RETURN lDisabled := lEnabled

ACCESS DropTarget 
	

	RETURN lDropTarget

ASSIGN DropTarget(lEnabled) 
	

	RETURN lDropTarget := lEnabled

ACCESS Focused 
	

	RETURN lFocused

ASSIGN Focused(lEnabled) 
	

	RETURN lFocused := lEnabled

METHOD GetText(symColumnName, nRefImageIndex) 
	LOCAL dwIndex, iLen AS DWORD
	LOCAL aColumn AS ARRAY

	

	// search the column's text value array

	//dwIndex := AScan(aColumnText, {|x| x[1] == symColumnName})
	iLen := ALen(aColumnText)
	FOR dwIndex := 1 TO iLen
		IF (aColumnText[dwIndex][1] == symColumnName)
			EXIT
		ENDIF
	NEXT

	IF (dwIndex != (iLen+1))
		aColumn := aColumnText[dwIndex]
		IF IsNumeric(nRefImageIndex)
			nRefImageIndex := aColumn[3]
		ENDIF
		RETURN aColumn[2]
	ELSE
		IF IsNumeric(nRefImageIndex)
			nRefImageIndex := 0l
		ENDIF
	ENDIF

	RETURN NULL_STRING

METHOD GetValue(symColumnName) 
	LOCAL dwIndex, iLen AS DWORD

	

	// search the column's value array
	//dwIndex := AScan(aColumnValue, {|x| x[1] == symColumnName})
	iLen := ALen(aColumnValue)
	FOR dwIndex := 1 TO iLen
		IF (aColumnValue[dwIndex][1] == symColumnName)
			EXIT
		ENDIF
	NEXT

	IF (dwIndex != (iLen+1))
		RETURN aColumnValue[dwIndex][2]
	ENDIF

	RETURN NIL

ACCESS ImageIndex 
	

	RETURN nImageIndex

ASSIGN ImageIndex(nNewImage) 
	

	RETURN nImageIndex := nNewImage


ACCESS Indent 
	

	RETURN iIndent

ASSIGN Indent(iNewIndent) 
	

	RETURN (iIndent := iNewIndent)

CONSTRUCTOR() 
	

	SUPER()

	// initialize the arrays
	aColumnText := {}
	aColumnValue := {}

	RETURN 

ACCESS ItemIndex 
	

	RETURN nItem

ASSIGN ItemIndex(nNewItemIndex) 
	RETURN nItem := nNewItemIndex

ACCESS OverlayImageIndex 
	RETURN nOverlayImage

ASSIGN OverlayImageIndex(nNewOverlayImage) 
	RETURN nOverlayImage := nNewOverlayImage

ACCESS Selected 
	RETURN lSelected

ASSIGN Selected(lEnabled) 
	RETURN lSelected := lEnabled

METHOD SetText(cNewText, symColumnName, nImageIndex) 
	LOCAL dwIndex, iLen AS DWORD
	LOCAL aColumn AS ARRAY

	

	// first attempt to find the entry and replace it
	//dwIndex := AScan(aColumnText, {|x| x[1] == symColumnName})
	iLen := ALen(aColumnText)
	FOR dwIndex := 1 TO iLen
		IF (aColumnText[dwIndex][1] == symColumnName)
			EXIT
		ENDIF
	NEXT

	IF (dwIndex != (iLen+1))
		aColumn    := aColumnText[dwIndex]
		aColumn[2] := cNewText
	ELSE
	   // otherwise, add the new entry to the end of the list
	   aColumn    := {symColumnName, cNewText, 0l}
	   AAdd(aColumnText, aColumn)
	ENDIF

   IF IsNumeric(nImageIndex)
	   aColumn[3] := LONGINT(nImageIndex)
   ENDIF

	RETURN NIL

METHOD SetValue(uNewValue, symColumnName) 
	LOCAL dwIndex, iLen AS DWORD
	// first attempt to find the entry and replace it
	//dwIndex := AScan(aColumnValue, {|x| x[1] == symColumnName})
	iLen := ALen(aColumnValue)
	FOR dwIndex := 1 TO iLen
		IF (aColumnValue[dwIndex][1] == symColumnName)
			EXIT
		ENDIF
	NEXT

	IF (dwIndex != (iLen+1))
		aColumnValue[dwIndex][2] := uNewValue
		RETURN NIL
	ENDIF
	// otherwise, add the new entry to the end of the list
	AAdd(aColumnValue, {symColumnName, uNewValue})

	RETURN NIL

ACCESS StateImageIndex 
	RETURN nStateImage

ASSIGN StateImageIndex(nNewStateImage) 
	RETURN nStateImage := nNewStateImage

END CLASS


#ifdef __VULCAN__
   INTERNAL DELEGATE ListView_CompareDelegate( lParam1 AS LONGINT, lParam2 AS LONGINT, lHandle AS LONGINT) AS INT 
   
   STATIC FUNCTION Get_ListView_ComparePtr() AS PTR
      STATIC LOCAL delListView_CompareDelegate AS ListView_CompareDelegate
      IF delListView_CompareDelegate == NULL
         delListView_CompareDelegate := ListView_CompareDelegate{ NULL, @ListView_Compare() }
      ENDIF
      RETURN System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) delListView_CompareDelegate )
         
#else
   STATIC FUNCTION Get_ListView_ComparePtr() AS PTR
      RETURN @ListView_Compare()
#endif


STATIC FUNCTION ListView_Compare(lParam1 AS LONGINT, lParam2 AS LONGINT, lHandle AS LONGINT) AS INT /* CALLBACK */
	LOCAL hHandle AS PTR
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL strucItem IS _winLV_Item
	LOCAL oListView AS ListView
	LOCAL oLVI1, oLVI2 AS ListViewItem

	//PP-041004
	oLVI1 := NULL_OBJECT
	oLVI2 := NULL_OBJECT

	// retrieve the ListView control handle and ListView object
	hHandle := PTR(_CAST, lHandle)
	oListView := (ListView) __WCGetControlByHandle(hHandle)

	IF (oListView == NULL_OBJECT) .OR. (oListView:__SortRoutineName == NULL_SYMBOL)
		RETURN 0
	ENDIF

	dwCount := oListView:ItemCount

	// search for the ListViewItems that correspond to lParam1 and lParam2
	dwIndex := 0
	// this doesn't work for LV w/ 1 column. Seem to be done inplace then.
	DO WHILE ((oLVI1 == NULL_OBJECT) .OR. (oLVI2 == NULL_OBJECT)) .AND. (dwIndex <= dwCount)
		dwIndex++
		strucItem:iItem := INT(_CAST, dwIndex - 1)
		strucItem:mask := LVIF_PARAM
		IF ListView_GetItem(hHandle,  @strucItem)
			IF (strucItem:lparam == lParam1)
				oLVI1 := oListView:GetItemAttributes(dwIndex)
			ELSEIF (strucItem:lparam == lParam2)
				oLVI2 := oListView:GetItemAttributes(dwIndex)
			ENDIF
		ENDIF
	ENDDO

	IF (dwIndex > dwCount)
		RETURN 0
	ENDIF

	// get the return value from the sort routine
	RETURN Send(oListView, oListView:__SortRoutineName, oLVI1, oLVI2)

