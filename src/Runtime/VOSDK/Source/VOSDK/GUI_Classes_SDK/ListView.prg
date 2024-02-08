/// <include file="Gui.xml" path="doc/ListView/*" />
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
 /// <exclude />
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


 /// <exclude />
METHOD __GetColumnFromIndex(nColumn AS DWORD) AS ListViewColumn
	//PP-030828 Strong typing
	RETURN aColumns[nColumn]


 /// <exclude />
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


 /// <exclude />
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


 /// <exclude />
ACCESS __SortRoutineName AS SYMBOL STRICT
	//PP-030828 Strong typing




	RETURN symSortRoutineName


/// <include file="Gui.xml" path="doc/ListView.AddColumn/*" />
METHOD AddColumn(oListViewColumn)




	RETURN SELF:InsertColumn(oListViewColumn)


/// <include file="Gui.xml" path="doc/ListView.AddGroup/*" />
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


/// <include file="Gui.xml" path="doc/ListView.AddItem/*" />
METHOD AddItem(oListViewItem)




	RETURN SELF:InsertItem(oListViewItem)


/// <include file="Gui.xml" path="doc/ListView.Arrange/*" />
METHOD Arrange(kAlignment)
	RETURN LOGIC(_CAST, SendMessage(SELF:Handle(), LVM_ARRANGE, DWORD(kAlignment), 0))


/// <include file="Gui.xml" path="doc/ListView.BackgroundColor/*" />
ACCESS BackgroundColor
	LOCAL oColor AS Color


	//PP-031001: Fix color assignment
	oColor := Color{}
	oColor:ColorRef := ListView_GetBkColor(SELF:Handle())
	RETURN oColor


/// <include file="Gui.xml" path="doc/ListView.BackgroundColor/*" />
ASSIGN BackgroundColor(oBackgroundColor)
	ListView_SetBkColor(SELF:Handle(), oBackgroundColor:ColorRef)
	RETURN


/// <include file="Gui.xml" path="doc/ListView.CheckBoxes/*" />
ACCESS CheckBoxes
	RETURN SELF:GetExLVStyle(LVS_EX_CHECKBOXES)


/// <include file="Gui.xml" path="doc/ListView.CheckBoxes/*" />
ASSIGN CheckBoxes(lNewVal)
	RETURN SELF:SetExLVStyle(LVS_EX_CHECKBOXES, lNewVal)


/// <include file="Gui.xml" path="doc/ListView.ColumnCount/*" />
ACCESS ColumnCount
	RETURN ALen(aColumns)


/// <include file="Gui.xml" path="doc/ListView.ColumnOrderArray/*" />
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


/// <include file="Gui.xml" path="doc/ListView.ColumnOrderArray/*" />
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


/// <include file="Gui.xml" path="doc/ListView.CurrentView/*" />
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


/// <include file="Gui.xml" path="doc/ListView.DeleteAll/*" />
METHOD DeleteAll()
	LOCAL dwIndex AS DWORD






	// for each column, re-initialize the value array
	FOR dwIndex := 1 TO SELF:ColumnCount
		SELF:__GetColumnFromIndex(dwIndex):__ValueList := {}
	NEXT  // dwIndex


	aDeleted := {}


	RETURN ListView_DeleteAllItems(SELF:Handle())


/// <include file="Gui.xml" path="doc/ListView.DeleteAllColumns/*" />
METHOD DeleteAllColumns()
	//SE-060526
	LOCAL lRet := TRUE AS LOGIC




	WHILE (ALen(aColumns) > 0)
		lRet := lRet .AND. SELF:DeleteColumn(1)
	END


	RETURN lRet


/// <include file="Gui.xml" path="doc/ListView.DeleteColumn/*" />
METHOD DeleteColumn(oListViewColumn)
	//SE-060526
	LOCAL dwIndex AS DWORD
	LOCAL dwCount AS DWORD


	// RvdH 070312 Bug report Markus Feser
	dwCount := ALen(aColumns)


	// find the index of this column in the column list
	IF (oListViewColumn IS ListViewColumn)
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


/// <include file="Gui.xml" path="doc/ListView.DeleteItem/*" />
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


/// <include file="Gui.xml" path="doc/ListView.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER

    if oLargeImageList != NULL_OBJECT
        oLargeImageList:Destroy()
        ListView_SetImageList(SELF:Handle(), NULL, LVSIL_NORMAL)
    	oLargeImageList := NULL_OBJECT
    endif
    if oSmallImageList != NULL_OBJECT
        oSmallImageList:Destroy()
        ListView_SetImageList(SELF:Handle(), NULL, LVSIL_SMALL)
    	oSmallImageList := NULL_OBJECT
    endif
    if oStateImageList != NULL_OBJECT
        oStateImageList:Destroy()
	    ListView_SetImageList(SELF:Handle(), NULL, LVSIL_STATE)
    	oStateImageList := NULL_OBJECT
    endif
    if oDragImageList != NULL_OBJECT
        oDragImageList:Destroy()
    	oDragImageList := NULL_OBJECT
    endif
	aColumns := NULL_ARRAY
	aDeleted := NULL_ARRAY
	SUPER:Destroy()
	RETURN NIL




/// <include file="Gui.xml" path="doc/ListView.DragDropEnabled/*" />
ACCESS DragDropEnabled




	RETURN lDragDropEnabled


/// <include file="Gui.xml" path="doc/ListView.DragImageList/*" />
ACCESS DragImageList




	RETURN oDragImageList


/// <include file="Gui.xml" path="doc/ListView.DragImageList/*" />
ASSIGN DragImageList(oNewDragImageList)




	RETURN oDragImageList := oNewDragImageList


/// <include file="Gui.xml" path="doc/ListView.EditItemLabel/*" />
METHOD EditItemLabel(nItem)




	SELF:SetFocus()
	ListView_EditLabel(SELF:Handle(), INT(nItem - 1))
	RETURN NIL


/// <include file="Gui.xml" path="doc/ListView.EnableDragDrop/*" />
METHOD EnableDragDrop(lEnable)




	DEFAULT(@lEnable, TRUE)


	lDragDropEnabled := lEnable


	RETURN TRUE


/// <include file="Gui.xml" path="doc/ListView.EnableGroupView/*" />
METHOD EnableGroupView(lSetting)
	//PP-030909
	// Listview groups require XP visual styles
	DEFAULT(@lSetting,TRUE)
	// LVM_ENABLEGROUPVIEW: 0=already enabled/disabled, 1=state changed, -1=failed
	RETURN SendMessage(SELF:handle(),LVM_ENABLEGROUPVIEW,DWORD(_CAST,lSetting),0) >= 0


/// <include file="Gui.xml" path="doc/ListView.EnableSort/*" />
METHOD EnableSort(symMethodName)




	RETURN symSortRoutineName := symMethodName


/// <include file="Gui.xml" path="doc/ListView.EnsureVisible/*" />
METHOD EnsureVisible(nItem, lPartiallyVisible)


	Default(@lPartiallyVisible , FALSE)
	RETURN LOGIC(_CAST, ListView_EnsureVisible(SELF:Handle(), INT(nItem - 1), iif(lPartiallyVisible , 1 , 0) ))


/// <include file="Gui.xml" path="doc/ListView.FullRowSelect/*" />
ACCESS FullRowSelect
	RETURN SELF:GetExLVStyle(LVS_EX_FULLROWSELECT)


/// <include file="Gui.xml" path="doc/ListView.FullRowSelect/*" />
ASSIGN FullRowSelect(lNewVal)
	RETURN SELF:SetExLVStyle(LVS_EX_FULLROWSELECT, lNewVal)


/// <include file="Gui.xml" path="doc/ListView.GetAllItems/*" />
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


/// <include file="Gui.xml" path="doc/ListView.GetAllSelectedItems/*" />
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


/// <include file="Gui.xml" path="doc/ListView.GetColumn/*" />
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


/// <include file="Gui.xml" path="doc/ListView.GetExLVStyle/*" />
METHOD GetExLVStyle(kExStyle)
	//SE-060519
	// 2.5b renaming of method - was conflicting with Control:SetExStyle
	LOCAL dwExStyle AS DWORD


	dwExStyle := DWORD(_CAST, SendMessage(SELF:handle(), LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0))


	IF IsLong(kExStyle)
		RETURN (_AND(dwExStyle, DWORD(kExStyle)) > 0)
	ENDIF


   RETURN dwExStyle


/// <include file="Gui.xml" path="doc/ListView.GetGroupTextColor/*" />
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


/// <include file="Gui.xml" path="doc/ListView.GetItemAtPosition/*" />
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


/// <include file="Gui.xml" path="doc/ListView.GetItemAttributes/*" />
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
    EnForceNumeric(@nItem)
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


/// <include file="Gui.xml" path="doc/ListView.GetItemBoundingBox/*" />
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


/// <include file="Gui.xml" path="doc/ListView.GetItemPosition/*" />
METHOD GetItemPosition(nItem)
	LOCAL strucPoint IS _winPoint
	LOCAL oBoundingBox AS BoundingBox






	oBoundingBox := SELF:GetItemBoundingBox(nItem)
	ListView_GetItemPosition(SELF:Handle(), nItem - 1,  @strucPoint)


	RETURN __WCConvertPoint(SELF, Point{strucPoint:x, strucPoint:y + oBoundingBox:Height-1})


/// <include file="Gui.xml" path="doc/ListView.GetItemSpacing/*" />
METHOD GetItemSpacing(symView)






	DEFAULT(@symView, #IconView)


	// this only works for IconView and SmallIconView
	IF symView == #IconView
		RETURN ListView_GetItemSpacing(SELF:Handle(), 0)
	ELSEIF symView == #SmallIconView
		RETURN ListView_GetItemSpacing(SELF:Handle(), 1)
	ENDIF


	RETURN -1


/// <include file="Gui.xml" path="doc/ListView.GetNextItem/*" />
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
			WCError{#GetNextItem, #ListView, __WCSTypeError, nItemStart, 6}:Throw()
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


/// <include file="Gui.xml" path="doc/ListView.GetSelectedColumn/*" />
METHOD GetSelectedColumn()
	//PP-030909
	// Requires XP visual styles
	RETURN SendMessage(SELF:handle(), LVM_GETSELECTEDCOLUMN, 0, 0) + 1




/// <include file="Gui.xml" path="doc/ListView.GetSelectedItem/*" />
METHOD GetSelectedItem()




	RETURN SELF:GetNextItem(LV_GNIBYITEM, FALSE, FALSE, FALSE, TRUE)


/// <include file="Gui.xml" path="doc/ListView.GridLines/*" />
ACCESS GridLines
	RETURN SELF:GetExLVStyle(LVS_EX_GRIDLINES)


/// <include file="Gui.xml" path="doc/ListView.GridLines/*" />
ASSIGN GridLines(lNewVal)
	RETURN SELF:SetExLVStyle(LVS_EX_GRIDLINES, lNewVal)


/// <include file="Gui.xml" path="doc/ListView.HasGroup/*" />
METHOD HasGroup(iGroupId)
	//PP-030909
	// Listview groups require XP visual styles
	RETURN LOGIC(_CAST,SendMessage(SELF:handle(),LVM_HASGROUP,DWORD(_CAST,iGroupId),0))


/// <include file="Gui.xml" path="doc/ListView.HeaderDragDrop/*" />
ACCESS HeaderDragDrop
	RETURN SELF:GetExLVStyle(LVS_EX_HEADERDRAGDROP)


/// <include file="Gui.xml" path="doc/ListView.HeaderDragDrop/*" />
ASSIGN HeaderDragDrop(lNewVal)
	RETURN SELF:SetExLVStyle(LVS_EX_HEADERDRAGDROP, lNewVal)


/// <include file="Gui.xml" path="doc/ListView.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)
	LOCAL dwStyle AS DWORD






	IF IsNil(kStyle)
		dwStyle := WS_BORDER
	ELSE
		dwStyle := _OR(DWORD(kStyle), DWORD(_CAST, WS_BORDER))
	ENDIF


	IF (xID IS ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, , dwStyle, TRUE)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, "SysListView32", dwStyle, TRUE)
	ENDIF
	SELF:SetStyle(LVS_SHAREIMAGELISTS)


	aColumns := {}
	// initialize array of deleted value indizes
	aDeleted := {}


	RETURN


/// <include file="Gui.xml" path="doc/ListView.InsertColumn/*" />
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


/// <include file="Gui.xml" path="doc/ListView.InsertItem/*" />
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
	strucListItem:lparam := IIF((idx == 0), (LONG) ALen(aColumnValues), idx)


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


/// <include file="Gui.xml" path="doc/ListView.IsGroupViewEnabled/*" />
ACCESS IsGroupViewEnabled
	//PP-030909
	// Listview groups require XP visual styles
	RETURN LOGIC(_CAST,SendMessage(SELF:handle(),LVM_ISGROUPVIEWENABLED,0,0))


/// <include file="Gui.xml" path="doc/ListView.ItemCount/*" />
ACCESS ItemCount




	RETURN ListView_GetItemCount(SELF:Handle())


/// <include file="Gui.xml" path="doc/ListView.ItemsPerPage/*" />
ACCESS ItemsPerPage




	RETURN SendMessage(SELF:Handle(), LVM_GETCOUNTPERPAGE, 0, 0)


/// <include file="Gui.xml" path="doc/ListView.LargeImageList/*" />
ACCESS LargeImageList




	RETURN oLargeImageList


/// <include file="Gui.xml" path="doc/ListView.LargeImageList/*" />
ASSIGN LargeImageList(oNewImageList)




	ListView_SetImageList(SELF:Handle(), oNewImageList:Handle(), LVSIL_NORMAL)
	oLargeImageList := oNewImageList


	RETURN


/// <include file="Gui.xml" path="doc/ListView.RedrawRange/*" />
METHOD RedrawRange(oRange)




	RETURN LOGIC(_CAST, ListView_RedrawItems(SELF:Handle(), oRange:Min, oRange:Max))


/// <include file="Gui.xml" path="doc/ListView.RemoveAllGroups/*" />
METHOD RemoveAllGroups()
	//PP-030909
	// Listview groups require XP visual styles
	SendMessage(SELF:handle(),LVM_REMOVEALLGROUPS,0,0)
	RETURN NIL


/// <include file="Gui.xml" path="doc/ListView.RemoveGroup/*" />
METHOD RemoveGroup(iGroupId)
	//PP-030909
	// Listview groups require XP visual styles
	RETURN SendMessage(SELF:handle(),LVM_REMOVEGROUP,DWORD(_CAST,iGroupId),0) >= 0


/// <include file="Gui.xml" path="doc/ListView.Scroll/*" />
METHOD Scroll(oDimension)




	RETURN LOGIC(_CAST, ListView_Scroll(SELF:Handle(), oDimension:Width, -oDimension:Height))


/// <include file="Gui.xml" path="doc/ListView.SearchString/*" />
ACCESS SearchString
	LOCAL pszSearchString AS PSZ
	LOCAL cSearchString AS STRING
	LOCAL DIM aBuf[257] AS BYTE






	pszSearchString:= @aBuf[1]
	ListView_GetISearchString(SELF:Handle(), pszSearchString)
	cSearchString := Psz2String(pszSearchString)


	RETURN cSearchString


/// <include file="Gui.xml" path="doc/ListView.Seek/*" />
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
	IF (uValue IS Point)
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


/// <include file="Gui.xml" path="doc/ListView.SelectedCount/*" />
ACCESS SelectedCount




	RETURN ListView_GetSelectedCount(SELF:Handle())


/// <include file="Gui.xml" path="doc/ListView.SelectItem/*" />
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


/// <include file="Gui.xml" path="doc/ListView.SelectNextItem/*" />
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


/// <include file="Gui.xml" path="doc/ListView.SetBackgroundImage/*" />
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


/// <include file="Gui.xml" path="doc/ListView.SetColumnFormat/*" />
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


/// <include file="Gui.xml" path="doc/ListView.SetExLVStyle/*" />
METHOD SetExLVStyle(kExStyle, lEnable)
	//SE-060519
	// 2.5b renaming of method - was conflicting with Control:SetExStyle






	IF !IsLong(kExStyle)
		WCError{#SetExLVStyle,#ListView,__WCSTypeError,kExStyle,}:Throw()
	ENDIF


	IF IsNil(lEnable) .OR. !IsLogic(lEnable)
		lEnable := TRUE
	ENDIF


	SendMessage(SELF:Handle(), LVM_SETEXTENDEDLISTVIEWSTYLE, kExStyle, IIF(lEnable, kExStyle, 0l))


	RETURN SELF


/// <include file="Gui.xml" path="doc/ListView.SetGroupName/*" />
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


/// <include file="Gui.xml" path="doc/ListView.SetGroupTextColor/*" />
METHOD SetGroupTextColor(oColor)
	//PP-030909
	// Listview groups require XP visual styles
	LOCAL pLVGroupMetric IS _WINLVGROUPMETRICS


	pLVGroupMetric:cbSize := _SIZEOF(_winLVGroupMetrics)


	pLVGroupMetric:mask := LVGMF_TEXTCOLOR
	pLVGroupMetric:crHeader := oColor:colorRef


	SendMessage(SELF:handle(),LVM_SETGROUPMETRICS,0,LONGINT(_CAST,@pLVGroupMetric))
	RETURN SELF




/// <include file="Gui.xml" path="doc/ListView.SetItemAttributes/*" />
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


/// <include file="Gui.xml" path="doc/ListView.SetItemGroupId/*" />
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


/// <include file="Gui.xml" path="doc/ListView.SetItemPosition/*" />
METHOD SetItemPosition(nItem, oPoint)
	LOCAL oBoundingBox AS BoundingBox






	oBoundingBox := SELF:GetItemBoundingBox(nItem)
	oPoint:Y := oPoint:Y + oBoundingBox:Height
	oPoint := __WCConvertPoint(SELF, oPoint)


	RETURN LOGIC(_CAST, ListView_SetItemPosition(SELF:Handle(), nItem - 1, oPoint:X, oPoint:Y))


/// <include file="Gui.xml" path="doc/ListView.SetSelectedColumn/*" />
METHOD SetSelectedColumn(nIndex)
	//PP-030909
	// Requires XP visual styles
	SendMessage(SELF:handle(), LVM_SETSELECTEDCOLUMN, nIndex-1, 0)


	InvalidateRect(SELF:handle(),NULL,TRUE)
	UpdateWindow(SELF:handle())
	RETURN SELF


/// <include file="Gui.xml" path="doc/ListView.SmallImageList/*" />
ACCESS SmallImageList




	RETURN oSmallImageList


/// <include file="Gui.xml" path="doc/ListView.SmallImageList/*" />
ASSIGN SmallImageList(oNewImageList)




	ListView_SetImageList(SELF:Handle(), oNewImageList:Handle(), LVSIL_SMALL)
	oSmallImageList := oNewImageList


	RETURN


/// <include file="Gui.xml" path="doc/ListView.SortItems/*" />
METHOD SortItems()
	LOCAL hListView AS PTR


	// send the sort message, passing the ListView handle as sort data
	hListView := SELF:Handle()


	RETURN LOGIC(_CAST, SendMessage(hListView, LVM_SORTITEMS, DWORD(_CAST, hListView), LONGINT(_CAST, Get_ListView_ComparePtr())))


/// <include file="Gui.xml" path="doc/ListView.StateImageList/*" />
ACCESS StateImageList




	RETURN oStateImageList


/// <include file="Gui.xml" path="doc/ListView.StateImageList/*" />
ASSIGN StateImageList(oNewImageList)




	ListView_SetImageList(SELF:Handle(), oNewImageList:Handle(), LVSIL_STATE)
	oStateImageList := oNewImageList


	RETURN


/// <include file="Gui.xml" path="doc/ListView.TextBackgroundColor/*" />
ACCESS TextBackgroundColor
	LOCAL dwCR AS DWORD

 	IF ( dwCR := ListView_GetTextBkColor(SELF:Handle()) ) == 0xFF000000
 		RETURN Color { GetSysColor ( COLOR_WINDOW ) }
 	ENDIF

 	RETURN Color { dwCR }


/// <include file="Gui.xml" path="doc/ListView.TextBackgroundColor/*" />
ASSIGN TextBackgroundColor(oTextBackgroundColor)




	ListView_SetTextBkColor(SELF:Handle(), oTextBackgroundColor:ColorRef)


	RETURN


/// <include file="Gui.xml" path="doc/ListView.TextColor/*" />
ACCESS TextColor
	LOCAL dwCR AS DWORD

 	IF ( dwCR := ListView_GetTextColor(SELF:Handle()) ) == 0xFF000000
 		RETURN Color { GetSysColor ( COLOR_WINDOWTEXT ) }
 	ENDIF

 	RETURN Color { dwCR }


/// <include file="Gui.xml" path="doc/ListView.TextColor/*" />
ASSIGN TextColor(oNewTextColor)






	ListView_SetTextColor(SELF:Handle(), oNewTextColor:ColorRef)
	RETURN


/// <include file="Gui.xml" path="doc/ListView.TopItem/*" />
ACCESS TopItem




	RETURN SendMessage(SELF:Handle(), LVM_GETTOPINDEX, 0, 0) + 1


/// <include file="Gui.xml" path="doc/ListView.TrackSelection/*" />
ACCESS TrackSelection
	RETURN SELF:GetExLVStyle(LVS_EX_TRACKSELECT)


/// <include file="Gui.xml" path="doc/ListView.TrackSelection/*" />
ASSIGN TrackSelection(lNewVal)
	RETURN SELF:SetExLVStyle(LVS_EX_TRACKSELECT, lNewVal)


/// <include file="Gui.xml" path="doc/ListView.Update/*" />
METHOD Update(nItem)




	RETURN LOGIC(_CAST, ListView_Update(SELF:Handle(), nItem - 1))


/// <include file="Gui.xml" path="doc/ListView.ViewAs/*" />
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


/// <include file="Gui.xml" path="doc/ListView.ViewBoundingBox/*" />
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


/// <include file="Gui.xml" path="doc/ListView.ViewOrigin/*" />
ACCESS ViewOrigin
	LOCAL strucPoint IS _winPoint






	// this call is only meaningful if the current view is list or report
	IF ListView_GetOrigin(SELF:Handle(),  @strucPoint)
		RETURN __WCConvertPoint(SELF, Point{strucPoint:x, strucPoint:y})
	ENDIF


	RETURN Point{}
END CLASS


/// <include file="Gui.xml" path="doc/ListViewColumn/*" />
CLASS ListViewColumn INHERIT VObject
	PROTECT aValues AS ARRAY
	PROTECT nAlignment AS INT
	PROTECT nWidth AS INT
	PROTECT lExplicitFS AS LOGIC
	PROTECT oFieldSpec AS FieldSpec
	PROTECT oHyperLabel AS HyperLabel
	PROTECT oOwner AS ListView


	//PP-030828 Strong typing
 /// <exclude />
	ASSIGN __Owner(oListView AS ListView)  STRICT
	//PP-030828 Strong typing




	RETURN oOwner := oListView


 /// <exclude />
ACCESS __ValueList AS ARRAY STRICT
	//PP-030828 Strong typing




	RETURN aValues


 /// <exclude />
ASSIGN __ValueList(aValueList AS ARRAY)  STRICT
	//PP-030828 Strong typing




	RETURN aValues := aValueList


/// <include file="Gui.xml" path="doc/ListViewColumn.Alignment/*" />
ACCESS Alignment




	RETURN nAlignment


/// <include file="Gui.xml" path="doc/ListViewColumn.Alignment/*" />
ASSIGN Alignment(nNewAlignment)




	RETURN nAlignment := nNewAlignment


/// <include file="Gui.xml" path="doc/ListViewColumn.Caption/*" />
ACCESS Caption




	RETURN oHyperLabel:Caption


/// <include file="Gui.xml" path="doc/ListViewColumn.Caption/*" />
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


/// <include file="Gui.xml" path="doc/ListViewColumn.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF !InCollect()
		// free the aValues array if not in garbage collection
		aValues := NULL_ARRAY
	ENDIF
	SUPER:Destroy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/ListViewColumn.FieldSpec/*" />
ACCESS FieldSpec




	RETURN oFieldSpec


/// <include file="Gui.xml" path="doc/ListViewColumn.FieldSpec/*" />
ASSIGN FieldSpec(oNewFieldSpec)




	IF !(oNewFieldSpec IS FieldSpec)
		WCError{#FieldSpec, #ListViewColumn, __WCSTypeError, oNewFieldSpec, 1}:Throw()
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


/// <include file="Gui.xml" path="doc/ListViewColumn.GetPixWidth/*" />
METHOD GetPixWidth() AS LONG
	LOCAL iCol 		as dword
	LOCAL iPixWidth	AS LONG
	iCol := oOwner:__GetColumnIndexFromSymbol(SELF:NameSym)
	iPixWidth := ListView_GetColumnWidth( oOwner:Handle(), int(_cast,iCol)-1)
	RETURN iPixWidth




/// <include file="Gui.xml" path="doc/ListViewColumn.HyperLabel/*" />
ACCESS HyperLabel




	RETURN oHyperLabel


/// <include file="Gui.xml" path="doc/ListViewColumn.HyperLabel/*" />
ASSIGN HyperLabel(oNewHyperLabel)




	RETURN oHyperLabel := oNewHyperLabel


/// <include file="Gui.xml" path="doc/ListViewColumn.ctor/*" />
CONSTRUCTOR(nWidth, xColumnID, kAlignment)




	SUPER()


	// set the width of the column
	IF nWidth IS FieldSpec VAR oFS
		SELF:nWidth := __GetFSDefaultLength(oFS)
		SELF:FieldSpec := oFS
	ELSEIF IsNumeric(nWidth)
		SELF:nWidth := nWidth
	ELSE
		SELF:nWidth := 16
	ENDIF


	// set the HyperLabel object for the column
	IF xColumnID IS HyperLabel VAR oHL
		oHyperLabel := oHL
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


/// <include file="Gui.xml" path="doc/ListViewColumn.NameSym/*" />
ACCESS NameSym




	RETURN oHyperLabel:NameSym


/// <include file="Gui.xml" path="doc/ListViewColumn.NameSym/*" />
ASSIGN NameSym(symNewNameSym)
	LOCAL oOldHL AS HyperLabel






	oOldHL := oHyperLabel


	oHyperLabel := HyperLabel{symNewNameSym, oOldHL:Caption, oOldHL:Description, oOldHL:HelpContext}
	RETURN


/// <include file="Gui.xml" path="doc/ListViewColumn.Owner/*" />
ACCESS Owner
	// DHer: 18/12/2008
RETURN SELF:oOwner




/// <include file="Gui.xml" path="doc/ListViewColumn.SetPixWidth/*" />
METHOD SetPixWidth(dwPixWidth AS LONG)  AS LOGIC
	LOCAL iCol 	as dword
	LOCAL lSet	AS LOGIC
	iCol := oOwner:__GetColumnIndexFromSymbol(SELF:NameSym)
	lSet := ListView_SetColumnWidth( oOwner:Handle(), int(_cast,iCol)-1 , (SHORT) dwPixWidth)
	RETURN lSet


/// <include file="Gui.xml" path="doc/ListViewColumn.Width/*" />
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


/// <include file="Gui.xml" path="doc/ListViewColumn.Width/*" />
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


/// <include file="Gui.xml" path="doc/ListViewItem/*" />
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
 /// <exclude />
	ACCESS __ColumnTextList AS ARRAY STRICT
	//PP-030828 Strong typing




	RETURN aColumnText


 /// <exclude />
ACCESS __ColumnValueList AS ARRAY STRICT
	//PP-030828 Strong typing




	RETURN aColumnValue


 /// <exclude />
ACCESS __lParam AS LONGINT STRICT
	//PP-030828 Strong typing




	RETURN lParam


 /// <exclude />
ASSIGN __lParam(uNewVal AS LONGINT)  STRICT
	//PP-030828 Strong typing




	RETURN (lParam := uNewVal)


/// <include file="Gui.xml" path="doc/ListViewItem.Checked/*" />
ACCESS Checked




	RETURN ((nStateImage-1) > 0)


/// <include file="Gui.xml" path="doc/ListViewItem.Checked/*" />
ASSIGN Checked(lNewVal)




	nStateImage := IIF(lNewVal, 2, 1)
	RETURN


/// <include file="Gui.xml" path="doc/ListViewItem.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF !InCollect()
		// if not in garbage collection, free the arrays
		aColumnText := NULL_ARRAY
		aColumnValue := NULL_ARRAY
	ENDIF
	SUPER:Destroy()


	RETURN SELF


/// <include file="Gui.xml" path="doc/ListViewItem.Disabled/*" />
ACCESS Disabled




	RETURN lDisabled


/// <include file="Gui.xml" path="doc/ListViewItem.Disabled/*" />
ASSIGN Disabled(lEnabled)




	RETURN lDisabled := lEnabled


/// <include file="Gui.xml" path="doc/ListViewItem.DropTarget/*" />
ACCESS DropTarget




	RETURN lDropTarget


/// <include file="Gui.xml" path="doc/ListViewItem.DropTarget/*" />
ASSIGN DropTarget(lEnabled)




	RETURN lDropTarget := lEnabled


/// <include file="Gui.xml" path="doc/ListViewItem.Focused/*" />
ACCESS Focused




	RETURN lFocused


/// <include file="Gui.xml" path="doc/ListViewItem.Focused/*" />
ASSIGN Focused(lEnabled)




	RETURN lFocused := lEnabled


/// <include file="Gui.xml" path="doc/ListViewItem.GetText/*" />
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


/// <include file="Gui.xml" path="doc/ListViewItem.GetValue/*" />
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


/// <include file="Gui.xml" path="doc/ListViewItem.ImageIndex/*" />
ACCESS ImageIndex




	RETURN nImageIndex


/// <include file="Gui.xml" path="doc/ListViewItem.ImageIndex/*" />
ASSIGN ImageIndex(nNewImage)




	RETURN nImageIndex := nNewImage




/// <include file="Gui.xml" path="doc/ListViewItem.Indent/*" />
ACCESS Indent




	RETURN iIndent


/// <include file="Gui.xml" path="doc/ListViewItem.Indent/*" />
ASSIGN Indent(iNewIndent)




	RETURN (iIndent := iNewIndent)


/// <include file="Gui.xml" path="doc/ListViewItem.ctor/*" />
CONSTRUCTOR()




	SUPER()


	// initialize the arrays
	aColumnText := {}
	aColumnValue := {}


	RETURN


/// <include file="Gui.xml" path="doc/ListViewItem.ItemIndex/*" />
ACCESS ItemIndex




	RETURN nItem


/// <include file="Gui.xml" path="doc/ListViewItem.ItemIndex/*" />
ASSIGN ItemIndex(nNewItemIndex)
	RETURN nItem := nNewItemIndex


/// <include file="Gui.xml" path="doc/ListViewItem.OverlayImageIndex/*" />
ACCESS OverlayImageIndex
	RETURN nOverlayImage


/// <include file="Gui.xml" path="doc/ListViewItem.OverlayImageIndex/*" />
ASSIGN OverlayImageIndex(nNewOverlayImage)
	RETURN nOverlayImage := nNewOverlayImage


/// <include file="Gui.xml" path="doc/ListViewItem.Selected/*" />
ACCESS Selected
	RETURN lSelected


/// <include file="Gui.xml" path="doc/ListViewItem.Selected/*" />
ASSIGN Selected(lEnabled)
	RETURN lSelected := lEnabled


/// <include file="Gui.xml" path="doc/ListViewItem.SetText/*" />
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


/// <include file="Gui.xml" path="doc/ListViewItem.SetValue/*" />
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


/// <include file="Gui.xml" path="doc/ListViewItem.StateImageIndex/*" />
ACCESS StateImageIndex
	RETURN nStateImage


/// <include file="Gui.xml" path="doc/ListViewItem.StateImageIndex/*" />
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


