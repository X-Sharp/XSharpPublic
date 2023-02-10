USING System.Collections.Generic
USING System.Diagnostics
USING Swf := System.Windows.Forms
/// <include file="Gui.xml" path="doc/TreeViewItem.NameSym/*" />

CLASS ListView INHERIT TextControl
	PROTECT lDragDropEnabled AS LOGIC
	PROTECT symSortRoutineName AS SYMBOL
	PROTECT oTextBackColor AS Color
	PROTECT oItemComparer as ListViewItemComparer

	/// <inheritdoc  />
    PROPERTY ControlType AS ControlType  GET ControlType.ListView

/// <include file="Gui.xml" path="doc/ListView.ctor/*" />

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
		SELF:SetStyle(LVS_SHAREIMAGELISTS, TRUE)
		oItemComparer := ListViewItemComparer{SELF}
		RETURN


	STATIC METHOD ListView_GetStringWidth(hwndLV AS IntPtr, _psz AS STRING) AS INT PASCAL
		RETURN (INT(_CAST, (GuiWin32.SendMessage(hwndLV, LVM_GETSTRINGWIDTH, 0, _psz))))


	#region Static Methods - API calls

	STATIC METHOD ListView_Update(hwndLV AS IntPtr, i AS DWORD) AS LOGIC STRICT
		RETURN (LOGIC(_CAST, (GuiWin32.SendMessage((hwndLV), LVM_UPDATE, i, 0L))))

	STATIC METHOD ListView_Scroll(hwndLV AS IntPtr, dx AS INT, dy AS INT) AS LOGIC STRICT
		RETURN (LOGIC(_CAST, (GuiWin32.SendMessage((hwndLV), LVM_SCROLL, DWORD(_CAST,dx), LONGINT(_CAST,dy)))))

	STATIC METHOD ListView_RedrawItems(hwndLV AS IntPtr, iFirst AS INT, iLast AS INT) AS LOGIC STRICT
		RETURN (LOGIC(_CAST, (GuiWin32.SendMessage((hwndLV), LVM_REDRAWITEMS, DWORD(_CAST, iFirst), LONGINT(_CAST, iLast)))))

	//STATIC METHOD ListView_EnsureVisible(hwndLV AS PTR, i AS INT, fPartialOK AS WORD) AS LOGIC
	//	RETURN (LOGIC(_CAST, (GuiWin32.SendMessage((hwndLV), LVM_ENSUREVISIBLE, DWORD(_CAST,i), MAKELPARAM(fPartialOK , 0)))))

	//STATIC METHOD ListView_SetItemPosition(hwndLV AS PTR, i AS INT, x AS WORD, y AS WORD) AS LOGIC STRICT
	//	RETURN (LOGIC(_CAST, (GuiWin32.SendMessage((hwndLV), LVM_SETITEMPOSITION, DWORD(_CAST, i), MAKELPARAM(x, y)))))
	STATIC METHOD ListView_GetNextItem(hwnd AS IntPtr, i AS INT, flags AS WORD) AS INT STRICT
		RETURN (INT(_CAST, (GuiWin32.SendMessage((hwnd), LVM_GETNEXTITEM, DWORD(_CAST,i), MAKELONG((flags), 0)))))

	STATIC METHOD ListView_GetItemSpacing(hwndLV AS IntPtr, fSmall AS DWORD) AS LONG STRICT
		RETURN (LONG(_CAST, (GuiWin32.SendMessage((hwndLV), LVM_GETITEMSPACING, fSmall, 0L))))

	#endregion


	ACCESS __ListView AS IVOListView
		RETURN (IVOListView) oCtrl

	METHOD __GetItemAtIndex(nIndex AS LONG) AS ListViewItem
		IF SELF:__IsValid
			IF nIndex > 0 .and. nIndex <= __ListView:Items:Count
				RETURN __ListView:Items[nIndex-1]:Tag
			ENDIF
		ENDIF
		RETURN NULL_OBJECT

	METHOD __CreateDragImageList(nItem AS DWORD) AS OBJECT STRICT
		//Todo __CreateDragImageList
		//LOCAL hImageList AS PTR
		//LOCAL strucPoint IS _winPoint
		//// create an ImageList handle from a ListView item
		//hImageList := ListView_CreateDragImage(SELF:Handle(), INT(nItem - 1),  @strucPoint)
		//// create an ImageList object from the ImageList handle
		//IF (hImageList != NULL_PTR)
		//	RETURN ImageList{hImageList}
		//ENDIF
		RETURN NULL_OBJECT

	METHOD __GetColumnFromIndex(nColumn AS LONG) AS ListViewColumn
		IF SELF:__IsValid
			IF nColumn <= SELF:__ListView:Columns:Count .and. nColumn > 0
				RETURN SELF:__ListView:Columns[nColumn-1]:Tag
			ENDIF
		ENDIF
		RETURN NULL_OBJECT

	METHOD __GetColumnIndexFromSymbol(symColumnName AS SYMBOL) AS DWORD STRICT
		FOREACH IMPLIED oCol IN __ListView:Columns
			LOCAL oColumn AS ListViewColumn
			oColumn := oCol:Tag
			IF (oColumn:NameSym == symColumnName)
				RETURN oCol:Index+1
			ENDIF
		NEXT  // dwIndex

		RETURN 0
	/*
	METHOD __SetItem(strucItem AS _winLV_Item, oLVItem AS ListViewItem) AS VOID STRICT
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
	*/
	ACCESS __SortRoutineName AS SYMBOL STRICT
		RETURN symSortRoutineName

	METHOD AddColumn(oListViewColumn AS ListViewColumn)
		RETURN SELF:InsertColumn(oListViewColumn)

/// <include file="Gui.xml" path="doc/ListView.AddGroup/*" />
	METHOD AddGroup(iGroupId,cGroupName,dwAlign)
		// iGroupId is stored in the Tag
		LOCAL oGroup AS Swf.ListViewGroup
		oGroup := Swf.ListViewGroup{(STRING) cGroupName}
		oGroup:Tag := (LONG) iGroupId
		IF ! IsNil(dwAlign)
			oGroup:HeaderAlignment := (	System.Windows.Forms.HorizontalAlignment) dwAlign
		ENDIF
		IF SELF:__IsValid
			__ListView:Groups:Add(oGroup)
			RETURN TRUE
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.AddItem/*" />
	METHOD AddItem(oListViewItem AS ListViewItem)
		RETURN SELF:InsertItem(oListViewItem)

/// <include file="Gui.xml" path="doc/ListView.Arrange/*" />
	METHOD Arrange(kAlignment AS LONG)
		IF SELF:__IsValid
			__ListView:ArrangeIcons ((System.Windows.Forms.ListViewAlignment) kAlignment)
			RETURN TRUE
		ENDIF
		RETURN FALSE
/// <include file="Gui.xml" path="doc/ListView.BackgroundColor/*" />
	ACCESS BackgroundColor AS Color
		IF SELF:__IsValid
			RETURN __ListView:BackColor
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/ListView.BackgroundColor/*" />
	ASSIGN BackgroundColor(oBackgroundColor  AS Color)
		IF SELF:__IsValid
			__ListView:BackColor := oBackgroundColor
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ListView.CheckBoxes/*" />
	ACCESS CheckBoxes AS LOGIC
		IF SELF:__IsValid
			RETURN __ListView:CheckBoxes
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.CheckBoxes/*" />
	ASSIGN CheckBoxes(lNewVal AS LOGIC)
		IF SELF:__IsValid
			__ListView:CheckBoxes := lNewVal
		ENDIF
/// <include file="Gui.xml" path="doc/ListView.ColumnCount/*" />
	ACCESS ColumnCount  AS LONG
		IF SELF:__IsValid
			RETURN __ListView:Columns:Count
		ENDIF
		RETURN 0
/// <include file="Gui.xml" path="doc/ListView.ColumnOrderArray/*" />
	ACCESS ColumnOrderArray AS ARRAY
		LOCAL aRet := {} AS ARRAY
		IF SELF:__IsValid
			aRet := ArrayNew(__ListView:Columns:Count)
			FOREACH IMPLIED oHeader IN __ListView:Columns
				aRet[oHeader:Index+1] := oHeader:DisplayIndex+1
			NEXT
		ENDIF
		RETURN aRet

/// <include file="Gui.xml" path="doc/ListView.ColumnOrderArray/*" />
	ASSIGN ColumnOrderArray(aNew AS ARRAY)
		IF SELF:__IsValid
			IF ALen(aNew) == __ListView:Columns:Count
				FOREACH IMPLIED oHeader IN __ListView:Columns
					oHeader:DisplayIndex := aNew[oHeader:Index+1] -1
				NEXT
			ENDIF
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ListView.CurrentView/*" />
	ACCESS CurrentView AS SYMBOL
		LOCAL nView AS System.Windows.Forms.View
		IF SELF:__IsValid
			nView := __ListView:View

			DO CASE
			CASE nView == System.Windows.Forms.View.LargeIcon
				RETURN #IconView

			CASE nView == System.Windows.Forms.View.SmallIcon
				RETURN #SmallIconView

			CASE nView == System.Windows.Forms.View.List
				RETURN #ListView

			CASE nView == System.Windows.Forms.View.Details
				RETURN #ReportView

			CASE nView == System.Windows.Forms.View.Tile
				RETURN #TileView

			END CASE
		ENDIF
		RETURN NULL_SYMBOL

	ASSIGN CurrentView(symView AS SYMBOL)
		LOCAL dwView AS System.Windows.Forms.View
		IF SELF:__IsValid

		DO CASE
		CASE symView == #IconView
			dwView := System.Windows.Forms.View.LargeIcon

		CASE symView == #SmallIconView
			dwView := System.Windows.Forms.View.SmallIcon

		CASE symView == #ListView
			dwView := System.Windows.Forms.View.List

		CASE symView == #ReportView
			dwView := System.Windows.Forms.View.Details

		CASE symView == #Tileview
			dwView := System.Windows.Forms.View.Tile

		OTHERWISE
			dwView := __ListView:View

		END CASE
		__ListView:View := dwView
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ListView.DeleteAll/*" />
	METHOD DeleteAll() AS LOGIC
		LOCAL oWindow AS Window
		IF SELF:__IsValid
			oWindow := (Window) SELF:Owner
			FOREACH item AS VOListViewItem IN __ListView:Items
				oWindow:ListViewItemDelete( ListViewDeleteEvent{SELF, item:Item})
			NEXT
			__ListView:Items:Clear()
			RETURN TRUE
		ENDIF
		RETURN FALSE
/// <include file="Gui.xml" path="doc/ListView.DeleteAllColumns/*" />
	METHOD DeleteAllColumns() AS LOGIC
		IF SELF:__IsValid

		SELF:__ListView:Columns:Clear()
		RETURN TRUE
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.DeleteColumn/*" />
	METHOD DeleteColumn(oListViewColumn AS ListViewColumn)
		LOCAL cKey AS STRING
		IF SELF:__IsValid
			cKey := (STRING) oListViewColumn:NameSym
			IF SELF:__ListView:ContainsColumn(cKey)
				SELF:__ListView:RemoveColumn(cKey)
				RETURN TRUE
			ENDIF
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.DeleteItem/*" />
	METHOD DeleteItem(nItem AS LONG)
		LOCAL IMPLIED oItem := __ListView:Items[nItem-1]
		LOCAL oWindow AS Window
		IF SELF:__IsValid
			IF oItem != NULL_OBJECT
				oWindow := (Window) SELF:Owner
				oWindow:ListViewItemDelete( ListViewDeleteEvent{SELF, oItem:Tag})
				__ListView:Items:Remove(oItem)
			ENDIF
		ENDIF
		RETURN oItem != NULL_OBJECT

	//METHOD Destroy() AS USUAL STRICT
	//	SUPER:Destroy()
	//	RETURN NIL

/// <include file="Gui.xml" path="doc/ListView.DragDropEnabled/*" />
	ACCESS DragDropEnabled AS LOGIC
		RETURN lDragDropEnabled

/// <include file="Gui.xml" path="doc/ListView.DragImageList/*" />
	ACCESS DragImageList AS ImageList
		//Todo	 DragImageList
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/ListView.DragImageList/*" />
	ASSIGN DragImageList(oNewDragImageList AS ImageList)
		//Todo	 DragImageList
		RETURN

/// <include file="Gui.xml" path="doc/ListView.EditItemLabel/*" />
	METHOD EditItemLabel(nItem AS LONG)
		SELF:SetFocus()
		IF SELF:__IsValid
			IF nItem > 0 .and. nItem <= __ListView:Items:Count
				__ListView:Items[nItem]:BeginEdit()
			ENDIF
		ENDIF
		RETURN NIL

/// <include file="Gui.xml" path="doc/ListView.EnableDragDrop/*" />
	METHOD EnableDragDrop(lEnable)


		DEFAULT(@lEnable, TRUE)

		lDragDropEnabled := lEnable

		RETURN TRUE

	METHOD EnableGroupView(lSetting := TRUE AS LOGIC)  AS VOID
		IF SELF:__IsValid
			__ListView:ShowGroups := lSetting
			ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ListView.EnableSort/*" />
	METHOD EnableSort(symMethodName)
		RETURN symSortRoutineName := symMethodName

/// <include file="Gui.xml" path="doc/ListView.EnsureVisible/*" />
	METHOD EnsureVisible(nItem AS LONG, lPartiallyVisible := FALSE AS LOGIC) AS LOGIC
		IF SELF:__IsValid .and. nItem > 0 .and. nItem <= __ListView:Items:Count
			__ListView:Items[nItem-1]:EnsureVisible()
			RETURN TRUE
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.FullRowSelect/*" />
	ACCESS FullRowSelect  AS LOGIC
		IF SELF:ValidateControl()
			RETURN __ListView:FullRowSelect
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.FullRowSelect/*" />
	ASSIGN FullRowSelect(lNewVal AS LOGIC)
		IF SELF:ValidateControl()
			__ListView:FullRowSelect := lNewVal
		ENDIF

/// <include file="Gui.xml" path="doc/ListView.GetAllItems/*" />
	METHOD GetAllItems( )
		LOCAL aSelected AS ARRAY
		IF SELF:__IsValid
			aSelected := {}
			FOREACH oLVItem AS VOListViewItem IN __ListView:Items
				AADD(aSelected,oLVItem:Item)
			NEXT
		ENDIF
		RETURN aSelected

/// <include file="Gui.xml" path="doc/ListView.GetAllSelectedItems/*" />
	METHOD GetAllSelectedItems( ) AS ARRAY
		LOCAL aResult AS ARRAY
		aResult := {}
		IF SELF:__IsValid
			FOREACH oLVItem AS VOListViewItem IN __ListView:SelectedItems
				AADD(aResult,oLVItem:Item)
			NEXT
		ENDIF
		RETURN aResult

/// <include file="Gui.xml" path="doc/ListView.GetColumn/*" />
	METHOD GetColumn(xColumnID AS USUAL)  AS ListViewColumn
		LOCAL dwIndex AS DWORD
		IF SELF:__IsValid

			IF IsSymbol(xColumnID)
				dwIndex := SELF:__GetColumnIndexFromSymbol(xColumnID)
			ELSEIF IsLong(xColumnID)
				dwIndex := xColumnID
			ENDIF
			IF dwIndex > 0
				RETURN __ListView:Columns[(LONG) dwIndex-1]:Tag
			ENDIF
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/ListView.GetExLVStyle/*" />
	METHOD GetExLVStyle(kExStyle AS USUAL)
		LOCAL dwExStyle AS DWORD
		dwExStyle := DWORD(_CAST, GuiWin32.SendMessage(SELF:Handle(), LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0))
		IF IsLong(kExStyle)
			RETURN (_AND(dwExStyle, DWORD(kExStyle)) > 0)
		ENDIF
		RETURN dwExStyle

/// <include file="Gui.xml" path="doc/ListView.GetGroupTextColor/*" />
	METHOD GetGroupTextColor() AS Color
		//PP-030909
		//LOCAL oColor AS Color
		//LOCAL pLVGroupMetric IS _WINLVGROUPMETRICS

		//pLVGroupMetric:cbSize := _SIZEOF(_winLVGroupMetrics)

		//pLVGroupMetric:mask := LVGMF_TEXTCOLOR

		//SendMessage(SELF:handle(),LVM_GETGROUPMETRICS,0,LONGINT(_CAST,@pLVGroupMetric))

		//oColor := COLOR{0}
		//oColor:colorref := pLVGroupMetric:crHeader

		//RETURN oColor
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/ListView.GetItemAtPosition/*" />
	METHOD GetItemAtPosition(oPoint AS Point)  AS ListViewItem
		IF SELF:__IsValid
			LOCAL IMPLIED oLVItem := __ListView:FindNearestItem(System.Windows.Forms.SearchDirectionHint.Down, (System.Drawing.Point) oPoint)
			IF oLVItem != NULL
				RETURN oLVItem:Tag
			ENDIF
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/ListView.GetItemAttributes/*" />
	METHOD GetItemAttributes(nItem AS LONG) AS ListViewItem
		LOCAL oLvItem		AS IVOListViewItem
		LOCAL oListViewItem AS ListViewItem
		LOCAL nIndex		AS LONG
		LOCAL oListViewColumn AS ListViewColumn
		IF nItem > 0 .and. nItem <= __ListView:Items:Count
			oLvItem := (IVOListViewItem) __ListView:Items[nItem-1]
			IF oLvItem:Item != NULL_OBJECT
				oListViewItem	:= oLvItem:Item
			ELSE
				oListViewItem := ListViewItem{oLvItem}

				nIndex  := 0
				FOREACH IMPLIED oSubItem IN oLvItem:SubItems
					++nIndex
					oListViewColumn := SELF:__GetColumnFromIndex(nIndex)
					oListViewItem:SetText(oSubItem:Text, oListViewColumn:NameSym)
				NEXT
			ENDIF
		ENDIF
		RETURN oListViewItem

/// <include file="Gui.xml" path="doc/ListView.GetItemBoundingBox/*" />
	METHOD GetItemBoundingBox(nItem AS LONG) AS BoundingBox
		LOCAL oItem AS IVOListViewItem
		IF nItem < __ListView:Items:Count .and. nItem > 0
			oItem := (IVOListViewItem) __ListView:Items[nItem-1]
			RETURN (BoundingBox) oItem:Bounds
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/ListView.GetItemPosition/*" />
	METHOD GetItemPosition(nItem AS LONG) AS Point
		LOCAL oItem AS IVOListViewItem
		IF nItem < __ListView:Items:Count .and. nItem > 0
			oItem := (IVOListViewItem) __ListView:Items[nItem-1]
			RETURN (Point) oItem:Position
		ENDIF
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/ListView.GetItemSpacing/*" />
	METHOD GetItemSpacing(symView)

		Default(@symView, #IconView)

		// this only works for IconView and SmallIconView
		IF symView == #IconView
			RETURN ListView_GetItemSpacing(SELF:Handle(), DWORD(_CAST, FALSE))
		ELSEIF symView == #SmallIconView
			RETURN ListView_GetItemSpacing(SELF:Handle(), DWORD(_CAST, TRUE))
		ENDIF

		RETURN -1

/// <include file="Gui.xml" path="doc/ListView.GetNextItem/*" />
	METHOD GetNextItem(kRelationship, lDisabled, lDropTarget, lFocused, lSelected, nItemStart)
		LOCAL dwState AS DWORD
		LOCAL nFoundItem AS INT
		LOCAL oListViewItem AS ListViewItem
		// handle default values
		Default(@lDisabled, FALSE)
		Default(@lDropTarget, FALSE)
		Default(@lFocused, FALSE)
		Default(@lSelected, FALSE)

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
			oListViewItem := __ListView:Items[nFoundItem]:Tag
		ENDIF

		RETURN oListViewItem

/// <include file="Gui.xml" path="doc/ListView.GetSelectedColumn/*" />
	METHOD GetSelectedColumn() AS LONG
		RETURN GuiWin32.SendMessage(SELF:Handle(), LVM_GETSELECTEDCOLUMN, 0, 0) + 1

/// <include file="Gui.xml" path="doc/ListView.GetSelectedItem/*" />
	METHOD GetSelectedItem() AS ListViewItem
		IF __ListView:SelectedItems:Count > 0
			RETURN __ListView:SelectedItems[0]:Tag
		ENDIF
		RETURN NULL_OBJECT
/// <include file="Gui.xml" path="doc/ListView.GridLines/*" />
	ACCESS GridLines AS LOGIC
		SELF:ValidateControl()
		RETURN __ListView:GridLines

/// <include file="Gui.xml" path="doc/ListView.GridLines/*" />
	ASSIGN GridLines(lNewVal AS LOGIC)
		SELF:ValidateControl()
		__ListView:GridLines := lNewVal

/// <include file="Gui.xml" path="doc/ListView.HasGroup/*" />
	METHOD HasGroup(iGroupId AS LONG)
		FOREACH IMPLIED oGroup IN __ListView:Groups
			IF (LONG) oGroup:Tag == iGroupId
				RETURN TRUE
			ENDIF
		NEXT
		RETURN FALSE
/// <include file="Gui.xml" path="doc/ListView.HeaderDragDrop/*" />

	ACCESS HeaderDragDrop AS LOGIC
		RETURN __ListView:AllowColumnReorder

/// <include file="Gui.xml" path="doc/ListView.HeaderDragDrop/*" />
	ASSIGN HeaderDragDrop(lNewVal  AS LOGIC)
		__ListView:AllowColumnReorder := TRUE

/// <include file="Gui.xml" path="doc/ListView.InsertColumn/*" />
	METHOD InsertColumn(oListViewColumn AS ListViewColumn, nInsertAfter:= -1 AS LONG) AS LOGIC
		LOCAL oHeader AS IVOColumnHeader
		IF ! SELF:ValidateControl()
			RETURN FALSE
		ENDIF
		oHeader := oListViewColumn:__Header
		IF nInsertAfter == -1
			__ListView:Columns:Add(oHeader:Header)
		ELSE
			__ListView:Columns:Insert(nInsertAfter, oHeader:Header)
		ENDIF
		oHeader:TextAlign := (System.Windows.Forms.HorizontalAlignment) oListViewColumn:Alignment
		oListViewColumn:__Owner := SELF


		RETURN TRUE

/// <include file="Gui.xml" path="doc/ListView.InsertItem/*" />
	METHOD InsertItem(oListViewItem AS ListViewItem, nInsertAfter := -1 AS LONG) AS LOGIC
		LOCAL dwIndex AS LONG
		LOCAL oListViewColumn	AS ListViewColumn
		LOCAL iListViewItem		AS IVOListViewItem
		LOCAL cCaption AS STRING
		// copy the usual values from the item to the column
		IF ! SELF:ValidateControl()
			RETURN FALSE
		ENDIF

		oListViewColumn := SELF:GetColumn(1)
		cCaption := oListViewItem:GetText(oListViewColumn:NameSym)
		iListViewItem := oListViewItem:__ListViewItem
		IF nInsertAfter == -1
			__ListView:Items:Add(iListViewItem:SWFItem)
		ELSE
			 __ListView:Items:Insert(nInsertAfter,iListViewItem:SWFItem)
		ENDIF
		FOR dwIndex := 1 TO __ListView:Columns:Count
			LOCAL IMPLIED oCol := __ListView:Columns[dwIndex-1]
			oListViewColumn := oCol:Tag
			cCaption        := oListViewItem:GetText(oListViewColumn:NameSym)
			IF String.IsNullOrEmpty(cCaption)
				LOCAL IMPLIED uVal := oListViewItem:GetValue(oListViewColumn:NameSym)
				IF uVal != NIL
					cCaption  := AsString(uVal)
				ENDIF
			ENDIF
			IF iListViewItem:SubItems:Count < dwIndex
                VAR subItem := System.Windows.Forms.ListViewItem.ListViewSubItem{iListViewItem:SWFItem, cCaption}
				iListViewItem:SubItems:Add(subItem)
			ELSE
				iListViewItem:SubItems[dwIndex-1]:Text := cCaption
			ENDIF
		NEXT
		iListViewItem:Tag := oListViewItem
		iListViewItem:IndentCount := oListViewItem:Indent

		IF (oListViewItem:StateImageIndex == 0) .AND. SELF:CheckBoxes
			oListViewItem:StateImageIndex := 1
		ENDIF
		IF oTextBackColor != NULL_OBJECT
			iListViewItem:BackColor := oTextBackColor
		ENDIF
		RETURN TRUE

/// <include file="Gui.xml" path="doc/ListView.IsGroupViewEnabled/*" />
	ACCESS IsGroupViewEnabled AS LOGIC
		// Listview groups require XP visual styles
		//RETURN LOGIC(_CAST,SendMessage(SELF:handle(),LVM_ISGROUPVIEWENABLED,0,0))
		RETURN __ListView:ShowGroups

/// <include file="Gui.xml" path="doc/ListView.ItemCount/*" />
	ACCESS ItemCount AS LONG
		RETURN __ListView:Items:Count

/// <include file="Gui.xml" path="doc/ListView.ItemsPerPage/*" />
	ACCESS ItemsPerPage AS LONG
		//tODO ItemsPerPage
		RETURN 0




/// <include file="Gui.xml" path="doc/ListView.LargeImageList/*" />
	ACCESS LargeImageList  as ImageList
		RETURN ImageList{__ListView:LargeImageList}

/// <include file="Gui.xml" path="doc/ListView.LargeImageList/*" />
	ASSIGN LargeImageList(oNewImageList AS ImageList)
		__ListView:LargeImageList := oNewImageList:__ImageList
		RETURN

/// <include file="Gui.xml" path="doc/ListView.RedrawRange/*" />
	METHOD RedrawRange(oRange)
		RETURN LOGIC(_CAST, ListView_RedrawItems(SELF:Handle(), oRange:Min, oRange:Max))

/// <include file="Gui.xml" path="doc/ListView.RemoveAllGroups/*" />
	METHOD RemoveAllGroups() AS VOID
		__ListView:Groups:Clear()
		RETURN

/// <include file="Gui.xml" path="doc/ListView.RemoveGroup/*" />
	METHOD RemoveGroup(iGroupId AS LONG)  AS LOGIC
		FOREACH IMPLIED oGroup IN __ListView:Groups
			IF (LONG) oGroup:Tag == iGroupId
				__ListView:Groups:Remove(oGroup)
				RETURN TRUE
			ENDIF
		NEXT
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.Scroll/*" />
	METHOD Scroll(oDimension)

		RETURN LOGIC(_CAST, ListView_Scroll(SELF:Handle(), oDimension:Width, -oDimension:Height))

/// <include file="Gui.xml" path="doc/ListView.SearchString/*" />
	ACCESS SearchString AS STRING
		//Todo SearchString
		//LOCAL pszSearchString AS PSZ
		//LOCAL cSearchString AS STRING
		//LOCAL DIM aBuf[257] AS BYTE
		//pszSearchString:= @aBuf[1]
		//ListView_GetISearchString(SELF:Handle(), pszSearchString)
		//cSearchString := Psz2String(pszSearchString)

		//RETURN cSearchString
		RETURN ""

/// <include file="Gui.xml" path="doc/ListView.Seek/*" />
	METHOD Seek(uValue, kSeekType, nStart, lWrap, lPartial)
		//Todo Seek
		LOCAL oPoint AS Point
		//LOCAL aValList AS ARRAY
		//LOCAL dwIndex, dwCount AS DWORD
		LOCAL nDir AS System.Windows.Forms.SearchDirectionHint
		LOCAL oLVI AS System.Windows.Forms.ListViewItem
		//LOCAL lParam as LONG
		// start the seek at the beginning by default
		IF IsNil(nStart)
			nStart := -1
		ELSE
			nStart--
		ENDIF

		// find the item closest to the given point
		IF IsInstanceOfUsual(uValue, #Point)
			// kSeekType is a usual
			Default(@kSeekType, LV_SEEKDOWN)

			IF kSeekType == LV_SEEKUP
				nDir := System.Windows.Forms.SearchDirectionHint.Up
			ELSE
				nDir := System.Windows.Forms.SearchDirectionHint.Down
			ENDIF
			oPoint := uValue
			oLVI := __ListView:FindNearestItem(nDir, oPoint)

		ELSEIF kSeekType == LV_SEEKTEXT
			// uValue is the item text, so a text seek is in order


			// set wrap-around (off by default)
			Default(@lWrap, FALSE)

			// set exact search (on by default)
			Default(@lPartial, FALSE)

			// do the seek
			oLVI := __ListView:FindItemWithText((STRING) uValue, FALSE, 0, lPartial)
		ELSEIF kSeekType == LV_SEEKVALUE
			// uValue is the associated usual value, so a usual seek is in order

			// set wrap-around (off by default)
			Default(@lWrap, FALSE)
		ENDIF
		IF oLVI != NULL_OBJECT
			oLVI:EnsureVisible()
			RETURN oLVI:Tag
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/ListView.SelectedCount/*" />
	ACCESS SelectedCount AS LONG
		RETURN __ListView:SelectedItems:Count

/// <include file="Gui.xml" path="doc/ListView.SelectItem/*" />
	METHOD SelectItem(nItem AS LONG, lSelect:= TRUE AS LOGIC) AS LOGIC
		LOCAL oLVI AS ListViewItem
		oLVI := SELF:__GetItemAtIndex(nItem)
		IF oLVI != NULL_OBJECT
			oLVI:Selected := lSelect
			RETURN TRUE
		ENDIF
		RETURN FALSE

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
		__ListView:SelectedIndices:Add(oLVI:Index)
		RETURN TRUE

/// <include file="Gui.xml" path="doc/ListView.SetBackgroundImage/*" />
	METHOD SetBackgroundImage(uImage,dwFlags,xOffSet,yOffSet)
		//Todo SetBackgroundImage
		RETURN FALSE
	// Requires a call to CoInitialize() or the OLE library linked in. If CoInitialize() call CoUnitialize() later.
	//LOCAL pLVBKI IS _WINLVBKIMAGE
	//LOCAL hImage AS PTR
	//LOCAL cURL AS STRING

	//DO CASE
	//CASE IsObject(uImage)
	//	hImage := uImage:handle()
	//	Default(@dwFlags,_OR(LVBKIF_SOURCE_HBITMAP,LVBKIF_STYLE_TILE))
	//CASE IsPtr(uImage)
	//	hImage := uImage
	//	Default(@dwFlags,_OR(LVBKIF_SOURCE_HBITMAP,LVBKIF_STYLE_TILE))
	//CASE IsString(uImage)
	//	cURL := uImage
	//	Default(@dwFlags,_OR(LVBKIF_SOURCE_URL,LVBKIF_STYLE_TILE))
	//	CASE IsNil(uImage)
	//	Default(@dwFlags,LVBKIF_SOURCE_NONE)
	//ENDCASE

	//Default(@xOffSet,0)
	//Default(@yOffSet,0)

	//pLVBKI:ulFlags := dwFlags
	//// _or(LVBKIF_TYPE_WATERMARK,LVBKIF_STYLE_NORMAL,LVBKIF_SOURCE_NONE)
	//// _or(LVBKIF_SOURCE_HBITMAP,LVBKIF_STYLE_TILE)
	//// _or(LVBKIF_SOURCE_URL,LVBKIF_STYLE_TILE)
	////RvdH 060608 optimized
	////IF ! Empty(cURL)
	//IF Slen(cURL) > 0
	//	pLVBKI:pszImage := String2Psz(cURL)
	//ENDIF
	//IF ! hImage == NULL_PTR
	//	pLVBKI:hbm := hImage
	//ENDIF
	//pLVBKI:xOffsetPercent := xOffSet
	//pLVBKI:yOffsetPercent := yOffSet

	//RETURN ! SendMessage(SELF:handle(),LVM_SETBKIMAGE,0,LONGINT(_CAST,@pLVBKI)) == 0

/// <include file="Gui.xml" path="doc/ListView.SetColumnFormat/*" />
	METHOD SetColumnFormat(nCol AS LONG, dwFlag:= 0 AS LONG,nImage := 0 AS LONG)
		//PP-030909
		LOCAL oHeader AS IVOColumnHeader
		IF nCol > 0 .and. nCol  <= __ListView:Columns:Count
			oHeader := (IVOColumnHeader) __ListView:Columns[nCol]
			IF nImage != 0
				oHeader:ImageIndex := nImage-1
			ENDIF
			IF dwFlag != 0
				oHeader:TextAlign := (System.Windows.Forms.HorizontalAlignment) dwFlag
			ENDIF
		ENDIF
		RETURN SELF
	//LOCAL pHeader AS PTR
	//LOCAL pItem IS _WINHDITEM
	//LOCAL dwFmt AS DWORD

	//// HDF_CENTER/HDF_LEFT/HDF_RIGHT, HDF_BITMAP/HDF_BITMAP_ON_RIGHT, HDF_SORTDOWN/HDF_SORTUP
	//// HDF_SORTDOWN/HDF_SORTUP require XP visual styles
	//Default(@dwFlag,0)

	//// image list index
	//Default(@nImage,0)

	//pHeader := PTR(_CAST,SendMessage(SELF:handle(), LVM_GETHEADER, 0,0))
	//pItem:mask := _OR(HDI_IMAGE,HDI_FORMAT)

	//dwFmt := _OR(HDF_STRING,DWORD(dwFlag))
	//IF nImage > 0
	//	dwFmt := _OR(dwFmt,HDF_IMAGE)
	//	pItem:iImage := nImage-1
	//ENDIF
	//pItem:fmt := INT(dwFmt)

	//RETURN ! SendMessage(pHeader, HDM_SETITEM, nCol-1, LONGINT(_CAST,@pItem)) == 0

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

		GuiWin32.SendMessage(SELF:Handle(), LVM_SETEXTENDEDLISTVIEWSTYLE, kExStyle, (LONG) IIF(lEnable, kExStyle, 0l))

		RETURN SELF

/// <include file="Gui.xml" path="doc/ListView.SetGroupName/*" />
	METHOD SetGroupName(iGroupId AS LONG,cGroupName AS STRING,dwAlign := -1 AS LONG) as LOGIC
		FOREACH IMPLIED oGroup IN __ListView:Groups
			IF (LONG) oGroup:Tag == iGroupId
				oGroup:Name := cGroupName
				IF dwAlign != -1
					oGroup:HeaderAlignment := (System.Windows.Forms.HorizontalAlignment) dwAlign
				ENDIF
				RETURN TRUE
			ENDIF
		NEXT
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.SetGroupTextColor/*" />
	METHOD SetGroupTextColor(oColor)
		//LOCAL pLVGroupMetric IS _WINLVGROUPMETRICS

		//pLVGroupMetric:cbSize := _SIZEOF(_winLVGroupMetrics)

		//pLVGroupMetric:mask := LVGMF_TEXTCOLOR
		//pLVGroupMetric:crHeader := oColor:colorRef

		//SendMessage(SELF:handle(),LVM_SETGROUPMETRICS,0,LONGINT(_CAST,@pLVGroupMetric))
		RETURN SELF


/// <include file="Gui.xml" path="doc/ListView.SetItemAttributes/*" />
	METHOD SetItemAttributes(oListViewItem AS ListViewItem)
		LOCAL oLvItem		AS IVOListViewItem
		LOCAL nIndex		AS LONG
		LOCAL oListViewColumn AS ListViewColumn
		IF oListViewItem != NULL_OBJECT
			oLvItem := oListViewItem:__ListViewItem
			nIndex  := 0
			FOREACH IMPLIED oSubItem IN oLvItem:SubItems
				++nIndex
				oListViewColumn := SELF:__GetColumnFromIndex(nIndex)
				oSubItem:Text := oListViewItem:GetText(oListViewColumn:NameSym)
			NEXT
		ENDIF
		RETURN NIL

/// <include file="Gui.xml" path="doc/ListView.SetItemGroupId/*" />
	METHOD SetItemGroupId(uLVI AS USUAL,nId AS LONG) AS LOGIC
		LOCAL oListViewItem AS ListViewItem
		IF IsNumeric(uLVI)
			oListViewItem := SELF:__GetItemAtIndex((LONG) uLVI)
		ELSEIF IsInstanceOfUsual(uLVI,#ListViewItem)
			oListViewItem := uLVI
		ENDIF
		IF oListViewItem != NULL_OBJECT
			FOREACH IMPLIED oG IN SELF:__ListView:Groups
				IF (LONG) oG:Tag == nId
					oListViewItem:__ListViewItem:Group :=  oG
					RETURN TRUE
				ENDIF
			NEXT
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.SetItemPosition/*" />
	METHOD SetItemPosition(nItem AS LONG, oPoint AS Point) AS LOGIC
		LOCAL oListViewItem AS ListViewItem
		oListViewItem := SELF:__GetItemAtIndex(nItem)
		IF oListViewItem != NULL_OBJECT
			oListViewItem:__ListViewItem:Position := oPoint
			RETURN TRUE
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListView.SetSelectedColumn/*" />
	METHOD SetSelectedColumn(nIndex AS LONG) AS LOGIC
		//Todo	 SetSelectedColumn
		//SendMessage(SELF:handle(), LVM_SETSELECTEDCOLUMN, nIndex-1, 0)

		//InvalidateRect(SELF:handle(),NULL,TRUE)
		//UpdateWindow(SELF:handle())
		RETURN FALSE

	METHOD SetStyle(kStyle AS LONG, lEnable AS LOGIC) AS USUAL

		DO CASE
		CASE kStyle == LVS_NOCOLUMNHEADER
			IF lEnable
				SELF:__ListView:HeaderStyle := System.Windows.Forms.ColumnHeaderStyle.None
			ELSE
				SELF:__ListView:HeaderStyle := System.Windows.Forms.ColumnHeaderStyle.Clickable
			ENDIF
		CASE kStyle == LVS_SORTASCENDING
			IF lEnable
				SELF:__ListView:Sorting := System.Windows.Forms.SortOrder.Ascending
			ELSE
				SELF:__ListView:Sorting := System.Windows.Forms.SortOrder.None
			ENDIF
		ENDCASE
		RETURN SUPER:SetStyle(kStyle, lEnable)

/// <include file="Gui.xml" path="doc/ListView.SmallImageList/*" />
	ACCESS SmallImageList  AS ImageList
		//ListView_SetImageList(SELF:Handle(), oNewImageList:Handle(), LVSIL_SMALL)
		RETURN ImageList{__ListView:SmallImageList}

/// <include file="Gui.xml" path="doc/ListView.SmallImageList/*" />
	ASSIGN SmallImageList(oNewImageList AS ImageList)
		__ListView:SmallImageList := oNewImageList:__ImageList
		RETURN

/// <include file="Gui.xml" path="doc/ListView.SortItems/*" />
	METHOD SortItems()
		IF SELF:symSortRoutineName != NULL_SYMBOL
			__ListView:ListViewItemSorter := oItemComparer
			__ListView:Sort()
		ENDIF
		RETURN TRUE

/// <include file="Gui.xml" path="doc/ListView.StateImageList/*" />
	ACCESS StateImageList
		RETURN ImageList{__ListView:StateImageList}

/// <include file="Gui.xml" path="doc/ListView.StateImageList/*" />
	ASSIGN StateImageList(oNewImageList)
		__ListView:StateImageList := oNewImageList:__ImageList
		RETURN

	/// <inheritdoc />
	METHOD RestoreUpdate() AS VOID STRICT
		SELF:__ListView:EndUpdate()
		SUPER:RestoreUpdate()

	/// <inheritdoc />
	METHOD SuspendUpdate() AS VOID STRICT
		SELF:__ListView:BeginUpdate()
		SUPER:SuspendUpdate()

/// <include file="Gui.xml" path="doc/ListView.TextBackgroundColor/*" />
	ACCESS TextBackgroundColor AS Color
		RETURN oTextBackColor

/// <include file="Gui.xml" path="doc/ListView.TextBackgroundColor/*" />
	ASSIGN TextBackgroundColor(oTextBackgroundColor AS Color)
		oTextBackColor := oTextBackgroundColor
		FOREACH oItem AS System.Windows.Forms.ListViewItem IN __ListView:Items
			oItem:BackColor := (System.Drawing.Color) oTextBackgroundColor
		NEXT
		RETURN

/// <include file="Gui.xml" path="doc/ListView.TextColor/*" />
	ACCESS TextColor AS Color
		RETURN (Color) __ListView:ForeColor

/// <include file="Gui.xml" path="doc/ListView.TextColor/*" />

	ASSIGN TextColor(oNewTextColor AS Color)
		__ListView:ForeColor := oNewTextColor
		RETURN

/// <include file="Gui.xml" path="doc/ListView.TopItem/*" />
	ACCESS TopItem AS LONG
		LOCAL oItem AS System.Windows.Forms.ListViewItem
		oItem := __ListView:TopItem
		IF oItem != NULL
			RETURN oItem:Index +1
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/ListView.TrackSelection/*" />
	ACCESS TrackSelection AS LOGIC
		RETURN __ListView:HotTracking

/// <include file="Gui.xml" path="doc/ListView.TrackSelection/*" />
	ASSIGN TrackSelection(lNewVal AS LOGIC)
		__ListView:HotTracking := lNewVal

/// <include file="Gui.xml" path="doc/ListView.Update/*" />
	METHOD Update(nItem AS DWORD)
		RETURN LOGIC(_CAST, ListView_Update(SELF:Handle(), nItem - 1))

	METHOD ViewAs(symView AS SYMBOL) AS VOID
		SELF:CurrentView := symView
		RETURN

/// <include file="Gui.xml" path="doc/ListView.ViewBoundingBox/*" />
	ACCESS ViewBoundingBox AS BoundingBox
		//Todo ViewBoundingBox
		//LOCAL strucRect IS _winRect
		//LOCAL oOrigin AS Point
		//LOCAL oSize AS Dimension

		//IF LOGIC(_CAST, ListView_GetViewRect(SELF:Handle(),  @strucRect))
		//	//PP-030910
		//	oOrigin := Point{strucRect:left, strucRect:bottom}
		//	oOrigin := oOrigin
		//	oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}
		//	RETURN BoundingBox{oOrigin, oSize}
		//ENDIF

		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/ListView.ViewOrigin/*" />
	ACCESS ViewOrigin AS Point
		//Todo ViewOrigin
		//LOCAL strucPoint IS _winPoint
		//// this call is only meaningful if the current view is list or report
		//IF ListView_GetOrigin(SELF:Handle(),  @strucPoint)
		//	RETURN Point{strucPoint:x, strucPoint:y}
		//ENDIF
		RETURN NULL_OBJECT
END CLASS

/// <include file="Gui.xml" path="doc/ListViewColumn/*" />
[DebuggerDisplay("{NameSym}")];
CLASS ListViewColumn INHERIT VObject
	PROTECT nWidth AS INT
	PROTECT lExplicitFS AS LOGIC
	PROTECT oFieldSpec AS FieldSpec
	PROTECT oHyperLabel AS HyperLabel
	PROTECT oOwner AS ListView
	PROTECT oHeader AS IVOColumnHeader

	PROPERTY __Header AS IVOColumnHeader GET oHeader SET oHeader := VALUE
	PROPERTY __Owner AS ListView
		GET
		RETURN oOwner
		END GET
		SET
			oOwner := Value
			SELF:__SetWidth()
		END SET
	END PROPERTY

/// <include file="Gui.xml" path="doc/ListViewColumn.Alignment/*" />
	ACCESS Alignment AS LONG
		RETURN (LONG) oHeader:TextAlign

	ASSIGN Alignment(nNewAlignment  AS LONG)
		oHeader:TextAlign := (System.Windows.Forms.HorizontalAlignment) nNewAlignment

/// <include file="Gui.xml" path="doc/ListViewColumn.Caption/*" />
	ACCESS Caption AS STRING
		RETURN oHyperLabel:Caption

	ACCESS Index AS LONG
		RETURN oHeader:Index


/// <include file="Gui.xml" path="doc/ListViewColumn.Caption/*" />
	ASSIGN Caption(cNewCaption AS STRING)
		LOCAL oOldHL AS HyperLabel
		oHeader:Text := cNewCaption
		oOldHL := oHyperLabel
		oHyperLabel := HyperLabel{oOldHL:NameSym, cNewCaption, oOldHL:Description, oOldHL:HelpContext}
		RETURN

	//METHOD Destroy() AS USUAL STRICT
	//	SUPER:Destroy()
	//	RETURN SELF

/// <include file="Gui.xml" path="doc/ListViewColumn.FieldSpec/*" />
	ACCESS FieldSpec  AS FieldSpec
		RETURN oFieldSpec


/// <include file="Gui.xml" path="doc/ListViewColumn.FieldSpec/*" />
	ASSIGN FieldSpec(oNewFieldSpec AS FieldSpec)
		// set up FieldSpec information for the column
		oFieldSpec := oNewFieldSpec
		lExplicitFS := TRUE
		IF oFieldSpec:ValType == "N"
			IF SubStr(oFieldSpec:Picture, 1, 2) == "@B"
				SELF:Alignment := LVCFMT_LEFT
			ELSE
				SELF:Alignment := LVCFMT_RIGHT
			ENDIF
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ListViewColumn.HyperLabel/*" />
	PROPERTY HyperLabel AS HyperLabel GET oHyperLabel SET oHyperLabel := Value

	CONSTRUCTOR(nWidth, xColumnID, kAlignment)
		SUPER()
		oHeader := (IVOColumnHeader) GUIFactory.Instance:CreateListViewElement(ControlType.ListViewColumn, SELF)
		// set the width of the column
		IF IsInstanceOfUsual(nWidth, #FieldSpec)
			self:nWidth := __GetFSDefaultLength((FieldSpec) nWidth)
			SELF:FieldSpec := nWidth
		ELSEIF IsNumeric(nWidth)
			self:nWidth := nWidth
		ELSE
			self:nWidth := 16
		ENDIF
		oHeader:Width := nWidth

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

		oHeader:Text := oHyperLabel:Caption
		IF IsNumeric(kAlignment)
			SELF:Alignment := kAlignment
		ENDIF

		RETURN

	ACCESS NameSym AS SYMBOL
		RETURN oHyperLabel:NameSym

/// <include file="Gui.xml" path="doc/ListViewColumn.NameSym/*" />
	ASSIGN NameSym(symNewNameSym AS SYMBOL)
		LOCAL oOldHL AS HyperLabel
		oOldHL := oHyperLabel
		oHyperLabel := HyperLabel{symNewNameSym, oOldHL:Caption, oOldHL:Description, oOldHL:HelpContext}
		RETURN

/// <include file="Gui.xml" path="doc/ListViewColumn.Owner/*" />
	ACCESS Owner
		RETURN SELF:oOwner

/// <include file="Gui.xml" path="doc/ListViewColumn.Width/*" />
	ACCESS Width AS LONG
		LOCAL nWidth as LONG
		IF SELF:Owner != NULL_OBJECT
			nWidth := ListView.ListView_GetStringWidth(SELF:Owner:Handle(), "M")
			nWidth := SELF:__Header:Width / nWidth

			RETURN nWidth
		ENDIF

		RETURN oHeader:Width

	METHOD __SetWidth() AS VOID
		SELF:Width := oheader:Width


/// <include file="Gui.xml" path="doc/ListViewColumn.Width/*" />
	ASSIGN Width(nNewWidth  AS LONG)
		IF nNewWidth == -2
			oHeader:AutoResize(System.Windows.Forms.ColumnHeaderAutoResizeStyle.HeaderSize)
		ELSEIF nNewWidth == -1
			oHeader:AutoResize(System.Windows.Forms.ColumnHeaderAutoResizeStyle.ColumnContent)
		ELSE
			IF nNewWidth > 0
				//oHeader:Width := nNewWidth *2
                oHeader:AutoResize(System.Windows.Forms.ColumnHeaderAutoResizeStyle.ColumnContent)
			ELSE
				oHeader:Width := nNewWidth * -1
			ENDIF
		ENDIF

END CLASS

/// <include file="Gui.xml" path="doc/ListViewItem/*" />

[DebuggerDisplay("Index: {Index}")];
CLASS ListViewItem INHERIT VObject
	PROTECT nSubItem AS INT
	PROTECT nOverlayImage AS INT
	PROTECT lDisabled AS LOGIC
	PROTECT lDropTarget AS LOGIC
	PROTECT dwState AS DWORD
	PROTECT dwStateMask AS DWORD
	PROTECT aColumnText AS Dictionary<SYMBOL, Tuple<STRING, LONG > >
	PROTECT aColumnValue AS Dictionary<SYMBOL, USUAL>
	PROTECT lParam AS LONGINT
	PROTECT oItem AS IVOListViewItem

	ACCESS __ListViewItem AS IVOListViewItem
		RETURN oItem

 /// <exclude />
	ACCESS __ColumnValueList AS Dictionary<SYMBOL, USUAL> STRICT
		RETURN aColumnValue

/// <include file="Gui.xml" path="doc/ListViewItem.Checked/*" />
	ACCESS Checked AS LOGIC
		IF oItem != NULL_OBJECT
			RETURN oItem:Checked
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ListViewItem.Checked/*" />
	ASSIGN Checked(lNewVal AS LOGIC)
		IF oItem != NULL_OBJECT
			oItem:Checked := lNewVal
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ListViewItem.Destroy/*" />
	METHOD Destroy() AS USUAL
		aColumnText := NULL
		aColumnValue := NULL
		SUPER:Destroy()
		RETURN SELF

/// <include file="Gui.xml" path="doc/ListViewItem.Disabled/*" />
	ACCESS Disabled AS LOGIC
		RETURN lDisabled

/// <include file="Gui.xml" path="doc/ListViewItem.Disabled/*" />
	ASSIGN Disabled(lEnabled AS LOGIC)
		lDisabled := lEnabled

/// <include file="Gui.xml" path="doc/ListViewItem.DropTarget/*" />
	ACCESS DropTarget  AS LOGIC
		//Todo DropTarget
		RETURN lDropTarget

/// <include file="Gui.xml" path="doc/ListViewItem.DropTarget/*" />
	ASSIGN DropTarget(lEnabled AS LOGIC)
		//Todo DropTarget
		lDropTarget := lEnabled

/// <include file="Gui.xml" path="doc/ListViewItem.Focused/*" />
	ACCESS Focused  AS LOGIC
		IF oItem != NULL_OBJECT
			RETURN oItem:Focused
		ENDIF
		RETURN FALSE
/// <include file="Gui.xml" path="doc/ListViewItem.Focused/*" />

	ASSIGN Focused(lEnabled AS LOGIC)
		IF oItem != NULL_OBJECT
			oItem:Focused := lEnabled
		ENDIF
/// <include file="Gui.xml" path="doc/ListViewItem.GetText/*" />

	METHOD GetText(symColumnName AS SYMBOL) AS STRING
		LOCAL dummy AS LONG
		RETURN SELF:GetText(symColumnName, REF dummy)

/// <include file="Gui.xml" path="doc/ListViewItem.GetText/*" />
	METHOD GetText(symColumnName AS SYMBOL, nRefImageIndex REF LONG) AS STRING
		IF aColumnText:ContainsKey( symColumnName)
			LOCAL IMPLIED item := aColumnText[symColumnName]
			nRefImageIndex := item:Item2
			RETURN item:Item1
		ENDIF
		RETURN NULL_STRING


/// <include file="Gui.xml" path="doc/ListViewItem.GetValue/*" />
	METHOD GetValue(symColumnName := NULL_SYMBOL AS SYMBOL) AS USUAL
        IF symColumnName != NULL_SYMBOL
		    IF aColumnValue:ContainsKey( symColumnName)
			    RETURN aColumnValue[symColumnName]
		    ENDIF
        ENDIF
		RETURN NIL

/// <include file="Gui.xml" path="doc/ListViewItem.ImageIndex/*" />
	ACCESS ImageIndex AS LONG
		IF oItem != NULL_OBJECT
			RETURN oItem:ImageIndex+1
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/ListViewItem.ImageIndex/*" />
	ASSIGN ImageIndex(nNewImage AS LONG)
		IF oItem != NULL_OBJECT
			oItem:ImageIndex := nNewImage-1
		ENDIF
	ACCESS Index AS LONG
		IF oItem != NULL_OBJECT
			RETURN oItem:Index +1
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/ListViewItem.Indent/*" />
	ACCESS Indent AS LONG
		IF oItem != NULL_OBJECT
			RETURN oItem:IndentCount
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/ListViewItem.Indent/*" />
	ASSIGN Indent(iNewIndent AS LONG)
		IF oItem != NULL_OBJECT
			oItem:IndentCount := iNewIndent
		ENDIF

	CONSTRUCTOR() STRICT
		LOCAL oItem AS IVOListViewItem
		oItem := (IVOListViewItem) GuiFactory.Instance:CreateListViewElement(Controltype.ListViewItem, SELF)
		SELF(oItem)
		RETURN


	CONSTRUCTOR(loItem AS IVOListViewItem)
		SUPER()
		SELF:oItem := loItem
		oItem:LinkTo(SELF)
		// initialize the arrays
		aColumnText := Dictionary<SYMBOL, Tuple<STRING, LONG> >{}
		aColumnValue := Dictionary<SYMBOL, USUAL> {}

		RETURN

/// <include file="Gui.xml" path="doc/ListViewItem.ItemIndex/*" />
	ACCESS ItemIndex AS LONG
		IF oItem != NULL_OBJECT
			RETURN oItem:Index+1
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/ListViewItem.OverlayImageIndex/*" />
	ACCESS OverlayImageIndex AS LONG
		//todo OverlayImageIndex
		// see http://www.hightechtalks.com/dotnet-framework-winforms-controls/overlay-image-listview-221468.html
		RETURN nOverlayImage

/// <include file="Gui.xml" path="doc/ListViewItem.OverlayImageIndex/*" />
	ASSIGN OverlayImageIndex(nNewOverlayImage AS LONG)
		nOverlayImage := nNewOverlayImage

/// <include file="Gui.xml" path="doc/ListViewItem.Selected/*" />
	ACCESS Selected AS LOGIC
		IF oItem != NULL_OBJECT
			RETURN oItem:Selected
		ENDIF
		RETURN FALSE

	ASSIGN Selected(lEnabled AS LOGIC)
		IF oItem != NULL_OBJECT
			oItem:Selected:= lEnabled
		ENDIF

/// <include file="Gui.xml" path="doc/ListViewItem.SetText/*" />
	METHOD SetText(cNewText AS STRING, symColumnName AS SYMBOL, nImageIndex:= -1 AS LONG) AS VOID
		IF aColumnText:ContainsKey(symColumnName)
			LOCAL IMPLIED item := aColumnText[symColumnName]
			IF nImageIndex == -1
				nImageIndex := item:Item2
			ENDIF
			aColumnText[symColumnName] := Tuple<STRING, LONG>{ cNewText, nImageIndex }
		ELSE
			aColumnText:Add(symColumnName, Tuple<STRING, LONG> {cNewText, nImageIndex})
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ListViewItem.SetValue/*" />
	METHOD SetValue(uNewValue AS USUAL, symColumnName AS SYMBOL) AS VOID
		IF aColumnValue:ContainsKey(symColumnName)
			aColumnValue[symColumnName] := uNewValue
		ELSE
			aColumnValue:Add(symColumnName, uNewValue)
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/ListViewItem.StateImageIndex/*" />
	ACCESS StateImageIndex AS LONG
		IF oItem != NULL_OBJECT
			RETURN oItem:StateImageIndex+1
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/ListViewItem.StateImageIndex/*" />
	ASSIGN StateImageIndex(nNewStateImage AS LONG)
		IF oItem != NULL_OBJECT
			oItem:StateImageIndex := nNewStateImage -1
		ENDIF

END CLASS

USING System.Collections
USING System.Reflection
CLASS ListViewItemComparer IMPLEMENTS IComparer
	PROTECT oListView AS ListView
	PROTECT symMethod AS SYMBOL
	CONSTRUCTOR(oLv AS ListView)
		oListView := oLv

	PUBLIC METHOD Compare(x AS OBJECT , y AS OBJECT ) AS INT
		IF oListView:__SortRoutineName != NULL_SYMBOL
			IF oListView:__SortRoutineName != symMethod
				IF IsMethod(oListView, oListView:__SortRoutineName)
					symMethod := oListView:__SortRoutineName
				ENDIF
            ENDIF
        	IF symMethod != NULL_SYMBOL
			    VAR oItem1 := (VOListViewItem) x
			    VAR oItem2 := (VOListViewItem) y
                RETURN __InternalSend(oListview, symMethod, oItem1:Item, oItem2:Item)
			ENDIF
		ENDIF
		RETURN 0

END CLASS
