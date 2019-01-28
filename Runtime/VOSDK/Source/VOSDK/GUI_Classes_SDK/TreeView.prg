CLASS TreeView INHERIT TextControl
	PROTECT oImageList AS ImageList
	PROTECT oStateImageList AS ImageList
	PROTECT oDragImageList AS ImageList
	PROTECT aTreeItems AS ARRAY
	PROTECT aValues AS ARRAY
	PROTECT lDragDropEnabled AS LOGIC
	PROTECT symSortRoutineName AS SYMBOL
	PROTECT dwItemCount AS DWORD

	//PP-030828 Strong typing
	//SE-060523

METHOD __CreateDragImageList(symItem AS USUAL) AS OBJECT STRICT 
	//PP-030828 Strong typing
	//PP-040101 Corrected data type of symItem - note SELF:__GetHandleFromSymbol takes a symbol or a treeviewitem
	LOCAL hThisItem AS PTR
	LOCAL hImageList AS PTR

	

	hThisItem := SELF:__GetHandleFromSymbol(symItem)

	IF (hThisItem != NULL_PTR)
		hImageList := TreeView_CreateDragImage(SELF:Handle(), hThisItem)
		IF (hImageList != NULL_PTR)
			RETURN ImageList{hImageList}
		ENDIF
	ENDIF

	RETURN NULL_OBJECT

METHOD __Expand(hItem AS PTR, dwMode AS DWORD, lAll AS LOGIC, lForceNotify AS LOGIC) AS LOGIC 
	//SE-060523
   LOCAL hChild  AS PTR
   LOCAL hOwner  AS PTR
   LOCAL sNMTV   IS _winNM_TREEVIEW
   LOCAL sTVItem AS _winTV_ITEM
   LOCAL lDo     AS LOGIC
   LOCAL lOK     AS LOGIC

   IF lForceNotify
   	sTVItem := @sNMTV:itemNew
   	sTVItem:hItem := hItem
   	sTVItem:mask  := TVIF_STATE
      IF ! TreeView_GetItem(hWnd, sTVItem)
      	RETURN FALSE
      ENDIF
      IF dwMode = TVE_TOGGLE
      	lDo := TRUE
      ELSE
	      IF _And(sTVItem:State, TVIS_EXPANDED) = TVIS_EXPANDED
	      	lDo := (dwMode = TVE_COLLAPSE)
	      ELSE
	      	lDo := (dwMode = TVE_EXPAND)
		   ENDIF
		ENDIF
	   lOK := TRUE
	   IF lDo
	   	IF _And(sTVItem:State, TVIS_EXPANDEDONCE) = TVIS_EXPANDEDONCE
	   		IF oFormSurface != NULL_OBJECT
	   			hOwner := oFormSurface:Handle()
	   		ELSE
	            hOwner := GetParent(hWnd)
	   		ENDIF
			   sNMTV:action       := dwMode
			   sNMTV:hdr:hwndFrom := hWnd
			   sNMTV:hdr:_code    := DWORD(TVN_ITEMEXPANDING)
		      lDo := ! LOGIC(_CAST, SendMessage(hOwner, WM_NOTIFY, 0, LONGINT(_CAST, @sNMTV)))
		   ENDIF
		   IF lDo
		   	IF (lOK := TreeView_Expand(hWnd, hItem, dwMode))
			   	IF hOwner != Null_Ptr
			   		sNMTV:action       := dwMode
			         sNMTV:hdr:hwndFrom := hWnd
			   		sNMTV:hdr:_code    := DWORD(TVN_ITEMEXPANDED)
			   		SendMessage(hOwner, WM_NOTIFY, 0, LONGINT(_CAST, @sNMTV))
			   	ENDIF
			   ENDIF
		   ENDIF
		ENDIF
	ELSE
		lOK := TreeView_Expand(hWnd, hItem, dwMode)
	ENDIF

   IF lOK
      IF lAll
	      hChild := TreeView_GetChild(hWnd, hItem)
	      WHILE hChild != NULL_PTR
			  	SELF:__Expand(hChild, dwMode, lAll, lForceNotify)
			  	hChild := TreeView_GetNextSibling(hWnd, hChild)
		   ENDDO
		   RETURN TRUE
	   ENDIF
	   RETURN TRUE
   ENDIF

   RETURN FALSE

METHOD __GetHandleFromSymbol(symItem AS USUAL) AS PTR STRICT 
	//PP-030828 Strong typing
	//SE-060525
	LOCAL symToLookUp AS SYMBOL
	LOCAL dwI, dwCount AS DWORD

   IF IsPtr(symItem)
   	RETURN symItem
	ELSEIF IsSymbol(symItem)
		symToLookUp := symItem
	ELSEIF IsInstanceOf(symItem, #TreeViewItem)
		symToLookUp := symItem:NameSym
	ELSE
		WCError{#__GetHandleFromSymbol,#TreeView,__WCSTypeError,symItem,1}:@@Throw()
	ENDIF

	//dwCount := AScan(aTreeItems, {|x| x[1] == symToLookUp})
	dwCount := ALen(aTreeItems)
	FOR dwI := 1 UPTO dwCount
		IF (aTreeItems[dwI][1] == symToLookUp)
			RETURN aTreeItems[dwI][2]
		ENDIF
	NEXT  // dwI

	RETURN NULL_PTR

METHOD __GetSymbolFromHandle(hLookUp AS PTR) AS SYMBOL STRICT 
	//PP-030828 Strong typing
	//SE-060525
	LOCAL dwI, dwCount AS DWORD

	//dwCount := AScan(aTreeItems, {|x| x[2] == hLookUp})
	dwCount := ALen(aTreeItems)
	FOR dwI := 1 UPTO dwCount
		IF (aTreeItems[dwI][2] == hLookUp)
			RETURN aTreeItems[dwI][1]
		ENDIF
	NEXT  // dwI

	RETURN NULL_SYMBOL

METHOD __GetValueFromSymbol(symItem AS USUAL) AS USUAL STRICT 
	//PP-030828 Strong typing
	//SE-060525
	LOCAL symToLookUp  AS SYMBOL
	LOCAL dwI, dwCount AS DWORD

	IF IsSymbol(symItem)
		symToLookUp := symItem
	ELSEIF IsInstanceOf(symItem, #TreeViewItem)
		symToLookUp := symItem:NameSym
	ELSE
		WCError{#__GetHandleFromSymbol,#TreeView,__WCSTypeError,symItem,1}:@@Throw()
	ENDIF

	//dwCount := AScan(aValues, {|x| x[1] == symToLookUp})
	dwCount := ALen(aValues)
	FOR dwI := 1 UPTO dwCount
		IF (aValues[dwI][1] == symToLookUp)
			RETURN aValues[dwI][2]
		ENDIF
	NEXT  // dwI

	RETURN NIL

METHOD __RemoveByHandle(hLookUp AS PTR ) AS LOGIC STRICT 
	//PP-030828 Strong typing
	//SE-060525
	LOCAL dwI     AS DWORD
	LOCAL dwCount AS DWORD
	LOCAL symItem AS SYMBOL

	dwCount := ALen(aTreeItems)
	FOR dwI := 1 UPTO dwCount
		IF aTreeItems[dwI][2] == hLookUp
			EXIT
		ENDIF
	NEXT  // dwI

   IF dwI <= dwCount
   	symItem := aTreeItems[dwI][1]
		ADel(aTreeItems, dwI)
		ASize(aTreeItems, dwCount - 1)

		dwCount := ALen(aValues)
      FOR dwI := 1 UPTO dwCount
			IF aValues[dwI][1] == symItem
				ADel(aValues, dwI)
			   ASize(aValues, dwCount - 1)
				EXIT
			ENDIF
		NEXT  // dwI

	   RETURN TRUE
   ENDIF

   RETURN FALSE
	/*
	dwCount := AScan(LOCAL dwCount AS DWORD, {|x| x[2] == hLookUp})
	IF dwCount != 0
		symItem := aTreeItems[dwCount][1]
		ADel(aTreeItems, dwCount)
		ASize(aTreeItems, ALen(aTreeItems) - 1)

		dwCount := AScan(aValues, {|x| x[1] == symItem})
		IF dwCount != 0
			ADel(aValues, dwCount)
			ASize(aValues, ALen(aValues) - 1)
		ENDIF
		RETURN TRUE
	ENDIF

	RETURN FALSE
   */

METHOD __RemoveBySymbol(symLookUp AS SYMBOL) AS LOGIC STRICT 
	//PP-030828 Strong typing
	//SE-060525
	LOCAL dwI     AS DWORD
	LOCAL dwCount AS DWORD

	dwCount := ALen(aTreeItems)
	FOR dwI := 1 UPTO dwCount
		IF aTreeItems[dwI][1] == symLookUp
			EXIT
		ENDIF
	NEXT  // dwI
   IF dwI <= dwCount
   	ADel(aTreeItems, dwI)
		ASize(aTreeItems, dwCount - 1)

		dwCount := ALen(aValues)
      FOR dwI := 1 UPTO dwCount
			IF aValues[dwI][1] == symLookUp
				ADel(aValues, dwI)
			   ASize(aValues, dwCount - 1)
				EXIT
			ENDIF
		NEXT  // dwI

	   RETURN TRUE
   ENDIF

   RETURN FALSE
/*
	LOCAL dwCount AS DWORD

	dwCount := AScan(aTreeItems, {|x| x[1] == symLookUp})
	IF dwCount != 0
		ADel(aTreeItems, dwCount)
		ASize(aTreeItems, ALen(aTreeItems) - 1)

		dwCount := AScan(aValues, {|x| x[1] == symLookUp})
		IF dwCount != 0
			ADel(aValues, dwCount)
			ASize(aValues, ALen(aValues) - 1)
		ENDIF
		RETURN TRUE
	ENDIF

	RETURN FALSE
*/

ACCESS __SortRoutineName AS SYMBOL STRICT 
	//PP-030828 Strong typing
	

	RETURN symSortRoutineName

METHOD __UpdateValue(symItem AS USUAL, uNewValue AS USUAL) AS USUAL STRICT 
	//PP-030828 Strong typing
	//SE-060525
	LOCAL symToLookUp AS SYMBOL
	LOCAL dwI, dwCount AS DWORD

	IF IsSymbol(symItem)
		symToLookUp := symItem
	ELSEIF IsInstanceOf(symItem, #TreeViewItem)
		symToLookUp := symItem:NameSym
	ELSE
		WCError{#__GetHandleFromSymbol,#TreeView,__WCSTypeError,symItem,1}:@@Throw()
	ENDIF

   dwCount := ALen(aValues)
	FOR dwI := 1 UPTO dwCount
		IF (aValues[dwI][1] == symToLookUp)
			RETURN (aValues[dwI][2] := uNewValue)
		ENDIF
	NEXT  // dwI

	RETURN NIL

METHOD AddItem(symParentName, oTreeViewItem) 

	

	// insert this item as the last item in the parent item's list
	RETURN SELF:InsertItem(symParentName, #Last, oTreeViewItem)

METHOD Collapse(symName, lRemoveChildItems, lAll, lForceNotify) 
	//SE-060523
	LOCAL hThisItem AS PTR
	LOCAL dwFlag AS DWORD

	

	Default(@lRemoveChildItems, FALSE)

	IF lRemoveChildItems
		dwFlag := _Or(TVE_COLLAPSERESET, TVE_COLLAPSE)
		lAll   := lForceNotify := FALSE
	ELSE
		dwFlag := TVE_COLLAPSE
	ENDIF

	hThisItem := SELF:__GetHandleFromSymbol(symName)

	IF (hThisItem == NULL_PTR)
		RETURN FALSE
	ENDIF

   RETURN SELF:__Expand(hThisItem, dwFlag, IsLogic(lAll) .and. lAll, IsLogic(lForceNotify) .and. lForceNotify)

METHOD DeleteAll() 
	

	IF TreeView_DeleteAllItems(SELF:Handle())
		aValues := {}
		RETURN TRUE
	ENDIF
	RETURN FALSE

METHOD DeleteItem(symName, lChildsOnly) 
	LOCAL hThisItem AS PTR
	LOCAL hChild    AS PTR

	

	hThisItem := SELF:__GetHandleFromSymbol(symName)
	// deleting of array elements is done by __RemoveByHandle
	//(called when DeleteItem notification is handled)
	IF (hThisItem == NULL_PTR)
		RETURN FALSE
	ENDIF

   IF IsLogic(lChildsOnly) .and. lChildsOnly
      hChild := TreeView_GetChild(SELF:Handle(), hThisItem)
	   DO WHILE hChild != NULL_PTR
			TreeView_DeleteItem(hWnd, hChild)
			hChild := TreeView_GetChild(hWnd, hThisItem)
		ENDDO
	   RETURN TRUE
   ENDIF

	RETURN TreeView_DeleteItem(SELF:Handle(), hThisItem)

METHOD Destroy() 
	

	IF !InCollect()
		oImageList := NULL_OBJECT
		oStateImageList := NULL_OBJECT
		oDragImageList := NULL_OBJECT
		aValues := NULL_ARRAY
		aTreeItems := NULL_ARRAY
	ENDIF

	SUPER:Destroy()

	RETURN NIL
/*
TEXTBLOCK METHOD dispatch(oE) CLASS TreeView
//PP-040410 This is better handled in control dispatch
	LOCAL x AS USUAL
	//PP-030319 added method for context menu support
	x := SUPER:dispatch(oE)
	IF oE:message == WM_KEYDOWN .and. oE:wParam == VK_APPS
		IF ! SELF:contextmenu == NULL_OBJECT
			SELF:contextmenu:showaspopup(SELF)
		ENDIF
	ENDIF
	RETURN x


ENDTEXT
*/
ACCESS DragDropEnabled 
	

	RETURN lDragDropEnabled

ACCESS DragImageList 
	

	RETURN oDragImageList

ASSIGN DragImageList(oNewDragImageList) 
	

	RETURN oDragImageList := oNewDragImageList

METHOD EditItemLabel(symName) 
	LOCAL hThisItem AS PTR

	

	hThisItem := SELF:__GetHandleFromSymbol(symName)

	IF (hThisItem != TVI_ROOT) .and. (hThisItem != TVI_FIRST) .and. (hThisItem != TVI_LAST) .and. (hThisItem != NULL_PTR)
		TreeView_EditLabel(SELF:Handle(), hThisItem)
	ENDIF

	RETURN NIL

METHOD EnableDragDrop(lEnable) 
	

	Default(@lEnable, TRUE)

	lDragDropEnabled := lEnable

	RETURN TRUE

METHOD EnableSort(symMethodName) 
	

	RETURN symSortRoutineName := symMethodName

METHOD EnsureVisible(symName) 
	LOCAL hThisItem AS PTR

	

	hThisItem := SELF:__GetHandleFromSymbol(symName)

	IF (hThisItem == NULL_PTR)
		RETURN FALSE
	ENDIF
	RETURN TreeView_EnsureVisible(SELF:Handle(), hThisItem)

METHOD Expand(symName, lAll, lForceNotify) 
	//SE-060523
	LOCAL hThisItem AS PTR

	

	hThisItem := SELF:__GetHandleFromSymbol(symName)

	IF (hThisItem == NULL_PTR)
		RETURN FALSE
	ENDIF

   RETURN SELF:__Expand(hThisItem, TVE_EXPAND, IsLogic(lAll) .and. lAll, IsLogic(lForceNotify) .and. lForceNotify)

METHOD GetDropHighlight() 
	//SE-060523
	

	RETURN SELF:GetItemAttributes(TreeView_GetDropHilight(SELF:Handle()))

METHOD GetFirstChildItem(symItem) 
	//SE-060524
	LOCAL hStartItem AS PTR

	

	IF ! IsNil(symItem)
		hStartItem := SELF:__GetHandleFromSymbol(symItem)
	ENDIF

	RETURN SELF:GetItemAttributes(TreeView_GetChild(SELF:Handle(), hStartItem))

METHOD GetFirstVisibleItem() 
//SE-060524
	

	RETURN SELF:GetItemAttributes(TreeView_GetNextItem(SELF:Handle(), NULL_PTR, TVGN_FIRSTVISIBLE))

METHOD GetItemAtPosition(oPoint) 
	LOCAL hItem AS PTR
	LOCAL strucHitTestInfo IS _winTV_HitTestInfo

	

	oPoint := __WCConvertPoint(SELF, oPoint)
	strucHitTestInfo:pt:x := oPoint:X
	strucHitTestInfo:pt:y := oPoint:Y
	hItem := TreeView_HitTest(SELF:Handle(), @strucHitTestInfo)

	IF hItem != NULL_PTR
		RETURN SELF:GetItemAttributes(hItem)
	ENDIF

	RETURN NULL_OBJECT

METHOD GetItemAttributes(symItem) 
	LOCAL strucItem IS _winTV_Item
	LOCAL oTreeViewItem AS TreeViewItem
	LOCAL pszItemText AS PSZ
	LOCAL dwMask AS DWORD
	LOCAL DIM aBuf[257] AS BYTE
	LOCAL symTemp AS SYMBOL

	

	// set up the structure to receive item attributes
	strucItem:hItem := SELF:__GetHandleFromSymbol(symItem)
	strucItem:mask := _Or(TVIF_TEXT, TVIF_CHILDREN, TVIF_IMAGE, TVIF_SELECTEDIMAGE, TVIF_PARAM, TVIF_STATE)
	pszItemText := @aBuf[1]
	strucItem:pszText := pszItemText
	strucItem:cchTextMax := 256

	// create the TreeViewItem object from the structure
	IF TreeView_GetItem(SELF:Handle(),  @strucItem)
		oTreeViewItem := TreeViewItem{}
      symTemp := SELF:__GetSymbolFromHandle(strucItem:hItem) 
		oTreeViewItem:NameSym := symTemp
		oTreeViewItem:TextValue := Psz2String(strucItem:pszText)
		oTreeViewItem:Value := SELF:__GetValueFromSymbol(symTemp)
		oTreeViewItem:Bold := _And(strucItem:state, TVIS_BOLD) != 0
		oTreeViewItem:Disabled := _And(strucItem:state, TVIS_CUT) != 0
		oTreeViewItem:DropTarget := _And(strucItem:state, TVIS_DROPHILITED) != 0
		oTreeViewItem:Expanded := _And(strucItem:state, TVIS_EXPANDED) != 0
		oTreeViewItem:Focused := _And(strucItem:state, TVIS_FOCUSED) != 0
		oTreeViewItem:Selected := _And(strucItem:state, TVIS_SELECTED) != 0
		oTreeViewItem:ImageIndex := strucItem:iImage + 1
		oTreeViewItem:SelectedImageIndex := strucItem:iSelectedImage + 1
		oTreeViewItem:__TreeViewControl := SELF

		// get extended image information by isolating the state bits and
		// shifting by the appropriate number of bytes to get the image index
		dwMask := _And(strucItem:state, TVIS_STATEIMAGEMASK)
		IF dwMask != 0
			oTreeViewItem:StateImageIndex := dwMask >> 12
		ENDIF

		dwMask := _And(strucItem:state, TVIS_OVERLAYMASK)
		IF dwMask != 0
			oTreeViewItem:OverlayImageIndex := dwMask >> 8
		ENDIF
	ENDIF

	RETURN oTreeViewItem

METHOD GetItemBoundingBox(symItem, lTextOnly) 
	LOCAL hItem AS PTR
	LOCAL strucRect IS _winRect
	LOCAL oOrigin AS Point
	LOCAL oSize AS Dimension

	

	Default(@lTextOnly, FALSE)

	hItem := SELF:__GetHandleFromSymbol(symItem)
	strucRect:left := LONGINT(_CAST, hItem)
	IF LOGIC(_CAST, SendMessage(SELF:Handle(), TVM_GETITEMRECT, DWORD(_CAST, lTextOnly), LONGINT(_CAST, @strucRect)))
		//PP-030910
		oOrigin := Point{strucRect:left, strucRect:bottom}
		oOrigin := __WCConvertPoint(SELF, oOrigin)
		oSize := Dimension{strucRect:right - strucRect:left, strucRect:bottom - strucRect:top}

		RETURN BoundingBox{oOrigin, oSize}
	ENDIF

	RETURN BoundingBox{}

METHOD GetNextSiblingItem(symItem) 
	//SE-060524
	LOCAL hItem AS PTR

	

	hItem := SELF:__GetHandleFromSymbol(symItem)

	RETURN SELF:GetItemAttributes(TreeView_GetNextSibling(SELF:Handle(), hItem))

METHOD GetNextVisibleItem(symItem) 
	//SE-060524
	LOCAL hItem AS PTR

	

	hItem := SELF:__GetHandleFromSymbol(symItem)

	RETURN SELF:GetItemAttributes(TreeView_GetNextVisible(SELF:Handle(), hItem))

METHOD GetParentItem(symItem) 
	//SE-060524
	LOCAL hItem AS PTR

	

	hItem := SELF:__GetHandleFromSymbol(symItem)

	RETURN SELF:GetItemAttributes(TreeView_GetParent(SELF:Handle(), hItem))

METHOD GetPreviousSiblingItem(symItem) 
	//SE-060524
	LOCAL hItem AS PTR

	

	hItem := SELF:__GetHandleFromSymbol(symItem)

	RETURN SELF:GetItemAttributes(TreeView_GetPrevSibling(SELF:Handle(), hItem))

METHOD GetPreviousVisibleItem(symItem) 
	//SE-060524
	LOCAL hItem AS PTR

	

	hItem := SELF:__GetHandleFromSymbol(symItem)

	RETURN SELF:GetItemAttributes(TreeView_GetPrevVisible(SELF:Handle(), hItem))

METHOD GetRootItem() 
   //SE-060524
	

	RETURN SELF:GetItemAttributes(TreeView_GetRoot(SELF:Handle()))

METHOD GetSelectedItem() 
	//SE-060524
	

	RETURN SELF:GetItemAttributes(TreeView_GetSelection(SELF:Handle()))

ACCESS ImageList 
	

	RETURN oImageList

ASSIGN ImageList(oNewImageList) 
   //SE-060524
	

	oImageList := oNewImageList
	TreeView_SetImageList(SELF:Handle(), oImageList:Handle(), TVSIL_NORMAL)

	RETURN 

ACCESS Indent 
	

	RETURN TreeView_GetIndent(SELF:Handle())

ASSIGN Indent(dwIndent) 
	

	RETURN TreeView_SetIndent(SELF:Handle(), dwIndent)

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
	LOCAL dwStyle AS DWORD

	

	IF IsNil(kStyle)
		dwStyle := WS_BORDER
	ELSE
		dwStyle := _Or(DWORD(kStyle), DWORD(_CAST, WS_BORDER))
	ENDIF

	IF IsInstanceOfUsual(xID, #ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, , dwStyle, TRUE)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, "SysTreeView32", dwStyle, TRUE)
		SELF:SetStyle(_Or(TVS_HASLINES, TVS_HASBUTTONS, TVS_LINESATROOT, TVS_EDITLABELS))
	ENDIF

	aValues := {}
	aTreeItems := {}
	AAdd(aTreeItems, {#Root, TVI_ROOT})
	AAdd(aTreeItems, {#First, TVI_FIRST})
	AAdd(aTreeItems, {#Last, TVI_LAST})
	AAdd(aTreeItems, {#Sort, TVI_SORT})

	RETURN 

METHOD InsertItem(symParentName, symInsertAfter, oTreeViewItem) 
	//SE-060525
	LOCAL hParentItem AS PTR
	LOCAL hInsertAfter AS PTR
	LOCAL hThisItem AS PTR
	LOCAL strucInsertItem IS _winTV_INSERTSTRUCT
	LOCAL strucItem IS _winTV_ITEM
	LOCAL pszItemText AS PSZ
	LOCAL dwMask AS DWORD
	LOCAL dwStateMask AS DWORD
	LOCAL dwState AS DWORD
	LOCAL cText						AS STRING
	LOCAL uValue					AS USUAL

	

	// translate symbols into item handles
	hParentItem := SELF:__GetHandleFromSymbol(symParentName)
	hInsertAfter := SELF:__GetHandleFromSymbol(symInsertAfter)

	// set up the initial item with text, image, and selected image
	IF hParentItem != NULL_PTR
		cText := oTreeViewItem:TextValue

		IF cText == NULL_STRING
			// if there is no text available, use the associated usual value
			uValue := oTreeViewItem:Value
			IF !IsNil(uValue)
				cText := AllTrim(AsString(uValue))
			ENDIF
		ENDIF

		IF NULL_STRING != cText
			pszItemText := StringAlloc(cText)
			strucInsertItem:u:item:pszText := pszItemText
		ENDIF

		// set item visual states individually
		IF oTreeViewItem:Bold
			dwMask := TVIF_STATE
			dwStateMask := _Or(dwStateMask, TVIS_BOLD)
			dwState := _Or(dwState, TVIS_BOLD)
		ENDIF

		IF oTreeViewItem:Disabled
			dwMask := TVIF_STATE
			dwStateMask := _Or(dwStateMask, TVIS_CUT)
			dwState := _Or(dwState, TVIS_CUT)
		ENDIF

		IF oTreeViewItem:DropTarget
			dwMask := TVIF_STATE
			dwStateMask := _Or(dwStateMask, TVIS_DROPHILITED)
			dwState := _Or(dwState, TVIS_DROPHILITED)
		ENDIF

		IF oTreeViewItem:Expanded
			dwMask := TVIF_STATE
			dwStateMask := _Or(dwStateMask, TVIS_EXPANDED)
			dwState := _Or(dwState, TVIS_EXPANDED)
		ENDIF

		IF oTreeViewItem:Focused
			dwMask := TVIF_STATE
			dwStateMask := _Or(dwStateMask, TVIS_FOCUSED)
			dwState := _Or(dwState, TVIS_FOCUSED)
		ENDIF

		IF oTreeViewItem:Selected
			dwMask := TVIF_STATE
			dwStateMask := _Or(dwStateMask, TVIS_SELECTED)
			dwState := _Or(dwState, TVIS_SELECTED)
		ENDIF

		// set up image information
		IF oTreeViewItem:ImageIndex != 0
			strucInsertItem:u:item:iImage := oTreeViewItem:ImageIndex - 1
		ENDIF

		IF oTreeViewItem:SelectedImageIndex != 0
			strucInsertItem:u:item:iSelectedImage := oTreeViewItem:SelectedImageIndex - 1
		ELSE
			strucInsertItem:u:item:iSelectedImage := strucInsertItem:u:item:iImage
		ENDIF

		dwItemCount := dwItemCount + 1
		IF Empty(oTreeViewItem:NameSym)
			oTreeViewItem:NameSym := String2Symbol(NTrim(dwItemCount))
		ENDIF

		// set up the insert structure
		strucInsertItem:u:item:mask := _Or(TVIF_TEXT, TVIF_IMAGE, TVIF_SELECTEDIMAGE, dwMask) //, TVIF_PARAM
		//strucInsertItem.u.item.lparam := LONG(_CAST, oTreeViewItem:NameSym)
		strucInsertItem:u:item:statemask := dwStateMask
		strucInsertItem:u:item:state := dwState
		strucInsertItem:hParent := hParentItem
		strucInsertItem:hInsertAfter := hInsertAfter

		// insert the item into the tree view and add the new item handle to the list
		hThisItem := TreeView_InsertItem(SELF:Handle(),  @strucInsertItem)

		IF (PTR(_CAST, pszItemText) != NULL_PTR)
			MemFree(pszItemText)
		ENDIF

		IF (hThisItem != NULL_PTR)
			//SE-060525 needed for TreeView:SortChildren()
			strucItem:mask   := TVIF_PARAM
			strucItem:hItem  := hThisItem
			strucItem:lparam := LONGINT(_CAST, hThisItem)
			TreeView_SetItem(hWnd,  @strucItem)

			oTreeViewItem:__TreeViewControl := SELF

			AAdd(aTreeItems, {oTreeViewItem:NameSym, hThisItem})

			// add the item's usual value to the control's value list
			AAdd(aValues, {oTreeViewItem:NameSym, oTreeViewItem:Value})

			// set additional image information individually
			IF oTreeViewItem:StateImageIndex != 0
				strucItem:mask := _Or(TVIF_HANDLE, TVIF_STATE)
				strucItem:hItem := hThisItem
				strucItem:statemask := TVIS_STATEIMAGEMASK
				strucItem:state := INDEXTOSTATEIMAGEMASK(oTreeViewItem:StateImageIndex)
				TreeView_SetItem(hWnd,  @strucItem)
			ENDIF

			IF oTreeViewItem:OverlayImageIndex != 0
				strucItem:mask := _Or(TVIF_HANDLE, TVIF_STATE)
				strucItem:hItem := hThisItem
				strucItem:statemask := TVIS_OVERLAYMASK
				strucItem:state := INDEXTOOVERLAYMASK(oTreeViewItem:OverlayImageIndex)
				TreeView_SetItem(hWnd,  @strucItem)
			ENDIF

			RETURN TRUE
		ENDIF
	ENDIF

	RETURN FALSE

ACCESS ItemCount 
	

	RETURN TreeView_GetCount(SELF:Handle())

ACCESS SearchString 
	LOCAL pszSearchString AS PSZ
	LOCAL cSearchString AS STRING
	LOCAL DIM aBuf[257] AS BYTE

	

	pszSearchString:= @aBuf[1]
	TreeView_GetISearchString(SELF:Handle(), pszSearchString)
	cSearchString := Psz2String(pszSearchString)

	RETURN cSearchString

METHOD SelectItem(symItem, symCode, lSelect) 
	LOCAL hThisItem AS PTR

	

	IF (SELF:ItemCount == 0)
		RETURN FALSE
	ENDIF

	Default(@lSelect, TRUE)
	IF (lSelect)
		hThisItem := SELF:__GetHandleFromSymbol(symItem)
	ELSE
		hThisItem := NULL_PTR
	ENDIF

	IF !IsNil(symCode)
		IF symCode == #FirstVisible
			RETURN LOGIC(_CAST, TreeView_Select(SELF:Handle(), hThisItem, TVGN_FIRSTVISIBLE))
		ELSEIF symCode == #DropHighlight
			RETURN LOGIC(_CAST, TreeView_SelectDropTarget(SELF:Handle(), hThisItem))
		ENDIF
	ELSE
		RETURN LOGIC(_CAST, TreeView_SelectItem(SELF:Handle(), hThisItem))
	ENDIF

	RETURN FALSE

METHOD SetItemAttributes(oTreeViewItem) 
	LOCAL strucItem IS _winTV_ITEM
	LOCAL pszItemText AS PSZ
	LOCAL dwMask := 0 AS DWORD

	

	IF (NULL_STRING != oTreeViewItem:TextValue)
		dwMask := _or(dwMask, TVIF_TEXT)
		pszItemText := StringAlloc(oTreeViewItem:TextValue)
		strucItem:pszText := pszItemText
	ENDIF

	IF (oTreeViewItem:ImageIndex != 0)
		dwMask := _Or(dwMask, TVIF_IMAGE)
		strucItem:iImage := oTreeViewItem:ImageIndex - 1
	ENDIF

	dwMask := _Or(dwMask, TVIF_SELECTEDIMAGE)
	IF (oTreeViewItem:SelectedImageIndex != 0)
		strucItem:iSelectedImage := oTreeViewItem:SelectedImageIndex - 1
	ELSE
		strucItem:iSelectedImage := oTreeViewItem:ImageIndex - 1
	ENDIF

	dwMask := _Or(dwMask, TVIF_STATE)
	strucItem:statemask := _Or(strucItem:statemask, TVIS_BOLD, TVIS_FOCUSED, TVIS_SELECTED,;
		TVIS_DROPHILITED, TVIS_CUT)
	IF oTreeViewItem:Bold
		strucItem:state := _Or(strucItem:state, TVIS_BOLD)
	ENDIF

	IF oTreeViewItem:Focused
		strucItem:state := _Or(strucItem:state, TVIS_FOCUSED)
	ENDIF

	IF oTreeViewItem:Selected
		strucItem:state := _Or(strucItem:state, TVIS_SELECTED)
	ENDIF

	IF oTreeViewItem:DropTarget
		strucItem:state := _Or(strucItem:state, TVIS_DROPHILITED)
	ENDIF

	IF oTreeViewItem:Disabled
		strucItem:state := _Or(strucItem:state, TVIS_CUT)
	ENDIF
	/*
	 if oTreeViewItem:Expanded
 	strucItem.state := _Or(strucItem.state, TVIS_EXPANDED)
	 endif
	*/
	strucItem:mask := _Or(dwMask, TVIF_HANDLE)
	strucItem:hItem := SELF:__GetHandleFromSymbol(oTreeViewItem:NameSym)
	TreeView_SetItem(SELF:Handle(),  @strucItem)

	//if oTreeViewItem:StateImageIndex != 0
	dwMask := _Or(TVIF_STATE, TVIF_HANDLE)
	strucItem:statemask := TVIS_STATEIMAGEMASK
	strucItem:state := INDEXTOSTATEIMAGEMASK(oTreeViewItem:StateImageIndex)
	TreeView_SetItem(SELF:Handle(),  @strucItem)
	//endif

	//if oTreeViewItem:OverlayImageIndex != 0
	dwMask := _Or(TVIF_STATE, TVIF_HANDLE)
	strucItem:statemask := TVIS_OVERLAYMASK
	strucItem:state := INDEXTOOVERLAYMASK(oTreeViewItem:OverlayImageIndex)
	TreeView_SetItem(SELF:Handle(),  @strucItem)
	//endif

	IF (oTreeViewItem:Value != NIL)
		SELF:__UpdateValue(oTreeViewItem:NameSym, oTreeViewItem:Value)
	ENDIF

	IF (PTR(_CAST, pszItemText) != NULL_PTR)
		MemFree(pszItemText)
	ENDIF

	RETURN NIL

METHOD SortChildren(symParentName) 
	LOCAL hTreeView AS PTR
	LOCAL hParent AS PTR
	LOCAL strucSort IS _winTV_SORTCB

	// send the sort message, passing the ListView namesym as sort data
	hTreeView := SELF:Handle()
	hParent := SELF:__GetHandleFromSymbol(symParentName)
	strucSort:hParent := hParent
	strucSort:lpfnCompare := LONGINT(_CAST, Get_TreeView_ComparePtr())
	strucSort:lParam := LONGINT(_CAST, hTreeView)

	RETURN TreeView_SortChildrenCB(hTreeView, @strucSort, 0)

ACCESS StateImageList 
	RETURN oStateImageList

ASSIGN StateImageList(oNewImageList) 
	

	oStateImageList := oNewImageList
	TreeView_SetImageList(SELF:Handle(), oNewImageList:Handle(), TVSIL_STATE)

	RETURN 

METHOD Toggle(symName, lAll, lForceNotify) 
	//SE-060524
	LOCAL hThisItem AS PTR

	IF (hThisItem := SELF:__GetHandleFromSymbol(symName)) == Null_Ptr
	   RETURN FALSE
	ENDIF

   RETURN SELF:__Expand(hThisItem, TVE_TOGGLE, IsLogic(lAll) .and. lAll, IsLogic(lForceNotify) .and. lForceNotify)

ACCESS VisibleCount 
	

	RETURN TreeView_GetVisibleCount(SELF:Handle())
END CLASS

CLASS TreeViewItem INHERIT VObject
	PROTECT symName AS SYMBOL
	PROTECT cText AS STRING
	PROTECT uValue AS USUAL
	PROTECT dwState AS DWORD
	PROTECT dwStateValue AS DWORD
	PROTECT nImage AS INT
	PROTECT nSelectedImage AS INT
	PROTECT nStateImage AS INT
	PROTECT nOverlayImage AS INT
	PROTECT lBold AS LOGIC
	PROTECT lDisabled AS LOGIC
	PROTECT lDropTarget AS LOGIC
	PROTECT lExpanded AS LOGIC
	PROTECT lFocused AS LOGIC
	PROTECT lSelected AS LOGIC
	PROTECT oTVControl AS TreeView

	//PP-030828 Strong typing
	ASSIGN __TreeViewControl(oNewTVCtl AS TreeView)  STRICT 
	//PP-030828 Strong typing
	

	oTVControl := oNewTVCtl

	RETURN 

ACCESS Bold 
	

	RETURN lBold

ASSIGN Bold(lEnabled) 
	

	RETURN lBold := lEnabled

METHOD Collapse(lAll, lForceNotify) 
	

	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:Collapse(symName,NIL , lAll, lForceNotify)
	ENDIF
	RETURN FALSE

METHOD @@Delete(lChildsOnly) 
	
	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:DeleteItem(symName, lChildsOnly)
	ENDIF
	RETURN FALSE

ACCESS Disabled 
	
	RETURN lDisabled

ASSIGN Disabled(lEnabled) 
	
	RETURN lDisabled := lEnabled

ACCESS DropTarget 
	
	RETURN lDropTarget

ASSIGN DropTarget(lEnabled) 
	

	RETURN lDropTarget := lEnabled

METHOD Expand(lAll, lForceNotify) 
	

	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:Expand(symName, lAll, lForceNotify)
	ENDIF
	RETURN FALSE

ACCESS Expanded 
	

	RETURN lExpanded

ASSIGN Expanded(lEnabled) 
	

	RETURN lExpanded := lEnabled

ACCESS FirstChild 
	

	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:GetFirstChildItem(symName)
	ENDIF
	RETURN NULL_OBJECT

ACCESS Focused 
	

	RETURN lFocused

ASSIGN Focused(lEnabled) 
	

	RETURN lFocused := lEnabled

ACCESS ImageIndex 
	

	RETURN nImage

ASSIGN ImageIndex(nNewImage) 
	

	RETURN nImage := nNewImage

CONSTRUCTOR(sName, cTextVal, uVal, iImage, iSelImage) 
	

	IF IsSymbol(sName)
		symName := sName
	ELSEIF IsString(sName)
		symName := String2Symbol(sName)
	ENDIF

	IF IsString(cTextVal)
		cText := cTextVal
	ENDIF

	IF !IsNil(uVal)
		uValue := uVal
	ENDIF

	IF !IsNil(iImage)
		nImage := iImage
	ENDIF

	IF !IsNil(iSelImage)
		nSelectedImage := iSelImage
	ENDIF

	RETURN 

ACCESS NameSym 
	

	RETURN symName

ASSIGN NameSym(symNewName) 
	

	RETURN symName := symNewName

ACCESS NextSibling 
	

	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:GetNextSiblingItem(symName)
	ENDIF
	RETURN NULL_OBJECT

ACCESS OverlayImageIndex 
	

	RETURN nOverlayImage

ASSIGN OverlayImageIndex(nNewOverlayImage) 
	

	RETURN nOverlayImage := nNewOverlayImage

ACCESS Parent 
	

	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:GetParentItem(symName)
	ENDIF
	RETURN NULL_OBJECT

ACCESS PreviousSibling 
	

	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:GetPreviousSiblingItem(symName)
	ENDIF
	RETURN NULL_OBJECT

ACCESS Selected 
	

	RETURN lSelected

ASSIGN Selected(lEnabled) 
	

	RETURN lSelected := lEnabled

ACCESS SelectedImageIndex 
	

	RETURN nSelectedImage

ASSIGN SelectedImageIndex(nNewSelectedImage) 
	

	RETURN nSelectedImage := nNewSelectedImage

ACCESS StateImageIndex 
	

	RETURN nStateImage

ASSIGN StateImageIndex(nNewStateImage) 
	

	RETURN nStateImage := nNewStateImage


ACCESS TextValue 
	

	RETURN cText

ASSIGN TextValue(cNewText) 
	

	RETURN cText := cNewText

ACCESS TreeViewControl 
	

	RETURN oTVControl

ACCESS Value 
	

	RETURN uValue

ASSIGN Value(uNewValue) 
	

	RETURN uValue := uNewValue
END CLASS


#ifdef __VULCAN__
   INTERNAL DELEGATE TreeView_CompareDelegate( lParam1 AS LONGINT, lParam2 AS LONGINT, lHandle AS LONGINT) AS INT 
   
   STATIC FUNCTION Get_TreeView_ComparePtr() AS PTR
      STATIC LOCAL delTreeView_CompareDelegate AS TreeView_CompareDelegate
      IF delTreeView_CompareDelegate == NULL
         delTreeView_CompareDelegate := TreeView_CompareDelegate{ NULL, @TreeView_Compare() }
      ENDIF
      RETURN System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) delTreeView_CompareDelegate )
         
#else
   STATIC FUNCTION Get_TreeView_ComparePtr() AS PTR
      RETURN @TreeView_Compare()
#endif


STATIC FUNCTION TreeView_Compare(lParam1 AS LONGINT, lParam2 AS LONGINT, lHandle AS LONGINT) AS INT /* CALLBACK */
	LOCAL symSortRoutineName AS SYMBOL
	LOCAL oTreeView AS TreeView
	LOCAL oTreeViewItem1 AS TreeViewItem
	LOCAL oTreeViewItem2 AS TreeViewItem


	// retrieve the TreeView control handle and TreeView object
	IF (oTreeView := (TreeView) __WCGetControlByHandle(PTR(_CAST, lHandle))) != NULL_OBJECT
		// retrieve the TreeViewItems that correspond to lParam1 and lParam2
		oTreeViewItem1 := oTreeView:GetItemAttributes(PTR(_CAST, lParam1))
		oTreeViewItem2 := oTreeView:GetItemAttributes(PTR(_CAST, lParam2))

		// get the return value from the sort routine
		symSortRoutineName := oTreeView:__SortRoutineName
		IF symSortRoutineName != NULL_SYMBOL
			RETURN Send(oTreeView, symSortRoutineName, oTreeViewItem1, oTreeViewItem2)
		ENDIF
	ENDIF

	RETURN 0

