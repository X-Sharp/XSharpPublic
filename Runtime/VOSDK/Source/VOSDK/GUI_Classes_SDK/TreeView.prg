/// <include file="Gui.xml" path="doc/TreeView/*" />
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


 /// <exclude />
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


 /// <exclude />
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
	      IF _AND(sTVItem:State, TVIS_EXPANDED) = TVIS_EXPANDED
	      	lDo := (dwMode = TVE_COLLAPSE)
	      ELSE
	      	lDo := (dwMode = TVE_EXPAND)
		   ENDIF
		ENDIF
	   lOK := TRUE
	   IF lDo
	   	IF _AND(sTVItem:State, TVIS_EXPANDEDONCE) = TVIS_EXPANDEDONCE
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
			   	IF hOwner != NULL_PTR
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


 /// <exclude />
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
		WCError{#__GetHandleFromSymbol,#TreeView,__WCSTypeError,symItem,1}:Throw()
	ENDIF


	//dwCount := AScan(aTreeItems, {|x| x[1] == symToLookUp})
	dwCount := ALen(aTreeItems)
	FOR dwI := 1 UPTO dwCount
		IF (aTreeItems[dwI][1] == symToLookUp)
			RETURN aTreeItems[dwI][2]
		ENDIF
	NEXT  // dwI


	RETURN NULL_PTR


 /// <exclude />
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


 /// <exclude />
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
		WCError{#__GetHandleFromSymbol,#TreeView,__WCSTypeError,symItem,1}:Throw()
	ENDIF


	//dwCount := AScan(aValues, {|x| x[1] == symToLookUp})
	dwCount := ALen(aValues)
	FOR dwI := 1 UPTO dwCount
		IF (aValues[dwI][1] == symToLookUp)
			RETURN aValues[dwI][2]
		ENDIF
	NEXT  // dwI


	RETURN NIL


 /// <exclude />
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


 /// <exclude />
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


 /// <exclude />
ACCESS __SortRoutineName AS SYMBOL STRICT
	//PP-030828 Strong typing




	RETURN symSortRoutineName


 /// <exclude />
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
		WCError{#__GetHandleFromSymbol,#TreeView,__WCSTypeError,symItem,1}:Throw()
	ENDIF


   dwCount := ALen(aValues)
	FOR dwI := 1 UPTO dwCount
		IF (aValues[dwI][1] == symToLookUp)
			RETURN (aValues[dwI][2] := uNewValue)
		ENDIF
	NEXT  // dwI


	RETURN NIL


/// <include file="Gui.xml" path="doc/TreeView.AddItem/*" />
METHOD AddItem(symParentName, oTreeViewItem)






	// insert this item as the last item in the parent item's list
	RETURN SELF:InsertItem(symParentName, #Last, oTreeViewItem)


/// <include file="Gui.xml" path="doc/TreeView.Collapse/*" />
METHOD Collapse(symName, lRemoveChildItems, lAll, lForceNotify)
	//SE-060523
	LOCAL hThisItem AS PTR
	LOCAL dwFlag AS DWORD






	DEFAULT(@lRemoveChildItems, FALSE)


	IF lRemoveChildItems
		dwFlag := _OR(TVE_COLLAPSERESET, TVE_COLLAPSE)
		lAll   := lForceNotify := FALSE
	ELSE
		dwFlag := TVE_COLLAPSE
	ENDIF


	hThisItem := SELF:__GetHandleFromSymbol(symName)


	IF (hThisItem == NULL_PTR)
		RETURN FALSE
	ENDIF


   RETURN SELF:__Expand(hThisItem, dwFlag, IsLogic(lAll) .AND. lAll, IsLogic(lForceNotify) .AND. lForceNotify)


/// <include file="Gui.xml" path="doc/TreeView.DeleteAll/*" />
METHOD DeleteAll()




	IF TreeView_DeleteAllItems(SELF:Handle())
		aValues := {}
		RETURN TRUE
	ENDIF
	RETURN FALSE


/// <include file="Gui.xml" path="doc/TreeView.DeleteItem/*" />
METHOD DeleteItem(symName, lChildsOnly)
	LOCAL hThisItem AS PTR
	LOCAL hChild    AS PTR






	hThisItem := SELF:__GetHandleFromSymbol(symName)
	// deleting of array elements is done by __RemoveByHandle
	//(called when DeleteItem notification is handled)
	IF (hThisItem == NULL_PTR)
		RETURN FALSE
	ENDIF


   IF IsLogic(lChildsOnly) .AND. lChildsOnly
      hChild := TreeView_GetChild(SELF:Handle(), hThisItem)
	   DO WHILE hChild != NULL_PTR
			TreeView_DeleteItem(hWnd, hChild)
			hChild := TreeView_GetChild(hWnd, hThisItem)
		ENDDO
	   RETURN TRUE
   ENDIF


	RETURN TreeView_DeleteItem(SELF:Handle(), hThisItem)


/// <include file="Gui.xml" path="doc/TreeView.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




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
/// <include file="Gui.xml" path="doc/TreeView.DragDropEnabled/*" />
ACCESS DragDropEnabled




	RETURN lDragDropEnabled


/// <include file="Gui.xml" path="doc/TreeView.DragImageList/*" />
ACCESS DragImageList




	RETURN oDragImageList


/// <include file="Gui.xml" path="doc/TreeView.DragImageList/*" />
ASSIGN DragImageList(oNewDragImageList)




	RETURN oDragImageList := oNewDragImageList


/// <include file="Gui.xml" path="doc/TreeView.EditItemLabel/*" />
METHOD EditItemLabel(symName)
	LOCAL hThisItem AS PTR






	hThisItem := SELF:__GetHandleFromSymbol(symName)


	IF (hThisItem != TVI_ROOT) .AND. (hThisItem != TVI_FIRST) .AND. (hThisItem != TVI_LAST) .AND. (hThisItem != NULL_PTR)
		TreeView_EditLabel(SELF:Handle(), hThisItem)
	ENDIF


	RETURN NIL


/// <include file="Gui.xml" path="doc/TreeView.EnableDragDrop/*" />
METHOD EnableDragDrop(lEnable)




	DEFAULT(@lEnable, TRUE)


	lDragDropEnabled := lEnable


	RETURN TRUE


/// <include file="Gui.xml" path="doc/TreeView.EnableSort/*" />
METHOD EnableSort(symMethodName)




	RETURN symSortRoutineName := symMethodName


/// <include file="Gui.xml" path="doc/TreeView.EnsureVisible/*" />
METHOD EnsureVisible(symName)
	LOCAL hThisItem AS PTR






	hThisItem := SELF:__GetHandleFromSymbol(symName)


	IF (hThisItem == NULL_PTR)
		RETURN FALSE
	ENDIF
	RETURN TreeView_EnsureVisible(SELF:Handle(), hThisItem)


/// <include file="Gui.xml" path="doc/TreeView.Expand/*" />
METHOD Expand(symName, lAll, lForceNotify)
	//SE-060523
	LOCAL hThisItem AS PTR






	hThisItem := SELF:__GetHandleFromSymbol(symName)


	IF (hThisItem == NULL_PTR)
		RETURN FALSE
	ENDIF


   RETURN SELF:__Expand(hThisItem, TVE_EXPAND, IsLogic(lAll) .AND. lAll, IsLogic(lForceNotify) .AND. lForceNotify)


/// <include file="Gui.xml" path="doc/TreeView.GetDropHighlight/*" />
METHOD GetDropHighlight()
	//SE-060523




	RETURN SELF:GetItemAttributes(TreeView_GetDropHilight(SELF:Handle()))


/// <include file="Gui.xml" path="doc/TreeView.GetFirstChildItem/*" />
METHOD GetFirstChildItem(symItem)
	//SE-060524
	LOCAL hStartItem AS PTR






	IF ! IsNil(symItem)
		hStartItem := SELF:__GetHandleFromSymbol(symItem)
	ENDIF


	RETURN SELF:GetItemAttributes(TreeView_GetChild(SELF:Handle(), hStartItem))


/// <include file="Gui.xml" path="doc/TreeView.GetFirstVisibleItem/*" />
METHOD GetFirstVisibleItem()
//SE-060524




	RETURN SELF:GetItemAttributes(TreeView_GetNextItem(SELF:Handle(), NULL_PTR, TVGN_FIRSTVISIBLE))


/// <include file="Gui.xml" path="doc/TreeView.GetItemAtPosition/*" />
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


/// <include file="Gui.xml" path="doc/TreeView.GetItemAttributes/*" />
METHOD GetItemAttributes(symItem)
	LOCAL strucItem IS _winTV_Item
	LOCAL oTreeViewItem AS TreeViewItem
	LOCAL pszItemText AS PSZ
	LOCAL dwMask AS DWORD
	LOCAL DIM aBuf[257] AS BYTE
	LOCAL symTemp AS SYMBOL




	// set up the structure to receive item attributes
    IF IsSymbol(symItem)
        strucItem:hItem := SELF:__GetHandleFromSymbol(symItem)
    ELSEIF IsPtr(symItem)
        strucItem:hItem := symItem
    ENDIF
	strucItem:mask := _OR(TVIF_TEXT, TVIF_CHILDREN, TVIF_IMAGE, TVIF_SELECTEDIMAGE, TVIF_PARAM, TVIF_STATE)
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
		oTreeViewItem:Bold := _AND(strucItem:state, TVIS_BOLD) != 0
		oTreeViewItem:Disabled := _AND(strucItem:state, TVIS_CUT) != 0
		oTreeViewItem:DropTarget := _AND(strucItem:state, TVIS_DROPHILITED) != 0
		oTreeViewItem:Expanded := _AND(strucItem:state, TVIS_EXPANDED) != 0
		oTreeViewItem:Focused := _AND(strucItem:state, TVIS_FOCUSED) != 0
		oTreeViewItem:Selected := _AND(strucItem:state, TVIS_SELECTED) != 0
		oTreeViewItem:ImageIndex := strucItem:iImage + 1
		oTreeViewItem:SelectedImageIndex := strucItem:iSelectedImage + 1
		oTreeViewItem:__TreeViewControl := SELF


		// get extended image information by isolating the state bits and
		// shifting by the appropriate number of bytes to get the image index
		dwMask := _AND(strucItem:state, TVIS_STATEIMAGEMASK)
		IF dwMask != 0
			oTreeViewItem:StateImageIndex := dwMask >> 12
		ENDIF


		dwMask := _AND(strucItem:state, TVIS_OVERLAYMASK)
		IF dwMask != 0
			oTreeViewItem:OverlayImageIndex := dwMask >> 8
		ENDIF
	ENDIF


	RETURN oTreeViewItem


/// <include file="Gui.xml" path="doc/TreeView.GetItemBoundingBox/*" />
METHOD GetItemBoundingBox(symItem, lTextOnly)
	LOCAL hItem AS PTR
	LOCAL strucRect IS _winRect
	LOCAL oOrigin AS Point
	LOCAL oSize AS Dimension






	DEFAULT(@lTextOnly, FALSE)


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


/// <include file="Gui.xml" path="doc/TreeView.GetNextSiblingItem/*" />
METHOD GetNextSiblingItem(symItem)
	//SE-060524
	LOCAL hItem AS PTR






	hItem := SELF:__GetHandleFromSymbol(symItem)


	RETURN SELF:GetItemAttributes(TreeView_GetNextSibling(SELF:Handle(), hItem))


/// <include file="Gui.xml" path="doc/TreeView.GetNextVisibleItem/*" />
METHOD GetNextVisibleItem(symItem)
	//SE-060524
	LOCAL hItem AS PTR






	hItem := SELF:__GetHandleFromSymbol(symItem)


	RETURN SELF:GetItemAttributes(TreeView_GetNextVisible(SELF:Handle(), hItem))


/// <include file="Gui.xml" path="doc/TreeView.GetParentItem/*" />
METHOD GetParentItem(symItem)
	//SE-060524
	LOCAL hItem AS PTR






	hItem := SELF:__GetHandleFromSymbol(symItem)


	RETURN SELF:GetItemAttributes(TreeView_GetParent(SELF:Handle(), hItem))


/// <include file="Gui.xml" path="doc/TreeView.GetPreviousSiblingItem/*" />
METHOD GetPreviousSiblingItem(symItem)
	//SE-060524
	LOCAL hItem AS PTR






	hItem := SELF:__GetHandleFromSymbol(symItem)


	RETURN SELF:GetItemAttributes(TreeView_GetPrevSibling(SELF:Handle(), hItem))


/// <include file="Gui.xml" path="doc/TreeView.GetPreviousVisibleItem/*" />
METHOD GetPreviousVisibleItem(symItem)
	//SE-060524
	LOCAL hItem AS PTR






	hItem := SELF:__GetHandleFromSymbol(symItem)


	RETURN SELF:GetItemAttributes(TreeView_GetPrevVisible(SELF:Handle(), hItem))


/// <include file="Gui.xml" path="doc/TreeView.GetRootItem/*" />
METHOD GetRootItem()
   //SE-060524




	RETURN SELF:GetItemAttributes(TreeView_GetRoot(SELF:Handle()))


/// <include file="Gui.xml" path="doc/TreeView.GetSelectedItem/*" />
METHOD GetSelectedItem()
	//SE-060524




	RETURN SELF:GetItemAttributes(TreeView_GetSelection(SELF:Handle()))


/// <include file="Gui.xml" path="doc/TreeView.ImageList/*" />
ACCESS ImageList




	RETURN oImageList


/// <include file="Gui.xml" path="doc/TreeView.ImageList/*" />
ASSIGN ImageList(oNewImageList)
   //SE-060524




	oImageList := oNewImageList
	TreeView_SetImageList(SELF:Handle(), oImageList:Handle(), TVSIL_NORMAL)


	RETURN


/// <include file="Gui.xml" path="doc/TreeView.Indent/*" />
ACCESS Indent




	RETURN TreeView_GetIndent(SELF:Handle())


/// <include file="Gui.xml" path="doc/TreeView.Indent/*" />
ASSIGN Indent(dwIndent)




	RETURN TreeView_SetIndent(SELF:Handle(), dwIndent)


/// <include file="Gui.xml" path="doc/TreeView.ctor/*" />
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
		SUPER(oOwner, xID, oPoint, oDimension, "SysTreeView32", dwStyle, TRUE)
		SELF:SetStyle(_OR(TVS_HASLINES, TVS_HASBUTTONS, TVS_LINESATROOT, TVS_EDITLABELS))
	ENDIF


	aValues := {}
	aTreeItems := {}
	AAdd(aTreeItems, {#Root, TVI_ROOT})
	AAdd(aTreeItems, {#First, TVI_FIRST})
	AAdd(aTreeItems, {#Last, TVI_LAST})
	AAdd(aTreeItems, {#Sort, TVI_SORT})


	RETURN


/// <include file="Gui.xml" path="doc/TreeView.InsertItem/*" />
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
			dwStateMask := _OR(dwStateMask, TVIS_BOLD)
			dwState := _OR(dwState, TVIS_BOLD)
		ENDIF


		IF oTreeViewItem:Disabled
			dwMask := TVIF_STATE
			dwStateMask := _OR(dwStateMask, TVIS_CUT)
			dwState := _OR(dwState, TVIS_CUT)
		ENDIF


		IF oTreeViewItem:DropTarget
			dwMask := TVIF_STATE
			dwStateMask := _OR(dwStateMask, TVIS_DROPHILITED)
			dwState := _OR(dwState, TVIS_DROPHILITED)
		ENDIF


		IF oTreeViewItem:Expanded
			dwMask := TVIF_STATE
			dwStateMask := _OR(dwStateMask, TVIS_EXPANDED)
			dwState := _OR(dwState, TVIS_EXPANDED)
		ENDIF


		IF oTreeViewItem:Focused
			dwMask := TVIF_STATE
			dwStateMask := _OR(dwStateMask, TVIS_FOCUSED)
			dwState := _OR(dwState, TVIS_FOCUSED)
		ENDIF


		IF oTreeViewItem:Selected
			dwMask := TVIF_STATE
			dwStateMask := _OR(dwStateMask, TVIS_SELECTED)
			dwState := _OR(dwState, TVIS_SELECTED)
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
		strucInsertItem:u:item:mask := (DWORD) _OR(TVIF_TEXT, TVIF_IMAGE, TVIF_SELECTEDIMAGE, (INT) dwMask) //, TVIF_PARAM
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
				strucItem:mask := _OR(TVIF_HANDLE, TVIF_STATE)
				strucItem:hItem := hThisItem
				strucItem:statemask := TVIS_STATEIMAGEMASK
				strucItem:state := INDEXTOSTATEIMAGEMASK(oTreeViewItem:StateImageIndex)
				TreeView_SetItem(hWnd,  @strucItem)
			ENDIF


			IF oTreeViewItem:OverlayImageIndex != 0
				strucItem:mask := _OR(TVIF_HANDLE, TVIF_STATE)
				strucItem:hItem := hThisItem
				strucItem:statemask := TVIS_OVERLAYMASK
				strucItem:state := INDEXTOOVERLAYMASK(oTreeViewItem:OverlayImageIndex)
				TreeView_SetItem(hWnd,  @strucItem)
			ENDIF


			RETURN TRUE
		ENDIF
	ENDIF


	RETURN FALSE


/// <include file="Gui.xml" path="doc/TreeView.ItemCount/*" />
ACCESS ItemCount




	RETURN TreeView_GetCount(SELF:Handle())


/// <include file="Gui.xml" path="doc/TreeView.SearchString/*" />
ACCESS SearchString
	LOCAL pszSearchString AS PSZ
	LOCAL cSearchString AS STRING
	LOCAL DIM aBuf[257] AS BYTE






	pszSearchString:= @aBuf[1]
	TreeView_GetISearchString(SELF:Handle(), pszSearchString)
	cSearchString := Psz2String(pszSearchString)


	RETURN cSearchString


/// <include file="Gui.xml" path="doc/TreeView.SelectItem/*" />
METHOD SelectItem(symItem, symCode, lSelect)
	LOCAL hThisItem AS PTR






	IF (SELF:ItemCount == 0)
		RETURN FALSE
	ENDIF


	DEFAULT(@lSelect, TRUE)
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


/// <include file="Gui.xml" path="doc/TreeView.SetItemAttributes/*" />
METHOD SetItemAttributes(oTreeViewItem)
	LOCAL strucItem IS _winTV_ITEM
	LOCAL pszItemText AS PSZ
	LOCAL dwMask := 0 AS DWORD






	IF (NULL_STRING != oTreeViewItem:TextValue)
		dwMask := _OR(dwMask, TVIF_TEXT)
		pszItemText := StringAlloc(oTreeViewItem:TextValue)
		strucItem:pszText := pszItemText
	ENDIF


	IF (oTreeViewItem:ImageIndex != 0)
		dwMask := _OR(dwMask, TVIF_IMAGE)
		strucItem:iImage := oTreeViewItem:ImageIndex - 1
	ENDIF


	dwMask := _OR(dwMask, TVIF_SELECTEDIMAGE)
	IF (oTreeViewItem:SelectedImageIndex != 0)
		strucItem:iSelectedImage := oTreeViewItem:SelectedImageIndex - 1
	ELSE
		strucItem:iSelectedImage := oTreeViewItem:ImageIndex - 1
	ENDIF


	dwMask := _OR(dwMask, TVIF_STATE)
	strucItem:statemask := _OR(strucItem:statemask, TVIS_BOLD, TVIS_FOCUSED, TVIS_SELECTED,;
		TVIS_DROPHILITED, TVIS_CUT)
	IF oTreeViewItem:Bold
		strucItem:state := _OR(strucItem:state, TVIS_BOLD)
	ENDIF


	IF oTreeViewItem:Focused
		strucItem:state := _OR(strucItem:state, TVIS_FOCUSED)
	ENDIF


	IF oTreeViewItem:Selected
		strucItem:state := _OR(strucItem:state, TVIS_SELECTED)
	ENDIF


	IF oTreeViewItem:DropTarget
		strucItem:state := _OR(strucItem:state, TVIS_DROPHILITED)
	ENDIF


	IF oTreeViewItem:Disabled
		strucItem:state := _OR(strucItem:state, TVIS_CUT)
	ENDIF
	/*
	 if oTreeViewItem:Expanded
 	strucItem.state := _Or(strucItem.state, TVIS_EXPANDED)
	 endif
	*/
	strucItem:mask := _OR(dwMask, TVIF_HANDLE)
	strucItem:hItem := SELF:__GetHandleFromSymbol(oTreeViewItem:NameSym)
	TreeView_SetItem(SELF:Handle(),  @strucItem)


	//if oTreeViewItem:StateImageIndex != 0
	dwMask := _OR(TVIF_STATE, TVIF_HANDLE)
	strucItem:statemask := TVIS_STATEIMAGEMASK
	strucItem:state := INDEXTOSTATEIMAGEMASK(oTreeViewItem:StateImageIndex)
	TreeView_SetItem(SELF:Handle(),  @strucItem)
	//endif


	//if oTreeViewItem:OverlayImageIndex != 0
	dwMask := _OR(TVIF_STATE, TVIF_HANDLE)
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


/// <include file="Gui.xml" path="doc/TreeView.SortChildren/*" />
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


/// <include file="Gui.xml" path="doc/TreeView.StateImageList/*" />
ACCESS StateImageList
	RETURN oStateImageList


/// <include file="Gui.xml" path="doc/TreeView.StateImageList/*" />
ASSIGN StateImageList(oNewImageList)




	oStateImageList := oNewImageList
	TreeView_SetImageList(SELF:Handle(), oNewImageList:Handle(), TVSIL_STATE)


	RETURN


/// <include file="Gui.xml" path="doc/TreeView.Toggle/*" />
METHOD Toggle(symName, lAll, lForceNotify)
	//SE-060524
	LOCAL hThisItem AS PTR


	IF (hThisItem := SELF:__GetHandleFromSymbol(symName)) == NULL_PTR
	   RETURN FALSE
	ENDIF


   RETURN SELF:__Expand(hThisItem, TVE_TOGGLE, IsLogic(lAll) .AND. lAll, IsLogic(lForceNotify) .AND. lForceNotify)


/// <include file="Gui.xml" path="doc/TreeView.VisibleCount/*" />
ACCESS VisibleCount




	RETURN TreeView_GetVisibleCount(SELF:Handle())
END CLASS


/// <include file="Gui.xml" path="doc/TreeViewItem/*" />
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
 /// <exclude />
	ASSIGN __TreeViewControl(oNewTVCtl AS TreeView)  STRICT
	//PP-030828 Strong typing




	oTVControl := oNewTVCtl


	RETURN


/// <include file="Gui.xml" path="doc/TreeViewItem.Bold/*" />
ACCESS Bold




	RETURN lBold


/// <include file="Gui.xml" path="doc/TreeViewItem.Bold/*" />
ASSIGN Bold(lEnabled)




	RETURN lBold := lEnabled


/// <include file="Gui.xml" path="doc/TreeViewItem.Collapse/*" />
METHOD Collapse(lAll, lForceNotify)




	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:Collapse(symName,NIL , lAll, lForceNotify)
	ENDIF
	RETURN FALSE


/// <include file="Gui.xml" path="doc/TreeViewItem.Delete/*" />
METHOD Delete(lChildsOnly)


	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:DeleteItem(symName, lChildsOnly)
	ENDIF
	RETURN FALSE


/// <include file="Gui.xml" path="doc/TreeViewItem.Disabled/*" />
ACCESS Disabled


	RETURN lDisabled


/// <include file="Gui.xml" path="doc/TreeViewItem.Disabled/*" />
ASSIGN Disabled(lEnabled)


	RETURN lDisabled := lEnabled


/// <include file="Gui.xml" path="doc/TreeViewItem.DropTarget/*" />
ACCESS DropTarget


	RETURN lDropTarget


/// <include file="Gui.xml" path="doc/TreeViewItem.DropTarget/*" />
ASSIGN DropTarget(lEnabled)




	RETURN lDropTarget := lEnabled


/// <include file="Gui.xml" path="doc/TreeViewItem.Expand/*" />
METHOD Expand(lAll, lForceNotify)




	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:Expand(symName, lAll, lForceNotify)
	ENDIF
	RETURN FALSE


/// <include file="Gui.xml" path="doc/TreeViewItem.Expanded/*" />
ACCESS Expanded




	RETURN lExpanded


/// <include file="Gui.xml" path="doc/TreeViewItem.Expanded/*" />
ASSIGN Expanded(lEnabled)




	RETURN lExpanded := lEnabled


/// <include file="Gui.xml" path="doc/TreeViewItem.FirstChild/*" />
ACCESS FirstChild




	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:GetFirstChildItem(symName)
	ENDIF
	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeViewItem.Focused/*" />
ACCESS Focused




	RETURN lFocused


/// <include file="Gui.xml" path="doc/TreeViewItem.Focused/*" />
ASSIGN Focused(lEnabled)




	RETURN lFocused := lEnabled


/// <include file="Gui.xml" path="doc/TreeViewItem.ImageIndex/*" />
ACCESS ImageIndex




	RETURN nImage


/// <include file="Gui.xml" path="doc/TreeViewItem.ImageIndex/*" />
ASSIGN ImageIndex(nNewImage)




	RETURN nImage := nNewImage


/// <include file="Gui.xml" path="doc/TreeViewItem.ctor/*" />
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


/// <include file="Gui.xml" path="doc/TreeViewItem.NameSym/*" />
ACCESS NameSym




	RETURN symName


/// <include file="Gui.xml" path="doc/TreeViewItem.NameSym/*" />
ASSIGN NameSym(symNewName)




	RETURN symName := symNewName


/// <include file="Gui.xml" path="doc/TreeViewItem.NextSibling/*" />
ACCESS NextSibling




	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:GetNextSiblingItem(symName)
	ENDIF
	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeViewItem.OverlayImageIndex/*" />
ACCESS OverlayImageIndex




	RETURN nOverlayImage


/// <include file="Gui.xml" path="doc/TreeViewItem.OverlayImageIndex/*" />
ASSIGN OverlayImageIndex(nNewOverlayImage)




	RETURN nOverlayImage := nNewOverlayImage


/// <include file="Gui.xml" path="doc/TreeViewItem.Parent/*" />
ACCESS Parent




	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:GetParentItem(symName)
	ENDIF
	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeViewItem.PreviousSibling/*" />
ACCESS PreviousSibling




	IF (oTVControl != NULL_OBJECT)
		RETURN oTVControl:GetPreviousSiblingItem(symName)
	ENDIF
	RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeViewItem.Selected/*" />
ACCESS Selected




	RETURN lSelected


/// <include file="Gui.xml" path="doc/TreeViewItem.Selected/*" />
ASSIGN Selected(lEnabled)




	RETURN lSelected := lEnabled


/// <include file="Gui.xml" path="doc/TreeViewItem.SelectedImageIndex/*" />
ACCESS SelectedImageIndex




	RETURN nSelectedImage


/// <include file="Gui.xml" path="doc/TreeViewItem.SelectedImageIndex/*" />
ASSIGN SelectedImageIndex(nNewSelectedImage)




	RETURN nSelectedImage := nNewSelectedImage


/// <include file="Gui.xml" path="doc/TreeViewItem.StateImageIndex/*" />
ACCESS StateImageIndex




	RETURN nStateImage


/// <include file="Gui.xml" path="doc/TreeViewItem.StateImageIndex/*" />
ASSIGN StateImageIndex(nNewStateImage)




	RETURN nStateImage := nNewStateImage




/// <include file="Gui.xml" path="doc/TreeViewItem.TextValue/*" />
ACCESS TextValue




	RETURN cText


/// <include file="Gui.xml" path="doc/TreeViewItem.TextValue/*" />
ASSIGN TextValue(cNewText)




	RETURN cText := cNewText


/// <include file="Gui.xml" path="doc/TreeViewItem.TreeViewControl/*" />
ACCESS TreeViewControl




	RETURN oTVControl


/// <include file="Gui.xml" path="doc/TreeViewItem.Value/*" />
ACCESS Value




	RETURN uValue


/// <include file="Gui.xml" path="doc/TreeViewItem.Value/*" />
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


