

#USING System.Collections.Generic
#USING System.Diagnostics
CLASS TreeView INHERIT TextControl
	PROTECT oDragImageList AS ImageList
	PROTECT lDragDropEnabled AS LOGIC
	PROTECT symSortRoutineName AS SYMBOL
	PROTECT aItems AS Dictionary<SYMBOL, TreeViewItem> 
	PROTECT cSearchString AS STRING
	PROTECT oItemComparer AS TreeViewItemComparer

    PROPERTY ControlType AS ControlType GET ControlType.TreeView


	ACCESS __TreeView AS VOTreeView
		RETURN (VOTreeView) SELF:oCtrl
	
	METHOD __NodeToTreeviewItem(oNode AS VOTreeNode) AS TreeViewItem
		IF oNode != NULL_OBJECT
			RETURN oNode:Item
		ENDIF
		RETURN NULL_OBJECT
	
	METHOD __CreateDragImageList(symItem AS USUAL) AS OBJECT STRICT 
		// Todo Implement __CreateDragImageList
		//LOCAL hThisItem AS PTR
		//LOCAL hImageList AS PTR

		//hThisItem := SELF:__GetNode(symItem)

		//IF (hThisItem != NULL_PTR)
		//	hImageList := TreeView_CreateDragImage(SELF:Handle(), hThisItem)
		//	IF (hImageList != NULL_PTR)
		//		RETURN ImageList{hImageList}
		//	ENDIF
		//ENDIF

		RETURN NULL_OBJECT

	METHOD __Expand(oNode AS VOTreeNode, dwMode AS DWORD, lAll AS LOGIC, lForceNotify AS LOGIC) AS LOGIC 
		// Todo Check  __Expand
		IF dwMode == TVE_TOGGLE
			IF oNode:IsExpanded
				dwMode := TVE_COLLAPSE
			ELSE
				dwMode := TVE_EXPAND
			ENDIF
		ENDIF
		IF dwMode == TVE_EXPAND
			IF lAll
				oNode:ExpandAll()
			ELSE
				oNode:Expand()
			ENDIF
		ELSEIF dwMode == TVE_COLLAPSE
			IF lAll
				oNode:Collapse(FALSE)
			ELSE
				oNode:Collapse(TRUE)
			ENDIF
		ENDIF
		RETURN FALSE

	METHOD __GetNode(symItem AS USUAL) AS VOTreeNode STRICT 
		LOCAL symToLookUp AS SYMBOL

		IF IsSymbol(symItem)
			symToLookUp := symItem
		ELSEIF IsInstanceOf(symItem, #TreeViewItem)
			symToLookUp := symItem:NameSym
		ELSE
			WCError{#__GetNode,#TreeView,__WCSTypeError,symItem,1}:@@Throw()
		ENDIF
		
		IF aItems:ContainsKey(symToLookUp)
			
			RETURN aItems[symToLookup]:__Node
		ENDIF
		RETURN NULL_OBJECT

	METHOD __GetSymbol(hLookUp AS VOTreeNode) AS SYMBOL STRICT 
		LOCAL oItem AS TreeViewItem
		IF hLookUp != NULL_OBJECT
			oItem := hLookUp:Item
			RETURN oItem:NameSym
		ENDIF
		RETURN NULL_SYMBOL

	METHOD __GetValue(oItem AS TreeViewItem) AS USUAL STRICT 
		IF oItem != NULL_OBJECT
			RETURN SELF:__GetValue(oItem:NameSym)
		ENDIF
		RETURN NIL

	METHOD __GetValue(symItem AS SYMBOL) AS USUAL STRICT 
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN oItem:Item:Value
		ENDIF
		RETURN NIL

	METHOD __Remove(oItem AS VOTreeNode ) AS LOGIC STRICT 
		IF SELF:__IsValid .and. oItem != NULL_OBJECT .and. __TreeView:Nodes:Contains(oItem)
			__TreeView:Nodes:Remove(oItem)
			RETURN TRUE
		ENDIF	
		RETURN FALSE
	
	METHOD __Remove(symLookUp AS SYMBOL) AS LOGIC STRICT 
		LOCAL IMPLIED oItem := SELF:__GetNode(symLookUp)
		RETURN SELF:__Remove(oItem)

	ACCESS __SortRoutineName AS SYMBOL STRICT 
		RETURN symSortRoutineName


	METHOD __UpdateValue(oItem AS TreeViewItem, uNewValue AS USUAL) AS LOGIC STRICT 
		IF oItem != NULL_OBJECT
			RETURN SELF:__UpdateValue(oItem:NameSym, uNewValue)
		ENDIF
		RETURN FALSE

	METHOD __UpdateValue(symItem AS Symbol, uNewValue AS USUAL) AS USUAL STRICT 
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT 
			LOCAL oTvItem AS TreeViewItem
			oTvItem := oItem:Item
			oTvItem:Value := uNewValue
		ENDIF
		RETURN FALSE


	METHOD AddItem(symParentName AS SYMBOL, oTreeViewItem AS TreeViewItem) 
		// insert this item as the last item in the parent item's list
		RETURN SELF:InsertItem(symParentName, #Last, oTreeViewItem)


	METHOD Collapse(oItem AS TreeViewItem, lRemoveChildItems AS LOGIC, lAll AS LOGIC, lForceNotify AS LOGIC) AS LOGIC
		LOCAL oNode as VOTreeNode
		oNode := oItem:__Node
		IF oNode != NULL_OBJECT
			IF lRemoveChildItems
				oNode:Nodes:Clear()
			ENDIF
			RETURN SELF:__Expand(oNode, TVE_COLLAPSE, lAll, lForceNotify)
		ENDIF
		RETURN FALSE

	METHOD Collapse(oItem AS TreeViewItem, lRemoveChildItems  AS LOGIC, lAll AS LOGIC) AS LOGIC
		RETURN SELF:Collapse(oItem, lRemoveChildItems, lAll, FALSE)

	METHOD Collapse(oItem AS TreeViewItem, lRemoveChildItems  AS LOGIC) AS LOGIC
		RETURN SELF:Collapse(oItem, lRemoveChildItems, FALSE, FALSE)

	METHOD Collapse(oItem AS TreeViewItem) AS LOGIC
		RETURN SELF:Collapse(oItem, FALSE, FALSE, FALSE)

	METHOD Collapse(symName AS SYMBOL, lRemoveChildItems  AS LOGIC, lAll AS LOGIC, lForceNotify AS LOGIC) 
		LOCAL IMPLIED oItem := SELF:GetItemAttributes(symName)
		RETURN SELF:Collapse(oItem, lRemoveChildItems, lAll, lForceNotify)

	METHOD Collapse(symName AS SYMBOL, lRemoveChildItems AS LOGIC, lAll AS LOGIC) 
		RETURN SELF:Collapse(symName, lRemoveChildItems, lAll, FALSE)

	METHOD Collapse(symName AS SYMBOL, lRemoveChildItems AS LOGIC) 
		RETURN SELF:Collapse(symName, lRemoveChildItems, FALSE, FALSE)

	METHOD Collapse(symName AS SYMBOL) 
		RETURN SELF:Collapse(symName, FALSE, FALSE, FALSE)

	METHOD DeleteAll() 
		IF SELF:__IsValid 
			SELF:__TreeView:Nodes:Clear()	
		ENDIF
		aItems:Clear()
		RETURN TRUE


	METHOD DeleteItemCore(oItem AS VOTreeNode, lChildsOnly AS LOGIC) AS LOGIC
		LOCAL oWin AS Window
		oWin := SELF:Owner
		// deleting of array elements is done by __Remove
		//(called when DeleteItem notification is handled)
		IF (oItem == NULL_OBJECT)
			RETURN FALSE
		ENDIF

		IF  lChildsOnly
			FOREACH oChildNode AS VOTreeNode IN oItem:Nodes
				oWin:TreeViewItemDelete(TreeViewDeleteEvent{SELF, oItem:Item})
				aItems:Remove( oChildNode:Item:NameSym)
			NEXT
			oItem:Nodes:Clear()
			RETURN TRUE
		ENDIF
		IF oItem:Parent != NULL_OBJECT
			IF oItem:Parent:Nodes:Contains(oItem)
				oWin:TreeViewItemDelete(TreeViewDeleteEvent{SELF, oItem:Item})
				aItems:Remove(oItem:Item:NameSym)
				oItem:Parent:Nodes:Remove(oItem)
				RETURN TRUE
			ENDIF
		ELSE
			IF SELF:__IsValid 
				IF __TreeView:Nodes:Contains(oItem)
					oWin:TreeViewItemDelete(TreeViewDeleteEvent{SELF, oItem:Item})			
					aItems:Remove(oItem:Item:NameSym)
					__TreeView:Nodes:Remove(oItem)
					RETURN TRUE
				ENDIF
			ENDIF			
		ENDIF
		RETURN FALSE



	METHOD DeleteItem(oItem AS USUAL) AS LOGIC
		IF IsSymbol(oItem)
			RETURN SELF:DeleteItem((SYMBOL)oItem, FALSE)			
		ELSEIF IsInstanceOf(oItem, #TreeViewItem)
			RETURN SELF:DeleteItem((TreeViewItem)oItem, FALSE)
		ENDIF
		RETURN FALSE

	METHOD DeleteItem(oItem AS USUAL, lChildsOnly AS LOGIC) AS LOGIC
		IF IsSymbol(oItem)
			RETURN SELF:DeleteItem((SYMBOL)oItem, lChildsOnly)			
		ELSEIF IsInstanceOf(oItem, #TreeViewItem)
			RETURN SELF:DeleteItem((TreeViewItem)oItem, lChildsOnly)
		ENDIF
		RETURN FALSE



	METHOD DeleteItem(oItem AS TreeViewItem) AS LOGIC
		RETURN SELF:DeleteItem(oItem, FALSE)

	METHOD DeleteItem(oItem AS TreeViewItem, lChildsOnly AS LOGIC) AS LOGIC
		IF oItem != NULL_OBJECT
			RETURN SELF:DeleteItemCore(oItem:__Node, lChildsOnly)
		ENDIF
		RETURN FALSE

	METHOD DeleteItem(symName AS SYMBOL) AS LOGIC
		RETURN SELF:DeleteItem(symName, FALSE)


	METHOD DeleteItem(symName AS SYMBOL, lChildsOnly AS LOGIC) AS LOGIC
		LOCAL IMPLIED oItem := SELF:__GetNode(symName)
		IF (oItem != NULL_OBJECT)
			RETURN SELF:DeleteItemCore(oItem, lChildsOnly)
		ENDIF
		RETURN FALSE

	METHOD Destroy() AS USUAL CLIPPER
		oDragImageList := NULL_OBJECT
		RETURN SUPER:Destroy()

	ACCESS DragDropEnabled AS LOGIC
		RETURN lDragDropEnabled

	ACCESS DragImageList AS ImageList
		RETURN oDragImageList

	ASSIGN DragImageList(oNewDragImageList AS ImageList) 
		oDragImageList := oNewDragImageList

	METHOD EditItemLabel(symItem AS SYMBOL) AS LOGIC
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			oItem:BeginEdit()
			RETURN TRUE
		ENDIF
		RETURN FALSE

	METHOD EnableDragDrop(lEnable := TRUE AS LOGIC) 
		lDragDropEnabled := lEnable
		RETURN TRUE

	METHOD EnableSort(symMethodName AS SYMBOL) AS VOID
		symSortRoutineName := symMethodName
		RETURN

	METHOD EnsureVisible(symItem AS SYMBOL) AS VOID
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		oItem:EnsureVisible()
		RETURN

	METHOD Expand(oItem AS TreeViewItem, lAll AS LOGIC, lForceNotify AS LOGIC) 
		IF oItem != NULL_OBJECT
			RETURN SELF:__Expand(oItem:__Node, TVE_EXPAND, lAll, lForceNotify)
		ENDIF
		RETURN FALSE
	METHOD Expand(symName AS SYMBOL, lAll AS LOGIC, lForceNotify AS LOGIC) 
		LOCAL IMPLIED oItem := SELF:GetItemAttributes(symName)
		RETURN SELF:Expand(oItem, lAll, lForceNotify)

	METHOD Expand(symName AS SYMBOL, lAll AS LOGIC) 
		RETURN SELF:Expand(symName, lAll, FALSE)

	METHOD Expand(symName AS SYMBOL) 
		RETURN SELF:Expand(symName, FALSE, FALSE)

	
	METHOD GetDropHighlight() AS TreeViewItem
		// Todo GetDropHighlight
		//RETURN SELF:GetItemAttributes(TreeView_GetDropHilight(SELF:Handle()))
		RETURN NULL_OBJECT


	METHOD GetFirstChildItem(oItem AS USUAL) AS TreeViewItem 
		IF IsSymbol(oItem)
			RETURN GetFirstChildItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN GetFirstChildItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT

	
	METHOD GetFirstChildItem(oItem AS TreeViewItem) AS TreeViewItem 
		IF oItem != NULL_OBJECT
			RETURN SELF:GetFirstChildItem(oItem:NameSym)
		ENDIF	
		RETURN NULL_OBJECT

	METHOD GetFirstChildItem(symItem AS SYMBOL) AS TreeViewItem
		LOCAL IMPLIED oItem := (VOTreeNode) SELF:__GetNode(symItem)
		IF oItem:Nodes:Count > 0
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode) oItem:Nodes:Item[0])
		ENDIF
		RETURN NULL_OBJECT		

	METHOD GetFirstVisibleItem() AS TreeViewItem
		//Todo GetFirstVisibleItem
		//LOCAL IMPLIED oItem := __TreeView:FirstNode
		//IF oItem != NULL_OBJECT
		RETURN NULL_OBJECT

	METHOD GetItemAtPosition(oPoint AS Point) 
		// Fenster-bezogene Koordinate in Control-bezogene Koordinate zurückrechnen
		LOCAL IMPLIED oNode := (VOTreeNode) __TreeView:GetNodeAt(oPoint)
		RETURN SELF:__NodeToTreeviewItem(oNode)

	METHOD GetCurrentItem() 
		LOCAL IMPLIED oNode := (VOTreeNode) __TreeView:SelectedNode 
		RETURN SELF:__NodeToTreeviewItem(oNode)


	METHOD GetItemAttributes(oItem AS TreeViewItem) AS TreeViewItem 
		IF oItem != NULL_OBJECT
			RETURN SELF:GetItemAttributes(oItem:NameSym)
		ENDIF	
		RETURN NULL_OBJECT


	METHOD GetItemAttributes(symItem AS SYMBOL) AS TreeViewItem
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		RETURN SELF:__NodeToTreeviewItem(oItem)
	
	METHOD GetItemBoundingBox(symItem AS SYMBOL, lTextOnly := FALSE AS LOGIC) AS BoundingBox 
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN (BoundingBox) oItem:Bounds
		ENDIF
		RETURN BoundingBox{}


	METHOD GetNextSiblingItem(oItem AS USUAL) AS TreeViewItem 
		IF IsSymbol(oItem)
			RETURN GetNextSiblingItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN GetNextSiblingItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT

	METHOD GetNextSiblingItem(oItem AS TreeViewItem) AS TreeViewItem 
		IF oItem != NULL_OBJECT
			RETURN SELF:GetNextSiblingItem(oItem:NameSym)
		ENDIF	
		RETURN NULL_OBJECT
	
	METHOD GetNextSiblingItem(symItem AS SYMBOL) AS TreeViewItem 
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:NextNode)
		ENDIF
		RETURN NULL_OBJECT

	METHOD GetNextVisibleItem(oItem AS USUAL) AS TreeViewItem 
		IF IsSymbol(oItem)
			RETURN GetNextVisibleItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN GetNextVisibleItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT


	METHOD GetNextVisibleItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetNextVisibleItem(oItem:NameSym)
		ENDIF	
		RETURN NULL_OBJECT


	METHOD GetNextVisibleItem(symItem AS SYMBOL) AS TreeViewItem
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:NextVisibleNode)
		ENDIF
		RETURN NULL_OBJECT

	METHOD GetParentItem(oItem AS USUAL) AS TreeViewItem 
		IF IsSymbol(oItem)
			RETURN GetParentItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN GetParentItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT

	METHOD GetParentItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetParentItem(oItem:NameSym)
		ENDIF	
		RETURN NULL_OBJECT

	METHOD GetParentItem(symItem AS SYMBOL) AS TreeViewItem
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:Parent)
		ENDIF
		RETURN NULL_OBJECT


	METHOD GetPreviousSiblingItem(oItem AS USUAL) AS TreeViewItem 
		IF IsSymbol(oItem)
			RETURN GetPreviousSiblingItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN GetPreviousSiblingItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT

	METHOD GetPreviousSiblingItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetPreviousSiblingItem(oItem:NameSym)
		ENDIF	
		RETURN NULL_OBJECT


	METHOD GetPreviousSiblingItem(symItem AS SYMBOL) AS TreeViewItem
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:PrevNode)
		ENDIF
		RETURN NULL_OBJECT



	METHOD GetPreviousVisibleItem(oItem AS USUAL) AS TreeViewItem 
		IF IsSymbol(oItem)
			RETURN GetPreviousVisibleItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN GetPreviousVisibleItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT


	METHOD GetPreviousVisibleItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetPreviousVisibleItem(oItem:NameSym)
		ENDIF	
		RETURN NULL_OBJECT


	METHOD GetPreviousVisibleItem(symItem AS SYMBOL) 
		LOCAL IMPLIED oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:PrevVisibleNode)
		ENDIF
		RETURN NULL_OBJECT

	METHOD GetRootItem() AS TreeViewItem
		LOCAL oItem AS VOTreeNode
		IF SELF:__IsValid .and. SELF:__TreeView:Nodes:Count > 0
			oItem := (VOTreeNode) SELF:__TreeView:Nodes[0]
		ENDIF
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem)
		ENDIF
		RETURN NULL_OBJECT

	METHOD GetSelectedItem() AS TreeViewItem
		IF SELF:__IsValid 
			LOCAL IMPLIED oItem := __TreeView:SelectedNode
			IF oItem != NULL_OBJECT
				RETURN SELF:__NodeToTreeviewItem((VOTreeNode) oItem)
			ENDIF
		ENDIF
		RETURN NULL_OBJECT

	ACCESS ImageList AS ImageList
		IF ValidateControl()
			RETURN ImageList{__TreeView:ImageList }
		ENDIF
		RETURN NULL_OBJECT

	ASSIGN ImageList(oNewImageList AS ImageList) 
		IF ValidateControl()
			__TreeView:ImageList := oNewImageList:__ImageList
		ENDIF
		RETURN 

	ACCESS Indent AS LONG
		IF SELF:__IsValid 
			RETURN __TreeView:Indent
		ENDIF
		RETURN 0

	ASSIGN Indent(dwIndent AS LONG) 
		IF SELF:__IsValid 
			__TreeView:Indent := dwIndent
		ENDIF
		
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 
		LOCAL dwStyle AS DWORD

		aItems := Dictionary<SYMBOL, TreeViewItem> {}

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
		oItemComparer := TreeViewItemComparer{SELF}

		RETURN 

	METHOD InsertItem(symParentName AS SYMBOL, symInsertAfter AS SYMBOL, oTreeViewItem AS TreeViewItem) AS LOGIC
		LOCAL oParent AS VOTreeNode
		LOCAL oAfter AS VOTreeNode
		LOCAL oNode AS VOTreeNode
		LOCAL nPosition AS LONG		
		LOCAL cText AS STRING
		LOCAL lLast AS LOGIC
		LOCAL oNodes AS System.Windows.Forms.TreeNodeCollection

		IF ! SELF:ValidateControl()
			RETURN FALSE
		ENDIF
		DO CASE
		CASE symParentName == #Root
			oParent := NULL_OBJECT
		CASE symParentName == #First
			IF __TreeView:Nodes:Count > 0
				oParent := (VOTreeNode) __TreeView:Nodes[1]
			ENDIF
		CASE symParentName == #Last
			IF __TreeView:Nodes:Count > 0
				oParent := (VOTreeNode) __TreeView:Nodes[__TreeView:Nodes:Count-1]
			ENDIF
		CASE symParentName == #Sort
			IF !__TreeView:Sorted
				__TreeView:Sorted := TRUE
			ENDIF
			oParent := (VOTreeNode) __TreeView:TopNode
		OTHERWISE
			oParent := SELF:__GetNode(symParentName)
			IF oParent == NULL_OBJECT
				RETURN FALSE
			ENDIF
		ENDCASE
		IF oParent != NULL
			oNodes := oParent:Nodes
		ELSE
			oNodes := __TreeView:Nodes
		ENDIF
		
		// translate symbols into item handles
		DO CASE
		CASE symInsertAfter == #Root
			oAfter := (VOTreeNode) __TreeView:TopNode
		CASE symInsertAfter == #First
			IF __TreeView:TopNode != NULL_OBJECT .and. __TreeView:TopNode:Nodes:Count > 0
				oAfter := (VOTreeNode) __TreeView:TopNode:Nodes[1]
			ENDIF
		CASE symInsertAfter == #Last
			lLast := TRUE

		CASE symInsertAfter == #Sort
			lLast := TRUE
			IF !__TreeView:Sorted
				__TreeView:Sorted := TRUE
			ENDIF
		OTHERWISE
			oAfter  := SELF:__GetNode(symInsertAfter)
		ENDCASE
		oNode   := oTreeViewItem:__Node
		
		cText := oTreeViewItem:TextValue

		IF cText == NULL_STRING
			// if there is no text available, use the associated usual value
			uValue := oTreeViewItem:Value
			IF !IsNil(uValue)
				cText := AllTrim(AsString(uValue))
			ENDIF
			oNode:Text := cText
		ENDIF
		

		// set item visual states individually
		//	IF oTreeViewItem:Bold
		//		dwMask := TVIF_STATE
		//		dwStateMask := _Or(dwStateMask, TVIS_BOLD)
		//		dwState := _Or(dwState, TVIS_BOLD)
		//	ENDIF

		//	IF oTreeViewItem:Disabled
		//		dwMask := TVIF_STATE
		//		dwStateMask := _Or(dwStateMask, TVIS_CUT)
		//		dwState := _Or(dwState, TVIS_CUT)
		//	ENDIF

		//	IF oTreeViewItem:DropTarget
		//		dwMask := TVIF_STATE
		//		dwStateMask := _Or(dwStateMask, TVIS_DROPHILITED)
		//		dwState := _Or(dwState, TVIS_DROPHILITED)
		//	ENDIF

		//	IF oTreeViewItem:Expanded
		//		dwMask := TVIF_STATE
		//		dwStateMask := _Or(dwStateMask, TVIS_EXPANDED)
		//		dwState := _Or(dwState, TVIS_EXPANDED)
		//	ENDIF

		//	IF oTreeViewItem:Focused
		//		dwMask := TVIF_STATE
		//		dwStateMask := _Or(dwStateMask, TVIS_FOCUSED)
		//		dwState := _Or(dwState, TVIS_FOCUSED)
		//	ENDIF

		//	IF oTreeViewItem:Selected
		//		dwMask := TVIF_STATE
		//		dwStateMask := _Or(dwStateMask, TVIS_SELECTED)
		//		dwState := _Or(dwState, TVIS_SELECTED)
		//	ENDIF


		IF Empty(oTreeViewItem:NameSym)
			oTreeViewItem:NameSym := String2Symbol("Node "+NTrim(SELF:ItemCount+1))
		ENDIF

		IF oAfter != NULL_OBJECT .and. ! lLast
			nPosition := oAfter:Index+1
			oNodes:Insert(nPosition, oNode)
		ELSE
			oNodes:Add(oNode)
		ENDIF
		
		oTreeViewItem:__TreeViewControl := SELF
		IF ! aItems:ContainsKey(oTreeViewItem:NameSym)
			aItems:Add(oTreeViewItem:NameSym, oTreeViewItem)
		ENDIF

		RETURN TRUE

	ACCESS ItemCount AS LONG
		IF SELF:__IsValid
			RETURN __TreeView:GetNodeCount(TRUE)
		ENDIF
		RETURN 0

	ACCESS SearchString AS STRING
		RETURN cSearchString

	METHOD SelectItem(symItem, symCode, lSelect) 
		LOCAL oItem AS VOTreeNode
		DEFAULT(@lSelect, TRUE)
		IF ! SELF:__IsValid .or. SELF:ItemCount == 0 
			RETURN FALSE
		ENDIF
		IF lSelect
			oItem := SELF:__GetNode(symItem)
		ELSE
			oItem := NULL_OBJECT
		ENDIF


		IF !IsNil(symCode)
			IF symCode == #FirstVisible
				SELF:__TreeView:TopNode := oItem
			ELSEIF symCode == #DropHighlight
				SELF:__TreeView:SelectedNode := oItem
			ENDIF
		ELSE
			SELF:__TreeView:SelectedNode := oItem
		ENDIF

		RETURN FALSE

	METHOD SetItemAttributes(oTreeViewItem AS TreeViewItem) AS VOID
		RETURN 

	

	METHOD SortChildren(symParentName AS SYMBOL) 
		IF SELF:__IsValid
			IF SELF:symSortRoutineName != NULL_SYMBOL
				SELF:__TreeView:TreeViewNodeSorter := SELF:oItemComparer
			ENDIF
			SELF:__TreeView:Sort()	
		ENDIF
		RETURN SELF
	

	ACCESS StateImageList AS ImageList
		RETURN ImageList{__TreeView:ImageList}

	ASSIGN StateImageList(oNewImageList AS ImageList) 
		IF SELF:__IsValid
		__TreeView:StateImageList := oNewImageList:__ImageList
		ENDIF
		RETURN 

	METHOD RestoreUpdate() AS VOID STRICT
		IF SELF:__IsValid
			SELF:__TreeView:EndUpdate()
			SUPER:RestoreUpdate()
		ENDIF


	METHOD SuspendUpdate() AS VOID STRICT
		IF SELF:__IsValid
			SELF:__TreeView:BeginUpdate()
			SUPER:SuspendUpdate()
		ENDIF

	METHOD Toggle(symName AS SYMBOL, lAll := FALSE AS LOGIC, lForceNotify := FALSE AS LOGIC) AS LOGIC 
		LOCAL IMPLIED oItem := SELF:__GetNode(symName)
		IF oItem == NULL_OBJECT
			RETURN FALSE
		ENDIF

		RETURN SELF:__Expand(oItem, TVE_TOGGLE, lAll, lForceNotify)

	ACCESS VisibleCount AS LONG
		RETURN __TreeView:VisibleCount

END CLASS

[DebuggerDisplay( "{symName}" )] ;
CLASS TreeViewItem INHERIT VObject
	PROTECT symName AS SYMBOL
	PROTECT uValue AS USUAL
	PROTECT nOverlayImage AS INT
	PROTECT lBold AS LOGIC
	PROTECT lDisabled AS LOGIC
	PROTECT lDropTarget AS LOGIC
	PROTECT lFocused AS LOGIC
	PROTECT oTVControl AS TreeView
	PROTECT oNode		AS VOTreeNode

	#region simple properties	
	PROPERTY __Node AS VOTreeNode GET oNode SET oNode := Value
	//Todo Set Bold when needed
	PROPERTY Bold  AS LOGIC GET lBold SET lBold := Value
	//Todo Set Disabled when needed
	PROPERTY Disabled			AS LOGIC	GET lDisabled					SET lDisabled := Value
	PROPERTY DropTarget			AS LOGIC	GET lDropTarget					SET lDropTarget := VALUE
	PROPERTY Expanded			AS LOGIC	GET oNode:IsExpanded			SET iif(Value, oNode:Expand(), oNode:Collapse())
	//Todo Set Focused when needed
	PROPERTY Focused			AS LOGIC	GET lFocused					SET lFocused := Value
	PROPERTY ImageIndex			AS LONG		GET oNode:ImageIndex+1			SET oNode:ImageIndex := Value-1
	PROPERTY Selected			AS LOGIC	GET oNode:IsSelected			SET oNode:TreeView:SelectedNode := iif(Value, (OBJECT) oNode, NULL_OBJECT)
	PROPERTY SelectedImageIndex AS LONG		GET oNode:SelectedImageIndex+1	SET oNode:SelectedImageIndex := value-1
	PROPERTY StateImageIndex	AS LONG		GET oNode:StateImageIndex		SET oNode:StateImageIndex := value
	PROPERTY TextValue			AS STRING	GET oNode:Text					SET oNode:Text := Value
	PROPERTY TreeViewControl	AS TreeView	GET oTVControl
	PROPERTY Value				AS USUAL    GET uValue						SET uValue := Value
	#endregion
	
	ASSIGN __TreeViewControl(oNewTVCtl AS TreeView)  STRICT 
		oTVControl := oNewTVCtl
		RETURN 


	METHOD Collapse(lAll AS LOGIC, lForceNotify AS LOGIC) 
		IF oNode != NULL_OBJECT
			IF lAll
				oNode:Collapse(FALSE)
			ELSE
				oNode:Collapse(TRUE)
			ENDIF
			RETURN TRUE
		ENDIF
		
		RETURN FALSE


	METHOD @@Delete(lChildsOnly := FALSE AS LOGIC) 
		
		IF (oTVControl != NULL_OBJECT)
			RETURN oTVControl:DeleteItem(SELF:symName, lChildsOnly)
		ENDIF
		RETURN FALSE

	
	METHOD Expand(lAll AS LOGIC, lForceNotify AS LOGIC) 
		IF oNode != NULL_OBJECT
			IF lAll
				oNode:Expand()
			ELSE
				oNode:ExpandAll()
			ENDIF
			RETURN TRUE
		ENDIF
		
		RETURN FALSE

	
	ACCESS FirstChild AS TreeViewItem
		IF oNode:FirstNode != NULL_OBJECT
			RETURN ((VOTreeNode) oNode:FirstNode):Item
		ENDIF		
		RETURN NULL_OBJECT


	CONSTRUCTOR(sName, cTextVal, uVal, iImage, iSelImage) 
		SUPER()
		oNode := VOTreeNode{SELF}

		IF IsSymbol(sName)
			symName := sName
		ELSEIF IsString(sName)
			symName := String2Symbol(sName)
		ENDIF

		IF IsString(cTextVal)
			oNode:Text := (STRING) cTextVal
		ENDIF

		IF !IsNil(uVal)
			uValue := uVal
		ENDIF

		IF !IsNil(iImage)
			oNode:ImageIndex := iImage -1
		ENDIF

		IF !IsNil(iSelImage)
			oNode:SelectedImageIndex := iSelImage-1
		ELSE
			oNode:SelectedImageIndex := oNode:ImageIndex
		ENDIF
		oNode:Name := (STRING) symName
		RETURN 

	PROPERTY NameSym AS SYMBOL GET symName SET symName := Value

	ACCESS NextSibling AS TreeViewItem
		LOCAL oSibling AS VOTreeNode
		oSibling := (VOTreeNode) oNode:NextNode
		IF oSibling != NULL_OBJECT
			RETURN oSibling:Item
		ENDIF
		RETURN NULL_OBJECT

	PROPERTY OverlayImageIndex AS LONG GET nOverlayImage SET nOverlayImage := Value

	ACCESS Parent as TreeViewItem
		LOCAL oSibling AS VOTreeNode
		oSibling := (VOTreeNode) oNode:Parent
		IF oSibling != NULL_OBJECT
			RETURN oSibling:Item
		ENDIF
		RETURN NULL_OBJECT

	ACCESS PreviousSibling As TreeViewItem
		LOCAL oSibling AS VOTreeNode
		oSibling := (VOTreeNode) oNode:PrevNode
		IF oSibling != NULL_OBJECT
			RETURN oSibling:Item
		ENDIF
		RETURN NULL_OBJECT



END CLASS

//Todo
//#ifdef __VULCAN__
//INTERNAL DELEGATE TreeView_CompareDelegate( lParam1 AS LONGINT, lParam2 AS LONGINT, lHandle AS LONGINT) AS INT 

//STATIC FUNCTION Get_TreeView_ComparePtr() AS PTR
//	STATIC LOCAL delTreeView_CompareDelegate AS TreeView_CompareDelegate
//	IF delTreeView_CompareDelegate == NULL
//		delTreeView_CompareDelegate := TreeView_CompareDelegate{ NULL, @TreeView_Compare() }
//	ENDIF
//	RETURN System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate( (System.Delegate) delTreeView_CompareDelegate )

//#else
//STATIC FUNCTION Get_TreeView_ComparePtr() AS PTR
//	RETURN @TreeView_Compare()
//#endif


//STATIC FUNCTION TreeView_Compare(lParam1 AS LONGINT, lParam2 AS LONGINT, lHandle AS LONGINT) AS INT /* CALLBACK */
//	LOCAL symSortRoutineName AS SYMBOL
//	LOCAL oTreeView AS TreeView
//	LOCAL oTreeViewItem1 AS TreeViewItem
//	LOCAL oTreeViewItem2 AS TreeViewItem


//	// retrieve the TreeView control handle and TreeView object
//	IF (oTreeView := (TreeView) __WCGetControlByHandle(PTR(_CAST, lHandle))) != NULL_OBJECT
//		// retrieve the TreeViewItems that correspond to lParam1 and lParam2
//		oTreeViewItem1 := oTreeView:GetItemAttributes(PTR(_CAST, lParam1))
//		oTreeViewItem2 := oTreeView:GetItemAttributes(PTR(_CAST, lParam2))

//		// get the return value from the sort routine
//		symSortRoutineName := oTreeView:__SortRoutineName
//		IF symSortRoutineName != NULL_SYMBOL
//			RETURN Send(oTreeView, symSortRoutineName, oTreeViewItem1, oTreeViewItem2)
//		ENDIF
//	ENDIF

//	RETURN 0



#using System.Collections
#using System.Reflection
CLASS TreeViewItemComparer IMPLEMENTS IComparer
	PROTECT oTreeView AS TreeView
	PROTECT oMethodInfo AS System.Reflection.MethodInfo
	PROTECT symMethod AS SYMBOL	  
	CONSTRUCTOR(oTv AS TreeView)
		oTreeView := oTv
	
	PUBLIC METHOD Compare(x AS OBJECT , y AS OBJECT ) AS INT
		IF oTreeView:__SortRoutineName != NULL_SYMBOL
			IF oTreeView:__SortRoutineName != symMethod 
				SELF:GetMethodInfo()
				IF oMethodInfo != NULL_OBJECT
					symMethod := oTreeView:__SortRoutineName
				ENDIF
			ENDIF
			IF oMethodInfo != NULL_OBJECT
				LOCAL oPars AS OBJECT[]
				oPars := OBJECT[]{2}
				LOCAL oItem1 AS VOTreeNode
				LOCAL oItem2 AS VOTreeNode
				oItem1 := (VOTreeNode) x
				oItem2 := (VOTreeNode) y
				oPars[1] := oItem1:Item
				oPars[2] := oItem2:Item
				RETURN Convert.ToInt32(oMethodInfo:Invoke(oTreeView, oPars))
			ENDIF
		ENDIF
		RETURN 0
	PROTECTED METHOD GetMethodInfo() AS VOID
		LOCAL oType AS System.Type
		LOCAL cMethod AS STRING
		oMethodInfo := NULL_OBJECT
		oType := oTreeView:GetType()
		cMethod := oTreeView:__SortRoutineName:ToString()
		oMethodInfo := oType:GetMethod(cMethod, BindingFlags.Instance+ BindingFlags.PUBLIC+BindingFlags.IgnoreCase)
		RETURN 
	
END CLASS
