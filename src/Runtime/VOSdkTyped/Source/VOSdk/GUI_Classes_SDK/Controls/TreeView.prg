//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Diagnostics
/// <include file="Gui.xml" path="doc/TreeView/*" />
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

    method __GetNode(oItem as TreeViewItem) as VOTreeNode strict
        return self:__GetNode(oItem:NameSym)

	method __GetNode(symToLookUp as symbol) as VOTreeNode strict
		if aItems:ContainsKey(symToLookUp)
			return aItems[symToLookup]:__Node
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
		VAR oItem := SELF:__GetNode(symItem)
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
		VAR oItem := SELF:__GetNode(symLookUp)
		RETURN SELF:__Remove(oItem)

	ACCESS __SortRoutineName AS SYMBOL STRICT
		RETURN symSortRoutineName


	METHOD __UpdateValue(oItem AS TreeViewItem, uNewValue AS USUAL) AS LOGIC STRICT
		IF oItem != NULL_OBJECT
			RETURN SELF:__UpdateValue(oItem:NameSym, uNewValue)
		ENDIF
		RETURN FALSE

	METHOD __UpdateValue(symItem AS Symbol, uNewValue AS USUAL) AS USUAL STRICT
		VAR oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			LOCAL oTvItem AS TreeViewItem
			oTvItem := oItem:Item
			oTvItem:Value := uNewValue
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/TreeView.AddItem/*" />

	METHOD AddItem(symParentName AS SYMBOL, oTreeViewItem AS TreeViewItem)
		// insert this item as the last item in the parent item's list
		RETURN SELF:InsertItem(symParentName, #Last, oTreeViewItem)

/// <include file="Gui.xml" path="doc/TreeView.Collapse/*" />

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

/// <include file="Gui.xml" path="doc/TreeView.Collapse/*" />
	METHOD Collapse(oItem AS TreeViewItem, lRemoveChildItems  AS LOGIC, lAll AS LOGIC) AS LOGIC
		RETURN SELF:Collapse(oItem, lRemoveChildItems, lAll, FALSE)

/// <include file="Gui.xml" path="doc/TreeView.Collapse/*" />
	METHOD Collapse(oItem AS TreeViewItem, lRemoveChildItems  AS LOGIC) AS LOGIC
		RETURN SELF:Collapse(oItem, lRemoveChildItems, FALSE, FALSE)

/// <include file="Gui.xml" path="doc/TreeView.Collapse/*" />
	METHOD Collapse(oItem AS TreeViewItem) AS LOGIC
		RETURN SELF:Collapse(oItem, FALSE, FALSE, FALSE)

/// <include file="Gui.xml" path="doc/TreeView.Collapse/*" />
	METHOD Collapse(symName AS SYMBOL, lRemoveChildItems  AS LOGIC, lAll AS LOGIC, lForceNotify AS LOGIC)
		VAR oItem := SELF:GetItemAttributes(symName)
		RETURN SELF:Collapse(oItem, lRemoveChildItems, lAll, lForceNotify)

/// <include file="Gui.xml" path="doc/TreeView.Collapse/*" />
	METHOD Collapse(symName AS SYMBOL, lRemoveChildItems AS LOGIC, lAll AS LOGIC)
		RETURN SELF:Collapse(symName, lRemoveChildItems, lAll, FALSE)
/// <include file="Gui.xml" path="doc/TreeView.Collapse/*" />
	METHOD Collapse(symName AS SYMBOL, lRemoveChildItems AS LOGIC)
		RETURN SELF:Collapse(symName, lRemoveChildItems, FALSE, FALSE)

/// <include file="Gui.xml" path="doc/TreeView.Collapse/*" />
	METHOD Collapse(symName AS SYMBOL)
		RETURN SELF:Collapse(symName, FALSE, FALSE, FALSE)
/// <include file="Gui.xml" path="doc/TreeView.DeleteAll/*" />

	METHOD DeleteAll()
		IF SELF:__IsValid
			SELF:__TreeView:Nodes:Clear()
		ENDIF
		aItems:Clear()
		RETURN TRUE

    /// <exclude />
	METHOD __DeleteItemCore(oItem AS VOTreeNode, lChildrenOnly AS LOGIC) AS LOGIC
		LOCAL oWin AS Window
		oWin := (Window) SELF:Owner
		// deleting of array elements is done by __Remove
		//(called when DeleteItem notification is handled)
		IF (oItem == NULL_OBJECT)
			RETURN FALSE
		ENDIF

		IF  lChildrenOnly
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


/// <include file="Gui.xml" path="doc/TreeView.DeleteItem/*" />

	METHOD DeleteItem(oItem AS USUAL) AS LOGIC
		IF IsSymbol(oItem)
			RETURN SELF:DeleteItem((SYMBOL)oItem, FALSE)
		elseif oItem is TreeViewItem var oTVI
			return self:DeleteItem(oTVI, false)
		ENDIF
		RETURN FALSE
/// <include file="Gui.xml" path="doc/TreeView.DeleteItem/*" />

	METHOD DeleteItem(oItem AS USUAL, lChildrenOnly AS LOGIC) AS LOGIC
		IF IsSymbol(oItem)
			RETURN SELF:DeleteItem((SYMBOL)oItem, lChildrenOnly)
		elseif oItem is TreeViewItem var oTVI
			return self:DeleteItem(oTVI, lChildrenOnly)
		ENDIF
		RETURN FALSE



/// <include file="Gui.xml" path="doc/TreeView.DeleteItem/*" />
	METHOD DeleteItem(oItem AS TreeViewItem) AS LOGIC
		RETURN SELF:DeleteItem(oItem, FALSE)

/// <include file="Gui.xml" path="doc/TreeView.DeleteItem/*" />
	METHOD DeleteItem(oItem AS TreeViewItem, lChildrenOnly AS LOGIC) AS LOGIC
		IF oItem != NULL_OBJECT
			RETURN SELF:__DeleteItemCore(oItem:__Node, lChildrenOnly)
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/TreeView.DeleteItem/*" />
	METHOD DeleteItem(symName AS SYMBOL) AS LOGIC
		RETURN SELF:DeleteItem(symName, FALSE)


/// <include file="Gui.xml" path="doc/TreeView.DeleteItem/*" />
	METHOD DeleteItem(symName AS SYMBOL, lChildrenOnly AS LOGIC) AS LOGIC
		VAR oItem := SELF:__GetNode(symName)
		IF (oItem != NULL_OBJECT)
			RETURN SELF:__DeleteItemCore(oItem, lChildrenOnly)
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/TreeView.Destroy/*" />
	METHOD Destroy() AS USUAL CLIPPER
		oDragImageList := NULL_OBJECT
		RETURN SUPER:Destroy()

/// <include file="Gui.xml" path="doc/TreeView.DragDropEnabled/*" />
	ACCESS DragDropEnabled AS LOGIC
		RETURN lDragDropEnabled

/// <include file="Gui.xml" path="doc/TreeView.DragImageList/*" />
	ACCESS DragImageList AS ImageList
		RETURN oDragImageList

/// <include file="Gui.xml" path="doc/TreeView.DragImageList/*" />
	ASSIGN DragImageList(oNewDragImageList AS ImageList)
		oDragImageList := oNewDragImageList

/// <include file="Gui.xml" path="doc/TreeView.EditItemLabel/*" />
	METHOD EditItemLabel(symName AS SYMBOL) AS LOGIC
		VAR oItem := SELF:__GetNode(symName)
		IF oItem != NULL_OBJECT
			oItem:BeginEdit()
			RETURN TRUE
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/TreeView.EnableDragDrop/*" />
	METHOD EnableDragDrop(lEnable := TRUE AS LOGIC)
		lDragDropEnabled := lEnable
		RETURN TRUE

/// <include file="Gui.xml" path="doc/TreeView.EnableSort/*" />
	METHOD EnableSort(symMethodName AS SYMBOL) AS VOID
		symSortRoutineName := symMethodName
		RETURN

/// <include file="Gui.xml" path="doc/TreeView.EnsureVisible/*" />
	METHOD EnsureVisible(symName AS SYMBOL) AS VOID
		VAR oItem := SELF:__GetNode(symName)
		oItem:EnsureVisible()
		RETURN

/// <include file="Gui.xml" path="doc/TreeView.Expand/*" />
	METHOD Expand(oItem AS TreeViewItem, lAll AS LOGIC, lForceNotify AS LOGIC)
		IF oItem != NULL_OBJECT
			RETURN SELF:__Expand(oItem:__Node, TVE_EXPAND, lAll, lForceNotify)
		ENDIF
		RETURN FALSE
/// <include file="Gui.xml" path="doc/TreeView.Expand/*" />
	METHOD Expand(symName AS SYMBOL, lAll AS LOGIC, lForceNotify AS LOGIC)
		VAR oItem := SELF:GetItemAttributes(symName)
		RETURN SELF:Expand(oItem, lAll, lForceNotify)

/// <include file="Gui.xml" path="doc/TreeView.Expand/*" />
	METHOD Expand(symName AS SYMBOL, lAll AS LOGIC)
		RETURN SELF:Expand(symName, lAll, FALSE)

/// <include file="Gui.xml" path="doc/TreeView.Expand/*" />
	METHOD Expand(symName AS SYMBOL)
		RETURN SELF:Expand(symName, FALSE, FALSE)

/// <include file="Gui.xml" path="doc/TreeView.GetDropHighlight/*" />
	METHOD GetDropHighlight() AS TreeViewItem
		// Todo GetDropHighlight
		//RETURN SELF:GetItemAttributes(TreeView_GetDropHilight(SELF:Handle()))
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeView.GetFirstChildItem/*" />
	METHOD GetFirstChildItem(uItem AS USUAL) AS TreeViewItem
		IF IsSymbol(uItem)
			RETURN SELF:GetFirstChildItem((SYMBOL) uItem )
		ELSEIF IsObject(uItem)
			RETURN SELF:GetFirstChildItem((TreeViewItem) uItem )
		ENDIF
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeView.GetFirstChildItem/*" />
	METHOD GetFirstChildItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetFirstChildItem(oItem:NameSym)
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetFirstChildItem/*" />
	METHOD GetFirstChildItem(symItem AS SYMBOL) AS TreeViewItem
		VAR oItem := (VOTreeNode) SELF:__GetNode(symItem)
		IF oItem:Nodes:Count > 0
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode) oItem:Nodes:Item[0])
		ENDIF
		RETURN NULL_OBJECT
/// <include file="Gui.xml" path="doc/TreeView.GetFirstVisibleItem/*" />
    METHOD GetFirstVisibleItem() AS TreeViewItem
        LOCAL oItem AS VOTreeNode
		IF SELF:__IsValid .and. SELF:__TreeView:Nodes:Count > 0
            oItem := (VOTreeNode) SELF:__TreeView:Nodes[0]
            do while oItem != NULL .and. ! oItem:IsVisible
                oItem := (VOTreeNode) oItem:NextNode
            enddo
            RETURN SELF:__NodeToTreeviewItem(oItem)
        ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetItemAtPosition/*" />
	METHOD GetItemAtPosition(oPoint AS Point)
		// Fenster-bezogene Koordinate in Control-bezogene Koordinate zurückrechnen
		VAR oNode := (VOTreeNode) __TreeView:GetNodeAt(oPoint)
		RETURN SELF:__NodeToTreeviewItem(oNode)

	METHOD GetCurrentItem()
		VAR oNode := (VOTreeNode) __TreeView:SelectedNode
		RETURN SELF:__NodeToTreeviewItem(oNode)


	METHOD GetItemAttributes(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetItemAttributes(oItem:NameSym)
		ENDIF
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeView.GetItemAttributes/*" />
	METHOD GetItemAttributes(symItem AS SYMBOL) AS TreeViewItem
		VAR oItem := SELF:__GetNode(symItem)
		RETURN SELF:__NodeToTreeviewItem(oItem)

/// <include file="Gui.xml" path="doc/TreeView.GetItemBoundingBox/*" />
	METHOD GetItemBoundingBox(symItem AS SYMBOL, lTextOnly := FALSE AS LOGIC) AS BoundingBox
		VAR oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN (BoundingBox) oItem:Bounds
		ENDIF
		RETURN BoundingBox{}


/// <include file="Gui.xml" path="doc/TreeView.GetNextSiblingItem/*" />
	METHOD GetNextSiblingItem(oItem AS USUAL) AS TreeViewItem
		IF IsSymbol(oItem)
			RETURN SELF:GetNextSiblingItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN SELF:GetNextSiblingItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetNextSiblingItem/*" />
	METHOD GetNextSiblingItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetNextSiblingItem(oItem:NameSym)
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetNextSiblingItem/*" />
	METHOD GetNextSiblingItem(symItem AS SYMBOL) AS TreeViewItem
		VAR oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:NextNode)
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetNextVisibleItem/*" />

	METHOD GetNextVisibleItem(oItem AS USUAL) AS TreeViewItem
		IF IsSymbol(oItem)
			RETURN SELF:GetNextVisibleItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN SELF:GetNextVisibleItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetNextVisibleItem/*" />

	METHOD GetNextVisibleItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetNextVisibleItem(oItem:NameSym)
		ENDIF
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeView.GetNextVisibleItem/*" />
	METHOD GetNextVisibleItem(symItem AS SYMBOL) AS TreeViewItem
		VAR oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:NextVisibleNode)
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetParentItem/*" />
	METHOD GetParentItem(oItem AS USUAL) AS TreeViewItem
		IF IsSymbol(oItem)
			RETURN SELF:GetParentItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN SELF:GetParentItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT
/// <include file="Gui.xml" path="doc/TreeView.GetParentItem/*" />

	METHOD GetParentItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetParentItem(oItem:NameSym)
		ENDIF
		RETURN NULL_OBJECT

	METHOD GetParentItem(symItem AS SYMBOL) AS TreeViewItem
		VAR oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:Parent)
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetPreviousSiblingItem/*" />

	METHOD GetPreviousSiblingItem(oItem AS USUAL) AS TreeViewItem
		IF IsSymbol(oItem)
			RETURN SELF:GetPreviousSiblingItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN SELF:GetPreviousSiblingItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetPreviousSiblingItem/*" />
	METHOD GetPreviousSiblingItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetPreviousSiblingItem(oItem:NameSym)
		ENDIF
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeView.GetPreviousSiblingItem/*" />
	METHOD GetPreviousSiblingItem(symItem AS SYMBOL) AS TreeViewItem
		VAR oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:PrevNode)
		ENDIF
		RETURN NULL_OBJECT



/// <include file="Gui.xml" path="doc/TreeView.GetPreviousVisibleItem/*" />
	METHOD GetPreviousVisibleItem(oItem AS USUAL) AS TreeViewItem
		IF IsSymbol(oItem)
			RETURN SELF:GetPreviousVisibleItem((SYMBOL) oItem )
		ELSEIF IsObject(oItem)
			RETURN SELF:GetPreviousVisibleItem((TreeViewItem) oItem )
		ENDIF
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeView.GetPreviousVisibleItem/*" />
	METHOD GetPreviousVisibleItem(oItem AS TreeViewItem) AS TreeViewItem
		IF oItem != NULL_OBJECT
			RETURN SELF:GetPreviousVisibleItem(oItem:NameSym)
		ENDIF
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeView.GetPreviousVisibleItem/*" />
	METHOD GetPreviousVisibleItem(symItem AS SYMBOL)
		VAR oItem := SELF:__GetNode(symItem)
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem:PrevVisibleNode)
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetRootItem/*" />
	METHOD GetRootItem() AS TreeViewItem
		LOCAL oItem AS VOTreeNode
		IF SELF:__IsValid .and. SELF:__TreeView:Nodes:Count > 0
			oItem := (VOTreeNode) SELF:__TreeView:Nodes[0]
		ENDIF
		IF oItem != NULL_OBJECT
			RETURN SELF:__NodeToTreeviewItem((VOTreeNode)oItem)
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.GetSelectedItem/*" />
	METHOD GetSelectedItem() AS TreeViewItem
		IF SELF:__IsValid
			VAR oItem := __TreeView:SelectedNode
			IF oItem != NULL_OBJECT
				RETURN SELF:__NodeToTreeviewItem((VOTreeNode) oItem)
			ENDIF
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.ImageList/*" />
	ACCESS ImageList AS ImageList
		IF SELF:ValidateControl()
			RETURN ImageList{__TreeView:ImageList }
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeView.ImageList/*" />
	ASSIGN ImageList(oNewImageList AS ImageList)
		IF SELF:ValidateControl()
			__TreeView:ImageList := oNewImageList
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/TreeView.Indent/*" />
	ACCESS Indent AS LONG
		IF SELF:__IsValid
			RETURN __TreeView:Indent
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/TreeView.Indent/*" />
	ASSIGN Indent(dwIndent AS LONG)
		IF SELF:__IsValid
			__TreeView:Indent := dwIndent
		ENDIF

/// <include file="Gui.xml" path="doc/TreeView.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)
		LOCAL dwStyle AS DWORD

		aItems := Dictionary<SYMBOL, TreeViewItem> {}

		IF IsNil(kStyle)
			dwStyle := WS_BORDER
		ELSE
			dwStyle := _Or(DWORD(kStyle), DWORD(_CAST, WS_BORDER))
		ENDIF

		IF xID IS ResourceID
			SUPER(oOwner, xID, oPoint, oDimension, , dwStyle, TRUE)
		ELSE
			SUPER(oOwner, xID, oPoint, oDimension, "SysTreeView32", dwStyle, TRUE)
			SELF:SetStyle(_Or(TVS_HASLINES, TVS_HASBUTTONS, TVS_LINESATROOT, TVS_EDITLABELS))
		ENDIF
		oItemComparer := TreeViewItemComparer{SELF}

		RETURN

/// <include file="Gui.xml" path="doc/TreeView.InsertItem/*" />
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

/// <include file="Gui.xml" path="doc/TreeView.ItemCount/*" />
	ACCESS ItemCount AS LONG
		IF SELF:__IsValid
			RETURN __TreeView:GetNodeCount(TRUE)
		ENDIF
		RETURN 0

/// <include file="Gui.xml" path="doc/TreeView.SearchString/*" />
	PROPERTY SearchString AS STRING GET cSearchString

/// <include file="Gui.xml" path="doc/TreeView.SelectItem/*" />
	METHOD SelectItem(symItem, symCode, lSelect)
		LOCAL oItem AS VOTreeNode
		DEFAULT( REF lSelect, TRUE)
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

/// <include file="Gui.xml" path="doc/TreeView.SetItemAttributes/*" />
    METHOD SetItemAttributes(oTreeViewItem AS TreeViewItem) AS VOID
        // Todo
		RETURN


/// <include file="Gui.xml" path="doc/TreeView.SortChildren/*" />
	METHOD SortChildren(symParentName AS SYMBOL)
		IF SELF:__IsValid
			IF SELF:symSortRoutineName != NULL_SYMBOL
				SELF:__TreeView:TreeViewNodeSorter := SELF:oItemComparer
			ENDIF
			SELF:__TreeView:Sort()
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/TreeView.StateImageList/*" />
	ACCESS StateImageList AS ImageList
		RETURN ImageList{__TreeView:ImageList}
/// <include file="Gui.xml" path="doc/TreeView.StateImageList/*" />
	ASSIGN StateImageList(oNewImageList AS ImageList)
		IF SELF:__IsValid
		    __TreeView:StateImageList := oNewImageList
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/TreeView.RestoreUpdate/*" />
	METHOD RestoreUpdate() AS VOID STRICT
		IF SELF:__IsValid
			SELF:__TreeView:EndUpdate()
			SUPER:RestoreUpdate()
		ENDIF


/// <include file="Gui.xml" path="doc/TreeView.SuspendUpdate/*" />
	METHOD SuspendUpdate() AS VOID STRICT
		IF SELF:__IsValid
			SELF:__TreeView:BeginUpdate()
			SUPER:SuspendUpdate()
		ENDIF

/// <include file="Gui.xml" path="doc/TreeView.Toggle/*" />
	METHOD Toggle(symName AS SYMBOL, lAll := FALSE AS LOGIC, lForceNotify := FALSE AS LOGIC) AS LOGIC
		VAR oItem := SELF:__GetNode(symName)
		IF oItem == NULL_OBJECT
			RETURN FALSE
		ENDIF

		RETURN SELF:__Expand(oItem, TVE_TOGGLE, lAll, lForceNotify)

/// <include file="Gui.xml" path="doc/TreeView.VisibleCount/*" />
	ACCESS VisibleCount AS LONG
		RETURN __TreeView:VisibleCount

END CLASS

/// <include file="Gui.xml" path="doc/TreeViewItem/*" />
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
    /// <exclude />
	PROPERTY __Node AS VOTreeNode GET oNode SET oNode := Value
	//Todo Set Bold when needed
	/// <include file="Gui.xml" path="doc/TreeViewItem.Bold/*" />
	PROPERTY Bold  AS LOGIC GET lBold SET lBold := Value
	//Todo Set Disabled when needed

	/// <include file="Gui.xml" path="doc/TreeViewItem.Disabled/*" />
	PROPERTY Disabled			AS LOGIC	GET lDisabled					SET lDisabled := Value

	/// <include file="Gui.xml" path="doc/TreeViewItem.DropTarget/*" />
	PROPERTY DropTarget			AS LOGIC	GET lDropTarget					SET lDropTarget := VALUE

	/// <include file="Gui.xml" path="doc/TreeViewItem.Expanded/*" />
	PROPERTY Expanded			AS LOGIC	GET oNode:IsExpanded			SET iif(Value, oNode:Expand(), oNode:Collapse())

	//Todo Set Focused when needed
	/// <include file="Gui.xml" path="doc/TreeViewItem.Focused/*" />
	PROPERTY Focused			AS LOGIC	GET lFocused					SET lFocused := Value

	/// <include file="Gui.xml" path="doc/TreeViewItem.ImageIndex/*" />
	PROPERTY ImageIndex			AS LONG		GET oNode:ImageIndex+1			SET oNode:ImageIndex := Value-1

	/// <include file="Gui.xml" path="doc/TreeViewItem.Selected/*" />
	PROPERTY Selected			AS LOGIC	GET oNode:IsSelected			SET oNode:TreeView:SelectedNode := iif(Value, oNode, NULL_OBJECT)

	/// <include file="Gui.xml" path="doc/TreeViewItem.SelectedImageIndex/*" />
	PROPERTY SelectedImageIndex AS LONG		GET oNode:SelectedImageIndex+1	SET oNode:SelectedImageIndex := value-1

	/// <include file="Gui.xml" path="doc/TreeViewItem.StateImageIndex/*" />
	PROPERTY StateImageIndex	AS LONG		GET oNode:StateImageIndex		SET oNode:StateImageIndex := value

	/// <include file="Gui.xml" path="doc/TreeViewItem.TextValue/*" />
	PROPERTY TextValue			AS STRING	GET oNode:Text					SET oNode:Text := Value

	/// <include file="Gui.xml" path="doc/TreeViewItem.TreeViewControl/*" />
	PROPERTY TreeViewControl	AS TreeView	GET oTVControl

	/// <include file="Gui.xml" path="doc/TreeViewItem.Value/*" />
	PROPERTY Value				AS USUAL    GET uValue						SET uValue := Value
	#endregion
    /// <exclude />

	ASSIGN __TreeViewControl(oNewTVCtl AS TreeView)  STRICT
		oTVControl := oNewTVCtl
		RETURN


/// <include file="Gui.xml" path="doc/TreeViewItem.Collapse/*" />
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


/// <include file="Gui.xml" path="doc/TreeViewItem.Delete/*" />
	METHOD Delete(lChildrenOnly := FALSE AS LOGIC)

		IF (oTVControl != NULL_OBJECT)
			RETURN oTVControl:DeleteItem(SELF:symName, lChildrenOnly)
		ENDIF
		RETURN FALSE


/// <include file="Gui.xml" path="doc/TreeViewItem.Expand/*" />
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


/// <include file="Gui.xml" path="doc/TreeViewItem.FirstChild/*" />
	ACCESS FirstChild AS TreeViewItem
		IF oNode:FirstNode != NULL_OBJECT
			RETURN ((VOTreeNode) oNode:FirstNode):Item
		ENDIF
		RETURN NULL_OBJECT


/// <include file="Gui.xml" path="doc/TreeViewItem.ctor/*" />
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

/// <include file="Gui.xml" path="doc/TreeViewItem.NameSym/*" />
	PROPERTY NameSym AS SYMBOL GET symName SET symName := Value

/// <include file="Gui.xml" path="doc/TreeViewItem.NextSibling/*" />
	ACCESS NextSibling AS TreeViewItem
		LOCAL oSibling AS VOTreeNode
		oSibling := (VOTreeNode) oNode:NextNode
		IF oSibling != NULL_OBJECT
			RETURN oSibling:Item
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeViewItem.OverlayImageIndex/*" />
	PROPERTY OverlayImageIndex AS LONG GET nOverlayImage SET nOverlayImage := Value

/// <include file="Gui.xml" path="doc/TreeViewItem.Parent/*" />
	ACCESS Parent as TreeViewItem
		LOCAL oSibling AS VOTreeNode
		oSibling := (VOTreeNode) oNode:Parent
		IF oSibling != NULL_OBJECT
			RETURN oSibling:Item
		ENDIF
		RETURN NULL_OBJECT

/// <include file="Gui.xml" path="doc/TreeViewItem.PreviousSibling/*" />
	ACCESS PreviousSibling As TreeViewItem
		LOCAL oSibling AS VOTreeNode
		oSibling := (VOTreeNode) oNode:PrevNode
		IF oSibling != NULL_OBJECT
			RETURN oSibling:Item
		ENDIF
		RETURN NULL_OBJECT



END CLASS

//Todo tree view comparison code
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



USING System.Collections
USING System.Reflection
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
