//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Treeview.prg
// This file contains a subclass of the Windows.Forms.Treeview control
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control

USING SWF := System.Windows.Forms
USING System.Windows.Forms
USING System.Collections.Generic
USING VOSDK := XSharp.VO.SDK

CLASS VOTreeNode INHERIT SWF.TreeNode
	PROPERTY Item AS TreeViewItem AUTO

	METHOD LinkTo(oItem AS TreeViewItem) AS VOID STRICT
		SELF:Item := oItem
		IF oItem:NameSym != NULL_SYMBOL
			SELF:Name := oItem:NameSym:ToString()
		ENDIF

	CONSTRUCTOR(oItem AS TreeViewItem)
		SUPER()
		SELF:LinkTo(oItem)

	ACCESS NestingLevel AS LONG
		RETURN 1 // SELF:Parents:Count+1


	// Example tree
	//  A
	//    B
	//       C
	//	  D
	//		 E
	//  F
	//    G
	// Parents of E are A-D
	// Parents of C are A-B
	// Parents of D is A
	// parents of B is A
	// A & F have no parents


END CLASS

class VOTreeView inherit SWF.TreeView implements IVOControlProperties
	PROPERTY TreeView AS VOSDK.TreeView GET (VOSDK.TreeView) oProperties:Control

	#include "PropControlStyle.xh"

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:SetVisualStyle()


	VIRTUAL PROTECTED METHOD OnAfterLabelEdit(e AS SWF.NodeLabelEditEventArgs) AS VOID
		LOCAL oWin AS Window
		SUPER:OnAfterLabelEdit(e)
		oWin := self:Control:Owner
		oWin:TreeViewItemEdit(TreeViewEditEvent{SELF:TreeView, ((VOTreeNode)e:Node):Item,e:Label, FALSE})
		RETURN

	VIRTUAL PROTECTED METHOD OnBeforeLabelEdit(e AS SWF.NodeLabelEditEventArgs) AS VOID
		LOCAL oWin AS Window
		IF !SELF:LabelEdit
			e:CancelEdit := TRUE
		ELSE
			SUPER:OnBeforeLabelEdit(e)
			oWin := self:Control:Owner
			oWin:TreeViewItemEdit(TreeViewEditEvent{SELF:TreeView, ((VOTreeNode)e:Node):Item,e:Label, TRUE})
		ENDIF
		RETURN


	VIRTUAL PROTECTED METHOD OnKeyDown(e AS SWF.KeyEventArgs) AS VOID
		LOCAL oWin AS Window
		SUPER:OnKeyDown(e)
		oWin := self:Control:Owner
		oWin:TreeViewKeyDown(TreeViewKeyEvent{SELF:TreeView, e:KeyCode})
		RETURN

	VIRTUAL PROTECTED METHOD OnAfterExpand(e AS SWF.TreeViewEventArgs) AS VOID
		LOCAL oWin AS Window
		SUPER:OnAfterExpand(e)
		oWin := self:Control:Owner
		oWin:TreeViewItemExpanded(TreeViewExpandedEvent{SELF:TreeView, ((VOTreeNode) e:Node):Item, e:Action})
		RETURN

	VIRTUAL PROTECTED METHOD OnAfterCollapse(e AS SWF.TreeViewEventArgs) AS VOID
		LOCAL oWin AS Window
		SUPER:OnAfterCollapse(e)
		oWin := self:Control:Owner
		oWin:TreeViewItemExpanded(TreeViewExpandedEvent{SELF:TreeView, ((VOTreeNode) e:Node):Item, e:Action})
		RETURN

	VIRTUAL PROTECTED METHOD OnBeforeCollapse(e AS SWF.TreeViewCancelEventArgs) AS VOID
		LOCAL oWin AS Window
		SUPER:OnBeforeCollapse(e)
		oWin := self:Control:Owner
		oWin:TreeViewItemExpanding(TreeViewExpandingEvent{SELF:TreeView, ((VOTreeNode) e:Node):Item})
		RETURN


	VIRTUAL PROTECTED METHOD OnBeforeExpand(e AS SWF.TreeViewCancelEventArgs) AS VOID
		LOCAL oWin AS Window
		SUPER:OnBeforeExpand(e)
		oWin := SELF:Control:Owner
		oWin:TreeViewItemExpanding(TreeViewExpandingEvent{SELF:TreeView, ((VOTreeNode) e:Node):Item})
		RETURN

	VIRTUAL PROTECTED METHOD OnAfterSelect(e AS SWF.TreeViewEventArgs) AS VOID
		LOCAL oWin AS Window
		SUPER:OnAfterSelect(e)
		oWin := SELF:Control:Owner
		oWin:TreeViewSelectionChanged(TreeViewSelectionEvent{SELF:TreeView, ((VOTreeNode) e:Node):Item, e:Action, FALSE})
		RETURN

	VIRTUAL PROTECTED METHOD OnBeforeSelect(e AS SWF.TreeViewCancelEventArgs) AS VOID
		LOCAL oWin AS Window
		LOCAL oItem AS TreeViewItem
		SUPER:OnBeforeSelect(e)
		oWin := SELF:Control:Owner
		IF e:Node != NULL_OBJECT
			oItem := ((VOTreeNode)e:Node):Item
		ENDIF
		oWin:TreeViewSelectionChanging(TreeViewSelectionEvent{SELF:TreeView, oItem, SWF.TreeViewAction.Unknown, TRUE})
		RETURN

	VIRTUAL PROTECTED METHOD OnNodeMouseClick(e AS SWF.TreeNodeMouseClickEventArgs ) AS VOID
		LOCAL oWin AS Window
		SUPER:OnNodeMouseClick(e)
		oWin := SELF:Control:Owner
		oWin:TreeViewMouseButtonDown(TreeViewMouseEvent{SELF:TreeView, ((VOTreeNode) e:Node):Item, e})
		RETURN

	VIRTUAL PROTECTED METHOD OnNodeMouseDoubleClick(e AS SWF.TreeNodeMouseClickEventArgs ) AS VOID
		LOCAL oWin AS Window
		SUPER:OnNodeMouseDoubleClick(e)
		oWin := SELF:Control:Owner
		oWin:TreeViewMouseButtonDoubleClick(TreeViewMouseEvent{SELF:TreeView, ((VOTreeNode) e:Node):Item, e})
		RETURN

END CLASS
