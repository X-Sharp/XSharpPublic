//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// ListViewprg.prg

// This file contains a subclass of the Windows.Forms.ListView control
// Also some On..() methods have been implemented that call the event handles on the VO Window
// class that owns the control
USING SWF := System.Windows.Forms
USING System.Collections
USING System.Collections.Generic
USING System.Windows.Forms
USING VOSDK := XSharp.VO.SDK
class VOListView inherit SWF.ListView  implements IVOControlProperties
    property ListView     as VOSDK.ListView get (VOSDK.ListView) oProperties:Control
	#include "PropControlStyle.xh"

	METHOD Initialize() AS VOID STRICT
		SELF:View := SWF.View.Details

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		oProperties := VOControlProperties{SELF, Owner, dwStyle, dwExStyle}
		SUPER()
		SELF:Initialize()
		SELF:SetVisualStyle()

    method ContainsColumn(sName as string) as logic
        RETURN SUPER:Columns:ContainsKey(sName)

    METHOD RemoveColumn(sName AS STRING) AS VOID
        SUPER:Columns:RemoveByKey(sName)
        RETURN


    new property Columns as IList get super:Columns
    NEW PROPERTY Groups AS IList GET SUPER:Groups
	NEW PROPERTY Items AS IList GET SUPER:Items
    NEW PROPERTY SelectedItems AS IList GET SUPER:SelectedItems
    NEW PROPERTY SelectedIndices AS IList GET SUPER:SelectedIndices

	#region Event Handlers

	VIRTUAL PROTECTED METHOD OnAfterLabelEdit(e AS SWF.LabelEditEventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnAfterLabelEdit(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewItemEdit(ListViewEditEvent{SELF:ListView, e, FALSE})
		RETURN

	VIRTUAL PROTECTED METHOD OnBeforeLabelEdit(e AS SWF.LabelEditEventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnBeforeLabelEdit(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewItemEdit(ListViewEditEvent{SELF:ListView, e, TRUE})
		RETURN

	VIRTUAL PROTECTED METHOD OnColumnClick(e AS SWF.ColumnClickEventArgs ) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnColumnClick(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewColumnClick(ListViewColumnClickEvent{SELF:ListView, e})
		RETURN


	VIRTUAL PROTECTED METHOD OnItemChecked(e AS SWF.ItemCheckedEventArgs ) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnItemChecked(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewItemChanged(ListViewItemEvent{SELF:ListView, e})
		RETURN

	VIRTUAL PROTECTED METHOD OnItemDrag(e AS SWF.ItemDragEventArgs ) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnItemDrag(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewItemDrag(ListViewDragEvent{SELF:ListView, e})
		RETURN


	VIRTUAL PROTECTED METHOD OnItemSelectionChanged(e AS SWF.ListViewItemSelectionChangedEventArgs ) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnItemSelectionChanged(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewItemChanged(ListViewItemEvent{SELF:ListView, e})
		RETURN

	VIRTUAL PROTECTED METHOD OnMouseDown(e AS SWF.MouseEventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnMouseDown(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewMouseButtonDown(ListViewMouseEvent{SELF:ListView, e})
		RETURN

	VIRTUAL PROTECTED METHOD OnMouseDoubleClick(e AS SWF.MouseEventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnMouseDoubleClick(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewMouseButtonDoubleClick(ListViewMouseEvent{SELF:ListView, e})
		RETURN

	VIRTUAL PROTECTED METHOD OnSelectedIndexChanged(e AS EventArgs ) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnSelectedIndexChanged(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewItemChanged(ListViewItemEvent{SELF:ListView, e})
		RETURN

	VIRTUAL PROTECT METHOD OnKeyDown(e AS SWF.KeyEventArgs) AS VOID
		LOCAL oWindow AS Window
		SUPER:OnKeyDown(e)
		oWindow := (Window) SELF:Control:Owner
		oWindow:ListViewKeyDown(ListViewKeyEvent{SELF:ListView, e})
		RETURN

	//VIRTUAL PROTECT METHOD WndProc(msg REF Message) AS VOID
	//	IF oProperties == NULL_OBJECT
	//		RETURN SUPER:WndProc(msg)
	//	ELSEIF ! oProperties:WndProc(msg)
	//		SUPER:WndProc(msg)
	//	ENDIF
	//	RETURN


	#endregion

END CLASS


CLASS VODataListView INHERIT VOListView

	CONSTRUCTOR(Owner AS VOSDK.Control, dwStyle AS LONG, dwExStyle AS LONG)
		SUPER(Owner, dwStyle, dwExStyle)
		SELF:VirtualMode := TRUE

END CLASS


CLASS VOColumnHeader INHERIT SWF.ColumnHeader
	PROPERTY Column AS VOSDK.ListViewColumn AUTO
	PROPERTY Header AS SWF.ColumnHeader GET SELF
	METHOD LinkTo(oColumn AS VOSDK.ListViewColumn) AS VOID STRICT
		SELF:Column := oColumn
		SELF:Tag	:= oColumn
		IF oColumn:HyperLabel != NULL_OBJECT
			SELF:Name := oColumn:HyperLabel:Name
		ENDIF

	CONSTRUCTOR(oColumn AS VOSDK.ListViewColumn) STRICT
		SUPER()
		SELF:LinkTo(oColumn)
END CLASS



CLASS VOListViewItem INHERIT SWF.ListViewItem
	PROPERTY Item AS VOSDK.ListViewItem AUTO
	property SWFItem as SWF.ListViewItem get self
    NEW PROPERTY Group AS SWF.ListViewGroup GET SUPER:Group SET SUPER:Group := VALUE
	NEW PROPERTY SubItems AS IList GET SUPER:SubItems
	METHOD LinkTo(oItem AS VOSDK.ListViewItem) AS VOID STRICT
		SELF:Item  := oItem
		SELF:Tag   := oItem

	CONSTRUCTOR(oItem AS VOSDK.ListViewItem) STRICT
		SUPER()
		SELF:LinkTo(oItem)

END CLASS

// Cannot create subclass from ListViewGroup because it is sealed

//CLASS VOListViewGroup INHERIT SWF.ListViewGroup IMPLEMENTS IVOListViewGroup
//    CONSTRUCTOR(cName)
//        SUPER(cName)
//END CLASS
