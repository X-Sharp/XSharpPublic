//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
#define  __EXPW_LABEL_HEIGHT	19

 /// <exclude />
CLASS __ExplorerLV INHERIT ListView
	PROPERTY symSortCol AS SYMBOL   AUTO

METHOD DefaultSort(oLVItem1, oLVItem2)
	LOCAL s1, s2 AS STRING
    local oLVI1, oLVI2 as ListViewItem
    
	s1 := ((ListViewItem) oLVItem1):GetText(symSortCol)
	s2 := ((ListViewItem) oLVItem2):GetText(symSortCol)

	IF (s1 == s2)
		RETURN 0
	ELSEIF (s1 < s2)
		RETURN -1
	ELSE
		RETURN 1
	ENDIF

 /// <exclude />
METHOD Dispatch(oEvent  AS @@Event)
	LOCAL oEvt := oEvent AS @@Event


	IF (oEvt:uMsg == WM_CHAR)	.AND. (oEvt:wParam == 0x00000009)
		Send(SELF:Owner, #__FocusTV)
		RETURN (SELF:EventReturnValue := 1L)
	ENDIF
	RETURN SUPER:Dispatch(oEvt)

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)

	SUPER(oOwner, xID, oPoint, oDimension, kStyle)


RETURN
END CLASS

 /// <exclude />
CLASS __ExplorerTV INHERIT TreeView

 /// <exclude />
METHOD Dispatch(oEvent AS @@Event)
	LOCAL oEvt := oEvent AS @@Event

	IF (oEvt:uMsg == WM_CHAR)	.AND. (oEvt:wParam == 0x00000009)
		Send(SELF:Owner, #__FocusLV)
		RETURN (SELF:EventReturnValue := 1L)
	ENDIF
	RETURN SUPER:Dispatch(oEvt)

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)

	SUPER(oOwner, xID, oPoint, oDimension, kStyle)


RETURN
END CLASS


/// <include file="Gui.xml" path="doc/ExplorerWindow/*" />
CLASS ExplorerWindow INHERIT SplitWindow

 /// <exclude />
METHOD __FocusLV AS ExplorerWindow STRICT
    SELF:ListView:SetFocus()
	RETURN SELF

 /// <exclude />
METHOD __FocusTV AS ExplorerWindow STRICT
	SELF:TreeView:SetFocus()
	RETURN SELF

/// <include file="Gui.xml" path="doc/ExplorerWindow.Destroy/*" />
METHOD Destroy() AS USUAL CLIPPER
	SELF:LabelLeft := NULL_OBJECT
	SELF:LabelRight := NULL_OBJECT
	SELF:TreeView := NULL_OBJECT
	SELF:ListView := NULL_OBJECT

	SUPER:Destroy()

	RETURN NIL

/// <include file="Gui.xml" path="doc/ExplorerWindow.ctor/*" />
CONSTRUCTOR(oOwner, lLabels, symTreeViewClassName, symListViewClassName)
	LOCAL oDimension	AS Dimension

	DEFAULT( REF lLabels, TRUE) // by default, add labels

	// by default, create view controls from base classes
	DEFAULT( REF symTreeViewClassName, #__ExplorerTV)
	DEFAULT( REF symListViewClassName, #__ExplorerLV)

	SUPER(oOwner)

	// if labels will be added, create a four-pane
	// window; otherwise, create a two-pane window
	IF lLabels
		// set the rows and columns
		SELF:Layout := Dimension{2, 2}

		// create the pane clients
		SELF:LabelLeft := FixedText{SELF, 1001, Point{}, Dimension{}, NULL_STRING}
		SELF:LabelLeft:SetExStyle(WS_EX_STATICEDGE)
		SELF:LabelRight := FixedText{SELF, 1002, Point{}, Dimension{}, NULL_STRING}
		SELF:LabelRight:SetExStyle(WS_EX_STATICEDGE)
		SELF:TreeView := CreateInstance(symTreeViewClassName, SELF, 1003, Point{}, Dimension{}, TVS_SHOWSELALWAYS)
		SELF:ListView := CreateInstance(symListViewClassName, SELF, 1004, Point{}, Dimension{}, _Or(LVS_SHOWSELALWAYS, LVS_AUTOARRANGE))

		// associate the clients with the respective panes
		SELF:SetPaneClient(SELF:LabelLeft, 1)
		SELF:SetPaneClient(SELF:LabelRight, 2)
		SELF:SetPaneClient(SELF:TreeView, 3)
		SELF:SetPaneClient(SELF:ListView, 4)

		// set the height of the label panes
		oDimension := SELF:GetPaneSize(1)
		oDimension := Dimension{oDimension:Width, __EXPW_LABEL_HEIGHT}
		SELF:SetPaneSize(oDimension, 1)
		oDimension := SELF:GetPaneSize(2)
		// Add 1 to the width below to avoid split view size anomalies
		oDimension := Dimension{oDimension:Width + 1, __EXPW_LABEL_HEIGHT}
		SELF:SetPaneSize(oDimension, 2)
	ELSE
		// set the rows and columns
		SELF:Layout := Dimension{2, 1}

		// create the pane clients
		SELF:TreeView := CreateInstance(symTreeViewClassName, SELF, 1001, Point{}, Dimension{}, TVS_SHOWSELALWAYS)
		SELF:ListView := CreateInstance(symListViewClassName, SELF, 1002, Point{}, Dimension{}, _OR(LVS_SHOWSELALWAYS, LVS_AUTOARRANGE))
		// associate the clients with the respective panes
		SELF:SetPaneClient(SELF:TreeView, 1)
		SELF:SetPaneClient(SELF:ListView, 2)
	ENDIF
	// 2.0a-1, add check
	IF SELF:ListView IS __ExplorerLV
		SELF:ListView:EnableSort(#DefaultSort)
	ENDIF

	RETURN

/// <include file="Gui.xml" path="doc/ExplorerWindow.LabelLeft/*" />
PROPERTY LabelLeft AS Fixedtext AUTO GET PRIVATE SET

/// <include file="Gui.xml" path="doc/ExplorerWindow.LabelRight/*" />
PROPERTY LabelRight AS Fixedtext AUTO GET PRIVATE SET

/// <include file="Gui.xml" path="doc/ExplorerWindow.ListView/*" />
PROPERTY ListView AS ListView AUTO GET PRIVATE SET

/// <include file="Gui.xml" path="doc/ExplorerWindow.ListViewColumnClick/*" />
METHOD ListViewColumnClick(oEvt AS ListViewColumnClickEvent) AS USUAL

	IF SELF:ListView IS __ExplorerLV VAR oLV
		oLV:symSortCol := oEvt:ListViewColumn:NameSym
		SELF:ListView:SortItems()
	ENDIF

	RETURN SUPER:ListViewColumnClick(oEvt)

/// <include file="Gui.xml" path="doc/ExplorerWindow.TreeView/*" />
PROPERTY TreeView AS TreeView AUTO GET PRIVATE SET
END CLASS


