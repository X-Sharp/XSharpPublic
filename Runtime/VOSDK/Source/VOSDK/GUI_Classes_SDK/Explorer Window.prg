STATIC DEFINE __EXPW_LABEL_HEIGHT	:= 19

CLASS __ExplorerLV INHERIT ListView
	EXPORT symSortCol AS SYMBOL

METHOD DefaultSort(oLVItem1, oLVItem2) 
	LOCAL s1, s2 AS STRING

	

	s1 := oLVItem1:GetText(symSortCol)
	s2 := oLVItem2:GetText(symSortCol)

	IF (s1 == s2)
		RETURN 0
	ELSEIF (s1 < s2)
		RETURN -1
	ELSE
		RETURN 1
	ENDIF

METHOD Dispatch(oEvent) 
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

CLASS __ExplorerTV INHERIT TreeView

METHOD Dispatch(oEvent) 
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

CLASS ExplorerWindow INHERIT SplitWindow
	PROTECT oLabelLeft	AS FixedText
	PROTECT oLabelRight	AS FixedText
	PROTECT oTreeView	AS TreeView
	PROTECT oListView	AS ListView

	//PP-030828 Strong typing
	METHOD __FocusLV AS ExplorerWindow STRICT 
	//PP-030828 Strong typing
	

	oListView:SetFocus()
	RETURN SELF

METHOD __FocusTV AS ExplorerWindow STRICT 
	//PP-030828 Strong typing
	

	oTreeView:SetFocus()
	RETURN SELF

METHOD Destroy() 
	

	IF !InCollect()
		oLabelLeft := NULL_OBJECT
		oLabelRight := NULL_OBJECT
		oTreeView := NULL_OBJECT
		oListView := NULL_OBJECT
	ENDIF

	SUPER:Destroy()

	RETURN NIL

CONSTRUCTOR(oOwner, lLabels, symTreeViewClassName, symListViewClassName) 
	LOCAL oDimension	AS Dimension

	

	DEFAULT(@lLabels, TRUE) // by default, add labels

	// by default, create view controls from base classes
	DEFAULT(@symTreeViewClassName, #__ExplorerTV)
	DEFAULT(@symListViewClassName, #__ExplorerLV)

	SUPER(oOwner)

	// if labels will be added, create a four-pane
	// window; otherwise, create a two-pane window
	IF lLabels
		// set the rows and columns
		SELF:Layout := Dimension{2, 2}

		// create the pane clients
		oLabelLeft := FixedText{SELF, 1001, Point{}, Dimension{}, NULL_STRING}
		oLabelLeft:SetExStyle(WS_EX_STATICEDGE)
		oLabelRight := FixedText{SELF, 1002, Point{}, Dimension{}, NULL_STRING}
		oLabelRight:SetExStyle(WS_EX_STATICEDGE)
		oTreeView := CreateInstance(symTreeViewClassName, SELF, 1003, Point{}, Dimension{}, TVS_SHOWSELALWAYS)
		oListView := CreateInstance(symListViewClassName, SELF, 1004, Point{}, Dimension{}, _OR(LVS_SHOWSELALWAYS, LVS_AUTOARRANGE))

		// associate the clients with the respective panes
		SELF:SetPaneClient(oLabelLeft, 1)
		SELF:SetPaneClient(oLabelRight, 2)
		SELF:SetPaneClient(oTreeView, 3)
		SELF:SetPaneClient(oListView, 4)

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
		oTreeView := CreateInstance(symTreeViewClassName, SELF, 1001, Point{}, Dimension{}, TVS_SHOWSELALWAYS)
		oListView := CreateInstance(symListViewClassName, SELF, 1002, Point{}, Dimension{}, _OR(LVS_SHOWSELALWAYS, LVS_AUTOARRANGE))

		// associate the clients with the respective panes
		SELF:SetPaneClient(oTreeView, 1)
		SELF:SetPaneClient(oListView, 2)
	ENDIF
	// 2.0a-1, add check
	IF IsInstanceOf(oListView, #__ExplorerLV)
		oListView:EnableSort(#DefaultSort)
	ENDIF

	RETURN 

ACCESS LabelLeft 
	

	RETURN oLabelLeft

ACCESS LabelRight 
	

	RETURN oLabelRight

ACCESS ListView 
	

	RETURN oListView

METHOD ListViewColumnClick(oEvt) 
	

	IF IsInstanceOf(oListView, #__ExplorerLV)
		IVarPut(oListView, #symSortCol, oEvt:ListViewColumn:NameSym)
		oListView:SortItems()
	ENDIF

	RETURN SUPER:ListViewColumnClick(oEvt)

ACCESS TreeView 
	

	RETURN oTreeView
END CLASS

