//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// EventsTreeview.prg




USING System.Diagnostics


CLASS TreeViewItemEvent INHERIT ControlNotifyEvent
	PROTECT oTvItem AS TreeViewItem
	CONSTRUCTOR(oTreeView AS TreeView, oItem AS TreeViewItem) 
		SUPER(oTreeview)
		oTvItem := oItem

		RETURN 

	ACCESS TreeViewItem AS TreeViewItem STRICT 
		RETURN oTvItem

END CLASS

CLASS TreeViewDeleteEvent INHERIT TreeViewItemEvent
	
	CONSTRUCTOR(oTreeView AS TreeView, oItem AS TreeViewItem) 
		SUPER(oTreeView, oItem)

END CLASS

CLASS TreeViewDragEvent INHERIT TreeViewItemEvent

	CONSTRUCTOR(oTreeView AS TreeView, oItem AS TreeViewItem) 
		SUPER(oTreeView, oItem)

	ACCESS IsLeftButton AS LOGIC STRICT 
		RETURN (SELF:NotifyCode == TVN_BEGINDRAG)

	ACCESS IsRightButton AS LOGIC STRICT  


		RETURN (SELF:NotifyCode == TVN_BEGINRDRAG)

	//ACCESS Position AS Point STRICT  
	//	LOCAL strucTreeView AS _winNM_TREEVIEW
	//	LOCAL oPoint AS Point



	//	strucTreeView := PTR(_CAST, SELF:lParam)

	//	oPoint := Point{strucTreeView:ptDrag:x, strucTreeView:ptDrag:y}
	//	oPoint := __WCConvertPoint(SELF:Control, oPoint)

	//	RETURN oPoint


END CLASS

CLASS TreeViewEditEvent INHERIT TreeViewItemEvent
	PROTECT cEditText AS STRING
	PROTECT lBeginning   AS LOGIC

	CONSTRUCTOR(oTreeView AS TreeView, oItem AS TreeViewItem, cLabel AS STRING, lBefore AS LOGIC) 
		SUPER(oTreeView, oItem)
		SELF:lBeginning := lBefore
		SELF:cEditText := cLabel

	ACCESS EditBeginning AS LOGIC STRICT 
		RETURN lBeginning

	ACCESS EditEnding AS LOGIC STRICT 
		RETURN !lBeginning

	ACCESS EditText  AS STRING STRICT 
		RETURN cEditText

END CLASS

CLASS TreeViewExpandedEvent INHERIT TreeViewItemEvent
	PROTECT nAction AS System.Windows.Forms.TreeViewAction
	
	CONSTRUCTOR(oTreeView AS TreeView, oItem AS TreeViewItem, Action AS System.Windows.Forms.TreeViewAction) 
		SUPER(oTreeView, oItem)
		
	ACCESS Collapsed AS LOGIC STRICT 
		RETURN nAction == System.Windows.Forms.TreeViewAction.Collapse

	ACCESS Expanded  AS LOGIC STRICT 
		RETURN nAction == System.Windows.Forms.TreeViewAction.Expand


END CLASS

CLASS TreeViewExpandingEvent INHERIT TreeViewItemEvent

	CONSTRUCTOR(oTreeView AS TreeView, oItem AS TreeViewItem) 
		SUPER(oTreeView, oItem)

END CLASS

CLASS TreeViewKeyEvent INHERIT ControlNotifyEvent
	PROTECT nKeyCode AS LONG
	CONSTRUCTOR(oTreeView AS TreeView, KeyCode AS LONG) 
		SUPER(oTreeView)
		nKeyCode := KeyCode
		RETURN 

	ACCESS KeyCode AS LONGINT STRICT 
		RETURN nKeyCode

END CLASS

CLASS TreeViewMouseEvent INHERIT TreeViewItemEvent
	PROTECT args AS System.Windows.Forms.TreeNodeMouseClickEventArgs
	PROTECT oInfo AS System.Windows.Forms.TreeViewHitTestInfo
	CONSTRUCTOR(oTreeView AS TreeView, oItem AS TreeViewItem, e AS  System.Windows.Forms.TreeNodeMouseClickEventArgs) 
		SUPER(oTreeView, oItem)
		args := e
		oInfo  := oTreeView:__TreeView:HitTest(e:Location)

	
	ACCESS ButtonID AS LONGINT STRICT 
		IF (SELF:IsLeftButton)
			RETURN BUTTONLEFT
		ENDIF
		IF (SELF:IsRightButton)
			RETURN BUTTONRIGHT
		ENDIF
		RETURN 0

	ACCESS PointOnItem  AS LOGIC STRICT 
		RETURN SELF:PointOnItemButton .or. SELF:PointOnItemImage .or. SELF:PointOnItemIndent .or. SELF:PointOnItemLabel .or. SELF:PointOnItemRight

	ACCESS PointAboveClientArea  AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.AboveClientArea

	ACCESS PointBelowClientArea  AS LOGIC STRICT 
	RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.BelowClientArea
	
	ACCESS PointLeftOfClientArea  AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.LeftOfClientArea

	ACCESS PointNowhere  AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.None

	ACCESS PointOnItemButton  AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.PlusMinus

	ACCESS PointOnItemImage  AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.Image

	ACCESS PointOnItemIndent  AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.Indent

	ACCESS PointOnItemLabel  AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.Label

	ACCESS PointOnItemRight AS LOGIC STRICT  
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.RightOfLabel

	ACCESS PointOnItemStateImage  AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.StateImage

	ACCESS PointRightOfClientArea  AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.TreeviewHitTestLocations.RightOfClientArea

	ACCESS IsLeftButton AS LOGIC STRICT 
		RETURN args:Button == System.Windows.Forms.MouseButtons.Left

	ACCESS IsRightButton  AS LOGIC STRICT 
		RETURN args:Button == System.Windows.Forms.MouseButtons.Right

	ACCESS Position AS Point STRICT 
		RETURN args:Location



END CLASS

CLASS TreeViewSelectionEvent INHERIT TreeViewItemEvent
	PROTECT nAction AS System.Windows.Forms.TreeViewAction
	PROTECT lChanging AS LOGIC
	
	CONSTRUCTOR(oTreeView AS TreeView, oItem AS TreeViewItem, Action AS System.Windows.Forms.TreeViewAction, lBeginning AS LOGIC) 
		SUPER(oTreeView, oItem)
		nAction := Action
		lChanging := lBeginning

	ACCESS KeyBoardAction AS LOGIC STRICT 
		RETURN nAction == System.Windows.Forms.TreeViewAction.ByKeyboard

	ACCESS MouseAction AS LOGIC STRICT 
		RETURN nAction == System.Windows.Forms.TreeViewAction.ByMouse

	ACCESS NewTreeViewItem AS TreeViewItem STRICT 
		IF lChanging
			RETURN NULL_OBJECT
		ELSE
			RETURN SELF:TreeViewItem
		ENDIF

	ACCESS OldTreeViewItem AS TreeViewItem STRICT 
		IF lChanging
			RETURN SELF:TreeViewItem
		ELSE
			RETURN NULL_OBJECT
		ENDIF

	ACCESS SelectionChanged  AS LOGIC STRICT 
		RETURN ! lChanging

	ACCESS SelectionChanging  AS LOGIC STRICT 
		RETURN lChanging

	ACCESS UnknownAction  AS LOGIC STRICT 
		RETURN nAction == System.Windows.Forms.TreeViewAction.Unknown

END CLASS


