//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// Event_Listview.prg




USING System.Diagnostics
USING VOSDK := XSharp.VO.SDK
#region ListViewEvents
CLASS ListViewColumnClickEvent INHERIT ControlNotifyEvent
	PROTECT oCol AS ListViewColumn
	PROTECT oListView AS ListView

	[DebuggerStepThrough];	
	CONSTRUCTOR(oLV AS ListView, e AS System.Windows.Forms.ColumnClickEventArgs) 
		SUPER(oLV)
		oListView := oLV
		oCol := oListView:GetColumn(e:Column+1)
	
	ACCESS ListViewColumn AS ListViewColumn STRICT 
		RETURN oCol
END CLASS

CLASS ListViewDeleteEvent INHERIT ListViewItemEvent
	CONSTRUCTOR(oLV AS ListView, oItem AS ListViewItem)
		SUPER(oLV, oItem)
		RETURN 
END CLASS

CLASS ListViewDragEvent INHERIT ListViewItemEvent
	PROTECT nButton AS System.Windows.Forms.MouseButtons
	PROTECT oPoint AS Point
	
	[DebuggerStepThrough];	
	CONSTRUCTOR(oLV AS ListView, e AS System.Windows.Forms.ItemDragEventArgs)
		SUPER(oLV)
        nButton := e:Button
        var oItem := (VOListViewItem) e:Item
        self:oLVI := oItem:Tag
		oPoint := Point{oItem:Position:X, oItem:Position:Y}

	ACCESS IsLeftButton AS LOGIC STRICT 
		RETURN nButton == System.Windows.Forms.MouseButtons.Left

	ACCESS IsRightButton AS LOGIC STRICT 
		RETURN nButton == System.Windows.Forms.MouseButtons.Right

	ACCESS Position AS Point STRICT 
		RETURN oPoint

END CLASS

CLASS ListViewEditEvent INHERIT ControlNotifyEvent
	PROTECT lBeginning	AS LOGIC
	PROTECT cText		AS STRING
	PROTECT oItem		AS ListViewItem
	PROTECT oListView	AS ListView

	ACCESS EditBeginning AS LOGIC STRICT 
		RETURN lBeginning

	ACCESS EditEnding AS LOGIC STRICT 
		RETURN !lBeginning

	ACCESS EditText AS STRING STRICT 
		RETURN cText

	[DebuggerStepThrough];
	CONSTRUCTOR(oLV AS ListView, e AS System.Windows.Forms.LabelEditEventArgs, lBefore AS LOGIC) 
		SUPER(oLV)
		lBeginning := lBefore
		oItem      := oLV:GetItemAttributes(e:Item+1)
		cText	   := e:Label
	
	ACCESS ListViewItem 
		RETURN oItem

END CLASS


CLASS ListViewItemEvent INHERIT ControlNotifyEvent
	PROTECT oLVI		AS ListViewItem
	PROTECT oListView	AS ListView

	
	CONSTRUCTOR(oLV AS VOSDK.ListView)
		SUPER(oLV)
		oListView := oLV
		oLVI := oListView:GetSelectedItem()
	
	[DebuggerStepThrough];
	CONSTRUCTOR(oLV AS VOSDK.ListView, oItem AS ListViewItem)
		SUPER(oLV)
		oLVI := oItem

	[DebuggerStepThrough];
	CONSTRUCTOR(oLV AS VOSDK.ListView, e AS System.EventArgs)
		SUPER(oLV)
		oListView := oLV
		oLVI := oListView:GetSelectedItem()

	[DebuggerStepThrough];	
	CONSTRUCTOR(oLV AS VOSDK.ListView, e AS System.Windows.Forms.ItemChangedEventArgs)
		SUPER(oLV)
		oListView := oLV
		oLVI	  := oLV:GetItemAttributes(e:Index+1)
	
	[DebuggerStepThrough];
	CONSTRUCTOR(oLV AS VOSDK.ListView, e AS System.Windows.Forms.ItemCheckEventArgs)
		SUPER(oLV)
		oListView := oLV
		oLVI	  := oLV:GetItemAttributes(e:Index)

	ACCESS ListViewItem AS ListViewItem STRICT 
		RETURN oLVI
END CLASS

CLASS ListViewKeyEvent INHERIT ControlNotifyEvent
	PROTECT nKey AS LONG		
	CONSTRUCTOR(oLV AS ListView, e AS System.Windows.Forms.KeyEventArgs) 
		SUPER(oLV)
		nKey := e:KeyCode
		RETURN 

	ACCESS KeyCode AS LONGINT STRICT 
		RETURN nKey

END CLASS

CLASS ListViewMouseEvent INHERIT ControlNotifyEvent
	PROTECT nButton AS System.Windows.Forms.MouseButtons
	PROTECT oLVI AS ListViewItem
	PROTECT oPoint AS Point
	PROTECT oInfo AS System.Windows.Forms.ListViewHitTestInfo

	ACCESS ButtonID AS LONGINT STRICT 
		IF nButton == System.Windows.Forms.MouseButtons.Left
			RETURN BUTTONLEFT
		ENDIF
		IF nButton == System.Windows.Forms.MouseButtons.Right
			RETURN BUTTONRIGHT
		ENDIF
		RETURN 0

	[DebuggerStepThrough];
	CONSTRUCTOR(oLv AS VOSDK.ListView, e AS System.Windows.Forms.MouseEventArgs) 
		SUPER(oLv)
		nButton := e:Button
		oPoint := e:Location
		oInfo  := oLv:__ListView:HitTest(e:Location)
		RETURN 

	ACCESS IsLeftButton AS LOGIC STRICT 
		RETURN	nButton == System.Windows.Forms.MouseButtons.Left

	ACCESS IsRightButton AS LOGIC STRICT 
		RETURN	nButton == System.Windows.Forms.MouseButtons.Right

	ACCESS ListViewItem AS ListViewItem STRICT 
		RETURN oLVI

	ACCESS PointAboveClientArea AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.AboveClientArea

	ACCESS PointBelowClientArea AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.BelowClientArea
	
	ACCESS PointLeftOfClientArea AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.LeftOfClientArea

	ACCESS PointNowhere AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.None
	
	ACCESS PointOnItem AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.Label .or. ;
		oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.Image .or. ;
		oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.StateImage
	

	ACCESS PointOnItemImage AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.Image

	ACCESS PointOnItemLabel AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.Label

	ACCESS PointOnItemStateImage AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.StateImage
	
	ACCESS PointRightOfClientArea AS LOGIC STRICT 
		RETURN oInfo:Location == System.Windows.Forms.ListViewHitTestLocations.RightOfClientArea
	
	ACCESS Position AS Point STRICT 
		RETURN oPoint

END CLASS
#endregion
