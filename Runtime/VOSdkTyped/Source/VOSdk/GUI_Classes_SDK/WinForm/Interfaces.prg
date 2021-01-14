// Interfaces.prg
// This file defgines a couple of interfaces that are used in the VO Compatible Unicode GUI Classes

USING VOSDK := XSharp.VO.SDK
USING SWF   := System.Windows.Forms
USING SD    := System.Drawing
USING System.Collections.Generic


INTERFACE IVOUIObject
    PROPERTY ClientRectangle AS SD.Rectangle GET
    PROPERTY DisplayRectangle AS SD.Rectangle GET 
    PROPERTY Dock    AS SWF.DockStyle GET SET
    PROPERTY Height AS LONG GET SET
    PROPERTY IsDisposed AS LOGIC GET
    PROPERTY Location  AS SD.Point GET SET
    PROPERTY Size AS SD.Size GET SET
    PROPERTY Text AS STRING GET SET
    PROPERTY Visible  AS LOGIC GET SET
    METHOD PerformLayout AS VOID STRICT
    METHOD ResumeLayout AS VOID STRICT
    METHOD ResumeLayout(performLayout AS LOGIC) AS VOID STRICT
    METHOD SuspendLayout AS VOID STRICT
END INTERFACE    


INTERFACE IVOControlProperties
	PROPERTY Control            AS VOSDK.Control GET
	PROPERTY ControlProperties  AS VOControlProperties GET
	METHOD SetVisualStyle AS VOID STRICT
	METHOD SetOwner(Owner AS VOSDK.Control) AS VOID
END INTERFACE

/// <summary>
/// This interface is defined for all VO..Control classes 
/// </summary>
INTERFACE IVOControl INHERIT IVOUIObject
	// Properties and methods implemented in SWF.Control that we depend on
    PROPERTY AccessibleDescription AS STRING GET SET
    PROPERTY AccessibleName AS STRING GET SET
    PROPERTY Anchor AS SWF.AnchorStyles GET SET
    PROPERTY BackColor AS SD.Color GET SET
    PROPERTY Controls AS SWF.Control.ControlCollection GET         
    PROPERTY Cursor AS SWF.Cursor GET SET
    PROPERTY Enabled AS LOGIC GET SET
    PROPERTY Focused AS LOGIC GET 
        
    PROPERTY Font AS SD.Font GET SET
    PROPERTY ForeColor AS SD.Color GET SET
    PROPERTY Handle AS IntPtr GET
    PROPERTY IsHandleCreated AS LOGIC GET
    PROPERTY Margin AS SWF.Padding GET SET
    PROPERTY Parent AS SWF.Control GET
    PROPERTY TabIndex AS LONG GET SET
    PROPERTY TabStop  AS LOGIC GET SET
    PROPERTY Tag  AS OBJECT GET SET
        
    METHOD BringToFront() AS VOID STRICT
    METHOD Focus() AS LOGIC STRICT
    METHOD Invalidate() AS VOID STRICT
    METHOD SendToBack() AS VOID STRICT
    METHOD Show() AS VOID STRICT

    EVENT Click  AS EventHandler
    EVENT GotFocus  AS EventHandler
    EVENT HandleCreated   AS EventHandler
    EVENT HandleDestroyed AS EventHandler
    EVENT KeyDown AS SWF.KeyEventHandler
    EVENT KeyPress AS SWF.KeyPressEventHandler
    EVENT KeyUp AS SWF.KeyEventHandler
    EVENT MouseClick AS SWF.MouseEventHandler    
    EVENT PreviewKeyDown AS SWF.PreviewKeyDownEventHandler
    EVENT Resize AS EventHandler
    EVENT VisibleChanged AS EventHandler

END INTERFACE

INTERFACE IVOControlInitialize INHERIT IVOControlProperties
	METHOD Initialize AS VOID STRICT
END INTERFACE


INTERFACE IVOControlContainer INHERIT IVOUIObject
    PROPERTY AllowDrop AS LOGIC GET SET
    PROPERTY BackColor AS SD.Color GET SET
    PROPERTY Controls AS SWF.Control.ControlCollection GET         
    METHOD AddControl(oCtrl AS IVOControl) AS VOID
    METHOD SetChildIndex(oCtrl AS IVOControl, nIndex AS LONG) AS VOID
        
END INTERFACE    


INTERFACE IVOPanel INHERIT IVOControl,IVOControlContainer
    PROPERTY AutoScroll AS LOGIC GET SET
    METHOD CleanUp() AS VOID STRICT
    METHOD Dispose() AS VOID STRICT
    METHOD Prepare() AS VOID STRICT
END INTERFACE

INTERFACE IVOFramePanel INHERIT IVOPanel
    PROPERTY DefinedSize    AS SD.Size GET SET
    PROPERTY IsSubForm      AS LOGIC GET SET
END INTERFACE    


INTERFACE IVOForm INHERIT IVOControlContainer, IVOUIObject
	PROPERTY Window		AS VOSDK.Window GET
	PROPERTY Properties AS VOFormProperties GET
END INTERFACE

/// <summary>
/// This interface defines a couple of common properties and methods  for Controls and Windows 
/// </summary>
INTERFACE IGuiObject
	PROPERTY Origin		AS Point GET SET
	PROPERTY Size		AS Dimension GET SET
	PROPERTY Caption	AS STRING GET SET
	PROPERTY HyperLabel AS HyperLabel GET SET
	PROPERTY NameSym	AS SYMBOL GET
    PROPERTY __Handle   AS IntPtr GET
	METHOD   Destroy()	AS USUAL CLIPPER
    METHOD   Show()     AS VOID STRICT
    METHOD   Hide()     AS VOID STRICT
    METHOD   SetFocus() AS VOID STRICT
        
END INTERFACE





/// <summary>
/// This interface defines a couple of properties and methods for 'owners' of Controls, such as the window class
/// </summary>
INTERFACE IControlParent
	PROPERTY __Handle AS IntPtr  GET
	PROPERTY __Form AS VOForm GET

    METHOD __AddTool(oControl AS Control) AS LOGIC STRICT
	METHOD __ShowToolTip(oControl AS Control) AS VOID STRICT
	METHOD __AddAlign(oControl AS IGuiObject, iType AS USUAL) AS LOGIC  STRICT
	METHOD ControlFocusChange(oControlFocusChangeEvent AS  ControlFocusChangeEvent) AS USUAL STRICT
	METHOD __SetupDataControl(oDC AS Control) AS VOID
	METHOD GetDlgItem(nID as LONG) as ResourceDialogItem
END INTERFACE	





INTERFACE IVOButton INHERIT IVOControl, IVOControlInitialize
    PROPERTY DefaultButton AS LOGIC GET 
    PROPERTY FlatStyle AS SWF.FlatStyle  GET SET
    PROPERTY Image AS SD.Image GET SET
END INTERFACE    


INTERFACE IVOCheckBox INHERIT IVOControl, IVOControlInitialize
    PROPERTY Checked AS LOGIC GET SET
    PROPERTY FlatStyle AS SWF.FlatStyle  GET SET
    PROPERTY Image AS SD.Image GET SET
END INTERFACE



INTERFACE IVORadioButton INHERIT IVOControl, IVOControlInitialize
    PROPERTY Checked AS LOGIC GET SET
    PROPERTY FlatStyle AS SWF.FlatStyle  GET SET
    PROPERTY Image AS SD.Image GET SET
END INTERFACE

INTERFACE IVOGroupBox INHERIT IVOControl, IVOControlContainer
    PROPERTY IsRadioGroup AS LOGIC GET SET
    METHOD GetAllChildren(aChildren AS IList<IVOControl>) AS IList<IVOControl>
END INTERFACE



INTERFACE IVOLabel INHERIT IVOControl, IVOControlInitialize
    PROPERTY FlatStyle AS SWF.FlatStyle  GET SET
    PROPERTY Image AS SD.Image GET SET
END INTERFACE    




INTERFACE IVOLinkLabel INHERIT IVOLabel
    PROPERTY Links AS SWF.LinkLabel.LinkCollection GET
    EVENT LinkClicked AS SWF.LinkLabelLinkClickedEventHandler
END INTERFACE    






INTERFACE IVOProgressBar INHERIT IVOControl, IVOControlProperties
    PROPERTY Minimum AS LONG GET SET
    PROPERTY Maximum AS LONG GET SET
    PROPERTY Step AS LONG GET SET
    PROPERTY Value AS LONG GET SET
    METHOD PerformStep AS VOID STRICT
END INTERFACE    

INTERFACE IVOScrollBar INHERIT IVOControl, IVOControlProperties
END INTERFACE  


INTERFACE IVOSlider INHERIT IVOControl, IVOControlProperties
END INTERFACE  

INTERFACE IVOStatusBar INHERIT IVOControl, IVOControlProperties
    PROPERTY CanOverflow        AS LOGIC GET SET
    PROPERTY Items              AS SWF.ToolStripItemCollection GET 
    PROPERTY ShowItemToolTips   AS LOGIC GET SET
    PROPERTY Stretch            AS LOGIC GET SET
        
END INTERFACE  



INTERFACE IVODateTimePicker INHERIT IVOControl, IVOControlProperties
    PROPERTY Checked        AS LOGIC    GET SET
    PROPERTY MinDate        AS DateTime GET SET
    PROPERTY MaxDate        AS DateTime GET SET
    PROPERTY ShowCheckBox   AS LOGIC    GET SET
    PROPERTY Value          AS DateTime GET SET
    PROPERTY CustomFormat   AS STRING GET SET
    PROPERTY Format         AS SWF.DateTimePickerFormat GET SET
    PROPERTY CalendarFont                   AS SD.Font GET SET
    PROPERTY CalendarForeColor              AS SD.Color GET SET
    PROPERTY CalendarMonthBackground        AS SD.Color GET SET
    PROPERTY CalendarTitleBackColor         AS SD.Color GET SET
    PROPERTY CalendarTitleForeColor         AS SD.Color GET SET
    PROPERTY CalendarTrailingForeColor      AS SD.Color GET SET
END INTERFACE  


INTERFACE IVOMonthCalendar INHERIT IVOControl, IVOControlProperties
    PROPERTY FirstDayOfWeek         AS SWF.Day GET SET
    PROPERTY MinDate                AS DateTime GET SET
    PROPERTY MaxDate                AS DateTime GET SET
    PROPERTY TodayDate              AS DateTime GET SET
    PROPERTY SelectionStart         AS DateTime GET SET
    PROPERTY SelectionEnd           AS DateTime GET SET
    PROPERTY MaxSelectionCount      AS LONG GET SET
    PROPERTY TitleBackColor         AS SD.Color GET SET
    PROPERTY TitleForeColor         AS SD.Color GET SET
    PROPERTY TrailingForeColor      AS SD.Color GET SET
END INTERFACE  


INTERFACE IVOListView INHERIT IVOControl, IVOControlInitialize
    PROPERTY AllowColumnReorder  AS LOGIC GET SET
    PROPERTY CheckBoxes AS LOGIC GET SET
    PROPERTY Columns    AS SWF.ListView.ColumnHeaderCollection GET
    PROPERTY FullRowSelect AS LOGIC GET SET
    PROPERTY GridLines  AS LOGIC GET SET
    PROPERTY LargeImageList AS SWF.ImageList GET SET
    PROPERTY HeaderStyle AS SWF.ColumnHeaderStyle GET SET
    PROPERTY HotTracking AS LOGIC GET SET
    PROPERTY Groups     AS SWF.ListViewGroupCollection         GET
    PROPERTY Items      AS SWF.ListView.ListViewItemCollection GET
    PROPERTY ListViewItemSorter AS System.Collections.IComparer GET SET
    PROPERTY SelectedItems AS SWF.ListView.SelectedListViewItemCollection GET
    PROPERTY SelectedIndices AS SWF.ListView.SelectedIndexCollection GET
    PROPERTY ShowGroups AS LOGIC GET SET
    PROPERTY SmallImageList AS SWF.ImageList GET SET
    PROPERTY Sorting    AS SWF.SortOrder GET SET
    PROPERTY StateImageList AS SWF.ImageList GET SET
    PROPERTY TopItem    AS SWF.ListViewItem GET SET
    PROPERTY View       AS SWF.View GET SET
    PROPERTY VirtualListSize AS LONG GET SET
    PROPERTY VirtualMode AS LOGIC GET SET
    
    METHOD ArrangeIcons AS VOID STRICT
    METHOD ArrangeIcons(alignments AS System.Windows.Forms.ListViewAlignment) AS VOID
    METHOD BeginUpdate AS VOID STRICT
    METHOD EndUpdate AS VOID STRICT
    METHOD EnsureVisible(nItem AS LONG) AS VOID
    METHOD FindNearestItem(hint AS SWF.SearchDirectionHint, point AS SD.Point) AS SWF.ListViewItem 
    METHOD FindItemWithText(text AS STRING, subitems AS LOGIC, startIndex AS LONG, partial AS LOGIC) AS SWF.ListViewItem
    METHOD HitTest (oPoint AS SD.Point) AS SWF.ListViewHitTestInfo
    METHOD RedrawItems(startIndex AS LONG, endIndex AS LONG, invalidateOnly AS LOGIC) AS VOID
    METHOD Sort() AS VOID STRICT
END INTERFACE  



INTERFACE IVOTextBox INHERIT IVOControl, IVOControlInitialize
   PROPERTY CanUndo AS LOGIC GET
   PROPERTY MaxLength AS LONG GET SET
   PROPERTY Modified AS LOGIC GET SET
   PROPERTY ReadOnly AS LOGIC GET SET
   PROPERTY SelectedText AS STRING GET SET
   PROPERTY SelectionStart AS LONG GET SET
   PROPERTY SelectionLength AS LONG GET SET
   PROPERTY TextAlign AS SWF.HorizontalAlignment GET SET
   PROPERTY TextLength AS LONG GET 
   PROPERTY UseSystemPasswordChar AS LOGIC GET SET
   METHOD Copy() AS VOID STRICT
   METHOD Clear() AS VOID STRICT
   METHOD Cut() AS VOID STRICT
   METHOD Paste() AS VOID STRICT
   METHOD Undo() AS VOID STRICT
END INTERFACE    



INTERFACE IVOHotKeyTextBox INHERIT IVOTextBox
END INTERFACE    


INTERFACE IVOMLETextBox INHERIT IVOTextBox
    PROPERTY Lines AS STRING[] GET
    METHOD GetLineFromCharIndex(nIndex AS LONG) AS LONG
END INTERFACE    


INTERFACE IVOIPAddressTextBox INHERIT IVOTextBox
END INTERFACE


INTERFACE IVOSpinnerTextBox  INHERIT IVOControl
	PROPERTY Hexadecimal AS LOGIC GET SET
	PROPERTY Maximum    AS DECIMAL GET SET 
	PROPERTY Minimum	AS DECIMAL GET SET
	PROPERTY Value  	AS DECIMAL GET SET
    
END INTERFACE    
