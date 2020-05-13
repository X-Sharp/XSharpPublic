// Window_Event_Handlers.prg
// Contains the 'empty' event handlers of the window class


#include "VOWin32APILibrary.vh"
#USING System.Windows.Forms

PARTIAL CLASS Window INHERIT @@EventContext

	METHOD Activate(oEvent) 
		RETURN SELF:Default(oEvent)
		
	METHOD AnimationStart(oControlEvent) 
		RETURN SELF:Default(oControlEvent)

	METHOD AnimationStop(oControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD AppCommand(oACEvent) 
		// FALSE means the message has not been processed, so it is passed on to windows so other default behaviour can occur
		RETURN FALSE

	METHOD ButtonClick(oControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD ButtonDoubleClick(oControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD ComboBoxExEndEdit(oComboBoxExEndEditEvent) 
		RETURN SELF:Default(oComboBoxExEndEditEvent)

	METHOD ControlFocusChange(oControlFocusChangeEvent AS  ControlFocusChangeEvent) AS USUAL STRICT
		RETURN SELF:Default(oControlFocusChangeEvent)

	METHOD EditChange(oControlEvent) 
		RETURN SELF:Default(oControlEvent)

	METHOD EditFocusChange(oEditFocusChangeEvent) 
		RETURN SELF:Default(oEditFocusChangeEvent)
	
	METHOD EditScroll(oControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD Expose(oExposeEvent) 
		RETURN SELF:Default(oExposeEvent)
	
	METHOD FocusChange(oFocusChangeEvent) 
		RETURN SELF:Default(oFocusChangeEvent)
	
	METHOD KeyDown(oKeyEvent) 
		RETURN SELF:Default(oKeyEvent)

	METHOD KeyUp(oKeyEvent) 
		RETURN SELF:Default(oKeyEvent)
	
	METHOD ListBoxClick(oControlEvent) 
		RETURN SELF:Default(oControlEvent)

	METHOD ListBoxSelect(oControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD ListViewColumnClick(oListViewColumnClickEvent) 
		RETURN SELF:Default(oListViewColumnClickEvent)
	
	METHOD ListViewItemChanged(oListViewItemEvent) 
		RETURN SELF:Default(oListViewItemEvent)
	

	METHOD ListViewItemChanging(oListViewItemEvent) 
		RETURN SELF:Default(oListViewItemEvent)

	METHOD ListViewItemDelete(oListViewDeleteEvent) 
		RETURN SELF:Default(oListViewDeleteEvent)
	
	METHOD ListViewItemEdit(oListViewEditEvent) 
		RETURN SELF:Default(oListViewEditEvent)
	
	METHOD ListViewKeyDown(oListViewKeyEvent) 
		RETURN SELF:Default(oListViewKeyEvent)
	
	METHOD ListViewMouseButtonDoubleClick(oListViewMouseEvent) 
		RETURN SELF:Default(oListViewMouseEvent)

	METHOD ListViewMouseButtonDown(oListViewMouseEvent) 
		RETURN SELF:Default(oListViewMouseEvent)
	

	METHOD MenuCommand(oMenuCommandEvent) 
		RETURN SELF:Default(oMenuCommandEvent)
	
	METHOD MenuInit(oMenuInitEvent) 
		RETURN SELF:Default(oMenuInitEvent)
	

	METHOD MenuSelect(oMenuSelectEvent) 
		RETURN SELF:Default(oMenuSelectEvent)
	
	METHOD MouseButtonDoubleClick(oMouseEvent) 
		RETURN SELF:Default(oMouseEvent)
	
	METHOD MouseButtonDown(oMouseEvent) 
		RETURN SELF:Default(oMouseEvent)
	
	METHOD MouseMove(oMouseEvent) 
		RETURN SELF:Default(oMouseEvent)

	METHOD Move(oMoveEvent) 
		RETURN SELF:Default(oMoveEvent)

	METHOD PreMenuCommand(oMenuCommandEvent) 
		RETURN FALSE
		
	METHOD QueryClose(oEvent) 
		RETURN TRUE


	METHOD RichEditProtected(oRichEditProtectEvent) 
		RETURN SELF:Default(oRichEditProtectEvent)

	METHOD RichEditSelectionChange(oRichEditSelectionEvent) 
		RETURN SELF:Default(oRichEditSelectionEvent)
	
	METHOD RichEditUndoLost(oControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)

	METHOD TabKeyDown(oControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)
	
	METHOD TabSelect(oControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)
	
	METHOD TabSelectionChanging(oControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)

	METHOD ToolBarHeightChanged(oControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)
	
	METHOD TrayIconBalloonClicked(dwID) 
		RETURN NIL

	METHOD TrayIconBalloonShown(dwID) 
		RETURN NIL

	METHOD TrayIconBalloonTimeOut(dwID) 
		RETURN NIL
	
	METHOD TrayIconClicked(dwID, lRightButton, lDoubleClick) 
		RETURN NIL

	METHOD TreeViewItemDelete(oTreeViewDeleteEvent) 
		RETURN SELF:Default(oTreeViewDeleteEvent)

	METHOD TreeViewItemEdit(oTreeViewEditEvent) 
		RETURN SELF:Default(oTreeViewEditEvent)
	

	METHOD TreeViewItemExpanded(oTreeViewExpandedEvent) 
		RETURN SELF:Default(oTreeViewExpandedEvent)
	

	METHOD TreeViewItemExpanding(oTreeViewExpandingEvent) 
		RETURN SELF:Default(oTreeViewExpandingEvent)
	
	METHOD TreeViewKeyDown(oTreeViewKeyEvent) 
		RETURN SELF:Default(oTreeViewKeyEvent)
	
	METHOD TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent) 
		RETURN SELF:Default(oTreeViewMouseEvent)

	METHOD TreeViewMouseButtonDown(oTreeViewMouseEvent) 
		RETURN SELF:Default(oTreeViewMouseEvent)
	

	METHOD TreeViewSelectionChanged(oTreeViewSelectionEvent) 
		RETURN SELF:Default(oTreeViewSelectionEvent)
	

	METHOD TreeViewSelectionChanging(oTreeViewSelectionEvent) 
		RETURN SELF:Default(oTreeViewSelectionEvent)
	


END CLASS
