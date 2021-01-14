// Window_Event_Handlers.prg
// Contains the 'empty' event handlers of the window class


#USING System.Windows.Forms

PARTIAL CLASS Window INHERIT @@EventContext

	METHOD Activate(oEvent  AS Event) 
		RETURN SELF:Default(oEvent)
		
	METHOD AnimationStart(oControlEvent AS ControlEvent) 
		RETURN SELF:Default(oControlEvent)

	METHOD AnimationStop(oControlEvent AS ControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD AppCommand(oACEvent) 
		// FALSE means the message has not been processed, so it is passed on to windows so other default behaviour can occur
		RETURN FALSE

	METHOD ButtonClick(oControlEvent AS ControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD ButtonDoubleClick(oControlEvent AS ControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD ComboBoxExEndEdit(oComboBoxExEndEditEvent AS ComboBoxExEndEditEvent) 
		RETURN SELF:Default(oComboBoxExEndEditEvent)

	METHOD ControlFocusChange(oControlFocusChangeEvent AS  ControlFocusChangeEvent) AS USUAL STRICT
		RETURN SELF:Default(oControlFocusChangeEvent)

	METHOD EditChange(oControlEvent AS ControlEvent) 
		RETURN SELF:Default(oControlEvent)

	METHOD EditFocusChange(oEditFocusChangeEvent AS EditFocusChangeEvent) 
		RETURN SELF:Default(oEditFocusChangeEvent)
	
	METHOD EditScroll(oControlEvent AS ControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD Expose(oExposeEvent AS ExposeEvent) 
		RETURN SELF:Default(oExposeEvent)
	
	METHOD FocusChange(oFocusChangeEvent AS FocusChangeEvent) 
		RETURN SELF:Default(oFocusChangeEvent)
	
	METHOD KeyDown(oKeyEvent AS KeyEvent) 
		RETURN SELF:Default(oKeyEvent)

	METHOD KeyUp(oKeyEvent AS KeyEvent) 
		RETURN SELF:Default(oKeyEvent)
	
	METHOD ListBoxClick(oControlEvent AS ControlEvent) 
		RETURN SELF:Default(oControlEvent)

	METHOD ListBoxSelect(oControlEvent  AS ControlEvent) 
		RETURN SELF:Default(oControlEvent)
	
	METHOD ListViewColumnClick(oListViewColumnClickEvent AS ListViewColumnClickEvent) 
		RETURN SELF:Default(oListViewColumnClickEvent)
	
	METHOD ListViewItemChanged(oListViewItemEvent AS ListViewItemEvent) 
		RETURN SELF:Default(oListViewItemEvent)
	

	METHOD ListViewItemChanging(oListViewItemEvent AS ListViewItemEvent) 
		RETURN SELF:Default(oListViewItemEvent)

	METHOD ListViewItemDelete(oListViewDeleteEvent AS ListViewDeleteEvent) 
		RETURN SELF:Default(oListViewDeleteEvent)
	
	METHOD ListViewItemEdit(oListViewEditEvent AS ListViewEditEvent) 
		RETURN SELF:Default(oListViewEditEvent)
	
	METHOD ListViewKeyDown(oListViewKeyEvent AS ListViewKeyEvent) 
		RETURN SELF:Default(oListViewKeyEvent)
	
	METHOD ListViewMouseButtonDoubleClick(oListViewMouseEvent AS ListViewMouseEvent) 
		RETURN SELF:Default(oListViewMouseEvent)

	METHOD ListViewMouseButtonDown(oListViewMouseEvent AS ListViewMouseEvent) 
		RETURN SELF:Default(oListViewMouseEvent)
	

	METHOD MenuCommand(oMenuCommandEvent AS MenuCommandEvent) 
		RETURN SELF:Default(oMenuCommandEvent)
	
	METHOD MenuInit(oMenuInitEvent AS MenuInitEvent) 
		RETURN SELF:Default(oMenuInitEvent)
	

	METHOD MenuSelect(oMenuSelectEvent AS MenuSelectEvent) 
		RETURN SELF:Default(oMenuSelectEvent)
	
	METHOD MouseButtonDoubleClick(oMouseEvent AS MouseEvent) 
		RETURN SELF:Default(oMouseEvent)
	
	METHOD MouseButtonDown(oMouseEvent  AS MouseEvent) 
		RETURN SELF:Default(oMouseEvent)
	
	METHOD MouseMove(oMouseEvent AS MouseEvent) 
		RETURN SELF:Default(oMouseEvent)

	METHOD Move(oMoveEvent AS MoveEvent) 
		RETURN SELF:Default(oMoveEvent)

	METHOD PreMenuCommand(oMenuCommandEvent AS MenuCommandEvent) 
		RETURN FALSE
		
	METHOD QueryClose(oEvent AS Event) AS LOGIC
		RETURN TRUE


	METHOD RichEditProtected(oRichEditProtectEvent AS RichEditProtectEvent) 
		RETURN SELF:Default(oRichEditProtectEvent)

	METHOD RichEditSelectionChange(oRichEditSelectionEvent AS RichEditSelectionEvent) 
		RETURN SELF:Default(oRichEditSelectionEvent)
	
	METHOD RichEditUndoLost(oControlNotifyEvent AS ControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)

	METHOD TabKeyDown(oControlNotifyEvent AS ControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)
	
	METHOD TabSelect(oControlNotifyEvent AS ControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)
	
	METHOD TabSelectionChanging(oControlNotifyEvent AS ControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)

	METHOD ToolBarHeightChanged(oControlNotifyEvent AS ControlNotifyEvent) 
		RETURN SELF:Default(oControlNotifyEvent)
	
	METHOD TrayIconBalloonClicked(dwID AS DWORD) 
		RETURN NIL

	METHOD TrayIconBalloonShown(dwID AS DWORD) 
		RETURN NIL

	METHOD TrayIconBalloonTimeOut(dwID AS DWORD) 
		RETURN NIL
	
	METHOD TrayIconClicked(dwID AS DWORD, lRightButton AS LOGIC, lDoubleClick AS LOGIC) 
		RETURN NIL

	METHOD TreeViewItemDelete(oTreeViewDeleteEvent AS TreeViewDeleteEvent) 
		RETURN SELF:Default(oTreeViewDeleteEvent)

	METHOD TreeViewItemEdit(oTreeViewEditEvent AS TreeViewEditEvent) 
		RETURN SELF:Default(oTreeViewEditEvent)
	

	METHOD TreeViewItemExpanded(oTreeViewExpandedEvent AS TreeViewExpandedEvent) 
		RETURN SELF:Default(oTreeViewExpandedEvent)
	

	METHOD TreeViewItemExpanding(oTreeViewExpandingEvent AS TreeViewExpandingEvent) 
		RETURN SELF:Default(oTreeViewExpandingEvent)
	
	METHOD TreeViewKeyDown(oTreeViewKeyEvent AS TreeViewKeyEvent) 
		RETURN SELF:Default(oTreeViewKeyEvent)
	
	METHOD TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent AS TreeViewMouseEvent) 
		RETURN SELF:Default(oTreeViewMouseEvent)

	METHOD TreeViewMouseButtonDown(oTreeViewMouseEvent AS TreeViewMouseEvent) 
		RETURN SELF:Default(oTreeViewMouseEvent)
	

	METHOD TreeViewSelectionChanged(oTreeViewSelectionEvent AS TreeViewSelectionEvent) 
		RETURN SELF:Default(oTreeViewSelectionEvent)
	

	METHOD TreeViewSelectionChanging(oTreeViewSelectionEvent AS TreeViewSelectionEvent) 
		RETURN SELF:Default(oTreeViewSelectionEvent)
	


END CLASS
