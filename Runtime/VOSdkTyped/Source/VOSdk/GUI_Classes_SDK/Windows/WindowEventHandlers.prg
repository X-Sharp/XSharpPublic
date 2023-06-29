// Window_Event_Handlers.prg
// Contains the 'empty' event handlers of the window class


USING System.Windows.Forms

PARTIAL CLASS Window INHERIT @@EventContext

 


 
    METHOD ControlFocusChange(oControlFocusChangeEvent AS  ControlFocusChangeEvent) AS USUAL
        RETURN SELF:Default(oControlFocusChangeEvent)




 


 
 
 








    /// <include file="Gui.xml" path="doc/Window.RichEditProtected/*" />
    METHOD RichEditProtected(oRichEditProtectEvent AS RichEditProtectEvent)  AS USUAL
        RETURN SELF:Default(oRichEditProtectEvent)

    /// <include file="Gui.xml" path="doc/Window.RichEditSelectionChange/*" />
    METHOD RichEditSelectionChange(oRichEditSelectionEvent AS RichEditSelectionEvent)  AS USUAL
        RETURN SELF:Default(oRichEditSelectionEvent)

    /// <include file="Gui.xml" path="doc/Window.RichEditUndoLost/*" />
    METHOD RichEditUndoLost(oControlNotifyEvent AS ControlNotifyEvent)  AS USUAL
        RETURN SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TabKeyDown/*" />
    METHOD TabKeyDown(oControlNotifyEvent AS ControlNotifyEvent)  AS USUAL
        RETURN SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TabSelect/*" />
    METHOD TabSelect(oControlNotifyEvent AS ControlNotifyEvent)  AS USUAL
        RETURN SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TabSelectionChanging/*" />
    METHOD TabSelectionChanging(oControlNotifyEvent AS ControlNotifyEvent)  AS USUAL
        RETURN SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.ToolBarHeightChanged/*" />
    METHOD ToolBarHeightChanged(oControlNotifyEvent AS ControlNotifyEvent)  AS USUAL
        RETURN SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TrayIconBalloonClicked/*" />
    METHOD TrayIconBalloonClicked(dwID AS DWORD) AS USUAL
        RETURN

    /// <include file="Gui.xml" path="doc/Window.TrayIconBalloonShown/*" />
    METHOD TrayIconBalloonShown(dwID AS DWORD) AS USUAL
        RETURN

    /// <include file="Gui.xml" path="doc/Window.TrayIconBalloonTimeOut/*" />
    METHOD TrayIconBalloonTimeOut(dwID AS DWORD) AS USUAL
        RETURN

    /// <include file="Gui.xml" path="doc/Window.TrayIconClicked/*" />
    METHOD TrayIconClicked(dwID AS DWORD, lRightButton AS LOGIC, lDoubleClick AS LOGIC)  AS USUAL
        RETURN

    /// <include file="Gui.xml" path="doc/Window.TreeViewItemDelete/*" />
    METHOD TreeViewItemDelete(oTreeViewDeleteEvent AS TreeViewDeleteEvent)  AS USUAL
         RETURN SELF:Default(oTreeViewDeleteEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewItemEdit/*" />
    METHOD TreeViewItemEdit(oTreeViewEditEvent AS TreeViewEditEvent) AS USUAL
         RETURN SELF:Default(oTreeViewEditEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewItemExpanded/*" />
    METHOD TreeViewItemExpanded(oTreeViewExpandedEvent AS TreeViewExpandedEvent) AS USUAL
         RETURN SELF:Default(oTreeViewExpandedEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewItemExpanding/*" />
    METHOD TreeViewItemExpanding(oTreeViewExpandingEvent AS TreeViewExpandingEvent) AS USUAL
         RETURN SELF:Default(oTreeViewExpandingEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewKeyDown/*" />
    METHOD TreeViewKeyDown(oTreeViewKeyEvent AS TreeViewKeyEvent) AS USUAL
         RETURN SELF:Default(oTreeViewKeyEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewMouseButtonDoubleClick/*" />
    METHOD TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent AS TreeViewMouseEvent) AS USUAL
         RETURN SELF:Default(oTreeViewMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewMouseButtonDown/*" />
    METHOD TreeViewMouseButtonDown(oTreeViewMouseEvent AS TreeViewMouseEvent) AS USUAL
         RETURN SELF:Default(oTreeViewMouseEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewSelectionChanged/*" />
    METHOD TreeViewSelectionChanged(oTreeViewSelectionEvent AS TreeViewSelectionEvent) AS USUAL
         RETURN SELF:Default(oTreeViewSelectionEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewSelectionChanging/*" />
    METHOD TreeViewSelectionChanging(oTreeViewSelectionEvent AS TreeViewSelectionEvent) AS USUAL
         RETURN SELF:Default(oTreeViewSelectionEvent)



END CLASS
