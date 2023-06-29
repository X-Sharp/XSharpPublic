// Window_Event_Handlers.prg
// Contains the 'empty' event handlers of the window class


USING System.Windows.Forms

PARTIAL CLASS Window INHERIT @@EventContext

    /// <include file="Gui.xml" path="doc/Window.Activate/*" />
    METHOD Activate(oEvent  AS Event) as void
        SELF:Default(oEvent)

    /// <include file="Gui.xml" path="doc/Window.AnimationStart/*" />
    METHOD AnimationStart(oControlEvent AS ControlEvent) as void
        SELF:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.AnimationStop/*" />
    METHOD AnimationStop(oControlEvent AS ControlEvent)as void
        SELF:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.AppCommand/*" />
    METHOD AppCommand(oACEvent)
        // FALSE means the message has not been processed, so it is passed on to windows so other default behaviour can occur
        RETURN FALSE

    /// <include file="Gui.xml" path="doc/Window.ButtonClick/*" />
    METHOD ButtonClick(oControlEvent AS ControlEvent) as void
        SELF:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.ButtonDoubleClick/*" />
    METHOD ButtonDoubleClick(oControlEvent AS ControlEvent) as void
        SELF:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.ComboBoxExEndEdit/*" />
    METHOD ComboBoxExEndEdit(oComboBoxExEndEditEvent AS ComboBoxExEndEditEvent) as void
        SELF:Default(oComboBoxExEndEditEvent)

    METHOD ControlFocusChange(oControlFocusChangeEvent AS  ControlFocusChangeEvent) AS void
        SELF:Default(oControlFocusChangeEvent)

    /// <include file="Gui.xml" path="doc/Window.EditChange/*" />
    METHOD EditChange(oControlEvent AS ControlEvent) as void
        SELF:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.EditFocusChange/*" />
    METHOD EditFocusChange(oEditFocusChangeEvent AS EditFocusChangeEvent) as void
        SELF:Default(oEditFocusChangeEvent)

    /// <include file="Gui.xml" path="doc/Window.EditScroll/*" />
    METHOD EditScroll(oControlEvent AS ControlEvent) as void
        SELF:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.Expose/*" />
    METHOD Expose(oExposeEvent AS ExposeEvent) as void
        SELF:Default(oExposeEvent)

    /// <include file="Gui.xml" path="doc/Window.FocusChange/*" />
    METHOD FocusChange(oFocusChangeEvent AS FocusChangeEvent) as void
        SELF:Default(oFocusChangeEvent)

    /// <include file="Gui.xml" path="doc/Window.KeyDown/*" />
    METHOD KeyDown(oKeyEvent AS KeyEvent) as void
        SELF:Default(oKeyEvent)

    /// <include file="Gui.xml" path="doc/Window.KeyUp/*" />
    METHOD KeyUp(oKeyEvent AS KeyEvent) as void
        SELF:Default(oKeyEvent)

    /// <include file="Gui.xml" path="doc/Window.ListBoxClick/*" />
    METHOD ListBoxClick(oControlEvent AS ControlEvent) AS VOID
        SELF:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.ListBoxSelect/*" />
    METHOD ListBoxSelect(oControlEvent  AS ControlEvent) AS VOID
        SELF:Default(oControlEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewColumnClick/*" />
    METHOD ListViewColumnClick(oListViewColumnClickEvent AS ListViewColumnClickEvent) AS VOID
        SELF:Default(oListViewColumnClickEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewItemChanged/*" />
    METHOD ListViewItemChanged(oListViewItemEvent AS ListViewItemEvent) AS VOID
        SELF:Default(oListViewItemEvent)


    /// <include file="Gui.xml" path="doc/Window.ListViewItemChanging/*" />
    METHOD ListViewItemChanging(oListViewItemEvent AS ListViewItemEvent) AS VOID
        SELF:Default(oListViewItemEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewItemDelete/*" />
    METHOD ListViewItemDelete(oListViewDeleteEvent AS ListViewDeleteEvent)  AS VOID
        SELF:Default(oListViewDeleteEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewItemEdit/*" />
    METHOD ListViewItemEdit(oListViewEditEvent AS ListViewEditEvent) AS VOID
         SELF:Default(oListViewEditEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewKeyDown/*" />
    METHOD ListViewKeyDown(oListViewKeyEvent AS ListViewKeyEvent) AS VOID
         SELF:Default(oListViewKeyEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewMouseButtonDoubleClick/*" />
    METHOD ListViewMouseButtonDoubleClick(oListViewMouseEvent AS ListViewMouseEvent) AS VOID
         SELF:Default(oListViewMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.ListViewMouseButtonDown/*" />
    METHOD ListViewMouseButtonDown(oListViewMouseEvent AS ListViewMouseEvent) AS VOID
         SELF:Default(oListViewMouseEvent)


    /// <include file="Gui.xml" path="doc/Window.MenuCommand/*" />
    METHOD MenuCommand(oMenuCommandEvent AS MenuCommandEvent) AS VOID
        SELF:Default(oMenuCommandEvent)

    /// <include file="Gui.xml" path="doc/Window.MenuInit/*" />
    METHOD MenuInit(oMenuInitEvent AS MenuInitEvent) AS VOID
        SELF:Default(oMenuInitEvent)


    /// <include file="Gui.xml" path="doc/Window.MenuSelect/*" />
    METHOD MenuSelect(oMenuSelectEvent AS MenuSelectEvent) AS VOID
        SELF:Default(oMenuSelectEvent)

    /// <include file="Gui.xml" path="doc/Window.MouseButtonDoubleClick/*" />
    METHOD MouseButtonDoubleClick(oMouseEvent AS MouseEvent) AS VOID
        SELF:Default(oMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.MouseButtonDown/*" />
    METHOD MouseButtonDown(oMouseEvent  AS MouseEvent) AS VOID
        SELF:Default(oMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.MouseButtonUp/*" />
    METHOD MouseMove(oMouseEvent AS MouseEvent) AS VOID
        SELF:Default(oMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.Move/*" />

    METHOD Move(oMoveEvent AS MoveEvent)AS VOID
        SELF:Default(oMoveEvent)

    METHOD PreMenuCommand(oMenuCommandEvent AS MenuCommandEvent) AS LOGIC
        RETURN FALSE

    /// <include file="Gui.xml" path="doc/Window.QueryClose/*" />
    METHOD QueryClose(oEvent AS Event) AS LOGIC
        RETURN TRUE


    /// <include file="Gui.xml" path="doc/Window.RichEditProtected/*" />
    METHOD RichEditProtected(oRichEditProtectEvent AS RichEditProtectEvent)  AS VOID
        SELF:Default(oRichEditProtectEvent)

    /// <include file="Gui.xml" path="doc/Window.RichEditSelectionChange/*" />
    METHOD RichEditSelectionChange(oRichEditSelectionEvent AS RichEditSelectionEvent)  AS VOID
        SELF:Default(oRichEditSelectionEvent)

    /// <include file="Gui.xml" path="doc/Window.RichEditUndoLost/*" />
    METHOD RichEditUndoLost(oControlNotifyEvent AS ControlNotifyEvent)  AS VOID
        SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TabKeyDown/*" />
    METHOD TabKeyDown(oControlNotifyEvent AS ControlNotifyEvent)  AS VOID
        SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TabSelect/*" />
    METHOD TabSelect(oControlNotifyEvent AS ControlNotifyEvent)  AS VOID
        SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TabSelectionChanging/*" />
    METHOD TabSelectionChanging(oControlNotifyEvent AS ControlNotifyEvent)  AS VOID
        SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.ToolBarHeightChanged/*" />
    METHOD ToolBarHeightChanged(oControlNotifyEvent AS ControlNotifyEvent)  AS VOID
        SELF:Default(oControlNotifyEvent)

    /// <include file="Gui.xml" path="doc/Window.TrayIconBalloonClicked/*" />
    METHOD TrayIconBalloonClicked(dwID AS DWORD) AS VOID
        RETURN

    /// <include file="Gui.xml" path="doc/Window.TrayIconBalloonShown/*" />
    METHOD TrayIconBalloonShown(dwID AS DWORD) AS VOID
        RETURN

    /// <include file="Gui.xml" path="doc/Window.TrayIconBalloonTimeOut/*" />
    METHOD TrayIconBalloonTimeOut(dwID AS DWORD) AS VOID
        RETURN

    /// <include file="Gui.xml" path="doc/Window.TrayIconClicked/*" />
    METHOD TrayIconClicked(dwID AS DWORD, lRightButton AS LOGIC, lDoubleClick AS LOGIC)  AS VOID
        RETURN

    /// <include file="Gui.xml" path="doc/Window.TreeViewItemDelete/*" />
    METHOD TreeViewItemDelete(oTreeViewDeleteEvent AS TreeViewDeleteEvent)  AS VOID
         SELF:Default(oTreeViewDeleteEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewItemEdit/*" />
    METHOD TreeViewItemEdit(oTreeViewEditEvent AS TreeViewEditEvent) AS VOID
         SELF:Default(oTreeViewEditEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewItemExpanded/*" />
    METHOD TreeViewItemExpanded(oTreeViewExpandedEvent AS TreeViewExpandedEvent) AS VOID
         SELF:Default(oTreeViewExpandedEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewItemExpanding/*" />
    METHOD TreeViewItemExpanding(oTreeViewExpandingEvent AS TreeViewExpandingEvent) AS VOID
         SELF:Default(oTreeViewExpandingEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewKeyDown/*" />
    METHOD TreeViewKeyDown(oTreeViewKeyEvent AS TreeViewKeyEvent) AS VOID
         SELF:Default(oTreeViewKeyEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewMouseButtonDoubleClick/*" />
    METHOD TreeViewMouseButtonDoubleClick(oTreeViewMouseEvent AS TreeViewMouseEvent) AS VOID
         SELF:Default(oTreeViewMouseEvent)

    /// <include file="Gui.xml" path="doc/Window.TreeViewMouseButtonDown/*" />
    METHOD TreeViewMouseButtonDown(oTreeViewMouseEvent AS TreeViewMouseEvent) AS VOID
         SELF:Default(oTreeViewMouseEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewSelectionChanged/*" />
    METHOD TreeViewSelectionChanged(oTreeViewSelectionEvent AS TreeViewSelectionEvent) AS VOID
         SELF:Default(oTreeViewSelectionEvent)


    /// <include file="Gui.xml" path="doc/Window.TreeViewSelectionChanging/*" />
    METHOD TreeViewSelectionChanging(oTreeViewSelectionEvent AS TreeViewSelectionEvent) AS VOID
         SELF:Default(oTreeViewSelectionEvent)



END CLASS
