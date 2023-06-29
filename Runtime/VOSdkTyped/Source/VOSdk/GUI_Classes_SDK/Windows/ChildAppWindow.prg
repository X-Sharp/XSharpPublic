
USING System.Windows.Forms
USING VOSDK := XSharp.VO.SDK
/// <include file="Gui.xml" path="doc/ChildAppWindow/*" />
CLASS ChildAppWindow INHERIT AppWindow
    PROTECT oShell AS ShellWindow

    /// <exclude />
    METHOD __CreateForm() AS VOForm STRICT
        LOCAL oChild AS VOForm
        IF oShell != NULL_OBJECT
            oChild := GuiFactory.Instance:CreateChildAppWindow(SELF, oShell:__Form)
        ELSE
            oChild := GuiFactory.Instance:CreateChildAppWindow(SELF, NULL_OBJECT)
        ENDIF
        RETURN oChild

    /// <include file="Gui.xml" path="doc/ChildAppWindow.ctor/*" />
    CONSTRUCTOR(oOwner, lManaged, lImpl)
        LOCAL lMng AS LOGIC
        IF oOwner IS ShellWindow // create an MDI child
            oShell := oOwner
        ENDIF

        SUPER(oOwner)
        IF !IsNil(lManaged)
            IF !IsLogic(lManaged)
                WCError{#Init,#ChildAppWindow,__WCSTypeError,lManaged,2}:Throw()
            ELSE
                lMng := lManaged
            ENDIF
        ENDIF
        Default(@lImpl, TRUE)
        IF (lImpl)
            //IF (oImp == NULL_OBJECT)
            IF lMng .AND. IsInstanceOf(oParent,#ShellWindow) // create an MDI child
                SELF:EnableSystemMenu()
                SELF:EnableBorder()
                SELF:EnableMinBox()
                SELF:EnableMaxBox()
            ELSE // create an SDI window or a Child window
                //SE-070904 no clientedge if oParent:Owner of a __FormFrame is a DialogWindow
                //__WindApp{SELF, IsInstanceOf(SELF, #__FormFrame) .AND. IsInstanceOf(oParent, #DataWindow)}
                //lClientEdge := IsInstanceOf(SELF, #__FormFrame) .AND. IsInstanceOf(oParent, #DataWindow) .AND. ! IsInstanceOf(oParent:Owner, #DialogWindow)
                //__WindApp{SELF, lClientEdge }
                //IF lManaged
                //SELF:EnableBorder(WindowNonSizingBorder)
                //ENDIF
            ENDIF
            //ENDIF
        ENDIF

    /// <include file="Gui.xml" path="doc/ChildAppWindow.Menu/*" />
    ASSIGN Menu(oNewMenu AS VOSDK.Menu)
        LOCAL i as DWORD
        SUPER:Menu := oNewMenu
        if oShell != NULL_OBJECT
            i := 0
            FOREACH oItem as VOMenuItem in oNewMenu:__Menu:MenuItems
                if i == oNewMenu:GetAutoUpdate()
                    oItem:MdiList := TRUE
                ENDIF
                ++i
                oItem:MergeType := System.Windows.Forms.MenuMerge.Add
            NEXT
        ENDIF
        RETURN

    /// <inheritdoc />
    METHOD Close(oEvent as event) as USUAL
        var res := SUPER:Close(oEvent)
        oShell:Menu := oShell:__ActualMenu
        RETURN res


END CLASS

