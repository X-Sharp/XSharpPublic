//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

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
        DEFAULT( REF lImpl, TRUE)
        IF (lImpl)
            //IF (oImp == NULL_OBJECT)
            IF lMng .AND. oParent IS ShellWindow // create an MDI child
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
                NOP

            ENDIF
            //ENDIF
        ENDIF

    /// <include file="Gui.xml" path="doc/ChildAppWindow.Menu/*" />
    PROPERTY Menu AS VOSDK.Menu
        GET
            RETURN SUPER:Menu
        END GET
        SET
            LOCAL i AS DWORD
            SUPER:Menu := VALUE
            IF oShell != NULL_OBJECT
                i := 0
                FOREACH oItem AS VOMenuItem IN VALUE:__Menu:MenuItems
                    IF i == VALUE:GetAutoUpdate()
                        oItem:MdiList := TRUE
                    ENDIF
                    ++i
                    oItem:MergeType := System.Windows.Forms.MenuMerge.Add
                NEXT
            ENDIF
            RETURN
        END SET
    END PROPERTY
    /// <inheritdoc />
    METHOD Close(oEvent AS event) AS USUAL
        VAR res := SUPER:Close(oEvent)
        oShell:Menu := oShell:__ActualMenu
        RETURN res


END CLASS

