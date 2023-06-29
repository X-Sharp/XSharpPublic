

/// <include file="Gui.xml" path="doc/ControlWindow/*" />
CLASS ControlWindow INHERIT Window
    PROTECT oCtrl AS Control
    PROTECT oSurface AS VOSurfacePanel

    PROPERTY __Surface AS IVOControlContainer GET oSurface

    /// <include file="Gui.xml" path="doc/ControlWindow.Control/*" />
    PROPERTY Control AS Control GET oCtrl

    /// <include file="Gui.xml" path="doc/ControlWindow.ControlID/*" />
    PROPERTY ControlID AS LONG GET oCtrl:ControlID

    /// <include file="Gui.xml" path="doc/ControlWindow.Destroy/*" />
    METHOD Destroy() AS USUAL
        IF oCtrl:__IsValid
            oCtrl:Destroy()
        ENDIF
        RETURN SUPER:Destroy()

    /// <include file="Gui.xml" path="doc/ControlWindow.Disable/*" />
    METHOD Disable() AS VOID
        IF oCtrl:__IsValid
            oCtrl:Disable()
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/ControlWindow.Enable/*" />
    METHOD Enable()  AS VOID
        IF oCtrl:__IsValid
            oCtrl:Enable()
        ENDIF
        RETURN

    /// <include file="Gui.xml" path="doc/ControlWindow.Hide/*" />
    METHOD Hide() AS VOID STRICT
        oSurface:Hide()
        RETURN

    /// <include file="Gui.xml" path="doc/ControlWindow.HyperLabel/*" />
    PROPERTY HyperLabel AS HyperLabel GET oCtrl:HyperLabel

    /// <include file="Gui.xml" path="doc/ControlWindow.ctor/*" />
    CONSTRUCTOR(oControl)

        IF !IsInstanceOfUsual(oControl,#Control)
            WCError{#Init,#ControlWindow,__WCSTypeError,oControl,1}:Throw()
        ENDIF

        oCtrl := oControl
        SUPER(oCtrl:Owner)
        oCtrl:ValidateControl()
        oSurface := GuiFactory.Instance:CreateSurfacePanel(SELF)
        oSurface:Text := "Surface "+oCtrl:Caption
        oCtrl:__ControlWindow := SELF

        oSurface:Dock := System.Windows.Forms.DockStyle.Fill
        oSurface:AddControl(oCtrl:__Control)
        oSurface:Visible := TRUE

        RETURN

    /// <include file="Gui.xml" path="doc/ControlWindow.Modified/*" />
    PROPERTY Modified AS LOGIC
        GET
            IF oCtrl:__IsValid
                RETURN oCtrl:Modified
            ENDIF
            RETURN FALSE
        END GET
        SET
            IF oCtrl:__IsValid
                oCtrl:Modified := value
            ENDIF
        END SET
    END PROPERTY
    /// <include file="Gui.xml" path="doc/ControlWindow.Origin/*" />
    PROPERTY Origin AS Point
        GET
            IF oCtrl:__IsValid
                RETURN oCtrl:Origin
            ENDIF
            RETURN SUPER:Origin
        END GET
        SET
            IF oCtrl:__IsValid
                oCtrl:Origin := value
            ENDIF
            SUPER:Origin := value
        END SET
    END PROPERTY
    /// <include file="Gui.xml" path="doc/ControlWindow.Override/*" />
    METHOD Override() STRICT
        RETURN NIL

    /// <include file="Gui.xml" path="doc/ControlWindow.SetFocus/*" />
    METHOD SetFocus() AS VOID STRICT
        IF oCtrl:__IsValid
            oCtrl:SetFocus()
        ENDIF
        RETURN

    METHOD Show(nShowState AS LONG ) AS VOID
        oSurface:Show()
        RETURN

    /// <include file="Gui.xml" path="doc/ControlWindow.Size/*" />
    PROPERTY Size AS Dimension
        GET
            IF oCtrl:__IsValid
                RETURN oCtrl:Size
            ENDIF
            RETURN SUPER:Size
        END GET
        SET
            IF oCtrl:__IsValid
                oCtrl:Size := value
            ENDIF
            SUPER:Size := value
        END SET
    END PROPERTY
END CLASS
