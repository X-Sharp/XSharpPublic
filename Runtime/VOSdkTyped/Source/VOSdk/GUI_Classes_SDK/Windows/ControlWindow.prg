

/// <include file="Gui.xml" path="doc/ControlWindow/*" />
CLASS ControlWindow INHERIT Window
	PROTECT oCtrl AS Control
	PROTECT oSurface AS VOSurfacePanel

	ACCESS __Surface AS IVOControlContainer
		RETURN oSurface

/// <include file="Gui.xml" path="doc/ControlWindow.Control/*" />
	ACCESS Control AS Control
		RETURN oCtrl

/// <include file="Gui.xml" path="doc/ControlWindow.ControlID/*" />
	ACCESS ControlID AS LONG
		RETURN oCtrl:ControlID


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
	ACCESS HyperLabel AS HyperLabel
		RETURN oCtrl:HyperLabel

/// <include file="Gui.xml" path="doc/ControlWindow.ctor/*" />
	CONSTRUCTOR(oControl)

		IF !IsInstanceOfUsual(oControl,#Control)
			WCError{#Init,#ControlWindow,__WCSTypeError,oControl,1}:@@Throw()
		ENDIF

		oCtrl := oControl
		SUPER((OBJECT) oCtrl:Owner)
		oCtrl:ValidateControl()
		oSurface := GuiFactory.Instance:CreateSurfacePanel(SELF)
		oSurface:Text := "Surface "+oCtrl:Caption
		oCtrl:__ControlWindow := SELF

		oSurface:Dock := System.Windows.Forms.DockStyle.Fill
		oSurface:AddControl(oCtrl:__Control)
		oSurface:Visible := TRUE

		RETURN

/// <include file="Gui.xml" path="doc/ControlWindow.Modified/*" />
	ACCESS Modified AS LOGIC
		IF oCtrl:__IsValid
			RETURN oCtrl:Modified
		ENDIF
		RETURN FALSE

/// <include file="Gui.xml" path="doc/ControlWindow.Modified/*" />
	ASSIGN Modified(lChanged AS LOGIC)
		IF oCtrl:__IsValid
			oCtrl:Modified := lChanged
		ENDIF

/// <include file="Gui.xml" path="doc/ControlWindow.Origin/*" />
	ACCESS Origin AS Point
		IF oCtrl:__IsValid
			RETURN oCtrl:Origin
		ENDIF
		RETURN SUPER:Origin

/// <include file="Gui.xml" path="doc/ControlWindow.Origin/*" />
	ASSIGN Origin(oPoint AS Point)
		IF oCtrl:__IsValid
			oCtrl:Origin := oPoint
		ENDIF

/// <include file="Gui.xml" path="doc/ControlWindow.Override/*" />
	METHOD Override()
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
	ACCESS Size AS Dimension
		IF oCtrl:__IsValid
			RETURN oCtrl:Size
		ENDIF
		RETURN SUPER:Size

/// <include file="Gui.xml" path="doc/ControlWindow.Size/*" />
	ASSIGN Size(oDimension AS Dimension)
		IF oCtrl:__IsValid
			oCtrl:Size := oDimension
		ENDIF
END CLASS
