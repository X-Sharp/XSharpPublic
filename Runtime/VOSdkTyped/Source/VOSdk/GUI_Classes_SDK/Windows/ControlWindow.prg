


CLASS ControlWindow INHERIT Window
	PROTECT oCtrl AS Control
	PROTECT oSurface AS VOSurfacePanel

	ACCESS __Surface AS System.Windows.Forms.Control
		RETURN oSurface
	
	ACCESS Control AS Control
		RETURN oCtrl

	ACCESS ControlID AS LONG
		RETURN oCtrl:ControlID


	METHOD Destroy() AS USUAL CLIPPER
		IF oCtrl:__IsValid
			oCtrl:Destroy()
		ENDIF
		RETURN SUPER:Destroy()

	METHOD Disable() AS VOID 
		IF oCtrl:__IsValid
			oCtrl:Disable()
		ENDIF
		RETURN 

	METHOD Enable()  AS VOID 
		IF oCtrl:__IsValid
			oCtrl:Enable()
		ENDIF
		RETURN 

	METHOD Hide() AS VOID STRICT
		oSurface:Hide()
		RETURN 

	ACCESS HyperLabel AS HyperLabel
		RETURN oCtrl:HyperLabel

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
		oSurface:Controls:Add(oCtrl:__Control)
		oSurface:Visible := TRUE

		RETURN 

	ACCESS Modified AS LOGIC
		IF oCtrl:__IsValid
			RETURN oCtrl:Modified
		ENDIF
		RETURN FALSE

	ASSIGN Modified(lChanged AS LOGIC) 
		IF oCtrl:__IsValid
			oCtrl:Modified := lChanged
		ENDIF

	ACCESS Origin AS Point
		IF oCtrl:__IsValid
			RETURN oCtrl:Origin
		ENDIF
		RETURN SUPER:Origin

	ASSIGN Origin(oPoint AS Point) 
		IF oCtrl:__IsValid		
			oCtrl:Origin := oPoint
		ENDIF

	METHOD Override() 
		RETURN NIL

	METHOD SetFocus() AS VOID STRICT
		IF oCtrl:__IsValid
			oCtrl:SetFocus()
		ENDIF
		RETURN 

	METHOD Show(nShowState AS LONG ) AS VOID
		oSurface:Show()
		RETURN 



	ACCESS Size AS Dimension
		IF oCtrl:__IsValid
			RETURN oCtrl:Size
		ENDIF
		RETURN SUPER:Size

	ASSIGN Size(oDimension AS Dimension) 
		IF oCtrl:__IsValid
			oCtrl:Size := oDimension
		ENDIF
END CLASS
