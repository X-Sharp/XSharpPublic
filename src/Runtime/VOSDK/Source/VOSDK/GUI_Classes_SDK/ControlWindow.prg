/// <include file="Gui.xml" path="doc/ControlWindow/*" />
CLASS ControlWindow INHERIT Window
	//export __ptrOldProc as ptr
	EXPORT __lpfnDefaultProc AS PTR
	PROTECT oCtrl AS OBJECT


/// <include file="Gui.xml" path="doc/ControlWindow.Control/*" />
ACCESS Control
	RETURN oCtrl


/// <include file="Gui.xml" path="doc/ControlWindow.ControlID/*" />
ACCESS ControlID
	RETURN oCtrl:ControlID


/// <include file="Gui.xml" path="doc/ControlWindow.Default/*" />
METHOD DEFAULT(oEvent)
	LOCAL oEvt := oEvent AS @@Event
	LOCAL lRetVal


	lRetVal := CallWindowProc(oCtrl:__lpfnDefaultProc, oEvt:hWnd, oEvt:umsg, oEvt:wParam, oEvt:lParam)


	SELF:EventReturnValue := lRetVal


	RETURN LOGIC(_CAST, lRetVal)


/// <include file="Gui.xml" path="doc/ControlWindow.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF !InCollect()
		oCtrl:Destroy()
		hWnd := NULL_PTR
	ENDIF


	SUPER:Destroy()
	//__WCUnRegisterControl(self)


	RETURN SELF


/// <include file="Gui.xml" path="doc/ControlWindow.Disable/*" />
METHOD Disable()
	oCtrl:Disable()
	RETURN SELF

/// <include file="Gui.xml" path="doc/ControlWindow.Dispatch/*" />
METHOD Dispatch(oEvent)
  	LOCAL oE := oEvent AS @@Event
	IF oE:uMsg == WM_PAINT
		//SELF:Expose(__ObjectCastClassPtr(oE, __pCExposeEvent))
		SELF:Expose(ExposeEvent{oE})
		RETURN SELF:EventReturnValue
	ELSE
		RETURN SUPER:Dispatch(oE)
	ENDIF


/// <include file="Gui.xml" path="doc/ControlWindow.Enable/*" />
METHOD Enable()
	oCtrl:Enable()

	RETURN SELF

/// <include file="Gui.xml" path="doc/ControlWindow.Hide/*" />
METHOD Hide()
	oCtrl:Hide()
	RETURN SELF


/// <include file="Gui.xml" path="doc/ControlWindow.HyperLabel/*" />
ACCESS HyperLabel
	RETURN oCtrl:HyperLabel

/// <include file="Gui.xml" path="doc/ControlWindow.ctor/*" />
CONSTRUCTOR(oControl)


	IF ! (oControl IS Control)
		WCError{#Init,#ControlWindow,__WCSTypeError,oControl,1}:Throw()
	ENDIF


	oCtrl := oControl
	SUPER(oCtrl:Owner)
	oCtrl:ValidateControl()


	hWnd := oCtrl:Handle()
	__lpfnDefaultProc := oCtrl:__lpfnDefaultProc


   // RvdH 080215 Do not replace the Control in the strucSelf anymore
   // but register the window with the control
   oCtrl:__ControlWindow := SELF
	RETURN


/// <include file="Gui.xml" path="doc/ControlWindow.Modified/*" />
ACCESS Modified
	RETURN oCtrl:Modified


/// <include file="Gui.xml" path="doc/ControlWindow.Modified/*" />
ASSIGN Modified(lChanged)
	RETURN (oCtrl:Modified := lChanged)


/// <include file="Gui.xml" path="doc/ControlWindow.Origin/*" />
ACCESS Origin
	RETURN oCtrl:Origin


/// <include file="Gui.xml" path="doc/ControlWindow.Origin/*" />
ASSIGN Origin(oPoint)
	RETURN (oCtrl:Origin := oPoint)


/// <include file="Gui.xml" path="doc/ControlWindow.Override/*" />
METHOD Override()
	// for 1.0 compatibility only
	RETURN NIL


/// <include file="Gui.xml" path="doc/ControlWindow.SetFocus/*" />
METHOD SetFocus()
	oCtrl:SetFocus()
	RETURN SELF


/// <include file="Gui.xml" path="doc/ControlWindow.Size/*" />
ACCESS Size
	RETURN oCtrl:Size


/// <include file="Gui.xml" path="doc/ControlWindow.Size/*" />
ASSIGN Size(oDimension)
	RETURN (oCtrl:Size := oDimension)
END CLASS


