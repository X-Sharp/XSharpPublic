PARTIAL CLASS ControlWindow INHERIT Window
	//export __ptrOldProc as ptr
	EXPORT __lpfnDefaultProc AS PTR
	PROTECT oCtrl AS OBJECT

ACCESS Control 
	RETURN oCtrl

ACCESS ControlID 
	RETURN oCtrl:ControlID

METHOD Default(oEvent) 
	LOCAL oEvt := oEvent AS @@Event
	LOCAL lRetVal

	lRetVal := CallWindowProc(oCtrl:__lpfnDefaultProc, oEvt:hWnd, oEvt:umsg, oEvt:wParam, oEvt:lParam)

	SELF:EventReturnValue := lRetVal

	RETURN LOGIC(_CAST, lRetVal)

METHOD Destroy() 
	

	IF !InCollect()
		oCtrl:Destroy()
		hWnd := NULL_PTR
	ENDIF

	SUPER:Destroy()
	//__WCUnRegisterControl(self)

	RETURN SELF

METHOD Disable() 
	oCtrl:Disable()

	RETURN SELF

METHOD Dispatch(oEvent) 
  	LOCAL oE := oEvent AS @@Event
	IF oE:uMsg == WM_PAINT
		//SELF:Expose(__ObjectCastClassPtr(oE, __pCExposeEvent))
		SELF:Expose(ExposeEvent{oE})
		RETURN SELF:EventReturnValue
	ELSE
		RETURN SUPER:Dispatch(oE)
	ENDIF

METHOD Enable() 
	

	oCtrl:Enable()

	RETURN SELF

METHOD Hide() 
	oCtrl:Hide()
	RETURN SELF

ACCESS HyperLabel 
	RETURN oCtrl:HyperLabel

CONSTRUCTOR(oControl) 

	IF !IsInstanceOfUsual(oControl,#Control)
		WCError{#Init,#ControlWindow,__WCSTypeError,oControl,1}:@@Throw()
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

ACCESS Modified 
	RETURN oCtrl:Modified

ASSIGN Modified(lChanged) 
	RETURN (oCtrl:Modified := lChanged)

ACCESS Origin 
	RETURN oCtrl:Origin

ASSIGN Origin(oPoint) 
	RETURN (oCtrl:Origin := oPoint)

method @@Override() 
	// for 1.0 compatibility only
	RETURN NIL

METHOD SetFocus() 
	oCtrl:SetFocus()
	RETURN SELF

ACCESS Size 
	RETURN oCtrl:Size

ASSIGN Size(oDimension) 
	RETURN (oCtrl:Size := oDimension)
END CLASS

