PARTIAL CLASS CheckBox INHERIT Button
	PROTECT lSavedChecked AS LOGIC

ACCESS Checked 
	

	IF SELF:ValidateControl()
		RETURN (SendMessage(SELF:Handle(), BM_GETCHECK, 0, 0) != 0)
	ELSE
		RETURN lSavedChecked
	ENDIF


ASSIGN Checked(lChecked) 
	

	IF SELF:ValidateControl()
		IF !IsLogic(lChecked)
			WCError{#Checked,#CheckBox,__WCSTypeError,lChecked,1}:@@Throw()
		ENDIF
		SendMessage(SELF:Handle(), BM_SETCHECK, DWORD(_CAST, lChecked), 0)
	ENDIF

	__lModified := TRUE
	SELF:__Update()

	RETURN 

METHOD Destroy() 
	

	IF IsWindow(hwnd)
		lSavedChecked := SELF:Checked
	ENDIF

	RETURN SUPER:Destroy()

ACCESS Image 
	//PP-031002
	RETURN SELF:__GetImage()

ASSIGN Image(oNewImage) 
	//PP-031002
	IF ! SELF:__SetImage(oNewImage)
		SUPER:Image := oNewImage
	ENDIF

	RETURN 

CONSTRUCTOR( oOwner, xID, oPoint, oDimension, cText, kStyle) 
	

	SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle, TRUE)

	IF !IsInstanceOfUsual(xID, #ResourceID) .and. IsNil(kStyle)
		SELF:SetStyle(BS_AUTOCHECKBOX)
	ENDIF

	RETURN 

ACCESS TextValue 
	LOCAL lTicked AS LOGIC
	LOCAL cTickValue AS STRING

	
	lTicked := SELF:Checked

	IF IsInstanceOfUsual(SELF:FieldSpec, #FieldSpec)
		cTickValue := SELF:FieldSpec:Transform(lTicked)
	ELSE
		cTickValue := AsString(lTicked)
	ENDIF

	RETURN cTickValue

ASSIGN TextValue(cNewValue) 
	LOCAL lOldTicked AS LOGIC
	LOCAL lTicked AS LOGIC
	LOCAL uTicked AS USUAL

	
	IF !IsString(cNewValue)
		WCError{#TextValue,#CheckBox,__WCSTypeError,cNewValue,1}:@@Throw()
	ENDIF

	lOldTicked := SELF:Checked
	IF IsInstanceOfUsual(SELF:FieldSpec, #FieldSpec)
		uTicked := SELF:FieldSpec:Val(cNewValue)
		IF IsNumeric(uTicked)
			lTicked := (uTicked != 0)
		ELSEIF IsLogic(uTicked)
			lTicked := uTicked
		ELSE
			lTicked := FALSE
		ENDIF
	ELSE
		lTicked := Unformat(cNewValue, "", "L")
	ENDIF

	IF (lTicked != lOldTicked)
		SELF:Checked := lTicked
		SELF:Modified := .T. 
		// self:SetFocus()
	ENDIF

	RETURN 

ACCESS Value 
	LOCAL uVal AS USUAL

	

	IF IsInstanceOf(SELF:Owner, #DataWindow)
		//PP-041004  uVal below must always be NIL since it is a LOCAL
		// IF IsNil(uVal)
			uValue := SELF:Checked
		// ENDIF

		uVal := SUPER:Value

		IF IsString(uVal)
			RETURN (uVal == ".T.")
		ELSE
			RETURN uVal
		ENDIF
	ENDIF
	RETURN SELF:Checked

END CLASS

