CLASS RadioButton INHERIT Button
	PROTECT lSavedPressed AS LOGIC

METHOD Destroy() 
	

	IF IsWindow(hwnd)
		lSavedPressed := SELF:Pressed
	ENDIF

	RETURN SUPER:Destroy()

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, kStyle) 
	

	SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle)

	IF !IsInstanceOfUsual(xID,#ResourceID)
		SELF:SetStyle(BS_RADIOBUTTON)
	ENDIF
	//	self:Pressed := .T.
	SELF:ValueChanged := FALSE

	RETURN 

ACCESS Pressed 
	

	IF SELF:ValidateControl()
		RETURN LOGIC(_CAST, SendMessage(SELF:Handle(), BM_GETCHECK, 0, 0))
	ELSE
		RETURN lSavedPressed
	ENDIF


ASSIGN Pressed(lPressed) 
	

	IF !IsLogic(lPressed)
		WCError{#Pressed,#RadioButton,__WCSTypeError,lPressed,1}:Throw()
	ENDIF

	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), BM_SETCHECK, IIF( lPressed,1,0 ), 0)
		SELF:Value := lPressed
	ENDIF

	RETURN 

ACCESS TextValue 
	LOCAL lTicked AS LOGIC
	LOCAL cTickValue AS STRING

	

	lTicked := SELF:pressed
	IF IsInstanceOfUsual(oFieldSpec, #FieldSpec)
		cTickValue := oFieldSpec:Transform(lTicked)
	ELSE
		cTickValue := AsString(lTicked)
	ENDIF

	RETURN cTickValue

ASSIGN TextValue(cNewValue) 
	LOCAL lOldTicked AS LOGIC
	LOCAL lTicked AS LOGIC

	

	IF !IsString(cNewValue)
		WCError{#TextValue,#RadioButton,__WCSTypeError,cNewValue,1}:Throw()
	ENDIF
	lOldTicked := SELF:pressed

	IF IsInstanceOfUsual(oFieldSpec, #FieldSpec)
		lTicked := oFieldSpec:Val(cNewValue)
	ELSE
		lTicked:=Unformat(cNewValue,"","L")
	ENDIF

	IF (lTicked != lOldTicked)
		SELF:pressed := lTicked
		SELF:Modified := TRUE
	ENDIF

	RETURN 

ACCESS Value 
	

	RETURN SELF:pressed
END CLASS

