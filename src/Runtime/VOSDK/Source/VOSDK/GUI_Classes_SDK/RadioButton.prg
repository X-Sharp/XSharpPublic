/// <include file="Gui.xml" path="doc/RadioButton/*" />
CLASS RadioButton INHERIT Button
	PROTECT lSavedPressed AS LOGIC


/// <include file="Gui.xml" path="doc/RadioButton.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF IsWindow(hwnd)
		lSavedPressed := SELF:Pressed
	ENDIF


	RETURN SUPER:Destroy()


/// <include file="Gui.xml" path="doc/RadioButton.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, cText, kStyle)




	SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle)


	IF !(xID IS ResourceID)
		SELF:SetStyle(BS_RADIOBUTTON)
	ENDIF
	//	self:Pressed := .T.
	SELF:ValueChanged := FALSE


	RETURN


/// <include file="Gui.xml" path="doc/RadioButton.Pressed/*" />
ACCESS Pressed
	IF SELF:ValidateControl()
		RETURN LOGIC(_CAST, SendMessage(SELF:Handle(), BM_GETCHECK, 0, 0))
	ELSE
		RETURN lSavedPressed
	ENDIF




/// <include file="Gui.xml" path="doc/RadioButton.Pressed/*" />
ASSIGN Pressed(lPressed)




	IF !IsLogic(lPressed)
		WCError{#Pressed,#RadioButton,__WCSTypeError,lPressed,1}:Throw()
	ENDIF


	IF SELF:ValidateControl()
		SendMessage(SELF:Handle(), BM_SETCHECK, IIF( lPressed,1,0 ), 0)
		SELF:Value := lPressed
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/RadioButton.TextValue/*" />
ACCESS TextValue
	LOCAL lTicked AS LOGIC
	LOCAL cTickValue AS STRING






	lTicked := SELF:pressed
	IF oFieldSpec IS FieldSpec VAR oFS
		cTickValue := oFS:Transform(lTicked)
	ELSE
		cTickValue := AsString(lTicked)
	ENDIF


	RETURN cTickValue


/// <include file="Gui.xml" path="doc/RadioButton.TextValue/*" />
ASSIGN TextValue(cNewValue)
	LOCAL lOldTicked AS LOGIC
	LOCAL lTicked AS LOGIC






	IF !IsString(cNewValue)
		WCError{#TextValue,#RadioButton,__WCSTypeError,cNewValue,1}:Throw()
	ENDIF
	lOldTicked := SELF:pressed


	IF oFieldSpec IS FieldSpec VAR oFS
		lTicked := oFS:Val(cNewValue)
	ELSE
		lTicked:=Unformat(cNewValue,"","L")
	ENDIF


	IF (lTicked != lOldTicked)
		SELF:pressed := lTicked
		SELF:Modified := TRUE
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/RadioButton.Value/*" />
ACCESS Value




	RETURN SELF:pressed
END CLASS


