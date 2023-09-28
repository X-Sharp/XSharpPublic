/// <include file="Gui.xml" path="doc/CheckBox/*" />
CLASS CheckBox INHERIT Button
	PROTECT lSavedChecked AS LOGIC


/// <include file="Gui.xml" path="doc/CheckBox.Checked/*" />
ACCESS Checked




	IF SELF:ValidateControl()
		RETURN (SendMessage(SELF:Handle(), BM_GETCHECK, 0, 0) != 0)
	ELSE
		RETURN lSavedChecked
	ENDIF




/// <include file="Gui.xml" path="doc/CheckBox.Checked/*" />
ASSIGN Checked(lChecked)




	IF SELF:ValidateControl()
		IF !IsLogic(lChecked)
			WCError{#Checked,#CheckBox,__WCSTypeError,lChecked,1}:Throw()
		ENDIF
		SendMessage(SELF:Handle(), BM_SETCHECK, DWORD(_CAST, lChecked), 0)
	ENDIF


	__lModified := TRUE
	SELF:__Update()


	RETURN


/// <include file="Gui.xml" path="doc/CheckBox.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER




	IF IsWindow(hwnd)
		lSavedChecked := SELF:Checked
	ENDIF


	RETURN SUPER:Destroy()


/// <include file="Gui.xml" path="doc/CheckBox.Image/*" />
ACCESS Image
	//PP-031002
	RETURN SELF:__GetImage()


/// <include file="Gui.xml" path="doc/CheckBox.Image/*" />
ASSIGN Image(oNewImage)
	//PP-031002
	IF ! SELF:__SetImage(oNewImage)
		SUPER:Image := oNewImage
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/CheckBox.ctor/*" />
CONSTRUCTOR( oOwner, xID, oPoint, oDimension, cText, kStyle)




	SUPER(oOwner, xID, oPoint, oDimension, cText, kStyle, TRUE)


	IF !IsInstanceOfUsual(xID, #ResourceID) .and. IsNil(kStyle)
		SELF:SetStyle(BS_AUTOCHECKBOX)
	ENDIF


	RETURN


/// <include file="Gui.xml" path="doc/CheckBox.TextValue/*" />
ACCESS TextValue
	LOCAL lTicked AS LOGIC
	LOCAL cTickValue AS STRING




	lTicked := SELF:Checked

	if self:FieldSpec is FieldSpec var oFS
		cTickValue := oFS:Transform(lTicked)
	ELSE
		cTickValue := AsString(lTicked)
	ENDIF


	RETURN cTickValue


/// <include file="Gui.xml" path="doc/CheckBox.TextValue/*" />
ASSIGN TextValue(cNewValue)
	LOCAL lOldTicked AS LOGIC
	LOCAL lTicked AS LOGIC
	LOCAL uTicked AS USUAL


	IF !IsString(cNewValue)
		WCError{#TextValue,#CheckBox,__WCSTypeError,cNewValue,1}:Throw()
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


/// <include file="Gui.xml" path="doc/CheckBox.Value/*" />
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


