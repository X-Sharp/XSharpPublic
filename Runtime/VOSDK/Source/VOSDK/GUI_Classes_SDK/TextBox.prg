CLASS ErrorBox INHERIT TextBox

CONSTRUCTOR(uParent, uText) 
	

	IF IsInstanceOfUsual( uText, #HyperLabel )
		uText := uText:Description
	ENDIF

	SUPER(uParent, ResourceString{__WCSError}:value, uText)

	SELF:Type := BOXICONHAND
	SELF:Beep := TRUE

	RETURN 

END CLASS

CLASS InfoBox INHERIT TextBox

CONSTRUCTOR(uParent, uCaption, uText) 
	

	IF IsInstanceOfUsual(uCaption,#HyperLabel) .AND. IsNil(uText)
		uText := uCaption:Description
		uCaption := uCaption:Caption
	ENDIF

	Default(@uCaption, ResourceString{__WCSInfoBox}:value)

	SUPER(uParent,uCaption,uText)
	SELF:Type := BOXICONASTERISK

	RETURN 
END CLASS

CLASS TextBox INHERIT VObject
	PROTECT oParent AS Window
	PROTECT dwType AS DWORD
	PROTECT ctext AS STRING
	PROTECT cCaption AS STRING
	PROTECT lBeep AS LOGIC

ACCESS Beep 
	

	RETURN lBeep

ASSIGN Beep(uBeep) 
	

	IF !IsLogic(uBeep)
		WCError{#Beep,#TextBox,__WCSTypeError,uBeep}:@@Throw()
	ENDIF

	RETURN (lBeep := uBeep)

ACCESS Caption 
	

	RETURN cCaption

ASSIGN Caption(uCaption) 
	

	IF !IsString(uCaption)
		WCError{#Caption,#TextBox,__WCSTypeError,uCaption}:@@Throw()
	ENDIF
	RETURN cCaption:=uCaption

CONSTRUCTOR(uParent, uCaption, uText, nType) 
	

	SUPER()

	IF !IsNil(uParent)
		IF !IsInstanceOfUsual(uParent, #Window)
			WCError{#Init,#TextBox,__WCSTypeError,uParent,1}:@@Throw()
		ELSE
			oParent := uParent
		ENDIF
	ENDIF

	IF IsInstanceOfUsual(uCaption,#HyperLabel) .AND. IsNil(uText)
		uText := uCaption:Description
		uCaption := uCaption:Caption
	ENDIF

	IF !IsNil(uCaption)
		IF !IsString(uCaption)
			WCError{#Init,#TextBox,__WCSTypeError,uCaption,2}:@@Throw()
		ENDIF
		cCaption := uCaption
	ELSE
		cCaption := ResourceString{__WCSTextBox}:value
	ENDIF

	IF !IsNil(uText)
		IF !IsString(uText)
			WCError{#Init,#TextBox,__WCSTypeError,uText,3}:@@Throw()
		ENDIF
		cText := uText
	ELSE
		cText := _CHR(0)
	ENDIF

	IF !IsNil(nType)
		dwType := nType
	ENDIF

	RETURN 


ACCESS Message 
	

	RETURN cText

ASSIGN Message(uText) 
	

	IF !IsString(uText)
		WCError{#text,#TextBox,__WCSTypeError,uText}:@@Throw()
	ENDIF

	RETURN cText:=uText

METHOD Show() 
	LOCAL iRetVal AS INT
	LOCAL aRetVal AS ARRAY
	LOCAL hParent AS PTR

	
	aRetVal:= {BoxReplyOkay, BoxReplyCancel, BoxReplyAbort, BoxReplyRetry,;
		BoxReplyIgnore, BoxReplyYes, BoxReplyNo, BoxReplyClose, ;
		-1, BoxReplyTryAgain, BoxReplyContinue}
    // Note IDHELP is 8, but this is never a return value 

	IF (oParent != NULL_OBJECT)
		hParent := oParent:Handle()
	ELSE
		dwType := _OR(dwType, DWORD(_CAST, MB_TASKMODAL))
		hParent := NULL_PTR
	ENDIF

	IF lBeep
		MessageBeep(0)
	ENDIF

	iRetVal := MessageBox(hParent, String2Psz(ctext), String2Psz(cCaption), dwType)
	IF (iRetVal > 0 .AND. DWORD(iRetVal) <= ALen(aRetVal))
		RETURN aRetVal[iRetVal]
	ENDIF
	RETURN -1

ACCESS Type 
	
	RETURN dwType

ASSIGN TYPE(uType) 
	

	IF !IsLong(uType)
		WCError{#Type,#TextBox,__WCSTypeError,uType,1}:@@Throw()
	ENDIF
	dwType := uType

	RETURN 

END CLASS

CLASS WarningBox INHERIT TextBox

CONSTRUCTOR(uParent, uCaption, uText) 
	

	IF IsInstanceOfUsual(uCaption,#HyperLabel) .AND. IsNil(uText)
		uText := uCaption:Description
		uCaption := uCaption:Caption
	ENDIF

	Default(@uCaption, _CHR(0))

	SUPER(uParent,uCaption,uText)

	SELF:Type := BOXICONEXCLAMATION
	SELF:Beep := TRUE

	RETURN 
END CLASS

