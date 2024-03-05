/// <include file="Gui.xml" path="doc/ErrorBox/*" />
CLASS ErrorBox INHERIT TextBox


/// <include file="Gui.xml" path="doc/ErrorBox.ctor/*" />
CONSTRUCTOR(uParent, uText)




	IF uText IS HyperLabel var oHL
		uText := oHL:Description
	ENDIF


	SUPER(uParent, ResourceString{__WCSError}:value, uText)


	SELF:Type := BOXICONHAND
	SELF:Beep := TRUE


	RETURN


END CLASS


/// <include file="Gui.xml" path="doc/InfoBox/*" />
CLASS InfoBox INHERIT TextBox


/// <include file="Gui.xml" path="doc/InfoBox.ctor/*" />
CONSTRUCTOR(uParent, uCaption, uText)




	IF uCaption IS HyperLabel var oHL .AND. IsNil(uText)
		uText := oHL:Description
		uCaption := oHL:Caption
	ENDIF


	Default(@uCaption, ResourceString{__WCSInfoBox}:value)


	SUPER(uParent,uCaption,uText)
	SELF:Type := BOXICONASTERISK


	RETURN
END CLASS


/// <include file="Gui.xml" path="doc/TextBox/*" />
CLASS TextBox INHERIT VObject
	PROTECT oParent AS Window
	PROTECT dwType AS DWORD
	PROTECT ctext AS STRING
	PROTECT cCaption AS STRING
	PROTECT lBeep AS LOGIC


/// <include file="Gui.xml" path="doc/TextBox.Beep/*" />
ACCESS Beep




	RETURN lBeep


/// <include file="Gui.xml" path="doc/TextBox.Beep/*" />
ASSIGN Beep(uBeep)




	IF !IsLogic(uBeep)
		WCError{#Beep,#TextBox,__WCSTypeError,uBeep}:Throw()
	ENDIF


	RETURN (lBeep := uBeep)


/// <include file="Gui.xml" path="doc/TextBox.Caption/*" />
ACCESS Caption




	RETURN cCaption


/// <include file="Gui.xml" path="doc/TextBox.Caption/*" />
ASSIGN Caption(uCaption)




	IF !IsString(uCaption)
		WCError{#Caption,#TextBox,__WCSTypeError,uCaption}:Throw()
	ENDIF
	RETURN cCaption:=uCaption


/// <include file="Gui.xml" path="doc/TextBox.ctor/*" />
CONSTRUCTOR(uParent, uCaption, uText, nType)




	SUPER()


	IF !IsNil(uParent)
		IF !(uParent IS Window)
			WCError{#Init,#TextBox,__WCSTypeError,uParent,1}:Throw()
		ELSE
			oParent := uParent
		ENDIF
	ENDIF


	IF uCaption IS HyperLabel VAR oHL .AND. IsNil(uText)
		uText := oHL:Description
		uCaption := oHL:Caption
	ENDIF


	IF !IsNil(uCaption)
		IF !IsString(uCaption)
			WCError{#Init,#TextBox,__WCSTypeError,uCaption,2}:Throw()
		ENDIF
		cCaption := uCaption
	ELSE
		cCaption := ResourceString{__WCSTextBox}:value
	ENDIF


	IF !IsNil(uText)
		IF !IsString(uText)
			WCError{#Init,#TextBox,__WCSTypeError,uText,3}:Throw()
		ENDIF
		cText := uText
	ELSE
		cText := _CHR(0)
	ENDIF


	IF !IsNil(nType)
		dwType := nType
	ENDIF


	RETURN




/// <include file="Gui.xml" path="doc/TextBox.Message/*" />
ACCESS Message




	RETURN cText


/// <include file="Gui.xml" path="doc/TextBox.Message/*" />
ASSIGN Message(uText)




	IF !IsString(uText)
		WCError{#text,#TextBox,__WCSTypeError,uText}:Throw()
	ENDIF


	RETURN cText:=uText


/// <include file="Gui.xml" path="doc/TextBox.Show/*" />
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


/// <include file="Gui.xml" path="doc/TextBox.Type/*" />
ACCESS Type


	RETURN dwType


/// <include file="Gui.xml" path="doc/TextBox.TYPE/*" />
ASSIGN TYPE(uType)




	IF !IsLong(uType)
		WCError{#Type,#TextBox,__WCSTypeError,uType,1}:Throw()
	ENDIF
	dwType := uType


	RETURN


END CLASS


/// <include file="Gui.xml" path="doc/WarningBox/*" />
CLASS WarningBox INHERIT TextBox


/// <include file="Gui.xml" path="doc/WarningBox.ctor/*" />
CONSTRUCTOR(uParent, uCaption, uText)




	IF uCaption IS HyperLabel VAR oHL .AND. IsNil(uText)
		uText := oHL:Description
		uCaption := oHL:Caption
	ENDIF


	Default(@uCaption, _CHR(0))


	SUPER(uParent,uCaption,uText)


	SELF:Type := BOXICONEXCLAMATION
	SELF:Beep := TRUE


	RETURN
END CLASS


