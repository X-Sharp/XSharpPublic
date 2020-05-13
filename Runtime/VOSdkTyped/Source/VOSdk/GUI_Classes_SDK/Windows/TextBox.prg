


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

		Default(@uCaption, ResourceString{__WCSInfoBox}:Value)

		SUPER(uParent,uCaption,uText)
		SELF:Type := BOXICONASTERISK

		RETURN 
END CLASS

CLASS TextBox INHERIT VObject
	PROTECT oParent		AS Window
	PROPERTY Beep		AS LOGIC	AUTO
	PROPERTY Caption	AS STRING	AUTO
	PROPERTY Message	AS STRING	AUTO
	PROPERTY Type		AS LONG		AUTO

	CONSTRUCTOR(uParent, uCaption, uText, nType) 
		SUPER()
		IF !IsNil(uParent)
			IF IsInstanceOfUsual(uParent, #Window)
				oParent := uParent
			ENDIF
		ENDIF

		IF IsInstanceOfUsual(uCaption,#HyperLabel) .AND. IsNil(uText)
			uText	:= uCaption:Description
			uCaption := uCaption:Caption
		ENDIF

		IF !IsNil(uCaption)
			IF !IsString(uCaption)
				WCError{#Init,#TextBox,__WCSTypeError,uCaption,2}:@@Throw()
			ENDIF
			Caption := uCaption
		ELSE
			Caption := ResourceString{__WCSTextBox}:Value
		ENDIF

		IF !IsNil(uText)
			IF !IsString(uText)
				WCError{#Init,#TextBox,__WCSTypeError,uText,3}:@@Throw()
			ENDIF
			Message := uText
		ELSE
			Message := String.Empty
		ENDIF

		IF !IsNil(nType)
			Type := nType
		ENDIF

		RETURN 

	METHOD Show() AS LONG STRICT
		LOCAL iRetVal AS INT
		LOCAL aRetVal AS ARRAY
		//LOCAL hParent AS IWin32Window
		LOCAL iButtons AS INT
		LOCAL iIcons AS INT
		LOCAL iDefault AS INT

		aRetVal:= {BoxReplyOkay, BoxReplyCancel, BoxReplyAbort, BoxReplyRetry,;
		BoxReplyIgnore, BoxReplyYes, BoxReplyNo, BoxReplyClose, ;
		-1, BoxReplyTryAgain, BoxReplyContinue}
		// Note IDHELP is 8, but this is never a return value 

		//IF (oParent != NULL_OBJECT)
		//	hParent := oParent:Handle()
		//ELSE
		Type := _OR(Type, (LONG) MB_TASKMODAL)
		//		hParent := NULL_PTR
		//ENDIF

		IF Beep
			Win32.MessageBeep(0)
		ENDIF
		iButtons := Type % 8
		iIcons	 := Type - iButtons
		iIcons	  := iIcons % 128
		iDefault  := Type - iButtons - iIcons
		iDefault  := iDefault % 1024

		//iRetVal := MessageBox(hParent, ctext, cCaption, dwType)
		IF oParent != NULL_OBJECT
				LOCAL iOptions AS INT
				iOptions  := Type - iButtons - iIcons - iDefault
				iRetVal := System.Windows.Forms.MessageBox.Show(oParent:__Form, Message, Caption, ;
					(System.Windows.Forms.MessageBoxButtons)iButtons, ;
					(System.Windows.Forms.MessageBoxIcon)iIcons,;
					(System.Windows.Forms.MessageBoxDefaultButton) iDefault, ;
					(System.Windows.Forms.MessageBoxOptions) iOptions)
		ELSE
				LOCAL iOptions AS INT
				iOptions  := Type - iButtons - iIcons - iDefault
				iRetVal := System.Windows.Forms.MessageBox.Show(Message, Caption, ;
					(System.Windows.Forms.MessageBoxButtons)iButtons, ;
					(System.Windows.Forms.MessageBoxIcon)iIcons,;
					(System.Windows.Forms.MessageBoxDefaultButton) iDefault, ;
					(System.Windows.Forms.MessageBoxOptions) iOptions)
		ENDIF
		IF (iRetVal > 0 .AND. DWORD(iRetVal) <= ALen(aRetVal))
			RETURN aRetVal[iRetVal]
		ENDIF
		RETURN -1


END CLASS

CLASS WarningBox INHERIT TextBox

	CONSTRUCTOR(uParent, uCaption, uText) 
		IF IsInstanceOfUsual(uCaption,#HyperLabel) .AND. IsNil(uText)
			uText := uCaption:Description
			uCaption := uCaption:Caption
		ENDIF

		Default(@uCaption, String.Empty)

		SUPER(uParent,uCaption,uText)

		SELF:Type := BOXICONEXCLAMATION
		SELF:Beep := TRUE

		RETURN 
END CLASS

