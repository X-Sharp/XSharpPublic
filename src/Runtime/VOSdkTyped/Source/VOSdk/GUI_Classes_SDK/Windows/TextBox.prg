//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="Gui.xml" path="doc/ErrorBox/*" />
CLASS ErrorBox INHERIT TextBox

    /// <include file="Gui.xml" path="doc/ErrorBox.ctor/*" />
    CONSTRUCTOR(uParent, uText)
        if uText is HyperLabel var oHL
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

        IF uCaption is HyperLabel VAR oHL .AND. IsNil(uText)
            uText := oHL:Description
            uCaption := oHL:Caption
        ENDIF

        DEFAULT( REF uCaption, ResourceString{__WCSInfoBox}:Value)

        SUPER(uParent,uCaption,uText)
        SELF:Type := BOXICONASTERISK

        RETURN
END CLASS

/// <include file="Gui.xml" path="doc/TextBox/*" />
CLASS TextBox INHERIT VObject
    PROTECT oParent		AS Window
    /// <include file="Gui.xml" path="doc/TextBox.Beep/*" />
    PROPERTY Beep		AS LOGIC	AUTO
    /// <include file="Gui.xml" path="doc/TextBox.Caption/*" />
    PROPERTY Caption	AS STRING	AUTO
    /// <include file="Gui.xml" path="doc/TextBox.Message/*" />
    PROPERTY Message	AS STRING	AUTO
    /// <include file="Gui.xml" path="doc/TextBox.TYPE/*" />
    PROPERTY Type		AS LONG		AUTO

    /// <include file="Gui.xml" path="doc/TextBox.ctor/*" />
    CONSTRUCTOR(uParent, uCaption, uText, nType)
        SUPER()
        IF !IsNil(uParent)
            IF uParent is Window var oWin
                oParent := oWin
            ENDIF
        ENDIF

        IF uCaption  is HyperLabel var oHL .AND. IsNil(uText)
            uText	:= oHL:Description
            uCaption := oHL:Caption
        ENDIF

        IF !IsNil(uCaption)
            IF !IsString(uCaption)
                WCError{#Init,#TextBox,__WCSTypeError,uCaption,2}:Throw()
            ENDIF
            Caption := uCaption
        ELSE
            Caption := ResourceString{__WCSTextBox}:Value
        ENDIF

        IF !IsNil(uText)
            IF !IsString(uText)
                WCError{#Init,#TextBox,__WCSTypeError,uText,3}:Throw()
            ENDIF
            Message := uText
        ELSE
            Message := String.Empty
        ENDIF

        IF !IsNil(nType)
            Type := nType
        ENDIF

        RETURN

    /// <include file="Gui.xml" path="doc/TextBox.Show/*" />
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
            GuiWin32.MessageBeep(0)
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

/// <include file="Gui.xml" path="doc/WarningBox/*" />
CLASS WarningBox INHERIT TextBox

    /// <include file="Gui.xml" path="doc/WarningBox.ctor/*" />
    CONSTRUCTOR(uParent, uCaption, uText)
        IF uCaption IS HyperLabel  VAR oHL .AND. IsNil(uText)
            uText := oHL:Description
            uCaption := oHL:Caption
        ENDIF

        DEFAULT( REF uCaption, String.Empty)

        SUPER(uParent,uCaption,uText)

        SELF:Type := BOXICONEXCLAMATION
        SELF:Beep := TRUE

        RETURN
END CLASS

