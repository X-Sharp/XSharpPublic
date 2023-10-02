//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



USING System.Runtime.InteropServices
/// <include file="Gui.xml" path="doc/TextControl/*" />
CLASS TextControl INHERIT Control
	PROTECT cSavedText   AS STRING // save text before DESTROY, for 1.0 compatibility
	PROTECT oFont        AS Font
	PROTECT lManageColor AS LOGIC
	PROTECT oTextColor   AS Color
	PROTECT symImeFlag   AS SYMBOL // default, don't check for IME
	PROTECT oToolTip     AS System.Windows.Forms.ToolTip
    /// <inheritdoc />
    PROPERTY ControlType AS ControlType GET ControlType.TextControl


 /// <exclude />
	METHOD __GetText() AS STRING STRICT
		IF !STRING.IsNullOrEmpty(cSavedText) .or. !SELF:__IsValid
			RETURN cSavedText
		ENDIF

		RETURN oCtrl:Text

    /// <inheritdoc />
	METHOD OnHandleDestroyed(o AS OBJECT, e AS EventArgs) AS VOID
		TRY
			SUPER:OnHandleDestroyed(o,e)
			IF SELF:__IsValid .and. !STRING.IsNullOrEmpty(oCtrl:Text)
				SELF:cSavedText := oCtrl:Text
			ENDIF
        CATCH  AS Exception
            NOP
		END TRY
		RETURN

    /// <inheritdoc />
	METHOD OnHandleCreated(o AS OBJECT, e AS EventArgs) AS VOID
		SUPER:OnHandleCreated(o,e)
		IF !STRING.IsNullOrEmpty(cSavedText)
			IF SELF:__IsValid
				oCtrl:Text := cSavedText
				cSavedText := STRING.Empty
			ENDIF
		ENDIF
		RETURN

	[Obsolete];
 	METHOD __InitTextMetrics() AS VOID STRICT
		RETURN

 /// <exclude />
	METHOD __RescalCntlB(oFont AS System.Drawing.Font) AS VOID STRICT
		oCtrl:Size := System.Windows.Forms.TextRenderer.MeasureText(SELF:Caption, oFont)
		RETURN

	[Obsolete];
	METHOD __SetColors(_hDC AS IntPtr) AS IntPtr STRICT
		RETURN IntPtr.Zero

 /// <exclude />
	METHOD __SetText(cNewText AS STRING) AS STRING STRICT
		if self:ValidateControl()
			if oCtrl:Text != cNewText
				oCtrl:Text := cNewText
			endif
		ENDIF
		RETURN cNewText


 /// <exclude />
	METHOD __Update() AS VOID STRICT
		//PP-030828 Strong typing
		// Added version of __Update() for TextControl
		LOCAL cText AS STRING
		LOCAL cNewText AS STRING
		LOCAL uOldValue AS USUAL


		IF SELF:Modified
			cText := SELF:TextValue
			uOldValue := AsString(uValue)
			if self:FieldSpec != null_object

				uValue := SELF:FieldSpec:Val(cText)

				// If theres a picture clause we need to reformat the data at this point
				//RvdH 060608 optimized
				//IF ((!IsNil(SELF:FieldSpec:Picture)) .AND. !Empty(SELF:FieldSpec:Picture))
				IF SLen(SELF:FieldSpec:Picture) > 0
					cNewText := SELF:FieldSpec:Transform(uValue)
				ELSEIF IsNil(uValue)
					cNewText := ""
				ELSE
					cNewText := AsString(uValue)
				ENDIF

				IF !(cNewText == cText)
					SELF:TextValue := cNewText
				ENDIF
			ELSE
				uValue := cText
			ENDIF

			SELF:Modified := .F.
			SELF:ValueChanged := !(uOldValue == AsString(uValue))
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/TextControl.Caption/*" />
	ACCESS Caption AS STRING
		IF !IsString(cCaption)
			cCaption := SELF:__GetText()
		ENDIF
		RETURN cCaption

/// <include file="Gui.xml" path="doc/TextControl.Caption/*" />
	ASSIGN Caption(cNewCaption AS STRING)
		cCaption := cNewCaption

		SELF:__SetText(cNewCaption)

/// <include file="Gui.xml" path="doc/TextControl.ControlFont/*" />
	ACCESS ControlFont as Font
		LOCAL oControl AS TextControl
		if (self:oFont == null_object)
			if IsAccess(oParent, #Font)
				oFont := IVarGet(oParent,#Font)
				IF oFont != NULL_OBJECT
					RETURN oFont
				ENDIF
			ELSEIF IsAccess(oParent, #ControlFont)
				oFont := IVarGet(oParent,#ControlFont)
				IF oFont != NULL_OBJECT
					RETURN oFont
				ENDIF
			ENDIF
			oFont := Font{FONTSYSTEM8}
			oFont:Create()
		ENDIF
		RETURN SELF:oFont

/// <include file="Gui.xml" path="doc/TextControl.ControlFont/*" />
	ASSIGN ControlFont(oNewFont AS Font)
		LOCAL hFont AS PTR
		IF SELF:ValidateControl()
			oFont:= oNewFont
			IF oFont != NULL_OBJECT
				oFont:Create()
				hFont := oFont:Handle()
			ELSE
				hFont := GuiWin32.GetStockObject(DEFAULT_GUI_FONT)
				IF (hFont == NULL_PTR)
					hFont := GuiWin32.GetStockObject(SYSTEM_FONT)
				ENDIF
			ENDIF
			SELF:oCtrl:Font := oFont:__Font
			SELF:__RescalCntlB(oFont)
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/TextControl.Create/*" />
	METHOD Create() AS System.Windows.Forms.Control STRICT
		RETURN SUPER:Create()



/// <include file="Gui.xml" path="doc/TextControl.CurrentText/*" />
	ACCESS CurrentText AS STRING
		RETURN SELF:__GetText()

/// <include file="Gui.xml" path="doc/TextControl.CurrentText/*" />
	ASSIGN CurrentText(cNewText AS STRING)
		LOCAL cCurrentText AS STRING
		LOCAL cOldValue AS STRING

		cCurrentText := SELF:__SetText(cNewText)
		cOldValue := AsString(uValue)

		if self:FieldSpec != null_object
			uValue := SELF:FieldSpec:Val(cCurrentText)
			SELF:ValueChanged := !(cOldValue == AsString(uValue))
		ELSE
			uValue := cCurrentText
			SELF:ValueChanged := !(cOldValue == uValue)
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/TextControl.Destroy/*" />
	METHOD Destroy() AS USUAL CLIPPER
		oFont		:= NULL_OBJECT
		oToolTip	:= NULL_OBJECT
		SUPER:Destroy()
		RETURN SELF

/// <include file="Gui.xml" path="doc/TextControl.EnableAutoComplete/*" />
	METHOD EnableAutoComplete(dwFlags AS DWORD) AS VOID STRICT
		// Todo: Implement EnableAutoComplete
		//DEFAULT( REF dwFlags,SHACF_DEFAULT)
		//RETURN ShellAutoComplete(SELF:handle(),dwFlags)
		RETURN

/// <include file="Gui.xml" path="doc/TextControl.Font/*" />
	METHOD Font(oNewFont, lRescal)
		LOCAL oOldFont AS Font
		LOCAL lRescalCntl AS LOGIC
		LOCAL hFont AS PTR

		IF !IsNil(oNewFont)
			if !(oNewFont is Font)
				WCError{#Font, #TextControl, __WCSTypeError, oNewFont,1}:Throw()
			ENDIF
		ENDIF

		IF IsNil(lRescal)
			lRescalCntl := FALSE
		ELSE
			lRescalCntl := lRescal
		ENDIF

		oOldFont := SELF:ControlFont
		oFont := oNewFont
		IF (oFont != NULL_OBJECT)
			oFont:Create()
		ELSE
			hFont := GuiWin32.GetStockObject(DEFAULT_GUI_FONT)
			IF hFont == NULL_PTR
				hFont := GuiWin32.GetStockObject(SYSTEM_FONT)
			ENDIF
			oFont := System.Drawing.Font.FromHfont(hFont)
		ENDIF
		SELF:Create()
		IF SELF:__IsValid
			oCtrl:Font := oFont
		//ELSE
			//oCtrl := oCtrl
		ENDIF
		IF lRescalCntl
			SELF:__RescalCntlB(oFont)
		ENDIF
		RETURN oOldFont

/// <include file="Gui.xml" path="doc/TextControl.Ime/*" />
	METHOD Ime(symIme)
		IF (symIme == NIL)
			RETURN SELF:symImeFlag
		ENDIF
		SELF:symImeFlag := symIme
		RETURN symIme

/// <include file="Gui.xml" path="doc/TextControl.ctor/*" />
	CONSTRUCTOR(oOwner, xId, oPoint, oDimension, cRegclass, kStyle, lDataAware)
		symImeFlag := #AUTO
		SUPER(oOwner, xId, oPoint, oDimension, cRegclass, kStyle, lDataAware)
        RETURN

/// <include file="Gui.xml" path="doc/TextControl.Length/*" />
	ACCESS Length AS LONG
		LOCAL lRetVal AS LONGINT
		IF SELF:ValidateControl()
			lRetVal := oCtrl:Text:Length
		ENDIF

		RETURN lRetVal

/// <include file="Gui.xml" path="doc/TextControl.RemoveEditBalloonTip/*" />
	METHOD RemoveEditBalloonTip(hControl)
		IF oToolTip != NULL_OBJECT
			oToolTip:RemoveAll()
		ENDIF
		RETURN SELF

	[DllImport("user32.dll", EntryPoint := "SendMessage")];
	STATIC METHOD winSendMessage(hWnd AS IntPtr, msg AS INT, wParam AS LONG, [MarshalAs(UnmanagedType.LPWStr)] lParam AS STRING ) AS LOGIC

/// <include file="Gui.xml" path="doc/TextControl.SetCueBanner/*" />
	METHOD SetCueBanner(cText AS STRING) AS LOGIC
		//PP-030902
		RETURN SELF:SetCueBanner(cText, SELF:Handle())


	METHOD SetCueBanner(cText AS STRING,hControl AS IntPtr) AS LOGIC
		//PP-030902
		LOCAL lReturn AS LOGIC
		lReturn := winSendMessage(hControl,EM_SETCUEBANNER,0, (STRING) cText)
		RETURN lReturn

/// <include file="Gui.xml" path="doc/TextControl.ShowEditBalloonTip/*" />
	METHOD ShowEditBalloonTip(cTitle,cText,dwIcon)
		// Requires XP or greater
		IF oToolTip == NULL_OBJECT
			oToolTip := System.Windows.Forms.ToolTip{}
		ENDIF
		IF IsString(cTitle)
			oToolTip:ToolTipTitle := cTitle
		ENDIF
		IF IsNumeric(dwIcon)
			oToolTip:ToolTipIcon := (System.Windows.Forms.ToolTipIcon) dwIcon
		ENDIF
		oToolTip:SetToolTip((System.Windows.Forms.Control) oCtrl, cText)
		RETURN SELF

/// <include file="Gui.xml" path="doc/TextControl.TextColor/*" />
	ACCESS TextColor AS Color
		IF SELF:__IsValid .and. SELF:oTextColor == NULL_OBJECT
			oTextColor := oCtrl:ForeColor
		ENDIF
		RETURN oTextColor

/// <include file="Gui.xml" path="doc/TextControl.TextColor/*" />
	ASSIGN TextColor(oColor AS Color)
		LOCAL dwDefaultColor AS DWORD
		LOCAL dwNewColor AS DWORD
		LOCAL dwOldColor AS DWORD

		dwDefaultColor := GuiWin32.GetSysColor(COLOR_WINDOWTEXT)
		dwNewColor     := oColor:ColorRef
		IF lManageColor
			dwOldColor := oTextColor:ColorRef
		ELSE
			dwOldColor := dwDefaultColor
		ENDIF

		oTextColor := oColor

		//Check if we need to revert to system default
		lManageColor := (dwNewColor != dwDefaultColor)

		IF SELF:__IsValid .and. dwNewColor != dwOldColor
			oCtrl:ForeColor := oColor
			oCtrl:Invalidate()
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/TextControl.TextValue/*" />
	ACCESS TextValue  AS STRING
		RETURN SELF:__GetText()

/// <include file="Gui.xml" path="doc/TextControl.TextValue/*" />
	ASSIGN TextValue(cNewCaption AS STRING)
		LOCAL cTextValue AS STRING
		LOCAL cOldValue AS STRING

		cTextValue := SELF:__SetText(cNewCaption)
		cOldValue := AsString(uValue)

		IF SELF:FieldSpec != NULL_OBJECT
			uValue := SELF:FieldSpec:Val(cTextValue)
			SELF:ValueChanged := !(cOldValue == AsString(uValue))
		ELSE
			uValue := cTextValue
			SELF:ValueChanged := !(cOldValue == uValue)
		ENDIF

		RETURN

END CLASS

#region defines
DEFINE EM_SETCUEBANNER := (ECM_FIRST + 1)
DEFINE EM_GETCUEBANNER := (ECM_FIRST + 2)
DEFINE EM_SHOWBALLOONTIP := (ECM_FIRST + 3)
DEFINE EM_HIDEBALLOONTIP := (ECM_FIRST + 4)
DEFINE TTI_NONE := 0
DEFINE TTI_INFO := 1
DEFINE TTI_WARNING := 2
DEFINE TTI_ERROR := 3
#endregion
