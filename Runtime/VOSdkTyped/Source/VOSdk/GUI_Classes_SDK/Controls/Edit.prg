USING System.Reflection
/// <include file="Gui.xml" path="doc/Edit/*" />

[XSharp.Internal.TypesChanged];
CLASS Edit INHERIT TextControl
	PROTECT lNoNotify AS LOGIC
	PROTECT lForceModFlag2True AS LOGIC

    PROPERTY ControlType AS ControlType GET ControlType.Sle


/// <include file="Gui.xml" path="doc/Edit.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)
		LOCAL dwStyle AS DWORD
		IF !IsInstanceOfUsual(xID,#ResourceID)
			dwStyle:= _OR(WS_CHILD, WS_CLIPSIBLINGS)
			IF !IsNil(kStyle)
				dwStyle := _OR(DWORD(kStyle), dwStyle)
			ENDIF
			SUPER(oOwner, xID, oPoint, oDimension, "Edit", dwStyle, TRUE)
		ELSE
			SUPER(oOwner, xID, oPoint, oDimension, , kStyle, TRUE)
		ENDIF

		RETURN


	ACCESS __TextBox AS IVOTextBox
		RETURN (IVOTextBox) oCtrl


	ASSIGN __ForceModFlag2True(lNewValue AS LOGIC)  STRICT
		lForceModFlag2True := lNewValue

	ACCESS __NoNotify AS LOGIC STRICT
		RETURN lNoNotify

	/// <inheritdoc />
	METHOD CanUndo()
		IF SELF:ValidateControl()
			RETURN SELF:__TextBox:CanUndo
		ENDIF

		RETURN FALSE

	/// <inheritdoc />
	ACCESS Caption AS STRING
		RETURN cCaption

	/// <inheritdoc />
	ASSIGN Caption(cNewCaption AS STRING)
		cCaption := cNewCaption

	METHOD ValidateControl() AS USUAL // usual wtf
		LOCAL lret := SUPER:ValidateControl() AS LOGIC
		IF lret .AND. SELF:__TextBox != NULL
			RETURN lret
		ELSE
			RETURN FALSE
		ENDIF

	/// <inheritdoc />
	METHOD Clear()
		IF SELF:ValidateControl()
			SELF:__TextBox:Clear()
		ENDIF
		RETURN SELF

	/// <inheritdoc />
	METHOD Copy()  CLIPPER
		IF SELF:ValidateControl()
			SELF:__TextBox:Copy()
		ENDIF
		RETURN SELF

	/// <inheritdoc />
	METHOD Cut()
		IF SELF:ValidateControl()
			SELF:__TextBox:Cut()
		ENDIF
		RETURN SELF

	/// <inheritdoc />
	METHOD Font(oNewFont, lRescal)
		LOCAL uRet AS USUAL
		LOCAL oMargins AS Dimension

		IF SELF:ValidateControl()
			oMargins := SELF:Margins
			uRet := SUPER:Font(oNewFont, lRescal)
			SELF:Margins := oMargins
		ENDIF

		RETURN uRet

/// <include file="Gui.xml" path="doc/Edit.IsPassword/*" />
	METHOD IsPassword()
		IF SELF:ValidateControl()
			RETURN __TextBox:UseSystemPasswordChar
		ENDIF
		RETURN _AND(dwStyle,ES_PASSWORD)!=0

/// <include file="Gui.xml" path="doc/Edit.Margins/*" />
	ACCESS Margins  AS Dimension
		IF SELF:ValidateControl()
			VAR nLeft := __TextBox:Margin:Left
			VAR nTop  := __TextBox:Margin:Top
            RETURN Dimension{nLeft, nTop}
        ENDIF
        RETURN Dimension{0, 0}

/// <include file="Gui.xml" path="doc/Edit.Margins/*" />
	ASSIGN Margins(oNewMargins AS Dimension)
		IF SELF:ValidateControl()
			LOCAL oPadding AS System.Windows.Forms.Padding
			oPadding := __TextBox:Margin
			IF oPadding:Left != oNewMargins:Width .or. oPadding:Right != oNewMargins:Height
				oPadding:Left := oNewMargins:Width
				oPadding:Right := oNewMargins:Height
				__TextBox:Margin := oPadding
			ENDIF
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/Edit.Modified/*" />
	ACCESS Modified AS LOGIC
		IF SELF:ValidateControl()
			IF lForceModFlag2True
				RETURN TRUE
			ELSE
				RETURN SELF:__TextBox:Modified
			ENDIF
		ENDIF

		RETURN FALSE

/// <include file="Gui.xml" path="doc/Edit.Modified/*" />
	ASSIGN Modified(lModified  AS LOGIC)
		IF SELF:ValidateControl()
			SELF:__TextBox:Modified := lModified
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/Edit.Paste/*" />
	METHOD Paste(cNewString)
		IF SELF:ValidateControl()
			IF IsNil(cNewString)
				SELF:__TextBox:Clear()
				SELF:__TextBox:Paste()
			ELSE
				IF !IsString(cNewString)
					WCError{#Paste,#Edit,__WCSTypeError,cNewString,1}:Throw()
				ENDIF
				SELF:__TextBox:SelectedText := cNewString
			ENDIF
		ENDIF

		RETURN SELF




/// <include file="Gui.xml" path="doc/Edit.ReadOnly/*" />
	ACCESS ReadOnly  AS LOGIC
		IF SELF:ValidateControl()
			RETURN SELF:__TextBox:ReadOnly
		ENDIF
		RETURN (_AND(dwStyle, ES_READONLY) > 0)


/// <include file="Gui.xml" path="doc/Edit.ReadOnly/*" />
	ASSIGN ReadOnly(lNewValue AS LOGIC)

		IF SELF:ValidateControl()
			SELF:__TextBox:ReadOnly := lNewValue
		ENDIF
		SELF:SetStyle(ES_READONLY, lNewValue)
		RETURN

/// <include file="Gui.xml" path="doc/Edit.SelectedText/*" />
	ACCESS SelectedText AS STRING
		LOCAL cReturn AS STRING

		IF SELF:ValidateControl()
			cReturn := __TextBox:SelectedText
		ENDIF
		RETURN cReturn

/// <include file="Gui.xml" path="doc/Edit.SelectedText/*" />
	ASSIGN SelectedText(cNewString AS STRING)

		IF SELF:ValidateControl()
			__TextBox:SelectedText := cNewString
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/Edit.Selection/*" />
	ACCESS Selection AS Selection
		LOCAL dwStart AS LONG
		LOCAL dwFinish AS LONG

		IF SELF:ValidateControl()
			dwStart		:= __TextBox:SelectionStart+1
			dwFinish	:= dwStart	+ __TextBox:SelectionLength
			RETURN Selection{dwStart, dwFinish}
		ENDIF

		RETURN Selection{0,0}

/// <include file="Gui.xml" path="doc/Edit.Selection/*" />
	ASSIGN Selection(oSel AS Selection)
		IF SELF:ValidateControl()
			IF oSel:Start == 0 .and. oSel:Finish == -1
				SELF:__TextBox:SelectionStart  := 0
				SELF:__TextBox:SelectionLength := SELF:__TextBox:Text:Length
			ELSE
				IF oSel:Finish - oSel:Start > 0
					SELF:__TextBox:SelectionStart	:= Math.MAX(oSel:Start-1 ,0)
				ELSE
					SELF:__TextBox:SelectionStart	:= Math.MAX(oSel:Start ,0)
				ENDIF
				SELF:__TextBox:SelectionLength  := oSel:Finish - oSel:Start
			ENDIF
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/Edit.SelectAll/*" />
	METHOD SelectAll()
		// Selects the whole text in an edit control
		IF SELF:ValidateControl()
			SELF:__TextBox:SelectionStart := 0
			SELF:__TextBox:SelectionLength := SELF:__TextBox:TextLength
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/Edit.SelectNone/*" />
	METHOD SelectNone()
		// Deselects the text in an edit control
		IF SELF:ValidateControl()

			__TextBox:SelectionLength := 0
		ENDIF

		RETURN NIL

/// <include file="Gui.xml" path="doc/Edit.SetSelectionFocus/*" />
	METHOD SetSelectionFocus()
		((Window) SELF:Owner):SetFocus()
		SELF:SetFocus()
		SELF:Selection := Selection{0,SLen(SELF:TextValue)+1}

		RETURN NIL

/// <include file="Gui.xml" path="doc/Edit.TextLimit/*" />
	ACCESS TextLimit AS LONG
		IF SELF:ValidateControl()
			RETURN __TextBox:MaxLength
		ENDIF

		RETURN 0

/// <include file="Gui.xml" path="doc/Edit.TextLimit/*" />
	ASSIGN TextLimit(nChars AS LONG)

		IF SELF:ValidateControl()
			__TextBox:MaxLength := nChars
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/Edit.TextValue/*" />
	ACCESS TextValue AS STRING
		RETURN SELF:__GetText()

/// <include file="Gui.xml" path="doc/Edit.TextValue/*" />
	ASSIGN TextValue(cNewText AS STRING)
		LOCAL cTextValue AS STRING
		LOCAL cOldValue AS STRING

		cOldValue := AsString(uValue)
		cTextValue := SELF:__SetText(cNewText)

		IF IsInstanceOfUsual(SELF:FieldSpec, #FieldSpec)
			uValue := SELF:FieldSpec:Val(cTextValue)
		ELSE
			uValue := cTextValue
		ENDIF

		SELF:ValueChanged := !(cOldValue == AsString(uValue))

		RETURN

/// <include file="Gui.xml" path="doc/Edit.Undo/*" />
	METHOD Undo()
		IF SELF:ValidateControl()
			__TextBox:Undo()
		ENDIF

		RETURN FALSE
END CLASS

ENUM FocusSelect
	MEMBER Home     := FSEL_HOME
	MEMBER @@End    := FSEL_END
	MEMBER TrimEnd  := FSEL_TRIMEND
	MEMBER Trim	    := FSEL_TRIM
END ENUM

ENUM OverWriteMode
	MEMBER Allways := OVERWRITE_ALWAYS
	MEMBER OnKey   := OVERWRITE_ONKEY
	MEMBER Never   := OVERWRITE_NEVER
END ENUM

ENUM ScrollMode
	MEMBER Full := SCRMODE_FULL
	MEMBER No   := SCRMODE_NO
	MEMBER Part := SCRMODE_PART
END ENUM

/// <include file="Gui.xml" path="doc/SingleLineEdit/*" />
CLASS SingleLineEdit INHERIT Edit
	PROTECT oEditString			AS __FormattedString
	PROTECT wOverWrite			AS OverWriteMode
	PROTECT wScrMode			AS ScrollMode
	PROTECT wFocusSel			AS FocusSelect
	PROTECT lAllowSelection		AS LOGIC
	PROTECT lAutoFocusChange	AS LOGIC
	PROTECT _iStart				AS LONG // These 2 vars are used to save position and length of the selection
	PROTECT _iLen				AS LONG // because the DotNet textBox changes the selection when a mouse button is clicked.

    METHOD OnControlCreated(oC AS IVOControl) AS VOID
		VAR oTextBox := (IVOTextBox) oC
		SELF:RegisterEvents(oTextBox)
		RETURN

	METHOD RegisterEvents(oTb AS IVOTextBox) AS VOID STRICT
		oTb:KeyDown += OnKeyDown
		oTb:KeyPress += OnKeyPress
		oTb:GotFocus += OnGotFocus
		oTb:MouseClick += OnMouseClick
		RETURN

	METHOD UnRegisterEvents() AS VOID STRICT
		oCtrl:KeyDown -= OnKeyDown
		oCtrl:KeyPress -= OnKeyPress
		oCtrl:GotFocus -= OnGotFocus
		oCtrl:MouseClick -= OnMouseClick
		RETURN

	VIRTUAL METHOD OnKeyDown(Sender AS OBJECT, e AS System.Windows.Forms.KeyEventArgs) AS VOID
		LOCAL lShiftOn AS LOGIC
		LOCAL lCtrlOn AS LOGIC
		LOCAL lExtSel AS LOGIC
		IF SELF:oEditString == NULL_OBJECT
			RETURN
		ENDIF
		lShiftOn := e:Shift
		lCtrlOn  := e:Control
		lExtSel := lShiftOn .AND. (e:KeyValue == VK_LEFT .or. ;
				e:KeyValue == VK_RIGHT .or. ;
				e:KeyValue == VK_HOME .or. ;
				e:KeyValue == VK_END )
		IF !((lAllowSelection .AND. lExtSel) .OR. lCtrlOn )
			IF oEditString:ProcessKeyEvent(KeyEvent{e})
				e:Handled := TRUE
			ENDIF
		ENDIF
		RETURN

	VIRTUAL METHOD OnKeyPress(Sender AS OBJECT, e AS System.Windows.Forms.KeyPressEventArgs) AS VOID
		IF SELF:oEditString == NULL_OBJECT
			RETURN
		ENDIF
		IF oEditString:ProcessKeyEvent(KeyEvent{e})
			e:Handled := TRUE
		ENDIF
		RETURN

	PROPERTY __AllowSelection AS LOGIC GET lAllowSelection SET lAllowSelection := Value
 /// <exclude />
	ACCESS __CurPos AS LONGINT STRICT
		LOCAL liStart AS LONGINT
		IF SELF:ValidateControl()
			liStart := __TextBox:SelectionStart+1
		ENDIF
		RETURN liStart

 /// <exclude />
	ASSIGN __CurPos(iNewPos AS LONGINT)  STRICT
		IF SELF:ValidateControl()
			IF (iNewPos > 0)
				__TextBox:SelectionStart := iNewPos-1
			ELSE
				__TextBox:SelectionStart := 0
			ENDIF
			__TextBox:SelectionLength := 0
		ENDIF
		RETURN

 /// <exclude />
	ACCESS __EditString AS __FormattedString STRICT
		RETURN oEditString

 /// <exclude />
	ACCESS __FSLength AS DWORD STRICT
		IF (oFieldSpec != NULL_OBJECT)
			RETURN oFieldSpec:Length
		ENDIF
		RETURN 0XFFFF

 /// <exclude />
	METHOD __Update() AS VOID STRICT
		LOCAL cText,cNumText AS STRING
		LOCAL cNewText AS STRING
		LOCAL uOldValue AS USUAL

		IF (NULL_STRING == SELF:Picture)
			SUPER:__Update()
            RETURN
		ELSEIF SELF:Modified
			cText := SELF:TextValue
			uOldValue := AsString(uValue)

			IF (oFieldSpec != NULL_OBJECT)
				IF (oFieldSpec:ValType == "N")
					IF (oEditString != NULL_OBJECT)
						cNumText := SubStr(cText, oEditString:NextEditPos(0))
					ELSE
						cNumText := cText
					ENDIF
					IF (oEditString != NULL_OBJECT) .AND. (At2(",", oEditString:Picture) != 0)
						cNumText := StrTran(cNumText, Chr(SetThousandSep()), "")
					ENDIF
					cNumText := StrTran(cNumText, " ", "")
					uValue := Val(cNumText)
				ELSEIF (IVarGet(oFieldSpec, #Nullable) == TRUE)
					uValue := Unformat(cText, SELF:Picture, oEditString:Type+"0")
				ELSE
					uValue := Unformat(cText, SELF:Picture, oEditString:Type)
				ENDIF
			ELSE
				uValue := Unformat(cText, SELF:Picture, oEditString:Type)
			ENDIF

		    cNewText := Transform(uValue, SELF:Picture)
			IF !(cNewText == cText)
				SELF:TextValue := cNewText
			ENDIF
			SELF:Modified := FALSE
			SELF:ValueChanged := !(uOldValue == AsString(uValue))
		ENDIF
		RETURN

 /// <exclude />
	ASSIGN __Value(uNewValue AS USUAL)  STRICT

		IF (oEditString != NULL_OBJECT .AND. (oFieldSpec == NULL_OBJECT))
			oEditString:UsualValue := uNewValue
			SELF:uValue := uNewValue
			oHLStatus := NULL_OBJECT
			SELF:Modified := FALSE
			SELF:lChanged := TRUE
		ELSE
			SUPER:__Value := uNewValue
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/SingleLineEdit.AutoFocusChange/*" />
	PROPERTY AutoFocusChange  AS LOGIC GET lAutoFocusChange SET lAutoFocusChange := Value

	METHOD Copy() CLIPPER
		SUPER:Copy()
		RETURN SELF

	METHOD Cut()
		IF SELF:oEditString != NULL_OBJECT
			SELF:oEditString:Cut()
		ELSE
			SUPER:Cut()
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/SingleLineEdit.CreateFormattedString/*" />
	METHOD CreateFormattedString(cPicture, cType, cDefTempl)
		oEditString := __FormattedString{SELF, cPicture, cType, wOverWrite, cDefTempl, wScrMode}
		RETURN SELF


/// <include file="Gui.xml" path="doc/SingleLineEdit.FieldSpec/*" />
	ASSIGN FieldSpec(oNewFS as FieldSpec)
		LOCAL sPic AS STRING
		LOCAL pos AS USUAL
		LOCAL vt AS STRING
		LOCAL sDefTempl AS STRING

		SUPER:FieldSpec := oNewFS

		vt := oFieldSpec:ValType
		DO CASE
		CASE (vt == "C")
			sDefTempl := Replicate("X", Math.Min((INT)  oFieldSpec:Length, MAX_FMTSTRING_LEN))
		CASE (vt == "L")
			sDefTempl := "L"
		CASE oFieldSpec:ValType == "N"
			sDefTempl := Replicate("#", Math.Min((INT) oFieldSpec:Length, MAX_FMTSTRING_LEN))
			IF (oFieldSpec:Decimals > 0)
				pos := oFieldSpec:Length - oFieldSpec:Decimals
				sDefTempl := Left(sDefTempl, Pos - 1) + "." + Replicate("#", oFieldSpec:Decimals)
			ENDIF
		ENDCASE

		IF (NULL_STRING != SELF:Picture)
			sPic := SELF:Picture
			ELSEIF	(NULL_STRING != oFieldSpec:Picture)
			sPic := oFieldSpec:Picture
		ELSEIF (vt == "D")
			sPic := "@D"
		ELSEIF !__DBCSEnabled()
			sPic := sDefTempl
		ENDIF

		IF (NULL_STRING != sPic)
			SELF:CreateFormattedString(sPic, vT, sDefTempl)
		ENDIF

		DO CASE
			//RvdH 050602 Added default value for STRINGS (issue 13031)
		CASE (vt == "C")
			SELF:__Value := ""
			SELF:__TextBox:TextAlign := System.Windows.Forms.HorizontalAlignment.Left
		CASE (vt == "L")
			SELF:__Value := FALSE
			SELF:__TextBox:TextAlign := System.Windows.Forms.HorizontalAlignment.Left
		CASE (vt == "N")
			SELF:__TextBox:TextAlign := System.Windows.Forms.HorizontalAlignment.Right
			IF (oFieldSpec:Decimals > 0)
				SELF:__Value := 0.0
			ELSE
				SELF:__Value := 0
			ENDIF
		ENDCASE

		SELF:Modified := FALSE

		RETURN

	METHOD OnMouseClick(sender AS OBJECT, e AS System.Windows.Forms.MouseEventArgs) AS VOID
		IF _iLen >= 0 .and. _iStart >= 0
			SELF:__TextBox:SelectionStart := _iStart
			SELF:__TextBox:SelectionLength := _iLen
			_iStart := _iLen := -1
		ENDIF
		RETURN

	METHOD OnGotFocus(sender AS OBJECT, e AS System.EventArgs) AS VOID
		LOCAL iPos AS INT
		LOCAL iLen	AS INT
		IF (oEditString != NULL_OBJECT)
			oEditString:lendkey := FALSE
		ENDIF
		iLen := 0
		iPos := 1
		IF (wFocusSel == FocusSelect.Home)
			IF (oEditString != NULL_OBJECT)
				iPos := 1
				IF !oEditString:IsEditPos(iPos)
					iPos := oEditString:NextEditPos(iPos)
				ENDIF
				iPos--
			ENDIF

		ELSEIF (wFocusSel == FocusSelect.End)
			IF (oEditString != NULL_OBJECT)
				iPos := oEditString:PrevEditPos(SELF:CurrentText:Length)
			ELSE
				iPos := SELF:CurrentText:Length
			ENDIF
		ELSEIF (wFocusSel == FocusSelect.Trim)
			iLen := SELF:CurrentText:TrimEnd():Length
		ELSEIF (wFocusSel == FocusSelect.TrimEnd)
			iPos := SELF:CurrentText:TrimEnd():Length
		ENDIF
		IF SELF:__IsValid
			IF iPos > 0
				SELF:__TextBox:SelectionStart := iPos-1
			ELSE
				SELF:__TextBox:SelectionStart := 0
			ENDIF
			SELF:__TextBox:SelectionLength := iLen
			// Save start & Length, so they can be restored in OnMouseClick when the focus was changed by clicking on the textbox
			_iStart := SELF:__TextBox:SelectionStart
			_iLen   := SELF:__TextBox:SelectionLength
		ENDIF
		RETURN
/// <include file="Gui.xml" path="doc/SingleLineEdit.FocusSelect/*" />

	ACCESS FocusSelect AS LONG
		RETURN (LONG) wFocusSel
/// <include file="Gui.xml" path="doc/SingleLineEdit.FocusSelect/*" />

	ASSIGN FocusSelect(wNewValue AS LONG)
		IF (wNewValue < 0) .OR. (wNewValue > 4)
			wNewValue := FSEL_TRIM
		ENDIF
		wFocusSel := (FocusSelect) wNewValue

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)
		SUPER(oOwner, xID, oPoint, oDimension, kStyle)
		wOverWrite      := OverwriteMode.Never
		wScrMode        := ScrollMode.Full
		wFocusSel       := FocusSelect.Trim
		lAllowSelection := TRUE

		RETURN

/// <include file="Gui.xml" path="doc/SingleLineEdit.OverWrite/*" />
	ACCESS OverWrite AS LONG
		RETURN wOverWrite


/// <include file="Gui.xml" path="doc/SingleLineEdit.OverWrite/*" />
	ASSIGN OverWrite(wNewValue AS LONG)

		IF (wNewValue < 0) .OR. (wNewValue > 2)
			wNewValue := OVERWRITE_NEVER
		ENDIF

		wOverWrite := (OverwriteMode) wNewValue
		IF (oEditString != NULL_OBJECT)
			oEditString:wOverWrite := wOverWrite
		ENDIF

		RETURN

	METHOD Paste(sNewString)
		IF SELF:oEditString != NULL_OBJECT
			SELF:oEditString:Paste()
		ELSE
			SUPER:Paste(sNewString)
		ENDIF
		RETURN SELF

/// <include file="Gui.xml" path="doc/SingleLineEdit.Picture/*" />
	ACCESS Picture AS STRING
		IF oEditString == NULL_OBJECT
			RETURN NULL_STRING
		ENDIF
		RETURN oEditString:Picture

/// <include file="Gui.xml" path="doc/SingleLineEdit.Picture/*" />
	ASSIGN Picture(cNewPicture AS STRING)
		LOCAL uOldVal AS USUAL

		IF (oEditString == NULL_OBJECT)
			lNoNotify := TRUE
			SELF:CreateFormattedString(cNewPicture, "C", NIL)
			lNoNotify := FALSE
		ELSE
			IF (cNewPicture == NULL_STRING)
				oEditString := NULL_OBJECT
				RETURN
			ELSE
				uOldVal := uValue // new for 2.0b, should allow picture change on the fly
				oEditString:Picture := cNewPicture
				oEditString:UsualValue := uOldVal // new for 2.0b
			ENDIF
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/SingleLineEdit.ScrollMode/*" />
	ACCESS ScrollMode AS LONG
		RETURN (LONG) wScrMode

/// <include file="Gui.xml" path="doc/SingleLineEdit.ScrollMode/*" />
	ASSIGN ScrollMode(wNewValue AS LONG)
		IF (wNewValue < 0) .OR. (wNewValue > 2)
			wNewValue := SCRMODE_FULL
		ENDIF

		wScrMode := (ScrollMode) wNewValue
		IF (oEditString != NULL_OBJECT)
			oEditString:wScrMode := wScrMode
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/SingleLineEdit.TextValue/*" />
	ACCESS TextValue AS STRING
		LOCAL cText			AS STRING
		LOCAL cOrigText		AS STRING
		LOCAL cFormat		AS STRING
		LOCAL nPosDay		AS DWORD
		LOCAL nPosMonth		AS DWORD
		LOCAL nDay			AS INT
		LOCAL nMonth		AS INT

		// DHer: 18/12/2008
		// Error in VO : Date "25/2 /2005" gets translated to in "25/04/2005" !!!  (in DD/MM/YYYY format)

		cOrigText := SUPER:TextValue
		cText := cOrigText
		IF SELF:oFieldSpec<>NULL_OBJECT
			IF SELF:oFieldSpec:ValType="D"				// When Date
				IF !SELF:__TextBox:Focused
					IF At2(" ",cText) <> 0			   // When space in cText
						cFormat     := Upper(GetDateFormat())
						nPosDay     := At("DD",cFormat)
						nPosMonth   := At("MM",cFormat)
						IF nPosDay<>0
							nDay := Val(SubStr(cText,nPosDay,2))
							IF nDay<>0
								cText := Stuff(cText, nPosDay, 2, StrZero(nDay,2,0))
							ENDIF
						ENDIF
						IF nPosMonth<>0
							nMonth := Val(SubStr(cText,nPosMonth,2))
							IF nMonth<>0
								cText := Stuff(cText, nPosMonth, 2, StrZero(nMonth,2,0))
							ENDIF
						ENDIF
						IF nDay<>0 .AND. nMonth<>0
							IF cOrigText<>cText
								SELF:__SetText(cText)
							ENDIF
						ENDIF
					ENDIF
				ENDIF
			ENDIF
		ENDIF

		RETURN cText

/// <include file="Gui.xml" path="doc/SingleLineEdit.TextValue/*" />
	ASSIGN TextValue(cNewText AS STRING)
		SUPER:TextValue := cNewText

		IF (oEditString != NULL_OBJECT)
			IF (IsInstanceOf(oFieldSpec, #FieldSpec) .AND. (IVarGet(oFieldSpec, #Nullable) == TRUE))
				uValue := Unformat(cNewText, SELF:Picture, oEditString:Type+"0")
			ELSE
				uValue := Unformat(cNewText, SELF:Picture, oEditString:Type)
			ENDIF
			oEditString:UsualValue := uValue
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/SingleLineEdit.Undo/*" />
	METHOD Undo()
		IF (oEditString != NULL_OBJECT)
			RETURN oEditString:Undo()
		ENDIF
		RETURN SUPER:Undo()

END CLASS


/// <exclude/>
CLASS strucPictureFuncFlags
	PROPERTY Flags       AS System.Collections.BitArray AUTO
	PROPERTY lLeftJust   AS LOGIC GET Flags[0]
	PROPERTY lDispCR     AS LOGIC GET Flags[1]
	PROPERTY lSetDate    AS LOGIC GET Flags[2]
	PROPERTY lBritDate   AS LOGIC GET Flags[3]
	PROPERTY lInsNonTemp AS LOGIC GET Flags[4]
	PROPERTY lDispDB     AS LOGIC GET Flags[5]
	PROPERTY lZeroBlank  AS LOGIC GET Flags[6]
	PROPERTY lNegInParen AS LOGIC GET Flags[7]
	PROPERTY lConvUpper  AS LOGIC GET Flags[8]
	PROPERTY lAlphaOnly  AS LOGIC GET Flags[9]
	CONSTRUCTOR
		Flags := System.Collections.BitArray{10}
END CLASS

CLASS SpinnerEdit INHERIT SingleLineEdit
	PROTECT oTextBox AS IVOTextBox

    METHOD OnControlCreated(oC AS IVOControl) AS VOID
		LOCAL oSpinner  AS IVOSpinnerTextBox
		oSpinner := (IVOSpinnerTextBox) oC
		oSpinner:KeyDown += OnKeyDown
		oSpinner:KeyPress += OnKeyPress
		oSpinner:GotFocus += OnGotFocus
		oSpinner:MouseClick += OnMouseClick
		LOCAL oProp	    AS PropertyInfo
		LOCAL oType		AS System.Type
		oType    := oSpinner:GetType()
		oProp    := oType:GetProperty("TextBox", BindingFlags.Instance+ BindingFlags.NonPUBLIC+BindingFlags.IgnoreCase)
		IF oProp != NULL_OBJECT
			oTextBox := oProp:GetValue(oSpinner,NULL)
		ENDIF
		RETURN

	ACCESS __TextBox AS IVOTextBox
		RETURN SELF:oTextBox

	PROPERTY __UpDownControl AS IVOSpinnerTextBox GET (VOSpinnerTextBox) oCtrl
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)
		SUPER(oOwner, xID, oPoint, oDimension, kStyle)


	PROPERTY Client     AS SpinnerEdit GET SELF
	PROPERTY IsHexBased AS LOGIC GET __UpDownControl:Hexadecimal SET __UpDownControl:Hexadecimal := Value
	PROPERTY MaxValue	AS LONG GET (LONG) Convert.ToInt32(__UpDownControl:Maximum) SET __UpDownControl:Maximum := Value
	PROPERTY MinValue	AS LONG GET (LONG) Convert.ToInt32(__UpDownControl:Minimum) SET __UpDownControl:Minimum := Value
	PROPERTY Position	AS LONG GET (LONG) Convert.ToInt32(__UpDownControl:Value) SET __UpDownControl:Value := Math.Max(Math.min(Value, MaxValue), MinValue)
	PROPERTY Range		AS Range
		GET
			RETURN Range{SELF:MinValue, SELF:MaxValue}
		END GET
		SET
			SELF:MinValue := Value:Min
			SELF:MaxValue := Value:Max
		END SET
	END PROPERTY
	PROPERTY ThumbPosition AS LONG GET Convert.ToInt32(__UpDownControl:Value) SET __UpDownControl:Value := Value
	PROPERTY Value		AS USUAL GET SELF:Position SET SELF:Position := (LONG) Value

END CLASS


DEFINE FSEL_ALL := 0
DEFINE FSEL_END := 3
DEFINE FSEL_HOME := 2
DEFINE FSEL_TRIM := 1
DEFINE FSEL_TRIMEND := 4
DEFINE MAX_FMTSTRING_LEN := 4096

DEFINE OVERWRITE_ALWAYS := 2

DEFINE OVERWRITE_NEVER := 0
DEFINE OVERWRITE_ONKEY := 1
DEFINE SCRMODE_FULL := 0
DEFINE SCRMODE_NO := 2

DEFINE SCRMODE_PART := 1

