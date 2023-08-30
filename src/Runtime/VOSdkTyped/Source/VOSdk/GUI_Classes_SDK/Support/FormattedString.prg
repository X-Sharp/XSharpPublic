//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

DEFINE DEFAULT_STRING_TEMPL_SIZE := 128
/// <exclude />
class __FormattedString
	PROTECT oEditOwner AS SingleLineEdit
	PROTECT sPicture AS STRING
	PROTECT sTemplate AS STRING
	PROTECT sValue AS STRING
	PROTECT sOldValue AS STRING
	PROTECT FuncFlags AS strucPictureFuncFlags
	PROTECT cType AS STRING
	PROTECT wDecSep AS DWORD		//RvdH 070205 changed from WORD to DWORD
	PROTECT wThousSep AS DWORD		//RvdH 070205 changed from WORD to DWORD
	PROTECT iDelStart AS INT
	PROTECT iDelEnd AS INT
	PROPERTY iTemplLen AS INT               AUTO
	PROPERTY lEndKey AS LOGIC               AUTO
	PROPERTY wOverWrite AS OverwriteMode    AUTO
	PROPERTY wScrMode AS ScrollMode         AUTO

	METHOD AsString() AS STRING STRICT
		RETURN sValue

	ACCESS ReadOnly AS LOGIC
		RETURN oEditOwner:__TextBox:ReadOnly .or. !oEditOwner:__TextBox:Enabled

	METHOD Cut() AS LOGIC STRICT
		LOCAL iStart AS INT
		LOCAL iEnd AS INT
		LOCAL iLen AS INT
		IF SELF:ReadOnly
			RETURN FALSE
		ENDIF
		iStart := oEditOwner:__TextBox:SelectionStart
		iEnd   := iStart+oEditOwner:__TextBox:SelectionLength

		IF (iStart == iEnd)
			RETURN FALSE
		ENDIF

		iStart++
		iEnd++
		iLen := iEnd - iStart

		System.Windows.Forms.Clipboard.SetText( SubStr3(sValue, DWORD(iStart), DWORD(iLen)))

		SELF:DeleteSelection(iStart, iEnd)
		RETURN TRUE

	ACCESS DecimalPos AS INT STRICT
		RETURN INT(_CAST, At2(".", sTemplate))

	METHOD DeleteChar(iPos AS INT, lInvert AS LOGIC, lUpdateOwner AS LOGIC) AS VOID STRICT
		LOCAL iCurPos AS INT
		LOCAL iNextPos AS INT
		LOCAL sNextChar AS STRING
		IF SELF:ReadOnly
			RETURN
		ENDIF

		IF IsNil(iPos)
			iCurPos := oEditOwner:__CurPos
		ELSE
			iCurPos := iPos
		ENDIF

		IF !SELF:IsEditPos(iCurPos)
			SELF:InvalidAction()
			RETURN
		ENDIF

		IF !lInvert
			iNextPos := SELF:NextEditPos(iCurPos + 1)
		ELSE
			iNextPos := SELF:PrevEditPos(iCurPos - 1)
		ENDIF

		IF (wScrMode != ScrollMode.No)
			WHILE (iCurPos > 0) .AND. (iNextPos > 0)
				IF (wScrMode != ScrollMode.Full) .AND. (Abs(iNextPos - iCurPos) != 1)
					EXIT
				ENDIF
				sNextChar := CharPos(sValue, DWORD(iNextPos))
				// 2.5b TBD chars shouldn't count
				IF ((iNextPos >= iDelStart) .AND. (iNextPos <= iDelEnd)) .OR. (SELF:IsValidChar(sNextChar, iCurPos, TRUE))
					IF SELF:IsEditPos(iNextPos)
						SELF:PutChar(sNextChar, iCurPos)
						SELF:PutChar(" ", iNextPos)
					ELSE
						SELF:PutChar(" ", iCurPos)
					ENDIF
				ELSE
					EXIT
				ENDIF
				iCurPos := iNextPos
				IF !lInvert
					iNextPos := SELF:NextEditPos(iCurPos + 1)
				ELSE
					iNextPos := SELF:PrevEditPos(iCurPos - 1)
				ENDIF
			END
		ENDIF

		IF (SELF:IsEditPos(iCurPos))
			SELF:PutChar(" ", iCurPos)
		ENDIF

		IF lUpdateOwner
			SELF:UpdateEditOwner()
		ENDIF
		RETURN

	METHOD DeleteSelection(iStart AS INT, iEnd AS INT) AS VOID STRICT
		LOCAL i AS INT
		LOCAL lInvert AS LOGIC
		LOCAL iDelPos AS INT
		LOCAL iDecPos AS INT
		LOCAL iSignPos AS INT
		LOCAL lNumeric AS LOGIC
		IF SELF:ReadOnly
			RETURN
		ENDIF

		IF (iEnd <= iStart)
			iEnd := iStart
		ELSE
			iEnd--
		ENDIF

		iDelStart := iStart
		iDelEnd := iEnd

		lNumeric := (cType == "N")

		IF lNumeric .AND. ((iDecPos := INT(_CAST, At2(Chr(wDecSep), sValue))) > iStart) .AND. (iDecPos < iEnd)
			SELF:DeleteSelection(iStart, iDecPos)
			SELF:DeleteSelection(iDecPos+1, iEnd+1)
		ELSE
			lInvert := ((lNumeric) .AND. (At3(Chr(wDecSep), sValue, DWORD(iEnd)) > 0))
			IF !lInvert
				iDelPos := iStart
				FOR i:= iStart TO iEnd
					IF SELF:IsEditPos(i)
						SELF:DeleteChar(iDelPos, lInvert, FALSE)
						iDelEnd := iDelEnd-1
					ELSEIF (wScrMode == ScrollMode.Part)
						iDelPos := i+1
					ENDIF
				NEXT
			ELSE
				IF (lNumeric)
					iSignPos := INT(_CAST, At2("-", sValue))
					IF (iSignPos > 0)
						SELF:PutChar(" ", iSignPos)
					ENDIF
				ENDIF
				FOR i:= iEnd DOWNTO iStart
					iDelPos := iEnd
					IF SELF:IsEditPos(i)
						SELF:DeleteChar(iDelPos, lInvert, FALSE)
						iDelStart := iDelStart + 1
					ELSE
						iDelPos := i-1
					ENDIF
				NEXT
			ENDIF
			SELF:UpdateEditOwner()

			IF (lInvert .OR. (wScrMode == ScrollMode.No))
				oEditOwner:__CurPos := SELF:PrevEditPos(iEnd+1)
			ENDIF
		ENDIF

		iDelStart := iDelEnd := 0
		RETURN

	ACCESS EmptyValue AS STRING STRICT
		LOCAL sEmpty AS STRING
		LOCAL cCurTplChar AS STRING
		LOCAL i AS DWORD

		sEmpty := ""

		FOR i:=1 TO DWORD(iTemplLen)
			cCurTplChar := CharPos(sTemplate, i)
			IF IsEditTemplChar(cCurTplChar)
				IF (sEmpty == NULL_STRING)
					sEmpty := Space(1)
				ELSE
					sEmpty += " "
				ENDIF
			ELSE
				IF (cCurTplChar == ".") .AND. (cType == "N")
					sEmpty += Chr(wDecSep)
				ELSEIF (cCurTplChar == ",") .AND. (cType == "N")
					sEmpty += Chr(wThousSep)
				ELSE
					sEmpty += cCurTplChar
				ENDIF
			ENDIF
		NEXT

		RETURN sEmpty

	CONSTRUCTOR(Owner, PicString, Type, OverWrite, DefTempl, ScrMode)

		SELF:FuncFlags := strucPictureFuncFlags{}
		DEFAULT( REF Type, "C")

		oEditOwner := Owner
		cType := Type // !!! order dependancy: ASSIGN Picture needs type to be set !!!
		SELF:Picture := PicString

		wDecSep := SetDecimalSep()
		wThousSep := SetThousandSep()
		wOverWrite := (OverwriteMode) OverWrite
		wScrMode := (ScrollMode) ScrMode

		IF !Empty(DefTempl) .AND. Empty(sTemplate)
			sTemplate := DefTempl
			sPicture := sPicture + " " + DefTempl
			iTemplLen := INT(_CAST, SLen(sTemplate))
		ENDIF

		sValue := SELF:EmptyValue

		SELF:UpdateEditOwner()
		RETURN

	METHOD InsertChar(cInsChar AS STRING, iPos AS INT, lInvert AS LOGIC) AS VOID STRICT
		LOCAL iCurPos AS INT
		LOCAL iNextPos AS INT
		LOCAL cSaveChar AS STRING
		LOCAL cTempChar AS STRING
		IF SELF:ReadOnly
			RETURN
		ENDIF

		IF IsNil(iPos)
			iCurPos := oEditOwner:__CurPos
		ELSE
			iCurPos := iPos
		ENDIF

		IF !SELF:IsEditPos(iCurPos) .AND. !lInvert
			IF (cType == "N") .AND. sValue[iCurPos-1] == wDecSep
				iCurPos++
				oEditOwner:__CurPos := iCurPos
			ELSE
				SELF:InvalidAction()
				RETURN
			ENDIF
		ENDIF

		IF !lInvert
			iNextPos := SELF:NextEditPos(iCurPos + 1)
		ELSE
			iCurPos--
			iNextPos := SELF:PrevEditPos(iCurPos - 1)
		ENDIF

		cSaveChar := CharPos(sValue, DWORD(iCurPos))
		SELF:PutChar(cInsChar, iCurPos)

		WHILE (iCurPos > 0) .AND. (iNextPos > 0)
			IF (wScrMode != SCRMODE_FULL) .AND. (Abs(iNextPos - iCurPos) != 1)
				//if (iNextPos != iCurPos+1) .and. (wScrMode != SCRMODE_FULL)
				EXIT
			ENDIF
			IF (cType == "N") .AND. !lInvert .AND. iCurPos < sValue:Length .AND. sValue[iCurPos] == wDecSep
				EXIT
			ENDIF
			IF (SELF:IsValidChar(cSaveChar, iNextPos, FALSE))
				cTempChar := CharPos(sValue, DWORD(iNextPos))
				SELF:PutChar(cSaveChar, iNextPos)
				cSaveChar := cTempChar
			ELSE
				EXIT
			ENDIF
			iCurPos := iNextPos
			IF !lInvert
				iNextPos := SELF:NextEditPos(iCurPos + 1)
			ELSE
				iNextPos := SELF:PrevEditPos(iCurPos - 1)
			ENDIF
		END
		RETURN


	METHOD InvalidAction() AS VOID STRICT
		GuiWin32.MessageBeep(0XFFFFFFFF)
		RETURN

	METHOD IsEditPos(iPos AS INT) AS LOGIC STRICT
		RETURN IsEditTemplChar(CharPos(sTemplate, DWORD(iPos)))

	ACCESS IsEmpty AS LOGIC STRICT
		RETURN (sValue == SELF:EmptyValue)

	METHOD IsValidChar(cChar AS STRING, iPos AS INT, lIgnoreBlank AS LOGIC) AS LOGIC STRICT
		RETURN SELF:MatchesTemplChar(cChar, CharPos(sTemplate, DWORD(iPos)), lIgnoreBlank)

	METHOD MatchesTemplChar( cTest AS STRING, _cTemplChar AS STRING, lIgnoreBlank AS LOGIC ) AS LOGIC
		LOCAL lIsNumeric := ( cType != "N" ) AS LOGIC
		LOCAL cTemplChar AS Char

		IF ! IsEditTemplChar( _cTemplChar ) .AND. lIsNumeric
			RETURN FALSE
		ENDIF

		IF ! STRING.IsNullOrEmpty( _cTemplChar )
			cTemplChar := _cTemplChar[0]
		ENDIF

		// speed up this case
		IF cTemplChar == 'X'
			RETURN TRUE
		ENDIF

		IF cTest == " " .AND. lIgnoreBlank
			RETURN TRUE
		ENDIF

		DO CASE
		CASE cTemplChar == 'A'
			RETURN IsAlpha( cTest )
		CASE cTemplChar == 'N'
			RETURN IsAlNum( cTest )
		CASE cTemplChar == '9' .OR. cTemplChar == '.'
			RETURN IsDigit( cTest ) .OR. ( cType == "N" .AND. At2( cTest, " +-" ) > 0 )
		CASE cTemplChar == '#'
			RETURN IsDigit( cTest ) .OR. At2( cTest, " +-" ) > 0
		CASE cTemplChar == '!'
			RETURN TRUE
		CASE cTemplChar == 'Y'
			RETURN At2( cTest:ToUpper(), __CavoStr( RT_MSG_YNSTRING ) ) > 0
		CASE cTemplChar == 'L'
			cTest := cTest:ToUpper()
			RETURN At2( cTest, __CavoStr( RT_MSG_YNSTRING ) ) > 0 .OR. cTest == __CavoStr( RT_MSG_SHORT_TRUE ) .OR. cTest == __CavoStr( RT_MSG_SHORT_FALSE )
		CASE cTemplChar == '.' .AND. cType == "N"
			RETURN TRUE
		ENDCASE

		RETURN FALSE

	METHOD NextEditPos(iPos AS INT) AS INT STRICT
		LOCAL i AS INT

		IF (iPos == -1)
			iPos := oEditOwner:__CurPos + 1
		ENDIF

		FOR i := iPos TO iTemplLen
			IF IsEditTemplChar(CharPos(sTemplate, DWORD(i)))
				RETURN i
			ELSEIF (cType == "N") .AND. (CharPos(sTemplate, DWORD(i)) == ".") .AND. (wOverWrite != OverwriteMode.Allways) .AND.;
				(wOverWrite != OverwriteMode.OnKey .OR. !IsOverWriteModeEnabled())
				RETURN i
			ENDIF
		NEXT

		RETURN iPos

	METHOD Paste() AS LOGIC STRICT
		LOCAL sPaste AS STRING
		LOCAL i, iLen AS INT
		LOCAL iStart, iEnd AS INT
		IF SELF:ReadOnly
			RETURN FALSE
		ENDIF

		IF (!System.Windows.Forms.Clipboard.ContainsText())
			RETURN FALSE
		ENDIF

		sPaste := System.Windows.Forms.Clipboard.GetText()
		IF !STRING.IsNullOrEmpty(sPaste)
			iStart := oEditOwner:__TextBox:SelectionStart
			iEnd   := oEditOwner:__TextBox:SelectionLength+iStart
			IF ((iEnd - iStart) > 0)
				SELF:DeleteSelection(iStart+1, iEnd+1)
			ENDIF
			iLen := INT(_CAST, SLen(sPaste))
			FOR i := 1 TO iLen
				LOCAL IMPLIED e := System.Windows.Forms.KeyPressEventArgs{sPaste[i-1]}
				SELF:ProcessKeyEvent(KeyEvent{e})
			NEXT
		ENDIF

		RETURN TRUE

	ACCESS Picture AS STRING STRICT
		RETURN sPicture

	ASSIGN Picture(cNewPicture AS STRING)  STRICT
		LOCAL iSpacePos AS LONG
		sPicture := cNewPicture
		// handle functions
		IF (Left(sPicture, 1) == "@")
			LOCAL i		  AS LONG
			iSpacePos := sPicture:IndexOf(' ')
			IF (iSpacePos == -1)
				iSpacePos :=  sPicture:Length-1
			ENDIF
			FOR i := 1 TO iSpacePos
				LOCAL sFuncChar AS CHAR
				LOCAL iFuncPos AS LONG
				sFuncChar := sPicture[i]
				iFuncPos := "BCDERXZ(!A":IndexOf(sFuncChar)
				IF (iFuncPos >=0)
					SELF:FuncFlags:Flags[iFuncPos] := TRUE
				ENDIF
			NEXT
			sTemplate := sPicture:Substring(iSpacePos+1)
		ELSE
			sTemplate := sPicture
		ENDIF
		iTemplLen := sTemplate:Length

		IF ((SELF:cType == "D") .OR. (SELF:cType == "C")) .AND. FuncFlags:lSetDate
			sTemplate := GetDateFormat()
			iTemplLen := sTemplate:Length
			sTemplate := sTemplate:Replace( 'D', '9' ):Replace( 'M', '9' ):Replace( 'Y', '9' )
		ENDIF

		IF (iTemplLen == 0)
			IF FuncFlags:lAlphaOnly
				sTemplate := Replicate("A", (DWORD) Math.Min(Math.Min(DEFAULT_STRING_TEMPL_SIZE, oEditOwner:TextLimit), (LONG) oEditOwner:__FSLength))
			ELSEIF FuncFlags:lConvUpper
				sTemplate := Replicate("!", (DWORD) Math.Min(Math.Min(DEFAULT_STRING_TEMPL_SIZE, oEditOwner:TextLimit), (LONG) oEditOwner:__FSLength))
			ENDIF
			iTemplLen := sTemplate:Length
		ENDIF

		RETURN

	METHOD PrevEditPos(iPos AS INT) AS INT STRICT
		LOCAL i AS DWORD

		IF (iPos == -1)
			iPos := oEditOwner:__CurPos - 1
		ENDIF

		FOR i := DWORD(iPos) DOWNTO 1
			IF IsEditTemplChar(CharPos(sTemplate, i))
				RETURN LONGINT(i)
			ELSEIF (cType == "N") .AND. (CharPos(sTemplate, i) == ".") .AND. (wOverWrite != OverwriteMode.Allways) .AND.;
				(wOverWrite != OverwriteMode.OnKey .OR. !IsOverWriteModeEnabled())
				RETURN LONGINT(i)
			ENDIF
		NEXT

		RETURN 0L

	METHOD ProcessChar(cChar AS STRING) AS LOGIC STRICT
		LOCAL iCurPos AS INT
		LOCAL cTemplChar AS STRING
		LOCAL lInvert AS LOGIC
		LOCAL iDecPos AS INT

		iCurPos 	:= oEditOwner:__CurPos
		cTemplChar 	:= CharPos(sTemplate, DWORD(iCurPos))

		IF SELF:MatchesTemplChar(cChar, cTemplChar, FALSE)
			IF cTemplChar == "!"
				cChar := cChar:ToUpper()
			ENDIF
			IF ((wOverWrite == OverwriteMode.Allways) .OR. (wOverWrite == OverwriteMode.OnKey .AND. IsOverWriteModeEnabled())) .OR.	((cType == "N") .AND. (sValue[iCurPos-1]) == 32)
				SELF:PutChar(cChar, iCurPos)
			ELSE
				lInvert := (cType == "N") .AND. ((iDecPos := INT(_CAST, At3(Chr(wDecSep), sValue, (DWORD)(iCurPos-1)))) > 0) .AND. ((At2(" ", SubStr3(sValue, DWORD(_CAST, iCurPos+1), Max(iDecPos-iCurPos,0))) == 0) .OR. (iCurPos == iDecPos)) .AND. sValue[Max(0, SELF:NextEditPos(0)-1)] == 32
				SELF:InsertChar(cChar, iCurPos, lInvert)
			ENDIF
			SELF:UpdateEditOwner()
			IF !lInvert
				iCurPos := SELF:NextEditPos(-1)
			ENDIF
			IF ((iCurPos > iTemplLen) .OR. (!SELF:IsEditPos(iCurPos)) .AND. cType != "N") .AND. oEditOwner:AutoFocusChange
				if oEditOwner:Owner is WIndow var oWindow
                    local oForm as VOForm
					oForm := oWindow:__Form
					oForm:NextControl()
				ENDIF
			ELSE
				oEditOwner:__CurPos := iCurPos
			ENDIF
		ELSE
			SELF:InvalidAction()
		ENDIF

		RETURN TRUE

	METHOD ProcessKeyEvent(oKeyEvt AS KeyEvent) AS LOGIC STRICT
		LOCAL lRet AS LOGIC
		//LOCAL iCurPos AS INT
		LOCAL uMsg AS DWORD
		LOCAL iStart AS INT
		LOCAL iEnd AS INT
		LOCAL dwKCode AS LONG

		uMsg := oKeyEvt:uMsg
		//iCurPos := oEditOwner:__CurPos
		lRet := TRUE

		IF (uMsg == WM_KEYDOWN)
			dwKCode := oKeyEvt:KeyCode
			DO CASE
			CASE(dwKCode == KEYARROWLEFT)

				oEditOwner:__CurPos := SELF:PrevEditPos(-1)
			CASE(dwKCode == KEYARROWRIGHT)

				oEditOwner:__CurPos := SELF:NextEditPos(-1)
			CASE(dwKCode == KEYHOME)

				oEditOwner:__CurPos := SELF:NextEditPos(1)
			CASE(dwKCode == KEYEND)

				IF lEndKey
					oEditOwner:__CurPos := SELF:PrevEditPos(iTemplLen) + 1
				ELSE
					oEditOwner:__CurPos := SELF:PrevEditPos(INT(_CAST, SLen(Trim(sValue)))) + 1
				ENDIF
				lEndKey := !lEndKey

			CASE(dwKCode == KEYDELETE) .OR. (dwKCode == KEYBACKSPACE)
				IF !SELF:ReadOnly
					sOldValue := SClone(sValue)
					iStart := oEditOwner:__TextBox:SelectionStart
					iEnd   := iStart + oEditOwner:__TextBox:SelectionLength
					IF (dwKCode == KEYDELETE) .OR. (iStart != iEnd)
						SELF:DeleteSelection(iStart+1, iEnd+1)
						//oEditOwner:__CurPos := iStart
					ELSE
						oEditOwner:__CurPos := SELF:PrevEditPos(-1)
						SELF:DeleteChar(oEditOwner:__CurPos, FALSE, TRUE)
					ENDIF
				ENDIF
			OTHERWISE
				lRet := FALSE
			ENDCASE
		ELSEIF (uMsg == WM_CHAR)
			// Disallow all keys on readonly but allow CTRL+C
			IF ! SELF:ReadOnly .OR. oKeyEvt:KeyCode == 3
				sOldValue := SClone(sValue)
				IF (oKeyEvt:ASCIIChar == wDecSep) .AND. (cType == "N") .AND. (SELF:DecimalPos > 0)
					oEditOwner:__CurPos := SELF:DecimalPos + 1
					// change for 2.5b Copy/cut/paste didn't work
				ELSEIF (oKeyEvt:wParam >= 32)
					iStart := oEditOwner:__TextBox:SelectionStart
					iEnd   := iStart + oEditOwner:__TextBox:SelectionLength
					IF ((iEnd - iStart) > 0)
						SELF:DeleteSelection(iStart+1, iEnd+1)
					ENDIF
					lRet := SELF:ProcessChar(ChrW(oKeyEvt:ASCIIChar))
				ELSE
					lRet := (oKeyEvt:KeyCode == KEYBACKSPACE)
				ENDIF
			ENDIF
		ENDIF

		RETURN lRet

	METHOD PutChar(cChar AS STRING, iPos AS INT) AS VOID STRICT
		// VO version modifies existing string, this is illegal in .NET
		IF SELF:ReadOnly
			RETURN
		ENDIF
		IF iPos <= sValue:Length .AND. ! STRING.IsNullOrEmpty( cChar )
			LOCAL sb := System.Text.StringBuilder{ sValue } AS System.Text.StringBuilder
			LOCAL ch := cChar[0] AS Char
			IF FuncFlags:lConvUpper
				ch := Char.ToUpper( ch )
			ENDIF
			sb[iPos - 1] := ch
			sValue := sb:ToString()
		ENDIF
		RETURN

	METHOD TestFirstChar(cChar)
		LOCAL iCurPos    AS LONGINT
		iCurPos    := oEditOwner:__CurPos
		iCurPos := 1
		IF !SELF:IsEditPos(iCurPos)
			iCurPos := SELF:NextEditPos(iCurPos)
			oEditOwner:__CurPos := iCurPos
		ENDIF
		RETURN NIL

	ACCESS Type AS STRING STRICT
		RETURN cType

	ASSIGN TYPE(cNewType AS STRING)  STRICT
		(cType := cNewType)

	METHOD Undo() AS LOGIC STRICT
		sValue := sOldValue
		SELF:UpdateEditOwner()

		RETURN TRUE

	METHOD UpdateEditOwner() AS VOID STRICT
		LOCAL iSaveCurPos AS INT
		IF SELF:ReadOnly
			RETURN
		ENDIF

		iSaveCurPos := oEditOwner:__CurPos

		oEditOwner:__ForceModFlag2True := TRUE

		oEditOwner:__SetText(sValue)
		oEditOwner:Modified := TRUE
		oEditOwner:__ForceModFlag2True := FALSE
		oEditOwner:__CurPos := iSaveCurPos
		RETURN

	ASSIGN UsualValue(uNewValue AS USUAL)  AS VOID STRICT

		IF !IsNil(uNewValue)
			cType := ValType(uNewValue)
		ENDIF

		IF !IsNil(uNewValue) .AND. (cType == "C") .AND. (iTemplLen > INT(_CAST, Len(uNewValue)))
			uNewValue := uNewValue + Space(DWORD(iTemplLen) - Len(uNewValue))
		ENDIF

		IF !IsNil(uNewValue)
			sValue := Transform(uNewValue, sPicture)
		ELSE
			sValue := SELF:EmptyValue
		ENDIF

		oEditOwner:__SetText(sValue)

		RETURN
	STATIC METHOD IsEditTemplChar(cTest AS STRING) AS LOGIC STRICT
		RETURN LOGIC(_CAST, At2(cTest, "ANX9!YL#"))

	STATIC METHOD IsOverWriteModeEnabled() AS LOGIC STRICT
		RETURN ! System.Windows.Forms.Control.IsKeyLocked(System.Windows.Forms.Keys.Insert)

END CLASS



