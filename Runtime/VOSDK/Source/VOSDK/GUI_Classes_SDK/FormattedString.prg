PARTIAL CLASS __FormattedString
	PROTECT oEditOwner AS SingleLineEdit
	PROTECT sPicture AS STRING
	PROTECT sTemplate AS STRING
	PROTECT sValue AS STRING
	PROTECT sOldValue AS STRING
	PROTECT FuncFlags IS strucPictureFuncFlags
	PROTECT cType AS STRING
	PROTECT wDecSep AS DWORD		//RvdH 070205 changed from WORD to DWORD
	PROTECT wThousSep AS DWORD		//RvdH 070205 changed from WORD to DWORD
	PROTECT iDelStart AS INT
	PROTECT iDelEnd AS INT
	EXPORT iTemplLen AS INT
	EXPORT lEndKey AS LOGIC
	EXPORT wOverWrite AS LONGINT			//RvdH 070205 changed from WORD to LONG
	EXPORT wScrMode AS LONGINT				//RvdH 070205 changed from WORD to LONG

	METHOD AsString() AS STRING STRICT 
	RETURN sValue

METHOD Cut() AS LOGIC STRICT 
	LOCAL iStart AS INT
	LOCAL iEnd AS INT
	LOCAL iLen AS INT
	LOCAL hGbl AS PTR
	LOCAL pszGbl AS PSZ

	SendMessage(oEditOwner:Handle(), EM_GETSEL, DWORD(_CAST, @iStart), LONGINT(_CAST, @iEnd))

	IF (iStart == iEnd)
		RETURN FALSE
	ENDIF

	IF (!OpenClipboard(NULL_PTR))
		RETURN FALSE
	ENDIF

	EmptyClipboard()

	iStart++
	iEnd++
	iLen := iEnd - iStart

	hGbl := GlobalAlloc(GMEM_DDESHARE, DWORD(iLen+1))

	IF (hGbl == NULL_PTR)
		CloseClipboard()
		RETURN FALSE
	ENDIF

	pszGbl := GlobalLock(hGbl)
	MemCopy(pszGbl, String2Psz(SubStr3(sValue, DWORD(iStart), DWORD(iLen))), DWORD(iLen+1))

	GlobalUnlock(hGbl)

	SetClipboardData(CF_TEXT, hGbl)

	CloseClipboard()

	SELF:DeleteSelection(iStart, iEnd)
	RETURN TRUE

ACCESS DecimalPos AS INT STRICT 
	RETURN INT(_CAST, At2(".", sTemplate))

METHOD DeleteChar(iPos AS INT, lInvert AS LOGIC, lUpdateOwner AS LOGIC) AS VOID STRICT 
	LOCAL iCurPos AS INT
	LOCAL iNextPos AS INT
	LOCAL sNextChar AS STRING

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

	IF (wScrMode != SCRMODE_NO)
		WHILE (iCurPos > 0) .AND. (iNextPos > 0)
			IF (wScrMode != SCRMODE_FULL) .AND. (Abs(iNextPos - iCurPos) != 1)
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

	IF (iEnd <= iStart)
		iEnd := iStart
	ELSE
		iEnd--
	ENDIF

	iDelStart := iStart
	iDelEnd := iEnd

	lNumeric := (cType == "N")

	IF lNumeric .AND. ((iDecPos := INT(_CAST, At2(Chr(wDecSep), sValue))) > istart) .AND. (iDecPos < iEnd)
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
				ELSEIF (wScrMode == SCRMODE_PART)
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

		IF (lInvert .OR. (wScrMode == SCRMODE_NO))
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

	Default(@Type, "C")

	oEditOwner := Owner
	cType := Type // !!! order dependancy: ASSIGN Picture needs type to be set !!!
	SELF:Picture := PicString

	wDecSep := SetDecimalSep()
	wThousSep := SetThousandSep()
	wOverWrite := OverWrite
	wScrMode := ScrMode

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

	IF IsNil(iPos)
		iCurPos := oEditOwner:__CurPos
	ELSE
		iCurPos := iPos
	ENDIF

	IF !SELF:IsEditPos(iCurPos) .AND. !lInvert
#ifdef __VULCAN__	
		IF (cType == "N") .AND. sValue[iCurPos-1] == wDecSep
			iCurPos++
			oEditOwner:__CurPos := iCurPos
		ELSE
			SELF:InvalidAction()
			RETURN
		ENDIF
#else
		IF (cType == "N") .AND. (_NGet(String2Psz( sValue), DWORD(iCurPos-1)) == wDecSep)
			iCurPos++
			oEditOwner:__CurPos := iCurPos
		ELSE
			SELF:InvalidAction()
			RETURN
		ENDIF
#endif		
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
#ifdef __VULCAN__		
		IF (cType == "N") .AND. !lInvert .AND. iCurPos < sValue:Length .AND. sValue[iCurPos] == wDecSep
			EXIT
		ENDIF
#else		
		IF (cType == "N") .AND. !lInvert .AND. (_NGet(String2Psz( sValue), DWORD(iCurPos)) == wDecSep)
			EXIT
		ENDIF
#endif		
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
	MessageBeep(0xFFFFFFFF)
   RETURN

METHOD IsEditPos(iPos AS INT) AS LOGIC STRICT 
	RETURN IsEditTemplChar(CharPos(sTemplate, DWORD(iPos)))

ACCESS IsEmpty AS LOGIC STRICT 
	RETURN (sValue == SELF:EmptyValue)

METHOD IsValidChar(cChar AS STRING, iPos AS INT, lIgnoreBlank AS LOGIC) AS LOGIC STRICT 
	RETURN SELF:MatchesTemplChar(cChar, CharPos(sTemplate, DWORD(iPos)), lIgnoreBlank)

#ifdef __VULCAN__
// This implementation avoids unnecesary String -> PSZ conversions that would happen using the VO implementation
METHOD MatchesTemplChar( cTest AS STRING, _cTemplChar AS STRING, lIgnoreBlank AS LOGIC ) AS LOGIC
	LOCAL lIsNumeric := ( cType != "N" ) AS LOGIC
	LOCAL cTemplChar AS Char

	IF ! IsEditTemplChar( _cTemplChar ) .AND. lIsNumeric
		RETURN FALSE
	ENDIF
	
	IF ! String.IsNullOrEmpty( _cTemplChar )
	   cTemplChar := _cTemplChar[0]
	ENDIF
#warning added "c" prefixes
	// speed up this case
	IF cTemplChar == c'X'
		RETURN TRUE
	ENDIF

	IF cTest == " " .AND. lIgnoreBlank
		RETURN TRUE
	ENDIF

	DO CASE
	CASE cTemplChar == c'A'
		RETURN IsAlpha( cTest )
	CASE cTemplChar == c'N'
		RETURN IsAlNum( cTest )
	CASE cTemplChar == c'9' .OR. cTemplChar == c'.'
		RETURN IsDigit( cTest ) .OR. ( cType == "N" .AND. At2( cTest, " +-" ) > 0 )
	CASE cTemplChar == c'#'
		RETURN IsDigit( cTest ) .OR. At2( cTest, " +-" ) > 0
	CASE cTemplChar == c'!'
		RETURN TRUE
	CASE cTemplChar == c'Y'
		RETURN At2( cTest:ToUpper(), __CavoStr( RT_MSG_YNSTRING ) ) > 0
	CASE cTemplChar == c'L'
		cTest := cTest:ToUpper()
		RETURN At2( cTest, __CavoStr( RT_MSG_YNSTRING ) ) > 0 .OR. cTest == __CavoStr( RT_MSG_SHORT_TRUE ) .OR. cTest == __CavoStr( RT_MSG_SHORT_FALSE )
	CASE cTemplChar == c'.' .AND. cType == "N"
		RETURN TRUE
	ENDCASE

	RETURN FALSE

#else

METHOD MatchesTemplChar(cTest AS STRING, cTemplChar AS STRING, lIgnoreBlank AS LOGIC) AS LOGIC STRICT 
	LOCAL lIsNumeric := (cType != "N") AS LOGIC
   LOCAL cYesNo, cTrue, cFalse AS STRING
	IF !IsEditTemplChar(cTemplChar) .AND. lIsNumeric
		RETURN FALSE
	ENDIF

	// speed up this case
	IF (cTemplChar == "X")
		RETURN TRUE
	ENDIF

	IF (cTest == " " .AND. lIgnoreBlank)
		RETURN TRUE
	ENDIF

	DO CASE
	CASE cTemplChar == "A"
		RETURN IsAlpha(PSZ(_CAST, cTest))
	CASE cTemplChar == "N"
		RETURN IsAlNum(PSZ(_CAST, cTest))
	CASE (cTemplChar == "9" .OR. cTemplChar == ".")
		RETURN IsDigit(PSZ(_CAST, cTest)) .OR. (cType == "N" .AND. (At2(cTest, " +-") > 0))
	CASE cTemplChar == "#"
		RETURN IsDigit(PSZ(_CAST, cTest)) .OR. (At2(cTest, " +-") > 0)
	CASE cTemplChar == "!"
		RETURN TRUE
	CASE cTemplChar == "Y"
		RETURN (At2(Upper(cTest), Psz2String(_GetStringDXAX(RT_MSG_YNSTRING))) > 0)
	CASE cTemplChar == "L"
		cTest := Upper(cTest)               
		cYesNo := Psz2String(_GetStringDXAX(RT_MSG_YNSTRING)) 
		cTrue  := Psz2String(_GetStringDXAX(RT_MSG_SHORT_TRUE)) 
		cFalse := Psz2String(_GetStringDXAX(RT_MSG_SHORT_FALSE)) 
		RETURN ((At2(cTest,cYesNo) >0) .OR. (cTest == cTrue) .OR. (cTest == cFalse))
	CASE cTemplChar == "." .AND. cType == "N"
		RETURN TRUE
	ENDCASE

	RETURN FALSE
#endif

METHOD NextEditPos(iPos AS INT) AS INT STRICT 
	LOCAL i AS INT

	IF (iPos == -1)
		iPos := oEditOwner:__CurPos + 1
	ENDIF

	FOR i := iPos TO iTemplLen
		IF IsEditTemplChar(CharPos(sTemplate, DWORD(i)))
			RETURN i
		ELSEIF (cType == "N") .AND. (CharPos(sTemplate, DWORD(i)) == ".") .AND. (wOverWrite != OVERWRITE_ALWAYS) .AND.;
			(wOverWrite != OVERWRITE_ONKEY .OR. !IsOverWriteModeEnabled())
			RETURN i
		ENDIF
	NEXT

	RETURN iPos

METHOD Paste() AS LOGIC STRICT 
	LOCAL hGlb AS PTR
	LOCAL pszPaste AS PSZ
	LOCAL sPaste AS STRING
	LOCAL i, iLen AS INT
	LOCAL iStart, iEnd AS INT
	LOCAL oKeyEvent AS KeyEvent

	IF (!IsClipboardFormatAvailable(CF_TEXT))
		RETURN FALSE
	ENDIF

	IF (!OpenClipboard(NULL_PTR))
		RETURN FALSE
	ENDIF

	hGlb := GetClipboardData(CF_TEXT)

	IF (hGlb != NULL_PTR)
		pszPaste := GlobalLock(hGlb)
		IF (PTR(_CAST, pszPaste) != NULL_PTR)
			SendMessage(oEditOwner:Handle(), EM_GETSEL, DWORD(_CAST, @iStart), LONGINT(_CAST, @iEnd))
			IF ((iEnd - iStart) > 0)
				SELF:DeleteSelection(iStart+1, iEnd+1)
			ENDIF
			oKeyEvent := KeyEvent{oEditOwner:Handle(), WM_CHAR, 0, 0}
			sPaste := Psz2String(pszPaste)
			iLen := INT(_CAST, SLen(sPaste))
			FOR i := 1 TO iLen
				oKeyEvent:wParam := Asc(CharPos(sPaste,DWORD(i)))
				SELF:ProcessKeyEvent(oKeyEvent)
			NEXT
		ENDIF
		GlobalUnlock(hGlb)
	ENDIF

	CloseClipboard()

	RETURN TRUE

ACCESS Picture AS STRING STRICT 
	RETURN sPicture

ASSIGN Picture(cNewPicture AS STRING)  STRICT 
	LOCAL pLogic AS LOGIC PTR
	LOCAL iSpcPos, iFuncPos, i AS DWORD
	LOCAL sFuncChar AS STRING
	#ifndef __VULCAN__
	LOCAL iAsc, iAscD, iAscM, iAscY, iAsc9 AS DWORD
	#endif

	sPicture := cNewPicture
	pLogic := (LOGIC PTR) @FuncFlags
	// handle functions
	IF (Left(sPicture, 1) == "@")
		iSpcPos := At2(" ", sPicture)
		IF (iSpcPos == 0)
			iSpcPos :=  SLen(sPicture) + 1
		ENDIF
		FOR i := 2 TO (iSpcPos-1)
			sFuncChar := CharPos(sPicture, i)
			iFuncPos := At2(sFuncChar, "BCDERXZ(!A")
			IF (iFuncPos > 0)
				LOGIC(pLogic + iFuncPos - 1) := TRUE
			ENDIF
		NEXT
	ENDIF

	sTemplate := SubStr2(sPicture, (iSpcPos + 1))
	iTemplLen := INT(_CAST, SLen(sTemplate))

	#ifndef __VULCAN__
	iAscD :=  68 	// Asc("D")
	iAscM :=  77 	// Asc("M")
	iAscY :=  89 	// Asc("Y")
	iAsc9 :=  57 	// Asc("9")
	#endif
	IF ((cType == "D") .OR. (cType == "C")) .AND. FuncFlags:lSetDate
		sTemplate := GetDateFormat()
		iTemplLen := LONGINT(SLen(sTemplate))
#ifdef __VULCAN__
      sTemplate := sTemplate:Replace( 'D', '9' ):Replace( 'M', '9' ):Replace( 'Y', '9' )
#else		
		FOR i:=1 TO DWORD(iTemplLen)
			iAsc := Asc(CharPos(sTemplate, i))
			IF (iAsc == iAscD) .OR. (iAsc == iAscM) .OR. (iAsc == iAscY)
				_NPut(PSZ(_CAST, sTemplate), i-1, iAsc9)
			ENDIF
		NEXT
#endif				
	ENDIF

	IF (iTemplLen == 0)
		IF FuncFlags:lAlphaOnly
			sTemplate := Replicate("A", Min(Min(DEFAULT_STRING_TEMPL_SIZE, oEditOwner:TextLimit), oEditOwner:__FSLength))
		ELSEIF FuncFlags:lConvUpper
			sTemplate := Replicate("!", Min(Min(DEFAULT_STRING_TEMPL_SIZE, oEditOwner:TextLimit), oEditOwner:__FSLength))
		ENDIF
		iTemplLen := LONGINT(SLen(sTemplate))
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
		ELSEIF (cType == "N") .AND. (CharPos(sTemplate, i) == ".") .AND. (wOverWrite != OVERWRITE_ALWAYS) .AND.;
			(wOverWrite != OVERWRITE_ONKEY .OR. !IsOverWriteModeEnabled())
			RETURN LONGINT(i)
		ENDIF
	NEXT

	RETURN 0L

METHOD ProcessChar(cChar AS STRING) AS LOGIC STRICT 
	LOCAL iCurPos AS INT
	LOCAL cTemplChar AS STRING
	LOCAL lInvert AS LOGIC
	LOCAL iDecPos AS INT

	iCurPos 		:= oEditOwner:__CurPos
	cTemplChar 	:= CharPos(sTemplate, DWORD(iCurPos))

	IF SELF:MatchesTemplChar(cChar, cTemplChar, FALSE)
		IF cTemplChar == "!"
			UpperA(cChar)
		ENDIF
#ifdef __VULCAN__
		IF ((wOverWrite == OVERWRITE_ALWAYS) .OR. (wOverWrite == OVERWRITE_ONKEY .AND. IsOverWriteModeEnabled())) .OR.	((cType == "N") .AND. (sValue[iCurPos-1]) == 32)
			SELF:PutChar(cChar, iCurPos)
		ELSE
			lInvert := (cType == "N") .AND. ((iDecPos := INT(_CAST, At3(Chr(wDecSep), sValue, iCurPos-1))) > 0) .AND. ((At2(" ", SubStr3(sValue, DWORD(_CAST, iCurPos+1), Max(iDecPos-iCurPos,0))) == 0) .OR. (iCurPos == iDecPos)) .AND. sValue[Max(0, SELF:NextEditPos(0)-1)] == 32
			SELF:InsertChar(cChar, iCurPos, lInvert)
		ENDIF
#else		
		IF ((wOverWrite == OVERWRITE_ALWAYS) .OR. (wOverWrite == OVERWRITE_ONKEY .AND. IsOverWriteModeEnabled())) .OR.;
			((cType == "N") .AND. (_NGet(String2Psz(sValue), DWORD(iCurPos-1)) == 32))
			SELF:PutChar(cChar, iCurPos)
		ELSE
			lInvert := (cType == "N") .AND. ((iDecPos := INT(_CAST, At3(Chr(wDecSep), sValue, DWORD(iCurPos-1)))) > 0);
				.AND. ((At2(" ", SubStr3(sValue, DWORD(iCurPos+1), Max(iDecPos-iCurPos,0))) == 0) .OR.;
				 (iCurPos == iDecPos)) .AND. (_NGet(String2Psz(sValue), Max(0, SELF:NextEditPos(0)-1)) == 32)
			SELF:InsertChar(cChar, iCurPos, lInvert)
		ENDIF
#endif		
		SELF:UpdateEditOwner()
		IF !lInvert
			iCurPos := SELF:NextEditPos(-1)
		ENDIF
		IF ((iCurPos > iTemplLen) .OR. (!SELF:IsEditPos(iCurPos)) .AND. cType != "N") .AND. oEditOwner:AutoFocusChange
			SendMessage(GetParent(oEditOwner:Handle()), WM_NEXTDLGCTL, 0, 0L)
		ELSE
			oEditOwner:__CurPos := iCurPos
		ENDIF
	ELSE
		SELF:InvalidAction()
	ENDIF

	RETURN TRUE

METHOD ProcessKeyEvent(oKeyEvt AS KeyEvent) AS LOGIC STRICT 
	LOCAL iCurPos AS INT
	LOCAL uMsg AS DWORD
	LOCAL lRet AS LOGIC
	LOCAL iStart AS INT
	LOCAL iEnd AS INT
	LOCAL dwKCode AS DWORD

	uMsg := oKeyEvt:umsg
	// causes code in oEditOwner:__CurPos to be executed, do not remove
	iCurPos := oEditOwner:__CurPos
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
			sOldValue := SClone(sValue)
			SendMessage(oEditOwner:Handle(), EM_GETSEL, DWORD(_CAST, @iStart), LONG(_CAST, @iEnd))
			IF (dwKCode == KEYDELETE) .OR. (iStart != iEnd)
				SELF:DeleteSelection(iStart+1, iEnd+1)
				//oEditOwner:__CurPos := iStart
			ELSE
				oEditOwner:__CurPos := SELF:PrevEditPos(-1)
				SELF:DeleteChar(oEditOwner:__CurPos, FALSE, TRUE)
			ENDIF
		OTHERWISE
			lRet := FALSE
		ENDCASE
	ELSEIF (uMSG == WM_CHAR)
		sOldValue := SClone(sValue)
		IF (oKeyEvt:ASCIIChar == wDecSep) .AND. (cType == "N") .AND. (SELF:DecimalPos > 0)
			oEditOwner:__CurPos := SELF:DecimalPos + 1
			// change for 2.5b Copy/cut/paste didn't work
		ELSEIF (oKeyEvt:wParam >= 32)
			SendMessage(oEditOwner:Handle(), EM_GETSEL, DWORD(_CAST, @iStart), LONG(_CAST, @iEnd))
			IF ((iEnd - iStart) > 0)
				SELF:DeleteSelection(iStart+1, iEnd+1)
			ENDIF
			lRet := SELF:ProcessChar(Chr(oKeyEvt:ASCIIChar))
		ELSE
			lRet := (oKeyEvt:KeyCode == KEYBACKSPACE)
		ENDIF
	ENDIF

	RETURN lRet

METHOD PutChar(cChar AS STRING, iPos AS INT) AS VOID STRICT 
#ifdef __VULCAN__
   // VO version modifies existing string, this is illegal in .NET
   IF iPos <= sValue:Length && ! String.IsNullOrEmpty( cChar )
      LOCAL sb := System.Text.StringBuilder{ sValue } AS System.Text.StringBuilder
      LOCAL ch := cChar[0] AS Char
      IF FuncFlags:lConvUpper
         ch := Char.ToUpper( ch )
      ENDIF
      sb[iPos - 1] := ch
      sValue := sb:ToString()
   ENDIF
#else
	LOCAL pString AS BYTE PTR
	LOCAL iOffset AS INT

	IF (iPos <= INT(_CAST, SLen(sValue)))
		iOffset := (iPos -1)
		IF FuncFlags:lConvUpper
			UpperA(cChar)
		ENDIF
		pString := PTR(_CAST, sValue)
		BYTE(pString + iOffSet) := Asc(cChar)
	ENDIF
#endif	
	RETURN

METHOD TestFirstChar(cChar) 
	LOCAL iCurPos    AS LONGINT
	LOCAL cTemplChar AS STRING

	iCurPos    := oEditOwner:__CurPos
	cTemplChar := CharPos(sTemplate, DWORD(iCurPos))
	iCurPos := 1
	IF !SELF:IsEditPos(iCurPos)
		iCurPos := SELF:NextEditPos(iCurPos)
		oEditOwner:__CurPos := iCurPos
	ENDIF
	RETURN NIL

ACCESS Type AS STRING STRICT 
	RETURN cType

ASSIGN TYPE(cNewType AS STRING)  STRICT 
	RETURN (cType := cNewType)

METHOD Undo() AS LOGIC STRICT 
	sValue := sOldValue
	SELF:UpdateEditOwner()

	RETURN TRUE

METHOD UpdateEditOwner() AS VOID STRICT 
	LOCAL iSaveCurPos AS INT

	iSaveCurPos := oEditOwner:__CurPos

	oEditOwner:__ForceModFlag2True := TRUE
	oEditOwner:__SetText(sValue)
	oEditOwner:Modified := TRUE
	oEditOwner:__ForceModFlag2True := FALSE
	oEditOwner:__CurPos := iSaveCurPos
	RETURN

ASSIGN UsualValue(uNewValue AS USUAL)  STRICT 
	IF !IsNil(uNewValue)
		cType := ValType(uNewValue)
	ENDIF

	IF !IsNil(uNewValue) .AND. (CType == "C") .AND. (iTemplLen > INT(_CAST, Len(uNewValue)))
		uNewValue := uNewValue + Space(DWORD(iTemplLen) - Len(uNewValue))
	ENDIF

	IF !IsNil(uNewValue)
		sValue := Transform(uNewValue, sPicture)
	ELSE
		sValue := SELF:EmptyValue
	ENDIF

	oEditOwner:__SetText(sValue)

	RETURN 

END CLASS

STATIC FUNCTION IsEditTemplChar(cTest AS STRING) AS LOGIC STRICT
   RETURN LOGIC(_CAST, At2(cTest, "ANX9!YL#"))

STATIC FUNCTION IsOverWriteModeEnabled() AS LOGIC STRICT
   /*STATIC*/ LOCAL DIM aKeyStates[256] AS BYTE // compiler bug!!!

   GetKeyboardState(@aKeyStates)

   RETURN !LOGIC(_CAST, _AND(aKeyStates[VK_INSERT + 1], 1))



#region defines
DEFINE DEFAULT_STRING_TEMPL_SIZE := 128
#endregion
