PARTIAL CLASS HotKey INHERIT VObject
	PROTECT bKey			AS BYTE
	PROTECT lAltKey		AS LOGIC
	PROTECT lCtrlKey		AS LOGIC
	PROTECT lExtKey		AS LOGIC
	PROTECT lShiftKey	AS LOGIC

ACCESS AltKey 
	

	RETURN lAltKey

ASSIGN AltKey(lEnable) 
	

	RETURN lAltKey := lEnable

ACCESS CtrlKey 
	

	RETURN lCtrlKey

ASSIGN CtrlKey(lEnable) 
	

	RETURN lCtrlKey := lEnable

ACCESS ExtendedKey 
	

	RETURN lExtKey

ASSIGN ExtendedKey(lEnable) 
	

	RETURN lExtKey := lEnable

CONSTRUCTOR(bKey, lAlt, lCtl, lShift, lExt) 
	

	IF IsNumeric(bKey)
		SELF:bKey := BYTE(bKey)
	ENDIF

	IF IsLogic(lAlt)
		SELF:lAltKey := lAlt
	ENDIF
	IF IsLogic(lCtl)
		SELF:lCtrlKey := lCtl
	ENDIF
	IF IsLogic(lShift)
		SELF:lShiftKey := lShift
	ENDIF
	IF IsLogic(lExt)
		SELF:lExtKey := lExt
	ENDIF
	RETURN 


ACCESS Key 
	

	RETURN bKey

ASSIGN Key(bNewKey) 
	

	RETURN bKey := bNewKey

ACCESS ShiftKey 
	

	RETURN lShiftKey

ASSIGN ShiftKey(lEnable) 
	

	RETURN lShiftKey := lEnable

END CLASS

PARTIAL CLASS HotKeyEdit INHERIT TextControl
	PROTECT oHotKeyRule	AS HotKeyRule

	//PP-030828 Strong typing
	METHOD __SetRule(oNewRule AS HotKeyRule) AS VOID STRICT 
	//PP-030828 Strong typing
	LOCAL dwInvalidCombinations	AS DWORD
	LOCAL wInvalidModifiers			AS WORD

	

	IF oNewRule:AltKeyInvalid
		dwInvalidCombinations := _OR(dwInvalidCombinations, HKCOMB_A)
	ENDIF
	IF oNewRule:CtrlKeyInvalid
		dwInvalidCombinations := _OR(dwInvalidCombinations, HKCOMB_C)
	ENDIF
	IF oNewRule:CtrlAltKeysInvalid
		dwInvalidCombinations := _OR(dwInvalidCombinations, HKCOMB_CA)
	ENDIF
	IF oNewRule:ShiftKeyInvalid
		dwInvalidCombinations := _OR(dwInvalidCombinations, HKCOMB_S)
	ENDIF
	IF oNewRule:ShiftAltKeysInvalid
		dwInvalidCombinations := _OR(dwInvalidCombinations, HKCOMB_SA)
	ENDIF
	IF oNewRule:ShiftCtrlKeysInvalid
		dwInvalidCombinations := _OR(dwInvalidCombinations, HKCOMB_SC)
	ENDIF
	IF oNewRule:ShiftCtrlAltKeysInvalid
		dwInvalidCombinations := _OR(dwInvalidCombinations, HKCOMB_SCA)
	ENDIF
	IF oNewRule:UnmodifiedKeysInvalid
		dwInvalidCombinations := _OR(dwInvalidCombinations, HKCOMB_NONE)
	ENDIF

	IF oNewRule:UseAltKeyOnInvalid
		wInvalidModifiers := _OR(wInvalidModifiers, HOTKEYF_ALT)
	ENDIF
	IF oNewRule:UseCtrlKeyOnInvalid
		wInvalidModifiers := _OR(wInvalidModifiers, HOTKEYF_CONTROL)
	ENDIF
	IF oNewRule:UseExtendedKeyOnInvalid
		wInvalidModifiers := _OR(wInvalidModifiers, HOTKEYF_EXT)
	ENDIF
	IF oNewRule:UseShiftKeyOnInvalid
		wInvalidModifiers := _OR(wInvalidModifiers, HOTKEYF_SHIFT)
	ENDIF

	SendMessage(SELF:Handle(), HKM_SETRULES, dwInvalidCombinations, MakeLong(wInvalidModifiers, 0))
  	RETURN

METHOD ApplyHotKey(oWindow) 
	LOCAL dwHotKey			AS DWORD

	

	Default(@oWindow, SELF:Owner)

	dwHotKey := _AND(SendMessage(SELF:Handle(), HKM_GETHOTKEY, 0, 0),0xFFFF)

	RETURN SendMessage(oWindow:Handle(), WM_SETHOTKEY, dwHotKey, 0)

METHOD Create() 
	

	IF (SUPER:Create() != NULL_PTR)
		IF oHotKeyRule != NULL_OBJECT
			SELF:__SetRule(oHotKeyRule)
		ENDIF
	ENDIF

	RETURN hWnd

ACCESS HotKey 
	LOCAL oHotKeyRet AS HotKey
	LOCAL wRet AS WORD

	wRet := _AND(0xFFFF,SendMessage(SELF:Handle(), DWORD(HKM_GETHOTKEY), 0U, 0L))

	oHotKeyRet := HotKey{LoByte(wRet)}
	oHotKeyRet:AltKey 		:= (_AND(HiByte(wRet), HOTKEYF_ALT) > 0)
	oHotKeyRet:CtrlKey 		:= (_AND(HiByte(wRet), HOTKEYF_CONTROL) > 0)
	oHotKeyRet:ExtendedKey 	:= (_AND(HiByte(wRet), HOTKEYF_EXT) > 0)
	oHotKeyRet:ShiftKey 		:= (_AND(HiByte(wRet), HOTKEYF_SHIFT) > 0)

	RETURN oHotKeyRet

ASSIGN HotKey(oNewHotKey) 
	LOCAL bModifierFlags AS BYTE

	

	IF oNewHotKey:AltKey
		bModifierFlags := _OR(bModifierFlags, HOTKEYF_ALT)
	ENDIF
	IF oNewHotKey:CtrlKey
		bModifierFlags := _OR(bModifierFlags, HOTKEYF_CONTROL)
	ENDIF
	IF oNewHotKey:ExtendedKey
		bModifierFlags := _OR(bModifierFlags, HOTKEYF_EXT)
	ENDIF
	IF oNewHotKey:ShiftKey
		bModifierFlags := _OR(bModifierFlags, HOTKEYF_SHIFT)
	ENDIF

	SendMessage(SELF:Handle(), HKM_SETHOTKEY, MakeWord(oNewHotKey:Key, bModifierFlags), 0)

	RETURN 

CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 

	

	IF !IsNil(kStyle)
		dwStyle := _OR(dwStyle, DWORD(kStyle))
	ENDIF

	IF !IsInstanceOfUsual(xID, #ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, HOTKEY_CLASS, dwStyle)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, , dwStyle)
	ENDIF

	SELF:__ClassName := HOTKEY_CLASS

	RETURN 

ACCESS Rule 
	

	RETURN oHotKeyRule

ASSIGN Rule(oNewRule) 
	

	oHotKeyRule := oNewRule
	SELF:__SetRule(oNewRule)

	RETURN 
END CLASS

PARTIAL CLASS HotKeyRule INHERIT VObject
	PROTECT lAltKeyInvalid			AS LOGIC
	PROTECT lCtrlKeyInvalid			AS LOGIC
	PROTECT lCtrlAltKeysInvalid		AS LOGIC
	PROTECT lUnmodKeysInvalid		AS LOGIC
	PROTECT lShiftKeyInvalid			AS LOGIC
	PROTECT lShiftAltKeysInvalid		AS LOGIC
	PROTECT lShiftCtrlKeysInvalid	AS LOGIC
	PROTECT lShiftCtrlAltKeysInvalid	AS LOGIC
	PROTECT lInvalidAltKey	AS LOGIC
	PROTECT lInvalidCtrlKey	AS LOGIC
	PROTECT lInvalidExtKey	AS LOGIC
	PROTECT lInvalidShiftKey	AS LOGIC

ACCESS AltKeyInvalid 
	

	RETURN lAltKeyInvalid

ASSIGN AltKeyInvalid(lEnable) 
	

	RETURN lAltKeyInvalid := lEnable

ACCESS CtrlAltKeysInvalid 
	

	RETURN lCtrlAltKeysInvalid

ASSIGN CtrlAltKeysInvalid(lEnable) 
	

	RETURN lCtrlAltKeysInvalid := lEnable

ACCESS CtrlKeyInvalid 
	

	RETURN lCtrlKeyInvalid

ASSIGN CtrlKeyInvalid(lEnable) 
	

	RETURN lCtrlKeyInvalid := lEnable

CONSTRUCTOR() 
    
    SUPER()


RETURN 

ACCESS ShiftAltKeysInvalid 
	

	RETURN lShiftAltKeysInvalid

ASSIGN ShiftAltKeysInvalid(lEnable) 

	

	RETURN lShiftAltKeysInvalid := lEnable

ACCESS ShiftCtrlAltKeysInvalid 
	

	RETURN lShiftCtrlAltKeysInvalid

ASSIGN ShiftCtrlAltKeysInvalid(lEnable) 
	

	RETURN lShiftCtrlAltKeysInvalid := lEnable

ACCESS ShiftCtrlKeysInvalid 
	

	RETURN lShiftCtrlKeysInvalid

ASSIGN ShiftCtrlKeysInvalid(lEnable) 
	

	RETURN lShiftCtrlKeysInvalid := lEnable

ACCESS ShiftKeyInvalid 
	

	RETURN lShiftKeyInvalid

ASSIGN ShiftKeyInvalid(lEnable) 
	

	RETURN lShiftKeyInvalid := lEnable

ACCESS UnmodifiedKeysInvalid 
	

	RETURN lUnmodKeysInvalid

ASSIGN UnmodifiedKeysInvalid(lEnable) 
	

	RETURN lUnmodKeysInvalid := lEnable

ACCESS UseAltKeyOnInvalid 
	

	RETURN lInvalidAltKey

ASSIGN UseAltKeyOnInvalid(lEnable) 

	

	RETURN lInvalidAltKey := lEnable

ACCESS UseCtrlKeyOnInvalid 
	

	RETURN lInvalidCtrlKey

ASSIGN UseCtrlKeyOnInvalid(lEnable) 
	

	RETURN lInvalidCtrlKey := lEnable

ACCESS UseExtendedKeyOnInvalid 
	

	RETURN lInvalidExtKey

ASSIGN UseExtendedKeyOnInvalid(lEnable) 
	

	RETURN lInvalidExtKey := lEnable

ACCESS UseShiftKeyOnInvalid 
	

	RETURN lInvalidShiftKey

ASSIGN UseShiftKeyOnInvalid(lEnable) 
	

	RETURN lInvalidShiftKey := lEnable

END CLASS

