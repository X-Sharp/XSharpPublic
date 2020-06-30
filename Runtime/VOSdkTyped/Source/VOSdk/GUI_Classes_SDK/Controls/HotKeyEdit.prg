

CLASS HotKey INHERIT VObject
	CONSTRUCTOR(bKey, lAlt, lCtl, lShift, lExt) 
		SUPER()
		IF IsNumeric(bKey)
			SELF:Key := BYTE(bKey)
		ENDIF
		IF IsLogic(lAlt)
			SELF:AltKey := lAlt
		ENDIF
		IF IsLogic(lCtl)
			SELF:CtrlKey := lCtl
		ENDIF
		IF IsLogic(lShift)
			SELF:ShiftKey := lShift
		ENDIF
		IF IsLogic(lExt)
			SELF:ExtendedKey := lExt
		ENDIF
		RETURN 

	PROPERTY AltKey       AS LOGIC AUTO
	PROPERTY CtrlKey      AS LOGIC AUTO
	PROPERTY ExtendedKey  AS LOGIC AUTO
	PROPERTY Key          AS BYTE AUTO
	PROPERTY ShiftKey     AS LOGIC AUTO

END CLASS

CLASS HotKeyEdit INHERIT TextControl
	PROTECT oHotKeyRule	AS HotKeyRule
    PROPERTY ControlType AS ControlType GET ControlType.Hotkey


	METHOD __SetRule(oNewRule AS HotKeyRule) AS VOID STRICT 
		LOCAL dwInvalidCombinations	AS DWORD
		LOCAL wInvalidModifiers			AS DWORD

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

		GuiWin32.SendMessage(SELF:Handle(), HKM_SETRULES, dwInvalidCombinations, MakeLong((WORD)wInvalidModifiers, 0))
		RETURN

	METHOD ApplyHotKey(oWindow) 
		LOCAL dwHotKey			AS DWORD

		IF IsNil(oWindow)
			oWindow := (Object)SELF:Owner
		ENDIF

		dwHotKey := _AND(GuiWin32.SendMessage(SELF:Handle(), HKM_GETHOTKEY, 0, 0),0XFFFF)

		RETURN GuiWin32.SendMessage(oWindow:Handle(), WM_SETHOTKEY, dwHotKey, 0)

	METHOD Create() AS System.Windows.Forms.Control
		IF (SUPER:Create() != NULL_OBJECT)
			IF oHotKeyRule != NULL_OBJECT
				SELF:__SetRule(oHotKeyRule)
			ENDIF
		ENDIF

		RETURN oCtrl

	ACCESS HotKey AS HotKey
		LOCAL oHotKeyRet AS HotKey
		LOCAL wRet AS WORD
		wRet := (WORD) _AND(0XFFFF,GuiWin32.SendMessage(SELF:Handle(), DWORD(HKM_GETHOTKEY), 0U, 0L))

		oHotKeyRet := HotKey{LoByte(wRet)}
		oHotKeyRet:AltKey 		:= (_AND(HiByte(wRet), HOTKEYF_ALT) > 0)
		oHotKeyRet:CtrlKey 		:= (_AND(HiByte(wRet), HOTKEYF_CONTROL) > 0)
		oHotKeyRet:ExtendedKey 	:= (_AND(HiByte(wRet), HOTKEYF_EXT) > 0)
		oHotKeyRet:ShiftKey 	:= (_AND(HiByte(wRet), HOTKEYF_SHIFT) > 0)

		RETURN oHotKeyRet

	ASSIGN HotKey(oNewHotKey AS HotKey) 
		LOCAL bModifierFlags AS DWORD
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

		GuiWin32.SendMessage(SELF:Handle(), HKM_SETHOTKEY, MakeWord(oNewHotKey:Key, (BYTE)bModifierFlags), 0)

		RETURN 

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle) 

		//IF !IsNil(kStyle)
		//	dwStyle := _OR(dwStyle, kStyle)
		//ENDIF
		SELF:cClassName := HOTKEY_CLASS

		IF !IsInstanceOfUsual(xID, #ResourceID)
			SUPER(oOwner, xID, oPoint, oDimension, HOTKEY_CLASS, kStyle)
		ELSE
			SUPER(oOwner, xID, oPoint, oDimension, , kStyle)
		ENDIF


		RETURN 

	ACCESS Rule AS HotKeyRule
		RETURN oHotKeyRule

	ASSIGN Rule(oNewRule AS HotKeyRule) 
		oHotKeyRule := oNewRule
		SELF:__SetRule(oNewRule)

		RETURN 
END CLASS

CLASS HotKeyRule INHERIT VObject


	PROPERTY AltKeyInvalid           AS LOGIC AUTO
	PROPERTY CtrlAltKeysInvalid      AS LOGIC AUTO
	PROPERTY CtrlKeyInvalid          AS LOGIC AUTO
	PROPERTY ShiftAltKeysInvalid     AS LOGIC AUTO
	PROPERTY ShiftCtrlAltKeysInvalid AS LOGIC AUTO
	PROPERTY ShiftCtrlKeysInvalid    AS LOGIC AUTO
	PROPERTY ShiftKeyInvalid         AS LOGIC AUTO
	PROPERTY UnmodifiedKeysInvalid   AS LOGIC AUTO
	PROPERTY UseAltKeyOnInvalid      AS LOGIC AUTO
	PROPERTY UseCtrlKeyOnInvalid     AS LOGIC AUTO
	PROPERTY UseExtendedKeyOnInvalid AS LOGIC AUTO
	PROPERTY UseShiftKeyOnInvalid    AS LOGIC AUTO

END CLASS

