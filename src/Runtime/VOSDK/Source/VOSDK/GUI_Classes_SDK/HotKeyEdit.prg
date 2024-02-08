/// <include file="Gui.xml" path="doc/HotKey/*" />
CLASS HotKey INHERIT VObject
	PROTECT bKey			AS BYTE
	PROTECT lAltKey		AS LOGIC
	PROTECT lCtrlKey		AS LOGIC
	PROTECT lExtKey		AS LOGIC
	PROTECT lShiftKey	AS LOGIC


/// <include file="Gui.xml" path="doc/HotKey.AltKey/*" />
ACCESS AltKey




	RETURN lAltKey


/// <include file="Gui.xml" path="doc/HotKey.AltKey/*" />
ASSIGN AltKey(lEnable)




	RETURN lAltKey := lEnable


/// <include file="Gui.xml" path="doc/HotKey.CtrlKey/*" />
ACCESS CtrlKey




	RETURN lCtrlKey


/// <include file="Gui.xml" path="doc/HotKey.CtrlKey/*" />
ASSIGN CtrlKey(lEnable)




	RETURN lCtrlKey := lEnable


/// <include file="Gui.xml" path="doc/HotKey.ExtendedKey/*" />
ACCESS ExtendedKey




	RETURN lExtKey


/// <include file="Gui.xml" path="doc/HotKey.ExtendedKey/*" />
ASSIGN ExtendedKey(lEnable)




	RETURN lExtKey := lEnable


/// <include file="Gui.xml" path="doc/HotKey.ctor/*" />
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




/// <include file="Gui.xml" path="doc/HotKey.Key/*" />
ACCESS Key




	RETURN bKey


/// <include file="Gui.xml" path="doc/HotKey.Key/*" />
ASSIGN Key(bNewKey)




	RETURN bKey := bNewKey


/// <include file="Gui.xml" path="doc/HotKey.ShiftKey/*" />
ACCESS ShiftKey




	RETURN lShiftKey


/// <include file="Gui.xml" path="doc/HotKey.ShiftKey/*" />
ASSIGN ShiftKey(lEnable)




	RETURN lShiftKey := lEnable


END CLASS


/// <include file="Gui.xml" path="doc/HotKeyEdit/*" />
CLASS HotKeyEdit INHERIT TextControl
	PROTECT oHotKeyRule	AS HotKeyRule


	//PP-030828 Strong typing
 /// <exclude />
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
		wInvalidModifiers := (WORD) _OR(wInvalidModifiers, HOTKEYF_ALT)
	ENDIF
	IF oNewRule:UseCtrlKeyOnInvalid
		wInvalidModifiers := (WORD) _OR(wInvalidModifiers, HOTKEYF_CONTROL)
	ENDIF
	IF oNewRule:UseExtendedKeyOnInvalid
		wInvalidModifiers := (WORD) _OR(wInvalidModifiers, HOTKEYF_EXT)
	ENDIF
	IF oNewRule:UseShiftKeyOnInvalid
		wInvalidModifiers := (WORD) _OR(wInvalidModifiers, HOTKEYF_SHIFT)
	ENDIF


	SendMessage(SELF:Handle(), HKM_SETRULES, dwInvalidCombinations, MakeLong(wInvalidModifiers, 0))
  	RETURN


/// <include file="Gui.xml" path="doc/HotKeyEdit.ApplyHotKey/*" />
METHOD ApplyHotKey(oWindow)
	LOCAL dwHotKey			AS DWORD






	Default(@oWindow, SELF:Owner)


	dwHotKey := (DWORD) _AND(SendMessage(SELF:Handle(), HKM_GETHOTKEY, 0, 0),0xFFFF)


	RETURN SendMessage(oWindow:Handle(), WM_SETHOTKEY, dwHotKey, 0)


/// <include file="Gui.xml" path="doc/HotKeyEdit.Create/*" />
METHOD Create()




	IF (SUPER:Create() != NULL_PTR)
		IF oHotKeyRule != NULL_OBJECT
			SELF:__SetRule(oHotKeyRule)
		ENDIF
	ENDIF


	RETURN hWnd


/// <include file="Gui.xml" path="doc/HotKeyEdit.HotKey/*" />
ACCESS HotKey
	LOCAL oHotKeyRet AS HotKey
	LOCAL wRet AS WORD


	wRet := (WORD) _AND(0xFFFF,SendMessage(SELF:Handle(), DWORD(HKM_GETHOTKEY), 0U, 0L))


	oHotKeyRet := HotKey{LoByte(wRet)}
	oHotKeyRet:AltKey 		:= (_AND(HiByte(wRet), HOTKEYF_ALT) > 0)
	oHotKeyRet:CtrlKey 		:= (_AND(HiByte(wRet), HOTKEYF_CONTROL) > 0)
	oHotKeyRet:ExtendedKey 	:= (_AND(HiByte(wRet), HOTKEYF_EXT) > 0)
	oHotKeyRet:ShiftKey 		:= (_AND(HiByte(wRet), HOTKEYF_SHIFT) > 0)


	RETURN oHotKeyRet


/// <include file="Gui.xml" path="doc/HotKeyEdit.HotKey/*" />
ASSIGN HotKey(oNewHotKey)
	LOCAL bModifierFlags AS BYTE






	IF oNewHotKey:AltKey
		bModifierFlags := (BYTE) _OR(bModifierFlags, HOTKEYF_ALT)
	ENDIF
	IF oNewHotKey:CtrlKey
		bModifierFlags := (BYTE) _OR(bModifierFlags, HOTKEYF_CONTROL)
	ENDIF
	IF oNewHotKey:ExtendedKey
		bModifierFlags := (BYTE) _OR(bModifierFlags, HOTKEYF_EXT)
	ENDIF
	IF oNewHotKey:ShiftKey
		bModifierFlags := (BYTE) _OR(bModifierFlags, HOTKEYF_SHIFT)
	ENDIF


	SendMessage(SELF:Handle(), HKM_SETHOTKEY, MakeWord(oNewHotKey:Key, bModifierFlags), 0)


	RETURN


/// <include file="Gui.xml" path="doc/HotKeyEdit.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)






	IF !IsNil(kStyle)
		dwStyle := _OR(dwStyle, DWORD(kStyle))
	ENDIF


	IF !(xID IS ResourceID)
		SUPER(oOwner, xID, oPoint, oDimension, HOTKEY_CLASS, dwStyle)
	ELSE
		SUPER(oOwner, xID, oPoint, oDimension, , dwStyle)
	ENDIF


	SELF:__ClassName := HOTKEY_CLASS


	RETURN


/// <include file="Gui.xml" path="doc/HotKeyEdit.Rule/*" />
ACCESS Rule




	RETURN oHotKeyRule


/// <include file="Gui.xml" path="doc/HotKeyEdit.Rule/*" />
ASSIGN Rule(oNewRule)




	oHotKeyRule := oNewRule
	SELF:__SetRule(oNewRule)


	RETURN
END CLASS


/// <include file="Gui.xml" path="doc/HotKeyRule/*" />
CLASS HotKeyRule INHERIT VObject
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


/// <include file="Gui.xml" path="doc/HotKeyRule.AltKeyInvalid/*" />
ACCESS AltKeyInvalid




	RETURN lAltKeyInvalid


/// <include file="Gui.xml" path="doc/HotKeyRule.AltKeyInvalid/*" />
ASSIGN AltKeyInvalid(lEnable)




	RETURN lAltKeyInvalid := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.CtrlAltKeysInvalid/*" />
ACCESS CtrlAltKeysInvalid




	RETURN lCtrlAltKeysInvalid


/// <include file="Gui.xml" path="doc/HotKeyRule.CtrlAltKeysInvalid/*" />
ASSIGN CtrlAltKeysInvalid(lEnable)




	RETURN lCtrlAltKeysInvalid := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.CtrlKeyInvalid/*" />
ACCESS CtrlKeyInvalid




	RETURN lCtrlKeyInvalid


/// <include file="Gui.xml" path="doc/HotKeyRule.CtrlKeyInvalid/*" />
ASSIGN CtrlKeyInvalid(lEnable)




	RETURN lCtrlKeyInvalid := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.ctor/*" />
CONSTRUCTOR()


    SUPER()




RETURN


/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftAltKeysInvalid/*" />
ACCESS ShiftAltKeysInvalid




	RETURN lShiftAltKeysInvalid


/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftAltKeysInvalid/*" />
ASSIGN ShiftAltKeysInvalid(lEnable)






	RETURN lShiftAltKeysInvalid := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftCtrlAltKeysInvalid/*" />
ACCESS ShiftCtrlAltKeysInvalid




	RETURN lShiftCtrlAltKeysInvalid


/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftCtrlAltKeysInvalid/*" />
ASSIGN ShiftCtrlAltKeysInvalid(lEnable)




	RETURN lShiftCtrlAltKeysInvalid := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftCtrlKeysInvalid/*" />
ACCESS ShiftCtrlKeysInvalid




	RETURN lShiftCtrlKeysInvalid


/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftCtrlKeysInvalid/*" />
ASSIGN ShiftCtrlKeysInvalid(lEnable)




	RETURN lShiftCtrlKeysInvalid := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftKeyInvalid/*" />
ACCESS ShiftKeyInvalid




	RETURN lShiftKeyInvalid


/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftKeyInvalid/*" />
ASSIGN ShiftKeyInvalid(lEnable)




	RETURN lShiftKeyInvalid := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.UnmodifiedKeysInvalid/*" />
ACCESS UnmodifiedKeysInvalid




	RETURN lUnmodKeysInvalid


/// <include file="Gui.xml" path="doc/HotKeyRule.UnmodifiedKeysInvalid/*" />
ASSIGN UnmodifiedKeysInvalid(lEnable)




	RETURN lUnmodKeysInvalid := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.UseAltKeyOnInvalid/*" />
ACCESS UseAltKeyOnInvalid




	RETURN lInvalidAltKey


/// <include file="Gui.xml" path="doc/HotKeyRule.UseAltKeyOnInvalid/*" />
ASSIGN UseAltKeyOnInvalid(lEnable)






	RETURN lInvalidAltKey := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.UseCtrlKeyOnInvalid/*" />
ACCESS UseCtrlKeyOnInvalid




	RETURN lInvalidCtrlKey


/// <include file="Gui.xml" path="doc/HotKeyRule.UseCtrlKeyOnInvalid/*" />
ASSIGN UseCtrlKeyOnInvalid(lEnable)




	RETURN lInvalidCtrlKey := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.UseExtendedKeyOnInvalid/*" />
ACCESS UseExtendedKeyOnInvalid




	RETURN lInvalidExtKey


/// <include file="Gui.xml" path="doc/HotKeyRule.UseExtendedKeyOnInvalid/*" />
ASSIGN UseExtendedKeyOnInvalid(lEnable)




	RETURN lInvalidExtKey := lEnable


/// <include file="Gui.xml" path="doc/HotKeyRule.UseShiftKeyOnInvalid/*" />
ACCESS UseShiftKeyOnInvalid




	RETURN lInvalidShiftKey


/// <include file="Gui.xml" path="doc/HotKeyRule.UseShiftKeyOnInvalid/*" />
ASSIGN UseShiftKeyOnInvalid(lEnable)




	RETURN lInvalidShiftKey := lEnable


END CLASS


