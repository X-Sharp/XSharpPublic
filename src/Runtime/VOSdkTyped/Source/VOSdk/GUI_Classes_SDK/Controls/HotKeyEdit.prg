//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/HotKey/*" />
[XSharp.Internal.TypesChanged];
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
/// <include file="Gui.xml" path="doc/HotKey.AltKey/*" />
	PROPERTY AltKey       AS LOGIC AUTO

	/// <include file="Gui.xml" path="doc/HotKey.CtrlKey/*" />
	PROPERTY CtrlKey      AS LOGIC AUTO

	/// <include file="Gui.xml" path="doc/HotKey.ExtendedKey/*" />
	PROPERTY ExtendedKey  AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKey.Key/*" />
	PROPERTY Key          AS BYTE AUTO
/// <include file="Gui.xml" path="doc/HotKey.ShiftKey/*" />
	PROPERTY ShiftKey     AS LOGIC AUTO

END CLASS
/// <include file="Gui.xml" path="doc/HotKeyEdit/*" />

[XSharp.Internal.TypesChanged];
CLASS HotKeyEdit INHERIT TextControl
	PROTECT oHotKeyRule	AS HotKeyRule
    PROPERTY ControlType AS ControlType GET ControlType.Hotkey


 /// <exclude />
	METHOD __SetRule(oNewRule AS HotKeyRule) AS VOID STRICT
		LOCAL dwInvalidCombinations	AS INT
		LOCAL wInvalidModifiers	    AS INT

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

/// <include file="Gui.xml" path="doc/HotKeyEdit.ApplyHotKey/*" />
	method ApplyHotKey(oWindow as IGuiObject)
		LOCAL dwHotKey			AS DWORD

		if oWindow == null
			oWindow := (IGuiObject)self:Owner
		ENDIF

		dwHotKey := (DWORD)_AND(GuiWin32.SendMessage(SELF:Handle(), HKM_GETHOTKEY, 0, 0),0XFFFF)

		RETURN GuiWin32.SendMessage(oWindow:__Handle, WM_SETHOTKEY, dwHotKey, 0)

/// <include file="Gui.xml" path="doc/HotKeyEdit.Create/*" />
	method Create() as System.Windows.Forms.Control strict
		IF (SUPER:Create() != NULL_OBJECT)
			IF oHotKeyRule != NULL_OBJECT
				SELF:__SetRule(oHotKeyRule)
			ENDIF
		ENDIF

		return oCtrl


/// <include file="Gui.xml" path="doc/HotKeyEdit.HotKey/*" />
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

/// <include file="Gui.xml" path="doc/HotKeyEdit.HotKey/*" />
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

/// <include file="Gui.xml" path="doc/HotKeyEdit.ctor/*" />
	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, kStyle)

		//IF !IsNil(kStyle)
		//	dwStyle := _OR(dwStyle, kStyle)
		//ENDIF
		SELF:cClassName := HOTKEY_CLASS

		IF !(xID IS ResourceID)
			SUPER(oOwner, xID, oPoint, oDimension, HOTKEY_CLASS, kStyle)
		ELSE
			SUPER(oOwner, xID, oPoint, oDimension, , kStyle)
		ENDIF


		RETURN

/// <include file="Gui.xml" path="doc/HotKeyEdit.Rule/*" />
	ACCESS Rule AS HotKeyRule
		RETURN oHotKeyRule

/// <include file="Gui.xml" path="doc/HotKeyEdit.Rule/*" />
	ASSIGN Rule(oNewRule AS HotKeyRule)
		oHotKeyRule := oNewRule
		SELF:__SetRule(oNewRule)

		RETURN
END CLASS

/// <include file="Gui.xml" path="doc/HotKeyRule/*" />
CLASS HotKeyRule INHERIT VObject

/// <include file="Gui.xml" path="doc/HotKeyRule.AltKeyInvalid/*" />

	PROPERTY AltKeyInvalid           AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.CtrlAltKeysInvalid/*" />
	PROPERTY CtrlAltKeysInvalid      AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.CtrlKeyInvalid/*" />
	PROPERTY CtrlKeyInvalid          AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftAltKeysInvalid/*" />
	PROPERTY ShiftAltKeysInvalid     AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftCtrlAltKeysInvalid/*" />
	PROPERTY ShiftCtrlAltKeysInvalid AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftCtrlKeysInvalid/*" />
	PROPERTY ShiftCtrlKeysInvalid    AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.ShiftKeyInvalid/*" />
	PROPERTY ShiftKeyInvalid         AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.UnmodifiedKeysInvalid/*" />
	PROPERTY UnmodifiedKeysInvalid   AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.UseAltKeyOnInvalid/*" />
	PROPERTY UseAltKeyOnInvalid      AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.UseCtrlKeyOnInvalid/*" />
	PROPERTY UseCtrlKeyOnInvalid     AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.UseExtendedKeyOnInvalid/*" />
	PROPERTY UseExtendedKeyOnInvalid AS LOGIC AUTO
/// <include file="Gui.xml" path="doc/HotKeyRule.UseShiftKeyOnInvalid/*" />
	PROPERTY UseShiftKeyOnInvalid    AS LOGIC AUTO

END CLASS

