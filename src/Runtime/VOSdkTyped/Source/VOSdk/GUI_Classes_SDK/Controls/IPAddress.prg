//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
/// <include file="Gui.xml" path="doc/IPAddress/*" />

[XSharp.Internal.TypesChanged];
CLASS IPAddress INHERIT TextControl
	/// <inheritdoc />
    PROPERTY ControlType AS ControlType  GET ControlType.IPAddress

/// <include file="Gui.xml" path="doc/IPAddress.ctor/*" />

	CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware)
		DEFAULT(REF lDataAware, TRUE)

		SUPER(oOwner, xID, oPoint, oDimension, "SysIPAddress32", dwStyle, lDataAware)
		RETURN

 /// <exclude />
	METHOD __SetText(sNewText AS STRING) AS STRING STRICT
		SELF:Address := Val(sNewText)
		RETURN sNewText

 /// <exclude />
	ASSIGN __Value(uValue AS USUAL)  STRICT
		SUPER:__Value := uValue

		IF IsNumeric(uValue)
			SELF:Address := uValue
		ENDIF
		RETURN

/// <include file="Gui.xml" path="doc/IPAddress.Address/*" />
	ACCESS Address AS DWORD
		LOCAL dwAddr AS DWORD
		GuiWin32.SendMessage(oCtrl:Handle, IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))
		RETURN dwAddr


/// <include file="Gui.xml" path="doc/IPAddress.Address/*" />
	ASSIGN Address(nNewAddress AS DWORD)
		LOCAL dwAddr AS DWORD
		dwAddr := nNewAddress
		GuiWin32.SendMessage(oCtrl:Handle, IPM_SETADDRESS, 0, LONGINT(_CAST, dwAddr))

		RETURN

/// <include file="Gui.xml" path="doc/IPAddress.EditHandle/*" />
	ACCESS EditHandle AS IntPtr
		IF SELF:ValidateControl()
			//RETURN GuiWin32.GetWindow(SELF:oCtrl:Handle,GW_CHILD)
			RETURN SELF:oCtrl:Handle
		ENDIF

		RETURN IntPtr.Zero
     /// <exclude />

    PRIVATE METHOD __GetAddress(nBits as LONG) AS BYTE
		LOCAL dwAddr AS DWORD
		dwAddr := SELF:Address
        RETURN (BYTE) _AND(dwAddr >> nBits, 0X000000FF)
    /// <exclude />
    PRIVATE METHOD __SetAddress(nBits as LONG, nMask as DWORD, nByte as BYTE) AS VOID
		LOCAL dwAddr AS DWORD
		dwAddr := SELF:Address
		dwAddr := _Or(_And(nMask, dwAddr), (DWORD(_CAST, nByte) << nBits))
		SELF:Address := dwAddr
		RETURN

    /// <include file="Gui.xml" path="doc/IPAddress.Field1/*" />
	PROPERTY Field1  AS BYTE GET SELF:__GetAddress(24) SET SELF:__SetAddress(24, 0X00FFFFFFU, value)

    /// <include file="Gui.xml" path="doc/IPAddress.Field2/*" />
	PROPERTY Field2  AS BYTE GET SELF:__GetAddress(16) SET SELF:__SetAddress(16, 0XFF00FFFF, value)

    /// <include file="Gui.xml" path="doc/IPAddress.Field3/*" />
	PROPERTY Field3  AS BYTE GET SELF:__GetAddress(8) SET SELF:__SetAddress(8, 0XFFFF00FF, value)

    /// <include file="Gui.xml" path="doc/IPAddress.Field4/*" />
	PROPERTY Field4  AS BYTE GET SELF:__GetAddress(0) SeT SELF:__SetAddress(0, 0XFFFFFF00, value)

    /// <include file="Gui.xml" path="doc/IPAddress.SetRange/*" />
    METHOD SetRange(iFieldIndex, iLower, iUpper)
		LOCAL wRange AS LONG
		LOCAL iField AS DWORD

		iField := iFieldIndex -1
		wRange := _Or((LoWord(iUpper) << 8), LoWord( iLower))
		RETURN (GuiWin32.SendMessage(oCtrl:Handle, IPM_SETRANGE, iField, LONGINT(_CAST, wRange)) != 0)


/// <include file="Gui.xml" path="doc/IPAddress.TextValue/*" />
	ACCESS TextValue AS STRING
		RETURN NTrim(SELF:Field1)+"."+NTrim(SELF:Field2)+"."+NTrim(SELF:Field3)+"."+NTrim(SELF:Field4)

/// <include file="Gui.xml" path="doc/IPAddress.TextValue/*" />
	ASSIGN TextValue(cIPAddress  AS STRING)
		//PP-030910
		LOCAL aString AS STRING[]
		LOCAL aField AS DWORD[]
		LOCAL nAddress AS DWORD

		aString := cIPAddress:Split(<Char>{'.'})
		IF aString:Length == 4
			aField := DWORD[]{4}
			aField[1] := Val(aString[1])
			aField[2] := Val(aString[2])
			aField[3] := Val(aString[3])
			aField[4] := Val(aString[4])
			nAddress := aField[1] << 24
			nAddress += aField[2] << 16
			nAddress += aField[3] << 8
			nAddress += aField[4]
			SELF:Address :=  nAddress
		ENDIF

/// <include file="Gui.xml" path="doc/IPAddress.Value/*" />
	ACCESS Value
		RETURN SELF:Address
END CLASS


