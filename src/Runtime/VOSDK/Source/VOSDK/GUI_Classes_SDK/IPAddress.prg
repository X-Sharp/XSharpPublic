/// <include file="Gui.xml" path="doc/IPAddress/*" />
CLASS IPAddress INHERIT TextControl


 /// <exclude />
METHOD __SetText(sNewText AS STRING) AS STRING STRICT
	//PP-030828 Strong typing
	SELF:Address := Val(sNewText)
	RETURN sNewText


 /// <exclude />
ASSIGN __Value(uValue AS USUAL)  STRICT
	//PP-030828 Strong typing
	SUPER:__Value := uValue


	IF IsNumeric(uValue)
		SELF:address := uValue
	ENDIF
	RETURN


/// <include file="Gui.xml" path="doc/IPAddress.Address/*" />
ACCESS Address
	LOCAL dwAddr AS DWORD
	SendMessage(SELF:handle(), IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))


	RETURN LONGINT(_CAST, dwAddr)




/// <include file="Gui.xml" path="doc/IPAddress.Address/*" />
ASSIGN Address(nNewAddress)
	LOCAL dwAddr AS DWORD
	dwAddr := nNewAddress
	SendMessage(SELF:handle(), IPM_SETADDRESS, 0, LONGINT(_CAST, dwAddr))


	RETURN


/// <include file="Gui.xml" path="doc/IPAddress.EditHandle/*" />
ACCESS EditHandle


	// DHer: 18/12/2008
	IF SELF:ValidateControl()
		RETURN GetWindow(SELF:hWnd,GW_CHILD)
	ENDIF


RETURN NULL_PTR


/// <include file="Gui.xml" path="doc/IPAddress.Field1/*" />
ACCESS Field1
	LOCAL dwAddr AS DWORD
	SendMessage(SELF:handle(), IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))
	RETURN _AND(dwAddr >> 24, 0x000000FF)




/// <include file="Gui.xml" path="doc/IPAddress.Field1/*" />
ASSIGN Field1(iNewVal)
	LOCAL dwAddr AS DWORD
    EnForceNumeric(@iNewVal)
	SendMessage(SELF:handle(), IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))
	dwAddr := _Or(_And(0x00FFFFFFU, dwAddr), (DWORD(_CAST, iNewVal) << 24))
	SendMessage(SELF:handle(), IPM_SETADDRESS, 0, LONGINT(_CAST, dwAddr))
	RETURN


/// <include file="Gui.xml" path="doc/IPAddress.Field2/*" />
ACCESS Field2
	LOCAL dwAddr AS DWORD
	SendMessage(SELF:handle(), IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))
	RETURN _And((dwAddr >> 16), 0x000000FF)




/// <include file="Gui.xml" path="doc/IPAddress.Field2/*" />
ASSIGN Field2(iNewVal)
	LOCAL dwAddr AS DWORD
    EnForceNumeric(@iNewVal)
	SendMessage(SELF:handle(), IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))
	dwAddr := _Or(_And(0xFF00FFFF, dwAddr), (DWORD(_CAST, iNewVal) << 16))
	SendMessage(SELF:handle(), IPM_SETADDRESS, 0, LONGINT(_CAST, dwAddr))


	RETURN




/// <include file="Gui.xml" path="doc/IPAddress.Field3/*" />
ACCESS Field3
	LOCAL dwAddr AS DWORD
	SendMessage(SELF:handle(), IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))
	RETURN _And((dwAddr >> 8), 0x000000FF)




/// <include file="Gui.xml" path="doc/IPAddress.Field3/*" />
ASSIGN Field3(iNewVal)
	LOCAL dwAddr AS DWORD
    EnForceNumeric(@iNewVal)
	SendMessage(SELF:handle(), IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))
	dwAddr := _Or(_And(0xFFFF00FF, dwAddr), (DWORD(_CAST, iNewVal) << 8))
	SendMessage(SELF:handle(), IPM_SETADDRESS, 0, LONGINT(_CAST, dwAddr))
	RETURN




/// <include file="Gui.xml" path="doc/IPAddress.Field4/*" />
ACCESS Field4
	LOCAL dwAddr AS DWORD
	SendMessage(SELF:handle(), IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))
	RETURN _And(dwAddr, 0x000000FF)


/// <include file="Gui.xml" path="doc/IPAddress.Field4/*" />
ASSIGN Field4(iNewVal)
	LOCAL dwAddr AS DWORD
    EnForceNumeric(@iNewVal)
	SendMessage(SELF:handle(), IPM_GETADDRESS, 0, LONGINT(_CAST, @dwAddr))
	dwAddr := _Or(_And(0xFFFFFF00, dwAddr), (DWORD(_CAST, iNewVal)))
	SendMessage(SELF:handle(), IPM_SETADDRESS, 0, LONGINT(_CAST, dwAddr))


	RETURN




/// <include file="Gui.xml" path="doc/IPAddress.ctor/*" />
CONSTRUCTOR(oOwner, xID, oPoint, oDimension, dwStyle, lDataAware)
	Default(@lDataAware, TRUE)


	SUPER(oOwner, xID, oPoint, oDimension, "SysIPAddress32", dwStyle, lDataAware)
	RETURN


/// <include file="Gui.xml" path="doc/IPAddress.SetRange/*" />
METHOD SetRange(iFieldIndex, iLower, iUpper)
	LOCAL wRange AS DWORD
	LOCAL iField AS DWORD


	iField := iFieldIndex -1
	wRange := _Or((LoWord(iUpper) << 8), LoWord( iLower))
	RETURN (SendMessage(SELF:Handle(), IPM_SETRANGE, iField, LONGINT(_CAST, wRange)) != 0)




/// <include file="Gui.xml" path="doc/IPAddress.TextValue/*" />
ACCESS TextValue

	//PP-030910 Bug 83
	RETURN NTrim(SELF:FIELD1)+"."+NTrim(SELF:FIELD2)+"."+NTrim(SELF:FIELD3)+"."+NTrim(SELF:FIELD4)


/// <include file="Gui.xml" path="doc/IPAddress.TextValue/*" />
ASSIGN TextValue(cIPAddress)
	//PP-030910
	LOCAL nPos AS DWORD
	LOCAL nField AS DWORD
	LOCAL cField AS STRING


	IF Occurs(".",cIPAddress) == 3 // Right number of separators
		// Add another "." for our loop logic
		cIPAddress := AllTrim(cIPAddress) + "."
		DO WHILE ( nPos := At(".",cIPAddress) ) > 0
			nField++
			cField := AllTrim(Left(cIPAddress,nPos-1))
			IVarPutSelf(SELF,String2Symbol("Field"+NTrim(nField)),Val(cField))
			cIPAddress := SubStr(cIPAddress,nPos+1)
		ENDDO
	ENDIF

	RETURN


/// <include file="Gui.xml" path="doc/IPAddress.Value/*" />
ACCESS Value
	RETURN SELF:Address
END CLASS


