
USING System.Text
/// <include file="Gui.xml" path="doc/ResourceString/*" />
CLASS ResourceString INHERIT VObject
	PROTECT iLength AS INT
	PROTECT sBuffer AS STRING

/// <include file="Gui.xml" path="doc/ResourceString.AsString/*" />
	METHOD AsString()
		RETURN sBuffer

/// <include file="Gui.xml" path="doc/ResourceString.ctor/*" />
	CONSTRUCTOR(xResourceID, nMaxLen)
		LOCAL hInst AS IntPtr
		LOCAL wID AS LONG
		LOCAL ptrBuffer AS StringBuilder
		LOCAL oResourceID as ResourceID

		SUPER()

		IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID)
			oResourceID := ResourceID{xResourceID} // , GetNatDLLHandle()}
		ELSEIF IsInstanceOfUsual(xResourceID, #ResourceID)
			oResourceID := xResourceID
		ELSE
			WCError{#Init, #ResourceString, __WCSTypeError, xResourceID, 1}:Throw()
		ENDIF

		IF IsNil(nMaxLen)
			nMaxLen := 256
		ELSEIF !IsNumeric(nMaxLen)
			WCError{#Init, #ResourceString, __WCSTypeError, nMaxLen, 2}:Throw()
		ELSE
			NOP 
		ENDIF

		hInst := oResourceID:Handle()

		IF String.IsNullOrEmpty(oResourceID:Name)
			wID := Val(oResourceID:Name)
		ELSE
			wID := oResourceID:ID
		ENDIF

		IF hInst == GetNatDllHandle()
			sBuffer := __CavoStr( (DWORD) wID )
			RETURN
		ENDIF

		ptrBuffer 	:= StringBuilder{(INT) nMaxLen+1}
		iLength 	:= GuiWin32.LoadString(hInst, wID, ptrBuffer, nMaxLen)
		IF iLength != 0
			sBuffer := ptrBuffer:ToString()
		ENDIF

		RETURN

/// <include file="Gui.xml" path="doc/ResourceString.Length/*" />
	ACCESS Length AS INT
		RETURN iLength

/// <include file="Gui.xml" path="doc/ResourceString.Value/*" />
	ACCESS Value AS STRING
		RETURN sBuffer

END CLASS

