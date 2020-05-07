
#USING System.Text
CLASS ResourceString INHERIT VObject
	PROTECT iLength AS INT
	PROTECT sBuffer AS STRING

	METHOD AsString() 
		RETURN sBuffer

	CONSTRUCTOR(xResourceID, uMaxLen) 
		LOCAL hInst AS IntPtr
		LOCAL wID AS LONG
		LOCAL ptrBuffer AS StringBuilder
		LOCAL oResourceID as ResourceID
		LOCAL nMaxLen as LONG

		SUPER()

		IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID)
			oResourceID := ResourceID{xResourceID} // , GetNatDLLHandle()}
		ELSEIF IsInstanceOfUsual(xResourceID, #ResourceID)
			oResourceID := xResourceID
		ELSE
			WCError{#Init, #ResourceString, __WCSTypeError, xResourceID, 1}:@@Throw()
		ENDIF

		IF IsNil(uMaxLen)
			nMaxLen := 256
		ELSEIF !IsNumeric(nMaxLen)
			WCError{#Init, #ResourceString, __WCSTypeError, nMaxLen, 2}:@@Throw()
		ELSE
			nMaxLen := (LONG) uMaxLen
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
		iLength 	:= Win32.LoadString(hInst, wID, ptrBuffer, nMaxLen)
		IF iLength != 0
			sBuffer := ptrBuffer:ToString()
		ENDIF

		RETURN 

	ACCESS Length AS INT
		RETURN iLength

	ACCESS Value AS STRING
		RETURN sBuffer

END CLASS

