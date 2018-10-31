PARTIAL CLASS ResourceString INHERIT VObject
	PROTECT iLength AS INT
	PROTECT sBuffer AS STRING

METHOD AsString() 
	

	RETURN sBuffer

CONSTRUCTOR(xResourceID, nMaxLen) 
	LOCAL hInst AS PTR
	LOCAL wID AS DWORD
	LOCAL ptrBuffer AS PTR

	SUPER()

	IF IsNumeric(xResourceID) .OR. IsSymbol(xResourceID)
		xResourceID := ResourceID{xResourceID} // , GetNatDLLHandle()}
	ELSEIF !IsInstanceOfUsual(xResourceID, #ResourceID)
		WCError{#Init, #ResourceString, __WCSTypeError, xResourceID, 1}:@@Throw()
	ENDIF

	IF IsNil(nMaxLen)
		nMaxLen := 256
	ELSEIF !IsNumeric(nMaxLen)
		WCError{#Init, #ResourceString, __WCSTypeError, nMaxLen, 2}:@@Throw()
	ENDIF

	hInst := xResourceID:Handle()

	IF IsString(xResourceID:ID)
		wID := DWORD(_CAST, String2Symbol(xResourceID:ID))
	ELSE
		wID := xResourceID:ID
	ENDIF

#ifdef __VULCAN__
   // If this is used to load a string from something other than
   // the nations DLL, use LoadString().  Otherwise, try to
   // load the string from the Vulcan runtime managed resources.
   IF hInst == GetNatDllHandle()
      sBuffer := __CavoStr( wID )
      RETURN
   ENDIF   
#endif

	ptrBuffer := MemAlloc(nMaxLen)
	iLength := LoadString(hInst, wID, ptrBuffer, nMaxLen)
	IF iLength != 0
		sBuffer := Psz2String(ptrBuffer)
	ENDIF
	MemFree(ptrBuffer)

	RETURN 

ACCESS Length 
	

	RETURN iLength

ACCESS Value 
	

	RETURN sBuffer

END CLASS

