PARTIAL CLASS ResourceFile INHERIT VObject
	PROTECT hLib AS PTR

METHOD Destroy() 
	

	IF (DWORD(_CAST, hLib) >= 32)
		FreeLibrary(hLib)
	ENDIF

	IF !InCollect()
		hLib := NULL_PTR
	ENDIF
	RETURN SELF

METHOD Handle() AS PTR
	

	RETURN hLib

CONSTRUCTOR(sName) 
	LOCAL rsCaption AS ResourceString
	LOCAL rsFormat AS ResourceString
	LOCAL sMessage AS STRING

	
	SUPER()

	IF !IsString(sName)
		WCError{#Init, #ResourceFile, __WCSTypeError, sName, 1}:@@Throw()
	ENDIF

	hLib := LoadLibrary(String2Psz(sName))
	IF DWORD(_CAST, hLib) < 32
		rsCaption := ResourceString{__WCSLibraryName}
		rsFormat := ResourceString{__WCSLoadLibraryError}
		sMessage := VO_Sprintf(rsFormat:value, sName)
		MessageBox(0, String2Psz(sMessage), String2Psz(rsCaption:VALUE), _OR(MB_TASKMODAL, _OR(MB_ICONHAND,MB_OK)))
	ENDIF

	

	RETURN 

END CLASS

