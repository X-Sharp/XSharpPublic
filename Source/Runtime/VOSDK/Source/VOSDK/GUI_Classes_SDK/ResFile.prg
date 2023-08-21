/// <include file="Gui.xml" path="doc/ResourceFile/*" />
CLASS ResourceFile INHERIT VObject
	PROTECT hLib AS PTR


/// <include file="Gui.xml" path="doc/ResourceFile.Destroy/*" />
METHOD Destroy()  AS USUAL CLIPPER
	
	


	IF (DWORD(_CAST, hLib) >= 32)
		FreeLibrary(hLib)
	ENDIF


	IF !InCollect()
		hLib := NULL_PTR
	ENDIF
	RETURN SELF


/// <include file="Gui.xml" path="doc/ResourceFile.Handle/*" />
METHOD Handle() AS PTR
	
	


	RETURN hLib


/// <include file="Gui.xml" path="doc/ResourceFile.ctor/*" />
CONSTRUCTOR(sName) 
	LOCAL rsCaption AS ResourceString
	LOCAL rsFormat AS ResourceString
	LOCAL sMessage AS STRING


	
	
	SUPER()


	IF !IsString(sName)
		WCError{#Init, #ResourceFile, __WCSTypeError, sName, 1}:Throw()
	ENDIF


	hLib := LoadLibrary(String2Psz(sName))
	IF DWORD(_CAST, hLib) < 32
		rsCaption := ResourceString{__WCSLibraryName}
		rsFormat := ResourceString{__WCSLoadLibraryError}
		sMessage := VO_Sprintf(rsFormat:value, sName)
		MessageBox(0, String2Psz(sMessage), String2Psz(rsCaption:Value), _OR(MB_TASKMODAL, _OR(MB_ICONHAND,MB_OK)))
	ENDIF


	
	


	RETURN 


END CLASS


