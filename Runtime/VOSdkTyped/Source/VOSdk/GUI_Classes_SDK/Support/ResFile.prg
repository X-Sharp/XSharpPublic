

CLASS ResourceFile INHERIT VObject
	PROTECT hLib AS IntPtr

METHOD Destroy() AS USUAL CLIPPER
	IF ((int) hLib >= 32)
		Win32.FreeLibrary(hLib)
	ENDIF

	hLib := IntPtr.Zero
	RETURN SELF

METHOD Handle() AS IntPtr STRICT
	RETURN hLib

CONSTRUCTOR(sName AS STRING) 
	LOCAL rsCaption AS ResourceString
	LOCAL rsFormat AS ResourceString
	LOCAL sMessage AS STRING
	
	SUPER()

	hLib := Win32.LoadLibrary(sName)
	IF (Int) hLib < 32
		rsCaption	:= ResourceString{__WCSLibraryName}
		rsFormat	:= ResourceString{__WCSLoadLibraryError}
		sMessage	:= VO_Sprintf(rsFormat:Value, sName)
		System.Windows.Forms.MessageBox.Show(sMessage, rsCaption:Value)
	ENDIF

	RETURN 

END CLASS

