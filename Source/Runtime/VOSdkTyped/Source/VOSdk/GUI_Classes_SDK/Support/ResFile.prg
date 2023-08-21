/// <include file="Gui.xml" path="doc/ResourceFile/*" />

CLASS ResourceFile INHERIT VObject
	PROTECT hLib AS IntPtr

/// <include file="Gui.xml" path="doc/ResourceFile.Destroy/*" />
METHOD Destroy() AS USUAL
	IF ((int) hLib >= 32)
		GuiWin32.FreeLibrary(hLib)
	ENDIF

	hLib := IntPtr.Zero
	RETURN SELF

/// <include file="Gui.xml" path="doc/ResourceFile.Handle/*" />
METHOD Handle() AS IntPtr STRICT
	RETURN hLib

/// <include file="Gui.xml" path="doc/ResourceFile.ctor/*" />
CONSTRUCTOR(sName AS STRING)
	LOCAL rsCaption AS ResourceString
	LOCAL rsFormat AS ResourceString
	LOCAL sMessage AS STRING

	SUPER()

	hLib := GuiWin32.LoadLibrary(sName)
	IF (Int) hLib < 32
		rsCaption	:= ResourceString{__WCSLibraryName}
		rsFormat	:= ResourceString{__WCSLoadLibraryError}
		sMessage	:= VO_Sprintf(rsFormat:Value, sName)
		System.Windows.Forms.MessageBox.Show(sMessage, rsCaption:Value)
	ENDIF

	RETURN

END CLASS

