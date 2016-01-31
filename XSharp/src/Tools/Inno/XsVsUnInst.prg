// This program is run at uninstall time to 
// delete the extension manager keys and the project and item templates
// cache for the user

FUNCTION Start AS VOID
	LOCAL oKey AS Microsoft.Win32.RegistryKey
	TRY
	oKey := Microsoft.Win32.Registry.CurrentUser
	oKey := oKey:OpenSubKey("SOFTWARE\Microsoft\VisualStudio\14.0\ExtensionManager",TRUE)    
	IF oKey != NULL_OBJECT
		FOREACH sName AS STRING IN 	oKey:GetSubKeyNames()
			oKey:DeleteSubKey(sName)   
		NEXT
		oKey := Microsoft.Win32.Registry.CurrentUser
		oKey:DeleteSubKey("SOFTWARE\Microsoft\VisualStudio\14.0\ExtensionManager")
	ENDIF
	LOCAL cPath AS STRING
	cPath := System.Environment.GetFolderPath(System.Environment.SpecialFolder.LocalApplicationData)
	IF ! cPath:EndsWith("\")
		cPath += "\"
	ENDIF
	cPath += "Microsoft\VisualStudio\14.0\vtc"
	IF System.IO.Directory.Exists(cPath)
		System.IO.Directory.Delete(cPath, TRUE)
	ENDIF
	END TRY
	RETURN

