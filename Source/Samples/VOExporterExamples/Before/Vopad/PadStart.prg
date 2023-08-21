GLOBAL aCB := {} AS ARRAY

[STAThread];
FUNCTION Start() AS INT
	LOCAL oXApp AS XApp
	TRY
		oXApp := XApp{}
		oXApp:Start()
	CATCH oException AS Exception
		ErrorDialog(oException)
	END TRY
RETURN 0

CLASS XApp INHERIT App
METHOD Start() 
	LOCAL oMainWindow AS PadShellWindow
	LOCAL cParam AS STRING
	LOCAL nStart AS INT
	
	cParam := _GetCmdLine()
	nStart := At(" ", Trim(cParam))
	oMainWindow := PadShellWindow{SELF}
	
	IF nStart != 0
		oMainWindow:NewEditWindow(SubStr(cParam, nStart + 1))
	ELSE	
		oMainWindow:FileNew()
	ENDIF	
	
	oMainWindow:Show(SHOWCENTERED)
	SELF:Exec()
	
	


END CLASS
