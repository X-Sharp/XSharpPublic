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
	LOCAL oExplorerShellWindow AS ExplorerShellWindow

	oExplorerShellWindow := ExplorerShellWindow{SELF}
	oExplorerShellWindow:Show(SHOWCENTERED)

	SELF:Exec()
	
RETURN NIL



END CLASS
