[STAThread];
FUNCTION Start() AS INT
	LOCAL oXApp AS XAPP
	TRY
		oXApp := XAPP{}
		oXApp:Start()
	CATCH oException AS Exception
		ErrorDialog(oException)
	END TRY
RETURN 0

CLASS XAPP INHERIT App
method Start() 
	local oMainWindow as GraphShellWindow
	
	oMainWindow := GraphShellWindow{self, gaSampleData1}
	oMainWindow:Show(SHOWCENTERED)
	
	self:Exec()
	
   return self	


END CLASS
