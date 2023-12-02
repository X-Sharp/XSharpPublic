[STAThread];
FUNCTION Start() AS INT
	LOCAL oXApp AS XApp
	TRY
        RddSetDefault("DBFVFP")
		oXApp := XApp{}
		oXApp:Start()
	CATCH oException AS Exception
		ErrorDialog(oException)
	END TRY
RETURN 0

CLASS XApp INHERIT App
	METHOD Start()
		LOCAL oMainWindow AS StandardShellWindow
        oMainWindow := StandardShellWindow{SELF}
        DoEvents()
		oMainWindow:Show(SHOWCENTERED)

		SELF:Exec()
   RETURN NIL
  METHOD AfterDispatch( hwnd , msg , wparam , lparam) AS LOGIC
        RETURN TRUE
  METHOD BeforeDispatch( hwnd , msg , wparam , lparam ) AS LOGIC
        RETURN TRUE
END CLASS
