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
        LOCAL oMainWindow AS StandardShellWindow
        oMainWindow := StandardShellWindow{SELF}
        oMainWindow:Show(SHOWCENTERED)

        SELF:Exec()
   RETURN NIL
END CLASS
