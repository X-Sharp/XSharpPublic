//#include "VOGUIClasses.vh"

[STAThreadAttribute];
FUNCTION Start( asCmdLine AS STRING[] ) AS INT
	
	LOCAL nExitCode AS INT
	LOCAL oMainWindow AS StandardShellWindow
	LOCAL oApp := NULL AS App
//DBServer{}
//? CreateInstance("VO.DBServer")
	
	RDDSetDefault( "DBF" )
	
	nExitCode := 0                            
	
	oApp := App{}
	oMainWindow := StandardShellWindow{oApp}
	oMainWindow:Show(SHOWCENTERED)
	oApp:Exec()
RETURN nExitCode







