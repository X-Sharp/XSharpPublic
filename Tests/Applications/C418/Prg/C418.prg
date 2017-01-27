// 418. runtime exception using VulcanLoadLibrary() with x# compiled dlls
// problem happens because VulcanLoadLibrary() expects to find a method 
// RunInitProcs() (which calls all _INT procs) in the <Module> class
FUNCTION Start( ) AS VOID
	IF VulcanLoadLibrary( Environment.CurrentDirectory + "\C418_helper.dll") == NULL 
		THROW Exception{"Assembly not loaded"}
	ENDIF
	IF (Console.Title != "test")
		THROW Exception{"Init procedure not called"}
	ENDIF
	? Console.Title		
RETURN                                                                      
         



