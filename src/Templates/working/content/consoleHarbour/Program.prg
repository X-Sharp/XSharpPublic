USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION Start() AS VOID STRICT
	? "Hello World! Today is ",Today()
	//
	os = Environment.OSVersion
	? "You are running on :"
	?  "OS Version: " + os.Version.ToString() 
	?  "OS Platform: " + os.Platform.ToString() 
    ? "Press any key to continue..."
    WAIT
    RETURN
	



