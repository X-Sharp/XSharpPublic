USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start AS VOID
TRY	
	? Os()
    ? Os(TRUE)

CATCH e AS Exception
    ErrorDialog(e)
END TRY    
WAIT
RETURN
