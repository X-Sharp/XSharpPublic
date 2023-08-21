GLOBAL oDel as MyDelegate
DELEGATE MyDelegate() AS VOID

FUNCTION Start( ) AS VOID
    oDel := TestMyDelegate
    oDel()	
RETURN


FUNCTION TestMyDelegate() AS VOID
    RETURN 
