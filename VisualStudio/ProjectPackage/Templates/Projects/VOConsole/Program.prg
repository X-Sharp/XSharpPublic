USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text


FUNCTION Start() AS VOID
    ? "Hello World! Today is ",ToDay()
    WAIT
	RETURN	
