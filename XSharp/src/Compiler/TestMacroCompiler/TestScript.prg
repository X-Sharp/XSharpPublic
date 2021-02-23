USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION testScript() AS VOID STRICT
    LOCAL cScript as STRING
    TRY
    TEXT to cScript  PRETEXT 3
        LOCAL x :=0 as LONG
        x := 10
        x += "abc"
        ? x
    ENDTEXT
    ExecScript(cScript,null)
    CATCH e as Exception
        ? e:ToString()
    END TRY
    WAIT
	RETURN	
