USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text

FUNCTION testScript() AS VOID STRICT
    LOCAL cScript as STRING
    TRY
    TEXT to cScript  PRETEXT 3
        LPARAMETERS x,y,z
        ? x
        ? y
        ? z.ToString()
        ? i"z = {z.ToString()}"
        ? i"z = {z:F2}"
        RETURN TestMe(x,y,z)
        FUNCTION TestMe(x,y,z)
            RETURN x+y/z

    ENDTEXT
    ? ExecScriptSlow(cScript,1,2,3)
    ? ExecScriptSlow(cScript,4,5,6)
    ? ExecScriptSlow(cScript,7,8,9)
    CATCH e as Exception
        ? e:ToString()

    END TRY
    WAIT
	RETURN
