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
    ? ExecScript(cScript,1,2,3)
    ? ExecScript(cScript,4,5,6)
    ? ExecScript(cScript,7,8,9)
    CATCH e as Exception
        ? e:ToString()

    END TRY
    WAIT
	RETURN
