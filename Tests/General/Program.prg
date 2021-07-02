USING VO

FUNCTION Start() AS VOID
LOCAL ctest AS STRING
TRY
    cTest := "? 'Hello world'"
    ExecScriptFast(cTest)
    cTest :=String.Join(e"\n",<STRING>{;
        "PARAMETERS a,b,c",;
        "RETURN CallMe(a,b,c)"})
    ? ExecScriptFast(cTest,1,2,3)
    cTest :=String.Join(e"\n",<STRING>{;
        "LPARAMETERS a,b,c",;
        "RETURN CallMe(a,b,c)"})
    ? ExecScriptFast(cTest,1,2,3)

CATCH e AS Exception
    ? e:ToString()
    END TRY

wait
RETURN


FUNCTION CallMe(a,b,c) AS USUAL
    ? "Inside function, parameters received",a,b,c
    RETURN a+b+c
