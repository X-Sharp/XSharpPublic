FUNCTION Start() AS VOID
	LOCAL a := "Robert" AS STRING
    LOCAL b := 1 AS LONG
    PRIVATE d
    PUBLIC e
    ? Type("c := b := 42")
    ? a
    ? b
    ? Type("c")
    ? d
    d := ToDay()
    ? d
    ? e
    WAIT

RETURN
