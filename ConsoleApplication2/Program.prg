USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID STRICT
    LOCAL d1 AS DateTime
    LOCAL d AS DATE
    LOCAL f AS FLOAT
    LOCAL s AS SYMBOL
    LOCAL a AS ARRAY
    a := {1,2,3}
    s := #DoSomething
    f := 1.234
    d1 := DateTime{2020,4,20}
    d := d1
    ? d1, d,f,a,s
    WAIT
	RETURN	
