// 411. Compiler crash assigning FLOAT to OBJECT
FUNCTION Start( ) AS VOID
	LOCAL o AS OBJECT
	LOCAL f := 123.456 AS FLOAT
	LOCAL r := 20.17 AS REAL8
	
	o := f
	? o
	o := 7.8
	? o
	o := r
	? o
	
	VAR dict := System.Collections.Generic.Dictionary<STRING,OBJECT>{}
	dict["test"] := f
	? dict["test"]
	dict["test"] := 7.8
	? dict["test"]
RETURN
