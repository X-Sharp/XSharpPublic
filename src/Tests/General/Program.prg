// 902. Macro compiler problem passing more than 2 arguments by reference
// https://www.xsharp.eu/forum/topic?p=29165#p29165
// https://github.com/X-Sharp/XSharpPublic/issues/1445

GLOBAL o AS TestClass
GLOBAL p1 AS STRING
GLOBAL p2 AS DATE
GLOBAL p3 AS INT
GLOBAL p4 AS INT
FUNCTION Start() AS VOID STRICT
	LOCAL cCmd	AS STRING

	o := TestClass{}
    p1 := ""
    p2 := null_date
    p3 := 0
    p4 := 0

    o:Test2(REF p1,REF p2) // OK
    ? p1,p2,p3,p4
    p1 := ""
    p2 := null_date
    p3 := 0
    p4 := 0
	o:Test4(REF p1,REF p2, REF p3, REF p4) // OK
    ? p1,p2,p3,p4

	cCmd := "o:Test2(ref p1,ref p2)"
    p1 := ""
    p2 := null_date
    p3 := 0
    p4 := 0
	Eval(&("{||"+cCmd+"}")) // OK
    ? p1,p2,p3,p4

	cCmd := "o:Test4(ref p1,ref p2,ref p3,ref p4)"
    p1 := ""
    p2 := null_date
    p3 := 0
    p4 := 0
	Eval(&("{||"+cCmd+"}")) // Exception

    ? p1,p2,p3,p4
    wait
RETURN

CLASS TestClass
	METHOD Test2(a REF STRING,b REF DATE) AS LOGIC
		a := "OK"
		b	:= Today()
	RETURN TRUE
	METHOD Test4(a REF STRING,b REF DATE, c REF INT, d REF INT) AS LOGIC
		a := "OK"
		b	:= Today()
		c := 1
		d := 2
	RETURN TRUE

END CLASS
