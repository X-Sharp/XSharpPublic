
FUNCTION Start( ) AS VOID
	LOCAL a, b AS LOGIC
	a := .T.
	b := .F.
	? a.and.b
	? a.or.b
	? .not.a
	? .not.b
	? 1.0m
	? 1.0e+100
	? 1.0e-100
	? .001e+100 
	? 1.
	? 1>2.and.3<4
	? 1>2.or.3<4    
	? 1.e10
RETURN
