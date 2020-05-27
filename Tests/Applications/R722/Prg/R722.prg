
FUNCTION Start( ) AS VOID
	LOCAL o as OBJECT
	LOCAL p as LONG PTR
	LOCAL l as LONG
	l := 0
	p := @l
	o := p
	p := o
	p[1] := 42
	? p == @l
	WAIT
RETURN
