USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID
	local cb1 as USUAL
	LOCAL i as int      
	local nSecs as float
    cb1 := MCompile("{|| 1+2 }")
    ? Time()         
    nSecs := Seconds()
	FOR i := 1 to 500
	    cb1 := MCompile("{|| 1+ 2 }")
	    cb1 := MCompile("{||'Hello world'}")
	    cb1 := MCompile("{||ToDay()}")
	    cb1 := MCompile("{||MyToDay()}")
	NEXT 
	? Seconds() - nSecs
    ? Time()
	cb1 := &("{|| 1+2 }")
    ? Eval(cb1)
    nSecs := Seconds()
	FOR i := 1 to 50000 
        Eval(cb1)
    NEXT
	? Seconds() - nSecs
    ? Time()
	cb1 := &("{||'Hello world'}")
    ? Eval(cb1)
	cb1 := &("{||ToDay()}")
    ? Eval(cb1)
	cb1 := &("{||MyToDay()}")
	? Eval(cb1)      
    wait
	
Function MyToday() as USUAL STRICT
RETURN Today()