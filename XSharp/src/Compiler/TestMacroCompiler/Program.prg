USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text


FUNCTION Start() AS VOID
	LOCAL cb1 AS USUAL
	LOCAL i AS INT      
	LOCAL nSecs AS FLOAT
    SetMacroCompiler(typeof(XSharp.MacroCompiler))
    cb1 := MCompile("{|| 1+2 }")
    ? Time()         
    nSecs := Seconds()
	FOR i := 1 TO 1
	    cb1 := MCompile("{|| 1+ 2 }")
	    cb1 := MCompile("{||'Hello world'}")
	    cb1 := MCompile("{||ToDay()}")
	    cb1 := MCompile("{||MyToDay()}")
        cb1 := MCompile("{||_FIELD->LASTNAME + _FIELD->FIRSTNAME}")
	NEXT 
	? Seconds() - nSecs
    ? Time()
	cb1 := &("{|| 1+2 }")
    ? Eval(cb1)
    nSecs := Seconds()
	FOR i := 1 TO 50000 
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
	
FUNCTION MyToday() AS USUAL STRICT
RETURN Today()
