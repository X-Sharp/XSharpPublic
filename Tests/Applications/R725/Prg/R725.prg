FUNCTION Start as VOID
LOCAL o AS OBJECT
LOCAL p AS PSZ
o := StringAlloc("abc")      
p := o             
? p
XAssert(p == String2Psz("abc"))
Console.ReadLine()
RETURN

PROC xAssert(l AS LOGIC)
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN	
