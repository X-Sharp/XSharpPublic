// R733 - Problems boxing - unboxing usuals
FUNCTION Start( ) AS VOID
	local u as usual
	local o as Object
	u := 42                    
	o := u
	xAssert(o:GetType() == typeof(LONG))
	
	o := __CastClass(OBJECT, u) // this creates an object with a boxed usual
	xAssert(o:GetType() == typeof(USUAL))
	u := o 						// Should call the implicit converter
	xAssert(u == 42)
	u := (USUAL) o 				// This too
	xAssert(u == 42)
	u := __CastClass(USUAL, o)  // this should unbox the usual. 
	                            // Right now it goes through the usual constructor
	xAssert(u == 42)
RETURN


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "assertion passed"
	ELSE
		//? "Assertion failed"
		THROW Exception{"Incorrect result"}
	END IF   
RETURN
