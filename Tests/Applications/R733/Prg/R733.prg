// R733 - Problems boxing - unboxing usuals
FUNCTION Start( ) AS VOID
	local u as usual
	local o as Object
	u := 42
	o := __CastClass(OBJECT, u) // this creates an object with a boxed usual
	u := __CastClass(USUAL, o)  // this should unbox the usual. 
	                            // Right now it goes through the usual constructor
	? u
RETURN
