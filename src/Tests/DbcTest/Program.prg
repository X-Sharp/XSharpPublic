GLOBAL ggg := 123

FUNCTION Start( ) AS VOID
PUBLIC ppp
ppp := 123.456
	? ppp

// Method is overloaded, Cannot determine the right overload to call.
//     ? Str(ggg)
//     ? Str(ppp)
//     ? Str(123)
//  	? &("Str(ggg)")
//  	? &("Str(ppp)")
//  	? &("Str(123)")

// System.Security.VerificationException
// Operation could destabilize the runtime.
	? &("Str(123,7,2)")
	? &("Str(ggg,7,2)")
    ? &("Str(ppp,7,2)")
    wait
RETURN
