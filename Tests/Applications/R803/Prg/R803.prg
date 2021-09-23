// R803: When the OUT parameter to a function was not passed
// then the compiler was passing in a reference to __Usual._NIL 
// causing assignment to the out parameter to update the _NIL

FUNCTION Start AS VOID

? Testme(, OUT VAR lRef), lRef
? TestMe(1)
? testMe(Today())
? TestMe(1.1)
? testMe({1,2,3})
? testMe(Error{})
? TestMe("abc")
? testMe(TRUE)
? testMe(#symbol)
? testMe((INT64) 1234)
? testMe(DateTime())
? TestMe(1.234m)
? TestMe($12.34)
? testMe(0h1234)
WAIT
RETURN




FUNCTION TestMe(val := NULL AS USUAL, lRef := NULL OUT USUAL) AS LOGIC
//? val, ValType(val), UsualType(val)
lRef := TRUE
RETURN val = NIL

