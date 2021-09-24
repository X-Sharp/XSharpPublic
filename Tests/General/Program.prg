FUNCTION Start AS VOID

? Testme()
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




FUNCTION TestMe(val := NULL AS USUAL) AS LOGIC
? val, ValType(val), UsualType(val)
RETURN val = NIL
