DELEGATE Multiply(x AS REAL8) AS REAL8
FUNCTION Start AS VOID
	LOCAL del AS Multiply
    // Lamda with untyped parameters
	del := {e =>  e * e}
	? del
	? del(1)
	? del(2)
    ? "Lamda with typed parameters"
 	del := {e as REAL8  =>  e * e * e}

	? del(3)
	? del(4)
    ? "Anonymous Method Expression"
    del := DELEGATE(e as REAL8) { 
                                e := e * e * e * e 
                                return e
                              }
    ? del(5)
	Console.ReadLine()
RETURN
