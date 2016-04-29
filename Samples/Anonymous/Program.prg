DELEGATE Multiply(x AS REAL8) AS REAL8
FUNCTION Start AS VOID
	LOCAL del AS Multiply
	del := {|e| e * e}
	? del(1)
	? del(2)
	? del(3)
	? del(4)
	?
	Console.ReadLine()
RETURN
