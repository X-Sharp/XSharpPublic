CLASS TestClass
	METHOD Seek(a AS INT) AS INT
		? "a"
	RETURN a
	METHOD Seek(a AS INT, b AS INT) AS INT
		? "b"
	RETURN a+b
	METHOD Seek(a AS INT, b AS INT, c AS INT) AS INT
		? "c"
	RETURN a+b+c
END CLASS

FUNCTION Start() AS VOID
	LOCAL u := TestClass{} AS USUAL
	? u:Seek(1)
	? u:Seek(1,2)
    ? u:Seek(1,2,3)
    wait
