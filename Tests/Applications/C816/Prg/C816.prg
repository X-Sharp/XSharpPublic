// 817. Various issues with the @ operator #2
// this code must report two compiler errrors AS shown below

FUNCTION Start( ) AS VOID
	NoRunTest9()
	NoRunTest10()
RETURN

PROCEDURE NoRunTest9()
	LOCAL cTest := "passed" AS STRING
	Test( "abc", @cTest ) // this must report an error, because it does not pass the var by reference as intended
	? cTest
	
FUNCTION Test( cDesc AS STRING, cString ) AS VOID STRICT
cString := "modifed"



PROCEDURE NoRunTest10()
LOCAL o AS OBJECT
o := TestClass{}
LOCAL rf := 123 AS INT
Send(o, "Test", 1, @rf) // this must also report an error, because it does not pass the var by reference as intended
? rf

CLASS TestClass
	METHOD Test(n AS INT, rf REF INT) AS VOID
		rf := 456
END CLASS

