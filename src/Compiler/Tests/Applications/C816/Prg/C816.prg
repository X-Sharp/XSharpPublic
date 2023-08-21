// 816. Various issues with the @ operator #2
// this code must report two compiler errrors AS shown below

FUNCTION Start( ) AS VOID
	NoRunTest9()
RETURN

PROCEDURE NoRunTest9()
	LOCAL cTest := "passed" AS STRING
    	Test( "abc", @cTest ) // this must report an error, because it does not pass the var by reference as intended
    	? cTest

FUNCTION Test( cDesc AS STRING, cString ) AS VOID STRICT
cString := "modifed"






