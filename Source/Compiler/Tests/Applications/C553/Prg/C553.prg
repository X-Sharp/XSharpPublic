// 553. error XS1686: Local 'u' or its members cannot have their address taken and be used inside an anonymous method or lambda expression
/*
Real problem is related to "u" is passed by reference to a CLIPPER function, which is not supported yet.
The error message is a little confusing here.
*/
FUNCTION Start() AS VOID
	LOCAL cb AS CODEBLOCK
	LOCAL u := Today()
	cb := {|o| Test(@u)}
	Eval(cb)
	? u
	IF u != 123
		// to be tested when we create the x# runtime which will support this
		THROW Exception{"Incorrect result"}
	END IF
RETURN

// u REF USUAL would also do the trick
FUNCTION Test(u) AS INT
	u := 123
RETURN 0
