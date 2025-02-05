// 934. Problem calling error handler from BEGIN...END SEQUENCE
// https://github.com/X-Sharp/XSharpPublic/issues/1673

USING System.Collections.Generic
GLOBAL gaMessages := List<STRING>{} AS List<STRING>

FUNCTION Start()  AS INT
	LOCAL oCB AS CODEBLOCK
	LOCAL err AS USUAL

	oCB := ErrorBlock( {|err| ErrorHandler(err) } )
	PrintMessage( "before BEGIN SEQUENCE", CanBreak() )
	BEGIN SEQUENCE
		PrintMessage( "in BEGIN SEQUENCE", CanBreak() )
		Error{}:Throw()
	RECOVER USING err      
		PrintMessage( "in RECOVER", CanBreak() )
	END SEQUENCE
	ErrorBlock( oCB )
	PrintMessage( "after END SEQUENCE", CanBreak() )

	?

	xAssert(gaMessages:Count == 4)
	xAssert(gaMessages[0] == "before BEGIN SEQUENCE .F.")
	xAssert(gaMessages[1] == "in BEGIN SEQUENCE .T.")
	xAssert(gaMessages[2] == "in ErrorHandler .T.")
	xAssert(gaMessages[3] == "in RECOVER .F.")
	xAssert(gaMessages[4] == "after END SEQUENCE .F.")
	
RETURN 0
	
FUNCTION ErrorHandler(err)
	PrintMessage( "in ErrorHandler", CanBreak() )
	_Break(err)
	PrintMessage( "in ErrorHandler after _break", CanBreak() )
RETURN NIL

PROCEDURE PrintMessage(cMessage AS STRING, lCanBreak AS LOGIC)
	? cMessage, lCanBreak
	gaMessages:Add(cMessage + " " + AsString(lCanBreak))

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF
RETURN
