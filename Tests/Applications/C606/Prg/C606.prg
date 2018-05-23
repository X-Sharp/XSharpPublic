// 606. Some issues with PROPERTY GET/SET visibility modifiers
// Ticket TECH-H6Z3688EQ4 by Stefan Hirsch
// I really think we need somehow easier to read ticket numbers :-)
FUNCTION Start( ) AS VOID
	LOCAL o AS TestClass
	o := TestClass{}
	
	xAssert(o:MyProp1 == 0)
	//xAssert(o:MyProp7 == 7)
	xAssert(o:MyProp8 == 8)
	xAssert(o:MyProp9 == 9)
	xAssert(o:MyProp10 == 10)
RETURN

CLASS TestClass
	// The following compile without errors/warnings:
	PROPERTY MyProp1 AS INT AUTO
	PROPERTY MyProp2 AS INT AUTO GET SET
	PROPERTY MyProp3 AS INT AUTO GET PRIVATE SET

	// The following report:
	// error XS0106: The modifier 'override' is not valid for this item
	// error XS0273: The accessibility modifier of the 'TestClass.MyProp3.set' accessor must be more restrictive than the property or indexer 'TestClass.MyProp3'
	PROPERTY MyProp4 AS INT AUTO GET PROTECTED SET 	// error XS0106
	PROTECTED PROPERTY MyProp5 AS INT AUTO GET PUBLIC SET 	// error XS0106, XS0273
	PROPERTY MyProp6 AS INT AUTO GET INTERNAL SET 	// error XS0106

	// The following report error XS0106, but also a warning:
	// warning XS9047: A get or set accessor must have a body, an auto property will be generated
	// The GET accessor does have a body, so this warning is wrong, right?
	PROPERTY MyProp7 AS INT PROTECTED GET 7 SET 	// error XS0106
	PROPERTY MyProp8 AS INT INTERNAL GET 8 SET 		// error XS0106
	PROPERTY MyProp9 AS INT GET 9 INTERNAL SET 	// error XS0106
	PROPERTY MyProp10 AS INT GET 10 PROTECTED SET 	// error XS0106

END CLASS


INTERFACE TestInterface
	PROPERTY MyProp1 AS INT GET // SHould not generate a warning about a missing body
END INTERFACE

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

