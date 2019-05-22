// 675. System.NullReferenceException
// Wehn /vo2 is enabled, STRING LOCALs and fields get initialized to ""
// It would be more consistent to make also the internal field of AUTO STRING properties to initialize to "" as well
CLASS TestClass
	EXPORT cExport AS STRING
	PROPERTY cAutoProperty AS STRING AUTO
END CLASS

FUNCTION Start() AS VOID
	LOCAL cLocal AS STRING

	xAssertTrue(cLocal:Length == 0)

	LOCAL IMPLIED o := TestClass{}

	xAssertTrue(o:cExport:Length == 0)

	xAssertTrue(o:cAutoProperty:Length == 0) // System.NullReferenceException
RETURN

PROC xAssertTrue(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

