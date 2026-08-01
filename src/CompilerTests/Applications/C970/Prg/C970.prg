// 970. Problems with the /fox3 option - static vars
// https://github.com/X-Sharp/XSharpPublic/issues/2025

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
CLASS Customers
	STATIC EXPORT id := 123 AS USUAL
	STATIC EXPORT typedId := 10 AS INT
	STATIC METHOD test()
		id := 456
		? id

		xAssert( Customers.id == 456 )

		Customers.id := 1000 // System.ArgumentException: Object of type 'System.Int32' cannot be converted to type 'XSharp.__Usual'.
		? Customers.id
		
		xAssert( Customers.id == 1000 )
		
		typedId := 50

		xAssert( typedId == 50 )

		Customers.typedId := 60
		? typedId

		xAssert( typedId == 60 )

	STATIC CONSTRUCTOR()
		test()
END CLASS

FUNCTION Start( ) AS VOID
	? System.Int32.MaxValue  // OK
	? System.Int32.:MaxValue // OK

	xAssert( System.Int32.:MaxValue == 2147483647 )
	xAssert( System.Int32.MaxValue == 2147483647 )

	? Int32.:MaxValue // OK
	? Int32.MaxValue // XSharp.Error: Variable does not exist: INT32

	xAssert( Int32.:MaxValue == 2147483647 )
	xAssert( Int32.MaxValue == 2147483647 )

	? String.:Empty // OK
	? String.Empty // XSharp.Error: Variable does not exist: STRING

	xAssert( String.:Empty == "" )
	xAssert( String.Empty == "" )

	? Customers.id  // OK
	xAssert( Customers.id == 1000 )

	Customers.id := 456 // System.ArgumentException: Object of type 'System.Int32' cannot be converted to type 'XSharp.__Usual'.
	LOCAL nId AS INT
	nId := Customers.id // OK
	? nId

	xAssert( nId == 456 )

	? Customers.typedId // OK
	Customers.typedId := 700 // OK
	? Customers.typedId // OK

	xAssert( Customers.typedId == 700 )

	Customers.test() // XSharp.Error: Exception of type 'XSharp.Error' was thrown.


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

