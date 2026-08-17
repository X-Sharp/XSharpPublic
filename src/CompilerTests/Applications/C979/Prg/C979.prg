// 979. Problems with the /fox3 option - instance vars #2055
// https://github.com/X-Sharp/XSharpPublic/issues/2055

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

CLASS Customers
	STATIC EXPORT id := 123 AS USUAL
	STATIC EXPORT typedId := 10 AS INT
	STATIC PROPERTY StaticProperty AS INT AUTO
		
	METHOD InstanceMethod() AS VOID
		Customers.id := 456 // no exported variable
		xAssert( Customers.id == 456 )

		Customers.typedId := 60 // no exported variable
		xAssert( Customers.typedId == 60 )

		xAssert( Int32.MaxValue == 2147483647 )
		xAssert( String.Empty == "" )
		xAssert( String.IsNullOrEmpty( String.Empty ) == TRUE )

		Customers.StaticProperty := 1000
		xAssert( Customers.StaticProperty == 1000 )
END CLASS

FUNCTION Start( ) AS VOID
	VAR oCustomers := Customers{}
	oCustomers:InstanceMethod()

PROC xAssert(l AS LOGIC) AS VOID
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

