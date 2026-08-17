// 970. Problems with the /fox3 option - full test
// https://github.com/X-Sharp/XSharpPublic/issues/2025

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)
CLASS Customers
	STATIC EXPORT id := 123 AS USUAL
	STATIC EXPORT typedId := 10 AS INT

	STATIC PROTECT StaticProtect AS INT
	STATIC PROTECT StaticPrivate AS INT
	
	EXPORT InstanceExport AS INT
	PROTECT InstanceProtect AS INT
	PRIVATE InstancePrivate AS INT
	
	STATIC PROPERTY StaticProperty AS INT AUTO
	STATIC PROPERTY StaticProtectedProperty AS INT AUTO
	PROPERTY InstanceProperty AS INT AUTO
	PROPERTY OnlyGetProperty AS INT GET 100
	
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

		xAssert( System.Int32.:MaxValue == 2147483647 )
		xAssert( System.Int32.MaxValue == 2147483647 )
		xAssert( Int32.:MaxValue == 2147483647 )
		xAssert( Int32.MaxValue == 2147483647 )
		xAssert( String.:Empty == "" )
		xAssert( String.Empty == "" )
		xAssert( String.Empty.Length == 0 )
		xAssert( String.IsNullOrEmpty( String.Empty ) == true )
		
		Customers.StaticProtect := 200
		xAssert( Customers.StaticProtect == 200 )
		Customers.StaticPrivate := 300
		xAssert( Customers.StaticPrivate == 300 )

		Customers.StaticProperty := 1000
		xAssert( Customers.StaticProperty == 1000 )
		Customers.StaticProtectedProperty := 2000
		xAssert( Customers.StaticProtectedProperty == 2000 )

		LOCAL localthis := Customers{} AS Customers
		localthis.InstanceExport := 123
		xAssert( localthis.InstanceExport == 123 )
		localthis.InstanceProtect := 321
		xAssert( localthis.InstanceProtect == 321 )
		localthis.InstancePrivate := 333
		xAssert( localthis.InstancePrivate == 333 )
		localthis.InstanceProperty := 500
		xAssert( localthis.InstanceProperty == 500 )
		xAssert( localthis.OnlyGetProperty == 100 )

		LOCAL localusual := Customers{} AS USUAL
		localusual.InstanceExport := 123
		xAssert( localusual.InstanceExport == 123 )
		localusual.InstanceProtect := 321
		xAssert( localusual.InstanceProtect == 321 )
		localusual.InstancePrivate := 333
		xAssert( localusual.InstancePrivate == 333 )
		localusual.InstanceProperty := 500
		xAssert( localusual.InstanceProperty == 500 )
		xAssert( localusual.OnlyGetProperty == 100 )
	
	METHOD InstanceMethod() AS VOID

		id := 456
		? id
		xAssert( id == 456 )

		xAssert( Customers.id == 456 )

		Customers.id := 1000 // System.ArgumentException: Object of type 'System.Int32' cannot be converted to type 'XSharp.__Usual'.
		? Customers.id
		
		xAssert( Customers.id == 1000 )
		
		typedId := 50

		xAssert( typedId == 50 )

		Customers.typedId := 60
		? typedId

		xAssert( typedId == 60 )

		xAssert( System.Int32.:MaxValue == 2147483647 )
		xAssert( System.Int32.MaxValue == 2147483647 )
		xAssert( Int32.:MaxValue == 2147483647 )
		xAssert( Int32.MaxValue == 2147483647 )
		xAssert( String.:Empty == "" )
		xAssert( String.Empty == "" )
		xAssert( String.Empty.Length == 0 )
		xAssert( String.IsNullOrEmpty( String.Empty ) == true )

		Customers.StaticProtect := 200
		xAssert( Customers.StaticProtect == 200 )
		Customers.StaticPrivate := 300
		xAssert( Customers.StaticPrivate == 300 )
		
		this.InstanceExport := 123
		xAssert( this.InstanceExport == 123 )
		this.InstanceProtect := 321
		xAssert( this.InstanceProtect == 321 )
		this.InstancePrivate := 333
		xAssert( this.InstancePrivate == 333 )
		this.InstanceProperty := 500
		xAssert( this.InstanceProperty == 500 )
		xAssert( this.OnlyGetProperty == 100 )

		Customers.StaticProperty := 1000
		xAssert( Customers.StaticProperty == 1000 )
		Customers.StaticProtectedProperty := 2000
		xAssert( Customers.StaticProtectedProperty == 2000 )

		LOCAL localthis := Customers{} AS Customers
		localthis.InstanceExport := 123
		xAssert( localthis.InstanceExport == 123 )
		localthis.InstanceProtect := 321
		xAssert( localthis.InstanceProtect == 321 )
		localthis.InstancePrivate := 333
		xAssert( localthis.InstancePrivate == 333 )
		localthis.InstanceProperty := 500
		xAssert( localthis.InstanceProperty == 500 )
		xAssert( localthis.OnlyGetProperty == 100 )

		LOCAL localusual := Customers{} AS USUAL
		localusual.InstanceExport := 123
		xAssert( localusual.InstanceExport == 123 )
		localusual.InstanceProtect := 321
		xAssert( localusual.InstanceProtect == 321 )
		localusual.InstancePrivate := 333
		xAssert( localusual.InstancePrivate == 333 )
		localusual.InstanceProperty := 500
		xAssert( localusual.InstanceProperty == 500 )
		xAssert( localusual.OnlyGetProperty == 100 )


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
	
	VAR oCustomers := Customers{}
	oCustomers:InstanceMethod()
	


PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

