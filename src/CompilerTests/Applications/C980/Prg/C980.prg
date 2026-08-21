// 980. Problems with the /fox3 option - protected/private vars #2056
// https://github.com/X-Sharp/XSharpPublic/issues/2056

#pragma options("fox3", enable)
#pragma options("allowdot", enable)
#pragma options("lb", enable)
#pragma options("memvar", enable)
#pragma options("undeclared", enable)

CLASS TestGlobalClass
	EXPORT n := 100 AS INT
	PROPERTY prop AS INT GET 500 SET SELF:n := value
END CLASS

GLOBAL GlobalString := "test" AS STRING
GLOBAL GlobalClass := TestGlobalClass{} AS TestGlobalClass

CLASS Customers
	STATIC PROTECT StaticProtect AS INT
	STATIC PROTECT StaticPrivate AS INT
	
	PROTECT InstanceProtect AS INT
	PRIVATE InstancePrivate AS INT
	
	STATIC PROPERTY StaticProperty AS INT AUTO
	STATIC PROPERTY StaticProtectedProperty AS INT AUTO
	PROPERTY InstanceProperty AS INT AUTO
	PROPERTY OnlyGetProperty AS INT GET 100
	
	STATIC METHOD StaticMethod() AS VOID
		Customers.StaticProtect := 200
		xAssert( Customers.StaticProtect == 200 )
		Customers.StaticPrivate := 300
		xAssert( Customers.StaticPrivate == 300 )

		LOCAL localthis := Customers{} AS Customers
		localthis.InstanceProtect := 321
		xAssert( localthis.InstanceProtect == 321 )

		localthis.InstancePrivate := 333
		xAssert( localthis.InstancePrivate == 333 )

		LOCAL localusual := Customers{} AS USUAL
		localusual.InstanceProtect := 321
		xAssert( localusual.InstanceProtect == 321 )
		localusual.InstancePrivate := 333
		xAssert( localusual.InstancePrivate == 333 )

		? GlobalString.Length
		? GlobalClass.n
		? GlobalClass.prop
		GlobalClass.prop := 777
		? GlobalClass.n
	
	METHOD InstanceMethod() AS VOID
		Customers.StaticProtect := 200
		xAssert( Customers.StaticProtect == 200 )
		Customers.StaticPrivate := 300
		xAssert( Customers.StaticPrivate == 300 )

		LOCAL localthis := Customers{} AS Customers
		localthis.InstanceProtect := 321
		xAssert( localthis.InstanceProtect == 321 )

		localthis.InstancePrivate := 333
		xAssert( localthis.InstancePrivate == 333 )

		LOCAL localusual := Customers{} AS USUAL
		localusual.InstanceProtect := 321
		xAssert( localusual.InstanceProtect == 321 )
		localusual.InstancePrivate := 333
		xAssert( localusual.InstancePrivate == 333 )

		? GlobalString.Length
		? GlobalClass.n
		? GlobalClass.prop
		GlobalClass.prop := 888
		? GlobalClass.n
	
END CLASS

FUNCTION Start( ) AS VOID
	? GlobalString:Length
	? GlobalString.Length
	? GlobalClass.n
	? GlobalClass.prop
	GlobalClass.prop := 999
	? GlobalClass.n
	
	Customers.StaticMethod()
	Customers{}:InstanceMethod()
	


PROC xAssert(l AS LOGIC) AS VOID
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"

