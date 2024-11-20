// 917b. Some issues with /allowdot+
// https://github.com/X-Sharp/XSharpPublic/issues/1515

// core dialect, /allowdot+ (enabled)
#pragma options (allowdot, on)
FUNCTION Start( ) AS VOID
RETURN

ENUM MyEnum 
	MEMBER m1
	MEMBER m2
END ENUM

CLASS FirstClass
	PROPERTY MyEnum AS MyEnum AUTO

	STATIC METHOD Test() AS VOID
		LOCAL e := MyEnum.m1 AS MyEnum // OK
		IF e == MyEnum.m1 // OK
			NOP
		END IF

	METHOD InstanceTest() AS VOID
		LOCAL e := MyEnum.m1 AS MyEnum // no error
		IF e == MyEnum.m1 // no error
			NOP
		END IF

END CLASS


CLASS ContainerClass
	EXPORT myEnum AS INT
	EXPORT AnotherTest AS AnotherTest

	CLASS NestedClass
		METHOD Test() AS VOID
			MyEnum.m1:ToString() // error XS0120: An object reference is required for the non-static field, method, or property 'ContainerClass.myEnum'
			LOCAL e := MyEnum.m1 AS MyEnum // error XS0120: An object reference is required for the non-static field, method, or property 'ContainerClass.myEnum'
			IF e == MyEnum.m1 // error XS0120: An object reference is required for the non-static field, method, or property 'ContainerClass.myEnum'
				NOP
			END IF
			
			AnotherTest.StaticMethod()
	END CLASS
	
END CLASS

CLASS AnotherTest
	STATIC METHOD StaticMethod() AS VOID
END CLASS




CLASS ThirdTest
	PROPERTY System AS LOGIC AUTO
		
	CONSTRUCTOR()
		System.Diagnostics.Debug.WriteLine( "Hi" ) // error XS1061: 'logic' does not contain a definition for 'Diagnostics' 
	METHOD Test1() AS VOID
		System.Diagnostics.Debug.WriteLine( "Hi" ) // error XS1061: 'logic' does not contain a definition for 'Diagnostics' 
	RETURN
	STATIC METHOD Test2() AS VOID
		System.Diagnostics.Debug.WriteLine( "Hi" ) // An object reference is required for the non-static field, method, or property 'ThirdTest.System'
	RETURN
END CLASS

