// 919. Problem with /vo9 in the VO dialect

// VO dialect
#pragma options("vo9", on)
FUNCTION Start( ) AS VOID
RETURN

CLASS TestClass
	ACCESS TestAccess1 AS INT
	RETURN // warning XS9026: Missing RETURN value (OK)
	ACCESS TestAccess2 AS INT
//	RETURN // warning XS9025: Missing RETURN statement  (OK)

	METHOD TestMethod1() AS INT
	RETURN // warning XS9026: Missing RETURN value (OK)
	METHOD TestMethod2() AS INT
//	RETURN // warning XS9025: Missing RETURN statement  (OK)

	PROPERTY TestProperty1 AS INT
		GET
			RETURN // warning XS9026: Missing RETURN value (OK)
		END GET
	END PROPERTY 
	PROPERTY TestProperty2 AS INT
		GET
//			RETURN // error XS0161: 'TestClass.TestProperty2.get': not all code paths return a value
		END GET
	END PROPERTY 
END CLASS
