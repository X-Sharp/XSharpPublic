// 919. Problems with /vo9 in the FoxPro dialect

// FoxPro dialect
#pragma options("vo9", on)
FUNCTION Start( ) AS VOID
RETURN

CLASS TestClass
	ACCESS TestAccess1 AS INT
	RETURN // no warning. Shouldn't there be one for the missing return value?
	ACCESS TestAccess2 AS INT
//	RETURN // warning XS9025: Missing RETURN statement  (OK)

	METHOD TestMethod1() AS INT
	RETURN // no warning. Shouldn't there be one for the missing return value?
	METHOD TestMethod2() AS INT
//	RETURN // warning XS9025: Missing RETURN statement  (OK)

	PROPERTY TestProperty1 AS INT
		GET
			RETURN // error XS0029: Cannot implicitly convert type 'logic' to 'int'
		END GET
	END PROPERTY 
	PROPERTY TestProperty2 AS INT
		GET
//			RETURN // error XS0161: 'TestClass.TestProperty2.get': not all code paths return a value
		END GET
	END PROPERTY 
END CLASS
