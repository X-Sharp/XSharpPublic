// 294. error XS0127: Since 'TestClass.Xs$Assign$TestAssign(int)' returns void, a return keyword must not be followed by an object expression
// vulcan reports warning property ASSIGN / SET methods do not return a value; return value ignored
#pragma warnings(9026, off) // missing return value
#pragma warnings(9032, off) // return value
#pragma warnings(165, off) // uniassigned local
FUNCTION Start() AS VOID
LOCAL o AS TestClass
o := TestClass{}
o:TestAssign := 123
? o:n

CLASS TestClass
	EXPORT n AS INT
	ACCESS TestAccess AS INT
		RETURN
	ASSIGN TestAssign(v AS INT)
	 RETURN SELF:n := v
	PROPERTY Test AS INT
		GET
			RETURN
		END GET
		SET
			RETURN n := VALUE
		END SET
	END PROPERTY
	EVENT TestEvent AS EventHandler
	ADD
		RETURN n := 10
	END ADD
	REMOVE
		RETURN n := 20
	END REMOVE
	END EVENT
END CLASS

PROCEDURE testme
	LOCAL n AS LONG
RETURN n := n * 2
