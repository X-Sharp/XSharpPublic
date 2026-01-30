// 953. Incompatibility with X# 2 when using INT <> ENUM in SWITCH statements
// https://github.com/X-Sharp/XSharpPublic/issues/1788

ENUM TestEnum AS INT
	MEMBER m1 := 1
	MEMBER m2 := 2
END ENUM

FUNCTION Start() AS VOID
	LOCAL nOrdinal AS INT
	nOrdinal := TestEnum.m2 // OK
	
	IF nOrdinal == TestEnum.m2 // OK
		? "test"
	END IF
	xAssert(nOrdinal == TestEnum.m2)
	xAssert(.not. (nOrdinal != TestEnum.m2))
	
	SWITCH nOrdinal
	CASE TestEnum.m1
		? TestEnum.m1:ToString() // error XS19060: Cannot use a numeric constant or relational pattern on 'int' because it inherits from or extends 'INumberBase<T>'. Consider using a type pattern to narrow to a specifc numeric type.
		xAssert(false)
	CASE TestEnum.m2
		xAssert(true)
		? TestEnum.m2:ToString() // error XS19060
	OTHERWISE
		xAssert(false)
	END SWITCH

	LOCAL dwOrdinal AS DWORD
	dwOrdinal := TestEnum.m1

	SWITCH dwOrdinal
	CASE TestEnum.m1
		xAssert(true)
		? TestEnum.m1:ToString()
	CASE TestEnum.m2
		xAssert(false)
		? TestEnum.m2:ToString()
	OTHERWISE
		xAssert(false)
	END SWITCH

	
PROC xAssert(l AS LOGIC) 
	IF .NOT. l
		THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
	END IF
	? "Assertion passed"   
RETURN
