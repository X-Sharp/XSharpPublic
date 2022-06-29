// 392. error XS1061: '__Array' does not contain a definition for 'CallMethodOnArray1' and no extension method 'CallMethodOnArray1' accepting a first argument of type '__Array' could be found (are you missing a using directive or an assembly reference?)
#pragma warnings(9087, off) // call converted to ASend

GLOBAL nCalled := 0 AS INT

FUNCTION Start() AS VOID
LOCAL aTest,aResult AS ARRAY
aTest := {TestClass{} , TestClass{}}

// vulcan translates this to a call to ASend(aTest , "CallMethodOnArray1", <USUAL>{1,2})
aResult := aTest:CallMethodOnArray1(1,2)
aResult := aTest:CallMethodOnArray2(5,10)

? aResult[1],aResult[2]
IF ALen(aResult) != 2
	THROW Exception{"Incorrect result length"}
END IF
IF nCalled != 4
	THROW Exception{"Methods not called correctly"}
END IF
RETURN

CLASS TestClass
	METHOD CallMethodOnArray1(a,b)
		nCalled ++
		? a,b
		IF a + b != 3
			THROW Exception{"Incorrect parameters"}
		END IF
	RETURN NIL
	METHOD CallMethodOnArray2(a AS INT,b AS INT) AS VOID
		nCalled ++
		? a,b
		IF a + b != 15
			THROW Exception{"Incorrect parameters"}
		END IF
	RETURN
END CLASS
