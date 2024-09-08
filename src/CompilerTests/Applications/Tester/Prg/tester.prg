FUNCTION Start( ) AS VOID
	TestClass{}:TestProp[TRUE,"b",1] := 123
	TestClass{}:TestAssign[TRUE,"b",1] := 123 // No overload for method 'TestClass.TestAssign[string, int]' takes 3 arguments
	//TestClass{}:TestPropEmpty[TRUE,"b",1] := 123
RETURN

CLASS TestClass

	//PROPERTY TestPropCrash[c1 AS LOGIC , c2 AS STRING, nValue AS INT] AS INT SET // compiler crash (if other errors are resolved)
	//PROPERTY TestPropEmpty[c1 AS LOGIC , c2 AS STRING, nValue AS INT] AS INT SET NOP // error XS0103: The name 'NOP' does not exist in the current context

	PROPERTY TestProp[c1 AS LOGIC , c2 AS STRING, nValue AS INT] AS INT
	SET
		? c1,c2,nValue, value
	END SET
	END PROPERTY

	ASSIGN TestAssign(iValue as INT, c1 AS LOGIC , c2 AS STRING, nValue AS INT) AS VOID
		? c1,c2,nValue, iValue
	RETURN


END CLASS

