FUNCTION Start() AS VOID STRICT
	TestClass.DoSomething()
	RETURN

CLASS TestClass

	STATIC METHOD DoSomething() AS VOID STRICT

		LOCAL cTime AS STRING
		LOCAL nH, nM, nS AS INT

		cTime := Time()
		_ToHMS(cTime, OUT nH, OUT nM, OUT nS)

		LOCAL FUNCTION _ToHMS(cTimeString AS STRING, nH OUT INT, nM OUT INT, nS OUT INT) AS LOGIC PASCAL
			nH := Val(Left(cTimeString, 2))
			nM := Val(SubStr(cTimeString, 4, 2))
			nS := Val(SubStr(cTimeString, 7, 2))
			RETURN TRUE
		END FUNCTION

		RETURN

END CLASS
