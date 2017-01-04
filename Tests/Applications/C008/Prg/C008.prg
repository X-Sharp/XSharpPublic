// 8. Incorrect decimal literals
FUNCTION Start() AS VOID
LOCAL r AS REAL8
r := 12.34 // 1234 is assigned
? r
IF r:ToString():Replace(',' , '.') != "12.34"
	THROW Exception{"Incorrect REAL8 value"}
END IF
