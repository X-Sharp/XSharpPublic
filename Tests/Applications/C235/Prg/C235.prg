// 235. Unassigned PSZ is not NULL_PSZ
FUNCTION Start() AS VOID
LOCAL p AS PSZ
IF p != NULL_PSZ
	THROW Exception{"Unassigned PSZ != NULL_PSZ"}
ENDIF

