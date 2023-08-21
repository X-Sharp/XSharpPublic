// 235. Unassigned PSZ is not NULL_PSZ
#pragma warnings(165, off) // unassigned
FUNCTION Start() AS VOID
LOCAL p AS PSZ
IF p != NULL_PSZ
	THROW Exception{"Unassigned PSZ != NULL_PSZ"}
ENDIF

