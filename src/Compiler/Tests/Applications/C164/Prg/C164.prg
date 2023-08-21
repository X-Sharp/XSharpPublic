// 164. Problem dereferencing pointer
FUNCTION Start() AS VOID
LOCAL p AS BYTE PTR
LOCAL b AS BYTE
b := 127
p := @b
? BYTE(p)
IF BYTE(p) != 127
	THROW Exception{"Wrong value"}
END IF

