// 169. error XS0034: Operator '+=' is ambiguous on operands of type '__VODate' and 'ushort'
#pragma warnings(219, off)
FUNCTION Start() AS VOID
LOCAL d AS DATE
LOCAL f := 1.0 AS FLOAT
LOCAL r := 1.0 AS REAL8
LOCAL n := 1 AS INT
LOCAL w := 1 AS WORD
LOCAL dw := 1 AS DWORD
LOCAL b := 1 AS BYTE
d := DATE{DateTime.Now}
d += n
d := d + r
d -= w
d := d - dw
d := d + b
? d
IF d - DATE{DateTime.Now} != 1
	THROW Exception{"Wrong date calculated"}
ENDIF

