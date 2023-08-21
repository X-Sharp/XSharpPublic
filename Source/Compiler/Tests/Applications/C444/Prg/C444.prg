// 444. error XS0133: The expression being assigned to 'Functions.mydefine' must be constant
// with /vo14+
DEFINE mydefine := 123.456
DEFINE REAL4_MAX     := 3.402823466e+38          // Maximum representable number
FUNCTION Start() AS VOID
? mydefine
? mydefine == 123.456
IF mydefine != 123.456
	THROW Exception{"Inocorrect result"}
END IF
RETURN
