// compiler crash with USUAL += operator
FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL i AS INT
LOCAL f AS FLOAT

i := 2
f := 5.0

u := 100.0
u += (i * f)
? u
xAssert(u == 110.0)

u := 100.0
u -= (i * f)
? u
xAssert(u == 90.0)

u := 100.0
u -= f * i
? u
xAssert(u == 90.0)


u := 100.0
u /= (i * f)
? u
xAssert(u == 10.0)

u := 100.0
u *= (i * f)
? u
xAssert(u == 1000.0)

u := 100.0
u *= f * i
? u
xAssert(u == 1000.0)
RETURN

PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

