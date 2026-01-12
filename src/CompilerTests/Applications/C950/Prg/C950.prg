// 950. Multiplication of numbers stored in a USUAL
// https://github.com/X-Sharp/XSharpPublic/issues/1764

FUNCTION Start( ) AS VOID

? 18 *596 *735 *875
xAssert(18 *596 *735 *875 == 6899445000)
xAssert(18U *596 *735 *875 == 6899445000)
xAssert(180 *596 *735 *875 == 68994450000)

LOCAL u AS USUAL

u := 18
u := u * 596 *735 *875
? u
xAssert(u == 6899445000)
u *= 10
xAssert(u == 68994450000)

LOCAL c AS STRING

c := "18U *596 *735 *875"
? Str(&(c) , 20 , 2)
xAssert(&(c) == 6899445000)

c := "18 *596 *735 *875"
? Str(&(c) , 20 , 2)
xAssert(&(c) == 6899445000)

xAssert(&("-2147483647 - 1") == -2147483648)
xAssert(&("-2147483647 - 2") == -2147483649)
xAssert(&("-2147483647 - 3") == -2147483650)
xAssert(&("-2147483648 - 2") == -2147483650)

u := Int32.MaxValue
xAssert( u * 2 == (INT64)Int32.MaxValue * 2)
xAssert( u - 2 == (INT64)Int32.MaxValue - 2)
xAssert( u + 2 == (INT64)Int32.MaxValue + 2)


PROC xAssert(l AS LOGIC) AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN

