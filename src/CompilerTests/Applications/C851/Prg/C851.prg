// 851. VO Incompatibilites with Mod() function
// https://github.com/X-Sharp/XSharpPublic/issues/571
FUNCTION Start() AS VOID
LOCAL f AS FLOAT
LOCAL u AS USUAL

LOCAL rDelta AS REAL8
rDelta := SetFloatDelta(0.00001)

f := 1.5
u := 0.2
? Mod(f,u) // 0.10 in both VO and X#
? Mod(u,f) // 0.20 in VO, 0 in X# <- error!
xAssert( Mod(f,u) == 0.10)
xAssert( Mod(u,f) == 0.20)

? "-----"

f := 0.2
u := 1.5
? Mod(f,u) // 0.20 in both VO and X#
? Mod(u,f) // 0.10 in VO, divide by zero exception in X#
xAssert( Mod(f,u) == 0.20)
xAssert( Mod(u,f) == 0.10)

? "-----"

f := 0.3
u := 0.3
// All following return 0.2 in VO:
? Mod(2 , FLOAT(0.3)) // System.DivideByZeroException
? Mod(2 , REAL8(0.3)) // 0.2, OK
? Mod(2 , REAL4(0.3)) // 0.2, OK
? Mod(2 , f) // System.DivideByZeroException
? Mod(2 , u) // System.DivideByZeroExceptio

xAssert( Mod(2,FLOAT(0.3)) == 0.2)
xAssert( Mod(2,REAL8(0.3)) == 0.2)
xAssert( Mod(2,REAL4(0.3)) == 0.2)
xAssert( Mod(2,f) == 0.2)
xAssert( Mod(2,u) == 0.2)


u := 3
? Mod(u,2)            // 1 in VO and X#
? UsualType(Mod(u,2)) // 1 in VO and X#
xAssert( Mod(u,2) == 1 )
xAssert( UsualType( Mod(u,2) ) == 1 )

u := 3.0
? Mod(u,2)            // 1.00 in VO, 1 in X#
? UsualType(Mod(u,2)) // 3 in VO, 1 in X#
xAssert( Mod(u,2) == 1.00 )
xAssert( UsualType( Mod(u,2) ) == 3 )


SetFloatDelta(rDelta)

PROC xAssert(l AS LOGIC)
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
