// 921. Problems with accessing VOSTRUCT members with the dot operator

#pragma options (allowdot, off)
VOSTRUCT _VOSTRUCT
MEMBER n AS INT
MEMBER m AS INT

VOSTRUCT _BIGSTRUCT
MEMBER x AS INT
MEMBER v IS _VOSTRUCT

GLOBAL gis IS _VOSTRUCT
GLOBAL gas AS _VOSTRUCT
FUNCTION Start() AS VOID
LOCAL vas AS _VOSTRUCT
LOCAL vis IS _VOSTRUCT

vas := (_VOSTRUCT PTR)MemAlloc(SizeOf(_VOSTRUCT))

vis:n := 123
? vis:n
xAssert(vis:n == 123)
vis.n := 456
? vis.n
xAssert(vis.n == 456)

vas:n := 123
? vas:n
xAssert(vas:n == 123)
vas.n := 456
? vas.n
xAssert(vas.n == 456)


gis:m := 2 // error XS0246: The type or namespace name 'gis' could not be found
gis.n := 3
? gis.m * gis:n // error XS0246: The type or namespace name 'gis' could not be found
xAssert(gis.m * gis:n == 6)

gas := @gis
gas:m := 2 // error XS0246: The type or namespace name 'gis' could not be found
gas.n := 3
? gas.m * gas:n // error XS0246: The type or namespace name 'gis' could not be found
xAssert(gas.m * gas:n == 6)


LOCAL IMPLIED o := TestClass{}
o:cis:m := 123
o:cis.n := 456
? o:cis.m + o:cis:n
xAssert(o:cis.m + o:cis:n == 579)

LOCAL vBig IS _BIGSTRUCT
vBig.v.n := 5
vBig.v.m := 15
? vBig.v.n + vBig.v.m
xAssert(vBig.v.n + vBig.v.m == 20)
xAssert(vBig:v.n + vBig.v:m == 20)
xAssert(vBig.v:n + vBig:v:m == 20)

CLASS TestClass
	EXPORT cis IS _VOSTRUCT
END CLASS


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	? "Assertion FAILED"
	THROW Exception{"Incorrect result"}
END IF
RETURN
