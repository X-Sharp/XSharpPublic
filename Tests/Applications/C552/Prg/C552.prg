// 552. Incorrect PSZ pointer arithmetic
/*
Same problem exists also in vulcan. The compiler(s) use the __Psz:Item[] indexer
for obtaining the value and the indexer is zero based. So the index must be subtracted by one.
The compiler was changed to fix this. The result in the Vulcan dialect is now different from the result
in the VO dialect
*/
FUNCTION Start() AS VOID

LOCAL p AS PSZ
LOCAL i AS DWORD

p := String2Psz("123")
FOR i := 1 UPTO 3
    ? p[i]
NEXT

xAssert(p[1] == 49)
xAssert(p[3] == 51)
i := 1
xAssert(p[i] == 49)
i := 3
xAssert(p[i] == 51)

RETURN

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result"}
END IF

