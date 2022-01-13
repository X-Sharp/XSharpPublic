// https://github.com/X-Sharp/XSharpPublic/issues/805
FUNCTION Start() AS VOID
PUBLIC a[3]
PUBLIC B[42]
a(1) = 42
? Type ( "a(1)" )
? Type ( "a[1]" )
b := Today()
? Type ( "b(42)" )
MemVarRelease("B")
b := Today()
? Type ( "b(42)" )
wait
RETURN


function b (num as int) as string
    return num:ToString()
