// 84. error XS7038: Failed to emit module 'SmallXtest'.
// compile with /unsafe
FUNCTION Start() AS VOID
LOCAL h AS PTR
h := NULL_PTR
test(h)

PROC test( h AS PTR)

