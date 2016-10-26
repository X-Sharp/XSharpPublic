// 187. compiler crash with FLOAT operations
// without enabling /vo14
FUNCTION Start() AS VOID
LOCAL f AS FLOAT
f := 1.0
f := f + 1.5
? f
