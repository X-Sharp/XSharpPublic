// 257. error XS7036: There is no argument given that corresponds to the required formal parameter 'n' of 'Functions.Test(int)'
FUNCTION Test(n AS INT) AS VOID
FUNCTION Start() AS VOID
LOCAL p AS PTR
// both should report an error message suggesting to use a delegate for callbacks
p := @Test()
p := @Test(1)

