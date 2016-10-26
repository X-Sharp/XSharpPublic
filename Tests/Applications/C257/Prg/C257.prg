// 257. error XS7036: There is no argument given that corresponds to the required formal parameter 'n' of 'Functions.Test(int)'
FUNCTION Test(n AS INT) AS VOID
FUNCTION Start() AS VOID
LOCAL p AS PTR
p := @Test()

