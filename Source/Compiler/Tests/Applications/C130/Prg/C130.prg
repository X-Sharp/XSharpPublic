// 130. Assertion "passing args byref should not clone them into temps"
// also it compiles without errors, even though 3rd argument must be passed by reference
FUNCTION Start() AS VOID
? Math.DivRem( 1, 2, 3 )

