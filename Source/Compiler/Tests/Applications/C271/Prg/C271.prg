// error XS0266: Cannot implicitly convert type 'object' to 'int'
// error XS0019: Operator '+' cannot be applied to operands of type 'int' and 'object'
// core, /vo10
FUNCTION Start() AS VOID
LOCAL n AS INT
n := 0
n := iif(n == 0 , 10 , 20)
? n + iif(n == 0 , 10 , 20)

