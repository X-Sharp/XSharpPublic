// 243. error XS0266: Cannot implicitly convert type 'char' to 'byte'. An explicit conversion exists (are you missing a cast?)
// compile with /vo4+
FUNCTION Start() AS VOID
LOCAL c AS STRING
LOCAL b AS BYTE
c := "abc"
b := c[1]

