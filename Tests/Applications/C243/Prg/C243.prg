// 243. error XS0266: Cannot implicitly convert type 'char' to 'byte'. An explicit conversion exists (are you missing a cast?)
// compile with /vo4+
// (vulcan compiles this only with /vo4+)
FUNCTION Start() AS VOID
LOCAL c AS STRING
LOCAL b AS BYTE
LOCAL ch := 'a' AS Char                    
c := "abc"
b := c[1]	// requires /vo4
? b
b := ch     // requires /vo4
? b
ch := b
? ch
