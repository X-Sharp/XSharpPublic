// 330. error XS0266: Cannot implicitly convert type 'int' to 'byte'. An explicit conversion exists (are you missing a cast?)
// vulcan compiles this without /vo4
// I promise, this is the last related one! :-)
FUNCTION Start() AS VOID
LOCAL b := 0 AS BYTE
LOCAL w := 0 AS WORD
LOCAL s := 0 AS SHORT
b := b + 1
w := w + 1
w := w + b
s := s + 1
? b , w , s

