// 97. error XS0266: Cannot implicitly convert type 'int' to 'System.IntPtr'. An explicit conversion exists (are you missing a cast?)
FUNCTION Start() AS VOID
LOCAL p AS IntPtr
p := 1
p := 1u
? p
