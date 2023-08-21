// 156. error XS0266: Cannot implicitly convert type 'void*' to 'System.IntPtr'. An explicit conversion exists (are you missing a cast?)
#pragma warnings(219, off) // assigned but never used
FUNCTION Start() AS VOID
LOCAL p AS IntPtr
p := FOpen("somefile")

