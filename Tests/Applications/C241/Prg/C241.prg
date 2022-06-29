// 241. error XS0266: Cannot implicitly convert type 'void*' to 'byte*'. An explicit conversion exists (are you missing a cast?)
// This requires /vo7 also in Vulcan
#pragma warnings(219, off)
FUNCTION Start() AS VOID
LOCAL pResult AS BYTE PTR
pResult := MemAlloc(123)

