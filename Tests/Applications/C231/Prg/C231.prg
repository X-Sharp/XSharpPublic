// 231. error XS0266: Cannot implicitly convert type 'void*' to '_myVOSTRUCT*'. An explicit conversion exists 
// needs /vo7
VOSTRUCT _myVOSTRUCT
MEMBER n AS INT

FUNCTION Start() AS VOID
LOCAL p AS _myVOSTRUCT
p := MemAlloc(SizeOf(_myVOSTRUCT))
? p
