// 231. error XS0266: Cannot implicitly convert type 'void*' to '_myVOSTRUCT*'. An explicit conversion exists 
// /vo4+ /vo7+
VOSTRUCT _myVOSTRUCT
MEMBER n AS INT
MEMBER m AS DWORD

FUNCTION Start() AS VOID
LOCAL u AS USUAL
LOCAL pas AS _myVOSTRUCT
pas := MemAlloc(SizeOf(_myVOSTRUCT))
pas:m := 111
? pas:m
? pas.m
? pas
u := pas
? u

LOCAL pis IS _myVOSTRUCT
? pis
pis.n := 123
? pis.n
? pis:n
u := pis
? u

