// 229. error XS0118: '_myVOSTRUCT [    ]' is a element but is used like a VOSTRUCT/UNION
VOSTRUCT _myVOSTRUCT
MEMBER n AS INT
FUNCTION Start() AS VOID
LOCAL DIM test[10] IS _myVOSTRUCT

