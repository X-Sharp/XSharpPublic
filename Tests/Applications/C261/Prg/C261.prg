// 261. error XS0118: '_myVOSTRUCT [    ]' is a element but is used like a VOSTRUCT/UNION
VOSTRUCT _myVOSTRUCT
MEMBER n AS INT

FUNCTION Start() AS VOID
LOCAL DIM dimStru[24] IS _myVOSTRUCT

