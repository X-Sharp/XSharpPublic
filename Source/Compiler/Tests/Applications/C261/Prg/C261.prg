// 261. error XS0118: '_myVOSTRUCT [    ]' is a element but is used like a VOSTRUCT/UNION
#pragma warnings(219, off)
VOSTRUCT _myVOSTRUCT
MEMBER n AS INT

FUNCTION Start() AS VOID
LOCAL DIM dimStru[24] AS _myVOSTRUCT

