// 258. error XS1666: You cannot use fixed size buffers contained in unfixed expressions. Try using the fixed statement.
#pragma warnings(219, off)
VOSTRUCT _MyVOSTRUCT
MEMBER DIM abDim [10] AS BYTE
GLOBAL gvostr IS _MyVOSTRUCT

FUNCTION Start() AS VOID
LOCAL p AS PTR
p := @gvostr:abDim

