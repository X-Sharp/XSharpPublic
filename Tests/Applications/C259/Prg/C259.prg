// 259. error XS0211: Cannot take the address of the given expression
VOSTRUCT _MyVOSTRUCT
MEMBER DIM abDim [10] AS BYTE

FUNCTION Start() AS VOID
LOCAL gvostr IS _MyVOSTRUCT
LOCAL p AS PTR
p := @gvostr:abDim
? p

