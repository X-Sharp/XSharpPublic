// 233. error XS0030: Cannot convert type 'int' to 'bool'
// this is used A LOT in the SDK code
FUNCTION Start() AS VOID
LOCAL n := 0 AS INT
LOCAL d := 1 AS DWORD
? LOGIC(_CAST,n)
? LOGIC(_CAST,d)

