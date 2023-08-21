// 321. error XS0030: Cannot convert type 'int' to 'logic'
FUNCTION Start() AS VOID
LOCAL n := 1 AS INT
LOCAL d := 0 AS DWORD
LOCAL b := 255 AS BYTE
LOCAL s := 8 AS SHORT
LOCAL l AS LOGIC

l := LOGIC(_CAST, IntFunc())
? l
l := LOGIC(_CAST, DWORDFunc())
? l
l := LOGIC(_CAST, ByteFunc())
? l
l := LOGIC(_CAST, ShortFunc())
? l

// the following compile without errrors
?
l := LOGIC(_CAST, n)
? l
l := LOGIC(_CAST,d)
? l
l := LOGIC(_CAST,b)
? l
l := LOGIC(_CAST,s)
? l

FUNCTION IntFunc() AS INT
RETURN 1
FUNCTION DWORDFunc() AS DWORD
RETURN 0
FUNCTION ByteFunc() AS BYTE
RETURN 255
FUNCTION ShortFunc() AS SHORT
RETURN 8

