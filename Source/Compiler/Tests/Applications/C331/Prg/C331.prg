// 331. error XS0030: Cannot convert type 'int' to 'logic'
// same as with C321, but with the extra parentheses
// original problem is in existing SDK code:

FUNCTION Animate_Stop(hwnd AS PTR) AS LOGIC STRICT
RETURN (LOGIC(_CAST, (SendMessage(hwnd, 0, 0, 0))))
FUNCTION SendMessage(hwnd AS PTR, Msg AS DWORD, wParam AS DWORD, lParam AS INT) AS INT
RETURN 0

FUNCTION Start() AS VOID
LOCAL n := 1 AS INT
LOCAL d := 0 AS DWORD
LOCAL b := 255 AS BYTE
LOCAL s := 8 AS SHORT
LOCAL l AS LOGIC

l := LOGIC(_CAST, (IntFunc()) )
? LOGIC(_CAST, ( (DWORDFunc()) ) )
l := LOGIC(_CAST, (ByteFunc()) )
? LOGIC(_CAST, (ShortFunc()) )

l := LOGIC(_CAST, (n) )
? LOGIC(_CAST, ((d)) )
l := LOGIC(_CAST, (((b))) )
? LOGIC(_CAST , (s) )
? l

FUNCTION IntFunc() AS INT
RETURN 1
FUNCTION DWORDFunc() AS DWORD
RETURN 0
FUNCTION ByteFunc() AS BYTE
RETURN 255
FUNCTION ShortFunc() AS SHORT
RETURN 8

