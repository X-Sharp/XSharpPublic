// 222. error XS0208: Cannot take the address of, get the size of, or declare a pointer to a managed type ('object')
// taken from COM module in WinAPI SDK
FUNCTION StgCreateDocfile(pStorage AS OBJECT PTR) AS LONGINT
RETURN 0
FUNCTION Start() AS VOID

RETURN
