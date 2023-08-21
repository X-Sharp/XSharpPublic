// 447. error XS0133: The expression being assigned to 'Functions.UUE_STOP' must be constant
// _Chr() can be treated like a constant, can't it?
DEFINE TEST := "aa" + "bb" + CRLF // ok
DEFINE UUE_STOP := _CHR(96) + CRLF // error
FUNCTION Start( ) AS VOID
	? UUE_STOP
RETURN
