// error XS0246: The type or namespace name 'Functions$C531$' could not be found
// Problem happens only in Core dialect
FUNCTION Start() AS VOID
? Test()
RETURN

STATIC FUNCTION Test() AS INT
RETURN 123
