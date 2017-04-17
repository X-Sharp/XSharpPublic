GLOBAL CRLF := e"/r/n" AS STRING

// 487. Compiler crash with GLOBAL CRLF in first line of .prg

FUNCTION Start() AS VOID
? CRLF
RETURN

