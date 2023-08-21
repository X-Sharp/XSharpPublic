// 503. Compiler crash with an identifier with same name with /define:<identifier>

// /vo8+ /define:test

FUNCTION Start( ) AS VOID
	LOCAL test AS INT
	test := 1
	? test
RETURN

// original code:
_DLL FUNCTION mysql_debug( debug AS PSZ ) AS VOID STRICT:libmySQL.mysql_debug

