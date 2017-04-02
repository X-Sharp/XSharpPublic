// 463. Compiler crash with conflicting #define and DEFINE
#define test
DEFINE test := 1

DEFINE CRLF := "AAA"

FUNCTION Start( ) AS VOID
	? test
	? CRLF
RETURN
