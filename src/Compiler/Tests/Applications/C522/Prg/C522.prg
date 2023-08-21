// 522. error XS9002: Parser: unexpected input 's'
FUNCTION Start() AS VOID
LOCAL s AS SYMBOL
s := #NULL
? s
? s == #NULL
Test(#NULL)
RETURN

FUNCTION Test(u)
	? u
RETURN NIL

