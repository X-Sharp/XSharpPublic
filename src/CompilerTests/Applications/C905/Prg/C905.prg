// 905. Problem using symbol literals #NULL_STRING, #NULL_DATE etc
// https://github.com/X-Sharp/XSharpPublic/issues/1456

// error XS9002: Parser: unexpected input '#'
FUNCTION Start() AS VOID
	LOCAL s AS SYMBOL
	s := #ABC
	s := #NULL
	s := #NULL_DATE
	s := #NULL_STRING
	s := #NULL_ARRAY
	? #NULL_DATE
	
	? Test(#NULL_PTR)

FUNCTION Test(u)
	? u
RETURN u
