// 517. error XS9002: Parser: mismatched input 'CRLF', are you missing a closing ')' or '}' ?

#define TEST_DEFINE

#ifndef TEST_DEFINE

CLASS TestClass
END CLASS

#endif

#ifdef DOES_NOT_EXIST
CLASS AnotherClass
END CLASS
#endif

FUNCTION Start() AS VOID
? "nothing"
RETURN
