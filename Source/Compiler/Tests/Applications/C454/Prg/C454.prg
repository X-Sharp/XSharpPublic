// 454. error XS9002: Parser: unexpected input 'TEST'

#command TEST <expr> [OPTION <opt>] => DoTest(<expr> , <opt>)
FUNCTION DoTest(a,b)
	? a,b
RETURN NIL

FUNCTION Start() AS VOID
TEST 1
TEST "a"
TEST #test
TEST 1 OPTION 2
TEST 1l OPTION 2u
TEST 1 OPTION #option
TEST 2016.12.29 OPTION 2017.03.17

TEST iif(TRUE,TRUE,FALSE) option iif(FALSE,TRUE,FALSE)

// error XS9002: Parser: unexpected input 'TEST':
TEST TRUE
TEST .T.
TEST .F.
TEST 1 OPTION .f.
TEST .t. OPTION .f.

RETURN
