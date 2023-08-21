// 454. error XS9002: Parser: mismatched input 'option'

#command TEST <expr> [OPTION <opt>] => DoTest(<expr> , <opt>)
FUNCTION DoTest(a,b)
	? a,b
RETURN NIL

FUNCTION Start() AS VOID
TEST {1,2,3}

TEST {1,2,3} option 4 // error

TEST {{1,2,3}} // ok
TEST {{1,2,3}} option 4 // error

TEST {1,2,3} option {4,5,6} // error
TEST {{1,2,3},{4,5,6}} option {{1,2,3},{4,5,6}} // error
RETURN
