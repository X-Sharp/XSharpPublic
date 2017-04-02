// error XS9003: Pre-processor: repeated match/result markers are not (yet) supported 
// Bogus error?

#command TEST ;
              [ONE <xOn>]          ;
              [TWO <xOff>]         ;
              [THREE <bPostEval>]  ;
=> ;
              DoTest(<xOn>,<xOff>,[<{bPostEval}>])
FUNCTION DoTest(a,b,c)
? a,b,c
IF .not. Eval(c) == 3
	THROW Exception{"Incorrect result"}
ELSE
	? "test passed"
END IF
RETURN NIL


FUNCTION Start() AS VOID
TEST THREE 3
RETURN
