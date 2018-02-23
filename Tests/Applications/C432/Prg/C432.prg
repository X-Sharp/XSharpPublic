// 432. error XS9002: Parser: unexpected input 'END'
// END WHILE, END DO and NEXT <identifier> also not allowed by vulcan
// VO incompatibility, shouldn't we allow them in x#? (VO/vulcan dialect of course only)
// Well, VO accepts anything after those keywords, I think we should allow those that do make sense
FUNCTION Start() AS VOID
DO WHILE FALSE
	? "don't print me"
END WHILE

DO CASE
CASE TRUE
? "do print me"
END //DO

LOCAL n AS INT
FOR n := 1 UPTO 11
	? n
	IF n > 10
		? "> 10"
	ENDIF //n > 10
	
NEXT //n  
FOREACH IMPLIED x IN <INT>{1,2}
	? x 
NEXT //x

RETURN

