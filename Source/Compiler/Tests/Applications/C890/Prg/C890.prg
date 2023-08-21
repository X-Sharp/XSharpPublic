// 890. Preprocessor can't handle UDCs in code spanning in multiple lines
// https://github.com/X-Sharp/XSharpPublic/issues/1298

FUNCTION Start() AS VOID
	LOCAL n := 5,m := 3 AS INT
	LOCAL i AS INT

#xtranslate TestUDC <expr1> <expr2> => <expr1>-<expr2>
	// OK:
    i := TestUDC n m
    ? i

    i := ;
    	TestUDC n m

	// error XS9002: Parser: unexpected input 'n'
    i := TestUDC ;
    	n m
    i := TestUDC n ;
    	 m


// original sample:
#xtranslate WITH <expr> {<v1>[, <vN>]} => {<expr>:<v1> [, <expr>:<vN>]}
	// OK:
    LOCAL arr := WITH Example() {A, B, C}
    arr := WITH Example() {A, B, C}

	// error XS9002: Parser: unexpected input 'WITH'
    arr := WITH Example() {;
    	A, B, C;
    	}

    arr := WITH Example() ;
	     {A, B, C}
    
    AEval(arr, {|c| QOut(c) })
    

FUNCTION Example() AS TestClass
RETURN TestClass{}

CLASS TestClass
    ACCESS A; RETURN "A"
    ACCESS B; RETURN "B"
    ACCESS C; RETURN "C"
END CLASS
