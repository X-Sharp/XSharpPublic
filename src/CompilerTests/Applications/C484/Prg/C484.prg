// 484. error XS1003: Syntax error, 'Source' expected
#ifdef not_defined
CLASS TestClass
END CLASS
#endif

#ifndef it_is_defined
CLASS TestClass2
END CLASS
#endif




// additional test for nested #ifdefs
#ifndef not_defined

FUNCTION Start() AS VOID
	LOCAL n AS INT
	n := 1
#ifndef it_is_defined
	? n
	n ++
#endif
	? n
	IF n != 1
		THROW Exception{"Incorrect Result"}
	END IF
RETURN
#endif

