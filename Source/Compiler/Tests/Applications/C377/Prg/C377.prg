// 377. compiler crash with comparison NULL with string
// with /vo2 NOT enabled
FUNCTION Start() AS VOID
LOCAL c AS STRING
c := NULL

? c == NULL
? c = NULL
?
? null_string == c // TRUE
? null_string != c // FALSE
? NULL = c  // TRUE
? NULL == c // TRUE
? NULL != c // FALSE

xAssert( c == NULL )
xAssert( c = NULL )

xAssert( null_string == c )// TRUE
xAssertNot( null_string != c )// FALSE
xAssert( NULL = c)  // TRUE
xAssert( NULL == c) // TRUE
xAssertNot( NULL != c) // FALSE

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result"}
END IF
PROC xAssertNot(l AS LOGIC)
xAssert(.not. l)
