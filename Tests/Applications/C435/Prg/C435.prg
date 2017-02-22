// error XS0029: Cannot implicitly convert type 'char' to 'string'
FUNCTION Start() AS VOID
LOCAL c AS STRING
c := 'AA' // OK
c := ' '
c := 'A'
? c == 'AA' // OK
? c == 'A'
? Left(c,1) == 'A'

IF .not. c == 'A'
	THROW Exception{"Incorrect result"}
END IF

? CharPos('ABC',2)=='B'

// the following does run correctly and returns the expected result TRUE
// because the macro compiler follows VO rules ('B' is treated as STRING)
? &("CharPos('ABC',2)=='B'")
? &("'B':GetType()") // System.String


LOCAL u AS USUAL
u := 'A'
? u == 'A'
IF .not. u == 'A'
	THROW Exception{"Incorrect result"}
END IF
RETURN
