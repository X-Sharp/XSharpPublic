// 674. error XS7038: Failed to emit module 'C674'.
// does not happen in Core dialect
FUNCTION Start() AS VOID
	LOCAL c := "" AS STRING
	IF c > 0
		? "the above should report a compiler error, but it throws a 'failed to emit module'"
	ENDIF
RETURN
