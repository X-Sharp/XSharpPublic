// 32. compiler crash
FUNCTION Start() AS VOID
TRY 
FINALLY
END

TRY
CATCH e AS Exception
	? e
END

