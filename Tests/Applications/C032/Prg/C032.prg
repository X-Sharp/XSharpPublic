// 32. compiler crash
FUNCTION Start() AS VOID
TRY
   NOP
FINALLY
   NOP
END

TRY
   NOP
CATCH e AS Exception
	? e
END

