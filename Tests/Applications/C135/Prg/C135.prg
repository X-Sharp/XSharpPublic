// 135. compiler crash with THROW without arguments
// THROW without arguments re-throws the original exception (in vulcan)
FUNCTION Start() AS VOID
LOCAL n:=0 AS INT
TRY
	n := n/n
CATCH
	THROW
END TRY

