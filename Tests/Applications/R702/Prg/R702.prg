FUNCTION Start() AS VOID
LOCAL u := "Test{0}" AS USUAL
? String.Format(u, "1") // System.InvalidCastException
RETURN
