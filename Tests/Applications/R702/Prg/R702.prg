FUNCTION Start() AS VOID
LOCAL u := "Test{0}" AS USUAL                           
TRY
? String.Format(u, "1") // System.InvalidCastException            
CATCH e as Exception
	? e:Message
end try	
RETURN
