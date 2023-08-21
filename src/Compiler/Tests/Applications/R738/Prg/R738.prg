FUNCTION Test( @@item  )
    IF IsInstanceOfUsual( @@item, #SomeClass )
        RETURN TRUE
    ENDIF
    RETURN FALSE
    
FUNCTION Start AS VOID
	? Test(123)
	RETURN 
	    
