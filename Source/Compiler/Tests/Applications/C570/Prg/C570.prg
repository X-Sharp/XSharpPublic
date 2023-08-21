FUNCTION Start AS VOID
	LOCAL oChild AS Child
	oChild := Child{}
	oChild:Value := "Test"
	? oChild:Value
	RETURN


PARTIAL CLASS Child INHERIT Parent
	PROTECT _cValue AS STRING
	ACCESS Value AS STRING
		RETURN _cValue
	ASSIGN Value(cValue AS STRING)
		_cValue := cValue     		
END CLASS	        

