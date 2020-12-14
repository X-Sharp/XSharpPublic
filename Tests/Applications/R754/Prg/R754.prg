#command TEST1 <#token> OTHER <*othertokens*> => TestFunc( <(token)>, #<othertokens> )
#command TEST2 <#token> OTHER <*othertokens*> => TestFunc( <(token)>, <"othertokens"> )
#command TEST3 <#token> OTHER <*othertokens*> => TestFunc( <(token)>, <(othertokens)> )


FUNCTION Start AS VOID
     TEST1 token1 OTHER a b c 
     TEST2 token2 OTHER d e f 
     TEST3 token3 OTHER "g h i"
     TEST3 token3 OTHER ("g h i")
     RETURN



FUNCTION TestFunc(cToken AS STRING, cOther  AS STRING)  AS VOID
    ? cToken
    ? cOther                 
    IF cToken == "token1"
        XAssert(cOther == "a b c")    
    ELSEIF cToken == "token2"
        XAssert(cOther == "d e f")    
    ELSE
        XAssert(cOther == "g h i")    
    ENDIF
    
    
    

PROC xAssert(l AS LOGIC) 
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN    
