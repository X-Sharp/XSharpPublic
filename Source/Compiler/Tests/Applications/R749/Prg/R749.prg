
FUNCTION Start( ) AS VOID
  test()
RETURN


FUNCTION Test
    TRY        
        LOCAL m.x = 0
        LOCAL m.y = 1/m.x
        xAssert(FALSE)
        ? m.y
    CATCH TO m.e WHEN Dow(DATE()) = 1
        ? "Dow == 1"  
        xassert(Dow(DATE()) = 1 AND m.e != NULL)
    CATCH TO m.e WHEN Dow(DATE()) != 1
        ? "Dow != 1"
        xassert(Dow(DATE()) != 1 AND m.e != NULL)        
    ENDTRY
    RETURN
    


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN    
