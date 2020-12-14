// R752 - paren expression with expression list
// See also https://github.com/X-Sharp/XSharpPublic/issues/495
FUNCTION start() AS VOID
    LOCAL l AS LOGIC
    LOCAL v AS STRING
    l := TRUE                 
    v := "abcd"
    ? iif (l, (v := Upper(v), Left(v,3)), (v := Lower(v), Left(v,4)))                  
    xAssert(iif (l, (v := Upper(v), Left(v,3)), (v := Lower(v), Left(v,4)))  == "ABC")
    l := FALSE
    ? iif (l, (v := Upper(v), Left(v,3)), (v := Lower(v), Left(v,4))) 
    xAssert(iif (l, (v := Upper(v), Left(v,3)), (v := Lower(v), Left(v,4)))  == "abcd")
    xAssert(Test{}:Foo(TRUE) == 3)
    xAssert(Test{}:Foo(FALSE) == 42)

RETURN 



PROC xAssert(l AS LOGIC) 
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN


CLASS Test
    METHOD Foo(lValue AS LOGIC) AS LONG
        RETURN iif(lValue, (1,2,3,4,5,Counter(),Counter(),Counter()), (Counter(40),Counter(),Counter())  )
END CLASS

FUNCTION Counter(nInit := -1 AS LONG) AS LONG
    STATIC nCounter := 0 AS LONG    
    IF (nInit != -1)
        nCounter := nInit
    ELSE
        nCounter ++
    ENDIF
    RETURN nCounter
