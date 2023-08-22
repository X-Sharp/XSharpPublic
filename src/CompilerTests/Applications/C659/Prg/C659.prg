// 659. Runtme error implementing CLIPPER INTERFACE
FUNCTION Start AS VOID  
    LOCAL oTest AS Test
    oTest := Test{}
    // runtime exception
    ? oTest:TestMe(1,2,3)
    xAssert(oTest:TestMe(1,2,3) == 6)
RETURN

INTERFACE ITest 
    METHOD TestMe(a,b,c)
END INTERFACE                                   

CLASS Test IMPLEMENTS ITest     
    VIRTUAL METHOD TestMe(a,b,c)
    	? a,b,c
        RETURN a+b+c
END CLASS


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	THROW Exception{"Incorrect result"}
END IF

