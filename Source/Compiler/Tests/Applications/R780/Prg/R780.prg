// R780 - Internal error UnconvertedConditionalOperator 
// With the /vo7 compiler option it works in VO dialect without cast
#pragma options("vo7", ON)
FUNCTION Start AS VOID
    LOCAL oTest AS Test 
    LOCAL dResult AS decimal
    oTest := Test{}
    oTest:Checked := TRUE
    
    dResult :=  iif(oTest:Checked, oTest:MyMethod(),;
        oTest:MyMethod())
    xAssert(dResult == 1.0m)
    oTest:Checked := FALSE
    dResult :=  iif(oTest:Checked, oTest:MyMethod(),;
        oTest:MyMethod())
    xAssert(dResult == 2.0m)

       
    
    
CLASS Test
    PROPERTY Checked AS LOGIC AUTO
    METHOD MyMethod() AS OBJECT => iif (Checked, 1.0m, 2.0m)
END CLASS    


PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN 
