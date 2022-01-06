// https://github.com/X-Sharp/XSharpPublic/issues/902   
#pragma options("vo7", on)

FUNCTION Start( ) AS VOID
LOCAL l AS LOGIC
LOCAL x1, x2 IS _winRECT
l := TRUE             
x1:Bottom := 42
x2:Bottom := 42 * 42
GetItemValues1(@l , @x1 , @x2) // Argument 2: cannot convert FROM 'VO._winRECT' TO 'VO._winRECT*'
GetItemValues6( @x1 , @x2 , @l) // Argument 1: cannot convert FROM 'VO._winRECT' TO 'VO._winRECT*'
GetItemValues2(l , @x1 , @x2)  // <----  this compiles 

GetItemValues4(@l , @x1 )// Argument 2: cannot convert FROM 'VO._winRECT' TO 'VO._winRECT*'	
GetItemValues3(l , @x1 )  // <----  this compiles 

GetItemValues5(@x1 , @x2 ) // <---- this compiles 

RETURN 	         


FUNCTION GetItemValues1 ( lSelected REF LOGIC , struRect1  AS _winRECT , struRect2  AS _winRECT ) AS VOID PASCAL
    xAssert(lSelected)
    xAssert(struRect1:bottom == 42)
    xAssert(struRect2:bottom == 42*42)
	RETURN
FUNCTION GetItemValues2 ( lSelected AS LOGIC , struRect1  AS _winRECT , struRect2  AS _winRECT ) AS VOID PASCAL
    xAssert(lSelected)
    xAssert(struRect1:bottom == 42)
    xAssert(struRect2:bottom == 42*42)
	RETURN
FUNCTION GetItemValues3 ( lSelected AS LOGIC , struRect1  AS _winRECT ) AS VOID PASCAL
    xAssert(lSelected)             
    xAssert(struRect1:bottom == 42)
	RETURN
FUNCTION GetItemValues4 ( lSelected REF LOGIC , struRect1  AS _winRECT ) AS VOID PASCAL
    xAssert(lSelected)                                                                 
    xAssert(struRect1:bottom == 42)
	RETURN
FUNCTION GetItemValues5 ( struRect1  AS _winRECT , struRect2  AS _winRECT ) AS VOID PASCAL
    xAssert(struRect1:bottom == 42)
    xAssert(struRect2:bottom == 42*42)
	RETURN	
FUNCTION GetItemValues6 ( struRect1  AS _winRECT , struRect2  AS _winRECT , lSelected REF LOGIC  ) AS VOID PASCAL
    xAssert(lSelected)
    xAssert(struRect1:bottom == 42)
    xAssert(struRect2:bottom == 42*42)
	RETURN			
	
	
PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN		


