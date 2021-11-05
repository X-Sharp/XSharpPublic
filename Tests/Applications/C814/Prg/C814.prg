// 814. Problem creating a codeblock inside a codeblock using the & syntax
// https://github.com/X-Sharp/XSharpPublic/issues/835
FUNCTION Start() AS VOID
PUBLIC cExpr
m->cExpr := "Left('12345', 3)"

? MExec(MCompile( "&(cExpr)") )
xAssert( MExec(MCompile( "&(cExpr)") ) == "123" )

? MExec(MCompile( "Left( &(cExpr) , 2)") )
xAssert( MExec(MCompile( "Left( &(cExpr) , 2)") ) == "12" )

PUBLIC lExpr := TRUE

m -> lExpr := TRUE
? MExec(MCompile( "iif(lExpr , &(cExpr) + 'A' , &(cExpr) + 'B' )") )
xAssert( MExec(MCompile( "iif(lExpr , &(cExpr) + 'A' , &(cExpr) + 'B' )") ) == "123A")

m -> lExpr := FALSE
? MExec(MCompile( "iif(lExpr , &(cExpr) + 'A' , &(cExpr) + 'B' )") )
xAssert( MExec(MCompile( "iif(lExpr , &(cExpr) + 'A' , &(cExpr) + 'B' )") ) == "123B")
RETURN

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
ELSE
	? "Assertion passed"   
END IF

