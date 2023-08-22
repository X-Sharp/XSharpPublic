// 795. DBEval() problems in the FoxPro dialect
/*
System.InvalidCastException
Unable to cast object of type 'System.Boolean' to type 'XSharp.ICodeblock'.
*/


GLOBAL gcTest AS STRING

FUNCTION Start() AS VOID
LOCAL cFileName AS STRING
cFileName := "testvfp"
DbCreate(cFileName , {{"FLD","N",5,0}})
DbUseArea(,,cFileName)
DbAppend()
DbAppend()
DbAppend()
DbAppend()
DbGoTop()

? "1:"
gcTest := ""
DbEval({|| EvalFunc( RecNo())} ) // System.InvalidCastException
xAssert(gcTest == "1234")
?

? "2:"
gcTest := ""
DbGoTop()
DbEval({|| EvalFunc( RecNo())} , {||RecNo() % 2 == 1}  ) // System.InvalidCastException
xAssert(gcTest == "13")
?

? "3:"
gcTest := ""
DbGoTop()
DbEval({|| EvalFunc( RecNo())} , {||RecNo() < 100} , {||RecNo() % 2 == 1} ) // no exception, but no records processed
xAssert(gcTest == "1")
?

? "4:"
gcTest := ""
DbGoTop()
DbEval({|| EvalFunc( RecNo())} , {||RecNo() < 100} , {||TRUE} ) // no exception, but no records processed
xAssert(gcTest == "1234")

DbCloseArea()

RETURN

FUNCTION EvalFunc(n AS DWORD) AS VOID
gcTest += n:ToString()
? n
RETURN 

PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN	
