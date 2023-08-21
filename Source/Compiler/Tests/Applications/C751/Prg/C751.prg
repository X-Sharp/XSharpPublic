// 751. Conflict between GLOBAL/DEFINE and field name in index expressions
DEFINE DEFINE_FLD := "abc"
GLOBAL GLOBAL_FLD := 123

FUNCTION Start() AS VOID
	LOCAL cDbf AS STRING
	cDbf := System.Environment.CurrentDirectory + "\C751"
	IF System.IO.File.Exists(cDbf + ".cdx")
		System.IO.File.Delete(cDbf + ".cdx")
	END IF
	
	RddSetDefault("DBFCDX")
	FErase(cDbf + ".dbf")
	FErase(cDbf + ".cdx")
	DbCreate(cDbf , {{"GLOBAL_FLD","C",10,0},{"DEFINE_FLD","D",8,0}})
	
	xAssert( DbUseArea(TRUE,,cDbf,,FALSE) )
	xAssert( DbCreateOrder("TEST1" , cDbf , "Left(GLOBAL_FLD,5)") )
	xAssert( DbCreateOrder("TEST2" , cDbf , "DToS(DEFINE_FLD)")   )
	
	DbCloseArea()
RETURN

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 	
