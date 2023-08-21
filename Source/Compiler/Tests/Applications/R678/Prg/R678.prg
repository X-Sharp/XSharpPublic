
FUNCTION Start( ) AS VOID
	local nArea1 as LONG
	local nArea2 as LONG
	DbCreate("Test1",{{"LASTNAME","C",10,0}})
	DbCreate("Test2",{{"LASTNAME","C",10,0}})
	DbUseArea(TRUE, ,"TEST1")
	DbUseArea(TRUE, ,"TEST2")
	Test1->DbAppend()
	test1->FieldPut(1,"Robert")
	XAssert(test1->LastName="Robert")
	Test2->DbAppend()
	test2->FieldPut(1,"Chris")
	XAssert(test2->LastName="Chris")
	Select(0)
	nArea1 := Select("test1")
	nArea2 := Select("test2")
	(nArea1)->LastName := test2->LastName
	XAssert(test1->LastName="Chris")
	test1->FieldPut(1,"Robert")
	(nArea2)->LastName := (nArea1)->LastName
	XAssert(test2->LastName="Robert")	
	XAssert((nArea2)->LastName == (nArea1)->LastName)	


RETURN



PROC xAssert(l AS LOGIC)
IF .not. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
