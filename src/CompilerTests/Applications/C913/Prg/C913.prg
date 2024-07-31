// 913. More VFP UDC tests
FUNCTION Start() AS VOID
	TestAppendFrom()
	TestScatterGather()


PROCEDURE TestAppendFrom()
	LOCAL cSource, cDest AS STRING
	cSource := "source"
	FErase(cSource + ".cdx")
	cDest := "dest"
	FErase(cDest + ".cdx")
	
	DbCreate(cSource,{{"FLD","N",10,0}})
	DbUseArea(TRUE,,cSource)
	DbAppend();FieldPut(1,1)
	DbAppend();FieldPut(1,3)
	DbAppend();FieldPut(1,1)
	DbAppend();FieldPut(1,3)
	DbAppend();FieldPut(1,0)
	DbCloseArea()
	
	DbCreate(cDest,{{"FLD","N",10,0}})
	DbUseArea(TRUE,,cDest)
	APPEND FROM (cSource) FOR _FIELD->FLD < 2
	DbGoTop()
	? RecCount() // 1, should be 3
	xAssert(RecCount() == 3)
	DbGoTop()
	xAssert(FieldGet(1) == 1)
	DbSkip()
	xAssert(FieldGet(1) == 1)
	DbSkip()
	xAssert(FieldGet(1) == 0)

	DbCloseArea()

PROCEDURE TestScatterGather()

	LOCAL cDbf AS STRING
	cDbf := "test"
	FErase(cDbf+".cdx")
	DbCreate(cDbf,{{"FLD1","C",30,0},{"FLD2","N",10,0},{"testmemo","M",10,0}},"DBFCDX")
	DbUseArea(TRUE,"DBFCDX",cDbf)
	DbAppend();	FieldPut(1,"Abc");FieldPut(2,123);FieldPut(3,"MEMO CONTENTS")
	

	SCATTER MEMVAR MEMO
	? m.FLD1
	? testmemo
	xAssert(AllTrim(m.FLD1) == "Abc")
	xAssert(m.FLD2 == 123)
	xAssert(AllTrim(m.testmemo) == "MEMO CONTENTS")
	
	m.FLD1 := "def"
	m.FLD2 := 999
	m.testmemo := "new memo"
	GATHER MEMVAR MEMO
	? FieldGet(2)
	? FieldGet(3)
	xAssert(AllTrim(FieldGet(1)) == "def")
	xAssert(FieldGet(2) == 999)
	xAssert(AllTrim(FieldGet(3)) == "new memo")


	LOCAL a
	SCATTER TO a MEMO
	? a[3]
	xAssert(AllTrim(a[1]) == "def")
	xAssert(a[2] == 999)
	xAssert(a[3] == "new memo")
	
	a[1] := "NEW"
	a[2] := 555
	a[3] := "modified!"
	
	GATHER FROM a MEMO
	? FieldGet(3)
	xAssert(AllTrim(FieldGet(1)) == "NEW")
	xAssert(FieldGet(2) == 555)
	xAssert(AllTrim(FieldGet(3)) == "modified!")
	
	
	SCATTER NAME obj MEMO
	? obj:testmemo
	? obj:FLD1
	xAssert(AllTrim(obj:FLD1) == "NEW")
	xAssert(obj:fld2 == 555)
	xAssert(AllTrim(obj:testmemo) == "modified!")
	
	
	obj:FLD1 := "obj"
	obj:FLD2 := 1
	obj:testmemo := "zzzzzzz"
	GATHER NAME obj MEMO
	? FieldGet(2)
	? FieldGet(3)
	xAssert(AllTrim(FieldGet(1)) == "obj")
	xAssert(FieldGet(2) == 1)
	xAssert(AllTrim(FieldGet(3)) == "zzzzzzz")


	LOCAL b
	SCATTER TO b
	xAssert(ALen(b) == 2)


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	? "Assertion FAILED"
	THROW Exception{"Incorrect result"}
END IF
RETURN
