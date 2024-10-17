// 913. More VFP UDC tests


FUNCTION Start() AS VOID
	TestAppendFrom()
	TestScatterGather()
	TestCopyStructure()

// https://github.com/X-Sharp/XSharpPublic/issues/1533
PROCEDURE TestCopyStructure()
	LOCAL cDbf,cDest AS STRING
	LOCAL source AS DWORD
	cDbf := "test"
	cDest := "dest"
	FErase(cDbf + ".cdx")
	FErase(cDest + ".cdx")

//	RddSetDefault("DBFCDX")

	? DbCreate(cDbf,{{"FLD1","C",30,0},{"FLD2","N",10,0},{"MEMO","M",10,0}})
	? DbUseArea(TRUE,,cDbf,"source")
	source := DbSelect()
	DbSelect(source)
	? DbCreateIndex(cDbf,"FLD1")
	DbAppend(); FieldPut(1,"FLD1");FieldPut(2,2);FieldPut(3,"MEMO")


	COPY STRUCTURE TO (cDest)

	? DbUseArea(TRUE,,cDest,"dest")
	DbAppend(); FieldPut(1,"1");FieldPut(2,2);FieldPut(3,"3")

	xAssert(AllTrim(FieldGet(1)) == "1")
	xAssert(FieldGet(2) == 2)
	xAssert(AllTrim(FieldGet(3)) == "3")

	xAssert(FieldName(1) == "FLD1")
	xAssert(FieldName(2) == "FLD2")
	xAssert(FieldName(3) == "MEMO")
	xAssert(.not. @@File(cDest + ".cdx"))


	DbCloseArea()


	DbSelect(source)
	FErase(cDest + ".cdx")

	COPY STRUCTURE TO (cDest) WITH CDX

	? DbUseArea(TRUE,,cDest)
	xAssert(FieldName(1) == "FLD1")
	xAssert(FieldName(2) == "FLD2")
	xAssert(FieldName(3) == "MEMO")
	xAssert(@@File(cDest + ".cdx"))

	DbCloseArea()


	DbSelect(source)
	FErase(cDest + ".cdx")

	COPY STRUCTURE TO (cDest) FIELDS FLD1,FLD2,MEMO

	? DbUseArea(TRUE,,cDest)
	xAssert(FieldName(1) == "FLD1")
	xAssert(FieldName(2) == "FLD2")
	xAssert(FieldName(3) == "MEMO")

	DbCloseArea()


	DbCloseAll()

/*	COPY STRUCTURE FIELDS FLD TO "test2"
	COPY STRUCTURE FIELDS FLD TO "test2" WITH CDX
	COPY STRUCTURE FIELDS FLD,FLD2 TO "test2"
	COPY STRUCTURE FIELDS FLD,FLD2 TO "test2" WITH CDX
	COPY STRUCTURE TO "test2" FIELDS FLD
	COPY STRUCTURE TO "test2" FIELDS FLD , FLD2
	COPY STRUCTURE TO "test2" FIELDS FLD , FLD2 , FLD3	*/


// https://github.com/X-Sharp/XSharpPublic/issues/1529
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


// https://github.com/X-Sharp/XSharpPublic/issues/1534
PROCEDURE TestScatterGather()

	LOCAL cDbf AS STRING
	cDbf := "test"
	FErase(cDbf+".cdx")
	DbCreate(cDbf,{{"FLD1","C",30,0},{"FLD2","N",10,0},{"testmemo","M",10,0}},"DBFCDX")
	DbUseArea(TRUE,"DBFCDX",cDbf)
	DbAppend();	FieldPut(1,"Abc");FieldPut(2,123);FieldPut(3,"MEMO CONTENTS")


	SCATTER MEMVAR MEMO
	? m.FLD1
	? m.testmemo
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


	LOCAL obj
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


	LOCAL a2
	SCATTER TO a2
	xAssert(ALen(a2) == 2)

	LOCAL a3
	SCATTER TO a3 FIELDS LIKE TESTMEMO MEMO
	xAssert(ALen(a3) == 1)
	xAssert(a3[1] == "zzzzzzz")

// https://github.com/X-Sharp/XSharpPublic/issues/1536
	LOCAL a4
	SCATTER TO a4 BLANK MEMO
	xAssert(a4[1] == "")
	xAssert(a4[2] == 0)
	xAssert(a4[3] == "")

	LOCAL a5
	SCATTER TO a5 FIELDS LIKE FLD* MEMO
	xAssert(ALen(a5) == 2)
	xAssert(AllTrim(a5[1]) == "obj")

	LOCAL a6
	SCATTER TO a6 FIELDS LIKE F* MEMO BLANK
	xAssert(ALen(a6) == 2)
	xAssert(a6[1] == "")
	xAssert(a6[2] == 0)

	LOCAL a7
	SCATTER TO a7 FIELDS LIKE F* BLANK
	xAssert(ALen(a7) == 2)

	#warning include SCATTER TO <a> FIELDS LIKE <flist> BLANK test when github.com/X-Sharp/XSharpPublic/issues/1536 is fixed
/*	xAssert(a7[1] == "")
	xAssert(a7[2] == 0)*/

	DbCloseArea()


PROC xAssert(l AS LOGIC)
IF l
	? "Assertion passed"
ELSE
	? "Assertion FAILED"
	THROW Exception{"Incorrect result"}
END IF
RETURN
