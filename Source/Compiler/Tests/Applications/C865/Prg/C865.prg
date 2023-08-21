// 865. Problems with StrEvaluate()
FUNCTION Start() AS VOID

	OriginalReport()
	
	PUBLIC abc := "test"
	PUBLIC abc_def := "underscore"
	MEMVAR _abc123 
	_abc123 := "newtest"
	
	? StrEvaluate("&abc")
	? StrEvaluate(" &abc ")
	? StrEvaluate("'&abc'")
	
	? StrEvaluate("abc&abc_def")
	? StrEvaluate("abc_def")
	? StrEvaluate("&abc_def==&abc_def")
	? StrEvaluate("&abc_def&abc_def")

	xAssert( StrEvaluate("abc") == "abc")
	xAssert( StrEvaluate("&abc") == "test")
	xAssert( StrEvaluate("&_abc123") == "newtest")
	xAssert( StrEvaluate(" &abc ") == " test ")
	xAssert( StrEvaluate("'&abc'") == "'test'")

	xAssert( StrEvaluate("abc&abc_def") == "abcunderscore")
	xAssert( StrEvaluate("abc_def") == "abc_def")
	xAssert( StrEvaluate("&abc_def") == "underscore")
	xAssert( StrEvaluate("&abc_def==&abc_def") == "underscore==underscore")
	xAssert( StrEvaluate("&abc_def&abc_def") == "underscoreunderscore")
	xAssert( StrEvaluate("&abc_def&abc_def*&abc_def") == "underscoreunderscore*underscore")
	
	
	// VO, Alaska, VFP do not replace values of non-string memvars
	abc := Today()
	? StrEvaluate("&abc")
	? StrEvaluate("&abc1")
	? StrEvaluate("&abc1&test &nothing")
	xAssert( StrEvaluate("&abc") == "&abc")
	xAssert( StrEvaluate("&abc1&test &nothing") == "&abc1&test &nothing")

	abc := #test
	? StrEvaluate("&abc")
	xAssert( StrEvaluate("&abc") == "&abc")

	abc := 123
	? StrEvaluate("&abc")
	xAssert( StrEvaluate("&abc") == "&abc")
	xAssert( StrEvaluate("&abc&abc") == "&abc&abc")

	abc := NIL
	? StrEvaluate("&abc")
	xAssert( StrEvaluate("&abc") == "&abc")
	xAssert( StrEvaluate("&abc&abc") == "&abc&abc")
RETURN

PROCEDURE OriginalReport()
LOCAL cFilter, cFilter2 AS STRING
LOCAL cPolicy AS STRING
MEMVAR cOpt_Filter_Var
MEMVAR cOptFilterVar

cPolicy := "100L"
cOpt_Filter_Var := cPolicy
cOptFilterVar := cPolicy

cFilter := StrEvaluate('mpolicy == "&cOpt_Filter_Var"')
cFilter2 := StrEvaluate('mpolicy == "&cOptFilterVar"')

? "cFilter: " + cFilter
? "cFilter2: " + cFilter2

xAssert(cFilter == 'mpolicy == "100L"')
xAssert(cFilter2 == 'mpolicy == "100L"')

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"
RETURN
