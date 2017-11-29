// 582. error XS9002: Parser: unexpected input 'test'
// Cannot use alias expression with field named after a keyword
FUNCTION Start() AS VOID

	LOCAL cDbf AS STRING
	LOCAL aDbfStruct AS ARRAY
	cDbf := System.Environment.CurrentDirectory + "\C582.dbf"
	IF System.IO.File.Exists(cDbf)
		System.IO.File.Delete(cDbf)
	END IF
	
//	aDbfStruct := {{"@@FOR", "C", 10, 0}}
	aDbfStruct := {{"FOR", "C", 10, 0}}
	DBCreate(cDbf, aDbfStruct, "DBFCDX", TRUE)
	DBCloseArea()
	
	DBUseArea(TRUE , "DBFCDX" , cDbf , "test")
	DBAppend()

	// compiler error. probably it's ok and we cannot workaround it with @@, but see blow
	//test->FOR := "abc"
	//? test->FOR

	// the following compiles, but does not work at runtime, tries to use a field named "@@FOR"
	test->@@FOR := "abc"
	? test->@@FOR
	DBCloseArea()
RETURN

