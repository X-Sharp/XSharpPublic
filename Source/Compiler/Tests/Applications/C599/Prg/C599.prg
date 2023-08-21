// 599. error XS9052: In .Net you cannot take the address of a method or a function. For callback functions you need to use a delegate instead.
// error XS0103: The name 'nfield' does not exist in the current context	22,8	C599.prg	C599 - Bogus error when using ++/-- in alias expression
// Note nField must be declared with a FIELD statement to make this work !
FUNCTION Start() AS VOID
	LOCAL cDbf AS STRING
	FIELD nField
	cDbf := System.Environment.CurrentDirectory + "\C599.dbf"
	IF System.IO.File.Exists(cDbf)
		System.IO.File.Delete(cDbf)
	END IF
	DBCreate(cDbf, {{"NFIELD", "N", 5, 0}}, "DBFCDX", TRUE)
	DBCloseArea()
	
	DBUseArea(TRUE , "DBFCDX" , cDbf , "test")
	DBAppend()
	? test->nfield
	test->nfield += 2
	test->nfield ++ // XS9052
	++test->nfield  // XS9052
	? +test->nfield
	? -test->nfield
	test->nfield -- // XS9052
	? test->nfield
	LOCAL c := "test" AS STRING
	? (c)-> nfield += 1
	(c)-> nfield ++ // XS0103
	(c)-> ++nfield  // XS0103
	(c)-> nfield ++ // XS0103
	(c)-> nfield -= 1
	(c)-> nfield -- // XS0103

	? test->nfield
	IF test->nfield != 5
		THROW Exception{"Incorrect result"}
	ELSE
		? "Test passed"
	END IF
	DBCloseArea()
RETURN
