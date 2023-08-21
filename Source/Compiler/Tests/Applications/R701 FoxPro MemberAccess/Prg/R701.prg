#pragma warnings(9093, off) // undeclared, assume it is cursor
FUNCTION Start( ) AS VOID
	local aStruct as array
	try
		aStruct := {{"FirstName","C",20,0}, {"LastName","C",20,0}}
		DbCreate("test701",aStruct)
		DbUseArea(,"DBFNTX","test701","TEST")
		DbAppend()
		m.somevar = 123
		xAssert( m.SomeVar == 123)
		xAssert(Empty(Test.FirstName))
		xAssert(Empty(Test.LastName))
		Test.FirstName = "Robert"
		Test.LastName  = "van der Hulst"
		xAssert(Test.FirstName = "Robert")
		xAssert(Test.LastName = "van der Hulst")

		Test.FirstName = "Chris"
		Test.LastName  = "Pyrgas"
		xAssert(Test.FirstName = "Chris")
		xAssert(Test.LastName = "Pyrgas")
		DbCLoseArea()
	catch e as Exception
		? e:ToString()
		XAssert(FALSE)
	end try

RETURN


PROC xAssert(l AS LOGIC)  AS VOID
	IF l
		? "Assertion passed"
	ELSE
		THROW Exception{"Incorrect result"}
	END IF
RETURN
