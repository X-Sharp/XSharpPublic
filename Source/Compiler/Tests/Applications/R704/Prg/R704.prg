
FUNCTION Start( ) AS VOID
	local cTest 
	cTest := "Test"
	use (cTest)
	USE Test
	USE C:Test
	USE C:\Test
	USE C:\Test\Test
	USE C:\Test\Test.Dbf
	USE .\Test\Test.dbf
	USE ..\Test\Test.dbf
	USE \\Computer\Test\Test.dbf
	USE C:\Date\String\test.dbf
	USE "C:\String\Date\test.dbf" 
	USE Foo Alias Bar
	USE ("somestring")
	USE Date.String
	CLOSE
	
	
RETURN
