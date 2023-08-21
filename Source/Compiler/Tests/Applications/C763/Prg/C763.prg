// 763. Problem with UDCs and keywords as field names
// Not sure if this needs to be addressed in the runtime or the pre processor..
// See inline comments
FUNCTION Start() AS VOID
	LOCAL cDbf AS STRING
	cDbf := "C763.dbf"
	DbCreate(cDbf, {{"TEST" , "C" , 10 , 0} , {"DATE" , "D" , 8 , 0} , {"NEXT" , "N" , 10 ,0}})
	DbUseArea(,,cDbf,"alias")

	DbAppend()

// following work fine of course
	_FIELD->TEST := "abc"
	alias->TEST := "def"
	REPLACE TEST WITH "qwe"
	? alias->TEST
	

// In the REPLACE command, DATE must be prefixed with @@, otherwise compiler reprots:
   REPLACE DATE WITH Today()
// error XS9002: Parser: unexpected input 'DATE'

// But when using @@DATE (in either the command, or in the other expressions, in which @@ is not actually needed), 
// then there's a runtime exception, because the field name "@@DATE" cannot be found
	REPLACE @@DATE WITH Today()
	_FIELD->@@DATE := Today()
	alias->@@DATE := Today()                             
	? alias->@@DATE

// same with @@NEXT etc
	REPLACE NEXT WITH 4    
	_FIELD->DATE := Today()
	REPLACE @@NEXT WITH 1
	_FIELD->@@NEXT := 2
	alias->@@NEXT := 3 // ok
	? alias->@@NEXT
	
	DbCloseArea()
RETURN
