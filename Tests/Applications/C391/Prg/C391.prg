// 391. Some issues with alias expressions
// warning XS1656: Cannot assign to 'row' because it is a 'method'
// error XS0103: The name 'colrec' does not exist in the current context
// error XS1662: Cannot convert lambda expression to intended delegate type because some of the return types in the block are not implicitly convertible to the delegate return type

// warnings as errors
GLOBAL cDbf AS STRING

FUNCTION Start() AS VOID
cDbf := System.Environment.CurrentDirectory + "\C391.dbf"
IF System.IO.File.Exists(cDbf)
	System.IO.File.Delete(cDbf)
END IF

LOCAL aDbfStruct AS ARRAY
LOCAL cRowAlias := "myalias" AS STRING

aDbfStruct := {{"ROW", "C", 128, 0}, {"COLREC", "N", 7, 0}, {"VALUE", "C", 25, 0}, {"SORT", "C", 80, 0}}
DBCreate(cDbf, aDbfStruct, "DBFCDX", TRUE, cRowAlias)
DBCloseArea()

DBUseArea(TRUE , "DBFCDX" , cDbf , cRowAlias , TRUE , FALSE)
(cRowAlias)->(VODBAppend(FALSE))
(cRowAlias)->row 		:= "abc"
(cRowAlias)->colrec 	:= 123
(cRowAlias)->value		:= "test"
(cRowAlias)->sort		:= "othertest"
(cRowAlias)->DBCloseArea()

DBUseArea(TRUE , "DBFCDX" , cDbf , cRowAlias , TRUE , FALSE)
(cRowAlias)->DBGoTop()
IF .not. ( AllTrim( (cRowAlias)->row ) == "abc")
	THROW Exception{"Incorrect value stored"}
ENDIF
(cRowAlias)->DBCloseArea()

TestClass{}:Test()

CLASS TestClass
	EXPORT cRowAlias := "myalias" AS STRING
METHOD Test() AS VOID
	DBUseArea(TRUE , "DBFCDX" , cDbf , SELF:cRowAlias , TRUE , FALSE)
	(SELF:cRowAlias)->(VODBAppend(FALSE))
	(SELF:cRowAlias)->(DBRLock())
	(SELF:cRowAlias)->row 		:= "def"
	(SELF:cRowAlias)->colrec 	:= 456
	(SELF:cRowAlias)->value		:= "test2"
	(SELF:cRowAlias)->sort		:= "othertest2"
	(SELF:cRowAlias)->DBCloseArea()
	
	DBUseArea(TRUE , "DBFCDX" , cDbf , SELF:cRowAlias , TRUE , FALSE)
	(SELF:cRowAlias)->DBGoBottom()
	IF .not. ( AllTrim( (SELF:cRowAlias)->row ) == "def")
		THROW Exception{"Incorrect value stored"}
	ENDIF
	
	? RTrim((SELF:cRowAlias)->row)
	? RTrim((SELF:cRowAlias)->value)
		
	(SELF:cRowAlias)->DBCloseArea()
END CLASS

