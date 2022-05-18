CLEAR
SET PATH TO c:\XSharp\DevRt\XsFoxConnect
SET MEMOWIDTH TO 80
SET PROCEDURE TO XsFoxManager
SET PROCEDURE TO XsFoxArea ADDITIVE
	LOCAL oMgr as XsFoxManager
	LOCAL oFox as XsFoxArea
	LOCAL nArea as long
    LOCAL nFields as LONG
    SET STEP off
    CLEAR
    OPEN DATABASE c:\VFF\Booth\Data\Northwind.dbc
	oMgr = CREATEOBJECT("XsFoxManager") 
	oMgr.CloseAll()
	? oMgr.Set("Deleted","ON")
	? oMgr.Set("Deleted")
	? oMgr.Set("Database")
	&&oFox = oMgr.OpenTable("c:\VFF\Booth\Data\Orders.dbf","Orders")
	
	oMgr.ExecSql("CREATE CURSOR employee (EmpID N(5), Name Character(20), Address C(30), City C(30), PostalCode C(10), OfficeNo C(8) NULL, Specialty Memo)")
	oMgr.ExecSql("Insert into employee(EmpId,Name) values(1,'Chris')")
	oMgr.ExecSql("Insert into employee(EmpId,Name) values(2,'Fabrice')")
	oMgr.ExecSql("Insert into employee(EmpId,Name) values(3,'Robert')")
	oFox = oMgr.GetArea("employee")
	? oFox.Alias()
	? oFox.DbStruct()
	? oFox.GoTop()
	nFields = oFox.FCount()
	? "Fields", nFields
	DO WHILE ! oFox.Eof()
	SET STEP ON 
		FOR nFld = 1 TO nFields
			? PADR(oFox.FieldCaption(nFld),20),LTRIM(PADR(oFox.FieldGet(nFld),100))
		NEXT	
		oFox.Skip(1)
		IF oFox.Recno() > 10
			EXIT
		ENDIF
		
	ENDDO
	oFox.Close()
	oMgr.CloseAll()

	
	
	