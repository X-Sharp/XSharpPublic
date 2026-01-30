FUNCTION Start() AS VOID
	LOCAL cDbf AS STRING
	cDbf := "c:\test\vfp.dbf"
	? DbCreate(cDbf,{{"CUR","Y",12,4}},"DBFVFP")
	? DbUseArea(TRUE,"DBFVFP", cDbf)
	? DbAppend()
	? FieldPut(1,$0.1234)
	? FieldGet(1) // 0.0000
	? FieldPut(1,$1.1234)
	? FieldGet(1) // 1.0000
	? DbCloseArea()
wait
