FUNCTION Start() AS VOID
	LOCAL cFileName AS STRING
	cFileName := "C:\temp\exception"
    DbUseArea(,"DBFCDX",cFileName,,TRUE)
	DO WHILE .not. eof()
		? RecNo(), FieldGet(1)
		DbSkip()
	END DO
    DBCloseArea()
    wait
RETURN
