FUNCTION Start(args as string[]) AS VOID
LOCAL cDbf AS STRING
LOCAL nRecords := 0 AS INT
LOCAL nSeconds AS REAL8
cDbf := "g:\INDEXFIL"

DbUseArea(,"DBFCDX",cDbf,,TRUE)
    if args:Length > 0
        ? "order 1"
    DbSetOrder(1)
else
        ? "order o"
    DbSetOrder(0)
endif

nSeconds := Seconds()
DbSetFilter({||_FIELD->FileType = "BOOKING"})
DbGoTop()
? "Recno after setting filter:", RecNo()
DO WHILE .not. Eof()
	nRecords ++
	DbSkip()
END DO
? "Records in filter:", nRecords
? "Time passed:", Seconds() - nSeconds
DbCloseArea()
wait
