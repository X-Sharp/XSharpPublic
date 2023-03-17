FUNCTION Start(args AS STRING[]) AS VOID
    LOCAL cDbf AS STRING
    LOCAL nRecords := 0 AS INT
    LOCAL nSeconds AS REAL8
    LOCAL uValue AS USUAL
    cDbf := "g:\INDEXFIL"
    FOR VAR i := 1 TO 3
        nRecords := 0
        DbCloseAll()
        ? i
        DbUseArea(,"DBFCDX",cDbf,,TRUE)
        IF TRUE // args:Length > 0
            ? "order 1"
            DbSetOrder(1)
        ELSE
            ? "order o"
            DbSetOrder(0)
        ENDIF

        nSeconds := Seconds()
        DbSetFilter({||_FIELD->FileType = "BOOKING"})
        DbGoTop()
        ? "Recno after setting filter:", RecNo()
        DO WHILE .NOT. Eof()
            uValue := FieldGet(1)
            nRecords ++
            DbSkip()
        END DO
        ? "Records in filter:", nRecords
        ? "Time passed:", Seconds() - nSeconds
        DbCloseArea()
    NEXT
    wait

