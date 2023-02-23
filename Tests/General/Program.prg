USING System.IO
USING XSharp.RT
USING XSharp.ADS
USING XSharp.RDD
USING XSharp.RDD.Support
FUNCTION Start() AS VOID
    IF File.Exists("C:\Temp\Test.dbf")
        File.Delete("C:\Temp\Test.dbf")
    ENDIF
    IF File.Exists("C:\Temp\Test.fpt")
        File.Delete("C:\Temp\Test.fpt")
    ENDIF
    IF File.Exists("f:\Temp\Test.cdx")
        File.Delete("f:\Temp\Test.cdx")
    ENDIF
    VAR fields := RddFieldInfo[]{5}
    fields[1] := RddFieldInfo{"OBJECTTYPE", "C", 30, 0}
    fields[2] := RddFieldInfo{"ID",         "C", 30, 0}
    fields[3] := RddFieldInfo{"NUMBER",     "C", 30, 0}
    fields[4] := RddFieldInfo{"PAYLOAD",    "M", 10, 0}
    fields[5] := RddFieldInfo{"TIMESTAMP",  "C", 14, 0}

    ?CoreDB.Create("C:\Temp\Test.dbf", fields, "AXDBFCDX", FALSE,"Test", NULL, FALSE, FALSE)
    VAR nArea := XSharp.RT.Functions.VoDbSetSelect(-1)
    VAR oi := DbOpenInfo{"C:\Temp\Test.dbf", "test"+DateTime.Now:Ticks:ToString(), nArea, TRUE, FALSE}
    VAR db := DBFCDX{}
    ?db:Open(oi)
    db:Area := nArea
    XSharp.RuntimeState.DataSession:SetArea(nArea, db)
    VAR ord := DBOrderCreateInfo{}
    ord:Order      := "ID"
    ord:Expression := "upper(ID)"
    ?db:OrderCreate(ord)
    TRY; ?RuntimeState.LastRddError:ToString(); CATCH; END TRY
        db:Close()
        wait
RETURN
/*
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
        IF true // args:Length > 0
            ? "order 1"
            DbSetOrder(1)
        ELSE
            ? "order o"
            DbSetOrder(0)
        ENDIF

        nSeconds := Seconds()
        //DbSetFilter({||_FIELD->FileType = "BOOKING"})
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

*/
