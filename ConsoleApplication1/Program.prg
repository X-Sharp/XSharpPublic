//
// Start.prg
//
#include "dbcmds.vh"
USING System.Reflection
FUNCTION Start() AS VOID
    LOCAL cb AS CODEBLOCK
    TRY
        TestChris()
        //TestCdx()
        //DumpNtx()
        //Start1a()
        //Start1b()
        //Start2()
        //Start3()
        //Start4()
        //Start5()
        //Start6()
        //Start7()
        //Start8()
        //Start9()
        //Start10()
        //Start11()
        CATCH e
        ErrorDialog(e)
    END TRY
    WAIT
    RETURN


FUNCTION TestChris() AS VOID
LOCAL aDbf AS ARRAY
    LOCAL cDBF AS STRING

    cDBF := "Foo"
    aDBF := {{ "AGE" , "N" , 2 , 0 }}
    ? DBCreate( cDBF , aDbf)
    ? DBUseArea(,"DBFNTX",cDBF,,FALSE)
    DBAppend()
    DBAppend()
    DBAppend()

    DBCloseArea()
   
    //  ----------------
    ? DBUseArea( TRUE ,"DBFNTX",cDBF,"AREA1" ,TRUE )  // open shared
   
    ? "Records" , RecCount() , "must be 3"
    ?
    DBGoTop()
    DBRLock ( RecNo() ) // lock first record
    ? "AREA1 locklist len is" , ALen ( DBRLockList() ) , "must be 1"
    DBGoBottom()
    DBRLock ( RecNo() )  // lock last record
    ? "AREA1 locklist len is" , ALen ( DBRLockList() ) , "must be 2"
    ?
//     try a flock() - it throws a invalidOperationException
//     but only if the current length of the locklist is > 0
    IF FLock()
                   ? "Flock() success"
    ELSE
                   ? "Flock() failed"
    ENDIF
    // No matter if a Flock() attempt returns true or false (if another record is locked in another instance),
    // afterwards the locklist is empty in VO. Not sure if this is as it should be.
    ?  "AREA1 locklist len after Flock() is" , ALen ( DBRLockList() ) , "must be 0"
    ?
    ? DBCloseArea()

    WAIT

RETURN

FUNCTION Progress() AS LOGIC
? Recno()
RETURN TRUE
FUNCTION TestCdx() AS VOID
    LOCAL f AS FLOAT
    f := Seconds()
    ? VoDbUseArea(TRUE, "DBFCDX", "c:\test\TEST10K.DBF", "TEST",TRUE,TRUE)
   	? DbSetIndex("c:\test\TEST10Ka.Cdx")
    ? Used()
    ? DbInfo(DBI_FULLPATH)
    ? "OrderCount", DbOrderInfo(DBOI_ORDERCOUNT)
    ? DbOrderInfo(DBOI_EXPRESSION,,"Age")
    ? DbOrderInfo(DBOI_BAGNAME,,8)
    ? DbOrderInfo(DBOI_FULLPATH,,1)
    ? DbOrderInfo(DBOI_FULLPATH,,8)
    ? DbOrderInfo(DBOI_INDEXEXT)
    ? DbOrderInfo(DBOI_NUMBER,"TEST10K","Age")
    FOR LOCAL IMPLIED nI := 1 TO DbOrderInfo(DBOI_ORDERCOUNT)
        ? nI, DbOrderInfo(DBOI_EXPRESSION,,nI), DbOrderInfo(DBOI_KEYSIZE,,nI), ;
            DbOrderInfo(DBOI_CONDITION,,nI), DbOrderInfo(DBOI_ISDESC,,nI), DbOrderInfo(DBOI_NUMBER,,nI) ,DbOrderInfo(DBOI_UNIQUE ,,nI),DbOrderInfo(DBOI_KEYTYPE ,,nI)
    NEXT
    ? Seconds() -f
    WAIT
    RETURN
    
FUNCTION DumpNtx() AS VOID
    SetAnsi(TRUE)
    SetCollation(#Windows)
    USE "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF"
    DbCreateIndex("10kName.xxx", "upper(Last+First)")
    DbCreateIndex("10kState", "State")
    DbCreateIndex("10kSalary", "Salary")
    DbCreateIndex("10kDate", "Hiredate")
    DbSetIndex()
    // Dump the indexes
    DbSetIndex("c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\10kName.xxx")
    DbOrderInfo(DBOI_USER+42)
    DbSetIndex("c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\10kSalary.ntx")
    DbOrderInfo(DBOI_USER+42)
    DbSetIndex("c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\10kState.ntx")
    DbOrderInfo(DBOI_USER+42)
    DbSetIndex("c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\10kDate.ntx")
    DbOrderInfo(DBOI_USER+42)
    DbCloseArea()
    WAIT
    RETURN
    
    
    
FUNCTION Start1a() AS VOID
    LOCAL aStruct AS ARRAY
    LOCAL i AS DWORD
    aStruct := {{"CHARFIELD","C",10,0},{"NUMFIELD","N",3,0},{"DATEFIELD","D", 8,0}}
    SetAnsi(TRUE)
    SetCollation(#Windows)
    SetNatDLL("german2.dll")
    DBCREATE("Test1Ansi",aStruct, "DBFNTX")
    DBCLOSEAREA()
    USE Test1Ansi       
    FOR i := 1 TO 255
        DBAPPEND()
        _FIELD->CHARFIELD := Replicate(CHR(i),10)
        _FIELD->NUMFIELD  := i
        _FIELD->DATEFIELD := ConDate(1800 + i, 1 + i % 12, 1 + i % 28)
    NEXT
    DBCREATEINDEX("test1Ansi1","CHARFIELD")
    DBCREATEINDEX("test1Ansi2","NUMFIELD")
    DBCREATEINDEX("test1Ansi3","DATEFIELD")
    
    DBCLOSEAREA()
    SetAnsi(FALSE)
    DBCREATE("Test1OEM",aStruct, "DBFNTX")
    DBCLOSEAREA()
    USE Test1OEM       
    FOR i := 1 TO 255
        DBAPPEND()
        _FIELD->CHARFIELD := Replicate(CHR(i),10)
        _FIELD->NUMFIELD  := i
        _FIELD->DATEFIELD := ConDate(1800 + i, 1 + i % 12, 1 + i % 28)
    NEXT                  
    SetCollation(#Clipper)
    DBCREATEINDEX("test1Oem1","CHARFIELD")
    DBCREATEINDEX("test1Oem2","NUMFIELD")
    DBCREATEINDEX("test1Oem3","DATEFIELD")
    DBCLOSEAREA()
    RETURN
    
FUNCTION Start1b() AS VOID
    LOCAL aStruct AS ARRAY
    LOCAL i AS DWORD
    aStruct := {{"CHARFIELD","C",10,0},{"NUMFIELD","N",3,0},{"DATEFIELD","D", 8,0}}
    SetAnsi(TRUE)
    ? XSharp.RuntimeState.WinCodePage
    SetCollation(#Windows)
    DBCREATE("Test2Ansi",aStruct, "DBFNTX")
    DBCLOSEAREA()
    USE Test2Ansi       
    FOR i := 32 TO 255
        DBAPPEND()
        _FIELD->CHARFIELD := Replicate(CHR(i),10)
        _FIELD->NUMFIELD  := i
        _FIELD->DATEFIELD := ConDate(1800 + i, 1 + i % 12, 1 + i % 28)
    NEXT
    DBCREATEINDEX("test2Ansi1","CHARFIELD")
    DBCREATEINDEX("test2Ansi2","NUMFIELD")
    DBCREATEINDEX("test2Ansi3","DATEFIELD")
    DbClearIndex()
    DbSetIndex("test2Ansi1")
    DbSetIndex("test2Ansi2")
    DbSetIndex("test2Ansi3")
    OrdSetFocus(0)
    DO WHILE ! EOF()
        _FIELD->CHARFIELD := ""
        _FIELD->NUMFIELD  := 0
        _FIELD->DATEFIELD := ToDay()
        DBSKIP(1)
    ENDDO
    DbCommit()
    OrdSetFocus(1,"TEST2ANSI1")
    DbOrderInfo(DBOI_USER+42)
    OrdSetFocus(2)
    DbOrderInfo(DBOI_USER+42)
    OrdSetFocus(3)
    DbOrderInfo(DBOI_USER+42)
    DBCLOSEAREA()
    RETURN
    
    
    
FUNCTION Start2() AS VOID
    LOCAL f AS FLOAT
    f := seconds()
    USE "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF"
    DbCreateIndex("10kName.xxx", "upper(Last+First)")
    DbCreateIndex("10kState", "State")
    DbCreateIndex("10kDate", "Hiredate")
    DbCreateIndex("10kSalary", "Salary")
    DbCloseArea()
    ? Seconds() - f
    WAIT
    RETURN
    
    
FUNCTION Start3() AS VOID
    LOCAL cFileName AS STRING
    cFileName := "C:\Test\teest.dbf"
    ? DBCreate(cFileName, {{"FLD1","C",10,0}})
    
    ? DBUseArea ( TRUE , , cFileName , "a1")
    ? DBGetSelect() // 1
    ? DBCloseArea()
    
    ? DBUseArea ( TRUE , , cFileName , "a2")
    ? DBGetSelect() // 2
    ? DBCloseArea()
    
    ? DBUseArea ( TRUE , , cFileName , "a3")
    ? DBGetSelect() // 3
    ? DBCloseArea()
    RETURN
    
    
    
FUNCTION Start4() AS VOID
    LOCAL cFileName AS STRING
    cFileName := "C:\test\laaarge"
    ? DBCreate(cFileName, {{"FLD1","C",10,0},{"FLD2","N",10,0}})
    ? DBUseArea( , , cFileName , , FALSE)
    FOR LOCAL n := 1 AS INT UPTO 10
        DBAppend()
        FieldPut(1, n:ToString())
        FieldPut(2, n)
    NEXT
    ? DBCreateIndex(cFileName + ".ntx" , "FLD2")
    ? DBCloseArea()
    ? "created"
    ? DBUseArea( , , cFileName , , FALSE)
    ? DBSetIndex(cFileName + ".ntx")
    ? DBGoTop()
    ? "skipping"
    DO WHILE ! EOF()
        ? FieldGet(2) , RecNo()
        ? EOF()
        DBSkip()
    END DO
    ? DBCloseArea()
    RETURN
    
    
FUNCTION Start5() AS VOID
    LOCAL cDbf AS STRING
    LOCAL cNtx AS STRING
    
    cDbf := "C:\test\testdbf"
    cNtx := cDbf + ".ntx"
    
    ? DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
    ? DBUseArea(,,cDbf)
    ? DBAppend()
    FieldPut ( 1 , "ABC")
    ? DBAppend()
    FieldPut ( 1 , "GHI")
    ? DBAppend()
    FieldPut ( 1 , "DEF")
    ? DBAppend()
    FieldPut ( 1 , "K")
    ? DBCloseArea()
    
    ? DBUseArea(,,cDbf)
    ? DBCreateIndex(cNtx , "CFIELD")
    ? DBCloseArea()
    
    ? DBUseArea(,,cDbf,,FALSE) // check also with TRUE
    ? DBSetIndex(cNtx)
    ShowRecords()
    // should be ABC, DEF, GHI, K
    
    DBGoTop()
    ? FieldGet(1)
    DBSkip()
    ? FieldGet(1)
    FieldPut(1,"HHH")
    DbCommit()
    ShowRecords()
    // should be ABC, GHI, HHH, K
    
    ? DBCloseArea()
    
FUNCTION ShowRecords() AS VOID
    DBGoTop()
    ? "========="
    ? " Records:"
    DO WHILE .NOT. Eof()
        ? FieldGet(1)
        ? DBSkip() // exception here
    END DO
    RETURN
    
    
FUNCTION Start6() AS VOID
    LOCAL cDbf AS STRING
    cDbf := "c:\test\testdbf"
    
    ? DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 },;
    {"DFIELD" , "D" , 8 , 0 }})
    ? DBUseArea(,,cDbf)
    ? DBAppend()
    FieldPut ( 1 , "B")
    ? DBAppend()
    FieldPut ( 1 , "A")
    ? DBCloseArea()
    
    ? DBUseArea(,,cDbf)
    LOCAL u AS USUAL
    u := FieldGet(2) // it should be a NULL_DATE
    ? u
    ? u == NULL_DATE
    FieldPut(2,u) // exception
    FieldPut(2,NULL_DATE) // exception
    ? DBCloseArea()
    
FUNCTION Start7() AS VOID
    LOCAL cDbf AS STRING
    LOCAL cNtx AS STRING
    cDbf := "C:\Test\testdbf"
    cNtx := cDbf + ".ntx"
    
    ? DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
    ? DBUseArea(,,cDbf,,FALSE)
    ? DBAppend()
    FieldPut ( 1 , "B")
    ? DBAppend()
    FieldPut ( 1 , "A")
    ? DBCloseArea()
    
    ? DBUseArea(,,cDbf,,TRUE) // ----- opening in SHARED mode
    ? DBCreateIndex(cNtx , "CFIELD") // returns TRUE
    ? DBCloseArea()
    
    ? DBUseArea(,,cDbf,,FALSE)
    ? DBSetIndex(cNtx)
    ? DBCloseArea() // XSharp.RDD.RddError here
    
    
FUNCTION Start8() AS VOID
    LOCAL cDbf AS STRING
    cDbf := System.Environment.CurrentDirectory + "\testdbf"
    ? DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
    ? DBUseArea(,,cDbf,,TRUE)
    ? DBAppend()
    ? DBRLock ()
    FieldPut ( 1 , "A")
    //? DBCommit() // makes no difference
    //? DBUnlock() // makes no difference
    ? DBCloseArea() // exception here
    
    
    
FUNCTION Start9() AS VOID
    LOCAL cFileName AS STRING
    LOCAL f AS FLOAT
    LOCAL n AS INT
    f := Seconds()
    ? "Generating dbf and index"
    cFileName := "C:\test\laaarge"
    ? DBCREATE(cFileName, {{"FLD1","C",10,0},{"FLD2","N",10,0}})
    ? "created file , time elapsed:",Seconds() - f 
    ? DBUSEAREA( , , cFileName , , FALSE)
    FOR n := 1 UPTO 100000
        DBAPPEND()
        FIELDPUT(1, AsString(n))
        FIELDPUT(2, 50000-n)
    NEXT
    ? "created records , time elapsed:",Seconds() - f 
    f := Seconds()
    ? DBCREATEINDEX(cFileName + ".ntx" , "FLD1")
    ? DBCLOSEAREA()
    ? "created index, time elapsed:",Seconds() - f 
    ? DBUSEAREA( , , cFileName , , FALSE)
    ? DbSetIndex(cFileName + ".ntx")
    ? DbGotop()
    ? 
    ? "started skipping with index:"
    f := Seconds()
    FOR  n := 1 UPTO 100000 - 1
        DBSKIP()
    NEXT
    ? "skipped, time elapsed:",Seconds() - f
    ? DBCLOSEAREA()
    WAIT
    RETURN 
    
    
FUNCTION Start10() AS VOID
    ? DBAppend()
    RETURN
    
    
FUNCTION Start11() AS VOID
    LOCAL a AS ARRAY
    LOCAL i AS DWORD
    
    setexclusive ( FALSE )
    
    
    IF dbcreate ( "test" , { {"id", "C", 5, 0} })
    
        IF dbuseArea ( , , "test" )
        
            dbappend()
            dbappend()
            dbAppend()
            
            dbGotop()
            
            ?
            ? "Record: " + ntrim ( Recno() )
            ? dbrlock ( Recno() )
            ? dbRecordInfo ( DBRI_LOCKED ) , "Should show TRUE"
            ? IsRlocked()
            ?
            
            dbSkip()
            // record 2 - no lock
            ? "Record: " + ntrim ( Recno() )
            ? dbRecordInfo ( DBRI_LOCKED )
            ? IsRlocked()
            ?
            
            dbskip()
            ? "Record: " + ntrim ( Recno() )
            ? dbrlock ( Recno() )
            ? dbRecordInfo ( DBRI_LOCKED ) , "Should show TRUE"
            ? IsRlocked()
            ?
            
            ? "length of Locklist-Array: " , alen ( a:= DBRlocklist() ) , "(must be 2)"
            ? "Locked records:" , "must show 1 and 3"
            FOR i := 1 UPTO alen ( a )
                ? a [i]
                
            NEXT
            
            
            dbclosearea()
            
        ENDIF
    ENDIF
    
    
    RETURN
    
FUNCTION IsRlocked() AS LOGIC // Helper func

    RETURN ascan ( DBRlocklist() , recno() ) > 0
