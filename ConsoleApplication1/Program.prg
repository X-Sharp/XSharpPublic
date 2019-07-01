//
// Start.prg
//
#include "dbcmds.vh"
USING XSharp.RDD
FUNCTION Start() AS VOID
    TRY
        TestGrowFpt()
        //TestNil()
        //OrdDescFreeze()
        //TestFreezeFilterDesc()
        // testGoTopGoTopCdx()
        //testFptCreate()
        //DescartesDbfTest()
        //FptMemoTest()
        //TestDbfFromChris()
        //TestIndexUpdates()
        //testVFPFiles()
        //testOrdNameInvalid()
        //aevalNil()
        //testFptMemo()
        //testUpdateCdx()
        //testOverFlow()
        //testIndexNoExtension()
        //testRelation()
        //testCustomIndex()
        //testOrdDescend()
        //testScopeDescend()
        //testScope()
        //TestRebuild()
        //Test__Str()
        //testOptValue()
        //testReal8Abs()
        //testIvarGet()
        //testUnlock()
        //testDelete()
        //testUnique2()
        //testCrypt()
        //TestLb()
        // TestCopyStruct()
        //TestDbf()
        //WaTest()
        //bigDbf()
        //testReplace()
        //testRebuild()
        //testUnique()
        //testClearOrderScope()
        //testReplaceTag()
        //TestCdxCreateConditional()
        //TestCloseArea()
//        TestGermanSeek()
//        wait
//        TestCdxForward()
//        wait
//        TestCdxBackward()
//        wait
        //TestCdxSeek()
        //TestCdxCount()
        //TestCdxNumSeek()
        //CloseCdx()
        //TestDescend()
        //TestCdxDateSeek()
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

FUNCTION TestGrowFpt AS VOID
LOCAL aFields AS ARRAY
LOCAL cFile AS STRING
RddSetDefault ( "DBFCDX" )
cFile := "C:\Test\TestFpt"
aFields := { { "ID" , "N" , 2 , 0 },{ "MEMO" , "M" , 10 , 0 }}
DbCreate(cFile, aFields)
DbUseArea(, "DBFCDX", cFile)
DbAppend()
FieldPut(1, 1)
FieldPut(2, Repl("X", 20))
DbCloseArea()
DbUseArea(, "DBFCDX", cFile)
FieldPut(1, 1)
FieldPut(2, Repl("X", 24))
DbCloseArea()
DbUseArea(, "DBFCDX", cFile)
FieldPut(1, 1)
FieldPut(2, Repl("X", 64))
DbCloseArea()
RETURN

FUNCTION TestNil() AS VOID
? ValType(Evaluate("NIL")) // O, wrong
? ValType(NIL) // U, ok
    RETURN

FUNCTION OrdDescFreeze() AS VOID
LOCAL cDBF, cPfad, cIndex, cDriver AS STRING 
LOCAL aFields, aValues AS ARRAY  
LOCAL i AS DWORD
LOCAL lSet AS LOGIC  

   	cDriver := RddSetDefault ( "DBFCDX" ) 
 	
	aFields := { { "LAST" , "C" , 20 , 0 }}  
	aValues := { "b" , "d" , "c", "e" , "a" , "o" , "p" , "r"}	
	cPfad := "C:\test\"         
	cDBF := cPfad + "Foo"
	cIndex := cPfad + "Foox" 
	
	FErase ( cIndex + IndexExt() )	
	
	// -----------------  
	
	lSet := SetDeleted(TRUE)	// <------------  SetDeleted(TRUE) must be set to TRUE !!!
	
	? DbCreate( cDBF , aFields)
	? DbUseArea( ,"DBFCDX",cDBF , , TRUE )		
	
	FOR i := 1 UPTO ALen ( aValues )
		DbAppend() 
		FieldPut ( 1 , aValues [ i ] ) 			
	NEXT 

	? DbCreateOrder ( "ORDER1" , cIndex , "upper(LAST)" , { || Upper ( _FIELD->LAST) }  )  		

	? DbSetOrder ( 1 )  
	
 	OrdScope(TOPSCOPE,  "A")   // <-------- Must be the *very* first order value !
	OrdScope(BOTTOMSCOPE, "O")
    ? "Ascending"
   	DbGoTop() 

	DO WHILE ! Eof()
	   ? recno(), FieldGet ( 1)
		DbSkip ( 1)
	ENDDO  
	
    	?

	OrdDescend( , , TRUE)
	? "Descending"
   	DbGoTop() 

	DO WHILE ! Eof()
	   ? recno(), FieldGet ( 1)
		DbSkip ( 1)
	ENDDO 
		
	DbCloseAll() 
	
	RddSetDefault ( cDriver )	 
    SetDeleted(lSet)
	
	RETURN	
FUNCTION TestFreezeFilterDesc() AS VOID

LOCAL cPath AS STRING
LOCAL cDbf AS STRING
cPath := "C:\test\"
cDbf := "mydbf"
? FErase ( cPath + cDbf + ".dbf" )
? FErase ( cPath + cDbf + ".cdx" )

RddSetDefault ( "DBFCDX" )
DbCreate(cPath + cDbf,{{"FIRST" , "C" , 10,0}})

DbUseArea(,"DBFCDX" , cPath + cDbf)
DbAppend()
FieldPut(1,"Karl-Heinz")
DbAppend()
FieldPut(1,"Robert")
DbAppend()
FieldPut(1,"Chris")
DbAppend()
FieldPut(1,"Karl")
DbGoTop()

? OrdCondSet(,,,,,,,,,,TRUE) // descending
? DbCreateOrder( "ORDER1" , cDbf + ".cdx" , "upper(FIRST)" , { || Upper (_FIELD->FIRST) } )

DbSetOrder(1)
DbGoTop()

DbSetFilter(,"upper(FIRST) = '" + "K" + "'" )
DbGoTop()
? Recno(), _FIELD->FIRST
DbSkip(1)
? Recno(), _FIELD->FIRST

DbGoTop()

LOCAL uRetVal AS USUAL
// program freeze here:
? VoDbOrderInfo( DBOI_POSITION, "", NIL, REF uRetVal )
? uRetVal
DbSkip(1)
? VoDbOrderInfo( DBOI_POSITION, "", NIL, REF uRetVal )
? uRetVal

RETURN



FUNCTION testGoTopGoTopCdx AS VOID
LOCAL cPath AS STRING
LOCAL cDbf AS STRING
cPath := "C:\test\"
cDbf := "mydbf"
? FErase ( cPath + cDbf + ".dbf" )
? FErase ( cPath + cDbf + ".cdx" )

RddSetDefault ( "DBFCDX" )
DbCreate(cPath + cDbf,{{"LAST" , "C" , 10,0}})
DbUseArea(,"DBFCDX" , cPath + cDbf)
FOR LOCAL n := 1 AS INT UPTO 1000
    DbAppend()
    FieldPut(1,"A"+AsString(n % 77))
NEXT
? DbCreateOrder( "ORDER1" , cDbf + ".cdx" , "upper(LAST)" , { || Upper ( _FIELD->LAST) } )
? DbCloseArea()

DbUseArea(,"DBFCDX" , cPath + cDbf)
DbGoTop()
DbGoTop()
LOCAL nCount := 0 AS INT
DO WHILE .NOT. VoDbEof()
    nCount ++
    // ? FieldGet(1), RecNo()
    DbSkip()
END DO
? "Count=", nCount // only 80
DbCloseArea()

FUNCTION TestFptCreate() AS VOID
    LOCAL aStruct AS ARRAY
    aStruct := { ;
        {"NUMBER","N", 10, 0} ,;
        {"MEMO","M", 10, 0} }
    DbCreate("C:\test\testFpt",aStruct, "DBFCDX")
    DbCLoseArea()
    DbUseArea(TRUE, "DBFCDX", "C:\test\testFpt", "TEST")
    FOR VAR i := 1 TO 10
        DbAppend()
        FieldPut(1, i)
        FieldPut(2, Repl("X", 10))
    NEXT
    DbCommit()
    DbCloseArea()
    RETURN

FUNCTION DescartesDbfTest AS VOID
	RDDSetDefault("DBFVFP")
	DbUseArea(TRUE, "DBFVFP", "c:\Descartes\testdbf\ZENSTATS.DBF","ZENSTATS")
    DbSetIndex("c:\Descartes\testdbf\ZENSTATS1Gen.NTX")
    DbOrderInfo(DBOI_USER+42)
    DbGoTop()
    ? DbSeek("117682820180906111733")
//    DbOrderInfo(DBOI_USER+42)
//    DbSetIndex("c:\Descartes\testdbf\zenstat2.ntx")
//    DbOrderInfo(DBOI_USER+42)
//    DbSetIndex("c:\Descartes\testdbf\zenstat3.ntx")
//    DbOrderInfo(DBOI_USER+42)
//    DbSetIndex("c:\Descartes\testdbf\zenstat9.ntx")
	FOR VAR i := 1 TO FCount()
        LOCAL oValue AS OBJECT
        oValue := FieldGet(i)
        ? i, oValue
	NEXT
	DBCloseArea()
	RETURN

FUNCTION FptMemoTest() AS VOID
	RDDSetDefault("DBFCDX")
	DbUseArea(TRUE, "DBFCDX", "c:\download\Memos X#\SETUP.DBF","MEMOS")
	FOR VAR i := 1 TO FCount()
        LOCAL oValue AS OBJECT
        oValue := FieldGet(i)
        IF IsArray(oValue)
            ShowArray(oValue, "Field"+Ntrim(i))
        ELSE
            ? i, oValue, FieldGetBytes((LONG) i)
        ENDIF

		
	NEXT
	DBCloseArea()
	RETURN
FUNCTION testDbfFromChris( ) AS VOID
LOCAL f AS FLOAT
f := seconds()
? DbUseArea(,"DBFCDX","C:\test\Foo")
? DbReindex() // null reference exception
? DbSetOrder(1)
? DbGoTop(), RecNo()
? DbGoBottom(), RecNo() // 2905
? DbSetOrder(0)
? DbGoTop(), RecNo()
? DbGoBottom(), RecNo() // 2964
? DbCloseArea()
? Seconds() - f
RETURN
DEFINE records := 50_000
DEFINE changes := 2000
FUNCTION TestIndexUpdates() 
    LOCAL aFields AS ARRAY
    LOCAL cDBF AS STRING
    LOCAL n AS DWORD
    LOCAL c AS STRING
    RddSetDefault("DBFCDX")
    cDBF := "C:\test\cdxtest"
    aFields := { { "LAST" , "C" , 40 , 0 } , { "FIRST" , "C" , 50 , 0 }}
    FErase(cDBF + ".cdx")
    
    DBCREATE( cDBF , AFields)
    
    DBUSEAREA(,,cDBF)
    DBCREATEINDEX(cDBF , "LAST" , {||_FIELD->LAST})
    FOR n := 1 UPTO records
        c := CHR( 65 + ((n * 41) % 26) )
        c := Replicate(c , 1 + ((n * 31) % 35) )
        DBAPPEND()
        FIELDPUT(1, c)
    NEXT
    DbCommit()
    ? "Commited"
    DbOrderInfo(DBOI_USER+42)
    ? "CheckOrder Start"
    CheckOrder()
    ? "CheckOrder Stop"
    WAIT
    _Quit()
    FOR  n := 1 UPTO changes
        VAR nRec := 1 + (n * 13) % (records - 1)
        DbGoto(nRec)
        c := CHR( 65 + ((n * 21) % 26) )
        c := Replicate(c , 1 + ((n * 23) % 35) )
        FIELDPUT(1, c)
    NEXT
    ? "CheckOrder 2"
    CheckOrder()
    DbOrderInfo(DBOI_USER+42)
    DBCLOSEALL()   
    WAIT
RETURN  NIL

PROCEDURE CheckOrder()
    LOCAL nCount := 0 AS INT
    LOCAL cPrev := NULL_STRING AS STRING
    DbGotop()
    DO WHILE .NOT. EOF()
        nCount ++
        IF cPrev != NULL_STRING
            IF cPrev > FIELDGET(1)
                ? "Wrong order at recno", RECNO(), cPrev, FieldGet(1)
            END IF
        END IF
        cPrev := FIELDGET(1)
        DBSKIP(1)
    END DO
    
    IF nCount != records
        ? "Wrong record count", nCount
    END IF
    
RETURN 

FUNCTION TestVFPFiles() AS VOID
LOCAL cDbf AS STRING
LOCAL aStruct AS ARRAY
RddSetDefault("DBFCDX")
//RddInfo(SET.AUTOOPEN, FALSE)
cDbf := "c:\test\bldquery.vcx"
cDbf := "c:\test\solution.scx"
cDbf := "c:\test\ADMKERTO.DBC"
DbUseArea(,,cDbf)
aStruct := DbStruct()
DbSetOrder(1)

DbSetFilter(,"ObjectType='Table'")
DbGoTop()
DO WHILE ! eof()
    ? recno()
    FOREACH aField AS ARRAY IN aStruct
        ?? /*aField[1], ":", */IIF(aField[1] != "OBJCODE", Trim(AsString(FieldGetSym(aField[1]))), "<binary>"),""
    NEXT
    ?
    DbSkip(1)
ENDDO
RETURN



FUNCTION TestOrdNameInvalid() AS VOID
LOCAL cDBF AS STRING
LOCAL aFields, aValues AS ARRAY
LOCAL i AS DWORD

RDDSetDefault ( "DBFNTX" )

aFields := { { "LAST" , "C" , 20 , 0 }}
aValues := { "b" , "c" , "d", "e" , "a" }

cDBF := "C:\test\mydbf"

? "Ferase" , FErase ( cDBF + IndexExt() )
// -----------------
? DBCreate( cDBF , AFields)
? DBUseArea(,,cDBF )

FOR i := 1 UPTO ALen ( aValues )
DBAppend()
FieldPut ( 1 , aValues [ i ] )
NEXT

FOR i := 1 UPTO 3
? DBCreateOrder ( "ORDER" +NTrim ( i ) , cDBF+NTrim ( i ) , "upper(LAST)" , { ||Upper ( _FIELD->LAST) } )
NEXT
DbClearIndex()
OrdListAdd(cDBF+"1")
OrdListAdd(cDBF+"2")
OrdListAdd(cDBF+"3")
?
DBSetOrder ( 2 )
? OrdBagName ( 1 ) // ok
? OrdBagName ( 2 ) // ok
? OrdBagName ( 3 ) // ok
? OrdBagName ( "order1" ) // ok
? OrdBagName ( 4 ) // order doesn/t exist, must return ""
? OrdBagName ( "order4" ) // order doesn/t exist, must return ""
?
? OrdNumber ( "order4") // order doesn/t exist, must return 0 instead of 2
? OrdNumber ( 4 ) // order doesn/t exist, must return 0 instead of 2
? OrdNumber() // ok , current order is 2
?
? OrdName() // ok, "ORDER2" is the current order
? OrdName(3) // ok, "ORDER3"
? OrdName("ORDER3") // ok, "ORDER3"
? OrdName(4) // order doesn/t exist, must return ""
? OrdName("ORDER4") // order doesn/t exist, must return ""

DBCloseAll()
RETURN

FUNCTION AevalNil() AS VOID
LOCAL aArr AS ARRAY

aArr := {1,2,3}
AEval(aArr, {|x| Console.WriteLine((INT)x)},NIL,NIL) 
RETURN

FUNCTION testFptMemo() AS VOID
LOCAL aFields AS ARRAY
LOCAL cDBF AS STRING

RDDSetDefault("DBFCDX")
cDBF := "c:\test\mytest"
aFields := { { "LAST" , "C" , 20 , 0 } , ;
{ "COMMENTS" , "M" , 10 , 0 }}
FErase(cDBF + ".dbt")
FErase(cDBF + ".fpt")

? DBCreate( cDBF , AFields) // generates .dbt file
// ? DBCreate( cDBF , AFields , "DBFCDX") // same

? File(cDBF + ".dbt")
? File(cDBF + ".fpt")

IF File(cDBF + ".dbt")
    FRename(cDBF + ".dbt",cDBF + ".fpt")
END IF
? DBUseArea(,,cDBF) // fails because it searchs for a .dbt file
DBCloseAll()
RETURN 

FUNCTION testUpdateCdx AS VOID
	LOCAL cDBF AS STRING
	LOCAL aFields, aValues AS ARRAY 
	LOCAL cPrev AS STRING
	LOCAL nCount AS INT
	LOCAL i AS DWORD
	
	RDDSetDefault ( "DBFCDX" )
	
	aFields := { { "NUM" , "N" , 8 , 0 },{ "LAST" , "C" , 100 , 0 }} 
	aValues := { "b" , "c" , "d", "e" , "a" , "r" , "t" , "g" , "m" , "n" , "t" , "b" , "h" , "f" , "y", "r", "t", "y", "z", "v", "e", "r", "b", "z", "b", "m", "w", "e" }
	
	cDBF := "C:\test\mycdx"
	? "Ferase" , FErase ( cDbf + IndexExt() )       

	DBCreate( cDBF , AFields)
	DBUseArea(,,cDBF )
	FOR i := 1 UPTO ALen ( aValues )
		DBAppend()
		FieldPut ( 1 , i )                                      
		FieldPut ( 2 , Replicate( aValues [ i ] , 50) )
	NEXT
	DBCreateIndex ( cDBF , "NUM" , {||_FIELD->NUM})
	DBCreateOrder ( "LAST" , cDBF , "LAST" , {||_FIELD->LAST})
	DBCloseAll()
	

	DBUseArea(,,cDBF )
	DBSetOrder(2)
	DBGoBottom()
	
	FieldPut(2, "a")
	DBSkip(-5)
	FieldPut(2, "d")
	DBSkip(5)
	FieldPut(2, "z")
	

	cPrev := NULL
	nCount := 0
	DBGoTop()
	DO WHILE .NOT. EoF()
		nCount ++
		? AllTrim(FieldGet(2))
		IF cPrev != NULL
			IF .NOT. (cPrev <= FieldGet(2))
				? " *** incorrect order ***"
			ENDIF
		END IF
		cPrev := FieldGet(2)
		DBSkip()
	END DO
	? ALen(aValues), nCount
	?
	? "no order"
	DBSetOrder(0)
	DBGoTop()
	DO WHILE .NOT. EoF()
		? AllTrim(FieldGet(2))
		DBSkip()
	END DO

	DBCloseArea()
RETURN

FUNCTION testOverflow() AS VOID
LOCAL cDBF AS STRING

cDBF := "c:\test\test"
FErase(cDBF + ".ntx")
FErase(cDBF + ".cdx")

RDDSetDefault("DBFNTX")

DBCreate( cDBF , {{ "AGE" , "N" , 4 , 1 }})
DBUseArea(,,cDBF, ,FALSE)
DBAppend()
FieldPut(1,12345)

? FieldGet(1) // exception

DBCreateIndex( cDbf, "age" ) // exception

DBCloseArea()
RETURN


FUNCTION TestIndexNoExtension AS VOID
LOCAL cDBF, cPfad, cIndex AS STRING
LOCAL aFields, aValues AS ARRAY
LOCAL i AS DWORD

RDDSetDefault("DBFNTX")
RDDSetDefault("DBFCDX")
aFields := { { "LAST" , "C" , 20 , 0 }}
aValues := { "b" , "c" , "d", "e" , "a" }

cPfad := "c:\test\"
cDBF := cPfad + "Foo"
cIndex := cPfad + "Foox"
? "Ferase" , FErase ( cIndex + IndexExt() )

? DBCreate( cDBF , AFields)
? DBUseArea(,,cDBF)
FOR i := 1 UPTO ALen ( aValues )
DBAppend()
FieldPut ( 1 , aValues [ i ] )
NEXT
? DBCreateOrder ( "ORDER1" , cIndex , "upper(LAST)" , { || Upper (_FIELD->LAST) } )
? DBCloseAll()
? "Open oDB"

// When ".cdx" is added SetIndex() returns true
// cIndex := cIndex + IndexExt()
? DBUseArea(,,cDBF)
? VODBOrdListAdd(cIndex , NIL) // Returns FALSE, error
? DBCloseAll()
RETURN
FUNCTION TestRelation() AS VOID
 LOCAL cDBF1, cDBf2 AS STRING
LOCAL cINdex1, cINdex2 AS STRING
LOCAL cPfad AS STRING
LOCAL aFields, aValues AS ARRAY
LOCAL i AS DWORD

cPfad := "C:\TEST\" // "c:\xide\projects\project1\bin\debug\"
cDBF1 := cPfad + "relation1"
cDBf2 := cPfad + "relation2"

cINdex1 := cPfad + "relation1"
cINdex2 := cPfad + "relation2"
// ------- create Parent DBF --------------
aFields := { { "ID" , "C" , 5 , 0 }}

aValues := { "00002" , "00001" , "00003" }

? DBCREATE( cDBF1 , AFields)
? DBUSEAREA(,"DBFCDX",cDBF1 )

FOR i := 1 UPTO ALen ( aValues )
DBAPPEND()
FIELDPUT ( 1 , aValues [ i ] )
NEXT

? DBCREATEINDEX( cINdex1, "ID" )

// ------- create Child DBF --------------
aFields := { { "ID" , "C" , 5 , 0 },;
	{ "TEXT1" , "C" ,20 , 0 }}
aValues := { { "00002" , "Text1 00002" } , { "00001" , "Text2 00001" }, { "00001" , "Text1 00001"} ,;
{ "00001" , "Text3 00001" } , {"00003" , "Text1 00003" } , { "00002" , "Text2 00002"} ,;
{ "00003" , "Text3 00003" } , {"00002" , "Text3 00002" } , { "00001" , "Text4 00001"} ,;
{ "00003" , "Text2 00003" } , {"00003" , "Text4 00003" } }

? DBCREATE( cDBf2 , AFields)
? DBUSEAREA(,"DBFCDX",cDBf2 )

FOR i := 1 UPTO ALen ( aValues )
DBAPPEND()
FIELDPUT ( 1 , aValues [ i , 1 ] )
FIELDPUT ( 2 , aValues [ i , 2 ] )
NEXT

? DBCREATEINDEX( cINdex2, "ID + TEXT1" )

DBCloseAll()

// ------------------------
// open Parent DBF

? DBUseArea(TRUE ,"DBFNTX",cDBF1 )
? DBSetIndex( cINdex1 )
? DBSetOrder ( 1 )
? DBGoTop()

// open Child DBF
? DBUseArea(TRUE,"DBFNTX",cDBf2 )
? DBSetIndex( cINdex2 )
? DBSetOrder ( 1 )

DBSetSelect ( 1 )
// set the relation to the common field ID
? DBSetRelation(2, {|| _FIELD->ID } , "ID" )
?
DO WHILE ! a->EOF()
    DO WHILE a->FieldGet ( 1 ) == b->FieldGet ( 1 ) 

        // excepion here. Removing it makes DO WHILE never end
        ? a->FieldGet ( 1 ) , b->FieldGet ( 1 ) ,b->FieldGet ( 2 )
        b->DBSkip(1)
    ENDDO
    ?
    a->DBSkip(1)
ENDDO

DBCLOSEALL()
FUNCTION TestCustomIndex AS VOID
LOCAL cDBF, cPfad, cIndex, cDriver AS STRING
LOCAL aFields, aValues AS ARRAY
LOCAL i AS DWORD

cDriver := RddSetDefault ( "DBFCDX" )

aFields := { { "LAST" , "C" , 20 , 0 }}

aValues := { "Goethe" , "Goldmann" , "Ober",;
"Osterreich" , "Gothe" , "Gotz" , "Gobel" ,;
"Otter" , "Anfang" , "Art" , "Arger" }

cPfad := "c:\test\"
cDBF := cPfad + "Foo"
cIndex := cPfad + "Foox"

FErase ( cIndex + INDEXEXT() )
// -----------------
? DBCREATE( cDBF , AFields)
? DBUSEAREA(,"DBFCDX",cDBF )
FOR i := 1 UPTO ALen ( aValues )
DBAPPEND()
FIELDPUT ( 1 , aValues [ i ] )
NEXT

FOR i := 1 UPTO 2
DBSETORDERCONDITION()
IF i == 2
// second order should be a custom order.
DBSETORDERCONDITION(,,,,,,,,,,,,, TRUE)
ENDIF
? DBCREATEORDER ( "ORDER"+ NTrim(i) , cIndex , "upper(LAST)" , { ||Upper ( _FIELD->LAST) } )
// ? OrdCreate(cIndex, "ORDER"+NTrim(i), "upper(LAST)", { || Upper ( _Field->LAST) } ) // ok
NEXT

?
? DbSetOrder ( 1 )
? OrdName() // "ORDER1" ok
? ORDNUMBER() // 1 ok
? ORDKEY(1) // "UPPER(LAST)" ok
? DbOrderInfo ( DBOI_CUSTOM ) // returns FALSE ok
? DbOrderInfo ( DBOI_KEYCOUNT ) // ok, shows 11
? OrdKeyCount( 1 , cIndex ) // ok, shows 11
? OrdKeyCount( "ORDER1" , cIndex) // ok, shows 11
? OrdKeyCount( 1 )
? OrdKeyCount()
?
? DbSetOrder ( 2 )
? OrdName() // "ORDER2" ok
? OrdNumber() // 2 ok
? OrdKey(2) // "UPPER(LAST)" ok
? DBOrderInfo ( DBOI_CUSTOM ) // NOTE: returns FALSE instead of TRUE
? DBOrderInfo ( DBOI_KEYCOUNT ) // NOTE: shows 11 instead of 0
? OrdKeyCount( 2 , cIndex ) // NOTE: shows 11 instead of 0
? OrdKeyCount( "ORDER2" , cIndex) // NOTE: shows 11 instead of 0
? OrdKeyCount( 2 )
? OrdKeyCount()

?
? IndexCount() // 2 ok
?

// NOTE: ORDDESTROY() problem. if the order doesn´t exist the func returns TRUE and
// seems to do nothing. VO throws an error if a order doesn´exist.
? OrdDestroy("ORDER4") // NOTE: "ORDER4" does not exist
? IndexCount()
? IndexOrd()

DBCloseAll()

RddSetDefault ( cDriver )
RETURN
FUNCTION testOrdDescend() AS VOID
LOCAL cDBF, cPfad, cIndex, cDriver AS STRING
LOCAL aFields, aValues AS ARRAY
LOCAL i AS DWORD
cDriver := RDDSetDefault ( "DBFCDX" )
aFields := { { "LAST" , "C" , 20 , 0 }}
aValues := { "b" , "d" , "c", "e" , "a" }
cPfad := "c:\test\"
cDBF := cPfad + "Foo"
cIndex := cPfad + "Foox"
FErase ( cIndex + IndexExt() )
// -----------------
? DBCreate( cDBF , AFields)
? DBUseArea(,"DBFCDX",cDBF )
FOR i := 1 UPTO ALen ( aValues )
DBAppend()
FieldPut ( 1 , aValues [ i ] )
NEXT

? DBCreateOrder ( "ORDER1" , cIndex , "upper(LAST)" , { || Upper(_FIELD->LAST) } )
? DBSetOrder ( 1 )
?
? "IsDesc" ,DBOrderInfo(DBOI_ISDESC) // false, correct
DBGoTop()
DO WHILE ! EOF()
// 5,1,3,2,4 correct
? FieldGet ( 1 ) , RecNo()
DBSkip(1)
ENDDO
?

? OrdDescend ( ,, TRUE )
? "IsDesc" ,DBOrderInfo(DBOI_ISDESC) // false, wrong

?
DBGoTop()
DO WHILE ! EOF()
// 5,1,3,2,4 again, wrong, should be 4,2,3,1,5
? FieldGet ( 1 ) , RecNo()
DBSkip(1)
ENDDO
DBCloseAll()
RDDSetDefault ( cDriver ) 

FUNCTION testScopeDescend() AS VOID
LOCAL cDbf AS STRING
cDBF := "testdbf"

DBCreate( cDBF , {{"FIELDN" , "N" ,5 , 0 } } )
DBUseArea(,"DBFCDX",cDBF)
DBAppend()
FieldPut(1,3)
DBAppend()
FieldPut(1,1)
DBAppend()
FieldPut(1,4)
DBAppend()
FieldPut(1,2)

DBSetOrderCondition(,,,,,,,,,,TRUE)
DBCreateIndex(cDbf, "FIELDN" )

DBOrderInfo( DBOI_SCOPETOP, "", NIL, 3 )
DBOrderInfo( DBOI_SCOPEBOTTOM, "", NIL,2 )

// prints 3,2,1. If the order of scopes is
// changed to 2->3, then it prints 2,1
? "Down"
DbGotop()
DO WHILE .NOT. EOF()
    ? "val",FIELDGET(1) , "#",RECNO()
    DBSKIP()
ENDDO

? "Up"
DbGoBottom()
DO WHILE .NOT. BOF()
    ? "val",FIELDGET(1) , "#",RECNO()
    DBSKIP(-1)
ENDDO
? "Clear scopes"
DBOrderInfo( DBOI_SCOPETOPCLEAR )
DBOrderInfo( DBOI_SCOPEBOTTOMCLEAR)
? "Down"
DbGotop()
DO WHILE .NOT. EOF()
    ? "val",FIELDGET(1) , "#",RECNO()
    DBSKIP()
ENDDO

? "Up"
DbGoBottom()
DO WHILE .NOT. BOF()
    ? "val",FIELDGET(1) , "#",RECNO()
    DBSKIP(-1)
ENDDO

DBCloseArea()

FUNCTION testscope AS VOID
LOCAL cDBF AS STRING
LOCAL aFields, aValues AS ARRAY
LOCAL i AS DWORD

//RDDSetDefault("DBFNTX")
RDDSetDefault("DBFCDX")

cDbf := "C:\test\mycdx"
FErase(cDbf + ".ntx")
FErase(cDbf + ".cdx")

aValues := {"Gas" , "Abc", "Golden" , "Guru" , "Ddd" , "Aaa" , "Ggg"}
aFields := { {"CFIELD" , "C" , 10 , 0} }

DBCreate(cDbf , aFields)
DBUseArea(,,cDBF)
DBCreateIndex(cDbf , "Upper(CFIELD)")
FOR i := 1 UPTO ALen(aValues)
DBAppend()
FieldPut(1, aValues[i])
NEXT

DBGoTop()
? DBOrderInfo( DBOI_KEYCOUNT ) // 7, correct

? "Setting order scope"
OrdScope(TOPSCOPE, "G")
OrdScope(BOTTOMSCOPE, "G")
DBGoTop()

// X#: -2 with both CDX and NTX
// VO: -2 with NTX, 4 with CDX
? DBOrderInfo( DBOI_KEYCOUNT )

? DBSeek("G") // TRUE, correct
? DBSeek("GOLD") // TRUE with NTX, FALSE with CDX. VO TRUE in both

? "Clearing order scope"
OrdScope(TOPSCOPE, NIL)
OrdScope(BOTTOMSCOPE, NIL)
? DBOrderInfo( DBOI_KEYCOUNT )
? DBSeek("G")
? DBSeek("GOLD")

? "Setting order scope again"
OrdScope(TOPSCOPE, "G")
OrdScope(BOTTOMSCOPE, "G")
DBGoTop()
? DBOrderInfo( DBOI_KEYCOUNT )
? DBSeek("G")
? DBSeek("GOLD")

DBCloseArea()
RETURN
FUNCTION TestRebuild() AS VOID
LOCAL cDBF, cPfad, cIndex  AS STRING
LOCAL aFields, aValues AS ARRAY
LOCAL i AS DWORD
LOCAL lSHared AS LOGIC

lSHared := TRUE

RddSetDefault ( "DBFCDX" )

aFields := { { "LAST" , "C" , 20 , 0 }}
aValues := { "b" , "d" , "c", "e" , "a" }

cPfad := "C:\test\"
cDBF := cPfad + "Foo"
cIndex := cPfad + "Foox"

FErase ( cIndex + INDEXEXT() )

DBCREATE( cDBF , AFields)
DBUSEAREA(,"DBFNTX",cDBF , , lSHared )
FOR i := 1 UPTO ALen ( aValues )
DBAPPEND()
FIELDPUT ( 1 , aValues [ i ] )
NEXT

? DBCREATEORDER ( "ORDER1" , cIndex , "upper(LAST)" , { || Upper (_FIELD->LAST) } )

? DbSetOrder ( 1 )

// DBReindex()
//
// When the DBF is opened shared VO throws an "Shared error",
// while X# seems to allow a reindex , no matter if the DBF is openedshared or not.

? "Reindex", DBDRIVER(), DbReindex()

DBCLOSEALL()   
FUNCTION Test__Str AS VOID
    LOCAL r := 1.2354 AS REAL8
    ? __str(r,-1,-1)
    WAIT
    RETURN

FUNCTION testOptValue() AS VOID STRICT

       

    LOCAL oTEst AS TestOpt

    LOCAL oObject AS OBJECT

   

    oTest := TestOpt{}

    oOBject := TestOpt{}

   

    oTest:SetStyle(1)  //Default param is TRUE

    oOBject:SetStyle(1) //Default param is FALSE

    

    RETURN     

 

 

    CLASS TestOpt

       

        METHOD SetStyle(liStyle AS INT,lEnableStyle := TRUE AS LOGIC) AS VOID

       

        LOCAL lTest AS LOGIC

       

        lTest := lEnableStyle
        ? lTest

       

        RETURN

        

    END CLASS    
    FUNCTION TestReal8Abs AS VOID
        
        LOCAL rReal1, rReal2 AS REAL8        
        LOCAL uUsual AS USUAL
        LOCAL c1, c2 AS STRING
        LOCAL nMenge1 := 1 AS INT
        nMenge1 := Integer(nMenge1)
        rReal1 := 10.25
               
        uUsual := rReal1 //Decimals -1 - not evaluated?
        c1 := NTRIM(uUsual) //10.25
       
        rReal2 := Abs(rReal1)  // 10.25 - OK
        uUsual := Abs(rReal1) // Problem - FLOAT 10, Value is 10.25 but DECIMALS is 0
        
        c2 := NTRIM(uUsual) //10
        ? c1, c2
        WAIT

    FUNCTION TestIvarGet() AS VOID
    LOCAL n := 0 AS INT
    LOCAL c := "abc" AS STRING
    LOCAL uNil := NIL AS USUAL
    ? n == NIL
    ? n == uNil
    ? NIL == n
    ? uNil == n
    
    ? uNil == c
    ? c == NIL
    ? c == uNil
    
    LOCAL o AS USUAL
    o := TestClass{}
    ? o:NilMethod() == 1
    ? o:NilMethodUntyped() == 1
    ? o:NilMethodUntyped() == c

    ? o:NilAccess == 1
    ? o:NilAccess == ""

    ? o:DoesNotExistAccess == 1
    ? o:DoesNotExistMethod() == 1

    ? o:DoesNotExistMethodNil() == 1
    ? o:DoesNotExistMethodNil() == "abc"

    ? o:DoesNotExistAccessNil == 1 // exception here "Value does not fall within the expected range."
    ? o:DoesNotExistAccessNil == "abc" // exception
    ? o:DoesNotExistAccessNil == n // exception

    ? o:DoesNotExistAccessNil == NIL // OK, TRUE
RETURN

CLASS TestClass
    METHOD NilMethod() AS USUAL
    RETURN NIL
    METHOD NilMethodUntyped()
    RETURN NIL
    
    ACCESS NilAccess
    RETURN NIL
    
    METHOD NoMethod(c)
        ? c
        IF AsString(c) == "DOESNOTEXISTMETHOD"
            RETURN 2
        END IF
    RETURN NIL
    METHOD NoIVarGet(c)
        ? c
        IF AsString(c) == "DOESNOTEXISTACCESS"
            RETURN 2
        END IF
    RETURN NIL
END CLASS

FUNCTION testUnlock() AS VOID
LOCAL cDBF,cPath AS STRING
LOCAL aFields, aValues AS ARRAY
LOCAL i AS DWORD

RDDSetDefault("DBFCDX")
SetExclusive( FALSE )

cPath := "C:\TEST\"
cDBF := cPath + "small"
FErase(cDBF)
FErase(cDBF + ".cdx")

aFields := { { "CFIELD" , "C" , 5 , 0 }}
aValues := { "AAA" , "CCC" , "BBB" }

? DBCreate( cDBF , AFields)
? DBUseArea(,"DBFCDX",cDBF )
FOR i := 1 UPTO ALen(aValues)
DBAppend()
FieldPut(1 , aValues[i])
NEXT
? DBCreateIndex(cDbf , "CFIELD")
DBCloseAll()


? DBUseArea(,"DBFCDX",cDBF )
DBGoTop()
DBSkip()
? RecNo(), FieldGet(1)

? DBRLock()
? DBDelete()
? DBUnlock() // StackOverflow here

DBCloseAll()
RETURN
FUNCTION testDelete() AS VOID
LOCAL cDBF,cPath AS STRING
LOCAL AFields AS ARRAY
//LOCAL i AS DWORD

cPath := "C:\TEST\" // "c:\xide\projects\project1\bin\debug\"
cDBF := cPath + "mydbf"
// ------- create Parent DBF --------------
AFields := { { "ID" , "C" , 5 , 0 }}

RDDSetDefault("DBFNTX")
? DBCreate( cDBF , AFields)
? DBUseArea(,"DBFNTX",cDBF )
? DBCreateIndex(cDbf , "ID")

DBGoTop()
? RecNo(), FieldGet(1)

? DBRLock()
? DBDelete()
? DBUnlock()

DBCloseAll()
RETURN

FUNCTION testUnique2() AS VOID
LOCAL cDBF, cPfad, cIndex, cDriver AS STRING
LOCAL aFields, aValues AS ARRAY
LOCAL i AS DWORD
LOCAL lUnique AS LOGIC
cDriver := RDDSetDefault ( "DBFCDX" )
lUnique := SetUnique()

aFields := { { "LAST" , "C" , 20 , 0 }}
aValues := { "a" , "d" , "f", "c" }

cPfad := "C:\test\"
cDBF := cPfad + "Foo"
cIndex := cPfad + "Foox"

FErase ( cIndex + IndexExt() )
// -----------------
? DBCreate( cDBF , AFields)
? DBUseArea(,"DBFCDX",cDBF )
FOR i := 1 UPTO ALen ( aValues )
DBAppend()
FieldPut ( 1 , aValues [ i ] )
NEXT


OrdCondSet()
OrdCreate(cIndex, "ORDER1", "upper(LAST)", { || Upper ( _FIELD-> LAST) } )
DBSetOrder ( 1 )
? OrdName() // "ORDER1"
? "OrdIsUnique() unique", OrdIsUnique() // always returns true !
? "DBOrderinfo unique", DBOrderInfo(DBOI_UNIQUE ) // ok
? "desc" , DBOrderInfo(DBOI_ISDESC ) // ok
?

OrdCondSet()
// create a descend and unique order
? OrdCondSet(,,,,,,,,,,TRUE)
SetUnique ( TRUE )
OrdCreate(cIndex, "ORDER2", "upper(LAST)", { || Upper ( _FIELD-> LAST) } )

DBSetOrder ( 2 )
? OrdName() // "ORDER2"
? "OrdIsUnique() unique", OrdIsUnique() // always returns true !
? "DBOrderinfo unique", DBOrderInfo(DBOI_UNIQUE ) // ok
? "desc" , DBOrderInfo(DBOI_ISDESC ) // ok
DBCloseAll()
RDDSetDefault ( cDriver )
SetUnique ( lUnique )
RETURN 
DEFINE LANG_GERMAN := 0x07
DEFINE SUBLANG_GERMAN := 0x01 // German
DEFINE SORT_DEFAULT := 0x0 // sorting default
DEFINE SORT_GERMAN_PHONE_BOOK := 0x1 // German Phone Book order
    FUNCTION TestCrypt() AS VOID
        LOCAL cRes AS STRING
	    LOCAL pRes AS BYTE PTR 
	    ? cRes := Crypt("abc", "def")
	    pRes := StringAlloc(cRes)
	    ? pRes[1], pRes[2], pRes[3]
	    MemFree(pRes)
        cRes := Crypt(cRes, "def")
        ? cRes
	    WAIT
        RETURN
	FUNCTION testLb() AS VOID STRICT
        LOCAL x AS TestLbClass
        LOCAL o AS OBJECT
        o := CreateInstance("TestLbClass2")
        x := TestLbClass{}
        o := x
        TRY
            o:ThisPropertyDoesNotExist := Today()
            CATCH Ex AS Error
            LOCAL y AS Error
            y := Ex
            ? y:ToString()
        END TRY
        
    RETURN      


    CLASS TestLbClass 
        
        PUBLIC CONSTRUCTOR() 
        
        RETURN 
        
                 
        
    END CLASS 

FUNCTION TestCopyStruct() AS VOID
LOCAL cDBF, cPfad, cCopyStructTo AS STRING
LOCAL aFields AS ARRAY

aFields := { { "LAST" , "C" , 512 , 0 },;
           { "AGE" , "N" , 3 , 0 } ,;
           { "SALARY" , "N" , 10 , 2 },;
           { "HIREDATE" , "D" , 8 , 0 },;
           { "MARRIED" , "L" , 1 , 0 }}

cPfad := ""
cDBF := cPfad + "Foo"
cCopyStructTo := "D:\test\foonew"

? DBCreate( cDBF , AFields)
? DBUseArea(,"DBFNTX",cDBF )

// "The name 'DBCopyStruct' does not exist in the current context"
// ? DBCopyStruct( cCopyStructTo )

? DBCopyStruct(cCopyStructTo, { "last" , "age" , "married" } )   // ok
DBCloseAll()
RETURN 

FUNCTION TestDbf() AS VOID
LOCAL cDBF, cPfad AS STRING
LOCAL aFields AS ARRAY

// DBF() throws an EG_NOTABLE error, but should return "" if no table is
// opened in the current workarea

? DBF() // exception

aFields := { { "LAST" , "C" , 10 , 0 }}
cPfad := ""
cDBF := cPfad + "Foo"
            // -----------------
? DBCreate( cDBF , AFields)
? DBUseArea(,"DBFNTX",cDBF , "FOOALIAS")

? DBF() // Returns the fullpath instead of the alias name

DBCloseAll()

? DBF() // should return empty string
RETURN 


FUNCTION TestUnique() AS VOID
LOCAL cDbf AS STRING
LOCAL aFields AS ARRAY
LOCAL nMax := 1000 AS LONG
LOCAL nCnt AS LONG
cDBF := "Foo"
aFields := {{ "NAME" , "C" , 10 , 0 }}
DBCreate( cDBF , aFields,"DBFCDX")
DBCloseAll()
DBUseArea( TRUE ,"DBFCDX",cDBF,"Foo",FALSE)
FOR VAR i := 1 TO nMax
    DbAppend()
    FieldPut(1, str(i,10,0))
NEXT
DbCreateIndex("Foo","Name",,TRUE)
DbGoTop()
DO WHILE ! EOF()
    ? Recno(), FieldGet(1)
    DbSkip(1)
ENDDO
WAIT
FOR VAR i := 1 TO nMax
    DbGoto(i)
    FieldPut(1, "AAAAAAAAAA")
NEXT
DbGoTop()
nCnt := 0
DO WHILE ! EOF()
    ? Recno(), FieldGet(1)
    DbSkip(1)
    nCnt++
ENDDO
? nCnt, "records"
WAIT
FOR VAR i := 1 TO nMax
    DbGoto(i)
    FieldPut(1, str(i,10,0))
NEXT
DbGoTop()
nCnt := 0
DO WHILE ! EOF()
    ? Recno(), FieldGet(1)
    DbSkip(1)
    nCnt++
ENDDO
? nCnt, "records"

DbCloseAll()
RETURN




FUNCTION WaTest() AS VOID
LOCAL cDBF AS STRING
LOCAL aFields AS ARRAY
LOCAL dwStartWA, dwMaxWA AS DWORD

RDDSetDefault ( "DBFNTX" )

cDBF := "Foo"
aFields := {{ "AGE" , "N" , 2 , 0 }}
DBCreate( cDBF , aFields)
DBCloseAll()

DBUseArea( TRUE ,"DBFNTX",cDBF,"FOO1",TRUE)
DBUseArea( TRUE ,"DBFNTX",cDBF,"FOO2",TRUE)

dwStartWA := DBGetSelect() // 2

? DBSetSelect (0 ) // 3 ok, shows the next unused workarea
? SELECT (0) // 3 ok, shows the next unused workarea
?
// ok, -1 returns the max workarea
dwMaxWA := DBSetSelect ( -1 ) // x# 4096 , VO 1023
? dwMaxWA

// if a workArea ( dwMaxWa + 1 ) is selected
// X# should return 0 as VO does, currently X# throws an argument exception instead
//
? DBSetSelect ( dwMaxWA + 1 ) // X# throws a exception, VO returns 0
? SELECT ( dwMaxWA + 1 ) // X# throws a exception, VO returns 0
?
// each param -1, e.g. -4, should be treated like a param 0, that shows the next unused workarea.
// in X# currently something like -4 returns (0xffffffff -4 ) + 1
? DBSetSelect ( - 4 ) // X# 4294967292 , VO 3
? SELECT ( - 4 ) // X# 4294967292 , VO 3
?
// switch back to the workarea 2
DBSetSelect ( dwStartWA )
?
? "DbSymSelect results"
?
? DBSymSelect() // 2 , ok - current work area

? DBSymSelect( #Foo2 ) // ok , 2
//
// VO throws an DataType Error if a param != symbol is used
//
// e.g. VO throws an Data Type Error type: longint requested type: symbol
? DBSymSelect( 1 ) // X# doesn´t throw a exceptions but returns 0

// e.g. VO throws an Data Type Error. type: string requested type: symbol
? DBSymSelect( "Foo1" ) // X# doesn´t throw a exceptions but returns 1

// e.g. VO throws an Data Type Error . type: codeblock requested type: symbol
? DBSymSelect( {|| } ) // X# doesn´t throw a exceptions but returns 0

DBCloseAll()
RETURN

FUNCTION BigDbf AS VOID
LOCAL aFields AS ARRAY
LOCAL cDBF, cPath AS STRING

RDDSetDefault ( "DBFNTX" )

aFields := {{ "LAST" , "C" , 511 , 0 } ,{ "AGE" , "N" , 3 , 0 }}
cPath := "c:\test\"

cDBF := cPath + "foo"

? DBCreate( cDBF , AFields , "DBFNTX" ) // true
? DBUseArea(,"DBFNTX",cDBF,,FALSE) // true
DbAppend()
// NOTE: the size of the "LAST" field is 255 and not as expected 511
// DBUSEAREA() doesn ´t crash
DBCloseAll()
// ------------------------------------------
cDBF := cPath + "foo2"
// Note: the size of the "LAST" field should be now 2048
aFields := {{ "LAST" , "C" , 2048 , 0 } , { "AGE" , "N" , 3 , 0 }}

? DBCreate( cDBF , AFields , "DBFNTX") // true

? DBUseArea(,"DBFNTX",cDBF,,FALSE) // exception
DbAppend()
// NOTE: the size of the "LAST" field shown is 0 and not 2048 !
// so DBUSEAREA crashes.

DBCloseAll()
RETURN

FUNCTION TestClearOrderScope AS VOID
    LOCAL cDbf AS STRING
    LOCAL aValues AS ARRAY
    LOCAL i AS DWORD
    
    cDBF := "c:\test\tmp1"

    RDDSetDefault("DBFNTX")

    aValues := { "vvv" , "abb", "acb" , "aaa"  , "bbb" }
    DBCreate( cDBF , {{"LAST" , "C" ,10 , 0 } })
    DBUseArea(,"DBFNTX",cDBF,,FALSE)
    FOR i := 1 UPTO ALen ( aValues )
        DBAppend()
        FieldPut(1,aValues [i])
    NEXT
    DBCloseArea()

    DBUseArea(,"DBFNTX",cDBF,,TRUE)
    DBCreateIndex(cDbf, "Upper(LAST)" )
    ? DBOrderInfo( DBOI_KEYCOUNT ) // 5 ok

    LOCAL u AS USUAL
    u := "A"
    ? VODBOrderInfo( DBOI_SCOPETOP, "", NIL, REF u )
    ? VODBOrderInfo( DBOI_SCOPEBOTTOM, "", NIL, REF u )
    ? DBOrderInfo( DBOI_KEYCOUNT ) // 3 ok

    
    // clear scope
    u := NIL // it is called in the SDK this way
    ? VODBOrderInfo( DBOI_SCOPETOPCLEAR, "", NIL, REF u )
    ? VODBOrderInfo( DBOI_SCOPEBOTTOMCLEAR, "", NIL, REF u )
    ? DBOrderInfo( DBOI_KEYCOUNT ) // 3 again, should be 5
    ?
    DBGoTop()
    // not all records are shown
    DO WHILE ! EOF()
        ? FieldGet(1)
        DBSkip(1)
    ENDDO

    DBCloseArea()
RETURN 

FUNCTION TestReplace() AS VOID
LOCAL cDBF AS STRING
LOCAL f AS FLOAT
LOCAL nMax := 50000 AS LONG
cDBF := "c:\test\TestRepl"
FErase(cDbf+".dbf")
FErase(cDbf+".cdx")
DBCreate(cDbf , {{"FIELDC","C",10,0},{"FIELDL","L",1,0}})
DBUseArea(,"DBFCDX",cDBF)

FOR VAR i := 1 TO nMax
    DbAppend()
    FieldPut(1, Str(i,10))
    FieldPut(2, TRUE)
NEXT
DBSETORDERCONDITION("FIELDL",{||_FIELD->FIELDL})
f := Seconds()
DbCreateIndex(cDbf, "FIELDC",,FALSE)
? Seconds() - f
? DbOrderInfo(DBOI_KEYCOUNT)
f := Seconds()
FOR VAR i := 1 TO nMax
    DbGoto(i)
    FieldPut(2, FALSE)
NEXT
? "update 1", Seconds() - f
//
DbSetOrder(1)
? DbOrderInfo(DBOI_KEYCOUNT)
? DbOrderInfo(DBOI_USER+42)
f := Seconds()

FOR VAR i := 1 TO nMax
    DbGoto(i)
    FieldPut(2, TRUE)
NEXT
? "update 2", Seconds() - f

? DbOrderInfo(DBOI_KEYCOUNT)


DbCloseArea()
RETURN


FUNCTION TestReplaceTag() AS VOID
LOCAL cDBF AS STRING
cDBF := "c:\test\Test10k"
RddSetDefault("DBFCDX")
DBUSEAREA(TRUE,"DBFCDX",cDBF,"Test",FALSE,FALSE)
DbCreateOrder("Salary",, "Salary")
DbCreateOrder("Age",, "AGE")
? DbOrderInfo(DBOI_ORDERCOUNT)
WAIT
DbCloseArea()
DBUSEAREA(TRUE,"DBFCDX",cDBF,"Test",FALSE,FALSE)
? DbOrderInfo(DBOI_ORDERCOUNT)
OrdDestroy("Age")
DbCreateOrder("Age",, "AGE")
WAIT
? DbOrderInfo(DBOI_ORDERCOUNT)
DbCLoseArea()
RETURN

FUNCTION TestCdxCreateConditional() AS VOID
LOCAL cDBF AS STRING
LOCAL f AS FLOAT
f := Seconds()
? "Creating tags"
cDBF := "c:\test\Test10k"
RddSetDefault("DBFCDX")
DBUSEAREA(TRUE,"DBFCDX",cDBF,"Test",FALSE,FALSE)
// Descending
? "Age"
DBSETORDERCONDITION("State='MT'",{||_FIELD->STATE='MT'},,,{|e|Qout(Recno()),TRUE},1000)
DBCREATEINDEX("Age", "AGE")         
? DbOrderInfo(DBOI_KEYCOUNT)
? "AgeDesc"
DBSETORDERCONDITION(,,,,,,,,,,TRUE)
DBCREATEINDEX("AgeDesc", "AGE")
? DbOrderInfo(DBOI_KEYCOUNT)
DBSETORDERCONDITION(,,FALSE,,,,1,10)
DBCREATEINDEX("Age10", "AGE")
DBSETORDERCONDITION("Age>40",,,,,,,,,,TRUE)
DBCREATEINDEX("STATE", "STATE")

? DbOrderInfo(DBOI_KEYCOUNT)
DBCLOSEALL()
RETURN

FUNCTION TestGermanSeek() AS VOID
LOCAL cDBF AS STRING
cDbf := "C:\Test\TESTDBF"
RDDSetDefault("DBFNTX")

SetCollation(#WINDOWS)
//SetAppLocaleId(MAKELCID(MAKELANGID(LANG_GERMAN, SUBLANG_GERMAN),SORT_GERMAN_PHONE_BOOK)) // Telefonbuch
SetAppLocaleID(MAKELCID(MAKELANGID(LANG_GERMAN, SUBLANG_GERMAN),SORT_DEFAULT)) // Wörterbuch

DBCreate(cDbf , {{"LAST","C",10,0}})
DBUseArea(,,cDBF)
DBAppend()
FieldPut(1 , "Ärger")
DBAppend()
FieldPut(1 , "Aaaaa")
DBCloseArea()


DBUseArea(,,cDBF)
DBCreateIndex(cDbf , "LAST")
? DBSeek("Ä") // FALSE in X#, TRUE in VO
? Recno(), Found(), FieldGet(1)
? DBSeek("Ärger") // TRUE in both
? Recno(), Found(), FieldGet(1)
DBGoTop()
DO WHILE .NOT. eof()
? Recno(), FieldGet(1)
DBSkip()
END DO
DBCloseArea()
RETURN
FUNCTION TestDbApp() AS VOID
LOCAL aFields, aValues AS ARRAY
LOCAL cDbf, cPath AS STRING
LOCAL i AS DWORD

cPath := "c:\test\"
// --------- create From.dbf ----------
cDbf := cPath + "from.dbf"

aFields := {{ "GRUPPE" , "C" , 30 , 0 } ,;
{ "ID" , "C" , 5 , 0 } }

aValues := { { "Grp1" , "00001" } ,;
{ "Grp2" , "00002" } }

? DBCreate( cDbf , aFields , "DBFNTX" )
? DBUseArea(,"DBFNTX",cDbf,,FALSE)
FOR VAR j :=1 TO 100000
FOR i := 1 UPTO ALen ( aValues )
DBAppend()
FieldPut ( 1 , aValues [ i , 1 ] )
FieldPut ( 2 , aValues [ i , 2 ] )
NEXT
NEXT
// --------- create To.dbf ----------
cDbf := cPath + "To.dbf"

// error happens also without that
AAdd ( aFields , { "ID2", "N" , 5 , 0 } )

? DBCreate( cDbf , aFields , "DBFNTX" )
DBCloseAll()
// ---- append the From.dbf content to the To.dbf ----
cDbf := cPath + "To.dbf"
? DBUseArea(,"DBFNTX",cDbf,,FALSE)
VAR f := seconds()
? DBApp ( cPath + "From.dbf" ) // ------- IndexOutofRangeException
? seconds() - f
DBCloseAll()
RETURN
FUNCTION TestCloseArea() AS VOID
LOCAL aFields AS ARRAY
LOCAL cDbf, cPath AS STRING


cPath := ""
cDbf := "Foo.dbf"

// ----------------


cDbf := cPath + cDBF

aFields := {{ "LAST" , "C" , 10 , 0 } }


? DBCreate( cDbf , aFields , "DBFNTX" )

?
? DBUseArea( TRUE ,"DBFNTX",cDbf, "FOO1", TRUE)
? DBGetSelect() // 1 ok

?
? DBUseArea( TRUE ,"DBFNTX",cDbf, "FOO2", TRUE)
? DBGetSelect() // 2 ok

?
? DBUseArea( TRUE ,"DBFNTX",cDbf, "FOO3", TRUE)
? DBGetSelect() // 3 ok

?
? DBUseArea( TRUE ,"DBFNTX",cDbf, "FOO4", TRUE)
? DBGetSelect() // 4 ok
?
? DBSetSelect ( 3 ) // 3 ok
?
?
? DBCloseAll()
? DBGetSelect() // Shows 3 instead of 1
? SELECT() // Shows 3 instead of 1

?

? DBUseArea( ,"DBFNTX",cDbf, "FOO1", TRUE)
? DBGetSelect() // shows 3 instead of 1

? DBUseArea( TRUE ,"DBFNTX",cDbf, "FOO2", TRUE)
? DBGetSelect() // shows 1 instead of 2

RETURN
FUNCTION TestChris() AS VOID
LOCAL aValues AS ARRAY
	LOCAL i AS DWORD
	LOCAL cDBF AS STRING
	LOCAL cCdx AS STRING

	RddSetDefault("DBFCDX")

	aValues := { 44 , 12, 34 , 21 }                                
	cDBF := "testcdx"
	cCdx := cDBF + ".cdx"
	FErase(cCdx)
	DBCREATE( cDBF , {{"AGE" , "N" , 3 , 0 } })                                        
	DBUSEAREA(,"DBFCDX",cDBF,,FALSE)
	? DbOrderInfo( DBOI_KEYCOUNT ) 
	FOR i := 1 UPTO ALen ( aValues )
		DBAPPEND()
		FIELDPUT(1,aValues [i]) 
	NEXT
	? DbOrderInfo( DBOI_KEYCOUNT  ) 
	? DbOrderInfo( DBOI_NUMBER )    


	DBCREATEINDEX( cCdx, "age" )
	DBCLOSEAREA()
	
	DBUSEAREA(,"DBFCDX",cDBF,,FALSE)
	DbSetOrder(1)
	
	DbGotop()
	DO WHILE ! EOF()
		DBSKIP(1)
	ENDDO

	? DBCLEARINDEX( )
    ? DbOrderInfo( DBOI_ORDERCOUNT )

	DbGotop()
	? RECNO()

	?  DbSetIndex ( cCdx ) 

	? DbOrderInfo( DBOI_KEYCOUNT )  // 4, ok
	? DbOrderInfo( DBOI_NUMBER )  // still  -1 , but should show  1
	?  DbOrderInfo( DBOI_NAME )   // ok , "TESTDBF"
	DBCLOSEAREA ()
DBCloseArea()
RETURN
FUNCTION TestCdxCreate() AS VOID
LOCAL cDBF AS STRING
LOCAL cIndex AS STRING
LOCAL aTags AS ARRAY
LOCAL aClone AS ARRAY
LOCAL i AS DWORD
LOCAL f AS FLOAT
f := Seconds()
? "Creating tags"
cDBF := "c:\test\Test10k"
RddSetDefault("DBFCDX")
aTags := {"AGE","CITY","FIRST","HIREDATE", "LAST","SALARY","STATE"}
FOR i := 1 TO ALen(aTags)
	VODBUSEAREA(TRUE,"DBFCDX",cDBF,"Test",FALSE,FALSE)
	aClone := AClone(aTags)
	cIndex := cDBF+NTrim(i)
    Ferase(cIndex+".CDX")
	ASize(aClone, i)
	AEval(aClone , {|cTag|  OrdCreate(cIndex, cTag, cTag) })
	DBCLOSEAREA()
NEXT
VODBUSEAREA(TRUE,"DBFCDX",cDBF,"Test",FALSE,FALSE)
AEval(aTags, {|cTag| OrdCreate(cDbf, cTag, cTag)})
? Seconds() - f
FOREACH cTag AS STRING IN aTags
    ? "Dumping tag ",cTag
    DbSetOrder(cTag)
    DbOrderInfo(DBOI_USER+42)
NEXT

RETURN


FUNCTION TestCdxCreate1() AS VOID
LOCAL cDbf AS STRING
LOCAL i AS LONG
cDbf := "c:\test\TestCreate"
RddSetDefault("DBFCDX")
DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
DBUseArea(,"DBFCDX",cDbf)
FOR i := 1 TO 100
DbAppend()
FieldPut(1, "AAA")
DbAppend()
FieldPut(1, "ABC")
NEXT
OrdCreate(cDBF, "CFIELD" ,"CFIELD")
DBCloseArea()
RETURN

FUNCTION TestDescend() AS VOID
LOCAL cDbf AS STRING
cDbf := "c:\test\Names"
DBUseArea(,"DBFCDX",cDbf)
DbSeek("Chris")
DO WHILE .NOT. EOF()
    ? Recno(), FieldGet(1) // should show 4,3
    DBSkip()
END DO
DBCloseArea()
RETURN

FUNCTION CloseCdx AS VOID
	LOCAL cDbf AS STRING
	LOCAL cCdx AS STRING
    RDDSetDefault("DBFCDX")
	cDbf := "C:\test\testcdx"
	cCdx := cDbf + ".cdx"
    FErase(cCdx)
			
	DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
	DBUseArea(,,cDbf,,FALSE)
	DBAppend()
	FieldPut ( 1 , "B")
	DBAppend()
	FieldPut ( 1 , "A")
	DBCloseArea()
			
	DBUseArea(,,cDbf,,TRUE)
	System.IO.File.WriteAllBytes(cCdx , Convert.FromBase64String("AAQAAAAAAAAAAAAACgDgAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQABAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAQD//////////94B//8AAA8PEAQEAwAGMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABURVNUQ0RYAAoAAAAAAAAAAAAACgBgAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABwABAAAABwBDRklFTEQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAgD//////////+AB//8AAA8PEAQEAwIAkAEAkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJB"))
    DBCloseArea()
			
	DBUseArea(,,cDbf,,FALSE)
	//DBSetIndex(cCdx)
    DBSetOrder(1)
	DBGoTop()
	DBCloseArea()

	? DBUseArea(,,cDbf,,FALSE)
	DBCloseArea()
			
			
    ? FErase(cDbf + ".dbf")
    ? FErase(cCdx)

RETURN


    
FUNCTION Start1() AS VOID
LOCAL cDbf AS STRING
cDBF := "testdbf"

DBCreate( cDBF , {{"FIELDN" , "N" ,5 , 0 } } )
DBUseArea(,"DBFNTX",cDBF)
DBAppend()
FieldPut(1,3)
DBAppend()
FieldPut(1,1)
DBAppend()
FieldPut(1,4)
DBAppend()
FieldPut(1,2)

DBSetOrderCondition(,,,,,,,,,,TRUE)
DBCreateIndex(cDbf, "FIELDN" )

DBOrderInfo( DBOI_SCOPETOP, "", NIL, 3 )
DBOrderInfo( DBOI_SCOPEBOTTOM, "", NIL, 2 )

// prints 3,2,1. If the order of scopes is
// changed to 2->3, then it prints 2,1
DBGoTop()
DO WHILE .NOT. EoF()
? FieldGet(1)
DBSkip()
END DO
DBCloseArea()
RETURN
    FUNCTION TestCdxNumSeek AS VOID
    ? VoDBUSEAREA(TRUE, typeof(DBFCDX), "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF", "TEST",TRUE,TRUE)
    DbSetOrder("Age")
    FOR VAR i := 20 TO 90
        IF DbSeek(i,TRUE,FALSE)
            ? FieldGetSym(#Age), str(Recno(),4), FieldGetSym(#First), FieldGetSym(#Last)
        ENDIF
        IF DbSeek(i,TRUE,TRUE)
            ? FieldGetSym(#Age), str(Recno(),4), FieldGetSym(#First), FieldGetSym(#Last)
        ENDIF
    NEXT
    RETURN
    FUNCTION TestCdxDateSeek AS VOID
    ? VoDBUSEAREA(TRUE, typeof(DBFCDX), "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF", "TEST",TRUE,TRUE)
    DbSetOrder("Hiredate")
    ? DbSeek(1989.06.15,TRUE,FALSE)
    ? Recno(), FieldGetSym(#First), FieldGetSym(#Last), FieldGetSym(#HireDate)
    RETURN
    FUNCTION TestCdxCount() AS VOID
    LOCAL aTags AS ARRAY
    LOCAL f AS FLOAT
    f := seconds()

    ? VoDbUseArea(TRUE, TYPEOF(DBFCDX), "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF", "TEST",TRUE,TRUE)
    aTags := {"AGE", "CITY", "STATE", "FIRST","LAST","HIREDATE","SALARY"} 
    FOR VAR i := 1 TO  alen(aTags)
        DbSetOrder(aTags[i])
        ? aTags[i], DbOrderInfo(DBOI_KeyCount)
    NEXT
    ? Seconds() - f
    RETURN    

FUNCTION TestCdxSeek() AS VOID
    //LOCAL aTags AS ARRAY
    ? VoDbUseArea(TRUE, TYPEOF(DBFCDX), "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF", "TEST",TRUE,TRUE)
    //aTags := {"AGE", "CITY", "STATE", "FIRST","LAST","HIREDATE","SALARY"} 
    //FOR VAR i := 1 TO  alen(aTags)
    VAR nCount := 0
    /*
    ? DbSetOrder("LAST")
    ? "Seek Peters"
    ? DbSeek("Peters",TRUE, FALSE), Found(), Eof()
    DO WHILE ! EOF() .AND. FieldGetSym(#Last) = "Peters"
        ? Recno(), FieldGetSym(#First), FieldGetSym(#Last)
        ++nCount
        VoDbSkip(1)
    ENDDO

    ? nCount
    ? DbSeek("Peters",TRUE, TRUE), Found(), Eof()
    nCount := 0
    DO WHILE ! BOF() .AND. FieldGetSym(#Last) = "Peters"
        ? Recno(), FieldGetSym(#First), FieldGetSym(#Last)
        ++nCount
        VoDbSkip(-1)
    ENDDO
    ? nCount
    wait

    ? "Seek State MI"
    ? DbSetOrder("STATE")
    nCount := 0
    ? DbSeek("MI",TRUE, FALSE), Found(), Eof()
    DO WHILE ! EOF() .AND. FieldGetSym(#State) == "MI"
        ? Recno(), FieldGetSym(#First), FieldGetSym(#Last), FieldGetSym(#State)
        ++nCount
        VoDbSkip(1)
    ENDDO
    ? nCount
    wait
    ? "Seek last State MI"
    ? DbSeek("MI",TRUE, TRUE), Found(), Eof()
    nCount := 0
    DO WHILE ! BOF() .AND. FieldGetSym(#State) = "MI"
        ? Recno(), FieldGetSym(#First), FieldGetSym(#Last), FieldGetSym(#State)
        ++nCount
        VoDbSkip(-1)
    ENDDO
    ? nCount
    */
    ? "Seek First Yao"
    ? DbSetOrder("FIRST")
    nCount := 0
    //? DbSeek("Yao",TRUE, FALSE), Found(), Eof()
    DbGoto(81)
    DO WHILE ! EOF() .AND. FieldGetSym(#First) = "Yao"
        ? Recno(), FieldGetSym(#First), FieldGetSym(#Last)
        ++nCount
        VoDbSkip(1)
    ENDDO
    ? nCount
    WAIT
    ? "Seek last Yao"
    ? DbSeek("Yao",TRUE, TRUE), Found(), Eof()
    nCount := 0
    DO WHILE ! BOF() .AND. FieldGetSym(#First) = "Yao"
        ? Recno(), FieldGetSym(#First), FieldGetSym(#Last)
        ++nCount
        VoDbSkip(-1)
    ENDDO

    ? nCount
    VoDbCLoseArea()

    RETURN
FUNCTION TestCdxForward() AS VOID
    LOCAL f AS FLOAT
    ? VoDbUseArea(TRUE, TYPEOF(DBFCDX), "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF", "TEST",TRUE,TRUE)
    LOCAL aTags AS ARRAY
    f := seconds()
    aTags := {"AGE", "CITY", "STATE", "FIRST","LAST","HIREDATE","SALARY"}
    ? " Count skip +1"
    FOR VAR i := 1 TO  alen(aTags)
        DbSetOrder(aTags[i])
        ? aTags[i]
        //DbOrderInfo(DBOI_USER+42,,aTags[i])
        VoDbGoTop()
        //? Recno(), FieldGetSym(#First), FieldGetSym(#Last), FieldGetSym(AsSymbol(aTags[i]))
        VAR nCount := 0
        DO WHILE ! VoDbEof()
            nCount++
            LOCAL u := NIL AS USUAL
            VoDbFIeldGet(1, REF u)
            VoDbSkip(1)
        ENDDO
        //DbGoBottom()
        //? Recno(), FieldGetSym(#First), FieldGetSym(#Last), FieldGetSym(AsSymbol(aTags[i]))
        ? nCount
     
    NEXT
    ? Seconds() - f
    DbCloseArea()
    RETURN
FUNCTION TestCdxBackward() AS VOID
    LOCAL f AS FLOAT
    ? VoDbUseArea(TRUE, TYPEOF(DBFCDX), "c:\XSharp\DevRt\Runtime\XSharp.Rdd.Tests\dbfs\TEST10K.DBF", "TEST",TRUE,TRUE)
    LOCAL aTags AS ARRAY
    f := seconds()
    aTags := {"AGE", "CITY", "STATE", "FIRST","LAST","HIREDATE","SALARY"}
    ? " Count skip -1"
    FOR VAR i := 1 TO  alen(aTags)
        DbSetOrder(aTags[i])
        ? aTags[i]
        //DbOrderInfo(DBOI_USER+42,,aTags[i])
        VoDbGoBottom()
        ? Recno(), FieldGetSym(#First), FieldGetSym(#Last), FieldGetSym(AsSymbol(aTags[i]))
        VAR nCount := 0
        DO WHILE ! VoDbBof()
            nCount++
            VoDbSkip(-1)
        ENDDO
        ? Recno(), FieldGetSym(#First), FieldGetSym(#Last), FieldGetSym(AsSymbol(aTags[i]))

        ? nCount
     
    NEXT
    DbCloseArea()
    ? Seconds() - f
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

