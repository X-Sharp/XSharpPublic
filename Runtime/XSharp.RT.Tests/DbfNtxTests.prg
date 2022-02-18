//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit
USING System.Globalization



BEGIN NAMESPACE XSharp.RT.Tests

    CLASS DbfNtxTests

        // TECH-9JPUGAOV3L , NTX problem with EoF after sequence of commands
        [Fact, Trait("Category", "DBF")];
            METHOD Ntx_Eof_test() AS VOID
            LOCAL cDbf AS STRING
            LOCAL cNtx AS STRING

            RddSetDefault("DBFNTX")

            cDbf := __FUNCTION__
            cNtx := cDbf + ".ntx"

            DbCreate( cDbf , {{"NFIELD" , "N" , 5 , 0 }})
            DbUseArea(,,cDbf,,FALSE)
            DbAppend()
            FieldPut(1,123)
            DbAppend()
            FieldPut(1,456)
            DbCreateIndex(cNtx, "NFIELD")
            DbCloseArea()


            // necessary sequence to reproduce the problem below
            DbUseArea(,,cDbf,,FALSE)
            DbSetIndex(cNtx)
            DbGoTop()
            Assert.Equal(1, (INT)RecNo()) // 1
            DbGoBottom()
            Assert.Equal(2, (INT)RecNo()) // 2
            DbSkip(-1)
            Assert.Equal(1, (INT)RecNo()) // 1
            DbGoto(2)
            Assert.Equal(2, (INT)RecNo()) // 2


            DbSkip(+1)
            Assert.Equal(TRUE, (LOGIC)Eof()) // FALSE, wrong
            Assert.Equal(3, (INT)RecNo()) // 2, wrong

            DbSkip(+1)
            Assert.Equal(TRUE, (LOGIC)Eof()) // TRUE
            Assert.Equal(3, (INT)RecNo()) // 3

            DbCloseArea()
        RETURN


        // TECH-546935N337, DBOrderInfo(DBOI_SETCODEBLOCK) causes an invalid cast exception, but only if a index is opened
        [Fact, Trait("Category", "DBF")];
            METHOD DBOrderInfo_DBOI_SETCODEBLOCK() AS VOID
            LOCAL cDBF, cNTX AS STRING

            RddSetDefault("DBFNTX")

            cDbf := __FUNCTION__
            cNTX := cDBF + ".ntx"

            DbCreate( cDBF , {{"ID" , "C" , 5 , 0 }})
            DbUseArea(,,cDBF)

            DbAppend()
            FieldPut(1, "one")

            DbCreateIndex(cNTX , "Upper (ID)")
            DbCloseArea()

            DbUseArea(,,cDBF)
            DbSetIndex(cNTX)

            ? "DBOI_SETCODEBLOCK" , DbOrderInfo( DBOI_SETCODEBLOCK )

            DbCloseArea()
        RETURN


        // TECH-3NY78C93EK, OrdScope() not working
        [Fact, Trait("Category", "DBF")];
            METHOD OrdScope_test() AS VOID
            LOCAL cDbf AS STRING
            cDbf := __FUNCTION__
            DbCreate( cDbf , {{"NFIELD" , "N" , 5 , 0 }})
            DbUseArea(,"DBFNTX",cDbf)
            FOR LOCAL n := 1 AS INT UPTO 20
                DbAppend()
                FieldPut(1,n)
            NEXT
            DbCreateIndex(cDbf , "NFIELD")
            DbCloseArea()

            DbUseArea(,"DBFNTX",cDbf) // 20 records
            DbSetIndex ( cDbf )
            Assert.Equal( (INT) DbOrderInfo( DBOI_KEYCOUNT ) , 20)
            Assert.Equal( (INT) OrdScope(TOPSCOPE, 5) , 0) // returns the previous value
            Assert.Equal( (INT) OrdScope(BOTTOMSCOPE, 10) , 0) // returns the previous value
            DbGoTop()

            Assert.Equal( (INT) DbOrderInfo( DBOI_KEYCOUNT ) , 6) // still 20 - but must be 6
            LOCAL nCount := 0 AS INT
            DO WHILE ! Eof() // all 20 records are listed
                ? FieldGet ( 1 )
                DbSkip ( 1 )
                nCount ++
            ENDDO
            Assert.Equal( 6 , nCount)
            Assert.Equal( 5 , (INT) DbOrderInfo( DBOI_SCOPETOP ) ) // {(0x0000)0x00000000} CLASS
            Assert.Equal( 10 , (INT) DbOrderInfo( DBOI_SCOPEBOTTOM ) ) // {(0x0000)0x00000000} CLASS
            DbCloseArea()
        RETURN

        [Fact, Trait("Category", "DBF")];
            METHOD OrdScope_test_with_Ordinal_Collation() AS VOID
            LOCAL cDbf AS STRING
            cDbf := __FUNCTION__

            LOCAL uCollation AS USUAL
            uCollation := SetCollation(#ORDINAL)

            DbCreate( cDbf , {{"NFIELD" , "N" , 5 , 0 }})
            DbUseArea(,"DBFNTX",cDbf)
            FOR LOCAL n := 1 AS INT UPTO 20
                DbAppend()
                FieldPut(1,n)
            NEXT
            DbCreateIndex(cDbf , "NFIELD")
            DbCloseArea()

            DbUseArea(,"DBFNTX",cDbf)
            DbSetIndex ( cDbf )
            Assert.Equal( (INT) DbOrderInfo( DBOI_KEYCOUNT ) , 20)
            Assert.Equal( (INT) UsualType( DbOrderInfo( DBOI_KEYCOUNT )  ) , 1) // first time returns 6
            Assert.Equal( (INT) UsualType( DbOrderInfo( DBOI_KEYCOUNT )  ) , 1) // second time it returns 1 correctly
            DbCloseArea()

            SetCollation(uCollation)
        RETURN


        // TECH-9TW65Q3XQE, NTX corruption with updating multiple fields and shared mode
        [Fact, Trait("Category", "DBF")];
            METHOD NTX_test() AS VOID
            LOCAL aValues AS ARRAY
            LOCAL i AS DWORD
            LOCAL cDBF, cNTX AS STRING

            RddSetDefault("DBFNTX")

            aValues := { "ssss" , "hhhh", "wwww" , "aaaa" }
            cDbf := __FUNCTION__
            cNTX := cDbf
            DbCreate( cDBF , {{"LAST" , "C" , 20 , 0 } , ;
            {"TEXT1" , "C" , 10 , 0 } , ;
            {"NUM1" , "N" , 10 , 2 }})

            DbUseArea(,,cDBF,,FALSE)
            FOR i := 1 UPTO ALen ( aValues )
                DbAppend()
                FieldPut(1,aValues [i])
            NEXT
            DbCreateIndex(cNTX, "Upper ( Last)")
            DbCloseArea()

            DbUseArea(,,cDBF,,TRUE) // open shared !
            DbSetIndex(cNTX)
            DbGoTop()
            ? "current (indexed) order"
            DO WHILE ! Eof()
                ? FieldGet ( 1 )
                DbSkip ( 1 )
            ENDDO
            DbGoTop()
            // "now replace the index field #LAST content 'aaaa' with 'pppp'"
            // "and also update another field"
            Assert.True( DbRLock ( RecNo() )  )
            // "Replacing", AllTrim(FieldGet(1)), "with 'pppp'"
            FieldPut ( 1 , "pppp" ) // Note: This is the index field

            //	 this is what causes the problem, updating a second field
            //	 if this fieldput is put above the first one, then sample works correctly
            //	 either one of the fieldputs below cause the problem to surface
            FieldPut ( 2 , "Eins" )
            FieldPut ( 3 , 123.45 )

            DbCommit()

            DbRUnLock ( RecNo() )

            Assert.False( DbSeek( "AAAA" ) ) // must show .F.
            Assert.True( DbSeek ("PPPP" ) ) // must show .T.
            // "Record order now:"
            // "(should be hhhh, pppp, ssss , wwww)"
            DbGoTop()
            LOCAL nCount := 0 AS INT
        DO WHILE ! Eof()
            ? AllTrim(FieldGet(1)) , FieldGet(3) , AllTrim(FieldGet(2))
            nCount ++
            DO CASE
            CASE nCount == 1
                Assert.Equal( "hhhh", AllTrim(FieldGet(1)) )
            CASE nCount == 2
                Assert.Equal( "pppp", AllTrim(FieldGet(1)) )
            CASE nCount == 3
                Assert.Equal( "ssss", AllTrim(FieldGet(1)) )
            CASE nCount == 4
                    Assert.Equal( "wwww", AllTrim(FieldGet(1)) )
                    END CASE
                DbSkip ( 1 )
            ENDDO
            DbCloseArea()
        RETURN


        // TECH-4JX6H10741, DBOrderInfo( DBOI_KEYCOUNT ) returns NIL when record pointer is at EoF
        [Fact, Trait("Category", "DBF")];
            METHOD DBOrderInfo_DBOI_KEYCOUNT() AS VOID
            LOCAL cDbf AS STRING
            LOCAL aValues AS ARRAY
            LOCAL i AS DWORD

            cDbf := __FUNCTION__
            aValues := { 44 , 12, 34 , 21 }
            DbCreate( cDBF , {{"AGE" , "N" , 3 , 0 } })
            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            Assert.Equal(0 , (INT) DbOrderInfo( DBOI_KEYCOUNT ) ) // 0,  ok
            FOR i := 1 UPTO ALen ( aValues )
                DbAppend()
                FieldPut(1,aValues [i])
            NEXT
            Assert.Equal(0 , (INT) DbOrderInfo( DBOI_KEYCOUNT ) ) // 0,  ok
            DbCreateIndex(cDbf, "age" )
            Assert.Equal(4 , (INT) DbOrderInfo( DBOI_KEYCOUNT ) ) // 4, correct
            DbGoTop()
            Assert.Equal(4 , (INT) DbOrderInfo( DBOI_KEYCOUNT ) ) // 4, correct
            DO WHILE ! Eof()
                //			? FieldGet ( 1 )
                DbSkip(1)
            ENDDO
            Assert.Equal(4 , (INT) DbOrderInfo( DBOI_KEYCOUNT ) ) // NIL, should be 4
            DbSkip(-1)
            Assert.Equal(4 , (INT) DbOrderInfo( DBOI_KEYCOUNT ) ) // 4, correct
            DbCloseArea ()
        RETURN

        [Fact, Trait("Category", "DBF")];
            METHOD SetDeleted_FALSE() AS VOID
            LOCAL cDbf AS STRING
            LOCAL aValues AS ARRAY
            LOCAL i AS DWORD

            cDbf := __FUNCTION__

            SetDeleted(FALSE)

            //			test also with those
            //			aValues := { "vaa" , "abba", "acb" , "aaab"  , "adab"  , "baac"  , "aeab"  , "baaAaa" }
            aValues := { "vvv" , "abb", "acb" , "aaa"  , "bbb" }
            DbCreate( cDBF , {{"LAST" , "C" ,10 , 0 } })
            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            FOR i := 1 UPTO ALen ( aValues )
                DbAppend()
                FieldPut(1,aValues [i])
            NEXT
            DbCloseArea()

            DbUseArea(,"DBFNTX",cDBF,,TRUE)
            Assert.Equal(0 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            DbCreateIndex(cDbf, "Upper(LAST)" )
            Assert.Equal(5 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            // ? "Setting scope"
            LOCAL u AS USUAL
            u := "A"
            VoDbOrderInfo( DBOI_SCOPETOP, "", NIL,  REF u )
            u := "A"
            VoDbOrderInfo( DBOI_SCOPEBOTTOM, "", NIL, REF  u )

            DbGoTop()
            Assert.Equal(4 , (INT)RecNo())
            DbGoBottom()
            Assert.Equal(3 , (INT)RecNo())

            DbGoTop()

            Assert.Equal(3 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            DO WHILE ! Eof()
                DbSkip(1)
            ENDDO
            Assert.Equal(3 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            DbCloseArea()

            SetDeleted(FALSE)
        RETURN


        // TECH-W0KKQ1I50C, Problems with SetDeleted(TRUE)
        [Fact, Trait("Category", "DBF")];
            METHOD SetDeleted_TRUE() AS VOID
            LOCAL cDbf AS STRING
            LOCAL aValues AS ARRAY
            LOCAL i AS DWORD

            cDbf := __FUNCTION__

            SetDeleted(TRUE)

            //			test also with those
            //			aValues := { "vaa" , "abba", "acb" , "aaab"  , "adab"  , "baac"  , "aeab"  , "baaAaa" }
            aValues := { "vvv" , "abb", "acb" , "aaa"  , "bbb" }
            DbCreate( cDBF , {{"LAST" , "C" ,10 , 0 } })
            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            FOR i := 1 UPTO ALen ( aValues )
                DbAppend()
                FieldPut(1,aValues [i])
            NEXT

            DbUseArea(,"DBFNTX",cDBF,,TRUE)
            Assert.Equal(0 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            DbCreateIndex(cDbf, "Upper(LAST)" )
            Assert.Equal(5 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            // ? "Setting scope"
            LOCAL u AS USUAL
            u := "A"
            VoDbOrderInfo( DBOI_SCOPETOP, "", NIL,  REF u )
            u := "A"
            VoDbOrderInfo( DBOI_SCOPEBOTTOM, "", NIL,  REF u )

            DbGoTop()
            Assert.Equal(4 , (INT)RecNo())
            DbGoBottom()
            Assert.Equal(3 , (INT)RecNo())

            DbGoTop()

            Assert.Equal(3 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            DO WHILE ! Eof()
                DbSkip(1)
            ENDDO
            Assert.Equal(3 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            DbCloseArea()

            SetDeleted(FALSE)
        RETURN


        // TECH-DD1J2Z3UVI, DBOrderInfo( DBOI_NUMBER ) incorrect results
        [Fact, Trait("Category", "DBF")];
            METHOD DBOrderInfo_DBOI_NUMBER() AS VOID
            LOCAL aValues AS ARRAY
            LOCAL i AS DWORD
            LOCAL cDBF AS STRING
            LOCAL cNTX AS STRING
            aValues := { 44 , 12, 34 , 21 }
            cDbf := "TESTDBF"
            cNTX := cDbf + ".ntx"
        IF System.IO.File.Exists(cNtx)
            System.IO.file.Delete(cNtx)
            END IF
            DbCreate( cDBF , {{"AGE" , "N" , 3 , 0 } })
            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            Assert.Equal(0, (INT)DbOrderInfo( DBOI_KEYCOUNT ) )   //  0  ok
            FOR i := 1 UPTO ALen ( aValues )
                DbAppend()
                FieldPut(1,aValues [i])
            NEXT
            Assert.Equal(0, (INT)DbOrderInfo( DBOI_KEYCOUNT ) ) //  0 ,ok
            Assert.Equal(0, (INT)DbOrderInfo( DBOI_NUMBER ) )  //  -1,  but should show 0

            DbCreateIndex( cNTX, "age" )
            DbGoTop()
            DO WHILE ! Eof()
                //				? FieldGet ( 1 )
                DbSkip(1)
            ENDDO

            Assert.True( DbClearIndex( cNTX) )

            DbGoTop()
            Assert.Equal(1, (INT)RecNo())

            Assert.True( DbSetIndex ( cNTX ) )

            Assert.Equal(4, (INT) DbOrderInfo( DBOI_KEYCOUNT ) ) // 4, ok
            Assert.Equal(1, (INT) DbOrderInfo( DBOI_NUMBER ) )  // still  -1 , but should show  1
            Assert.Equal("TESTDBF", (STRING) DbOrderInfo( DBOI_NAME ) )  // ok , "TESTDBF"
            DbCloseArea ()
        RETURN


        // TECH-RGU0U0636C, DBSetOrderCondition() results to NotImplementedException
        [Fact, Trait("Category", "DBF")];
            METHOD DBSetOrderCondition_test() AS VOID
            LOCAL cDbf AS STRING
            LOCAL aValues AS ARRAY
            LOCAL i AS DWORD

            cDbf := __FUNCTION__
            aValues := { 1,4,2,3 }
            DbCreate( cDBF , {{"NUM" , "N" ,5 , 0 } })
            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            FOR i := 1 UPTO ALen ( aValues )
                DbAppend()
                FieldPut(1,aValues [i])
            NEXT
            DbGoTop()

            // DESCENDING = TRUE
            Assert.True( DbSetOrderCondition(,,,,,,,,,,TRUE) )
            Assert.True( DbCreateIndex(cDbf, "NUM" ) )

            DbGoTop()
            aValues := { 4,3,2,1 }
            LOCAL nCount := 0 AS INT
        DO WHILE .NOT. EoF()
            nCount ++
            Assert.Equal((INT)aValues[nCount] , (INT)FieldGet(1))
            DbSkip()
            END DO

            DbCloseArea()
        RETURN



        // TECH-T365UMTY4V, Incorrect values returned by DBOrderInfo( DBOI_KEYTYPE )
        [Fact, Trait("Category", "DBF")];
            METHOD DBOrderInfo_DBOI_KEYTYPE() AS VOID
            LOCAL cDbf AS STRING
            DbCloseAll()

            cDbf := __FUNCTION__
            DbCreate( cDBF , {{"FIELDN" , "N" ,5 , 0 } , ;
            {"FIELDS" , "C" ,15 , 0 } , ;
            {"FIELDL" , "L" ,1 , 0 } , ;
            {"FIELDD" , "D" ,8 , 0 } })
            DbUseArea(,"DBFNTX",cDBF)
            DbAppend()
            FieldPut(1,1)
            DbCloseArea()

            DbUseArea(,"DBFNTX",cDBF)
            DbCreateIndex(cDbf, "FIELDN" )
            Assert.Equal(3, (INT)DbOrderInfo( DBOI_KEYTYPE ) ) // 14 (), should be 3 (FLOAT)
            DbCloseArea()

            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            DbCreateIndex(cDbf, "FIELDS" )
            Assert.Equal(7, (INT)DbOrderInfo( DBOI_KEYTYPE ) ) // 18 (PTR), should be 7 (STRING)

            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            DbCreateIndex(cDbf, "FIELDD" )
            Assert.Equal(2, (INT)DbOrderInfo( DBOI_KEYTYPE ) ) // 16, should be 2 (DATE)

            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            DbCreateIndex(cDbf, "FIELDL" )
            Assert.Equal(8, (INT)DbOrderInfo( DBOI_KEYTYPE ) ) // 3 (FLOAT), should be 8

            DbCloseArea()
        RETURN


        // TECH-YV2072YP4B, DBSetOrderCondition() not respecting FOR condition
        [Fact, Trait("Category", "DBF")];
            METHOD DBSetOrderCondition_with_FOR() AS VOID
            LOCAL cDbf AS STRING
            DbCloseAll()

            cDbf := __FUNCTION__
            DbCreate( cDBF , {{"FIELDN" , "N" ,5 , 0 } })
            DbUseArea(,"DBFNTX",cDBF)
            DbAppend()
            FieldPut(1,1)
            DbAppend()
            FieldPut(1,4)
            DbAppend()
            FieldPut(1,2)
            DbAppend()
            FieldPut(1,3)
            DbCloseArea()

            DbUseArea(,"DBFNTX",cDBF)

            // Should show only 4,3, but it shows all records 4,3,2,1
            DbSetOrderCondition( "FIELDN>2",{||_FIELD->FIELDN>2},,,,,,,,, TRUE)
            DbCreateIndex(cDbf, "FIELDN" )
            DbGoTop()
            LOCAL nCount := 0 AS INT
        DO WHILE .NOT. EoF()
            nCount ++
            IF nCount == 1
                Assert.Equal(4 ,(INT)FieldGet(1))
            ELSEIF nCount == 1
            Assert.Equal(3 ,(INT)FieldGet(1))
            END IF
            DbSkip()
            END DO

            Assert.Equal(2 ,nCount)

            // Should both show true, but both return false
            Assert.True( DbOrderInfo( DBOI_ISCOND ) )
            Assert.True( DbOrderInfo( DBOI_ISDESC ) )

            DbCloseArea()
        RETURN


        // TECH-3KG4A5V124, DBSetFound() always sets the Found flag to TRUE, no matter the value passed
        [Fact, Trait("Category", "DBF")];
            METHOD DBSetFound_test() AS VOID
            LOCAL aDbf AS ARRAY
            LOCAL cDbf AS STRING
            DbCloseAll()

            cDbf := __FUNCTION__
            aDbf := {{ "AGE" , "N" , 2 , 0 }}
            DbCreate( cDBF , aDbf)

            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            DbAppend()

            Assert.False( Found() )// FALSE, ok
            Assert.True( DbSetFound( FALSE ) )
            Assert.False( Found() ) // TRUE! wrong
            Assert.True( DbSetFound( TRUE ) )
            Assert.True( Found() ) // TRUE correct
            DbCloseArea()
        RETURN


        [Fact, Trait("Category", "DBF")];
            METHOD DBSetFound_test2() AS VOID
            LOCAL aDbf AS ARRAY
            LOCAL cDbf AS STRING
            DbCloseAll()

            cDbf := __FUNCTION__
            aDbf := {{ "AGE" , "N" , 2 , 0 }}
            DbCreate( cDBF , aDbf)

            DbCreate( cDBF , aDbf)
            DbUseArea(,"DBFNTX",cDBF,,FALSE)

            DbAppend()

            DbCloseArea()

            DbUseArea( TRUE ,"DBFNTX",cDBF,"AREA1" ,TRUE )
            DbUseArea( TRUE ,"DBFNTX",cDBF,"AREA2" ,TRUE )

            Assert.True( DbSelectArea ( "AREA1" ) )
            Assert.Equal("AREA1", Alias() )
            Assert.False( Found() )
            Assert.True( DbSetFound ( TRUE ) )
            Assert.True( Found() )

            Assert.True( DbSelectArea ( "AREA2" ) )
            Assert.Equal( "AREA2" , Alias() )
            Assert.False( Found() )
            Assert.True( DbSetFound  ( FALSE ) )
            Assert.False( Found() )

            Assert.True( DbCloseArea() )
            Assert.True( DbSelectArea ( "AREA1" ) )
            Assert.True( DbCloseArea() )
        RETURN


        // TECH-56GA29Y57C, Found() does not return correct results for the active workarea
        [Fact, Trait("Category", "DBF")];
            METHOD DBSetFound_test3() AS VOID
            LOCAL aDbf AS ARRAY
            LOCAL cDbf AS STRING
            DbCloseAll()

            cDbf := __FUNCTION__
            aDbf := {{ "AGE" , "N" , 2 , 0 }}
            DbCreate( cDBF , aDbf)
            DbUseArea(,"DBFNTX",cDBF,,FALSE)

            DbAppend()
            FieldPut(1,1)
            DbCloseArea()

            DbUseArea( TRUE ,"DBFNTX",cDBF,"AREA1" ,TRUE )
            DbUseArea( TRUE ,"DBFNTX",cDBF,"AREA2" ,TRUE )

            DbSelectArea ( "AREA1" )
            Assert.True( DbLocate({||_FIELD->AGE == 1}) )
            Assert.True( Found() )

            DbSelectArea ( "AREA2" )
            Assert.False( Found() )

            DbCloseAll()
        RETURN

        // TECH-8HN2I0UUNA , Index file not correctly created when dbf is opened in SHARED mode
        [Fact, Trait("Category", "DBF")];
            METHOD Shared_Ntx() AS VOID
            LOCAL cDbf AS STRING
            LOCAL cNtx AS STRING

            RddSetDefault("DBFNTX")

            cDbf := DbfTests.GetTempFileName()
            cNtx := cDbf + ".ntx"

            Assert.True( DbCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }}) )
            Assert.True( DbUseArea(,,cDbf,,FALSE) )
            Assert.True( DbAppend() )
            FieldPut ( 1 , "B")
            Assert.True( DbAppend() )
            FieldPut ( 1 , "A")
            Assert.True( DbCloseArea() )

            Assert.True( DbUseArea(,,cDbf,,TRUE) ) // ----- opening in SHARED mode
            Assert.True( DbCreateIndex(cNtx , "CFIELD") ) // returns TRUE
            Assert.True( DbCloseArea() )

            Assert.True( DbUseArea(,,cDbf,,FALSE) )
            Assert.True( DbSetIndex(cNtx) )
            DbGoTop()
            Assert.True( AllTrim(FieldGet(1)) == "A" )
            DbGoBottom()
            Assert.True( AllTrim(FieldGet(1)) == "B" )
            Assert.True( DbCloseArea() ) // XSharp.RDD.RddError here
        RETURN

        // TECH-588I8LB67J , Problems with NTX indexes
        [Fact, Trait("Category", "DBF")];
            METHOD Ntx_Issues() AS VOID
            LOCAL cDbf AS STRING
            LOCAL cNtx AS STRING
            LOCAL aResult AS ARRAY

            RddSetDefault("DBFNTX")

            cDbf := DbfTests.GetTempFileName()
            cNtx := cDbf + ".ntx"

            DbCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
            DbUseArea(,,cDbf)
            DbAppend()
            FieldPut ( 1 , "ABC")
            DbAppend()
            FieldPut ( 1 , "GHI")
            DbAppend()
            FieldPut ( 1 , "DEF")
            DbAppend()
            FieldPut ( 1 , "K")
            DbCloseArea()

            Assert.True( DbUseArea(,,cDbf) )
            Assert.True( DbCreateIndex(cNtx , "CFIELD") )
            Assert.True( DbCloseArea() )

            Assert.True( DbUseArea(,,cDbf,,FALSE) )
            Assert.True( DbSetIndex(cNtx) )
            aResult := DbfTests.GetRecords()
            //			should be ABC, DEF, GHI, K
            Assert.True( aResult[1] == "ABC")
            Assert.True( aResult[2] == "DEF")
            Assert.True( aResult[3] == "GHI")
            Assert.True( aResult[4] == "K")

            DbGoTop()
            DbSkip()
            FieldPut(1,"HHH")
            aResult := DbfTests.GetRecords()
            //			should be ABC, GHI, HHH, K
            Assert.True( aResult[1] == "ABC")
            Assert.True( aResult[2] == "GHI")
            Assert.True( aResult[3] == "HHH")
            Assert.True( aResult[4] == "K")

            DbGoTop()
            DbSkip(2)
            FieldPut(1,"DEF") // restore it

            Assert.True( DbCloseArea() )


            Assert.True( DbUseArea(,,cDbf,,TRUE) )
            Assert.True( DbSetIndex(cNtx) )
            aResult := DbfTests.GetRecords()
            //			should be ABC, DEF, GHI, K
            Assert.True( aResult[1] == "ABC")
            Assert.True( aResult[2] == "DEF")
            Assert.True( aResult[3] == "GHI")
            Assert.True( aResult[4] == "K")

            DbGoTop()
            DbSkip()
            Assert.True(RLock())
            FieldPut(1,"III")
            aResult := DbfTests.GetRecords()
            //			should be ABC, GHI, III, K
            Assert.True( aResult[1] == "ABC")
            Assert.True( aResult[2] == "GHI")
            Assert.True( aResult[3] == "III")
            Assert.True( aResult[4] == "K")

            Assert.True( DbCloseArea() )
        RETURN



        // TECH-V7A528Z0ZL , Problems with ntx indexes 2
        [Fact, Trait("Category", "DBF")];
            METHOD Ntx_Issues2() AS VOID
            LOCAL cFileName AS STRING
            cFileName := __FUNCTION__

            RddSetDefault("DBFNTX")

            DbCreate(cFileName, {{"FLD1","C",10,0},{"FLD2","N",10,0}})
            DbUseArea( , , cFileName , , FALSE)
            FOR LOCAL n := 1 AS INT UPTO 10
                DbAppend()
                FieldPut(1, n:ToString())
                FieldPut(2, n)
            NEXT
            Assert.True( DbCreateIndex(cFileName + ".ntx" , "FLD2") )
            Assert.True( DbCloseArea() )

            DbUseArea( , , cFileName , , FALSE)
            Assert.True( DbSetIndex(cFileName + ".ntx") )
            DbGoTop()
            LOCAL nCount := 0 AS INT
        DO WHILE ! Eof()
            nCount ++
            Assert.True( FieldGet(2) == RecNo() )
            Assert.True( FieldGet(2) == nCount )
            Assert.True( DbSkip() )
            END DO
            Assert.True( DbCloseArea() )
        RETURN
        // TECH-9JPUGAOV3L , NTX problem with EoF after sequence of commands
        [Fact, Trait("Category", "DBF")];
            METHOD Ntx_SoftSeek_Test_Numeric() AS VOID
            LOCAL cDbf AS STRING
            LOCAL cNtx AS STRING

            RddSetDefault("DBFNTX")

            cDbf := __FUNCTION__
            cNtx := cDbf + ".ntx"

            DbCreate( cDbf , {{"NFIELD" , "N" , 5 , 0 }})
            DbUseArea(,,cDbf,,FALSE)
            DbAppend()
            FieldPut(1,123)
            DbAppend()
            FieldPut(1,456)
            DbCreateIndex(cNtx, "NFIELD")
            DbCloseArea()
            DbUseArea(,,cDbf,,FALSE)
            DbSetIndex(cNtx)
            SetSoftSeek(TRUE)
            DbSeek(100)
            Assert.Equal(123, (INT)FieldGet(1))
            Assert.Equal(FALSE, Eof())
            Assert.Equal(FALSE, Found())
            DbSeek(200)
            Assert.Equal(456, (INT)FieldGet(1))
            Assert.Equal(FALSE, Eof())
            Assert.Equal(FALSE, Found())
            SetSoftSeek(FALSE)
            DbSeek(200)
            Assert.Equal(0, (INT)FieldGet(1))
            Assert.Equal(TRUE, Eof())
            Assert.Equal(FALSE, Found())
        SET(_SET_SOFTSEEK, "on")
            DbSeek(100)
            Assert.Equal(123, (INT)FieldGet(1))
            Assert.Equal(FALSE, Eof())
            Assert.Equal(FALSE, Found())
            DbSeek(200)
            Assert.Equal(456, (INT)FieldGet(1))
            Assert.Equal(FALSE, Eof())
            Assert.Equal(FALSE, Found())
        SET(_SET_SOFTSEEK, "off")
            DbSeek(200)
            Assert.Equal(0, (INT)FieldGet(1))
            Assert.Equal(TRUE, Eof())
            Assert.Equal(FALSE, Found())
        RETURN

        // TECH-9JPUGAOV3L , NTX problem with EoF after sequence of commands
        [Fact, Trait("Category", "DBF")];
            METHOD Ntx_SoftSeek_Test_String() AS VOID
            LOCAL cDbf AS STRING
            LOCAL cNtx AS STRING

            RddSetDefault("DBFNTX")

            cDbf := __FUNCTION__
            cNtx := cDbf + ".ntx"

            DbCreate( cDbf , {{"CFIELD" , "C" , 5 , 0 }})
            DbUseArea(,,cDbf,,FALSE)
            DbAppend()
            FieldPut(1,"bbbbb")
            DbAppend()
            FieldPut(1,"kkkkk")
            DbCreateIndex(cNtx, "CFIELD")
            DbCloseArea()
            DbUseArea(,,cDbf,,FALSE)
            DbSetIndex(cNtx)
            SetSoftSeek(TRUE)
            DbSeek("aaaaa")
            Assert.Equal("bbbbb", (STRING)FieldGet(1))
            Assert.Equal(FALSE, Eof())
            Assert.Equal(FALSE, Found())
            DbSeek("ccccc")
            Assert.Equal("kkkkk", (STRING)FieldGet(1))
            Assert.Equal(FALSE, Eof())
            Assert.Equal(FALSE, Found())
            SetSoftSeek(FALSE)
            DbSeek("ccccc")
            Assert.Equal("     ", (STRING)FieldGet(1))
            Assert.Equal(TRUE, Eof())
            Assert.Equal(FALSE, Found())

        SET(_SET_SOFTSEEK, "on")
            DbSeek("aaaaa")
            Assert.Equal("bbbbb", (STRING)FieldGet(1))
            Assert.Equal(FALSE, Eof())
            Assert.Equal(FALSE, Found())
            DbSeek("ccccc")
            Assert.Equal("kkkkk", (STRING)FieldGet(1))
            Assert.Equal(FALSE, Eof())
            Assert.Equal(FALSE, Found())
        SET(_SET_SOFTSEEK, "off")
            DbSeek("ccccc")
            Assert.Equal("     ", (STRING)FieldGet(1))
            Assert.Equal(TRUE, Eof())
            Assert.Equal(FALSE, Found())

        RETURN
        // TECH-Z7BO7GN926, Cannot clear order scope
        [Fact, Trait("Category", "DBF")];
            METHOD OrderScopeClear_test() AS VOID
            LOCAL cDbf AS STRING
            LOCAL aValues AS ARRAY
            LOCAL i AS DWORD

            cDBF := __FUNCTION__

            RddSetDefault("DBFNTX")

            //			test also with those
            //			aValues := { "vaa" , "abba", "acb" , "aaab"  , "adab"  , "baac"  , "aeab"  , "baaAaa" }
            aValues := { "vvv" , "abb", "acb" , "aaa"  , "bbb" }
            DbCreate( cDBF , {{"LAST" , "C" ,10 , 0 } })
            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            FOR i := 1 UPTO ALen ( aValues )
                DbAppend()
                FieldPut(1,aValues [i])
            NEXT
            DbCloseArea()

            DbUseArea(,"DBFNTX",cDBF,,TRUE)
            Assert.Equal(0 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            DbCreateIndex(cDbf, "Upper(LAST)" )
            Assert.Equal(5 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            // ? "Setting scope"
            LOCAL u AS USUAL
            u := "A"
            VoDbOrderInfo( DBOI_SCOPETOP, "", NIL,  REF u )
            u := "A"
            VoDbOrderInfo( DBOI_SCOPEBOTTOM, "", NIL,  REF u )

            DbGoTop()
            Assert.Equal(4 , (INT)RecNo())
            DbGoBottom()
            Assert.Equal(3 , (INT)RecNo())

            DbGoTop()

            Assert.Equal(3 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))
            DO WHILE ! Eof()
                DbSkip(1)
            ENDDO
            Assert.Equal(3 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))



            u := NIL
            VoDbOrderInfo( DBOI_SCOPETOPCLEAR, "", NIL, REF u )
            u := NIL
            VoDbOrderInfo( DBOI_SCOPEBOTTOMCLEAR, "", NIL, REF u )
            DbGoTop()

            Assert.Equal(5 , (INT)DbOrderInfo( DBOI_KEYCOUNT ))

            LOCAL nCount := 0 AS INT
            DO WHILE ! Eof()
                nCount ++
                //				? FieldGet(1)
                DbSkip(1)
            ENDDO
            Assert.Equal(5 , nCount)

            DbCloseArea()
        RETURN


        // TECH-8H9WL71978, OrdIsUnique() always returns TRUE
        [Fact, Trait("Category", "DBF")];
            METHOD OrdIsUnique_test() AS VOID
            LOCAL cDBF, cIndex AS STRING
            LOCAL lUnique AS LOGIC

            RddSetDefault ( "DBFCDX" )
            lUnique := SetUnique()

            cDBF := __FUNCTION__
            cIndex := cDbf
            DbfTests.CreateDatabase(cDbf , { { "LAST" , "C" , 20 , 0 }} , { "a" , "d" , "f", "c" })

            OrdCondSet()
            OrdCreate(cIndex, "ORDER1", "upper(LAST)", { || Upper ( _FIELD-> LAST) } )
            DbSetOrder ( 1 )
            Assert.Equal( "ORDER1", OrdName() )
            Assert.False( OrdIsUnique() ) // always returns true !
            Assert.False( DbOrderInfo(DBOI_UNIQUE ) ) // ok
            Assert.False( DbOrderInfo(DBOI_ISDESC ) )

            OrdCondSet()
            //			Create a descend and unique order
            OrdCondSet(,,,,,,,,,,TRUE)
            SetUnique ( TRUE )
            OrdCreate(cIndex, "ORDER2", "upper(LAST)", { || Upper ( _FIELD-> LAST) } )

            DbSetOrder ( 2 )
            Assert.Equal( "ORDER2", OrdName() )
            Assert.True( OrdIsUnique() ) // always returns true !
            Assert.True( DbOrderInfo(DBOI_UNIQUE ) )
            Assert.True( DbOrderInfo(DBOI_ISDESC ) )
            DbCloseAll()
            SetUnique ( lUnique )
        RETURN
        // TECH-OBS2512J5P, Runtime error with OrdKeyCount() and only one param passed
        [Fact, Trait("Category", "DBF")];
            METHOD OrdKeyCount_test() AS VOID
            LOCAL cDBF AS STRING
            cDBF := __FUNCTION__
            FErase(cDbf + ".cdx")
            FErase(cDbf + ".ntx")

            FOR LOCAL n := 1 AS INT UPTO 2
                RddSetDefault ( IIF( n == 1 , "DBFNTX" , "DBFCDX" ) )

                DbCreate( cDBF , { { "LAST" , "C" , 20 , 0 }} )
                DbUseArea(,,cDBF,,FALSE )
                DbAppend()
                FieldPut ( 1 , "test" )

                Assert.True( DbCreateIndex ( cDbf , "upper(LAST)" , { || Upper ( _FIELD->LAST) } ) )
                Assert.Equal( 1 , (INT) OrdKeyCount(1) )
                DbCloseArea()
            NEXT
        RETURN


        // TECH-545T6VKW27, Problems with OrdScope() and DBSeek()
        [Fact, Trait("Category", "DBF")];
            METHOD OrdScope_and_DBSeek_test() AS VOID
            LOCAL cDBF AS STRING
            LOCAL aFields, aValues AS ARRAY
            LOCAL i AS DWORD

            RddSetDefault("DBFNTX")

            cDBF := __FUNCTION__
            FErase(cDbf + ".ntx")

            aValues := {"Gas" , "Abc", "Golden" , "Guru" , "Ddd" , "Aaa" , "Ggg"}
            aFields := { {"CFIELD" , "C" , 10 , 0} }

            DbCreate(cDbf , aFields)
            DbUseArea(,,cDBF , , FALSE)
            DbCreateIndex(cDbf , "Upper(CFIELD)")
            FOR i := 1 UPTO ALen(aValues)
                DbAppend()
                FieldPut(1, aValues[i])
            NEXT

            DbGoTop()
            Assert.Equal((INT) DbOrderInfo( DBOI_KEYCOUNT ) ,7 ) // 7, correct

            // Setting order scope
            OrdScope(TOPSCOPE, "G")
            OrdScope(BOTTOMSCOPE, "G")
            DbGoTop()

            // VO: -2 with NTX, 4 with CDX
            Assert.Equal((INT) DbOrderInfo( DBOI_KEYCOUNT ) ,4)

            Assert.True( DbSeek("G")    ) // TRUE, correct
            Assert.True( DbSeek("GOLD") ) // TRUE with NTX, FALSE with CDX. VO TRUE in both

            // Clearing order scope
            OrdScope(TOPSCOPE, NIL)
            OrdScope(BOTTOMSCOPE, NIL)
            Assert.Equal(  (INT) DbOrderInfo( DBOI_KEYCOUNT ) ,7)
            Assert.True( DbSeek("G") )
            Assert.True( DbSeek("GOLD") )

            // Setting order scope again
            OrdScope(TOPSCOPE, "G")
            OrdScope(BOTTOMSCOPE, "G")
            DbGoTop()
            // VO: -2 with NTX, 4 with CDX
            Assert.Equal( (INT) DbOrderInfo( DBOI_KEYCOUNT ) ,4 )

            Assert.True( DbSeek("G")    ) // TRUE, correct
            Assert.True( DbSeek("GOLD") ) // TRUE with NTX, FALSE with CDX. VO TRUE in both

            DbCloseArea()
        RETURN



        [Fact, Trait("Category", "DBF")];
            METHOD TestDescendOrderDeletedSeek_ntx() AS VOID
            LOCAL cDbf AS STRING
            LOCAL lDeleted AS LOGIC
            cDbf := DbfTests.GetTempFileName()

            RddSetDefault("DBFNTX")

            DbfTests.CreateDatabase(cDbf, {{ "FLD1", "C", 10, 0 }})

            lDeleted := SetDeleted( TRUE )

            DbUseArea(TRUE,,cDbf)
            DbCreateOrder( "ORDER1",cDbf, "FLD1")
            FOR LOCAL nI := 1 AS INT UPTO 10
                DbAppend()
                FieldPut(1,Str(nI,10))
            NEXT
            DbSetOrder("ORDER1")
            OrdDescend(,, TRUE)

            Assert.True( DbSeek( Str( 3, 10 ), FALSE ) )
            Assert.Equal( 3U, RecNo() )

            Assert.True( DbDelete() )

            Assert.False( DbSeek( Str( 3, 10 ), FALSE ) )
            Assert.Equal( 11U, RecNo() )

            DbCloseAll()

            SetDeleted( lDeleted )
        RETURN


        [Fact, Trait("Category", "DBF")];
            METHOD SetDeleted_SetDeleted_and_OrdScope_Ntx() AS VOID
            LOCAL aValues AS ARRAY
            LOCAL cDBF AS STRING

        TRY

            RddSetDefault("DBFNTX")
            cDBF := DbfTests.GetTempFileName()

            DbfTests.CreateDatabase(cDbf , { { "LAST" , "C" , 20 , 0 } } )
            aValues := { "a1" , "a2", "a3" , "a4" , "a5" , "a6" ,"g1", "g2" , "g3" , "g4" , "g5" , "o1" , "o2" , "o3" , "o4" , "o5" , "u1", "u2","u3" , "u4" }

            DbUseArea( ,,cDBF , , TRUE )

            FOR LOCAL i := 1 AS INT UPTO ALen ( aValues )
                DbAppend()
                FieldPut ( 1 , aValues [ i ] )
                IF InList ( Upper ( aValues [ i ] ) , "G1" , "G2" , "O5" )
                    DbDelete()
                ENDIF
            NEXT

            DbCreateOrder ( "ORDER1" , cDbf , "upper(LAST)" , { || Upper ( _FIELD->LAST) } )

            DbSetOrder ( 1 )

            OrdScope ( TOPSCOPE, "G" )
            OrdScope ( BOTTOMSCOPE, "G" )
            //			OrdScope ( TOPSCOPE, "A" )
            //			OrdScope ( BOTTOMSCOPE, "G" )

            Assert.Equal(5, (INT) OrdKeyCount() )

            SetDeleted ( TRUE )  // no problem if set to FALSE

            OrdDescend ( , , TRUE )
            DbGoTop()

            // SetDeleted(TRUE) causes a endless loop ...
            LOCAL nCount := 0 AS INT
            DO WHILE ! Eof()
                    //				? AllTrim(FieldGet ( 1 ) ) , RecNo(), Eof(), Bof()
                    DbSkip ( 1 )
                    nCount ++
                    IF nCount > 100
                        THROW Exception{"Skip() loop never ends"}
                        END IF
                    ENDDO

                    Assert.Equal(3, (INT) OrdKeyCount() )


                    SetDeleted ( TRUE )
                    OrdDescend ( , , TRUE )
                    DbGoTop()
                    Assert.Equal(3, (INT) OrdKeyCount() )

                    SetDeleted ( FALSE )
                    OrdDescend ( , , TRUE )
                    DbGoTop()
                    Assert.Equal(5, (INT) OrdKeyCount() )

                    SetDeleted ( TRUE )
                    OrdDescend ( , , FALSE )
                    DbGoTop()
                    Assert.Equal(3, (INT) OrdKeyCount() )

                    SetDeleted ( FALSE )
                    OrdDescend ( , , FALSE )
                    DbGoTop()
                    Assert.Equal(5, (INT) OrdKeyCount() )


                    DbCloseAll()

            CATCH

                THROW

            FINALLY

                SetDeleted(FALSE)

            END TRY
        RETURN


        // TECH-5OD246EMC4, VODBOrdListAdd() fails when index filename passed without extension
        [Fact, Trait("Category", "DBF")];
            METHOD VODBOrdListAdd_test() AS VOID
            LOCAL cDBF, cIndex AS STRING
            LOCAL aFields, aValues AS ARRAY
            LOCAL i AS DWORD

            RddSetDefault("DBFNTX")
            aFields := { { "LAST" , "C" , 20 , 0 }}
            aValues := { "b" , "c" , "d", "e" , "a" }

            cDBF := __FUNCTION__
            cIndex := cDbf
            FErase ( cIndex + IndexExt() )

            DbCreate( cDBF , AFields)
            DbUseArea(,,cDBF)
            FOR i := 1 UPTO ALen ( aValues )
                DbAppend()
                FieldPut ( 1 , aValues [ i ] )
            NEXT
            DbCreateOrder ( "ORDER1" , cIndex , "upper(LAST)" , { || Upper (_FIELD->LAST) } )
            DbCloseAll()

            //			 When ".ntx" is added SetIndex() returns true
            //			 cIndex := cIndex + IndexExt()
            DbUseArea(,,cDBF)
            Assert.True( VoDbOrdListAdd(cIndex , NIL) ) // Returns FALSE, error
            DbCloseAll()
        RETURN

        // Descartes discovered a problem with DbSeek and Found
        [Fact, Trait("Category", "DBF")];
            METHOD DbSeekFound_test() AS VOID
            LOCAL cPath, cDbf AS STRING
            RddSetDefault("DBFNTX")

            cPath := System.IO.Path.GetTempPath()
        IF .NOT. cPath:EndsWith("\")
            cPath += "\"
            END IF
            cDbf := "DbseekTest"
            DbCreate(cDbf, {{"KEY","C",10,0}},"DBFNTX")
            DbUseArea(TRUE,"DBFNTX",cDbf)
            DbCreateIndex(cDbf,"KEY")
            DbAppend()
            VAR aValues := {"A","B","C","D","E","F"}
            FOREACH sValue AS STRING IN aValues
                DbAppend()
                FieldPut(1, sValue)
            NEXT
            FOREACH sValue AS STRING IN aValues
                Assert.True(DbSeek(sValue))
                Assert.True(Found())
            NEXT
            DbCloseArea()
            // Repeat for DBFCDX
            VAR cCdx := Path.ChangeExtension(cDbf, ".CDX")
            FErase(cCdx)
            DbCreate(cDbf, {{"KEY","C",10,0}},"DBFCDX")
            DbUseArea(TRUE,"DBFCDX",cDbf)
            DbCreateIndex(cDbf,"KEY")
            DbAppend()
            FOREACH sValue AS STRING IN aValues
                DbAppend()
                FieldPut(1, sValue)
            NEXT
            FOREACH sValue AS STRING IN aValues
                Assert.True(DbSeek(sValue))
                Assert.True(Found())
            NEXT
        DbCloseArea()


        // TECH-429V6Q2959, IndexExt() problems
        [Fact, Trait("Category", "DBF")];
            METHOD IndexExt_test() AS VOID
            LOCAL aDbf AS ARRAY
            LOCAL cDBF AS STRING
            LOCAL aValues AS ARRAY
            LOCAL i AS DWORD
            DbCloseAll()

            cDbf := __FUNCTION__

            RddSetDefault("DBFNTX")
            Assert.True( IndexKey() == "")
            Assert.True( IndexOrd() == 0)
            Assert.True( IndexCount() == 0)
            Assert.True( Upper(IndexExt()) ==  ".NTX")
            Assert.True( DbOrderInfo(DBOI_INDEXEXT) == NIL )

            aValues := { 44 , 12, 34 , 21 }
            aDbf := {{ "AGE" , "N" , 2 , 0 }}

            DbCreate( cDBF , aDbf)
        IF File.Exists(cDbf +".cdx")
            File.Delete(cDbf +".cdx")
            END IF
        IF File.Exists(cDbf +".ntx")
            File.Delete(cDbf +".ntx")
            END IF

            DbCloseAll()
            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            Assert.True( DbOrderInfo(DBOI_INDEXEXT) == ".NTX")
            Assert.True( DbOrderInfo(DBOI_BAGEXT) == ".NTX")
            DbCloseArea()
            DbUseArea(,"DBFCDX",cDBF,,FALSE)
            Assert.True( DbOrderInfo(DBOI_INDEXEXT) == ".CDX")
            Assert.True( DbOrderInfo(DBOI_BAGEXT) == ".CDX")
            DbCloseArea()


            DbUseArea(,"DBFNTX",cDBF,,FALSE)
            FOR i := 1 UPTO ALen ( aValues )
                DbAppend()
                FieldPut(1,aValues [i])
            NEXT
            DbCreateIndex( cDbf, "age" )

            Assert.Equal( "age" , (STRING) IndexKey() )
            Assert.True( IndexOrd() == 1 )
            Assert.True( IndexCount() == 1 )
            Assert.True( Upper(IndexExt()) == ".NTX")
            Assert.True( DbOrderInfo(DBOI_INDEXEXT) == ".NTX")

            DbCloseArea()
        RETURN


        [Fact, Trait("Category", "DBF")];
            METHOD TestOrdScopeReturnValue() AS VOID
            LOCAL cDbf AS STRING
            cDbf := DbfTests.GetTempFileName()
            RddSetDefault("DBFNTX")

            DbfTests.CreateDatabase(cDbf, {{"CFIELD","C",1,0}} , {"S","S","N"})
            DbCreateIndex(cDbf , "CFIELD")
            DbCloseArea()

            DbUseArea( TRUE ,,cDBF )
            DbSetIndex(cDbf )
            Assert.True(OrdScope(TOPSCOPE, "S") == NIL)
            Assert.True(OrdScope(BOTTOMSCOPE, "S") == NIL)
            Assert.Equal("S", OrdScope(TOPSCOPE, "N"))
            Assert.Equal("S", OrdScope(BOTTOMSCOPE, "N"))
            DbCloseArea()
        RETURN

        // test based on corrupted index example from Remco (Descartes)
        [Fact, Trait("Category", "DBF")];
            METHOD TestCorruptedIndex() AS VOID
            LOCAL aStruct AS ARRAY
            aStruct := {;
                {"MSGREF","C",14,0},;
                {"REFNR","C",12,0},;
                {"ZENDNR","C",9,0},;
                {"CONSREF","C",12,0},;
                {"QUAL","C",2,0},;
                {"TYPE","C",10,0}}
            DbCreate("PCI", aStruct)
            DbUseArea(TRUE, "DBFNTX", "PCI",,FALSE)
            VAR aValues := List<STRING>{}

            aValues:Add( "T202122302;;9306532/2;;;PK")  // 1
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 2
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 3
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 4
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 5
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 6
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 7
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 8
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 9
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 10
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 11
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 12
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 13
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 14
            aValues:Add( "T202122302;;9306532/2;;;PK")  // 15
            aValues:Add( "T202122302;;9306533/2;;;PK")  // 16
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 17
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 18
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 19
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 20
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 21
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 22
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 23
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 24
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 25
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 26
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 27
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 28
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 29
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 30
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 31
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 32
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 33
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 34
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 35
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 36
            aValues:Add( "T202122302;;9306534/2;;;PK")  // 37
            aValues:Add( "T202122302;;9306535/2;;;PK")  // 38
            aValues:Add( "T202122302;;9306536/2;;;PK")  // 39
            aValues:Add( "T202122302;;9306537/2;;;PK")  // 40
            aValues:Add( "T202122302;;9306538/2;;;PK")  // 41
            aValues:Add( "T202122302;;9306539/2;;;PK")  // 42
            aValues:Add( "T202122302;;9306540/2;;;PK")  // 43
            aValues:Add( "T202122302;;9306541/2;;;PK")  // 44
            aValues:Add( "T202122302;;9306542/2;;;PK")  // 45
            aValues:Add( "T202122302;;9306543/2;;;PK")  // 46
            aValues:Add( "T202122302;;9306543/2;;;PK")  // 47
            aValues:Add( "T202122302;;9306544/2;;;PK")  // 48
            aValues:Add( "T202122302;;9306544/2;;;PK")  // 49
            aValues:Add( "T202122302;;9306544/2;;;PK")  // 50
            aValues:Add( "T202122302;;9306544/2;;;PK")  // 51
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 52
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 53
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 54
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 55
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 56
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 57
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 58
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 59
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 60
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 61
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 62
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 63
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 64
            aValues:Add( "T202122302;;9306545/2;;;PK")  // 65
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 66
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 67
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 68
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 69
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 70
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 71
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 72
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 73
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 74
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 75
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 76
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 77
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 78
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 79
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 80
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 81
            aValues:Add( "T202122302;;9306546/2;;;PK")  // 82
            aValues:Add( "T202122302;;9306547/2;;;PK")  // 83
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 84
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 85
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 86
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 87
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 88
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 89
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 90
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 91
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 92
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 93
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 94
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 95
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 96
            aValues:Add( "T202122302;;9307161/2;;;PK")  // 97
        FOREACH VAR line IN aValues
            VAR elements := line:Split(<char>{';'})
            DbAppend()
            FOR VAR i := 1 TO elements:Length
                FieldPut(i, elements[i])
            NEXT
        NEXT
        DbCloseArea()
        DbUseArea(TRUE, "DBFNTX", "pci.DBF","PCI")
	    OrdCreate("pci1","pci1","msgref+consref+qual",{||_FIeld->msgref+_FIELD->consref+_FIELD->qual})
        OrdCreate("pci2","pci2","msgref+zendnr+type",{||_FIeld->MsgRef+_FIELD->zendnr+_FIELD->type})
        OrdListClear()
        OrdListAdd("pci1")
        OrdListAdd("pci2")
        OrdSetFocus("PCI2")
        VAR aKeys := { ;
         {"T202122302    9306547/2", "0003157522"},;
         {"T202122302    9306541/2", "0003158882"},;
         {"T202122302    9306533/2", "0003158887"},;
         {"T202122302    9306540/2", "0003158888"},;
         {"T202122302    9306537/2", "0003158891"},;
         {"T202122302    9307161/2", "0003158895"},;
         {"T202122302    9306545/2", "0003158902"},;
         {"T202122302    9306546/2", "0003158905"},;
         {"T202122302    9306534/2", "0003158887"},;
         {"T202122302    9306532/2", "0003158909"} }
         // the code below caused a crash in 2.9 because
         // the key for recno 25 in the root page was removed
         // causing the remaining records from page 000400 to
         // be inaccessible
         // Initially the root contained key for record 25
         // and the first page below it the keys for record 1-24
         /*
            Dump BEFORE the update of record 25
                NTX Header Filedump for: pci1.NTX
                ----------------------------------------------
                Signature Default, Version 1, First page 001400, Unused Page 000000
                Item size 36, Key Size 28, Decimals 0, Max Items 24, HalfPage 12
                Key Expression: msgref+consref+qual, Unique False, Descending False
                For Expression:
                Order name    : PCI1
                ----------------------------------------------
                Page 001400, # of keys: 4
                Item  0, Page 000400 Record   25 : T202122302
                Item  1, Page 000800 Record   50 : T202122302
                Item  2, Page 000C00 Record   74 : T202122302
                Item  3, Page 001000 Record   87 : T202122302
                Right page reference 001800

                Page 000400, # of keys: 16
                Item  0, Page 000000 Record    1 : T202122302
                Item  1, Page 000000 Record    2 : T202122302
                Item  2, Page 000000 Record    3 : T202122302
                Item  3, Page 000000 Record    4 : T202122302
                Item  4, Page 000000 Record    5 : T202122302
                Item  5, Page 000000 Record    6 : T202122302
                Item  6, Page 000000 Record    7 : T202122302
                Item  7, Page 000000 Record    8 : T202122302
                Item  8, Page 000000 Record    9 : T202122302
                Item  9, Page 000000 Record   10 : T202122302
                Item 10, Page 000000 Record   11 : T202122302
                Item 11, Page 000000 Record   12 : T202122302
                Item 12, Page 000000 Record   13 : T202122302
                Item 13, Page 000000 Record   14 : T202122302
                Item 14, Page 000000 Record   15 : T202122302
                Item 15, Page 000000 Record   16 : T202122302
                Right page reference 000000

            DUMP After the change of record 25 (INCORRECT)

                Page 001400, # of keys: 3
                Item  0, Page 000800 Record   50 : T202122302
                Item  1, Page 000C00 Record   74 : T202122302
                Item  2, Page 001000 Record   87 : T202122302
                Right page reference 001800

            Item 0 should have been replaced with Record 16
            and page 000400 should have its last node removed.

         */
         FOR VAR i := 1 TO ALen(aKeys)
			     VAR cKey   := aKeys[i,1]
                 VAR cValue := aKeys[i,2]
                 VAR lFound := pci->DbSeek(cKey,FALSE,FALSE)
                 Assert.True(lFound .AND. ! Empty(cValue))
                 IF lFound .AND. ! Empty(cValue)
                     DO WHILE pci->msgref + pci->zendnr == cKey
                         pci->consref := cValue
                         ? cKey, "Recno", pci->RecNo()
                         pci->(DbSkip())
                     ENDDO
                 ENDIF
             NEXT

        DbCloseArea()


        [Fact, Trait("Category", "DBF")];
		METHOD TestSoftSeekWithScope() AS VOID
			// https://github.com/X-Sharp/XSharpPublic/issues/905
			LOCAL cDbf AS STRING
			cDbf := DbfTests.GetTempFileName()
			RddSetDefault("DBFNTX")

			DbfTests.CreateDatabase(cDbf, {{"CFIELD","C",5,0}} , {"A","B","B","C"})
			DbCreateIndex(cDbf , "CFIELD")
			DbCloseArea()

			DbUseArea( TRUE ,,cDBF )
            DbSetIndex(cDbf)

			OrdScope(TOPSCOPE, "B")
			OrdScope(BOTTOMSCOPE, "B")

			DbGoTop()
			Assert.Equal(2U, RecNo())
			Assert.False(DbSeek("BB",FALSE))
			Assert.True(Eof())
			Assert.Equal(5U, RecNo())

			DbGoTop()
			Assert.Equal(2U, RecNo())
			Assert.False(DbSeek("BB",TRUE))
            // different results here in VO for DBFNTX too
            // but this now matches the results for DBFCDX
			Assert.True(Eof())
			Assert.Equal(5U, RecNo())

			DbCloseArea()
		RETURN


        [Fact, Trait("Category", "DBF")];
		METHOD TestDBSetIndex() AS VOID
			// https://github.com/X-Sharp/XSharpPublic/issues/958
			LOCAL cDbf AS STRING
			cDbf := DbfTests.GetTempFileName()

			RddSetDefault("DBFNTX")

			DbfTests.CreateDatabase(cDbf, {{"FLD","N",5,0}} , {1,3,2})
			DbCreateIndex(cDbf + "1", "FLD")
			DbCreateIndex(cDbf + "2", "-FLD")
			DbCloseArea()

			DbUseArea(TRUE,,cDbf)

			DbSetIndex(cDbf + "1")
			DbGoTop()
			Assert.Equal(1, (INT)FieldGet(1))
			DbGoBottom()
			Assert.Equal(3, (INT)FieldGet(1))
			
			DbSetIndex(cDbf + "2")
			DbGoTop()
			Assert.Equal(1, (INT)FieldGet(1))
			DbGoBottom()
			Assert.Equal(3, (INT)FieldGet(1))

			DbCloseArea()



			DbUseArea(TRUE,,cDbf)

			DbSetIndex(cDbf + "1")
			DbGoTop()
			Assert.Equal(1, (INT)FieldGet(1))
			DbGoBottom()
			Assert.Equal(3, (INT)FieldGet(1))
			
			DbSetOrder(0) // when using this, VO now keeps the natural order active even after the following SetIndex() !!!

			DbSetIndex(cDbf + "2")
			DbGoTop()
			Assert.Equal(1, (INT)FieldGet(1))
			DbGoBottom()
			Assert.Equal(2, (INT)FieldGet(1))

			DbCloseArea()
		RETURN


    END CLASS
END NAMESPACE
