//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XUnit
USING System.Globalization
USING System.Threading
USING System.IO

BEGIN NAMESPACE XSharp.RT.Tests

	CLASS DbfTests

		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBCreate_Tests() AS VOID
			LOCAL aFields AS ARRAY
			aFields := {{"TEST","C",10,0}}

			LOCAL cFileName_WithExt AS STRING
			LOCAL cFileName_NoExt AS STRING
			cFileName_NoExt := "DBCreate_Tests"
			cFileName_WithExt := cFileName_NoExt + ".dbf"
			IF System.IO.File.Exists(cFileName_WithExt)
				System.IO.File.Delete(cFileName_WithExt)
			END IF
			Assert.True(  DbCreate(cFileName_NoExt , aFields , "DBFNTX")  )
			Assert.True(  System.IO.File.Exists(cFileName_WithExt) )

			IF System.IO.File.Exists(cFileName_WithExt)
				System.IO.File.Delete(cFileName_WithExt)
			END IF
			Assert.True(  DbCreate(cFileName_WithExt , aFields , "DBFNTX")  )
			Assert.True(  System.IO.File.Exists(cFileName_WithExt) )

			cFileName_WithExt := cFileName_NoExt + ".none"
			IF System.IO.File.Exists(cFileName_WithExt)
				System.IO.File.Delete(cFileName_WithExt)
			END IF
			Assert.True(  DbCreate(cFileName_WithExt , aFields , "DBFNTX")  )
			Assert.True(  System.IO.File.Exists(cFileName_WithExt) )
		RETURN

		[Fact, Trait("Category", "DBFFuncs")];
		METHOD testInputBufferDeletedWhenClosed AS VOID
            LOCAL cDbf AS STRING
            local cCopy as STRING
            cDbf := "test.dbf"
            cCopy := System.IO.Path.ChangeExtension(cDbf, "TMP")
            RddSetDefault("DBFNTX")

            DbCreate(cDbf,{{"FLD","N",5,0}})

            DbUseArea(TRUE,,cDbf,"Test")
            DbAppend()
            FieldPut(1,1)
            DbAppend()
            FieldPut(1,2)
            DbCloseArea("test")
            System.IO.File.Copy(cDbf, cCopy, true)
            DbUseArea(TRUE,,cDbf)
            Assert.Equal(1, (int) FieldGet(1))
            FieldPut(1,3)
            Assert.Equal(3, (int) FieldGet(1))
            DbCloseArea()
            System.IO.File.Copy(cCopy, cDbf, true)
            DbUseArea(TRUE,,cDbf)
            // field 1 should be 1 and not 3
            Assert.Equal(1, (int) FieldGet(1))
            DbCloseArea()


		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBAppend_Exclusive() AS VOID
			LOCAL aFields AS ARRAY
			LOCAL cFileName AS STRING
			aFields := {{"TEST","C",10,0}}
			cFileName := "DBAppend_Exclusive"

			Assert.True(  DbCreate(cFileName , aFields , "DBFNTX")  )
			Assert.True(  DbUseArea(,"DBFNTX",cFileName,"test",FALSE) )
			Assert.True(  RecCount() == 0 )
			Assert.True(  DbAppend() )
			FieldPut(1 , "test")
            Assert.True(  AllTrim(FieldGet(1)) == "test" )
            Assert.True(  DbAppend(TRUE,"test") )
			__FieldSetWa("test", "test","test")
			Assert.True(  DbCloseArea("test") )
		RETURN

		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBAppend_Shared() AS VOID
			LOCAL aFields AS ARRAY
			LOCAL cDbf AS STRING
			aFields := {{"TEST","C",10,0}}
			cDbf := __FUNCTION__

			Assert.True(  DbCreate(cDbf , aFields , "DBFNTX")  )
			Assert.True(  DbUseArea(,"DBFNTX",cDbf,,TRUE) )
			Assert.True(  RecCount() == 0 )
			Assert.True(  DbAppend() )
			FieldPut(1 , "test")
			Assert.True(  AllTrim(FieldGet(1)) == "test" )
			Assert.True(  DbCloseArea() )
		RETURN

		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBAppend_more() AS VOID
		LOCAL cDbf AS STRING
			cDbf := __FUNCTION__+".Dbf"
			RddSetDefault( "DBFNTX" )
			Assert.True(  DbCreate(cDbf , { {"TEST","C",10,0} }) )
			// Appending in exclusive mode:
			Assert.True( DbUseArea(, , cDbf , "alias1" , FALSE) )
			Assert.True( DbAppend() )
			Assert.True( RecCount() == 1 )
			FieldPut(1, "test") // ok
			Assert.True( AllTrim(FieldGet(1)) == "test" )
			Assert.True( DbCloseArea() )

			// Appending in SHARED mode:
			Assert.True( DbUseArea(, , cDbf , "alias2" , TRUE) )
			Assert.True( RecCount() == 1 )
			Assert.True( DbAppend() )// returns true but does not append record
			Assert.True( RecCount() == 2 )
			FieldPut(1, "test2") // ok
			Assert.True( AllTrim(FieldGet(1)) == "test2" )
			Assert.True( DbCloseArea() )
		RETURN

		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBUseArea_same_file_twice() AS VOID
		LOCAL cDbf AS STRING
			RddSetDefault( "DBFNTX" )
			cDbf := __FUNCTION__+".DbF"
			Assert.True(  DbCreate(cDbf , { {"TEST","C",10,0} }) )

			// shared mode
			Assert.True( DbUseArea(, , cDbf , , FALSE) )
			Assert.True( DbCloseArea() )

			Assert.True( DbUseArea(, , cDbf , , FALSE) )
			Assert.True( DbCloseArea() )
		RETURN


		// TECH-K3TL5J8M7V
		[Fact, Trait("Category", "DBF")];
		METHOD VODBInfoTest() AS VOID

			LOCAL cFileName AS STRING
			cFileName := __FUNCTION__+".DBF"
			DbCreate(cFileName , { {"CFIELD","C",10,0} })
			DbUseArea(,,cFileName,"TEST")
			DbAppend()

			LOCAL u := NIL AS USUAL
			VoDbSkip(-1)

			VoDbInfo(DBI_FULLPATH , REF u)
			LOCAL c AS STRING
			c := u
			? c
			Assert.True(c:EndsWith(cFileName) .AND. c:Contains(":\"))
			VoDbInfo(DBI_DB_VERSION , REF u)
			Assert.True(SLen(u) > 1)
			VoDbInfo(DBI_ALIAS , REF u)
			Assert.Equal("TEST" , u)

			VoDbInfo(DBI_BOF , REF u)
			Assert.Equal(TRUE , (LOGIC) u)
			VoDbInfo(DBI_EOF , REF u)
			Assert.Equal(FALSE ,(LOGIC)  u)
			VoDbInfo(DBI_ISANSI , REF u)
			Assert.Equal(SetAnsi() ,(LOGIC)  u)
			VoDbInfo(DBI_FCOUNT , REF u)
			Assert.Equal(1, (LONG)  u)
			VoDbInfo(DBI_READONLY , REF u)
			Assert.Equal(FALSE,(LOGIC)  u)

			DbCloseArea()
		RETURN

		// TECH-E6Y9GNHB99
		[Fact, Trait("Category", "DBF")];
		METHOD AppendShared() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName("testAppendShared")
			RddSetDefault( "DBFNTX" )
			DbCreate(cDbf , { {"TEST","C",10,0} })

//			Appending in exclusive mode:
			DbUseArea(, , cDbf , "alias2" , FALSE)
			Assert.True( DbAppend() )
			Assert.Equal(  RecCount() ,1)
			FieldPut(1, "test") // ok
			DbCloseArea()

//			Appending in SHARED mode:
			DbUseArea(, , cDbf , "alias2" , TRUE)
			Assert.True( DbAppend() ) // returns true but does not append record
			Assert.Equal(  RecCount() ,2) // returns 1, wrong
			DbCloseArea()
		RETURN

		// TECH-6U40UQ0JV3
		[Fact, Trait("Category", "DBF")];
		METHOD AliasNameReuse() AS VOID
			LOCAL cDbf AS STRING
			cDbf := __FUNCTION__

			RddSetDefault( "DBFNTX" )

			DbCreate(cDbf , { {"TEST","C",10,0} })

//			opening and closing once
			DbUseArea(, , cDbf , , FALSE)
			DbCloseArea()

//			opening and closing again
			Assert.True( DbUseArea(, , cDbf , , FALSE) )
			Assert.True( DbCloseArea() )
		RETURN

		// TECH-SAK5955895
		[Fact, Trait("Category", "DBF")];
		METHOD SavingDecimalValues() AS VOID
			LOCAL cFileName AS STRING
			cFileName := __FUNCTION__
			DbCreate(cFileName, {{"FLD1","N",10,2},{"FLD2","N",10,0}})
			DbUseArea(,,cFileName)
			DbAppend()

			SetDecimalSep(Asc(","))
			FieldPut(1 , 12.34) // not saved in the dbf
			Assert.Equal((FLOAT) FieldGet(1),12.34 ) // 0,00

			SetDecimalSep(Asc("."))
			FieldPut(1 , -12.34)
			Assert.Equal(-12.34 , (FLOAT) FieldGet(1))

			DbCloseArea()
		RETURN

		// TECH-C8WB52EA4A , Runtime exception when reading from a float field, after writing to it and DBCommit()
		[Fact, Trait("Category", "DBF")];
		METHOD DBCommitAfterFieldput() AS VOID
			LOCAL cFileName AS STRING
			cFileName := __FUNCTION__
			DbCreate(cFileName, {{"FLD1","N",10,4}})
			DbUseArea( , , cFileName , "tempalias")
			DbAppend()
			DbCloseArea()

			DbUseArea( , , cFileName)
			FieldPut(1 , 46.11) // ! a float isn/t stored !
			DbCommit()
			Assert.Equal(46.11 , (FLOAT) FieldGet(1)) // runtime exception
			DbCloseArea()
		RETURN


		// TECH-J61EXJ870D , FieldName() and FieldSym() throw an exception with incorrect field no
		[Fact, Trait("Category", "DBF")];
		METHOD FieldNameSym() AS VOID
			LOCAL cFileName AS STRING
			cFileName := __FUNCTION__
			DbCreate(cFileName, {{"FLD1","N",10,4}})
			DbUseArea( , , cFileName , "tempalias")
			DbAppend()
			Assert.Equal("", FieldName(100)) // exception
			Assert.Equal("", FieldName(0))
			Assert.Equal(NULL_SYMBOL, FieldSym(100))
			Assert.Equal(NULL_SYMBOL, FieldSym(0))
			DbCloseArea()
		RETURN



		// TECH-560ANYQI2P , DBRLockList() always returns zero
		[Fact, Trait("Category", "DBF")];
		METHOD DBRLockListTest() AS VOID
			LOCAL cFileName AS STRING
			cFileName := __FUNCTION__
			DbCreate(cFileName, {{"FLD1","C",10,0}})
			DbUseArea( , , cFileName , "tempalias" , TRUE)
			DbAppend()
			DbAppend()
			DbGoTop()
			DbRLock()
			DbRLockList()
			Assert.Equal(1, (INT) ALen(DbRLockList()))
			Assert.Equal(1, (INT) DbRLockList()[1])
			DbUnLock()
			DbSkip()
			DbRLock()
			Assert.Equal(1, (INT)  ALen(DbRLockList()))
			Assert.Equal(2, (INT) DbRLockList()[1])
			DbCloseArea()
		RETURN

		// TECH-34OWD3RR1Z , DBF problems with filters
		[Fact, Trait("Category", "DBF")];
		METHOD DBFilter() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()
			IF System.IO.File.Exists(cDbf)
				System.IO.File.Delete(cDbf)
			END IF
			DbCreate(cDbf, {{"CFIELD","C",10,0}}, "DBFNTX", TRUE)
			DbAppend()
			FieldPut(1, "ABC")
			DbAppend()
			FieldPut(1, "DEF")
			DbAppend()
			FieldPut(1, "GHI")
			DbAppend()
			FieldPut(1, "JKL")
			Assert.Equal(4 , (INT) RecCount())
			Assert.Equal(4 , (INT) LastRec())
			DbCloseArea()

			DbUseArea(,,cDbf)
//			"Setting filter to GHI, should be one record:"
//			"Instead, record 1 and 3 are shown"
			DbSetFilter({||AllTrim(FIELD->CFIELD) == "GHI"})
			DbGoTop()
			LOCAL nCount := 0 AS INT
			DO WHILE .NOT. EoF()
				Assert.Equal(3 , (INT) RecNo())
				FieldGet(1)
				DbSkip(+1)
				nCount ++
			END DO
			Assert.Equal(1 , nCount)

			DbGoBottom()
			Assert.False( Eof() )
			nCount := 0
			DO WHILE .NOT. EoF()
				Assert.Equal(3 , (INT) RecNo())
				nCount ++
				FieldGet(1)
				DbSkip(+1)
			END DO
			Assert.Equal(1 , nCount)

			DbCloseArea()
		RETURN

		// TECH-8C175D53DN , DBRecordInfo() always returns NULL_OBJECT
		[Fact, Trait("Category", "DBF")];
		METHOD DBRecordInfo_test() AS VOID
			LOCAL cDbf AS STRING
			LOCAL l AS LOGIC
			cDbf := GetTempFileName()
			DbCreate(cDbf, {{"CFIELD","C",10,0}}, "DBFNTX", TRUE)
			DbAppend()
			FieldPut(1, "ABC")

			l := DbRecordInfo( DBRI_RECNO ) // exception
			Assert.True(l)
			l := DbRecordInfo( DBRI_DELETED ) // exception
			Assert.False(l)
			l := DbRecordInfo( DBRI_LOCKED ) // exception
			Assert.True(l)

			DbCloseArea()
		RETURN

		// TECH-NVMBVB2Y44 , NullReferenceExpetion with DBFieldInfo()
		[Fact, Trait("Category", "DBF")];
		METHOD DBFieldInfo_test() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()
			DbCreate(cDbf, {{"NFIELD","N",10,3}}, "DBFNTX", TRUE)
			DbAppend()
			FieldPut(1, 1.23)

			Assert.Equal("NFIELD",	DbFieldInfo( DBS_NAME , 1 ) ) // NullReferenceException
			Assert.Equal("N",		DbFieldInfo( DBS_TYPE , 1 ) )
			Assert.Equal(10,		(INT) DbFieldInfo( DBS_LEN , 1 ) )
			Assert.Equal(3,			(INT) DbFieldInfo( DBS_DEC , 1 ) )
			Assert.Equal(5,			(INT) DbFieldInfo( DBS_PROPERTIES , 1 ) )

			DbCloseArea()
		RETURN

		// TECH-52M9YX557W , DBRecordInfo() changes record pointer
		[Fact, Trait("Category", "DBF")];
		METHOD DBRecordInfo_test2() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()
			DbCreate(cDbf, {{"CFIELD","C",10,0}}, "DBFNTX", TRUE)
			DbAppend()
			FieldPut(1, "ABC")
			DbAppend()
			FieldPut(1, "DEF")

			DbGoTop()
			Assert.Equal(1, (INT) RecNo())
			Assert.Equal(FALSE, Eof())

//			 Any of the below cause the record pointer to go EOF
			Assert.False( DbRecordInfo(DBRI_DELETED , 0) )
			DbRecordInfo(DBRI_BUFFPTR , 0)
			DbRecordInfo(DBRI_RAWDATA , 0)

			Assert.Equal(1, (INT) RecNo())
			Assert.Equal(FALSE, Eof())

			DbCloseArea()
		RETURN

		// TECH-C6Y1L51V1O , DBContinue() not working correctly
		[Fact, Trait("Category", "DBF")];
		METHOD DBContinue_test() AS VOID
			LOCAL cDbf AS STRING
			cDbf := __FUNCTION__
			DbCreate(cDbf, {{"NFIELD","N",10,0}}, "DBFNTX", TRUE)
			DbAppend()
			FieldPut(1, 123)
			DbAppend()
			FieldPut(1, 456)
			DbAppend()
			FieldPut(1, 789)

			DbGoTop()
			Assert.True( DbLocate({||_FIELD->NFIELD > 300} , , , , TRUE) ) // DBSCOPEREST
			Assert.True( Found() )
			Assert.Equal(456.0 , (FLOAT)  FieldGet(1) )

//			DBContinue() returns TRUE (correct) but does not move record pointer at all
			Assert.True( DbContinue() )
			Assert.True( Found() )
			Assert.Equal( 789.0 , (FLOAT)  FieldGet(1) )

			Assert.True( DbContinue() )
			Assert.False( Found() )
			Assert.Equal( 0.0 , (FLOAT) FieldGet(1) )

			Assert.True( DbContinue() )
			Assert.False( Found() )
			Assert.Equal( 0.0 , (FLOAT) FieldGet(1) )

			DbCloseArea()
		RETURN


		// TECH-Y4UUA09473 , Problem on creating error for invalid RDD command
		[Fact, Trait("Category", "DBF")];
		METHOD DBAppendWithNoWorkarea() AS VOID
			DbCloseAll()
			Assert.False( DbAppend() )
		RETURN



		// TECH-U43F26KOT7 , Runtime error saving NULL_DATE to DATE dbf field
		[Fact, Trait("Category", "DBF")];
		METHOD Save_NULL_DATE() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()
			Assert.True(  DbCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 },;
			{"DFIELD" , "D" , 8 , 0 }}) )
			Assert.True( DbUseArea(,,cDbf) )
			DbAppend()
			FieldPut ( 1 , "B")
			DbAppend()
			FieldPut ( 1 , "A")
			Assert.True( DbCloseArea() )

			Assert.True( DbUseArea(,,cDbf) )
			LOCAL u AS USUAL
			u := FieldGet(2) // it should be a NULL_DATE
			Assert.True( u == NULL_DATE )
			FieldPut(2,u) // exception
			FieldPut(2,NULL_DATE) // exception
			Assert.True( FieldGet(2) == NULL_DATE )
			Assert.True(  DbCloseArea() )
		RETURN


		INTERNAL STATIC METHOD GetRecords() AS ARRAY
			LOCAL aResult AS ARRAY
			aResult := {}
			DbGoTop()
			DO WHILE .NOT. Eof()
				AAdd(aResult , AllTrim(FieldGet(1)))
				DbSkip()
			END DO
		RETURN aResult

		// TECH-965270UG7K , Workareas not being reused
		[Fact, Trait("Category", "DBF")];
		METHOD WorkareaNums() AS VOID
			LOCAL cFileName AS STRING
            DbCloseAll()
			cFileName := __FUNCTION__
			Assert.True( DbCreate(cFileName, {{"FLD1","C",10,0}}) )

			Assert.True( DbUseArea ( TRUE , , cFileName , "a1") )
			Assert.Equal( 1u , DbGetSelect() )
			Assert.True( DbCloseArea() )

			Assert.True( DbUseArea ( TRUE , , cFileName , "a2") )
			Assert.Equal( 1u , DbGetSelect() )
			Assert.True( DbCloseArea() )

			Assert.True( DbUseArea ( TRUE , , cFileName , "a3") )
			Assert.Equal( 1u , DbGetSelect() )
			Assert.True( DbCloseArea() )
		RETURN

		// FOX-ES1QLR6Y5L , dbRecordInfo ( DBRI_LOCKED ) always returns .f.
		[Fact, Trait("Category", "DBF")];
		METHOD DBRI_LOCKED_test() AS VOID
			LOCAL cFileName AS STRING
            TRY
			cFileName := __FUNCTION__
			SetExclusive ( FALSE )
			DbCreate ( cFileName , { {"id", "C", 5, 0} })

			DbUseArea ( , , cFileName )
			DbAppend()
			DbAppend()
			DbAppend()
			DbGoTop()

			Assert.True( DbRLock ( RecNo() ) )
			Assert.True( DbRecordInfo ( DBRI_LOCKED ) ) // Should show TRUE
			Assert.True( AScan ( DbRLockList() , RecNo() ) > 0 )

			DbSkip()
//			record 2 - no lock
			Assert.False( DbRecordInfo ( DBRI_LOCKED ) )
			Assert.False( AScan ( DbRLockList() , RecNo() ) > 0 )

			DbSkip()
			Assert.True( DbRLock ( RecNo() ) )
			Assert.True( DbRecordInfo ( DBRI_LOCKED ) ) // Should show TRUE
			Assert.True( AScan ( DbRLockList() , RecNo() ) > 0 )

			LOCAL a AS ARRAY
			a:= DbRLockList()
			Assert.Equal( 2 , (INT) ALen(a) )
			Assert.Equal( 1 , (INT) a[1] )
			Assert.Equal( 3 , (INT) a[2] )

            FINALLY
			DbCloseArea()

			SetExclusive ( TRUE ) // restore
            END TRY
		RETURN

		// TECH-XQES14W9J0 , Aliasxxx() funcs throw exceptions
		[Fact, Trait("Category", "DBF")];
		METHOD Alias_test() AS VOID
			DbCloseAll()
			Assert.True( Alias() == "" )
			Assert.True( Alias0() == "" )
			Assert.True( Alias0Sym() == "" )
		RETURN

		// TECH-IXV5X91A74 , DBCreate() problem after having opened a dbf in exclusive mode
		[Fact, Trait("Category", "DBF")];
		METHOD DBCreate_test() AS VOID
			LOCAL cFileName AS STRING
			cFileName := __FUNCTION__

			RddSetDefault("DBFNTX")

			DbCreate(cFileName, {{"FLD1","C",10,0}})
			DbUseArea(,,cFileName,,FALSE)
			DbCloseArea()

//			exception here
			Assert.True( DbCreate(cFileName, {{"FLD1","N",10,0}}) )

			Assert.True( DbUseArea(,,cFileName) )
			DbAppend()
			FieldPut(1 , -123)
			Assert.True( FieldGet(1) == -123 )
			Assert.True( DbCloseArea() )
		RETURN


		// TECH-ONPOSM84VS , Runtime exception on Error:ToString()
		[Fact, Trait("Category", "DBF")];
		METHOD DBError_test() AS VOID
			LOCAL cDbf AS STRING

			LOCAL FUNCTION _Throw(oError AS Exception) AS VOID
				THROW oError
			END FUNCTION

			cDbf := __FUNCTION__

			DbCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
			DbUseArea(,,cDbf,,TRUE)
			Assert.True( DbAppend() )
			Assert.True( DbUnLock() )
			LOCAL oErrorBlock AS CODEBLOCK
			oErrorBlock := ErrorBlock({|oError|_Throw(oError)})
			TRY
				? FieldPut ( 1 , "ABC") // record not locked
			CATCH e AS XSharp.Error
				? e:Message
				? e:GenCodeText
				? e:SubCodeText // check if this displays correctly
				? e:OSCodeText
				? e:ToString() // exception here
			FINALLY
				ErrorBlock(oErrorBlock)
				Assert.True( DbCloseArea() )
			END TRY
		RETURN




		// TECH-P956TFHW74, Cannot use fields in macro exceptions when they are named after a function
		[Fact, Trait("Category", "DBF")];
		METHOD Fields_Named_After_Funcs() AS VOID
			LOCAL cDbf AS STRING
			cDbf := __FUNCTION__

			DbCloseAll() // worakaroudn for previous test crashing

			DbCreate( cDBF , {{"LEFT" , "N" ,5 , 0 } , {"STR" , "C" ,5 , 0 } })
			DbUseArea(,"DBFNTX",cDBF,,FALSE)
			DbAppend()
			FieldPut(1,1)
			FieldPut(2,"A")
			DbAppend()
			FieldPut(1,11)
			FieldPut(2,"B")
			DbGoTop()
			Assert.True( DbCreateIndex(cDbf, "STR" ) )
			Assert.True( DbSetFilter(&("{||LEFT<10}")) )
			DbGoTop()
			DbSkip()
			Assert.True( Eof() )
			DbCloseArea()
		RETURN


		// TECH-R933ZKYT9Q, Problem creating index on dbf with "uninitialized" fields
		[Fact, Trait("Category", "DBF")];
		METHOD Uninitialized_fields() AS VOID
			LOCAL cDbf AS STRING
			DbCloseAll() // worakaroudn for previous test crashing

			cDbf := __FUNCTION__
			DbCreate( cDBF , {{"FIELDN" , "N" ,5 , 0 } })
			DbUseArea(,"DBFNTX",cDBF)
			DbAppend()
			FieldPut(1,1)
			DbAppend() // no field assigned
			DbCloseArea()

			DbUseArea(,"DBFNTX",cDBF)
			Assert.True( DbCreateIndex(cDbf, "FIELDN" ) )
			DbCloseArea()
		RETURN


		// TECH-5YKDFJWM4N, FLock() runtime exception when some records are already locked
		[Fact, Trait("Category", "DBF")];
		METHOD FLock_test() AS VOID
			LOCAL aDbf AS ARRAY
			LOCAL cDbf AS STRING
			DbCloseAll()

			cDbf := __FUNCTION__
		    aDbf := {{ "AGE" , "N" , 2 , 0 }}
			DbCreate( cDBF , aDbf)

			DbUseArea(,"DBFNTX",cDBF,,FALSE)
			DbAppend()
			DbAppend()
			DbAppend()
			DbCloseArea()

			Assert.True( DbUseArea( TRUE ,"DBFNTX",cDBF,"AREA1" ,TRUE ) )// open shared
			DbGoTop()
			Assert.True( DbRLock ( RecNo() ) ) // lock first record
			Assert.Equal( 1 , (INT) ALen ( DbRLockList() ) )

			Assert.True( DbUseArea( TRUE ,"DBFNTX",cDBF,"AREA2" ,TRUE ) ) // open shared
			DbGoBottom()
			Assert.True( DbRLock ( RecNo() ) ) // lock last record
			Assert.Equal( 1 , (INT) ALen ( DbRLockList() ) )

			Assert.False( Flock() )
			// what's the correct return value here??
			// Assert.Equal( 0 , (INT) ALen ( DBRLockList() ) )
			DbCloseAll()
		RETURN



		// TECH-TFF4K29132, VO-incompatible results with workarea numbering
		[Fact, Trait("Category", "DBF")];
		METHOD WorkareaNums_2() AS VOID
			LOCAL cDBF AS STRING
			LOCAL aFields AS ARRAY
			DbCloseAll()

			cDbf := __FUNCTION__

			RddSetDefault("DBFNTX")

		  	aFields := {{ "LAST"  , "C" ,  10 , 0 }  }

			DbCreate( cDbf , aFields , "DBFNTX" )
			DbUseArea( TRUE ,"DBFNTX",cDbf, "FOO1", TRUE)
			Assert.Equal(1 , (INT) DbGetSelect() ) // 1  ok

			DbUseArea( TRUE ,"DBFNTX",cDbf, "FOO2", TRUE)
			Assert.Equal(2 , (INT) DbGetSelect() )

			DbUseArea( TRUE ,"DBFNTX",cDbf, "FOO3", TRUE)
			Assert.Equal(3 , (INT) DbGetSelect() ) // 3  ok

			DbUseArea( TRUE ,"DBFNTX",cDbf, "FOO4", TRUE)
			Assert.Equal(4 , (INT) DbGetSelect() )

			Assert.Equal(3 , (INT) DbSetSelect ( 3 ) ) // 3 ok

			DbCloseAll()
			Assert.Equal(1 , (INT) DbGetSelect() ) // Shows  3 instead of 1
			Assert.Equal(1 , (INT) SELECT() )      // Shows  3 instead of 1

			DbUseArea( ,"DBFNTX",cDbf, "FOO1", TRUE)
			Assert.Equal(1 , (INT) DbGetSelect() ) // shows 3 instead of 1

			DbUseArea( TRUE ,"DBFNTX",cDbf, "FOO2", TRUE)
			Assert.Equal(2 , (INT) DbGetSelect() ) // shows 1 instead of 2 !

			DbCloseAll()
		RETURN


		// TECH-02UU54BZ37, Problem with DBApp() function
		[Fact, Trait("Category", "DBF")];
		METHOD DBApp_test() AS VOID
			LOCAL cDBF AS STRING
			LOCAL cDBFto AS STRING
			LOCAL aFields AS ARRAY
			LOCAL aValues AS ARRAY
			LOCAL i AS DWORD
			DbCloseAll()

			cDBF := __FUNCTION__+"from"
			cDBFto := __FUNCTION__+"to"

			RddSetDefault("DBFNTX")

			aFields := {{ "GRUPPE" , "C" , 30 , 0 } ,;
			{ "ID" , "C" , 5 , 0 } }

			aValues := { { "Grp1" , "00001" } ,;
			{ "Grp2" , "00002" } }

			DbCreate( cDbf , aFields , "DBFNTX" )
			DbUseArea(,"DBFNTX",cDbf,,FALSE)

			FOR i := 1 UPTO ALen ( aValues )
				DbAppend()
				FieldPut ( 1 , aValues [ i , 1 ] )
				FieldPut ( 2 , aValues [ i , 2 ] )
			NEXT

//			 --------- create To.dbf ----------
//			 error happens also without that
			AAdd ( aFields , { "ID2", "N" , 5 , 0 } )



			DbCreate( cDbfTo , aFields , "DBFNTX" )
			DbCloseAll()

			DbUseArea(,"DBFNTX",cDbfTo,,FALSE)
			Assert.True( DbApp ( cDbf ) )// ------- IndexOutofRangeException
			DbCloseAll()
		RETURN



		// TECH-3257Q835A2, Dbf() function incompatibilities
		[Fact, Trait("Category", "DBF")];
		METHOD DBF_func_test() AS VOID
			LOCAL cDBF AS STRING
//			DBF() throws an EG_NOTABLE error, but should return "" if no table is
//			opened in the current workarea
			Assert.Equal( "", DBF() ) // exception
			cDBF := __FUNCTION__
			? DbCreate( cDBF , { { "LAST" , "C" , 10 , 0 }})
			? DbUseArea(,"DBFNTX",cDBF , "FOOALIAS")
			Assert.Equal( "FOOALIAS", DBF() ) // Returns the fullpath instead of the alias name
			DbCloseAll()
			Assert.Equal( "", DBF() )// should return empty string
		RETURN


		// TECH-GT2ZK9PUK9, Inconsistent behavior of RDDName()
		[Fact, Trait("Category", "DBF")];
		METHOD RDDName_test() AS VOID
			LOCAL cDbf AS STRING
			RddSetDefault("DBFNTX")
			Assert.Equal("DBFNTX", RddName())
			cDBF := __FUNCTION__
			DbCreate( cDBF , {{"AAA","N",10,0}})
			DbUseArea(,"DBFNTX",cDBF,,FALSE)
			Assert.Equal("DBFNTX", RddName())
			DbCloseArea()

			RddSetDefault("DBFCDX")
			Assert.Equal("DBFCDX", RddName())
			cDBF := __FUNCTION__
			FErase(cDbf + ".cdx")
			DbCreate( cDBF , {{"AAA","N",10,0}})
			DbUseArea(,"DBFCDX",cDBF,,FALSE)
			Assert.Equal("DBFCDX", RddName())
			DbCloseArea()
		RETURN


		// TECH-2NVB6A63V4, DBMemoExt() shows ".DBT" only if a DBF is opened
		[Fact, Trait("Category", "DBF")];
		METHOD DBMemoExt_test() AS VOID
			LOCAL cDBF AS STRING

			RddSetDefault ( "DBFNTX" )
			Assert.Equal(".DBT" , DbMemoExt() ) // NULL_STRING instead of ".DBT"

			cDBF := __FUNCTION__
			DbCreate( cDBF , {{ "AGE" , "N" , 2 , 0 }})
			DbCloseAll()

			DbUseArea( TRUE ,"DBFNTX",cDBF,"FOO1",TRUE)
			Assert.Equal(".DBT" , DbMemoExt() ) // ".DBT" ok
			DbCloseAll()
			Assert.Equal(".DBT" , DbMemoExt() ) // NULL_STRING again instead of ".DBT"


			RddSetDefault ( "DBFCDX" )
			Assert.Equal(".FPT" , DbMemoExt() )

			FErase(cDbf + ".cdx")
			DbCreate( cDBF , {{ "AGE" , "N" , 2 , 0 }})
			DbCloseAll()

			DbUseArea( TRUE ,"DBFCDX",cDBF)
			Assert.Equal(".FPT" , DbMemoExt() )
			DbCloseAll()
			Assert.Equal(".FPT" , DbMemoExt() )
		RETURN


		// TECH-YO5LTEFO82, DBSetRelation() problem
		[Fact, Trait("Category", "DBF")];
		METHOD DBSetRelation_test() AS VOID
			LOCAL cDBF1, cDBf2 AS STRING
			LOCAL cINdex1, cINdex2 AS STRING
			LOCAL cPfad AS STRING
			LOCAL AFields, aValues AS ARRAY
			LOCAL i AS DWORD

			cPfad := __FUNCTION__
			cDBF1 := cPfad + "relation1"
			cDBf2 := cPfad + "relation2"
			FErase(cDBF1 + ".dbf")
			FErase(cDBF2 + ".dbf")

			cINdex1 := cPfad + "relation1"
			cINdex2 := cPfad + "relation2"
			FErase(cINdex1 + ".ntx")
			FErase(cINdex2 + ".ntx")
//			 ------- create Parent DBF --------------
			AFields := { { "ID" , "C" , 5 , 0 }}

			aValues := { "00002" , "00001" , "00003" }

			DbCreate( cDBF1 , AFields)
			DbUseArea(,"DBFNTX",cDBF1 )

			FOR i := 1 UPTO ALen ( aValues )
				DbAppend()
				FieldPut ( 1 , aValues [ i ] )
			NEXT

			DbCreateIndex( cINdex1, "ID" )

//			 ------- create Child DBF --------------
			AFields := { { "ID" , "C" , 5 , 0 } , { "TEXT1" , "C" ,20 , 0 }}
			aValues := { { "00002" , "Text1 00002" } , { "00001" , "Text2 00001" }, { "00001" , "Text1 00001"} ,;
			{ "00001" , "Text3 00001" } , {"00003" , "Text1 00003" } , { "00002" , "Text2 00002"} ,;
			{ "00003" , "Text3 00003" } , {"00002" , "Text3 00002" } , { "00001" , "Text4 00001"} ,;
			{ "00003" , "Text2 00003" } , {"00003" , "Text4 00003" } }

			DbCreate( cDBf2 , AFields)
			DbUseArea(,"DBFNTX",cDBf2 )

			FOR i := 1 UPTO ALen ( aValues )
				DbAppend()
				FieldPut ( 1 , aValues [ i , 1 ] )
				FieldPut ( 2 , aValues [ i , 2 ] )
			NEXT

			DbCreateIndex( cINdex2, "ID + TEXT1" )

			DbCloseAll()

//			 ------------------------
//			 open Parent DBF

			DbUseArea(TRUE ,"DBFNTX",cDBF1 )
			DbSetIndex( cINdex1 )
			DbSetOrder ( 1 )
			DbGoTop()

//			 open Child DBF
			DbUseArea(TRUE,"DBFNTX",cDBf2 )
			DbSetIndex( cINdex2 )
			DbSetOrder ( 1 )

			DbSetSelect ( 1 )
//			 set the relation to the common field ID
			Assert.True( DbSetRelation(2, {|| _FIELD->ID } , "ID" ) )

			LOCAL nOuter, nInner AS INT
			nOuter := 0
			DO WHILE ! a->Eof()
				nOuter ++
				nInner := 0
				DO WHILE a->FieldGet ( 1 ) == b->FieldGet ( 1 )
					nInner ++
//					excepion here. Removing it makes DO WHILE never end
//					? a->FieldGet ( 1 ) , b->FieldGet ( 1 ) ,b->FieldGet ( 2 )
					LOCAL c AS STRING
					c := a->FieldGet( 1 ) + b->FieldGet ( 1 ) + b->FieldGet ( 2 )
					Assert.Equal( String.Format("0000{0}0000{0}Text{1} 0000{0}         " , nOuter , nInner) , c )

					b->DbSkip(1)
				ENDDO
				a->DbSkip(1)
			ENDDO

			DbCloseAll()
		RETURN



		// TECH-1694P45N94, DBDelete() runtime exception with no records
		[Fact, Trait("Category", "DBF")];
		METHOD DBDelete_test() AS VOID
			LOCAL cDBF AS STRING

			RddSetDefault("DBFNTX")

			cDBF := __FUNCTION__

			CreateDatabase(cDbf , { { "ID" , "C" , 5 , 0 }})

			DbUseArea(,"DBFNTX",cDBF )
			DbCreateIndex(cDbf , "ID")

			DbGoTop()
			Assert.Equal(1 , (INT) RecNo() )
			Assert.Equal("     " , (STRING) FieldGet(1) )

			Assert.True( DbRLock() )
			Assert.True( DbDelete())
			Assert.True( DbUnLock())

			DbCloseArea()
		RETURN






		// TECH-P7S0R7P25H, DBMemoExt() incorrectly returns ""
		[Fact, Trait("Category", "DBF")];
		METHOD DBMemoExt_test2() AS VOID
			LOCAL cDbf AS STRING

			RddSetDefault ( "DBFNTX" )
			Assert.Equal(".DBT" , DbMemoExt() ) // ok ".DBT"
			RddSetDefault ( "DBFCDX" )
			Assert.Equal(".FPT" , DbMemoExt() ) // "" instead of ".FPT"

			RddSetDefault ( "DBFCDX" )
			Assert.Equal(".FPT" , DbMemoExt ( "DBFCDX" ) ) // "" instead of ".FPT"
			Assert.Equal(".DBT" , DbMemoExt ( "DBFNTX" ) ) // "" instead of ".DBT"

			RddSetDefault ( "DBFNTX" )
			Assert.Equal(".FPT" , DbMemoExt ( "DBFCDX" ) ) // "" instead of ".FPT"
			Assert.Equal(".DBT" , DbMemoExt ( "DBFNTX" ) ) // "" instead of ".DBT"

			cDBF := __FUNCTION__
			DbCreate( cDBF ,  {{"VE" , "C" , 3 , 0 }})

			FErase(cDbf + ".cdx")

			DbUseArea( TRUE,"DBFNTX",cDBF , "FOO1" , TRUE )
			Assert.Equal(".DBT" , (STRING) DbInfo ( DBI_MEMOEXT ) ) // returns ".FPT" instead of ".DBT"

			DbUseArea( TRUE,"DBFCDX",cDBF , "FOO2" , TRUE )
			Assert.Equal(".FPT" , (STRING) DbInfo ( DBI_MEMOEXT ) ) // returns "" instead of ".FPT"

			DbCloseAll()
		RETURN




		// TECH-0YI914Z4I2, Problem opening dbf with dbt via SetDefault()
		[Fact, Trait("Category", "DBF")];
		METHOD SetDefault_test() AS VOID
			LOCAL cPath, cDbf AS STRING
			LOCAL cDefault AS STRING
			RddSetDefault("DBFNTX")

			cPath := System.IO.Path.GetTempPath()
			IF .NOT. cPath:EndsWith("\")
				cPath += "\"
			END IF
			cDbf := "dbttest"
			FErase(cPath + cDBF + ".dbf")
			FErase(cPath + cDBF + ".ntx")
			FErase(cPath + cDBF + ".cdx")
			FErase(cPath + cDBF + ".dbt")
			FErase(cPath + cDBF + ".fpt")

			cDefault := GetDefault()
			SetDefault(cPath)

			Assert.True( DbCreate( cPath + cDbf , { { "ID" , "C" , 5 , 0 } , {"MEM" , "M" , 10 , 0}} ) )
//			 System.IO.FileNotFoundException
//			 Could not find file '<path_of_exe>\mydbf.DBT'.
			Assert.True( DbUseArea(,,cDbf ) )
			Assert.True( DbCloseArea()      )

			SetDefault(cDefault)
		RETURN


		[Fact, Trait("Category", "DBF")];
		METHOD FptFieldPut_test() AS VOID
			LOCAL cDbf AS STRING
			LOCAL aStruct AS ARRAY

			RddSetDefault("DBFCDX")
			cDbf := GetTempFileName()
			aStruct := {}
			FOR LOCAL n := 1 AS INT UPTO 10
				AAdd(aStruct , {"MEMO" + n:ToString() , "M" , 10 , 0})
            NEXT
            LOCAL cCdx AS STRING
            cCdx := Path.ChangeExtension(cDbf,".CDX")
            IF File(cCdx)
                FErase(cCdx)
            ENDIF
			DbCreate(cDbf , aStruct)
			DbUseArea(,,cDbf)
			DbAppend()
			DbAppend()
			DbAppend()

			FOR LOCAL i := 1 AS DWORD UPTO 10
				FOR LOCAL m := 1 AS DWORD UPTO 10
					FOR LOCAL n := 65 AS DWORD UPTO 97
						LOCAL nTextLen AS DWORD
						LOCAL nRec AS DWORD
						nTextLen := (m*i*2) % 2000
						nRec := i % 3 + 1
						LOCAL c AS STRING
						c := Replicate(Chr(n + m) , nTextLen )
						DbGoto(nRec)
						FieldPut(m , c)
						Assert.Equal(c ,(STRING)FieldGet(m))
						DbGoTop()
						FieldPut((m % 10) + 1, "asd")
						DbGoto(nRec)
						Assert.Equal(c ,(STRING)FieldGet(m))
					NEXT
				NEXT
			NEXT
			DbCloseArea()
		RETURN

		[Fact, Trait("Category", "DBF")];
		METHOD TestDBFFileStamps() AS VOID
			LOCAL cDbf AS STRING
			LOCAL dtDbf, dtIndex, dtMemo AS DateTime

			cDbf := GetTempFileName()
			CreateDatabase(cDbf, {{"TEST","C",10,0}} , {"aa","bb"})
			DbCloseArea()
			dtDbf := File.GetLastWriteTime(cDbf + ".dbf")
			Thread.Sleep(100)

			DbUseArea(,,cDbf)
			DbCloseArea()
			Assert.True(File.GetLastWriteTime(cDbf + ".dbf") == dtDbf)

			DbUseArea(,,cDbf)
			DbGoTop()
			DbGoBottom()
			DbSkip()
			DbGoto(1)
			DbCloseArea()
			Assert.True(File.GetLastWriteTime(cDbf + ".dbf") == dtDbf)



			RddSetDefault("DBFNTX")
			cDbf := GetTempFileName()
			CreateDatabase(cDbf, {{"TEST","C",10,0},{"TEST2","M",10,0}} , {"aa","bb"})
			FieldPut(2 ,"memo")
			DbCreateIndex(cDbf,"TEST + TEST2")
			DbCloseArea()
			dtDbf := File.GetLastWriteTime(cDbf + ".dbf")
			dtIndex := File.GetLastWriteTime(cDbf + ".ntx")
			dtMemo := File.GetLastWriteTime(cDbf + ".dbt")
			Thread.Sleep(100)

			DbUseArea(,,cDbf)
			DbGoBottom()
			DbGoTop()
			DbSkip(-1)
			DbGoto(1)
			DbCloseArea()
			Assert.True(File.GetLastWriteTime(cDbf + ".dbf") == dtDbf)
			Assert.True(File.GetLastWriteTime(cDbf + ".dbt") == dtMemo)
			Assert.True(File.GetLastWriteTime(cDbf + ".ntx") == dtIndex)



			RddSetDefault("DBFCDX")
			cDbf := GetTempFileName()
			CreateDatabase(cDbf, {{"TEST","C",10,0},{"TEST2","M",10,0}} , {"aa","bb"})
			FieldPut(2 ,"memo")
			DbCreateIndex(cDbf,"TEST + TEST2")
			DbCloseArea()
			dtDbf := File.GetLastWriteTime(cDbf + ".dbf")
			dtIndex := File.GetLastWriteTime(cDbf + ".cdx")
			dtMemo := File.GetLastWriteTime(cDbf + ".fpt")
			Thread.Sleep(100)

			DbUseArea(,,cDbf)
			DbGoBottom()
			DbGoTop()
			DbSkip(-1)
			DbGoto(1)
			DbCloseArea()
			Assert.True(File.GetLastWriteTime(cDbf + ".dbf") == dtDbf)
			Assert.True(File.GetLastWriteTime(cDbf + ".fpt") == dtMemo)
			Assert.True(File.GetLastWriteTime(cDbf + ".cdx") == dtIndex)
		RETURN


		[Fact, Trait("Category", "DBF")];
		METHOD TestDBFSize() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()
			CreateDatabase(cDbf, {{"TEST","C",10,0}})
			Assert.Equal(67, (INT)FileInfo{cDbf + ".dbf"}:Length)
		RETURN


		[Fact, Trait("Category", "DBF")];
		METHOD TestBofWithOrdScope() AS VOID
			LOCAL cDbf AS STRING

			cDbf := GetTempFileName()
			RddSetDefault("DBFCDX")

			CreateDatabase(cDbf, {{"CFIELD","C",1,0}} , {"S","S","N","N","N","S","S"})
			DbCreateIndex(cDbf , "CFIELD")
			DbCloseArea()

			DbUseArea( TRUE ,,cDBF )

			OrdScope(TOPSCOPE, "S")
			OrdScope(BOTTOMSCOPE, "S")

			LOCAL cTest := "" AS STRING
			DbGoTop()
			DO WHILE ! Eof()
				cTest += AsString(RecNo())
				DbSkip ( 1 )
			ENDDO
			cTest += AsString(RecNo())

			Assert.Equal("12678" , cTest)

			Assert.Equal(8 , (INT)RecNo())
			Assert.True(Eof())
			Assert.False(Bof())

			DbSkip(-1)
			Assert.Equal(7 , (INT)RecNo())
			Assert.False(Eof())
			Assert.False(Bof())
			DbSkip(-1)
			Assert.Equal(6 , (INT)RecNo())
			Assert.False(Eof())
			Assert.False(Bof())
			DbSkip(-1)
			Assert.Equal(2 , (INT)RecNo())
			Assert.False(Eof())
			Assert.False(Bof())
			DbSkip(-1)
			Assert.Equal(1 , (INT)RecNo())
			Assert.False(Eof())
			Assert.False(Bof())
			DbSkip(-1)
			Assert.Equal(1 , (INT)RecNo())
			Assert.False(Eof())
			Assert.True(Bof())

			DbCloseArea()
		RETURN



		[Fact, Trait("Category", "DBF")];
		METHOD TestMemoDbt() AS VOID
			// https://github.com/X-Sharp/XSharpPublic/issues/776
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()

			LOCAL FUNCTION GetLastByte() AS INT
				LOCAL aBytes AS BYTE[]
				aBytes := System.IO.File.ReadAllBytes(cDbf + ".dbt")
				RETURN aBytes[aBytes:Length]
			END FUNCTION

			RddSetDefault("DBFNTX")

			CreateDatabase(cDbf, {{"CFIELD","C",1,0},{"MFIELD","M",10,0}} )
			Assert.Equal(0, GetLastByte())

			DbUseArea( TRUE ,,cDBF )
			DbAppend()
			FieldPut(2,"123")
			DbCloseArea()

			Assert.Equal(26, GetLastByte())

			DbUseArea( TRUE ,,cDBF )
			DbGoBottom()
			LOCAL c123 AS STRING
			c123 := FieldGet(2)
			Assert.Equal(3, c123:Length)
			Assert.Equal("123", c123)
			DbCloseArea()

			Assert.Equal(26, GetLastByte())
		RETURN


		[Fact, Trait("Category", "DBF")];
		METHOD TestMemoDbt2() AS VOID
			LOCAL cDbf, cUpdate1, cUpdate2 AS STRING
			cDbf := GetTempFileName()

			RddSetDefault("DBFNTX")

			DbfTests.CreateDatabase(cDbf, {{"MFIELD","M",10,0},{"CFIELD","C",10,0}}, {"rec1","rec2"})
			DbCloseArea()

			DbUseArea(TRUE,,cDbf,"alias1",TRUE)
			DbUseArea(TRUE,,cDbf,"alias2",TRUE)
			alias1->DbAppend()
			alias1->FieldPut(1,"rec3")
//			alias1->DbCommit() // not making a difference
//			alias1->DbUnLock()

			alias2->DbGoto(1)
			alias2->DbRLock()
			cUpdate1 := Repl("A long update that should be visible only in record 1",10)
			alias2->FieldPut(1, cUpdate1)
//			alias2->DbCommit() // not making a difference
//			alias2->DbUnLock()

			DbCloseAll()


			DbUseArea(TRUE,,cDbf,"alias1",TRUE)
			DbGoTop()
			Assert.Equal(cUpdate1, (STRING)FieldGet(1))
			DbSkip()
			Assert.Equal("rec2", (STRING)FieldGet(1))
			DbGoBottom()
			Assert.Equal("rec3", (STRING)FieldGet(1))

			// updating record 3 only...
			cUpdate2 := "Putting a new value in record 3 only"
			RLock()
			FieldPut(1, cUpdate2)

			DbGoTop()
			Assert.Equal(cUpdate1, (STRING)FieldGet(1))
			DbSkip()
			Assert.Equal("rec2", (STRING)FieldGet(1))
			DbGoBottom()
			Assert.Equal(cUpdate2, (STRING)FieldGet(1))

			DbCloseArea()

		RETURN



		[Fact, Trait("Category", "DBF")];
		METHOD TestFieldPutBytes_ntx() AS VOID
			LOCAL cDbf AS STRING
			RddSetDefault("DBFNTX")

			cDbf := GetTempFileName()
			DbCreate(cDbf, {{"FLD1","C",5,0},{"MEMO1","M",10,0}})

			DbUseArea(,,cDbf)
			DbAppend()
			FieldPutBytes( 1, <BYTE>{1,2,3,4,5} )
			FieldPutBytes( 2, <BYTE>{6,7,8} )

			LOCAL a AS BYTE[]
			a := FieldGetBytes(1)
			Assert.Equal(5, a:Length)
			FOR LOCAL n := 1 AS INT UPTO 5
				Assert.Equal((BYTE)n, a[n])
			NEXT

			a := FieldGetBytes(2)
			Assert.Equal(3, a:Length)
			FOR LOCAL n := 1 AS INT UPTO 3
				Assert.Equal((BYTE)(n+5), a[n])
			NEXT

			a := BYTE[]{10_000}
			FOR LOCAL n := 1 AS INT UPTO a:Length
				a[n] := (BYTE)(n % 256)
                IF a[n] == 26   // This is the EOF char. Needs to be replaced with something harmless
                    a[n] := 42
                ENDIF
			NEXT
			FieldPutBytes( 2, a )
			a := FieldGetBytes(2)
			Assert.Equal(10_000, a:Length)
			LOCAL lSame := TRUE AS LOGIC
			FOR LOCAL n := 1 AS INT UPTO 10_000
				IF a[n] != (BYTE)(n % 256)
                    IF n % 256 == 26
                        IF a[n] == 42
                            LOOP
                        ENDIF
                    ENDIF
					lSame := FALSE
					EXIT
				END IF
			NEXT
			Assert.True( lSame )

			DbCloseArea()


		[Fact, Trait("Category", "DBF")];
		METHOD VariousDbtTests() AS VOID
			LOCAL cDbf AS STRING
			LOCAL nRecords := 100 AS DWORD
			LOCAL aSizes := <DWORD>{1,5,20,2000,500,100,200,30,20,10,5000,1,500,5,5,700,700,150,2000,15000,1000,10,20} AS DWORD[]
			LOCAL aValues AS ARRAY
			aValues := ArrayCreate(nRecords)
			FOR LOCAL n := 1 AS DWORD UPTO nRecords
				aValues[n] := Replicate(Left(n:ToString(),1) , aSizes[n % aSizes:Length + 1])
			NEXT

			LOCAL FUNCTION CheckValues() AS VOID
				dbt1->DbGoTop()
				FOR LOCAL n := 1 AS DWORD UPTO nRecords
					Assert.Equal( (STRING) aValues[n], Trim(dbt1->FieldGet(2)) )
					dbt1->DbSkip()
				NEXT
			END FUNCTION

			RddSetDefault("DBFNTX")

			cDbf := GetTempFileName()
			DbCreate(cDbf, {{"FLD1","C",3,0} , {"MEMOFLD2","M",10,0}})

			DbUseArea(TRUE,,cDbf,"dbt1",TRUE)
			DbUseArea(TRUE,,cDbf,"dbt2",TRUE)
			FOR LOCAL n := 1 AS INT UPTO nRecords
				IF n % 2 == 1
					dbt1->DbAppend()
					dbt1->FieldPut(2, aValues[n])
				ELSE
					dbt2->DbAppend()
					dbt2->FieldPut(2, aValues[n])
				END IF
			NEXT
			dbt1->DbGoTop()
			dbt2->DbGoTop()

			CheckValues()

			FOR LOCAL i := 1 AS INT UPTO 20
				// shift memo sizes
				AAdd(aValues, aValues[1])
				ADel(aValues, 1)
				ASize(aValues , nRecords)
				FOR LOCAL n := 1 AS INT UPTO nRecords
					IF n % 2 == 1
						dbt1->DbGoto(n)
						dbt1->RLock()
						dbt1->FieldPut(2, aValues[n])
					ELSE
						dbt2->DbGoto(n)
						dbt2->RLock()
						dbt2->FieldPut(2, aValues[n])
					END IF
					dbt1->DbUnLock()
					dbt2->DbUnLock()
				NEXT
				dbt1->DbGoTop()
				dbt2->DbGoTop()

				CheckValues()
			NEXT

			dbt1->DbCloseArea()
			dbt2->DbCloseArea()


		[Fact, Trait("Category", "DBF")];
		METHOD CreateFromTestsDBFNTX() AS VOID
         // https://github.com/X-Sharp/XSharpPublic/issues/917
            RddSetDefault ( "DBFNTX" ) // DBFNTX
            _CreateFromTests()

		[Fact, Trait("Category", "DBF")];
		METHOD CreateFromTestsDBFCDX() AS VOID
         // https://github.com/X-Sharp/XSharpPublic/issues/917
            RddSetDefault ( "DBFCDX" ) // DBFNTX
            _CreateFromTests()
        METHOD _CreateFromTests() AS VOID
            LOCAL cDbf, cDbf2, cDbf3 as STRING
            LOCAL aStruct as ARRAY
	        cDbf := GetTempFileName()
            cDbf2 := GetTempFileName()
            cDbf3 := GetTempFileName()

	        Assert.True(DbCreate( cDBF , { { "LAST" , "C" , 1025 , 0 }}))
	        Assert.True(DbUseArea( ,, cDbf ))
            aStruct := DbStruct()
	        Assert.True(1 == aStruct:Length)
            Assert.True("LAST" == aStruct[1,1])
            Assert.True("C" == aStruct[1,2])
            Assert.True(1025 == aStruct[1,3])
            Assert.True(0 == aStruct[1,4])
	        ?
//	        // same as COPY STRUCTURE EXTENDED TO
	        Assert.True(DbCopyXStruct( cDbf2 ))

	        Assert.True(DbCloseArea())

            Assert.True(DbUseArea( ,, cDbf2 ))

	        Assert.True(1 == LastRec())
            Assert.True("LAST" == Trim(FieldGet(1)))
            Assert.True("C" == FieldGet(2))
            Assert.True(1 == FieldGet(3))
            Assert.True(4 == FieldGet(4))
            Assert.True(DbCloseArea())

 	        Assert.True(_DbCreate( cDbf3 , cDbf2 )) // -> falls back to DBCreate() etc.
	        DbCloseArea()

            DbUseArea( ,, cDbf3 )
            aStruct := DbStruct()
	        Assert.True(1 == aStruct:Length)
            Assert.True("LAST" == aStruct[1,1])
            Assert.True("C" == aStruct[1,2])
            Assert.True(1025 == aStruct[1,3])
            Assert.True(0 == aStruct[1,4])


	        DbCloseArea()

	        RETURN


		STATIC INTERNAL METHOD GetTempFileName() AS STRING
            STATIC nCounter AS LONG
            ++nCounter
		    RETURN GetTempFileName("testdbf"+NTrim(nCounter))
		STATIC PRIVATE METHOD GetTempFileName(cFileName AS STRING) AS STRING
			// we may want to put them to a specific folder etc
		    IF File(cFileName+".DBF")
                FErase(FPathName())
            ENDIF
            IF File(cFileName+".DBT")
                FErase(FPathName())
            ENDIF
            IF File(cFileName+".FPT")
                FErase(FPathName())
            ENDIF
            IF File(cFileName+".CDX")
                FErase(FPathName())
            ENDIF
            IF File(cFileName+".NTX")
                FErase(FPathName())
            ENDIF
		    RETURN cFileName
		STATIC INTERNAL METHOD CreateDatabase(cFileName AS STRING, aFields AS ARRAY) AS VOID
			CreateDatabase(cFileName, aFields , {})
		STATIC INTERNAL METHOD CreateDatabase(cFileName AS STRING, aFields AS ARRAY, aValues AS ARRAY) AS VOID
            IF File(cFileName + IndexExt())
			    FErase ( FPathName() )
            ENDIF
            IF File(cFileName + ".fpt")
			    FErase ( FPathName() )
            ENDIF
			IF File ( cFileName + ".dbt" )
			    FErase ( FPathName() )
            ENDIF
			DbCreate( cFileName , aFields)
			IF ALen(aValues) == 0
				RETURN
			END IF
			DbUseArea(,,cFileName)
			FOR LOCAL i := 1 AS DWORD UPTO ALen ( aValues )
				DbAppend()
				FieldPut (1 , aValues[i])
            NEXT
            DbGoTop()
		RETURN


	END CLASS
END NAMESPACE
