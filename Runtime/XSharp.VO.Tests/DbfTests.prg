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

BEGIN NAMESPACE XSharp.VO.Tests

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
		METHOD DBAppend_Exclusive() AS VOID
			LOCAL aFields AS ARRAY
			LOCAL cFileName AS STRING
			aFields := {{"TEST","C",10,0}}
			cFileName := "DBAppend_Exclusive"
	
			Assert.True(  DbCreate(cFileName , aFields , "DBFNTX")  )
			Assert.True(  DbUseArea(,"DBFNTX",cFileName,,FALSE) )
			Assert.True(  RecCount() == 0 )
			Assert.True(  DbAppend() )
			FieldPut(1 , "test")
			Assert.True(  AllTrim(FieldGet(1)) == "test" )
			Assert.True(  DbCloseArea() )
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
			cDbf := "testappend.DbF"
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
			cDbf := "testtwice.DbF"
			Assert.True(  DbCreate(cDbf , { {"TEST","C",10,0} }) )
	
			// shared mode
			Assert.True( DbUseArea(, , cDbf , , FALSE) )
			Assert.True( DbCloseArea() )
	
			Assert.True( DbUseArea(, , cDbf , , FALSE) )
			Assert.True( DbCloseArea() )
		RETURN


		// TECH-K3TL5J8M7V
		[Fact, Trait("Category", "DBF")];
		METHOD VODBInfo() AS VOID
			
			LOCAL cFileName AS STRING
			cFileName := "test.DBF"
			DbCreate(cFileName , { {"CFIELD","C",10,0} })
			DbUseArea(,,cFileName)
			DbAppend()
			
			LOCAL u := NIL AS USUAL
			VoDbSkip(-1)
			
			VoDbInfo(DBI_FULLPATH , REF u)
			LOCAL c AS STRING
			c := u
			? c
			Assert.True(c:EndsWith("test.DBF") .AND. c:Contains(":\"))
			VoDbInfo(DBI_DB_VERSION , REF u)
			Assert.True(SLen(u) > 1)
			VoDbInfo(DBI_ALIAS , REF u)
			Assert.Equal("test" , u)
			
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
			FieldPut(1 , 12.34)
			Assert.Equal(12.34 , (FLOAT) FieldGet(1))

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
		METHOD DBRLockList() AS VOID
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


		// TECH-8HN2I0UUNA , Index file not correctly created when dbf is opened in SHARED mode
		[Fact, Trait("Category", "DBF")];
		METHOD Shared_Ntx() AS VOID
			LOCAL cDbf AS STRING
			LOCAL cNtx AS STRING
			
			RddSetDefault("DBFNTX")

			cDbf := GetTempFileName()
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


		// TECH-588I8LB67J , Problems with NTX indexes
		[Fact, Trait("Category", "DBF")];
		METHOD Ntx_Issues() AS VOID
			LOCAL cDbf AS STRING
			LOCAL cNtx AS STRING
			LOCAL aResult AS ARRAY

			RddSetDefault("DBFNTX")
			
			cDbf := GetTempFileName()
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
			aResult := GetRecords()
//			should be ABC, DEF, GHI, K
			Assert.True( aResult[1] == "ABC")
			Assert.True( aResult[2] == "DEF")
			Assert.True( aResult[3] == "GHI")
			Assert.True( aResult[4] == "K")
			
			DbGoTop()
			DbSkip()
			FieldPut(1,"HHH")
			aResult := GetRecords()
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
			aResult := GetRecords()
//			should be ABC, DEF, GHI, K
			Assert.True( aResult[1] == "ABC")
			Assert.True( aResult[2] == "DEF")
			Assert.True( aResult[3] == "GHI")
			Assert.True( aResult[4] == "K")
			
			DbGoTop()
			DbSkip()
			Assert.True(RLock())
			FieldPut(1,"III")
			aResult := GetRecords()
//			should be ABC, GHI, III, K
			Assert.True( aResult[1] == "ABC")
			Assert.True( aResult[2] == "GHI")
			Assert.True( aResult[3] == "III")
			Assert.True( aResult[4] == "K")
			
			Assert.True( DbCloseArea() )
		RETURN

		PRIVATE STATIC METHOD GetRecords() AS ARRAY
			LOCAL aResult AS ARRAY
			aResult := {}
			DbGoTop()
			DO WHILE .NOT. Eof()
				AAdd(aResult , AllTrim(FieldGet(1)))
				DbSkip()
			END DO
		RETURN aResult

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

		// TECH-965270UG7K , Workareas not being reused
		[Fact, Trait("Category", "DBF")];
		METHOD WorkareaNums() AS VOID
			LOCAL cFileName AS STRING
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
			FieldPut(1 , 123)
			Assert.True( FieldGet(1) == 123 )
			Assert.True( DbCloseArea() )
		RETURN


		// TECH-ONPOSM84VS , Runtime exception on Error:ToString()
		[Fact, Trait("Category", "DBF")];
		METHOD DBError_test() AS VOID
			LOCAL cDbf AS STRING
			
			cDbf := __FUNCTION__
			
			DbCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
			DbUseArea(,,cDbf,,TRUE)
			Assert.True( DbAppend() )
			Assert.True( DbUnLock() )
			TRY
				? FieldPut ( 1 , "ABC") // record not locked
			CATCH e AS XSharp.Error
				? e:Message
				? e:GenCodeText
				? e:SubCodeText // check if this displays correctly
				? e:OSCodeText
				? e:ToString() // exception here
			FINALLY
				Assert.True( DbCloseArea() )
			END TRY
		RETURN


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
			Assert.Equal( (INT) OrdScope(TOPSCOPE, 5) , 5) // NULL
			Assert.Equal( (INT) OrdScope(BOTTOMSCOPE, 10) , 10) // NULL
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
			VoDbOrderInfo( DBOI_SCOPETOP, "", NIL, REF u )
			VoDbOrderInfo( DBOI_SCOPEBOTTOM, "", NIL, REF u )
		
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
			VoDbOrderInfo( DBOI_SCOPETOP, "", NIL, REF u )
			VoDbOrderInfo( DBOI_SCOPEBOTTOM, "", NIL, REF u )
		
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
			VoDbOrderInfo( DBOI_SCOPETOP, "", NIL, REF u )
			VoDbOrderInfo( DBOI_SCOPEBOTTOM, "", NIL, REF u )
		
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
			CreateDatabase(cDbf , { { "LAST" , "C" , 20 , 0 }} , { "a" , "d" , "f", "c" })

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



		STATIC PRIVATE METHOD GetTempFileName() AS STRING
            STATIC nCounter AS LONG
            ++nCounter
		    RETURN GetTempFileName("testdbf"+NTrim(nCounter))
		STATIC PRIVATE METHOD GetTempFileName(cFileName AS STRING) AS STRING
			// we may want to put them to a specific folder etc
		RETURN cFileName
		STATIC INTERNAL METHOD CreateDatabase(cFileName AS STRING, aFields AS ARRAY) AS VOID
			CreateDatabase(cFileName, aFields , {})
		STATIC INTERNAL METHOD CreateDatabase(cFileName AS STRING, aFields AS ARRAY, aValues AS ARRAY) AS VOID
			FErase ( cFileName + IndexExt() )
			DbCreate( cFileName , aFields)
			IF ALen(aValues) == 0
				RETURN
			END IF
			DbUseArea(,,cFileName)
			FOR LOCAL i := 1 AS DWORD UPTO ALen ( aValues )
				DbAppend()
				FieldPut (1 , aValues[i])
			NEXT
		RETURN
			
	END CLASS
END NAMESPACE
