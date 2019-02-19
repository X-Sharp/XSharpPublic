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

	CLASS DbfCdxTests

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
			Assert.True(  DBCreate(cFileName_NoExt , aFields , "DBFCDX")  )
			Assert.True(  System.IO.File.Exists(cFileName_WithExt) )
			
			IF System.IO.File.Exists(cFileName_WithExt)
				System.IO.File.Delete(cFileName_WithExt)
			END IF
			Assert.True(  DBCreate(cFileName_WithExt , aFields , "DBFCDX")  )
			Assert.True(  System.IO.File.Exists(cFileName_WithExt) )
	
			cFileName_WithExt := cFileName_NoExt + ".none"
			IF System.IO.File.Exists(cFileName_WithExt)
				System.IO.File.Delete(cFileName_WithExt)
			END IF
			Assert.True(  DBCreate(cFileName_WithExt , aFields , "DBFCDX")  )
			Assert.True(  System.IO.File.Exists(cFileName_WithExt) )
		RETURN
	
		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBAppend_Exclusive() AS VOID
			LOCAL aFields AS ARRAY
			LOCAL cFileName AS STRING
			aFields := {{"TEST","C",10,0}}
			cFileName := "DBAppend_Exclusive"
	
			Assert.True(  DBCreate(cFileName , aFields , "DBFCDX")  )
			Assert.True(  DBUseArea(,"DBFCDX",cFileName,,FALSE) )
			Assert.True(  RecCount() == 0 )
			Assert.True(  DBAppend() )
			FieldPut(1 , "test")
			Assert.True(  AllTrim(FieldGet(1)) == "test" )
			Assert.True(  DBCloseArea() )
		RETURN
	
		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBAppend_Shared() AS VOID
			LOCAL aFields AS ARRAY
			LOCAL cFileName AS STRING
			aFields := {{"TEST","C",10,0}}
			cFileName := "DBAppend_Shared"
	
			Assert.True(  DBCreate(cFileName , aFields , "DBFCDX")  )
			Assert.True(  DBUseArea(,"DBFCDX",cFileName,,TRUE) )
			Assert.True(  RecCount() == 0 )
			Assert.True(  DBAppend() )
			FieldPut(1 , "test")
			Assert.True(  AllTrim(FieldGet(1)) == "test" )
			Assert.True(  DBCloseArea() )
		RETURN
	
		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBAppend_more() AS VOID
		LOCAL cDbf AS STRING
			cDbf := "testappend.DbF"
			RDDSetDefault( "DBFCDX" )
			Assert.True(  DBCreate(cDbf , { {"TEST","C",10,0} }) )
			// Appending in exclusive mode:
			Assert.True( DBUseArea(, , cDbf , "alias1" , FALSE) )
			Assert.True( DBAppend() )
			Assert.True( RecCount() == 1 )
			FieldPut(1, "test") // ok
			Assert.True( AllTrim(FieldGet(1)) == "test" )
			Assert.True( DBCloseArea() )
	
			// Appending in SHARED mode:
			Assert.True( DBUseArea(, , cDbf , "alias2" , TRUE) )
			Assert.True( RecCount() == 1 )
			Assert.True( DBAppend() )// returns true but does not append record
			Assert.True( RecCount() == 2 )
			FieldPut(1, "test2") // ok
			Assert.True( AllTrim(FieldGet(1)) == "test2" )
			Assert.True( DBCloseArea() )
		RETURN
	
		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBUseArea_same_file_twice() AS VOID
		LOCAL cDbf AS STRING
			RDDSetDefault( "DBFCDX" )
			cDbf := "testtwice.DbF"
			Assert.True(  DBCreate(cDbf , { {"TEST","C",10,0} }) )
	
			// shared mode
			Assert.True( DBUseArea(, , cDbf , , FALSE) )
			Assert.True( DBCloseArea() )
	
			Assert.True( DBUseArea(, , cDbf , , FALSE) )
			Assert.True( DBCloseArea() )
		RETURN


		// TECH-K3TL5J8M7V
		[Fact, Trait("Category", "DBF")];
		METHOD VODBInfo() AS VOID
			
			LOCAL cFileName AS STRING
			cFileName := GetTempFileName("test")

			RDDSetDefault("DBFCDX")

			DBCreate(cFileName , { {"CFIELD","C",10,0} })
			DBUseArea(,,cFileName)
			DBAppend()
			
			LOCAL u AS USUAL
			VODBSkip(-1)
			
			VODBInfo(DBI_FULLPATH , REF u)
			LOCAL c AS STRING
			c := u
			? c
			Assert.True(c:EndsWith("test.DBF") .AND. c:Contains(":\"))
			VODBInfo(DBI_DB_VERSION , REF u)
			Assert.True(SLen(u) > 1)
			VODBInfo(DBI_ALIAS , REF u)
			Assert.Equal("test" , u)
			
			VODBInfo(DBI_BOF , REF u)
			Assert.Equal(TRUE , (LOGIC) u)
			VODBInfo(DBI_EOF , REF u)
			Assert.Equal(FALSE ,(LOGIC)  u)
			VODBInfo(DBI_ISANSI , REF u)
			Assert.Equal(SetAnsi() ,(LOGIC)  u)
			VODBInfo(DBI_FCOUNT , REF u)
			Assert.Equal(1, (LONG)  u)
			VODBInfo(DBI_READONLY , REF u)
			Assert.Equal(FALSE,(LOGIC)  u)
			
			DBCloseArea()
		RETURN

		// TECH-E6Y9GNHB99
		[Fact, Trait("Category", "DBF")];
		METHOD AppendShared() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName("testAppendShared")
			RDDSetDefault( "DBFCDX" )
			DBCreate(cDbf , { {"TEST","C",10,0} })
			
//			Appending in exclusive mode:
			DBUseArea(, , cDbf , "alias2" , FALSE)
			Assert.True( DBAppend() )
			Assert.Equal( 1 , RecCount() )
			FieldPut(1, "test") // ok
			DBCloseArea()
			
//			Appending in SHARED mode:
			DBUseArea(, , cDbf , "alias2" , TRUE)
			Assert.True( DBAppend() ) // returns true but does not append record
			Assert.Equal( 2 , RecCount() ) // returns 1, wrong
			DBCloseArea()
		RETURN

		// TECH-6U40UQ0JV3
		[Fact, Trait("Category", "DBF")];
		METHOD AliasNameReuse() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName("testdbf")
			
			RDDSetDefault( "DBFCDX" )
			
			DBCreate(cDbf , { {"TEST","C",10,0} })
			
//			opening and closing once
			DBUseArea(, , cDbf , , FALSE)
			DBCloseArea()
			
//			opening and closing again
			Assert.True( DBUseArea(, , cDbf , , FALSE) )
			Assert.True( DBCloseArea() )
		RETURN

		// TECH-SAK5955895
		[Fact, Trait("Category", "DBF")];
		METHOD SavingDecimalValues() AS VOID
			LOCAL cFileName AS STRING
			cFileName := GetTempFileName()

			RDDSetDefault("DBFCDX")

			DBCreate(cFileName, {{"FLD1","N",10,2},{"FLD2","N",10,0}})
			DBUseArea(,,cFileName)
			DBAppend()

			SetDecimalSep(Asc(","))
			FieldPut(1 , 12.34) // not saved in the dbf
			Assert.Equal(12.34 , (FLOAT) FieldGet(1)) // 0,00

			SetDecimalSep(Asc("."))
			FieldPut(1 , 12.34)
			Assert.Equal(12.34 , (FLOAT) FieldGet(1))

			DBCloseArea()
		RETURN

		// TECH-C8WB52EA4A , Runtime exception when reading from a float field, after writing to it and DBCommit()
		[Fact, Trait("Category", "DBF")];
		METHOD DBCommitAfterFieldput() AS VOID
			LOCAL cFileName AS STRING
			cFileName := GetTempFileName()

			RDDSetDefault("DBFCDX")

			DBCreate(cFileName, {{"FLD1","N",10,4}})
			DBUseArea( , , cFileName , "tempalias")
			DBAppend()
			DBCloseArea()
			
			DBUseArea( , , cFileName)
			FieldPut(1 , 46.11) // ! a float isn/t stored !
			DBCommit()
			Assert.Equal(46.11 , (FLOAT) FieldGet(1)) // runtime exception
			DBCloseArea()
		RETURN
	
	
		// TECH-J61EXJ870D , FieldName() and FieldSym() throw an exception with incorrect field no
		[Fact, Trait("Category", "DBF")];
		METHOD FieldNameSym() AS VOID
			LOCAL cFileName AS STRING
			cFileName := GetTempFileName()

			RDDSetDefault("DBFCDX")

			DBCreate(cFileName, {{"FLD1","N",10,4}})
			DBUseArea( , , cFileName , "tempalias")
			DBAppend()
			Assert.Equal("", FieldName(100)) // exception
			Assert.Equal("", FieldName(0))
			Assert.Equal(NULL_SYMBOL, FieldSym(100))
			Assert.Equal(NULL_SYMBOL, FieldSym(0))
			DBCloseArea()
		RETURN
	
	
	
		// TECH-560ANYQI2P , DBRLockList() always returns zero
		[Fact, Trait("Category", "DBF")];
		METHOD DBRLockList() AS VOID
			LOCAL cFileName AS STRING
			cFileName := GetTempFileName()

			RDDSetDefault("DBFCDX")

			DBCreate(cFileName, {{"FLD1","C",10,0}})
			DBUseArea( , , cFileName , "tempalias" , TRUE)
			DBAppend()
			DBAppend()
			DBGoTop()
			DBRLock()
			DBRLockList()
			Assert.Equal(1, (INT) ALen(DBRLockList()))
			Assert.Equal(1, (INT) DBRLockList()[1])
			DBUnlock()
			DBSkip()
			DBRLock()
			Assert.Equal(1, (INT)  ALen(DBRLockList()))
			Assert.Equal(2, (INT) DBRLockList()[1])
			DBCloseArea()
		RETURN
	
		// TECH-34OWD3RR1Z , DBF problems with filters
		[Fact, Trait("Category", "DBF")];
		METHOD DBFilter() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()

			RDDSetDefault("DBFCDX")

			IF System.IO.File.Exists(cDbf)
				System.IO.File.Delete(cDbf)
			END IF
			DBCreate(cDbf, {{"CFIELD","C",10,0}}, "DBFCDX", TRUE)
			DBAppend()
			FieldPut(1, "ABC")
			DBAppend()
			FieldPut(1, "DEF")
			DBAppend()
			FieldPut(1, "GHI")
			DBAppend()
			FieldPut(1, "JKL")
			Assert.Equal(4 , (INT) RecCount())
			Assert.Equal(4 , (INT) LastRec())
			DBCloseArea()
						
			DBUseArea(,,cDbf)
//			"Setting filter to GHI, should be one record:"
//			"Instead, record 1 and 3 are shown"
			DBSetFilter({||AllTrim(FIELD->CFIELD) == "GHI"})
			DBGoTop()
			LOCAL nCount := 0 AS INT
			DO WHILE .NOT. EoF()
				Assert.Equal(3 , (INT) RecNo())
				FieldGet(1)
				DBSkip(+1)
				nCount ++
			END DO
			Assert.Equal(1 , nCount)
			
			DBGoBottom()
			Assert.False( EOF() )
			nCount := 0
			DO WHILE .NOT. EoF()
				Assert.Equal(3 , (INT) RecNo())
				nCount ++
				FieldGet(1)
				DBSkip(+1)
			END DO
			Assert.Equal(1 , nCount)
			
			DBCloseArea()
		RETURN

		// TECH-8C175D53DN , DBRecordInfo() always returns NULL_OBJECT
		[Fact, Trait("Category", "DBF")];
		METHOD DBRecordInfo_test() AS VOID
			LOCAL cDbf AS STRING
			LOCAL l AS LOGIC
			cDbf := GetTempFileName()
			DBCreate(cDbf, {{"CFIELD","C",10,0}}, "DBFCDX", TRUE)
			DBAppend()
			FieldPut(1, "ABC")
			
			l := DBRecordInfo( DBRI_RECNO ) // exception
			Assert.True(l)
			l := DBRecordInfo( DBRI_DELETED ) // exception
			Assert.False(l)
			l := DBRecordInfo( DBRI_LOCKED ) // exception
			Assert.True(l)
			
			DBCloseArea()
		RETURN
		
		// TECH-NVMBVB2Y44 , NullReferenceExpetion with DBFieldInfo()
		[Fact, Trait("Category", "DBF")];
		METHOD DBFieldInfo_test() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()
			DBCreate(cDbf, {{"NFIELD","N",10,3}}, "DBFCDX", TRUE)
			DBAppend()
			FieldPut(1, "ABC")
			
			Assert.Equal("NFIELD",	DBFieldInfo( DBS_NAME , 1 ) ) // NullReferenceException
			Assert.Equal("N",		DBFieldInfo( DBS_TYPE , 1 ) )
			Assert.Equal(10,		(INT) DBFieldInfo( DBS_LEN , 1 ) )
			Assert.Equal(3,			(INT) DBFieldInfo( DBS_DEC , 1 ) )
			Assert.Equal(5,			(INT) DBFieldInfo( DBS_PROPERTIES , 1 ) )
			
			DBCloseArea()
		RETURN

		// TECH-52M9YX557W , DBRecordInfo() changes record pointer
		[Fact, Trait("Category", "DBF")];
		METHOD DBRecordInfo_test2() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()
			DBCreate(cDbf, {{"CFIELD","C",10,0}}, "DBFCDX", TRUE)
			DBAppend()
			FieldPut(1, "ABC")
			DBAppend()
			FieldPut(1, "DEF")
			
			DBGoTop()
			Assert.Equal(1, (INT) RecNo())
			Assert.Equal(FALSE, EOF())
			
//			 Any of the below cause the record pointer to go EOF
			Assert.False( DBRecordInfo(DBRI_DELETED , 0) )
			DBRecordInfo(DBRI_BUFFPTR , 0)
			DBRecordInfo(DBRI_RAWDATA , 0)
			
			Assert.Equal(1, (INT) RecNo())
			Assert.Equal(FALSE, EOF())
			
			DBCloseArea()
		RETURN
	
		// TECH-C6Y1L51V1O , DBContinue() not working correctly
		[Fact, Trait("Category", "DBF")];
		METHOD DBContinue_test() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()

			RDDSetDefault("DBFCDX")

			DBCreate(cDbf, {{"NFIELD","N",10,0}}, "DBFCDX", TRUE)
			DBAppend()
			FieldPut(1, 123)
			DBAppend()
			FieldPut(1, 456)
			DBAppend()
			FieldPut(1, 789)
			
			DBGoTop()
			Assert.True( DBLocate({||_FIELD->NFIELD > 300} , , , , TRUE) ) // DBSCOPEREST
			Assert.True( Found() )
			Assert.Equal(456.0 , (FLOAT)  FieldGet(1) )
			
//			DBContinue() returns TRUE (correct) but does not move record pointer at all
			Assert.True( DBContinue() )
			Assert.True( Found() )
			Assert.Equal( 789.0 , (FLOAT)  FieldGet(1) )
			
			Assert.True( DBContinue() )
			Assert.False( Found() )
			Assert.Equal( 0.0 , (FLOAT) FieldGet(1) )
			
			Assert.True( DBContinue() )
			Assert.False( Found() )
			Assert.Equal( 0.0 , (FLOAT) FieldGet(1) )
			
			DBCloseArea()
		RETURN


		// TECH-Y4UUA09473 , Problem on creating error for invalid RDD command
		[Fact, Trait("Category", "DBF")];
		METHOD DBAppendWithNoWorkarea() AS VOID
			RDDSetDefault("DBFCDX")

			DBCloseAll()
			Assert.False( DBAppend() )
		RETURN


		// TECH-8HN2I0UUNA , Index file not correctly created when dbf is opened in SHARED mode
		[Fact, Trait("Category", "DBF")];
		METHOD Shared_Ntx() AS VOID
			LOCAL cDbf AS STRING
			LOCAL cCdx AS STRING
			cDbf := GetTempFileName("testcdx")
			cCdx := cDbf + ".cdx"
			
			Assert.True( DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }}) )
			Assert.True( DBUseArea(,,cDbf,,FALSE) )
			Assert.True( DBAppend() )
			FieldPut ( 1 , "B")
			Assert.True( DBAppend() )
			FieldPut ( 1 , "A")
			Assert.True( DBCloseArea() )
			
			Assert.True( DBUseArea(,,cDbf,,TRUE) ) // ----- opening in SHARED mode
			System.IO.File.WriteAllBytes(cCdx , Convert.FromBase64String("AAQAAAAAAAAAAAAACgDgAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQABAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAQD//////////94B//8AAA8PEAQEAwAGMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABURVNUQ0RYAAoAAAAAAAAAAAAACgBgAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABwABAAAABwBDRklFTEQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMAAgD//////////+AB//8AAA8PEAQEAwIAkAEAkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEJB"))
//			Assert.True( DBCreateIndex(cNtx , "CFIELD") ) // returns TRUE
			Assert.True( DBCloseArea() )
			
			Assert.True( DBUseArea(,,cDbf,,FALSE) )
			Assert.True( DBSetIndex(cCdx) )
			DBGoTop()
			Assert.True( AllTrim(FieldGet(1)) == "A" )
			DBGoBottom()
			Assert.True( AllTrim(FieldGet(1)) == "B" )
			Assert.True( DBCloseArea() ) // XSharp.RDD.RddError here
		RETURN

		// TECH-U43F26KOT7 , Runtime error saving NULL_DATE to DATE dbf field
		[Fact, Trait("Category", "DBF")];
		METHOD Save_NULL_DATE() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()

			RDDSetDefault("DBFCDX")

			Assert.True(  DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 },;
			{"DFIELD" , "D" , 8 , 0 }}) )
			Assert.True( DBUseArea(,,cDbf) )
			DBAppend()
			FieldPut ( 1 , "B")
			DBAppend()
			FieldPut ( 1 , "A")
			Assert.True( DBCloseArea() )
			
			Assert.True( DBUseArea(,,cDbf) )
			LOCAL u AS USUAL
			u := FieldGet(2) // it should be a NULL_DATE
			Assert.True( u == NULL_DATE )
			FieldPut(2,u) // exception
			FieldPut(2,NULL_DATE) // exception
			Assert.True( FieldGet(2) == NULL_DATE )
			Assert.True(  DBCloseArea() )
		RETURN


		// TECH-588I8LB67J , Problems with NTX indexes
		[Fact, Trait("Category", "DBF")];
		METHOD Ntx_Issues() AS VOID
			LOCAL cDbf AS STRING
			LOCAL cNtx AS STRING
			LOCAL aResult AS ARRAY
			
			cDbf := GetTempFileName()
			cNtx := cDbf + ".ntx"
			
			DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
			DBUseArea(,,cDbf)
			DBAppend()
			FieldPut ( 1 , "ABC")
			DBAppend()
			FieldPut ( 1 , "GHI")
			DBAppend()
			FieldPut ( 1 , "DEF")
			DBAppend()
			FieldPut ( 1 , "K")
			DBCloseArea()
			
			Assert.True( DBUseArea(,,cDbf) )
			Assert.True( DBCreateIndex(cNtx , "CFIELD") )
			Assert.True( DBCloseArea() )
			
			Assert.True( DBUseArea(,,cDbf,,FALSE) )
			Assert.True( DBSetIndex(cNtx) )
			aResult := GetRecords()
//			should be ABC, DEF, GHI, K
			Assert.True( aResult[1] == "ABC")
			Assert.True( aResult[2] == "DEF")
			Assert.True( aResult[3] == "GHI")
			Assert.True( aResult[4] == "K")
			
			DBGoTop()
			DBSkip()
			FieldPut(1,"HHH")
			aResult := GetRecords()
//			should be ABC, GHI, HHH, K
			Assert.True( aResult[1] == "ABC")
			Assert.True( aResult[2] == "GHI")
			Assert.True( aResult[3] == "HHH")
			Assert.True( aResult[4] == "K")

			DBGoTop()
			DBSkip(2)
			FieldPut(1,"DEF") // restore it
			
			Assert.True( DBCloseArea() )


			Assert.True( DBUseArea(,,cDbf,,TRUE) )
			Assert.True( DBSetIndex(cNtx) )
			aResult := GetRecords()
//			should be ABC, DEF, GHI, K
			Assert.True( aResult[1] == "ABC")
			Assert.True( aResult[2] == "DEF")
			Assert.True( aResult[3] == "GHI")
			Assert.True( aResult[4] == "K")
			
			DBGoTop()
			DBSkip()
			Assert.True(RLock())
			FieldPut(1,"III")
			aResult := GetRecords()
//			should be ABC, GHI, III, K
			Assert.True( aResult[1] == "ABC")
			Assert.True( aResult[2] == "GHI")
			Assert.True( aResult[3] == "III")
			Assert.True( aResult[4] == "K")
			
			Assert.True( DBCloseArea() )
		RETURN

		PRIVATE STATIC METHOD GetRecords() AS ARRAY
			LOCAL aResult AS ARRAY
			aResult := {}
			DBGoTop()
			DO WHILE .NOT. Eof()
				AAdd(aResult , AllTrim(FieldGet(1)))
				DBSkip()
			END DO
		RETURN aResult

		// TECH-V7A528Z0ZL , Problems with ntx indexes 2
		[Fact, Trait("Category", "DBF")];
		METHOD Ntx_Issues2() AS VOID
			LOCAL cFileName AS STRING
			cFileName := GetTempFileName()
			DBCreate(cFileName, {{"FLD1","C",10,0},{"FLD2","N",10,0}})
			DBUseArea( , , cFileName , , FALSE)
			FOR LOCAL n := 1 AS INT UPTO 10
				DBAppend()
				FieldPut(1, n:ToString())
				FieldPut(2, n)
			NEXT
			Assert.True( DBCreateIndex(cFileName + ".ntx" , "FLD2") )
			Assert.True( DBCloseArea() )

			DBUseArea( , , cFileName , , FALSE)
			Assert.True( DBSetIndex(cFileName + ".ntx") )
			DBGoTop()
			LOCAL nCount := 0 AS INT
			DO WHILE ! EOF()
				nCount ++
				Assert.True( FieldGet(2) == RecNo() )
				Assert.True( FieldGet(2) == nCount )
				Assert.True( DBSkip() )
			END DO
			Assert.True( DBCloseArea() )
		RETURN

		// TECH-965270UG7K , Workareas not being reused
		[Fact, Trait("Category", "DBF")];
		METHOD WorkareaNums() AS VOID
			LOCAL cFileName AS STRING
			cFileName := GetTempFileName()

			RDDSetDefault("DBFCDX")

			Assert.True( DBCreate(cFileName, {{"FLD1","C",10,0}}) )
			
			Assert.True( DBUseArea ( TRUE , , cFileName , "a1") )
			Assert.Equal( 1u , DBGetSelect() )
			Assert.True( DBCloseArea() )
			
			Assert.True( DBUseArea ( TRUE , , cFileName , "a2") )
			Assert.Equal( 1u , DBGetSelect() )
			Assert.True( DBCloseArea() )
			
			Assert.True( DBUseArea ( TRUE , , cFileName , "a3") )
			Assert.Equal( 1u , DBGetSelect() )
			Assert.True( DBCloseArea() )
		RETURN

		// FOX-ES1QLR6Y5L , dbRecordInfo ( DBRI_LOCKED ) always returns .f.
		[Fact, Trait("Category", "DBF")];
		METHOD DBRI_LOCKED_test() AS VOID
			LOCAL cFileName AS STRING
			cFileName := GetTempFileName()

			RDDSetDefault("DBFCDX")

			SetExclusive ( FALSE )
			DBCreate ( cFileName , { {"id", "C", 5, 0} })
		
			DBUseArea ( , , cFileName )
			DBAppend()
			DBAppend()
			DBAppend()
			DBGoTop()
			
			Assert.True( DBRLock ( RecNo() ) )
			Assert.True( DBRecordInfo ( DBRI_LOCKED ) ) // Should show TRUE
			Assert.True( AScan ( DBRLockList() , RecNo() ) > 0 )
			
			DBSkip()
//			record 2 - no lock
			Assert.False( DBRecordInfo ( DBRI_LOCKED ) )
			Assert.False( AScan ( DBRLockList() , RecNo() ) > 0 )
			
			DBSkip()
			Assert.True( DBRLock ( RecNo() ) )
			Assert.True( DBRecordInfo ( DBRI_LOCKED ) ) // Should show TRUE
			Assert.True( AScan ( DBRLockList() , RecNo() ) > 0 )
			
			LOCAL a AS ARRAY
			a:= DBRLockList()
			Assert.Equal( 2 , (INT) ALen(a) )
			Assert.Equal( 1 , (INT) a[1] )
			Assert.Equal( 3 , (INT) a[2] )

			DBCloseArea()
			
			SetExclusive ( TRUE ) // restore
		RETURN

		// TECH-XQES14W9J0 , Aliasxxx() funcs throw exceptions
		[Fact, Trait("Category", "DBF")];
		METHOD Alias_test() AS VOID

			RDDSetDefault("DBFCDX")

			DBCloseAll()
			Assert.True( Alias() == "" )
			Assert.True( Alias0() == "" )
			Assert.True( Alias0Sym() == "" )
		RETURN

		// TECH-IXV5X91A74 , DBCreate() problem after having opened a dbf in exclusive mode
		[Fact, Trait("Category", "DBF")];
		METHOD DBCreate_test() AS VOID
			LOCAL cFileName AS STRING
			cFileName := GetTempFileName()
			
			RDDSetDefault("DBFCDX")

			DBCreate(cFileName, {{"FLD1","C",10,0}})
			DBUseArea(,,cFileName,,FALSE)
			DBCloseArea()

//			exception here
			Assert.True( DBCreate(cFileName, {{"FLD1","N",10,0}}) )
			
			Assert.True( DBUseArea(,,cFileName) )
			DBAppend()
			FieldPut(1 , 123)
			Assert.True( FieldGet(1) == 123 )
			Assert.True( DBCloseArea() )
		RETURN


		// TECH-ONPOSM84VS , Runtime exception on Error:ToString()
		[Fact, Trait("Category", "DBF")];
		METHOD DBError_test() AS VOID
			LOCAL cDbf AS STRING
			
			RDDSetDefault("DBFCDX")

			cDbf := GetTempFileName()
			
			DBCreate( cDbf , {{"CFIELD" , "C" , 10 , 0 }})
			DBUseArea(,,cDbf,,TRUE)
			Assert.True( DBAppend() )
			Assert.True( DBUnlock() )
			TRY
				? FieldPut ( 1 , "ABC") // record not locked
			CATCH e AS XSharp.Error
				? e:Message
				? e:GenCodeText
				? e:SubCodeText // check if this displays correctly
				? e:OSCodeText
				? e:ToString() // exception here
			FINALLY
				Assert.True( DBCloseArea() )
			END TRY
		RETURN


		// TECH-9JPUGAOV3L , NTX problem with EoF after sequence of commands
		[Fact, Trait("Category", "DBF")];
		METHOD Ntx_Eof_test() AS VOID
			LOCAL cDbf AS STRING
			LOCAL cNtx AS STRING
			
			cDbf := GetTempFileName()
			cNtx := cDbf + ".ntx"
			
			DBCreate( cDbf , {{"NFIELD" , "N" , 5 , 0 }})
			DBUseArea(,,cDbf,,FALSE)
			DBAppend()
			FieldPut(1,123)
			DBAppend()
			FieldPut(1,456)
			DBCreateIndex(cNtx, "NFIELD")
			DBCloseArea()
			

			// necessary sequence to reproduce the problem below
			DBUseArea(,,cDbf,,FALSE)
			DBSetIndex(cNtx)
			DBGoTop()
			Assert.Equal(1, (INT)RecNo()) // 1
			DBGoBottom()
			Assert.Equal(2, (INT)RecNo()) // 2
			DBSkip(-1)
			Assert.Equal(1, (INT)RecNo()) // 1
			DBGoTo(2)
			Assert.Equal(2, (INT)RecNo()) // 2


			DBSkip(+1)
			Assert.Equal(TRUE, (LOGIC)EOF()) // FALSE, wrong
			Assert.Equal(3, (INT)RecNo()) // 2, wrong

			DBSkip(+1)
			Assert.Equal(TRUE, (LOGIC)EOF()) // TRUE
			Assert.Equal(3, (INT)RecNo()) // 3

			DBCloseArea()
		RETURN


		// TECH-546935N337, DBOrderInfo(DBOI_SETCODEBLOCK) causes an invalid cast exception, but only if a index is opened
		[Fact, Trait("Category", "DBF")];
		METHOD DBOrderInfo_DBOI_SETCODEBLOCK() AS VOID
			LOCAL cDBF, cNTX AS STRING
			
			cDBF := GetTempFileName("test")
			cNTX := cDBF + ".ntx"
			
			DBCreate( cDBF , {{"ID" , "C" , 5 , 0 }})
			DBUseArea(,,cDBF)
			
			DBAppend()
			FieldPut(1, "one")
			
			DBCreateIndex(cNTX , "Upper (ID)")
			DBCloseArea()
			
			DBUseArea(,,cDBF)
			DBSetIndex(cNTX)
			
			? "DBOI_SETCODEBLOCK" , DBOrderInfo( DBOI_SETCODEBLOCK )
			
			DBCloseArea()		
		RETURN

		
		// TECH-3NY78C93EK, OrdScope() not working
		[Fact, Trait("Category", "DBF")];
		METHOD OrdScope_test() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()
			DBCreate( cDbf , {{"NFIELD" , "N" , 5 , 0 }})
			DBUseArea(,"DBFNTX",cDbf)
			FOR LOCAL n := 1 AS INT UPTO 20
				DBAppend()
				FieldPut(1,n)
			NEXT
			DBCreateIndex(cDbf , "NFIELD")
			DBCloseArea()
			
			DBUseArea(,"DBFNTX",cDbf) // 20 records
			DBSetIndex ( cDbf )
			Assert.Equal( (INT) DBOrderInfo( DBOI_KEYCOUNT ) , 20)
			Assert.Equal( (INT) OrdScope(TOPSCOPE, 5) , 5) // NULL
			Assert.Equal( (INT) OrdScope(BOTTOMSCOPE, 10) , 10) // NULL
			DBGoTop()
			
			Assert.Equal( (INT) DBOrderInfo( DBOI_KEYCOUNT ) , 6) // still 20 - but must be 6
			LOCAL nCount := 0 AS INT
			DO WHILE ! EOF() // all 20 records are listed
				? FieldGet ( 1 )
				DBSkip ( 1 )
				nCount ++
			ENDDO
			Assert.Equal( 6 , nCount)
			Assert.Equal( 5 , (INT) DBOrderInfo( DBOI_SCOPETOP ) ) // {(0x0000)0x00000000} CLASS
			Assert.Equal( 10 , (INT) DBOrderInfo( DBOI_SCOPEBOTTOM ) ) // {(0x0000)0x00000000} CLASS
			DBCloseArea()
		RETURN
		

		[Fact, Trait("Category", "DBF")];
		METHOD OrdScope_test_with_Ordinal_Collation() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileName()
			
			LOCAL uCollation AS USUAL
			uCollation := SetCollation(#ORDINAL)
			
			DBCreate( cDbf , {{"NFIELD" , "N" , 5 , 0 }})
			DBUseArea(,"DBFNTX",cDbf)
			FOR LOCAL n := 1 AS INT UPTO 20
				DBAppend()
				FieldPut(1,n)
			NEXT
			DBCreateIndex(cDbf , "NFIELD")
			DBCloseArea()
			
			DBUseArea(,"DBFNTX",cDbf)
			DBSetIndex ( cDbf )
			Assert.Equal( (INT) DBOrderInfo( DBOI_KEYCOUNT ) , 20)
			Assert.Equal( (INT) UsualType( DBOrderInfo( DBOI_KEYCOUNT )  ) , 1) // first time returns 6
			Assert.Equal( (INT) UsualType( DBOrderInfo( DBOI_KEYCOUNT )  ) , 1) // second time it returns 1 correctly
			DBCloseArea()
			
			SetCollation(uCollation)
		RETURN
		
		
		// TECH-9TW65Q3XQE, NTX corruption with updating multiple fields and shared mode
		[Fact, Trait("Category", "DBF")];
		METHOD NTX_test() AS VOID
			LOCAL aValues AS ARRAY
			LOCAL i AS DWORD
			LOCAL cDBF, cNTX AS STRING
			
			aValues := { "ssss" , "hhhh", "wwww" , "aaaa" }
			cDBF := GetTempFileName()
			cNTX := cDbf
			DBCreate( cDBF , {{"LAST" , "C" , 20 , 0 } , ;
								{"TEXT1" , "C" , 10 , 0 } , ;
								{"NUM1" , "N" , 10 , 2 }})
			
			DBUseArea(,,cDBF,,FALSE)
			FOR i := 1 UPTO ALen ( aValues )
				DBAppend()
				FieldPut(1,aValues [i])
			NEXT
			DBCreateIndex(cNTX, "Upper ( Last)")
			DBCloseArea()
			
			DBUseArea(,,cDBF,,TRUE) // open shared !
			DBSetIndex(cNTX)
			DBGoTop()
			? "current (indexed) order"
			DO WHILE ! EOF()
				? FieldGet ( 1 )
				DBSkip ( 1 )
			ENDDO
			DBGoTop()
			// "now replace the index field #LAST content 'aaaa' with 'pppp'"
			// "and also update another field"
			Assert.True( DBRLock ( RecNo() )  )
			// "Replacing", AllTrim(FieldGet(1)), "with 'pppp'"
			FieldPut ( 1 , "pppp" ) // Note: This is the index field
				
//	 this is what causes the problem, updating a second field
//	 if this fieldput is put above the first one, then sample works correctly
//	 either one of the fieldputs below cause the problem to surface
			FieldPut ( 2 , "Eins" )
			FieldPut ( 3 , 123.45 )
			
			DBCommit()
			
			DBRUnlock ( RecNo() )
			
			Assert.False( DBSeek( "AAAA" ) ) // must show .F.
			Assert.True( DBSeek ("PPPP" ) ) // must show .T.
			// "Record order now:"
			// "(should be hhhh, pppp, ssss , wwww)"
			DBGoTop()
			LOCAL nCount := 0 AS INT
			DO WHILE ! EOF()
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
				DBSkip ( 1 )
			ENDDO
			DBCloseArea()
		RETURN
	
	
		// TECH-4JX6H10741, DBOrderInfo( DBOI_KEYCOUNT ) returns NIL when record pointer is at EoF
		[Fact, Trait("Category", "DBF")];
		METHOD DBOrderInfo_DBOI_KEYCOUNT() AS VOID
			LOCAL cDbf AS STRING
			LOCAL aValues AS ARRAY 
			LOCAL i AS DWORD
			
			cDBF := GetTempFileName()
			aValues := { 44 , 12, 34 , 21 }                                 
			DBCreate( cDBF , {{"AGE" , "N" , 3 , 0 } })
			DBUseArea(,"DBFNTX",cDBF,,FALSE) 
			Assert.Equal(0 , (INT) DBOrderInfo( DBOI_KEYCOUNT ) ) // 0,  ok
			FOR i := 1 UPTO ALen ( aValues )
				DBAppend()
				FieldPut(1,aValues [i])  
			NEXT 
			Assert.Equal(0 , (INT) DBOrderInfo( DBOI_KEYCOUNT ) ) // 0,  ok
			DBCreateIndex(cDbf, "age" ) 
			Assert.Equal(4 , (INT) DBOrderInfo( DBOI_KEYCOUNT ) ) // 4, correct
			DBGoTop() 
			Assert.Equal(4 , (INT) DBOrderInfo( DBOI_KEYCOUNT ) ) // 4, correct
			DO WHILE ! EOF()
//			? FieldGet ( 1 ) 
				DBSkip(1)
			ENDDO 
			Assert.Equal(4 , (INT) DBOrderInfo( DBOI_KEYCOUNT ) ) // NIL, should be 4
			DBSkip(-1)
			Assert.Equal(4 , (INT) DBOrderInfo( DBOI_KEYCOUNT ) ) // 4, correct
			DBCloseArea ()
		RETURN


		[Fact, Trait("Category", "DBF")];
		METHOD SetDeleted_FALSE() AS VOID
			LOCAL cDbf AS STRING
			LOCAL aValues AS ARRAY 
			LOCAL i AS DWORD
			
			cDBF := GetTempFileName()
		
			SetDeleted(FALSE)
			
//			test also with those
//			aValues := { "vaa" , "abba", "acb" , "aaab"  , "adab"  , "baac"  , "aeab"  , "baaAaa" }
			aValues := { "vvv" , "abb", "acb" , "aaa"  , "bbb" }
			DBCreate( cDBF , {{"LAST" , "C" ,10 , 0 } })
			DBUseArea(,"DBFNTX",cDBF,,FALSE)
			FOR i := 1 UPTO ALen ( aValues )
				DBAppend()
				FieldPut(1,aValues [i])  
			NEXT 
		
			DBUseArea(,"DBFNTX",cDBF,,TRUE) 
			Assert.Equal(0 , (INT)DBOrderInfo( DBOI_KEYCOUNT ))
			DBCreateIndex(cDbf, "Upper(LAST)" ) 
			Assert.Equal(5 , (INT)DBOrderInfo( DBOI_KEYCOUNT ))
			// ? "Setting scope"
			LOCAL u AS USUAL
			u := "A"
			VODBOrderInfo( DBOI_SCOPETOP, "", NIL, REF u )
			VODBOrderInfo( DBOI_SCOPEBOTTOM, "", NIL, REF u )
		
			DBGoTop()
			Assert.Equal(4 , (INT)RecNo())
			DBGoBottom()
			Assert.Equal(3 , (INT)RecNo())
		
			DBGoTop() 
		
			Assert.Equal(3 , (INT)DBOrderInfo( DBOI_KEYCOUNT ))
			DO WHILE ! EOF()
				DBSkip(1)
			ENDDO 
			Assert.Equal(3 , (INT)DBOrderInfo( DBOI_KEYCOUNT ))
			DBCloseArea()

			SetDeleted(FALSE)
		RETURN


		// TECH-W0KKQ1I50C, Problems with SetDeleted(TRUE)
		[Fact, Trait("Category", "DBF")];
		METHOD SetDeleted_TRUE() AS VOID
			LOCAL cDbf AS STRING
			LOCAL aValues AS ARRAY 
			LOCAL i AS DWORD
			
			cDBF := GetTempFileName()
		
			SetDeleted(TRUE)
			
//			test also with those
//			aValues := { "vaa" , "abba", "acb" , "aaab"  , "adab"  , "baac"  , "aeab"  , "baaAaa" }
			aValues := { "vvv" , "abb", "acb" , "aaa"  , "bbb" }
			DBCreate( cDBF , {{"LAST" , "C" ,10 , 0 } })
			DBUseArea(,"DBFNTX",cDBF,,FALSE)
			FOR i := 1 UPTO ALen ( aValues )
				DBAppend()
				FieldPut(1,aValues [i])  
			NEXT 
		
			DBUseArea(,"DBFNTX",cDBF,,TRUE) 
			Assert.Equal(0 , (INT)DBOrderInfo( DBOI_KEYCOUNT ))
			DBCreateIndex(cDbf, "Upper(LAST)" ) 
			Assert.Equal(5 , (INT)DBOrderInfo( DBOI_KEYCOUNT ))
			// ? "Setting scope"
			LOCAL u AS USUAL
			u := "A"
			VODBOrderInfo( DBOI_SCOPETOP, "", NIL, REF u )
			VODBOrderInfo( DBOI_SCOPEBOTTOM, "", NIL, REF u )
		
			DBGoTop()
			Assert.Equal(4 , (INT)RecNo())
			DBGoBottom()
			Assert.Equal(3 , (INT)RecNo())
		
			DBGoTop() 
		
			Assert.Equal(3 , (INT)DBOrderInfo( DBOI_KEYCOUNT ))
			DO WHILE ! EOF()
				DBSkip(1)
			ENDDO 
			Assert.Equal(3 , (INT)DBOrderInfo( DBOI_KEYCOUNT ))
			DBCloseArea()

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
			cDBF := GetTempFileName("testdbf")
			cNTX := cDbf + ".ntx"
			IF System.IO.File.Exists(cNtx)
				System.IO.file.Delete(cNtx)
			END IF
			DBCreate( cDBF , {{"AGE" , "N" , 3 , 0 } })                                        
			DBUseArea(,"DBFNTX",cDBF,,FALSE)
			Assert.Equal(0, (INT)DBOrderInfo( DBOI_KEYCOUNT ) )   //  0  ok
			FOR i := 1 UPTO ALen ( aValues )
				DBAppend()
				FieldPut(1,aValues [i]) 
			NEXT
			Assert.Equal(0, (INT)DBOrderInfo( DBOI_KEYCOUNT ) ) //  0 ,ok
			Assert.Equal(0, (INT)DBOrderInfo( DBOI_NUMBER ) )  //  -1,  but should show 0

			DBCreateIndex( cNTX, "age" )
			DBGoTop()
			DO WHILE ! EOF()
//				? FieldGet ( 1 )
				DBSkip(1)
			ENDDO

			Assert.True( DBClearIndex( cNTX) )
			Assert.True( DBSetIndex ( cNTX ) )

			Assert.Equal(4, (INT) DBOrderInfo( DBOI_KEYCOUNT ) ) // 4, ok
			Assert.Equal(1, (INT) DBOrderInfo( DBOI_NUMBER ) )  // still  -1 , but should show  1
			Assert.Equal("TESTDBF", (STRING) DBOrderInfo( DBOI_NAME ) )  // ok , "TESTDBF"
			DBCloseArea ()
		RETURN


		// TECH-RGU0U0636C, DBSetOrderCondition() results to NotImplementedException
		[Fact, Trait("Category", "DBF")];
			METHOD DBSetOrderCondition_test() AS VOID
			LOCAL cDbf AS STRING
			LOCAL aValues AS ARRAY
			LOCAL i AS DWORD
			
			cDBF := GetTempFileName()
			aValues := { 1,4,2,3 }
			DBCreate( cDBF , {{"NUM" , "N" ,5 , 0 } })
			DBUseArea(,"DBFNTX",cDBF,,FALSE)
			FOR i := 1 UPTO ALen ( aValues )
				DBAppend()
				FieldPut(1,aValues [i])
			NEXT
			DBGoTop()
			
			// DESCENDING = TRUE
			Assert.True( DBSetOrderCondition(,,,,,,,,,,TRUE) )
			Assert.True( DBCreateIndex(cDbf, "NUM" ) )
			
			DBGoTop()
			aValues := { 4,3,2,1 }
			LOCAL nCount := 0 AS INT
			DO WHILE .NOT. EoF()
				nCount ++
				Assert.Equal((INT)aValues[nCount] , (INT)FieldGet(1))
				DBSkip()
			END DO
			
			DBCloseArea()
		RETURN


		// TECH-P956TFHW74, Cannot use fields in macro exceptions when they are named after a function
		[Fact, Trait("Category", "DBF")];
		METHOD Fields_Named_After_Funcs() AS VOID
			LOCAL cDbf AS STRING
			cDBF := GetTempFileName()
			
			DBCloseAll() // worakaroudn for previous test crashing
			
			DBCreate( cDBF , {{"LEFT" , "N" ,5 , 0 } , {"STR" , "C" ,5 , 0 } })
			DBUseArea(,"DBFNTX",cDBF,,FALSE)
			DBAppend()
			FieldPut(1,1)
			FieldPut(2,"A")
			DBAppend()
			FieldPut(1,11)
			FieldPut(2,"B")
			DBGoTop()
			Assert.True( DBCreateIndex(cDbf, "STR" ) )
			Assert.True( DBSetFilter(&("{||LEFT<10}")) )
			DBGoTop()
			DBSkip()
			Assert.True( EOF() )
			DBCloseArea()
		RETURN


		// TECH-R933ZKYT9Q, Problem creating index on dbf with "uninitialized" fields
		[Fact, Trait("Category", "DBF")];
		METHOD Uninitialized_fields() AS VOID
			LOCAL cDbf AS STRING

			RDDSetDefault("DBFCDX")

			DBCloseAll()
			
			cDBF := GetTempFileName()
			DBCreate( cDBF , {{"FIELDN" , "N" ,5 , 0 } })
			DBUseArea(,"DBFCDX",cDBF)
			DBAppend()
			FieldPut(1,1)
			DBAppend() // no field assigned
			DBCloseArea()
			
			DBUseArea(,"DBFNTX",cDBF)
			Assert.True( DBCreateIndex(cDbf, "FIELDN" ) )
			DBCloseArea()
		RETURN


		// TECH-T365UMTY4V, Incorrect values returned by DBOrderInfo( DBOI_KEYTYPE )
		[Fact, Trait("Category", "DBF")];
		METHOD DBOrderInfo_DBOI_KEYTYPE() AS VOID
			LOCAL cDbf AS STRING
			DBCloseAll()
			
			cDBF := GetTempFileName("testnewer")
			DBCreate( cDBF , {{"FIELDN" , "N" ,5 , 0 } , ;
			{"FIELDS" , "C" ,15 , 0 } , ;
			{"FIELDL" , "L" ,1 , 0 } , ;
			{"FIELDD" , "D" ,8 , 0 } })
			DBUseArea(,"DBFNTX",cDBF)
			DBAppend()
			FieldPut(1,1)
			DBCloseArea()
			
			DBUseArea(,"DBFNTX",cDBF)
			DBCreateIndex(cDbf, "FIELDN" )
			Assert.Equal(3, (INT)DBOrderInfo( DBOI_KEYTYPE ) ) // 14 (), should be 3 (FLOAT)
			DBCloseArea()
			
			DBUseArea(,"DBFNTX",cDBF,,FALSE)
			DBCreateIndex(cDbf, "FIELDS" )
			Assert.Equal(7, (INT)DBOrderInfo( DBOI_KEYTYPE ) ) // 18 (PTR), should be 7 (STRING)
			
			DBUseArea(,"DBFNTX",cDBF,,FALSE)
			DBCreateIndex(cDbf, "FIELDD" )
			Assert.Equal(2, (INT)DBOrderInfo( DBOI_KEYTYPE ) ) // 16, should be 2 (DATE)
			
			DBUseArea(,"DBFNTX",cDBF,,FALSE)
			DBCreateIndex(cDbf, "FIELDL" )
			Assert.Equal(8, (INT)DBOrderInfo( DBOI_KEYTYPE ) ) // 3 (FLOAT), should be 8
			
			DBCloseArea()
		RETURN


		// TECH-YV2072YP4B, DBSetOrderCondition() not respecting FOR condition
		[Fact, Trait("Category", "DBF")];
		METHOD DBSetOrderCondition_with_FOR() AS VOID
			LOCAL cDbf AS STRING
			DBCloseAll()
			
			cDBF := GetTempFileName("testnewer")
			DBCreate( cDBF , {{"FIELDN" , "N" ,5 , 0 } })
			DBUseArea(,"DBFNTX",cDBF) 
			DBAppend()
			FieldPut(1,1)
			DBAppend()
			FieldPut(1,4)
			DBAppend()
			FieldPut(1,2)
			DBAppend()
			FieldPut(1,3)
			DBCloseArea()
		
			DBUseArea(,"DBFNTX",cDBF) 
			
			// Should show only 4,3, but it shows all records 4,3,2,1
			DBSetOrderCondition( "FIELDN>2",{||_FIELD->FIELDN>2},,,,,,,,, TRUE)
			DBCreateIndex(cDbf, "FIELDN" )
			DBGoTop()
			LOCAL nCount := 0 AS INT
			DO WHILE .NOT. EoF()
				nCount ++
				IF nCount == 1
					Assert.Equal(4 ,(INT)FieldGet(1))
				ELSEIF nCount == 1
					Assert.Equal(3 ,(INT)FieldGet(1))
				END IF
				DBSkip()
			END DO
		    
			Assert.Equal(2 ,nCount)
			
			// Should both show true, but both return false
			Assert.True( DBOrderInfo( DBOI_ISCOND ) )
			Assert.True( DBOrderInfo( DBOI_ISDESC ) )
			
			DBCloseArea()
		RETURN


		// TECH-3KG4A5V124, DBSetFound() always sets the Found flag to TRUE, no matter the value passed
		[Fact, Trait("Category", "DBF")];
		METHOD DBSetFound_test() AS VOID
			LOCAL aDbf AS ARRAY
			LOCAL cDbf AS STRING

			RDDSetDefault("DBFCDX")

			DBCloseAll()
			
			cDBF := GetTempFileName()
			aDbf := {{ "AGE" , "N" , 2 , 0 }}
			DBCreate( cDBF , aDbf)
			
			DBUseArea(,"DBFCDX",cDBF,,FALSE)
			DBAppend()
			
			Assert.False( Found() )// FALSE, ok
			Assert.True( DBSetFound( FALSE ) )
			Assert.False( Found() ) // TRUE! wrong
			Assert.True( DBSetFound( TRUE ) )
			Assert.True( Found() ) // TRUE correct
			DBCloseArea()
		RETURN


		[Fact, Trait("Category", "DBF")];
		METHOD DBSetFound_test2() AS VOID
			LOCAL aDbf AS ARRAY
			LOCAL cDbf AS STRING

			RDDSetDefault("DBFCDX")

			DBCloseAll()
			
			cDBF := GetTempFileName()
			aDbf := {{ "AGE" , "N" , 2 , 0 }}
			DBCreate( cDBF , aDbf)
			
		    DBCreate( cDBF , aDbf)
		    DBUseArea(,"DBFNTX",cDBF,,FALSE)
		   
		    DBAppend()
		   
		    DBCloseArea()
		   
		    DBUseArea( TRUE ,"DBFCDX",cDBF,"AREA1" ,TRUE )
		    DBUseArea( TRUE ,"DBFCDX",cDBF,"AREA2" ,TRUE )
		    
		    Assert.True( DBSelectArea ( "AREA1" ) )
		    Assert.Equal("AREA1", Alias() )
		    Assert.False( Found() )
		    Assert.True( DBSetFound ( TRUE ) )
		    Assert.True( Found() )
		   
		    Assert.True( DBSelectArea ( "AREA2" ) )
		    Assert.Equal( "AREA2" , Alias() )
		    Assert.False( Found() )
		    Assert.True( DBSetFound  ( FALSE ) )
		    Assert.False( Found() )

		    Assert.True( DBCloseArea() )
		    Assert.True( DBSelectArea ( "AREA1" ) )
		    Assert.True( DBCloseArea() )
		RETURN


		// TECH-56GA29Y57C, Found() does not return correct results for the active workarea
		[Fact, Trait("Category", "DBF")];
		METHOD DBSetFound_test3() AS VOID
			LOCAL aDbf AS ARRAY
			LOCAL cDbf AS STRING

			RDDSetDefault("DBFCDX")

			DBCloseAll()
			
			cDBF := GetTempFileName()
		    aDbf := {{ "AGE" , "N" , 2 , 0 }}
		    DBCreate( cDBF , aDbf)
		    DBUseArea(,,cDBF,,FALSE)
		   
		    DBAppend()
		    FieldPut(1,1)
		    DBCloseArea()
		   
		    DBUseArea( TRUE ,"DBFNTX",cDBF,"AREA1" ,TRUE )
		    DBUseArea( TRUE ,"DBFNTX",cDBF,"AREA2" ,TRUE )
		    
		    DBSelectArea ( "AREA1" )  
		    Assert.True( DBLocate({||_FIELD->AGE == 1}) )
		    Assert.True( Found() )
		   
		    DBSelectArea ( "AREA2" )   
		    Assert.False( Found() )
		
		    DBCloseAll()
		RETURN


		// TECH-5YKDFJWM4N, FLock() runtime exception when some records are already locked
		[Fact, Trait("Category", "DBF")];
		METHOD FLock_test() AS VOID
			LOCAL aDbf AS ARRAY
			LOCAL cDbf AS STRING
			
			RDDSetDefault("DBFCDX")
			
			DBCloseAll()
			
			cDBF := GetTempFileName()
		    aDbf := {{ "AGE" , "N" , 2 , 0 }}
			DBCreate( cDBF , aDbf)

			DBUseArea(,"DBFCDX",cDBF,,FALSE)
			DBAppend()
			DBAppend()
			DBAppend()
			DBCloseArea()
			
			Assert.True( DBUseArea( TRUE ,"DBFNTX",cDBF,"AREA1" ,TRUE ) )// open shared
			DBGoTop()
			Assert.True( DBRLock ( RecNo() ) ) // lock first record
			Assert.Equal( 1 , (INT) ALen ( DBRLockList() ) )
			
			Assert.True( DBUseArea( TRUE ,"DBFNTX",cDBF,"AREA2" ,TRUE ) ) // open shared
			DBGoBottom()
			Assert.True( DBRLock ( RecNo() ) ) // lock last record
			Assert.Equal( 1 , (INT) ALen ( DBRLockList() ) )
			
			Assert.False( FLock() )
			// what's the correct return value here??
			// Assert.Equal( 0 , (INT) ALen ( DBRLockList() ) )
			DBCloseAll()
		RETURN


		// TECH-429V6Q2959, IndexExt() problems
		[Fact, Trait("Category", "DBF")];
		METHOD IndexExt_test() AS VOID
			LOCAL aDbf AS ARRAY
			LOCAL cDBF AS STRING
			LOCAL aValues AS ARRAY
			LOCAL i AS DWORD       
			DBCloseAll()
			
			cDBF := GetTempFileName()

			RDDSetDefault("DBFNTX")
			Assert.True( IndexKey() == "")
			Assert.True( IndexOrd() == 0)
			Assert.True( IndexCount() == 0)
			Assert.True( Upper(IndexExt()) ==  ".NTX")
			Assert.True( DBOrderInfo(DBOI_INDEXEXT) == NIL )
			
			aValues := { 44 , 12, 34 , 21 }
			aDbf := {{ "AGE" , "N" , 2 , 0 }}
			
			DBCreate( cDBF , aDbf)
			DBUseArea(,"DBFNTX",cDBF,,FALSE)                    
			FOR i := 1 UPTO ALen ( aValues )
				DBAppend()
				FieldPut(1,aValues [i])
			NEXT    
			DBCreateIndex( cDbf, "age" )
			
			Assert.True( IndexKey() == "age")
			Assert.True( IndexOrd() == 1 )
			Assert.True( IndexCount() == 1 )
			Assert.True( Upper(IndexExt()) == ".NTX")
			Assert.True( DBOrderInfo(DBOI_INDEXEXT) == ".NTX")
			
			DBCloseArea()
		RETURN



		STATIC PRIVATE METHOD GetTempFileName() AS STRING
		RETURN GetTempFileName("testdbf")
		STATIC PRIVATE METHOD GetTempFileName(cFileName AS STRING) AS STRING
			// we may want to put them to a specific folder etc
		RETURN cFileName
			
	END CLASS
END NAMESPACE
