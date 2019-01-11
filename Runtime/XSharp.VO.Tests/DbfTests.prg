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
			Assert.True(  DBCreate(cFileName_NoExt , aFields , "DBFNTX")  )
			Assert.True(  System.IO.File.Exists(cFileName_WithExt) )
			
			IF System.IO.File.Exists(cFileName_WithExt)
				System.IO.File.Delete(cFileName_WithExt)
			END IF
			Assert.True(  DBCreate(cFileName_WithExt , aFields , "DBFNTX")  )
			Assert.True(  System.IO.File.Exists(cFileName_WithExt) )
	
			cFileName_WithExt := cFileName_NoExt + ".none"
			IF System.IO.File.Exists(cFileName_WithExt)
				System.IO.File.Delete(cFileName_WithExt)
			END IF
			Assert.True(  DBCreate(cFileName_WithExt , aFields , "DBFNTX")  )
			Assert.True(  System.IO.File.Exists(cFileName_WithExt) )
		RETURN
	
		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBAppend_Exclusive() AS VOID
			LOCAL aFields AS ARRAY
			LOCAL cFileName AS STRING
			aFields := {{"TEST","C",10,0}}
			cFileName := "DBAppend_Exclusive"
	
			Assert.True(  DBCreate(cFileName , aFields , "DBFNTX")  )
			Assert.True(  DBUseArea(,"DBFNTX",cFileName,,FALSE) )
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
	
			Assert.True(  DBCreate(cFileName , aFields , "DBFNTX")  )
			Assert.True(  DBUseArea(,"DBFNTX",cFileName,,TRUE) )
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
			RDDSetDefault( "DBFNTX" )
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
			RDDSetDefault( "DBFNTX" )
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
			cFileName := GetTempFileame("test")
			DBCreate(cFileName , { {"CFIELD","C",10,0} })
			DBUseArea(,,cFileName)
			DBAppend()
			
			LOCAL u AS USUAL
			VODBSkip(-1)
			
			VODBInfo(DBI_FULLPATH , REF u)
			LOCAL c AS STRING
			c := u
			? c
			Assert.True(c:EndsWith("test.DBF") .and. c:Contains(":\"))
			VODBInfo(DBI_DB_VERSION , REF u)
			Assert.True(SLen(u) > 1)
			VODBInfo(DBI_ALIAS , REF u)
			Assert.Equal("test" , u)
			
			VODBInfo(DBI_BOF , REF u)
			Assert.Equal(TRUE , u)
			VODBInfo(DBI_EOF , REF u)
			Assert.Equal(FALSE , u)
			VODBInfo(DBI_ISANSI , REF u)
			Assert.Equal(SetAnsi() , u)
			VODBInfo(DBI_FCOUNT , REF u)
			Assert.Equal(1, u)
			VODBInfo(DBI_READONLY , REF u)
			Assert.Equal(FALSE, u)
			
			DBCloseArea()
		RETURN

		// TECH-E6Y9GNHB99
		[Fact, Trait("Category", "DBF")];
		METHOD AppendShared() AS VOID
			LOCAL cDbf AS STRING
			cDbf := GetTempFileame("testAppendShared")
			RDDSetDefault( "DBFNTX" )
			DBCreate(cDbf , { {"TEST","C",10,0} })
			
//			Appending in exclusive mode:
			DBUseArea(, , cDbf , "alias2" , FALSE)
			LOCAL lRet AS LOGIC
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
			cDbf := GetTempFileame("testdbf")
			
			RDDSetDefault( "DBFNTX" )
			
			DBCreate(cDbf , { {"TEST","C",10,0} })
			
//			opening and closing once
			DBUseArea(, , cDbf , , FALSE)
			DBCloseArea()
			
//			opening and closing again
			Assert.True( DBUseArea(, , cDbf , , FALSE) )
			Assert.True( DBCloseArea() )
		RETURN




		STATIC PRIVATE METHOD GetTempFileame(cFileName AS STRING) AS STRING
			// we may want to put them to a specific folder etc
		RETURN cFileName
			
	END CLASS
END NAMESPACE
