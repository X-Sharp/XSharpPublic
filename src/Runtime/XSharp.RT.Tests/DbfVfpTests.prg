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

BEGIN NAMESPACE XSharp.RT.Tests

	CLASS DbfVfpTests

        [Fact, Trait("Category", "DBF")];
		METHOD DBFVFP_Test() AS VOID
			// https://github.com/X-Sharp/XSharpPublic/issues/1557
			LOCAL cFileName AS STRING
			cFileName := DbfTests.GetTempFileName()
			cFileName := cFileName + "."

			DbCreate(cFileName, {{"TEST","Y",10,4}} , "DBFVFP")
			DbUseArea(true, "DBFVFP", cFileName)
			DbAppend()
			FieldPut(1, 0.1234)
			DbAppend()
			FieldPut(1, $1234.5678)
			DbGoTop()
			Assert.Equal( $0.1234, (Currency) FieldGet(1))
			DbSkip()
			Assert.Equal( $1234.5678, (Currency) FieldGet(1))
			DbCloseArea()
		RETURN


        [Fact, Trait("Category", "DBF")];
		METHOD DBFVFP_Recall() AS VOID
			// https://github.com/X-Sharp/XSharpPublic/issues/1838
			LOCAL cFileName AS STRING
			cFileName := DbfTests.GetTempFileName()

			RddSetDefault("DBFVFP")

			DbCreate(cFileName, {{"TEST","N",6,0}})

			LOCAL lDeleted := SetDeleted(TRUE) AS LOGIC
			LOCAL eDialect := XSharp.RuntimeState.Dialect AS XSharp.XSharpDialect
			XSharp.RuntimeState.Dialect := XSharp.XSharpDialect.FoxPro


			DbCreate(cFileName, {{"FLD","N",5,0}})
			DbUseArea(TRUE,,cFileName)

			DbAppend();FieldPut(1,1)
			DbAppend();FieldPut(1,2)
			DbAppend();FieldPut(1,3)
			DbAppend();FieldPut(1,4)
			DbAppend();FieldPut(1,5)

			// delete records 2,3,5
			DbGoTop()
			DbSkip()
			DbDelete()
			DbSkip()
			DbDelete()
			DbSkip(2)
			DbDelete()
			DbCloseArea()

			DbUseArea(TRUE,"DBFVFP",cFileName)
			DbGoTop()
			RECALL NEXT 2

			SetDeleted(FALSE)
			DbGoTop()
			Assert.False( Deleted() )
			DbSkip()
			Assert.False( Deleted() )
			DbSkip()
			Assert.True( Deleted() )
			DbSkip()
			Assert.False( Deleted() )
			DbSkip()
			Assert.True( Deleted() )

			SetDeleted(TRUE)

			RECALL ALL

			SetDeleted(FALSE)
			DbGoTop()
			Assert.False( Deleted() )
			DbSkip()
			Assert.False( Deleted() )
			DbSkip()
			Assert.False( Deleted() )
			DbSkip()
			Assert.False( Deleted() )
			DbSkip()
			Assert.False( Deleted() )

			DbCloseArea()

			SetDeleted(lDeleted)
			XSharp.RuntimeState.Dialect := eDialect
		RETURN

        [Fact, Trait("Category", "DBF")];
		METHOD DBFVFP_VariableIndexLength() AS VOID
			// https://github.com/X-Sharp/XSharpPublic/issues/1837
			LOCAL cFileName AS STRING
			cFileName := DbfTests.GetTempFileName()

			RddSetDefault("DBFVFP")

			DbCreate(cFileName, {{"FLD1","C",6,0},{"FLD2","C",6,0}})
			DbUseArea(TRUE, ,cFileName)
			DbAppend();FieldPut(1,"A");FieldPut(2,"B")
			DbAppend();FieldPut(1,"B");FieldPut(2,"B2")
			DbAppend();FieldPut(1,"B");FieldPut(2,"B1")
			DbAppend();FieldPut(1,"C");FieldPut(2,"")
			DbAppend();FieldPut(1,"DDDDDD");FieldPut(2,"EEEE")
			DbAppend();FieldPut(1,"C");FieldPut(2,"D")

			Assert.True( DbCreateIndex(cFileName, "AllTrim(FLD1) + Alltrim(FLD2)") )

			Assert.False( DbSeek("AB1", TRUE) )
			Assert.False( DbSeek("AB1", FALSE) )
			Assert.True(  DbSeek("AB", FALSE) )
			Assert.Equal( 1U , RecNo() )
			Assert.True(  DbSeek("BB2", FALSE) )
			Assert.Equal( 2U , RecNo() )
			Assert.True(  DbSeek("BB1", FALSE) )
			Assert.Equal( 3U , RecNo() )
			Assert.True(  DbSeek("AB", FALSE) )
			Assert.Equal( 1U , RecNo() )
			Assert.True(  DbSeek("C", FALSE) )
			Assert.Equal( 4U , RecNo() )
			Assert.True(  DbSeek("CD", FALSE) )
			Assert.Equal( 6U , RecNo() )
			Assert.False(  DbSeek("CA", FALSE) )
			Assert.True(  DbSeek("DDDDDDEEEE", FALSE) )
			Assert.Equal( 5U , RecNo() )

			DbCloseArea()

        [Fact, Trait("Category", "DBF")];
		METHOD DBFVFP_AutoIncrField() AS VOID
			// https://github.com/X-Sharp/XSharpPublic/issues/1829

			LOCAL cFileName AS STRING
			cFileName := DbfTests.GetTempFileName()

			RddSetDefault("DBFVFP")

			DbCreate(cFileName, {{"FLD1","I:+",6,0}})
			DbUseArea(TRUE, ,cFileName, "alias1", TRUE)
			alias1->DbAppend()
			Assert.Equal(1, (INT)alias1->FieldGet(1))

			DbUseArea(TRUE, ,cFileName, "alias2", TRUE)
			alias2->DbAppend()
			Assert.Equal(2, (INT)alias2->FieldGet(1))
			alias2->DbAppend()
			Assert.Equal(3, (INT)alias2->FieldGet(1))

			alias1->DbAppend()
			Assert.Equal(4, (INT)alias1->FieldGet(1))

			DbUseArea(TRUE, ,cFileName, "alias3", TRUE)
			alias3->DbAppend()
			Assert.Equal(5, (INT)alias3->FieldGet(1))

			Assert.Equal(5U, alias3->RecCount())

			Assert.True( alias1->DbCloseArea() )
			Assert.True( alias2->DbCloseArea() )
			Assert.True( alias3->DbCloseArea() )

	END CLASS
END NAMESPACE
