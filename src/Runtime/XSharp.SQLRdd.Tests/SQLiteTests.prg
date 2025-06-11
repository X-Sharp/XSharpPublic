USING XUnit
using XSharp.RDD.SqlRDD
using XSharp.RDD.SqlRDD.Providers

BEGIN NAMESPACE XSharp.SQLRdd.Tests

	CLASS SQLiteTests

		[Fact, Trait("Category", "DBFFuncs")];
		METHOD DBCreate_Tests() AS VOID

			LOCAL hConn := IntPtr.Zero AS IntPtr
			LOCAL cDataFile := "gstutor.db" AS STRING
			//LOCAL fact AS System.Data.Common.DbProviderFactory
			// This makes sure that the provider is linked in
            var Fact := System.Data.SQLite.SQLiteFactory.Instance
            ? Fact:ToString()
			Assert.True(SqlDbSetProvider("SQLITE"))
			hConn := SqlDbOpenConnection(i"Data Source={cDataFile};Pooling=False;LegacyFieldTypes=True;") //
			//      hConn:= SqlDbOpenConnection("Server=(local);Initial catalog=gsTutor;Trusted_Connection=True;") // LegacyFieldTypes=True;
			LOCAL oConn AS XSharp.RDD.SqlRDD.SqlDbConnection
			oConn := SqlDbGetConnection(hConn)
			oConn:MetadataProvider := SqlMetadataProviderDatabase{oConn}
//			oConn:CallBack += MyEventHandler

			Assert.True( DbUseArea(TRUE,"SQLRDD","CUSTOMER") )
			Assert.True( DbZap() )
			Assert.True( DbCloseArea() )

			DbUseArea(TRUE,"SQLRDD","CUSTOMER")
			DbAppend()
			FieldPut (2,"ABC")
			DbAppend()
			FieldPut (2,"DEF")
			DbAppend()
			FieldPut (2,"ZZZ")
			DbAppend()
			FieldPut (2,"AAA")
			DbCloseArea()

			LOCAL cTest AS STRING

			cTest := ""
			DbUseArea(TRUE,"SQLRDD","CUSTOMER")
			DbGoTop()
			DO WHILE .not. eof()
				cTest += AllTrim( FieldGet(2) )
				DbSkip()
			END DO
			Assert.Equal( "ABCDEFZZZAAA", cTest)
			DbCloseArea()

			DbUseArea(TRUE,"SQLRDD","CUSTOMER")
			DbSkip()
			FieldPut(2,"123")
			DbSkip()
			DbSkip()
			FieldPut(2,"456")
			DbSkip()
			DbCloseArea()

			cTest := ""
			DbUseArea(TRUE,"SQLRDD","CUSTOMER")
			DbGoTop()
			DO WHILE .not. eof()
				cTest += AllTrim( FieldGet(2) )
				DbSkip()
			END DO
			Assert.Equal( "ABC123ZZZ456", cTest)
			DbCloseArea()

			DbUseArea(TRUE,"SQLRDD","CUSTOMER")
			FieldPut(2,"000")
			DbSkip()
			FieldPut(2,"123")
			DbSkip()
			FieldPut(2,"999")
			DbSkip()
			FieldPut(2,"456")
			DbSkip()
			DbCloseArea()

			cTest := ""
			DbUseArea(TRUE,"SQLRDD","CUSTOMER")
			DbGoTop()
			DO WHILE .not. eof()
				cTest += AllTrim( FieldGet(2) )
				DbSkip()
			END DO
			Assert.Equal( "000123999456", cTest)
			DbCloseArea()


			SqlDbCloseConnection(hConn)

	END CLASS

END NAMESPACE

