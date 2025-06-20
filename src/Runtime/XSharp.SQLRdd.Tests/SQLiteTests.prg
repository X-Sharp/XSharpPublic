USING XUnit
USING XSharp.RDD.SqlRDD
USING XSharp.RDD.SqlRDD.Providers

BEGIN NAMESPACE SqlRDD.SQLiteTests

    CLASS SQLiteTests
        STATIC PRIVATE factory AS System.Data.Common.DbProviderFactory
        STATIC CONSTRUCTOR
			// This makes sure that the provider is linked in
            factory := System.Data.SQLite.SQLiteFactory.Instance

		STATIC METHOD GetSQLiteConnection() AS IntPtr
			LOCAL hConn := IntPtr.Zero AS IntPtr
			LOCAL cDataFile := "gstutor.db" AS STRING
			//LOCAL fact AS
			Assert.True(SqlDbSetProvider("SQLITE"))
			hConn := SqlDbOpenConnection(i"Data Source={cDataFile};Pooling=False;LegacyFieldTypes=True;") //
			//      hConn:= SqlDbOpenConnection("Server=(local);Initial catalog=gsTutor;Trusted_Connection=True;") // LegacyFieldTypes=True;
			LOCAL oConn AS XSharp.RDD.SqlRDD.SqlDbConnection
			oConn := SqlDbGetConnection(hConn)
			oConn:MetadataProvider := SqlMetadataProviderDatabase{oConn}
//			oConn:CallBack += MyEventHandler
		RETURN hConn

		[Fact, Trait("Category", "SQLRDD")];
		METHOD VariousSQLiteTests() AS VOID
			
			LOCAL hConn := GetSQLiteConnection() AS IntPtr

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

		[Fact, Trait("Category", "SQLRDD")];
		METHOD OrdCreateSQLite1() AS VOID

			LOCAL hConn := GetSQLiteConnection() AS IntPtr

			DbUseArea(TRUE,"SQLRDD","CUSTOMER")
			DbZap()
			Assert.True( OrdCreate("test1","test1","CUSTNUM") )
			DbAppend();FieldPut(1,2);FieldPut(2,"A")
			DbAppend();FieldPut(1,1);FieldPut(2,"C")
			DbAppend();FieldPut(1,3);FieldPut(2,"B")
			DbAppend();FieldPut(1,1);FieldPut(2,"A")
			DbAppend();FieldPut(1,3);FieldPut(2,"B")
			DbAppend();FieldPut(1,4);FieldPut(2,"A")
			DbAppend();FieldPut(1,2);FieldPut(2,"C")
			DbAppend();FieldPut(1,1);FieldPut(2,"C")
			
			LOCAL cTest AS STRING

			DbGoTop()

			cTest := ""
			DO WHILE .not. eof()
				cTest += AsString( FieldGet(1) )
				DbSkip()
			END DO
			Assert.Equal( "1122334", cTest)

			Assert.True( OrdCreate("test4","test4","FIRSTNAME") )

			DbGoTop()

			cTest := ""
			DO WHILE .not. eof()
				cTest += AllTrim( FieldGet(2) )
				DbSkip()
			END DO
			Assert.Equal( "AAABBCCC", cTest)

			DbCloseArea()

			SqlDbCloseConnection(hConn)

		[Fact, Trait("Category", "SQLRDD")];
		METHOD OrdCreateSQLite2() AS VOID

			LOCAL hConn := GetSQLiteConnection() AS IntPtr

			DbUseArea(TRUE,"SQLRDD","CUSTOMER")

			Assert.True( OrdCreate("test1","test1","FIRSTNAME") ) // OK
			
			DbGoTop()

			DbAppend();FieldPut(1,2);FieldPut(2,"A")
			DbAppend();FieldPut(1,1);FieldPut(2,"C")
			DbAppend();FieldPut(1,3);FieldPut(2,"B")
			
//			DbSkip() // this fixes the error below

			Assert.True( OrdCreate("test2","test2","CUSTNUM") )

			DbGoTop() //  error Column 'custnum' does not belong to table CUSTOMER
			
			Assert.Equals( 1 , (INT) FieldGet(1) )

			DbCloseArea()

			SqlDbCloseConnection(hConn)

		[Fact, Trait("Category", "SQLRDD")];
		METHOD CompositeOrder() AS VOID

			LOCAL hConn := GetSQLiteConnection() AS IntPtr

			DbUseArea(TRUE,"SQLRDD","CUSTOMER")

			DbZap()

			DbAppend();FieldPut(1,2);FieldPut(2,"A")
			DbAppend();FieldPut(1,1);FieldPut(2,"C")
			DbAppend();FieldPut(1,1);FieldPut(2,"B")
			DbAppend();FieldPut(1,1);FieldPut(2,"A")
			DbAppend();FieldPut(1,1);FieldPut(2,"B")
			DbAppend();FieldPut(1,3);FieldPut(2,"A")
			
			Assert.True( OrdCreate("test","test","FIRSTNAME + Str(CUSTNUM)") )

			LOCAL cTest AS STRING

			DbGoTop()

			cTest := ""
			DO WHILE .not. eof()
				cTest += AllTrim( FieldGet(2) ) + AsString( FieldGet(1) )
				DbSkip()
			END DO
			Assert.Equal( "A1A2A3B1B1C1", cTest) // empty

			DbCloseArea()

			SqlDbCloseConnection(hConn)

		[Fact, Trait("Category", "SQLRDD")];
		METHOD FilterTest() AS VOID

			LOCAL hConn := GetSQLiteConnection() AS IntPtr

			DbUseArea(TRUE,"SQLRDD","CUSTOMER")

			DbZap()

			DbAppend();FieldPut(2,"AB")
			DbAppend();FieldPut(2,"CA")
			DbAppend();FieldPut(2,"BA")
			DbAppend();FieldPut(2,"AB")
			DbAppend();FieldPut(2,"BC")
			DbAppend();FieldPut(2,"AA")
			
			
			Assert.True( DbSetFilter('FIRSTNAME="A"') )
			
			DbGoTop()

			LOCAL cTest AS STRING
			cTest := ""
			DO WHILE .not. eof()
				cTest += AllTrim( FieldGet(2) )
				FieldPut( 2, "ZZ")
				DbGoTop()
			END DO
			Assert.Equal( "ABABAA", cTest)

			DbCloseArea()

			DbUseArea(TRUE,"SQLRDD","CUSTOMER")
			DbGoTop()
			cTest := ""
			DO WHILE .not. eof()
				cTest += AllTrim( FieldGet(2) )
				DbSkip()
			END DO
			Assert.Equal( "ZZCABAZZBCZZ", cTest)

			DbCloseArea()

			SqlDbCloseConnection(hConn)

	END CLASS

END NAMESPACE

