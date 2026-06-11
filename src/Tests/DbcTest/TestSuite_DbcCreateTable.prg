//
// TestSuite_DbcCreateTable.prg
//
// Tests for CREATE TABLE with an active DBC:
//   auto-registration, FREE keyword, long field names, NAME clause,
//   no active DBC, table left open after creation.
//

USING System
USING System.IO
USING XSharp.RDD
//#include "E:\XSharp\dev\XSharp\src\Common\FoxProCmd.xh"

STATIC CLASS TestSuite_DbcCreateTable

    PRIVATE STATIC _nPass     AS INT
    PRIVATE STATIC _nFail     AS INT
    PRIVATE STATIC _cDataPath AS STRING

    PUBLIC STATIC PROPERTY PassCount AS INT
        GET
            RETURN _nPass
        END GET
    END PROPERTY

    PUBLIC STATIC PROPERTY FailCount AS INT
        GET
            RETURN _nFail
        END GET
    END PROPERTY

    PUBLIC STATIC METHOD RunAll(cDataPath AS STRING) AS LOGIC
        _nPass     := 0
        _nFail     := 0
        _cDataPath := cDataPath

        SET(Set.Default, cDataPath)

        ?
        ? "  --- TestSuite_DbcCreateTable ---"

        Safe("CREATE TABLE auto-registers in active DBC",              {|| TestCreateTableRegisters()})
        Safe("CREATE TABLE leaves table open",                         {|| TestCreateTableIsOpen()})
        Safe("CREATE TABLE FREE does not register in DBC",             {|| TestCreateTableFreeNotRegistered()})
        Safe("CREATE TABLE without active DBC creates file only",      {|| TestCreateTableNoActiveDbc()})
        Safe("CREATE TABLE stores long field names in DBC",            {|| TestCreateTableLongFieldNames()})
        Safe("CREATE TABLE NAME stores long table name in DBC",        {|| TestCreateTableWithName()})
        Safe("CREATE TABLE with quoted full path",                     {|| TestCreateTableQuotedPath()})
        Safe("CREATE TABLE with PRIVATE variable path",               {|| TestCreateTablePrivatePath()})
        Safe("CREATE TABLE deduplicates colliding truncated names",   {|| TestCreateTableDuplicateTruncatedNames()})

        RETURN _nFail == 0
    END METHOD

    // -----------------------------------------------------------------------
    // Tests
    // -----------------------------------------------------------------------

    PRIVATE STATIC METHOD TestCreateTableRegisters() AS LOGIC
        LOCAL cDb  := DbPath("crt_reg")  AS STRING
        LOCAL cTbl := DbPath("crt_reg_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            CREATE TABLE crt_reg_t (Id I, Name C(20))
            LOCAL oDb := Dbc.GetCurrent() AS DbcDatabase
            RETURN AssertTrue(oDb:FindTable("crt_reg_t") != NULL_OBJECT, "Table must be registered in DBC after CREATE TABLE")
        FINALLY
            CloseTable(cTbl)
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestCreateTableIsOpen() AS LOGIC
        LOCAL cDb  := DbPath("crt_open")  AS STRING
        LOCAL cTbl := DbPath("crt_open_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            CREATE TABLE crt_open_t (Id I, Name C(20))
            // Used() + Alias() are reliable; DbUsed() uses a different lookup path and
            // doesn't find the area opened via DbUseArea inside CreateTableCursor.
            RETURN AssertTrue(Used() .AND. Alias() == "CRT_OPEN_T", "Table must be open with alias CRT_OPEN_T after CREATE TABLE")
        FINALLY
            CloseTable(cTbl)
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestCreateTableFreeNotRegistered() AS LOGIC
        LOCAL cDb  := DbPath("crt_free")  AS STRING
        LOCAL cTbl := DbPath("crt_free_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            CREATE TABLE crt_free_t FREE (Id I, Name C(20))
            LOCAL oDb := Dbc.GetCurrent() AS DbcDatabase
            RETURN AssertTrue(File(cTbl + ".DBF"), ".DBF file must exist after CREATE TABLE FREE") ;
                .AND. AssertTrue(oDb:FindTable("crt_free_t") == NULL_OBJECT, "FREE table must NOT be registered in DBC")
        FINALLY
            CloseTable(cTbl)
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestCreateTableNoActiveDbc() AS LOGIC
        LOCAL cTbl := DbPath("crt_nodb_t") AS STRING
        TestHelper.CleanupTable(cTbl)
        TRY
            Dbc.Select()
            CREATE TABLE crt_nodb_t (Id I, Name C(20))
            RETURN AssertTrue(File(cTbl + ".DBF"), ".DBF file must exist when CREATE TABLE is used without an active DBC")
        FINALLY
            CloseTable(cTbl)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestCreateTableLongFieldNames() AS LOGIC
        LOCAL cDb  := DbPath("crt_long")  AS STRING
        LOCAL cTbl := DbPath("crt_long_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            CREATE TABLE crt_long_t (CustomerLastName C(50), OrderDate D)
            LOCAL oDb    := Dbc.GetCurrent() AS DbcDatabase
            LOCAL oTable := oDb:FindTable("crt_long_t") AS DbcTable
            IF !AssertTrue(oTable != NULL_OBJECT, "Table must be registered in DBC") ; RETURN FALSE ; ENDIF
            LOCAL lFirstFieldLong := oTable:Fields:Count >= 1 .AND. oTable:Fields[0]:ObjectName:Length > 10 AS LOGIC
            RETURN AssertTrue(lFirstFieldLong, "First field name in DBC must be the full long name (> 10 chars)") ;
                .AND. AssertEqual(oTable:Fields[0]:ObjectName, "CustomerLastName", "Full long field name must be stored in DBC")
        FINALLY
            CloseTable(cTbl)
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestCreateTableWithName() AS LOGIC
        LOCAL cDb  := DbPath("crt_name")  AS STRING
        LOCAL cTbl := DbPath("crt_name_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            CREATE TABLE crt_name_t NAME "MyLongTableName" (Id I)
            LOCAL oDb := Dbc.GetCurrent() AS DbcDatabase
            RETURN AssertTrue(oDb:FindTable("MyLongTableName") != NULL_OBJECT, "Table must be registered under its long NAME in DBC")
        FINALLY
            CloseTable(cTbl)
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    // STRING_CONST path: ParseTableName accepts a quoted string, so a full Windows path
    // avoids the colon-as-alias-separator ambiguity. The UDC stringifies the quotes.
    PRIVATE STATIC METHOD TestCreateTableQuotedPath() AS LOGIC
        LOCAL cTbl := DbPath("crt_qpath_t") AS STRING
        TestHelper.CleanupTable(cTbl)
        // Build quoted SQL dynamically — the parser treats the quoted token as STRING_CONST.
        // To be sure that that if we had a fullPath in the CREATE TABLE command it works
        LOCAL cSql := 'CREATE TABLE "' + cTbl + '" FREE (Id I, Name C(20))' AS STRING
        TRY
            Dbc.Select()
            __SqlCreateTable(cSql)
            RETURN AssertTrue(File(cTbl + ".DBF"), ".DBF must exist when created with a quoted full path")
        FINALLY
            CloseTable(cTbl)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    // PRIVATE variable path: ParseTableName calls __VarGetSafe(name) for the (varName) syntax.
    // This lets CREATE TABLE use a full runtime path via the UDC — the variable must be
    // PRIVATE or PUBLIC, not LOCAL, because __VarGetSafe looks up the memvar stack.
    PRIVATE STATIC METHOD TestCreateTablePrivatePath() AS LOGIC
        LOCAL cTbl := DbPath("crt_ppath_t") AS STRING
        TestHelper.CleanupTable(cTbl)
        TRY
            Dbc.Select()
            PRIVATE cPrivatePath AS STRING
            cPrivatePath := cTbl
            CREATE TABLE (cPrivatePath) FREE (Id I, Name C(20))
            RETURN AssertTrue(File(cTbl + ".DBF"), ".DBF must exist when created via PRIVATE variable path")
        FINALLY
            CloseTable(cTbl)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    // CustomerAddressLine and CustomerAddressCity share the same first 10 chars ("CustomerAd").
    // Without the dedup fix both physical DBF field names would be "CustomerAd", producing an
    // invalid DBF.  With the fix the second field gets the suffix "0" → "CustomerA0".
    PRIVATE STATIC METHOD TestCreateTableDuplicateTruncatedNames() AS LOGIC
        LOCAL cDb  := DbPath("crt_dedup")  AS STRING
        LOCAL cTbl := DbPath("crt_dedup_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            CREATE TABLE crt_dedup_t (CustomerAddressLine C(50), CustomerAddressCity C(50))
            LOCAL oDb    := Dbc.GetCurrent() AS DbcDatabase
            LOCAL oTable := oDb:FindTable("crt_dedup_t") AS DbcTable
            IF !AssertTrue(oTable != NULL_OBJECT, "Table must be registered in DBC") ; RETURN FALSE ; ENDIF
            // Physical names must be distinct
            LOCAL cField1 := FieldName(1):ToUpper() AS STRING
            LOCAL cField2 := FieldName(2):ToUpper() AS STRING
            IF !AssertTrue(cField1 != cField2, "Physical DBF field names must be unique after truncation deduplication") ; RETURN FALSE ; ENDIF
            // First field keeps plain 10-char truncation; second gets suffix "0"
            IF !AssertEqual(cField1, "CUSTOMERAD", "First field physical name") ; RETURN FALSE ; ENDIF
            IF !AssertEqual(cField2, "CUSTOMERA0", "Second field physical name after dedup suffix") ; RETURN FALSE ; ENDIF
            // DBC must preserve the full long names
            IF !AssertEqual(oTable:Fields[0]:ObjectName, "CustomerAddressLine", "DBC long name for field 1") ; RETURN FALSE ; ENDIF
            RETURN AssertEqual(oTable:Fields[1]:ObjectName, "CustomerAddressCity", "DBC long name for field 2")
        FINALLY
            CloseTable(cTbl)
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    PRIVATE STATIC METHOD DbPath(cName AS STRING) AS STRING
        RETURN Path.Combine(_cDataPath, cName)
    END METHOD

    // Close the work area opened by CREATE TABLE.
    // X# stores the alias exactly as passed to DbUseArea (EmbeddedSql uses
    // filename-without-extension as-is, lowercase for relative names), so
    // try both cases to be safe.
    PRIVATE STATIC METHOD CloseTable(cBasePath AS STRING) AS VOID
        LOCAL cAlias := Path.GetFileNameWithoutExtension(cBasePath) AS STRING
        IF DbUsed(cAlias)
            DbCloseArea(cAlias)
        ELSEIF DbUsed(cAlias:ToUpper())
            DbCloseArea(cAlias:ToUpper())
        ENDIF
    END METHOD

    // -----------------------------------------------------------------------
    // Infrastructure
    // -----------------------------------------------------------------------

    PRIVATE STATIC METHOD Safe(cName AS STRING, cbTest AS USUAL) AS VOID
        LOCAL lOk := FALSE AS LOGIC
        TRY
            lOk := (LOGIC) Eval(cbTest)
        CATCH ex AS Exception
            ? "  [FAIL] " + cName
            ? "         EXCEPTION: " + ex:Message
            _nFail += 1
            RETURN
        END TRY
        IF lOk
            _nPass += 1
            ? "  [PASS] " + cName
        ELSE
            _nFail += 1
            ? "  [FAIL] " + cName
        ENDIF
    END METHOD

    PRIVATE STATIC METHOD AssertEqual(uActual AS USUAL, uExpected AS USUAL, cMsg AS STRING) AS LOGIC
        RETURN TestHelper.AssertEqual(uActual, uExpected, cMsg)
    END METHOD

    PRIVATE STATIC METHOD AssertTrue(lCond AS LOGIC, cMsg AS STRING) AS LOGIC
        RETURN TestHelper.AssertTrue(lCond, cMsg)
    END METHOD

END CLASS
