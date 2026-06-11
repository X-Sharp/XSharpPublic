//
// TestSuite_DbcTable.prg
//
// Tests for table-level DBC operations:
//   ADD TABLE [NAME], REMOVE TABLE [DELETE], RENAME TABLE
//

USING System
USING System.IO
USING XSharp.RDD
//#include "E:\XSharp\dev\XSharp\src\Common\FoxProCmd.xh"

STATIC CLASS TestSuite_DbcTable

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
        ? "  --- TestSuite_DbcTable ---"

        // ADD TABLE
        Safe("ADD TABLE registers table in DBC",             {|| TestAddTableRegisters()})
        Safe("ADD TABLE NAME registers long name",           {|| TestAddTableWithLongName()})
        Safe("ADD TABLE registers fields in DBC",            {|| TestAddTableRegistersFields()})
        Safe("ADD TABLE non-existent file returns FALSE",    {|| TestAddTableNonExistent()})
        Safe("ADD TABLE without active DB returns FALSE",    {|| TestAddTableNoActiveDb()})

        // REMOVE TABLE
        Safe("REMOVE TABLE unregisters from DBC",            {|| TestRemoveTableUnregisters()})
        Safe("REMOVE TABLE keeps file on disk by default",   {|| TestRemoveTableKeepsFile()})
        Safe("REMOVE TABLE DELETE deletes file from disk",   {|| TestRemoveTableDelete()})
        Safe("REMOVE TABLE unknown name returns FALSE",      {|| TestRemoveTableNotFound()})

        // RENAME TABLE
        Safe("RENAME TABLE changes logical name in DBC",     {|| TestRenameTableChangesName()})
        Safe("RENAME TABLE does not rename physical file",   {|| TestRenameTableFileUnchanged()})
        Safe("RENAME TABLE unknown name returns FALSE",      {|| TestRenameTableNotFound()})

        RETURN _nFail == 0
    END METHOD

    // -----------------------------------------------------------------------
    // ADD TABLE tests
    // -----------------------------------------------------------------------

    PRIVATE STATIC METHOD TestAddTableRegisters() AS LOGIC
        LOCAL cDb  := DbPath("addreg")  AS STRING
        LOCAL cTbl := DbPath("addreg_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            LOCAL lCreate := TestHelper.CreateFreeTable(cTbl) AS LOGIC
            IF !AssertTrue(lCreate, "CreateFreeTable must succeed") ; RETURN FALSE ; ENDIF
            LOCAL lAdd := __VFPAddTable(cTbl, "") AS LOGIC
            IF !AssertTrue(lAdd, "ADD TABLE must return TRUE") ; RETURN FALSE ; ENDIF
            LOCAL oDb := Dbc.GetCurrent() AS DbcDatabase
            RETURN AssertTrue(oDb:FindTable("ADDREG_T") != NULL_OBJECT, "Table must appear in DBC after ADD TABLE")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestAddTableWithLongName() AS LOGIC
        LOCAL cDb  := DbPath("addname")  AS STRING
        LOCAL cTbl := DbPath("addname_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            LOCAL lCreate := TestHelper.CreateFreeTable(cTbl) AS LOGIC
            IF !AssertTrue(lCreate, "CreateFreeTable must succeed") ; RETURN FALSE ; ENDIF
            LOCAL lAdd := __VFPAddTable(cTbl, "MyLongTableName") AS LOGIC
            IF !AssertTrue(lAdd, "ADD TABLE NAME must return TRUE") ; RETURN FALSE ; ENDIF
            LOCAL oDb := Dbc.GetCurrent() AS DbcDatabase
            RETURN AssertTrue(oDb:FindTable("MyLongTableName") != NULL_OBJECT, "Long name must be registered in DBC")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestAddTableRegistersFields() AS LOGIC
        LOCAL cDb  := DbPath("addfld")  AS STRING
        LOCAL cTbl := DbPath("addfld_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            LOCAL lCreate := TestHelper.CreateFreeTable(cTbl) AS LOGIC  // ID (I) + NAME (C)
            IF !AssertTrue(lCreate, "CreateFreeTable must succeed") ; RETURN FALSE ; ENDIF
            LOCAL lAdd := __VFPAddTable(cTbl, "") AS LOGIC
            IF !AssertTrue(lAdd, "ADD TABLE must return TRUE") ; RETURN FALSE ; ENDIF
            LOCAL oDb    := Dbc.GetCurrent() AS DbcDatabase
            LOCAL oTable := oDb:FindTable("ADDFLD_T") AS DbcTable
            RETURN AssertTrue(oTable != NULL_OBJECT, "Table must appear in DBC") ;
                .AND. AssertEqual(oTable:Fields:Count, 2, "DBC must record 2 fields")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestAddTableNonExistent() AS LOGIC
        LOCAL cDb  := DbPath("addnofile")  AS STRING
        LOCAL cTbl := DbPath("nosuchtable") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            LOCAL lResult := __VFPAddTable(cTbl + ".DBF", "") AS LOGIC
            RETURN AssertTrue(!lResult, "ADD TABLE on a non-existent file must return FALSE")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestAddTableNoActiveDb() AS LOGIC
        LOCAL cTbl := DbPath("addnodb_t") AS STRING
        TestHelper.CleanupTable(cTbl)
        TRY
            // Ensure no database is active
            Dbc.Select()
            TestHelper.CreateFreeTable(cTbl)
            LOCAL lResult := __VFPAddTable(cTbl + ".DBF", "") AS LOGIC
            RETURN AssertTrue(!lResult, "ADD TABLE without an active database must return FALSE")
        FINALLY
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    // -----------------------------------------------------------------------
    // REMOVE TABLE tests
    // -----------------------------------------------------------------------

    PRIVATE STATIC METHOD TestRemoveTableUnregisters() AS LOGIC
        LOCAL cDb  := DbPath("remreg")   AS STRING
        LOCAL cTbl := DbPath("remreg_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            TestHelper.CreateFreeTable(cTbl)
            ADD TABLE (cTbl)
            REMOVE TABLE REMREG_T
            LOCAL oDb := Dbc.GetCurrent() AS DbcDatabase
            RETURN AssertTrue(oDb:FindTable("REMREG_T") == NULL_OBJECT, "Table must be gone from DBC after REMOVE TABLE")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestRemoveTableKeepsFile() AS LOGIC
        LOCAL cDb  := DbPath("remkeep")   AS STRING
        LOCAL cTbl := DbPath("remkeep_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            TestHelper.CreateFreeTable(cTbl)
            ADD TABLE (cTbl)
            REMOVE TABLE REMKEEP_T
            RETURN AssertTrue(File(cTbl + ".DBF"), ".DBF file must still exist after REMOVE TABLE without DELETE")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestRemoveTableDelete() AS LOGIC
        LOCAL cDb  := DbPath("remdel")   AS STRING
        LOCAL cTbl := DbPath("remdel_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            TestHelper.CreateFreeTable(cTbl)
            ADD TABLE (cTbl)
            REMOVE TABLE REMDEL_T DELETE
            RETURN AssertTrue(!File(cTbl + ".DBF"), ".DBF file must be deleted after REMOVE TABLE DELETE")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestRemoveTableNotFound() AS LOGIC
        LOCAL cDb := DbPath("remnotfound") AS STRING
        TestHelper.CleanupDb(cDb)
        TRY
            TestHelper.OpenActiveDb(cDb)
            LOCAL lResult := __VFPRemoveTable("nosuchTable", FALSE, FALSE) AS LOGIC
            RETURN AssertTrue(!lResult, "REMOVE TABLE on an unknown table name must return FALSE")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
        END TRY
    END METHOD

    // -----------------------------------------------------------------------
    // RENAME TABLE tests
    // -----------------------------------------------------------------------

    PRIVATE STATIC METHOD TestRenameTableChangesName() AS LOGIC
        LOCAL cDb  := DbPath("renname")   AS STRING
        LOCAL cTbl := DbPath("renname_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            TestHelper.CreateFreeTable(cTbl)
            ADD TABLE (cTbl)
            RENAME TABLE RENNAME_T TO RenamedTable
            LOCAL oDb := Dbc.GetCurrent() AS DbcDatabase
            RETURN AssertTrue(oDb:FindTable("RENNAME_T")   == NULL_OBJECT, "Old name must not be found after RENAME TABLE") ;
                .AND. AssertTrue(oDb:FindTable("RenamedTable") != NULL_OBJECT, "New name must be found after RENAME TABLE")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestRenameTableFileUnchanged() AS LOGIC
        LOCAL cDb  := DbPath("renfile")   AS STRING
        LOCAL cTbl := DbPath("renfile_t") AS STRING
        TestHelper.CleanupDb(cDb)
        TestHelper.CleanupTable(cTbl)
        TRY
            TestHelper.OpenActiveDb(cDb)
            TestHelper.CreateFreeTable(cTbl)
            ADD TABLE (cTbl)
            RENAME TABLE RENFILE_T TO NewLogicalName
            RETURN AssertTrue(File(cTbl + ".DBF"), "Physical .DBF must still exist after RENAME TABLE")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
            TestHelper.CleanupTable(cTbl)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestRenameTableNotFound() AS LOGIC
        LOCAL cDb := DbPath("rennotfound") AS STRING
        TestHelper.CleanupDb(cDb)
        TRY
            TestHelper.OpenActiveDb(cDb)
            LOCAL lResult := __VFPRenameTable("nosuchTable", "newName") AS LOGIC
            RETURN AssertTrue(!lResult, "RENAME TABLE on an unknown name must return FALSE")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cDb)
        END TRY
    END METHOD

    // -----------------------------------------------------------------------
    // Infrastructure
    // -----------------------------------------------------------------------

    PRIVATE STATIC METHOD DbPath(cName AS STRING) AS STRING
        RETURN Path.Combine(_cDataPath, cName)
    END METHOD

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
