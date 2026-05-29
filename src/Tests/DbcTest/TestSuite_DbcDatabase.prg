//
// TestSuite_DbcDatabase.prg
//
// Tests for database-level DBC operations:
//   CREATE DATABASE, OPEN DATABASE, SET DATABASE TO, CLOSE DATABASES
//

USING System
USING System.IO
USING XSharp.RDD
#include "E:\XSharp\dev\XSharp\src\Common\FoxProCmd.xh"

STATIC CLASS TestSuite_DbcDatabase

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
        ? "  --- TestSuite_DbcDatabase ---"

        Safe("CREATE DATABASE creates .DBC file on disk",          {|| TestCreateCreatesFile()})
        Safe("CREATE DATABASE does not open the database",         {|| TestCreateDoesNotOpen()})
        Safe("OPEN DATABASE registers the database as used",       {|| TestOpenIsUsed()})
        Safe("OPEN DATABASE does not activate the database",       {|| TestOpenDoesNotActivate()})
        Safe("OPEN DATABASE EXCLUSIVE opens successfully",         {|| TestOpenExclusive()})
        Safe("OPEN DATABASE NOUPDATE opens successfully",          {|| TestOpenReadOnly()})
        Safe("OPEN DATABASE twice returns FALSE",                  {|| TestOpenAlreadyOpen()})
        Safe("OPEN DATABASE non-existent file returns FALSE",      {|| TestOpenNonExistent()})
        Safe("OPEN DATABASE VALIDATE on valid DBC succeeds",       {|| TestOpenValidate()})
        Safe("SET DATABASE TO activates the database",             {|| TestSetDatabaseActivates()})
        Safe("CLOSE DATABASES closes and deactivates",             {|| TestCloseDatabasesDeactivates()})

        RETURN _nFail == 0
    END METHOD

    // -----------------------------------------------------------------------
    // Tests
    // -----------------------------------------------------------------------

    PRIVATE STATIC METHOD TestCreateCreatesFile() AS LOGIC
        LOCAL cPath := DbPath("create1") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            RETURN AssertTrue(File(cPath + ".DBC"), ".DBC file must exist after CREATE DATABASE")
        FINALLY
            TestHelper.CleanupDb(cPath)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestCreateDoesNotOpen() AS LOGIC
        LOCAL cPath := DbPath("create2") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            RETURN AssertTrue(!Dbc.IsUsed("CREATE2"), "CREATE DATABASE must not register the database as open")
        FINALLY
            TestHelper.CleanupDb(cPath)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestOpenIsUsed() AS LOGIC
        LOCAL cPath := DbPath("open1") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            OPEN DATABASE (cPath)
            RETURN AssertTrue(Dbc.IsUsed("OPEN1"), "OPEN DATABASE must register the database as used")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cPath)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestOpenDoesNotActivate() AS LOGIC
        LOCAL cPath := DbPath("open2") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            OPEN DATABASE (cPath)
            RETURN AssertTrue(Dbc.GetCurrent() == NULL_OBJECT, "OPEN DATABASE must not activate the database")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cPath)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestOpenExclusive() AS LOGIC
        LOCAL cPath := DbPath("openex") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            OPEN DATABASE (cPath) EXCLUSIVE
            RETURN AssertTrue(Dbc.IsUsed("OPENEX"), "OPEN DATABASE EXCLUSIVE must register the database as used")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cPath)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestOpenReadOnly() AS LOGIC
        LOCAL cPath := DbPath("openro") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            OPEN DATABASE (cPath) NOUPDATE
            RETURN AssertTrue(Dbc.IsUsed("OPENRO"), "OPEN DATABASE NOUPDATE must register the database as used")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cPath)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestOpenAlreadyOpen() AS LOGIC
        LOCAL cPath := DbPath("opendup") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            OPEN DATABASE (cPath)
            LOCAL lSecond := Dbc.Open(cPath + ".DBC", TRUE, FALSE, FALSE) AS LOGIC
            RETURN AssertTrue(!lSecond, "OPEN DATABASE on an already-open DBC must return FALSE") ;
                .AND. AssertTrue(Dbc.IsUsed("OPENDUP"), "First open must still be registered")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cPath)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestOpenNonExistent() AS LOGIC
        LOCAL cPath := DbPath("doesnotexist") AS STRING
        TestHelper.CleanupDb(cPath)
        LOCAL lResult := Dbc.Open(cPath + ".DBC", TRUE, FALSE, FALSE) AS LOGIC
        RETURN AssertTrue(!lResult, "OPEN DATABASE on a non-existent file must return FALSE")
    END METHOD

    PRIVATE STATIC METHOD TestOpenValidate() AS LOGIC
        LOCAL cPath := DbPath("openval") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            OPEN DATABASE (cPath) VALIDATE
            RETURN AssertTrue(Dbc.IsUsed("OPENVAL"), "OPEN DATABASE VALIDATE on a valid DBC must succeed")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cPath)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestSetDatabaseActivates() AS LOGIC
        LOCAL cPath := DbPath("setdb1") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            OPEN DATABASE (cPath)
            SET DATABASE TO SETDB1
            LOCAL oCurrent := Dbc.GetCurrent() AS DbcDatabase
            RETURN AssertTrue(oCurrent != NULL_OBJECT, "SET DATABASE TO must activate the database") ;
                .AND. AssertEqual(oCurrent:Name, "SETDB1", "Active database name must match")
        FINALLY
            CLOSE DATABASES ALL
            TestHelper.CleanupDb(cPath)
        END TRY
    END METHOD

    PRIVATE STATIC METHOD TestCloseDatabasesDeactivates() AS LOGIC
        LOCAL cPath := DbPath("close1") AS STRING
        TestHelper.CleanupDb(cPath)
        TRY
            CREATE DATABASE (cPath)
            OPEN DATABASE (cPath)
            SET DATABASE TO CLOSE1
            CLOSE DATABASES
            RETURN AssertTrue(!Dbc.IsUsed("CLOSE1"), "CLOSE DATABASES must unregister the database") ;
                .AND. AssertTrue(Dbc.GetCurrent() == NULL_OBJECT, "CLOSE DATABASES must deactivate the database")
        FINALLY
            TestHelper.CleanupDb(cPath)
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
