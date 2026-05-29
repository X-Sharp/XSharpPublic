//
// TestRunner.prg
//
// Test orchestrator: calls each TestSuite_*.RunAll() in order and prints
// the grand total.  Individual test methods live in the TestSuite_* files.
//
// Suite layout
// ─────────────────────────────────────────────────────
//  TestSuite_Misc            — miscellaneous smoke tests (date/string functions on USUAL/PRIVATE)
//  TestSuite_DbcDatabase     — database-level operations (CREATE/OPEN/CLOSE DATABASE, SET DATABASE TO)
//  TestSuite_DbcTable        — table-level operations (ADD/REMOVE/RENAME TABLE)
//  TestSuite_DbcCreateTable  — CREATE TABLE with DBC (auto-register, FREE, long names, NAME clause)
// ─────────────────────────────────────────────────────
//
//  TestSuite_Experimental — sandbox (not counted); call is commented out below.
//

USING System


STATIC CLASS TestRunner

    PRIVATE STATIC _nPass AS INT
    PRIVATE STATIC _nFail AS INT

    // -----------------------------------------------------------------------
    // Entry point
    // -----------------------------------------------------------------------

    PUBLIC STATIC METHOD RunAll(cDataPath AS STRING) AS LOGIC
        _nPass := 0
        _nFail := 0

        LOCAL cbPrev := ErrorBlock({|oErr| MyErrorHandler(oErr)}) AS USUAL

        ? "=== XSharp DBC Tests ==="

        // Experimental sandbox — uncomment while designing new tests;
        // re-comment before committing so it never pollutes the official total.
        //TestSuite_Experimental.RunAll(cDataPath)
        //_nPass += TestSuite_Experimental.PassCount
        //_nFail += TestSuite_Experimental.FailCount

        TestSuite_Misc.RunAll(cDataPath)
        _nPass += TestSuite_Misc.PassCount
        _nFail += TestSuite_Misc.FailCount

        TestSuite_DbcDatabase.RunAll(cDataPath)
        _nPass += TestSuite_DbcDatabase.PassCount
        _nFail += TestSuite_DbcDatabase.FailCount

        TestSuite_DbcTable.RunAll(cDataPath)
        _nPass += TestSuite_DbcTable.PassCount
        _nFail += TestSuite_DbcTable.FailCount

        TestSuite_DbcCreateTable.RunAll(cDataPath)
        _nPass += TestSuite_DbcCreateTable.PassCount
        _nFail += TestSuite_DbcCreateTable.FailCount

        ErrorBlock(cbPrev)

        ?
        ? "=== Results: " + _nPass:ToString() + " passed, " + _nFail:ToString() + " failed ==="
        RETURN _nFail == 0
    END METHOD

END CLASS


FUNCTION MyErrorHandler(oError)
    ? oError:Message
    ? oError:Source
    ? oError:StackTrace
    BREAK oError
