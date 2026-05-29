//
// TestSuite_Experimental.prg
//
// Sandbox for work-in-progress tests.
//
// PURPOSE
// -------
// Write exploratory or diagnostic tests here during design and debugging.
// Once a test is proven and stable it should be promoted to the appropriate
// permanent TestSuite_*.prg file.
//
// The call to this suite is commented out in TestRunner.prg so it never
// contributes to the official pass/fail total.  Uncomment that call while
// you are actively working on an experiment; re-comment it before committing.
//
// Current experiments
// -------------------
//   (none — sandbox is empty; add new experiments here)
//

USING System
USING System.Collections.Generic

STATIC CLASS TestSuite_Experimental

    PRIVATE STATIC _nPass     AS INT
    PRIVATE STATIC _nFail     AS INT

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
        _nPass := 0
        _nFail := 0

        SET(Set.Default, cDataPath)

        ?
        ? "  --- Experimental (work-in-progress) ---"

        // Add new experiment calls here, e.g.:
        // TestSuite_Experimental.Safe("My experiment", {|| TestSuite_Experimental.TestMyExperiment()})

        RETURN _nFail == 0
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
