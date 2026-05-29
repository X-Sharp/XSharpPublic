//
// TestSuite_Misc.prg
//
// Miscellaneous smoke tests originally in Program.prg.
// These verify that date/string functions accept USUAL and PRIVATE variables
// without throwing exceptions.
//

USING System

STATIC CLASS TestSuite_Misc

    PRIVATE STATIC _nPass AS INT
    PRIVATE STATIC _nFail AS INT

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

        ?
        ? "  --- TestSuite_Misc ---"

        Safe("Date functions on USUAL variable", {|| TestDateFunctionsUsual()})
        Safe("Date functions on PRIVATE variable", {|| TestDateFunctionsPrivate()})
        Safe("Quarter with string USUAL", {|| TestQuarterWithString()})

        RETURN _nFail == 0
    END METHOD

    // -----------------------------------------------------------------------
    // Tests
    // -----------------------------------------------------------------------

    PRIVATE STATIC METHOD TestDateFunctionsUsual() AS LOGIC
        LOCAL u AS USUAL
        u := Today()
        ? GoMonth(u, 1)
        ? Quarter(u, 1)
        ? Week(u, 1)
        ? Week(u, 2)
        ? Week(u, 3)
        ? Dmy(u)
        ? Mdy(u)
        RETURN TRUE
    END METHOD

    PRIVATE STATIC METHOD TestDateFunctionsPrivate() AS LOGIC
        PRIVATE p
        p := Today()
        ? GoMonth(p, 1)
        ? Quarter(p, 1)
        ? Week(p, 1)
        ? Week(p, 2)
        ? Week(p, 3)
        ? Dmy(p)
        ? Mdy(p)
        RETURN TRUE
    END METHOD

    PRIVATE STATIC METHOD TestQuarterWithString() AS LOGIC
        LOCAL u AS USUAL
        u := "abc"
        ? Quarter(u, 1)
        RETURN TRUE
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
