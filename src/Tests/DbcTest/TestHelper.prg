//
// TestHelper.prg
//
// Shared assertion, cleanup, and setup helpers used by every TestSuite_* class.
// All methods are PUBLIC STATIC so any suite can call them directly.
//

USING System
USING System.IO
USING XSharp.RDD

STATIC CLASS TestHelper

    // -----------------------------------------------------------------------
    // Assertion helpers
    // -----------------------------------------------------------------------

    PUBLIC STATIC METHOD AssertEqual(uActual AS USUAL, uExpected AS USUAL, cMsg AS STRING) AS LOGIC
        IF uActual != uExpected
            ? "         Expected: <" + Str(uExpected) + "> Got: <" + Str(uActual) + "> (" + cMsg + ")"
            RETURN FALSE
        ENDIF
        RETURN TRUE
    END METHOD

    PUBLIC STATIC METHOD AssertTrue(lCond AS LOGIC, cMsg AS STRING) AS LOGIC
        IF !lCond
            ? "         Assertion failed: " + cMsg
            RETURN FALSE
        ENDIF
        RETURN TRUE
    END METHOD

    // Install a no-op error block and return the previous one so the caller
    // can restore it in a FINALLY block.
    PUBLIC STATIC METHOD InstallSwallowErrorBlock() AS USUAL
        RETURN ErrorBlock({|oErr| 0})
    END METHOD

    // -----------------------------------------------------------------------
    // DBC file helpers
    // -----------------------------------------------------------------------

    // Delete .DBC / .DCT / .DCX companion files for a database at cBasePath (no extension).
    PUBLIC STATIC METHOD CleanupDb(cBasePath AS STRING) AS VOID
        LOCAL aExts := <STRING>{ ".DBC", ".DCT", ".DCX" } AS STRING[]
        FOREACH VAR cExt IN aExts
            LOCAL cFile := cBasePath + cExt AS STRING
            IF File.Exists(cFile)
                TRY
                    File.Delete(cFile)
                CATCH AS Exception
                    NOP
                END TRY
            ENDIF
        NEXT
    END METHOD

    // Delete .DBF / .FPT / .CDX companion files for a table at cBasePath (no extension).
    PUBLIC STATIC METHOD CleanupTable(cBasePath AS STRING) AS VOID
        LOCAL aExts := <STRING>{ ".DBF", ".FPT", ".CDX" } AS STRING[]
        FOREACH VAR cExt IN aExts
            LOCAL cFile := cBasePath + cExt AS STRING
            IF File.Exists(cFile)
                TRY
                    File.Delete(cFile)
                CATCH AS Exception
                    NOP
                END TRY
            ENDIF
        NEXT
    END METHOD

    // Create a simple two-field free DBF at cBasePath (no extension).
    // Uses DBFVFP driver so it is compatible with DBC link operations.
    // We deactivate any open DBC before calling DbCreate so the DBFVFP driver
    // does not auto-link the new file to the active database (VFP behaviour).
    // DbCreate also leaves the file open; we close it so ADD TABLE can open it.
    PUBLIC STATIC METHOD CreateFreeTable(cBasePath AS STRING) AS LOGIC
        LOCAL oActive := Dbc.GetCurrent() AS DbcDatabase
        IF oActive != NULL_OBJECT
            Dbc.Select()
        ENDIF
        TRY
            LOCAL aStruct AS ARRAY
            aStruct := {{ "ID", "I", 4, 0 }, { "NAME", "C", 20, 0 }}
            LOCAL lOk := DbCreate(cBasePath + ".DBF", aStruct, "DBFVFP") AS LOGIC
            IF lOk
                DbCloseArea()
            ENDIF
            RETURN lOk
        FINALLY
            IF oActive != NULL_OBJECT
                Dbc.Select(oActive:Name)
            ENDIF
        END TRY
    END METHOD

    // Create, open, and activate a database at cBasePath (no extension).
    // Returns .T. on success.  The DBC is left open and active on return.
    PUBLIC STATIC METHOD OpenActiveDb(cBasePath AS STRING) AS LOGIC
        Dbc.Create(cBasePath)
        LOCAL lOk := Dbc.Open(cBasePath, FALSE, FALSE, FALSE) AS LOGIC  // exclusive so Delete/FieldPut work without explicit locking
        IF lOk
            LOCAL cName := Path.GetFileNameWithoutExtension(cBasePath):ToUpper() AS STRING
            Dbc.Select(cName)
        ENDIF
        RETURN lOk
    END METHOD

END CLASS
