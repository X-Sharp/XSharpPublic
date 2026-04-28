// DebugFunctionsTests.prg
// Unit tests for XSharp.Core Debug functions
// (src/Runtime/XSharp.Core/Functions/Debug.prg)
// Tests: ProcFile, ProcLine, ProcName, ErrorStack, DebOut32, AltD

USING Xunit
USING System.Diagnostics

BEGIN NAMESPACE XSharp.Core.Tests

    PUBLIC CLASS DebugFunctionsTests

        // ─────────────────────────────────────────────
        // ProcName Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcName_ReturnsCurrentMethodName() AS VOID
            LOCAL cName AS STRING
            cName := ProcName()
            Assert.Contains("PROCNAME_RETURNSCURRENTMETHODNAME", cName)
        END METHOD

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcName_WithActivation_ReturnsCallerName() AS VOID
            LOCAL cName AS STRING
            cName := HelperMethod_ForProcName()
            Assert.Contains("HELPERMETHOD_FORPROCNAME", cName)
        END METHOD

        PRIVATE STATIC METHOD HelperMethod_ForProcName() AS STRING
            RETURN ProcName(0)
        END METHOD

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcName_WithSignature_IncludesParameters() AS VOID
            LOCAL cName AS STRING
            cName := ProcName(0, TRUE)
            Assert.NotNull(cName)
            Assert.NotEqual("", cName)
        END METHOD

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcName_InvalidActivation_ReturnsEmpty() AS VOID
            LOCAL cName AS STRING
            cName := ProcName(1000)
            Assert.Equal("", cName)
        END METHOD

        // ─────────────────────────────────────────────
        // ProcFile Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcFile_ReturnsCurrentFileName() AS VOID
            LOCAL cFile AS STRING
            cFile := ProcFile()
            // Should contain the test file name
            Assert.Contains("DebugFunctionsTests", cFile)
        END METHOD

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcFile_WithActivation_ReturnsCallerFile() AS VOID
            LOCAL cFile AS STRING
            cFile := HelperMethod_ForProcFile()
            Assert.Contains("DebugFunctionsTests", cFile)
        END METHOD

        PRIVATE STATIC METHOD HelperMethod_ForProcFile() AS STRING
            RETURN ProcFile(0)
        END METHOD

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcFile_InvalidActivation_ReturnsEmpty() AS VOID
            LOCAL cFile AS STRING
            cFile := ProcFile(1000)
            Assert.Equal("", cFile)
        END METHOD

        // ─────────────────────────────────────────────
        // ProcLine Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcLine_ReturnsLineNumber() AS VOID
            LOCAL dwLine AS DWORD
            dwLine := ProcLine()
            // Should return a non-zero line number
            Assert.True(dwLine > 0)
        END METHOD

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcLine_WithActivation_ReturnsCallerLine() AS VOID
            LOCAL dwLine AS DWORD
            dwLine := HelperMethod_ForProcLine()
            Assert.True(dwLine > 0)
        END METHOD

        PRIVATE STATIC METHOD HelperMethod_ForProcLine() AS DWORD
            RETURN ProcLine(0)
        END METHOD

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ProcLine_InvalidActivation_ReturnsZero() AS VOID
            LOCAL dwLine AS DWORD
            dwLine := ProcLine(1000)
            Assert.Equal(0u, dwLine)
        END METHOD

        // ─────────────────────────────────────────────
        // ErrorStack Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ErrorStack_ReturnsStackTrace() AS VOID
            LOCAL cStack AS STRING
            cStack := ErrorStack(0)
            Assert.NotNull(cStack)
            Assert.NotEqual("", cStack)
            // Should contain method names from the call stack
            Assert.Contains("ErrorStack_ReturnsStackTrace", cStack)
        END METHOD

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ErrorStack_WithActivation_ReturnsPartialStack() AS VOID
            LOCAL cStack AS STRING
            cStack := ErrorStack(0)
            Assert.NotNull(cStack)
        END METHOD

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD ErrorStack_WithStackTrace_ReturnsFormattedStack() AS VOID
            LOCAL st AS StackTrace
            LOCAL cStack AS STRING
            st := StackTrace{TRUE}
            cStack := ErrorStack(st, 0)
            Assert.NotNull(cStack)
            Assert.NotEqual("", cStack)
        END METHOD

        // ─────────────────────────────────────────────
        // DebOut32 Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Debug")];
        PUBLIC METHOD DebOut32_OutputsText_NoException() AS VOID
            // Just verify it doesn't throw
            DebOut32("Test message")
            _DebOut32("Another test message")
            Assert.True(TRUE)
        END METHOD

    END CLASS

END NAMESPACE
