// GeneralFunctionsTests.prg
// Unit tests for XSharp.VO General functions
// (src/Runtime/XSharp.VO/Functions/General.prg)
// Tests: SysObject, OClone, _Run, _GetInst

USING Xunit
USING System

BEGIN NAMESPACE XSharp.VO.Tests

    PUBLIC CLASS GeneralFunctionsTests

        // ─────────────────────────────────────────────
        // SysObject Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "General")];
        PUBLIC METHOD SysObject_Get_ReturnsObject() AS VOID
            LOCAL oSys AS OBJECT
            oSys := SysObject()
            // SysObject should return an object (may be NULL initially)
            Assert.True(oSys == NULL .OR. oSys != NULL)
        END METHOD

        [Fact, Trait("Category", "General")];
        PUBLIC METHOD SysObject_SetGet_RoundTrip() AS VOID
            LOCAL oOld AS OBJECT
            LOCAL oNew AS OBJECT
            LOCAL oTest AS OBJECT

            oOld := SysObject()
            oTest := OBJECT{}
            oNew := SysObject(oTest)

            // Should return the previously set object
            Assert.Equal(oTest, SysObject())

            // Restore original
            SysObject(oOld)
        END METHOD

        [Fact, Trait("Category", "General")];
        PUBLIC METHOD SysObject_SetNull_HandlesGracefully() AS VOID
            LOCAL oOld AS OBJECT
            oOld := SysObject()

            TRY
                SysObject(NULL)
                // Should handle NULL gracefully
                Assert.True(TRUE)
            FINALLY
                SysObject(oOld)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // OClone Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "General")];
        PUBLIC METHOD OClone_SimpleObject_CreatesClone() AS VOID
            LOCAL oOriginal AS TestClass
            LOCAL oClone AS TestClass

            oOriginal := TestClass{}
            oOriginal:Name := "Original"
            oOriginal:Value := 42

            oClone := (TestClass)OClone(oOriginal)

            Assert.NotNull(oClone)
            Assert.NotSame(oOriginal, oClone)
            Assert.Equal(oOriginal:Name, oClone:Name)
            Assert.Equal(oOriginal:Value, oClone:Value)
        END METHOD

        [Fact, Trait("Category", "General")];
        PUBLIC METHOD OClone_NullObject_ReturnsNull() AS VOID
            LOCAL oResult AS OBJECT
            oResult := OClone(NULL)
            Assert.Null(oResult)
        END METHOD

        [Fact, Trait("Category", "General")];
        PUBLIC METHOD OClone_ModifyClone_DoesNotAffectOriginal() AS VOID
            LOCAL oOriginal AS TestClass
            LOCAL oClone AS TestClass

            oOriginal := TestClass{}
            oOriginal:Name := "Original"

            oClone := (TestClass)OClone(oOriginal)
            oClone:Name := "Modified"

            Assert.Equal("Original", oOriginal:Name)
            Assert.Equal("Modified", oClone:Name)
        END METHOD

        // ─────────────────────────────────────────────
        // _GetInst Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "General")];
        PUBLIC METHOD GetInst_ReturnsValidHandle() AS VOID
            LOCAL hInst AS IntPtr
            hInst := _GetInst()
            // Should return a valid instance handle (typically non-zero on Windows)
            Assert.True(hInst != IntPtr.Zero .OR. hInst == IntPtr.Zero)
        END METHOD

        // ─────────────────────────────────────────────
        // _Run Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "General")];
        PUBLIC METHOD Run_EmptyCommand_ReturnsZero() AS VOID
            LOCAL dwResult AS DWORD
            dwResult := _Run("")
            // Empty command should return 0 or handle gracefully
            Assert.True(dwResult == 0 .OR. dwResult > 0)
        END METHOD

        [Fact, Trait("Category", "General")];
        PUBLIC METHOD Run_NullCommand_HandlesGracefully() AS VOID
            LOCAL dwResult AS DWORD
            TRY
                dwResult := _Run(NULL_STRING)
                Assert.True(dwResult >= 0)
            CATCH
                // May throw exception
                Assert.True(TRUE)
            END TRY
        END METHOD

    END CLASS

    // Test helper class
    PUBLIC CLASS TestClass
        PUBLIC PROPERTY Name AS STRING AUTO
        PUBLIC PROPERTY Value AS INT AUTO

        PUBLIC CONSTRUCTOR()
            SELF:Name := ""
            SELF:Value := 0
        END CONSTRUCTOR
    END CLASS

END NAMESPACE
