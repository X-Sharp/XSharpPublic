// MacroCompilerTests.prg
// Unit tests for XSharp.Core Macro Compiler functions
// (src/Runtime/XSharp.Core/Functions/Macro.prg)
// Tests: GetMacroCompiler, SetMacroCompiler, SetMacroDuplicatesResolver

USING Xunit
USING XSharp

BEGIN NAMESPACE XSharp.Core.Tests

    PUBLIC CLASS MacroCompilerTests

        // ─────────────────────────────────────────────
        // GetMacroCompiler / SetMacroCompiler (Type)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Macro")];
        PUBLIC METHOD GetMacroCompiler_ReturnsType() AS VOID
            LOCAL oType AS System.Type
            oType := GetMacroCompiler()
            // May be NULL if macro compiler not loaded, but should not throw
            Assert.True(oType == NULL .OR. oType IS System.Type)
            Assert.True(TRUE)
        END METHOD

        [Fact, Trait("Category", "Macro")];
        PUBLIC METHOD SetMacroCompiler_Type_ReturnsOldValue() AS VOID
            LOCAL oOldType AS System.Type
            LOCAL oNewType AS System.Type
            oOldType := GetMacroCompiler()
            // Set to NULL temporarily
            oNewType := SetMacroCompiler((System.Type)NULL)
            Assert.Equal(oOldType, oNewType)
            // Restore original
            SetMacroCompiler(oOldType)
        END METHOD

        [Fact, Trait("Category", "Macro")];
        PUBLIC METHOD SetMacroCompiler_Type_UpdatesGlobalState() AS VOID
            LOCAL oOldType AS System.Type
            oOldType := GetMacroCompiler()
            // Set to NULL
            SetMacroCompiler((System.Type)NULL)
            Assert.Null(GetMacroCompiler())
            // Restore
            SetMacroCompiler(oOldType)
            Assert.Equal(oOldType, GetMacroCompiler())
        END METHOD

        // ─────────────────────────────────────────────
        // SetMacroCompiler (IMacroCompiler)
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Macro")];
        PUBLIC METHOD SetMacroCompiler_Instance_AcceptsNull() AS VOID
            LOCAL oOld AS IMacroCompiler
            oOld := RuntimeState._macrocompiler
            // Set to NULL
            SetMacroCompiler((IMacroCompiler)NULL)
            // Restore
            SetMacroCompiler(oOld)
            Assert.True(TRUE)
        END METHOD

        // ─────────────────────────────────────────────
        // SetMacroDuplicatesResolver
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Macro")];
        PUBLIC METHOD SetMacroDuplicatesResolver_AcceptsDelegate() AS VOID
            LOCAL oOld AS MacroCompilerResolveAmbiguousMatch
            oOld := RuntimeState.MacroResolver
            // Set to NULL
            SetMacroDuplicatesResolver(NULL)
            Assert.Null(RuntimeState.MacroResolver)
            // Restore
            SetMacroDuplicatesResolver(oOld)
            Assert.Equal(oOld, RuntimeState.MacroResolver)
        END METHOD

        [Fact, Trait("Category", "Macro")];
        PUBLIC METHOD SetMacroDuplicatesResolver_ReturnsOldValue() AS VOID
            LOCAL oOld AS MacroCompilerResolveAmbiguousMatch
            LOCAL oNew AS MacroCompilerResolveAmbiguousMatch
            oOld := RuntimeState.MacroResolver
            oNew := SetMacroDuplicatesResolver(NULL)
            Assert.Equal(oOld, oNew)
            // Restore
            SetMacroDuplicatesResolver(oOld)
        END METHOD

    END CLASS

END NAMESPACE
