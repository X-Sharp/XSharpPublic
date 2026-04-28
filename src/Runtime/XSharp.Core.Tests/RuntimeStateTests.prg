// RuntimeStateTests.prg
// Unit tests for XSharp.Core RuntimeState (src/Runtime/XSharp.Core/State/State.prg)
// Requires: xunit (NuGet), XSharp.Core reference

USING Xunit
USING System.Threading
USING XSharp

BEGIN NAMESPACE XSharp.Core.Tests

    PUBLIC CLASS RuntimeStateTests

        // ─────────────────────────────────────────────
        // Default Values
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD DefaultEpoch_Is1910() AS VOID
            Assert.Equal(1910u, RuntimeState.Epoch)
        END METHOD

        [Fact];
        PUBLIC METHOD DefaultDecimalSep_IsNonZero() AS VOID
            // decimal separator must be a printable ASCII char (e.g. '.' = 46)
            Assert.NotEqual(0u, RuntimeState.DecimalSep)
        END METHOD

        [Fact];
        PUBLIC METHOD DefaultExact_IsFalse() AS VOID
            Assert.False(RuntimeState.Exact)
        END METHOD

        [Fact];
        PUBLIC METHOD DefaultBreakLevel_IsZero() AS VOID
            LOCAL oState AS RuntimeState
            oState := RuntimeState.GetInstance()
            Assert.Equal(0, oState:BreakLevel)
        END METHOD

        [Fact];
        PUBLIC METHOD DefaultDeleted_IsFalse() AS VOID
            Assert.False(RuntimeState.Deleted)
        END METHOD

        [Fact];
        PUBLIC METHOD DefaultSoftSeek_IsFalse() AS VOID
            Assert.False(RuntimeState.SoftSeek)
        END METHOD

        // ─────────────────────────────────────────────
        // Get / Set Round-Trips
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD Exact_RoundTrip_True() AS VOID
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            TRY
                RuntimeState.Exact := TRUE
                Assert.True(RuntimeState.Exact)
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD Exact_RoundTrip_False() AS VOID
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            TRY
                RuntimeState.Exact := FALSE
                Assert.False(RuntimeState.Exact)
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD Decimals_RoundTrip() AS VOID
            LOCAL nOld AS DWORD
            nOld := RuntimeState.Decimals
            TRY
                RuntimeState.Decimals := 4
                Assert.Equal(4u, RuntimeState.Decimals)
            FINALLY
                RuntimeState.Decimals := nOld
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetValue_ReturnsPreviousValue() AS VOID
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.SoftSeek
            TRY
                RuntimeState.SoftSeek := FALSE
                LOCAL lPrev AS LOGIC
                lPrev := RuntimeState.SetValue<LOGIC>(XSharp.Set.Softseek, TRUE)
                Assert.False(lPrev)
                Assert.True(RuntimeState.SoftSeek)
            FINALLY
                RuntimeState.SoftSeek := lOld
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // Thread Isolation
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD RuntimeState_IsThreadLocal() AS VOID
            LOCAL nOrig AS DWORD
            nOrig := RuntimeState.Decimals
            TRY
                RuntimeState.Decimals := 2
                LOCAL nThreadSaw AS DWORD
                nThreadSaw := 99

                LOCAL oThread AS Thread
                oThread := Thread{ ThreadStart { { => ;
                    // New thread gets a clone: should also see 2
                    nThreadSaw := RuntimeState.Decimals
                    // Change inside thread must not bleed back
                    RuntimeState.Decimals := 6
                }}}
                oThread:Start()
                oThread:Join()

                // Main thread is unaffected
                Assert.Equal(2u, RuntimeState.Decimals)
                // Thread saw the cloned value
                Assert.Equal(2u, nThreadSaw)
            FINALLY
                RuntimeState.Decimals := nOrig
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // StateChanged Event
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD StateChangedEvent_IsFiredOnSetValue() AS VOID
            LOCAL oCaptured AS StateChangedEventArgs
            oCaptured := NULL
            LOCAL oHandler AS XSharp.StateChanged
            oHandler := StateChanged{ { e => oCaptured := e } }
            RuntimeState.StateChanged += oHandler
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Deleted
            TRY
                RuntimeState.Deleted := TRUE
                Assert.NotNull(oCaptured)
                Assert.Equal(XSharp.Set.Deleted, oCaptured:Setting)
            FINALLY
                RuntimeState.StateChanged -= oHandler
                RuntimeState.Deleted := lOld
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD StateChangedEvent_OldAndNewValues_AreCorrect() AS VOID
            LOCAL oCaptured AS StateChangedEventArgs
            oCaptured := NULL
            LOCAL oHandler AS XSharp.StateChanged
            oHandler := StateChanged{ { e => oCaptured := e } }
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            RuntimeState.Exact := FALSE
            RuntimeState.StateChanged += oHandler
            TRY
                RuntimeState.Exact := TRUE
                Assert.NotNull(oCaptured)
                Assert.Equal((OBJECT)FALSE, oCaptured:OldValue)
                Assert.Equal((OBJECT)TRUE,  oCaptured:NewValue)
            FINALLY
                RuntimeState.StateChanged -= oHandler
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // DialectChanged Event
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD DialectChangedEvent_IsFired_WhenDialectChanges() AS VOID
            LOCAL oOldDialect AS XSharpDialect
            oOldDialect := RuntimeState.Dialect
            LOCAL capturedOld AS XSharpDialect
            LOCAL capturedNew AS XSharpDialect
            LOCAL bFired AS LOGIC
            bFired := FALSE

            LOCAL oHandler AS XSharp.DialectChanged
            oHandler := DialectChanged{ { o, n =>
                capturedOld := o
                capturedNew := n
                bFired := TRUE
            }}
            RuntimeState.DialectChanged += oHandler
            TRY
                RuntimeState.Dialect := XSharpDialect.FoxPro
                Assert.True(bFired)
                Assert.Equal(oOldDialect,         capturedOld)
                Assert.Equal(XSharpDialect.FoxPro, capturedNew)
            FINALLY
                RuntimeState.DialectChanged -= oHandler
                RuntimeState.Dialect := oOldDialect
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD DialectChangedEvent_NotFired_WhenSameDialect() AS VOID
            RuntimeState.Dialect := XSharpDialect.Core
            LOCAL bFired AS LOGIC
            bFired := FALSE
            LOCAL oHandler AS XSharp.DialectChanged
            oHandler := DialectChanged{ { o, n => bFired := TRUE } }
            RuntimeState.DialectChanged += oHandler
            TRY
                RuntimeState.Dialect := XSharpDialect.Core   // same value
                Assert.False(bFired)
            FINALLY
                RuntimeState.DialectChanged -= oHandler
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // StringCompare
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD StringCompare_ExactFalse_ShortRHSMatches() AS VOID
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            TRY
                RuntimeState.Exact := FALSE
                // "Hello World" starts with "Hello" => equal in non-exact mode
                Assert.Equal(0, RuntimeState.StringCompare("Hello World", "Hello"))
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD StringCompare_ExactTrue_DifferentLengthNotEqual() AS VOID
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            TRY
                RuntimeState.Exact := TRUE
                Assert.NotEqual(0, RuntimeState.StringCompare("Hello World", "Hello"))
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD StringCompare_NullLHS_IsLessThanString() AS VOID
            Assert.Equal(-1, RuntimeState.StringCompare(NULL, "abc"))
        END METHOD

        [Fact];
        PUBLIC METHOD StringCompare_BothNull_IsZero() AS VOID
            Assert.Equal(0, RuntimeState.StringCompare(NULL, NULL))
        END METHOD

        [Fact];
        PUBLIC METHOD StringCompare_SameReference_IsZero() AS VOID
            LOCAL s AS STRING
            s := "hello"
            Assert.Equal(0, RuntimeState.StringCompare(s, s))
        END METHOD

        [Fact];
        PUBLIC METHOD StringCompare_EmptyRHS_IsZeroInNonExactMode() AS VOID
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.Exact
            TRY
                RuntimeState.Exact := FALSE
                // Any LHS starts with "" in non-exact mode
                Assert.Equal(0, RuntimeState.StringCompare("anything", ""))
            FINALLY
                RuntimeState.Exact := lOld
            END TRY
        END METHOD

    END CLASS

END NAMESPACE

