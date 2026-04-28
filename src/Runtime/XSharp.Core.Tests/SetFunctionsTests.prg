// SetFunctionsTests.prg
// Unit tests for XSharp.Core Set functions (src/Runtime/XSharp.Core/State/Set.prg)
// Requires: xunit (NuGet), XSharp.Core reference

USING Xunit
USING XSharp

BEGIN NAMESPACE XSharp.Core.Tests

    PUBLIC CLASS SetFunctionsTests

        // ─────────────────────────────────────────────
        // SetEpoch
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD SetEpoch_SetsEpochYearAndCentury() AS VOID
            LOCAL nOld AS DWORD
            nOld := SetEpoch()
            TRY
                SetEpoch(1950)
                Assert.Equal(1950u, RuntimeState.Epoch)
                Assert.Equal(50u,   RuntimeState.EpochYear)
                Assert.Equal(2000u, RuntimeState.EpochCent)  // (1950/100 + 1) * 100
            FINALLY
                SetEpoch(nOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetEpoch_Returns_PreviousValue() AS VOID
            LOCAL nOld AS DWORD
            nOld := SetEpoch()
            TRY
                SetEpoch(1910)
                LOCAL nPrev AS DWORD
                nPrev := SetEpoch(2000)
                Assert.Equal(1910u, nPrev)
            FINALLY
                SetEpoch(nOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetEpoch_YearMod100_IsCorrect() AS VOID
            LOCAL nOld AS DWORD
            nOld := SetEpoch()
            TRY
                SetEpoch(2050)
                Assert.Equal(50u, RuntimeState.EpochYear)
            FINALLY
                SetEpoch(nOld)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // SetCentury
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD SetCentury_True_ExtendsFormatToFourDigitYear() AS VOID
            LOCAL lOld AS LOGIC
            lOld := SetCentury()
            TRY
                SetCentury(FALSE)
                SetCentury(TRUE)
                Assert.Contains("YYYY", RuntimeState.DateFormat)
            FINALLY
                SetCentury(lOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetCentury_False_ShortensFormatToTwoDigitYear() AS VOID
            LOCAL lOld AS LOGIC
            lOld := SetCentury()
            TRY
                SetCentury(TRUE)
                SetCentury(FALSE)
                Assert.DoesNotContain("YYYY", RuntimeState.DateFormat)
                Assert.Contains("YY", RuntimeState.DateFormat)
            FINALLY
                SetCentury(lOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetCentury_ReturnsPreviousValue() AS VOID
            LOCAL lOld AS LOGIC
            lOld := SetCentury()
            TRY
                SetCentury(FALSE)
                LOCAL lPrev AS LOGIC
                lPrev := SetCentury(TRUE)
                Assert.False(lPrev)
            FINALLY
                SetCentury(lOld)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // SetDateFormat
        // ─────────────────────────────────────────────

        [Theory];
        [InlineData("MM/DD/YY",  (DWORD)XSharp.DateCountry.American)];
        [InlineData("DD/MM/YY",  (DWORD)XSharp.DateCountry.British)];
        [InlineData("DD.MM.YY",  (DWORD)XSharp.DateCountry.German)];
        [InlineData("DD-MM-YY",  (DWORD)XSharp.DateCountry.Italian)];
        [InlineData("YY/MM/DD",  (DWORD)XSharp.DateCountry.Japanese)];
        [InlineData("YY.MM.DD",  (DWORD)XSharp.DateCountry.Ansi)];
        [InlineData("MM-DD-YY",  (DWORD)XSharp.DateCountry.USA)];
        [InlineData("MM/DD/YYYY",  (DWORD)XSharp.DateCountry.American)];
        [InlineData("DD/MM/YYYY",  (DWORD)XSharp.DateCountry.British)];
        [InlineData("DD.MM.YYYY",  (DWORD)XSharp.DateCountry.German)];
        [InlineData("DD-MM-YYYY",  (DWORD)XSharp.DateCountry.Italian)];
        [InlineData("YYYY/MM/DD",  (DWORD)XSharp.DateCountry.Japanese)];
        [InlineData("YYYY.MM.DD",  (DWORD)XSharp.DateCountry.Ansi)];
        [InlineData("MM-DD-YYYY",  (DWORD)XSharp.DateCountry.USA)];
        PUBLIC METHOD SetDateFormat_SetsExpectedDateCountry(cFormat AS STRING, nExpected AS DWORD) AS VOID
            LOCAL cOld AS STRING
            cOld := GetDateFormat()
            TRY
                SetDateFormat(cFormat)
                Assert.Equal(nExpected, RuntimeState.DateCountry)
                if (cFormat.Contains("YYYY"))
                    Assert.True(SetCentury())
                else
                    Assert.False(SetCentury())
                endif
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetDateFormat_InvalidFormat_IsIgnored() AS VOID
            LOCAL cOld AS STRING
            cOld := GetDateFormat()
            TRY
                SetDateFormat("INVALID")
                // Format without DD/MM/YY tokens must be silently ignored
                Assert.Equal(cOld, RuntimeState.DateFormat)
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetDateFormat_ReturnsPreviousValue() AS VOID
            LOCAL cOld AS STRING
            cOld := GetDateFormat()
            TRY
                SetDateFormat("MM/DD/YY")
                LOCAL cPrev AS STRING
                cPrev := SetDateFormat("DD/MM/YY")
                Assert.Equal("MM/DD/YY", cPrev)
            FINALLY
                SetDateFormat(cOld)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // SetDateCountry
        // ─────────────────────────────────────────────

        [Theory];
        [InlineData((DWORD)XSharp.DateCountry.German,   "DD.MM.YY")];
        [InlineData((DWORD)XSharp.DateCountry.Japanese, "YY/MM/DD")];
        [InlineData((DWORD)XSharp.DateCountry.USA,      "MM-DD-YY")];
        PUBLIC METHOD SetDateCountry_SetsExpectedFormat(nCountry AS DWORD, cExpected AS STRING) AS VOID
            LOCAL cOld AS STRING
            cOld := GetDateFormat()
            LOCAL lOldCentury AS LOGIC
            lOldCentury := SetCentury()
            TRY
                SetCentury(FALSE)
                SetDateCountry(nCountry)
                Assert.Equal(cExpected, RuntimeState.DateFormat)
            FINALLY
                SetDateFormat(cOld)
                SetCentury(lOldCentury)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetDateCountry_Century_True_ProducesLongYear() AS VOID
            LOCAL cOld AS STRING
            cOld := GetDateFormat()
            LOCAL lOldCentury AS LOGIC
            lOldCentury := SetCentury()
            TRY
                SetCentury(TRUE)
                SetDateCountry((DWORD)XSharp.DateCountry.American)
                Assert.Equal("MM/DD/YYYY", RuntimeState.DateFormat)
            FINALLY
                SetDateFormat(cOld)
                SetCentury(lOldCentury)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // SetHours
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD SetHours_12_SetsAmPmTrue() AS VOID
            LOCAL nOld AS LONG
            nOld := SetHours()
            TRY
                SetHours(12)
                Assert.True(RuntimeState.GetValue<LOGIC>(XSharp.Set.AmPm))
            FINALLY
                SetHours(nOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetHours_24_SetsAmPmFalse() AS VOID
            LOCAL nOld AS LONG
            nOld := SetHours()
            TRY
                SetHours(24)
                Assert.False(RuntimeState.GetValue<LOGIC>(XSharp.Set.AmPm))
            FINALLY
                SetHours(nOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetHours_InvalidValue_ThrowsError() AS VOID
            Assert.Throws<XSharp.Error>(action{ {=> SetHours(13) }})
        END METHOD

        [Fact];
        PUBLIC METHOD SetHours_ReturnsPreviousValue() AS VOID
            LOCAL nOld AS LONG
            nOld := SetHours()
            TRY
                SetHours(12)
                LOCAL nPrev AS LONG
                nPrev := SetHours(24)
                Assert.Equal(12, nPrev)
            FINALLY
                SetHours(nOld)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // SetCollation
        // ─────────────────────────────────────────────

        [Theory];
        [InlineData("CLIPPER", CollationMode.Clipper)];
        [InlineData("XPP",     CollationMode.Xpp)];
        [InlineData("WINDOWS", CollationMode.Windows)];
        [InlineData("UNICODE", CollationMode.Unicode)];
        [InlineData("ORDINAL", CollationMode.Ordinal)];
        PUBLIC METHOD SetCollation_SetsCollationMode(cMode AS STRING, eExpected AS CollationMode) AS VOID
            LOCAL cOld AS STRING
            cOld := SetCollation()
            TRY
                SetCollation(cMode)
                Assert.Equal(eExpected, RuntimeState.CollationMode)
            FINALLY
                SetCollation(cOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetCollation_InvalidName_ThrowsError() AS VOID
            Assert.Throws<XSharp.Error>(action{ {=> SetCollation("BOGUS") }})
        END METHOD

        [Fact];
        PUBLIC METHOD SetCollation_ReturnsPreviousValue() AS VOID
            LOCAL cOld AS STRING
            cOld := SetCollation()
            TRY
                SetCollation("UNICODE")
                LOCAL cPrev AS STRING
                cPrev := SetCollation("ORDINAL")
                Assert.Equal("UNICODE", cPrev)
            FINALLY
                SetCollation(cOld)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // SetInternational
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD SetInternational_Clipper_SetsDecimalDot_ThousandComma() AS VOID
            LOCAL cOld AS STRING
            cOld := SetInternational()
            TRY
                SetInternational("CLIPPER")
                Assert.Equal(46u, RuntimeState.DecimalSep)   // '.'
                Assert.Equal(44u, RuntimeState.ThousandSep)  // ','
            FINALLY
                SetInternational(cOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetInternational_Windows_SetsWindowsCollation() AS VOID
            LOCAL cOld AS STRING
            cOld := SetInternational()
            TRY
                SetInternational("WINDOWS")
                Assert.Equal(CollationMode.Windows, RuntimeState.CollationMode)
            FINALLY
                SetInternational(cOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetInternational_InvalidValue_ThrowsError() AS VOID
            Assert.Throws<XSharp.Error>(action{ {=> SetInternational("BOGUS") } })
        END METHOD

        // ─────────────────────────────────────────────
        // SetExact
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD SetExact_ReturnsPreviousValue() AS VOID
            LOCAL lOld AS LOGIC
            lOld := SetExact()
            TRY
                SetExact(FALSE)
                LOCAL lPrev AS LOGIC
                lPrev := SetExact(TRUE)
                Assert.False(lPrev)
                Assert.True(SetExact())
            FINALLY
                SetExact(lOld)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // SetAnsi (also syncs CharSet)
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD SetAnsi_True_SetsCharSetZero() AS VOID
            LOCAL lOld AS LOGIC
            lOld := SetAnsi()
            TRY
                SetAnsi(TRUE)
                Assert.Equal(0L, RuntimeState.GetValue<LONG>(XSharp.Set.CharSet))
            FINALLY
                SetAnsi(lOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetAnsi_False_SetsCharSetOne() AS VOID
            LOCAL lOld AS LOGIC
            lOld := SetAnsi()
            TRY
                SetAnsi(FALSE)
                Assert.Equal(1L, RuntimeState.GetValue<LONG>(XSharp.Set.CharSet))
            FINALLY
                SetAnsi(lOld)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // SetAmPm (also syncs Hours)
        // ─────────────────────────────────────────────

        [Fact];
        PUBLIC METHOD SetAmPm_True_SetsHours12() AS VOID
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.GetValue<LOGIC>(XSharp.Set.AmPm)
            TRY
                SetAmPm(TRUE)
                Assert.Equal(12L, RuntimeState.GetValue<LONG>(XSharp.Set.Hours))
            FINALLY
                SetAmPm(lOld)
            END TRY
        END METHOD

        [Fact];
        PUBLIC METHOD SetAmPm_False_SetsHours24() AS VOID
            LOCAL lOld AS LOGIC
            lOld := RuntimeState.GetValue<LOGIC>(XSharp.Set.AmPm)
            TRY
                SetAmPm(FALSE)
                Assert.Equal(24L, RuntimeState.GetValue<LONG>(XSharp.Set.Hours))
            FINALLY
                SetAmPm(lOld)
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // SetDeleted / SetSoftSeek / SetUnique / SetFixed
        // ─────────────────────────────────────────────

        [Theory];
        [InlineData(TRUE)];
        [InlineData(FALSE)];
        PUBLIC METHOD SetDeleted_RoundTrip(lValue AS LOGIC) AS VOID
            LOCAL lOld AS LOGIC
            lOld := SetDeleted()
            TRY
                SetDeleted(lValue)
                Assert.Equal(lValue, SetDeleted())
            FINALLY
                SetDeleted(lOld)
            END TRY
        END METHOD

        [Theory];
        [InlineData(TRUE)];
        [InlineData(FALSE)];
        PUBLIC METHOD SetSoftSeek_RoundTrip(lValue AS LOGIC) AS VOID
            LOCAL lOld AS LOGIC
            lOld := SetSoftSeek()
            TRY
                SetSoftSeek(lValue)
                Assert.Equal(lValue, SetSoftSeek())
            FINALLY
                SetSoftSeek(lOld)
            END TRY
        END METHOD

    END CLASS

END NAMESPACE

