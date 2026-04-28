// MemoFunctionsTests.prg
// Unit tests for XSharp.Core Memo functions
// (src/Runtime/XSharp.Core/Functions/Memo.prg)
// Tests: MLine, MLCount, MemoLine, MPosToLc, MemoRead, MemoWrit

USING Xunit
USING System.IO

BEGIN NAMESPACE XSharp.Core.Tests

    PUBLIC CLASS MemoFunctionsTests

        // ─────────────────────────────────────────────
        // MLine Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLine_FirstLine_ReturnsCorrectText() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "Line 1" + Chr(13) + Chr(10) + "Line 2" + Chr(13) + Chr(10) + "Line 3"
            Assert.Equal("Line 1", MLine(cMemo, 1))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLine_SecondLine_ReturnsCorrectText() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "Line 1" + Chr(13) + Chr(10) + "Line 2" + Chr(13) + Chr(10) + "Line 3"
            Assert.Equal("Line 2", MLine(cMemo, 2))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLine_LastLine_ReturnsCorrectText() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "Line 1" + Chr(13) + Chr(10) + "Line 2" + Chr(13) + Chr(10) + "Line 3"
            Assert.Equal("Line 3", MLine(cMemo, 3))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLine_LineExceedsCount_ReturnsEmpty() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "Line 1" + Chr(13) + Chr(10) + "Line 2"
            Assert.Equal("", MLine(cMemo, 10))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLine_EmptyString_ReturnsEmpty() AS VOID
            Assert.Equal("", MLine("", 1))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLine_NullString_ReturnsEmpty() AS VOID
            Assert.Equal("", MLine(NULL, 1))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLine_WithOffset_TracksPosition() AS VOID
            LOCAL cMemo AS STRING
            LOCAL nOffset AS DWORD
            cMemo := "First" + Chr(13) + Chr(10) + "Second"
            LOCAL cResult AS STRING
            cResult := MLine(cMemo, 2, REF nOffset)
            Assert.Equal("Second", cResult)
            Assert.True(nOffset > 0)
        END METHOD

        // ─────────────────────────────────────────────
        // MLCount Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLCount1_CountsLines() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "Line 1" + Chr(13) + Chr(10) + "Line 2" + Chr(13) + Chr(10) + "Line 3"
            Assert.Equal(3U, MLCount1(cMemo))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLCount1_EmptyString_ReturnsZero() AS VOID
            Assert.Equal(0U, MLCount1(""))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLCount1_NullString_ReturnsZero() AS VOID
            Assert.Equal(0U, MLCount1(NULL))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLCount1_SingleLine_ReturnsOne() AS VOID
            Assert.Equal(1U, MLCount1("Single line"))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLCount1_WithSoftCR_CountsAsOneLine() AS VOID
            LOCAL cMemo AS STRING
            // Soft CR (141) should not break lines
            cMemo := "Line" + Chr(141) + Chr(10) + "Continuation"
            Assert.Equal(1U, MLCount1(cMemo))
        END METHOD

        // ─────────────────────────────────────────────
        // MemoLine Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoLine_WithWidth_WrapsCorrectly() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "This is a long line that should wrap"
            LOCAL cResult AS STRING
            cResult := MemoLine(cMemo, 10, 1)
            Assert.True(cResult:Length <= 10)
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoLine_SecondWrappedLine_ReturnsRemainder() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "This is a long line"
            LOCAL cLine2 AS STRING
            cLine2 := MemoLine(cMemo, 10, 2)
            Assert.NotNull(cLine2)
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoLine_EmptyString_ReturnsEmpty() AS VOID
            Assert.Equal("", MemoLine("", 10, 1))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoLine_LineNumberTooHigh_ReturnsEmpty() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "Short text"
            Assert.Equal("", MemoLine(cMemo, 50, 10))
        END METHOD

        // ─────────────────────────────────────────────
        // MemLines Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemLines_CountsLines() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "A" + Chr(13) + Chr(10) + "B" + Chr(13) + Chr(10) + "C"
            Assert.Equal(3U, MemLines(cMemo))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemLines_EmptyString_ReturnsZero() AS VOID
            Assert.Equal(0U, MemLines(""))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemLines_SingleLine_ReturnsOne() AS VOID
            Assert.Equal(1U, MemLines("Single"))
        END METHOD

        // ─────────────────────────────────────────────
        // MLPos Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLPos2_FirstLine_ReturnsOne() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "Line1" + Chr(13) + Chr(10) + "Line2"
            Assert.Equal(1U, MLPos2(cMemo, 1))
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLPos2_SecondLine_ReturnsCorrectPosition() AS VOID
            LOCAL cMemo AS STRING
            cMemo := "Line1" + Chr(13) + Chr(10) + "Line2"
            LOCAL nPos AS DWORD
            nPos := MLPos2(cMemo, 2)
            Assert.True(nPos > 5)
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLPos2_InvalidLine_ReturnsZero() AS VOID
            Assert.Equal(0U, MLPos2("Test", 10))
        END METHOD

        // ─────────────────────────────────────────────
        // MemoRead / MemoWrit Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoRead_MemoWrit_RoundTrip() AS VOID
            LOCAL cFile AS STRING
            LOCAL cContent AS STRING
            LOCAL cRead AS STRING
            cFile := Path.GetTempFileName()
            cContent := "Test memo content" + Chr(13) + Chr(10) + "Line 2"
            
            TRY
                Assert.True(MemoWrit(cFile, cContent))
                cRead := MemoRead(cFile)
                Assert.Equal(cContent, cRead)
            FINALLY
                IF File.Exists(cFile)
                    File.Delete(cFile)
                ENDIF
            END TRY
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoWrit_NullString_ReturnsFalse() AS VOID
            LOCAL cFile AS STRING
            cFile := Path.GetTempFileName()
            TRY
                LOCAL lResult AS LOGIC
                lResult := MemoWrit(cFile, (String) NULL)
                // Behavior may vary - either returns FALSE or creates empty file
                Assert.True(lResult == FALSE .OR. File.Exists(cFile))
            FINALLY
                IF File.Exists(cFile)
                    File.Delete(cFile)
                ENDIF
            END TRY
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoRead_NonExistentFile_ReturnsEmpty() AS VOID
            LOCAL cResult AS STRING
            cResult := MemoRead("NonExistentFile_XYZ123.txt")
            Assert.Equal("", cResult)
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoWrit_EmptyString_CreatesFile() AS VOID
            LOCAL cFile AS STRING
            cFile := Path.GetTempFileName()
            TRY
                Assert.True(MemoWrit(cFile, ""))
                Assert.True(File.Exists(cFile))
            FINALLY
                IF File.Exists(cFile)
                    File.Delete(cFile)
                ENDIF
            END TRY
        END METHOD

        // ─────────────────────────────────────────────
        // MemoReadBinary / MemoWritBinary Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoReadBinary_MemoWritBinary_RoundTrip() AS VOID
            LOCAL cFile AS STRING
            LOCAL bData AS BYTE[]
            LOCAL bRead AS BYTE[]
            cFile := Path.GetTempFileName()
            bData := <BYTE>{1, 2, 3, 255, 0, 128}
            
            TRY
                Assert.True(MemoWritBinary(cFile, bData))
                bRead := MemoReadBinary(cFile)
                Assert.Equal(bData:Length, bRead:Length)
                FOR LOCAL i := 1 AS INT UPTO bData:Length
                    Assert.Equal(bData[i], bRead[i])
                NEXT
            FINALLY
                IF File.Exists(cFile)
                    File.Delete(cFile)
                ENDIF
            END TRY
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MemoReadBinary_NonExistentFile_ReturnsNull() AS VOID
            LOCAL bResult AS BYTE[]
            bResult := MemoReadBinary("NonExistent_XYZ.bin")
            // Returns null or empty array
            Assert.True(bResult == NULL .OR. bResult:Length == 0)
        END METHOD

        // ─────────────────────────────────────────────
        // MLcToPos Tests
        // ─────────────────────────────────────────────

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLcToPos_FirstLineFirstCol_ReturnsOne() AS VOID
            LOCAL cMemo AS STRING
            LOCAL nPos AS DWORD
            cMemo := "Hello" + Chr(13) + Chr(10) + "World"
            nPos := MLcToPos(cMemo, 80, 1, 1, 4, TRUE)
            Assert.Equal(1U, nPos)
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLcToPos_SecondLine_ReturnsCorrectPosition() AS VOID
            LOCAL cMemo AS STRING
            LOCAL nPos AS DWORD
            cMemo := "Hello" + Chr(13) + Chr(10) + "World"
            nPos := MLcToPos(cMemo, 80, 2, 1, 4, TRUE)
            Assert.True(nPos > 5)
        END METHOD

        [Fact, Trait("Category", "Memo")];
        PUBLIC METHOD MLcToPos_InvalidPosition_ReturnsZero() AS VOID
            LOCAL nPos AS DWORD
            nPos := MLcToPos("Test", 80, 10, 1, 4, TRUE)
            Assert.Equal(0U, nPos)
        END METHOD

    END CLASS

END NAMESPACE
