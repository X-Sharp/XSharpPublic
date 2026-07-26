//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING System.Text
USING XUnit

// The expected values in these tests were captured from Visual FoxPro 9
// running the same cursors through CURSORTOXML().
BEGIN NAMESPACE XSharp.VFP.Tests

    CLASS CursorToXmlTests

        STATIC CONSTRUCTOR
            XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
        END CONSTRUCTOR

        #region helpers

        PRIVATE METHOD CreateTestCursor() AS VOID
            CREATE CURSOR curxml (id I, nombre C(10), precio N(8,2), fecha D, activo L)
            INSERT INTO curxml VALUES (1, "uno",  10.50, {^2024-01-15}, .T.)
            INSERT INTO curxml VALUES (2, "dos",  20.75, {^2024-02-20}, .F.)
            INSERT INTO curxml VALUES (3, "tres", 30.00, {^2024-03-25}, .T.)
            GO TOP
        END METHOD

        PRIVATE METHOD CreateTypesCursor() AS VOID
            LOCAL tStamp, tEmpty, dEmpty AS USUAL
            // A datetime literal containing a space cannot be written inside
            // INSERT INTO, so the values are prepared first.
            tStamp := CToT("2024-01-15 10:30:45")
            tEmpty := CToT("")
            dEmpty := SToD("")
            CREATE CURSOR curtipos (cchar C(8), nnum N(10,3), ycur Y, ddate D, tstamp T, llog L)
            INSERT INTO curtipos VALUES ("abc", 12.5, 99.95, {^2024-01-15}, tStamp, .T.)
            INSERT INTO curtipos VALUES ("",    0,    0,     dEmpty,        tEmpty, .F.)
            GO TOP
        END METHOD

        PRIVATE METHOD TempFile() AS STRING
            RETURN Path.Combine(Path.GetTempPath(), "CursorToXml_" + Guid.NewGuid():ToString("N") + ".xml")
        END METHOD

        // Writes the given alias to a temporary file and returns the XML
        PRIVATE METHOD ToXml(cAlias AS STRING) AS STRING
            RETURN SELF:ToXml(cAlias, 512, 0, "")
        END METHOD

        PRIVATE METHOD ToXml(cAlias AS STRING, nFlags AS LONG, nRecords AS LONG, cSchema AS STRING) AS STRING
            LOCAL cXml AS STRING
            VAR cFile := SELF:TempFile()
            CursorToXml(cAlias, cFile, 1, nFlags, nRecords, cSchema)
            cXml := File.ReadAllText(cFile)
            File.Delete(cFile)
            RETURN cXml
        END METHOD

        PRIVATE METHOD CountOf(cText AS STRING, cNeedle AS STRING) AS LONG
            LOCAL nCount := 0 AS LONG
            LOCAL nPos   := 0 AS LONG
            DO WHILE TRUE
                nPos := cText:IndexOf(cNeedle, nPos, StringComparison.Ordinal)
                IF nPos < 0
                    EXIT
                ENDIF
                nCount++
                nPos += cNeedle:Length
            ENDDO
            RETURN nCount
        END METHOD

        #endregion

        #region structure

        [Fact, Trait("Category", "CursorToXml")];
        METHOD RootIsVfpDataAndRowElementIsLowercaseAlias AS VOID
            SELF:CreateTestCursor()
            VAR cXml := SELF:ToXml("curxml")
            Assert.Contains("<VFPData>", cXml)
            Assert.Contains("</VFPData>", cXml)
            // VFP writes the alias in lower case, one element per record
            Assert.Equal(3, SELF:CountOf(cXml, "<curxml>"))
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD FieldNamesAreLowercase AS VOID
            SELF:CreateTestCursor()
            VAR cXml := SELF:ToXml("curxml")
            Assert.Contains("<nombre>", cXml)
            Assert.DoesNotContain("<NOMBRE>", cXml)
        END METHOD

        #endregion

        #region value formatting

        [Fact, Trait("Category", "CursorToXml")];
        METHOD NumericKeepsTheFieldScale AS VOID
            SELF:CreateTestCursor()
            VAR cXml := SELF:ToXml("curxml")
            // N(8,2): VFP keeps the trailing zeros of the declared scale
            Assert.Contains("<precio>10.50</precio>", cXml)
            Assert.Contains("<precio>30.00</precio>", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD CharacterIsTrimmed AS VOID
            SELF:CreateTestCursor()
            VAR cXml := SELF:ToXml("curxml")
            Assert.Contains("<nombre>uno</nombre>", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD LogicalIsWrittenInLowercase AS VOID
            SELF:CreateTestCursor()
            VAR cXml := SELF:ToXml("curxml")
            Assert.Contains("<activo>true</activo>", cXml)
            Assert.Contains("<activo>false</activo>", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD DateUsesIsoFormat AS VOID
            SELF:CreateTestCursor()
            VAR cXml := SELF:ToXml("curxml")
            Assert.Contains("<fecha>2024-01-15</fecha>", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD DateTimeUsesIsoFormat AS VOID
            SELF:CreateTypesCursor()
            VAR cXml := SELF:ToXml("curtipos")
            Assert.Contains("<tstamp>2024-01-15T10:30:45</tstamp>", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD CurrencyAlwaysHasFourDecimals AS VOID
            SELF:CreateTypesCursor()
            VAR cXml := SELF:ToXml("curtipos")
            Assert.Contains("<ycur>99.9500</ycur>", cXml)
            Assert.Contains("<ycur>0.0000</ycur>", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD EmptyValuesProduceAnEmptyElement AS VOID
            SELF:CreateTypesCursor()
            VAR cXml := SELF:ToXml("curtipos")
            // empty, but not NULL: the element is present and carries no text
            Assert.True(cXml:Contains("<ddate></ddate>") .OR. cXml:Contains("<ddate />"))
            Assert.True(cXml:Contains("<cchar></cchar>") .OR. cXml:Contains("<cchar />"))
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD NullFieldsAreOmitted AS VOID
            LOCAL cXml AS STRING
            CREATE CURSOR curnul (id I, nombre C(10) NULL, fecha D NULL)
            INSERT INTO curnul VALUES (1, "ok", {^2024-01-15})
            INSERT INTO curnul VALUES (2, NULL, NULL)
            GO TOP
            cXml := SELF:ToXml("curnul")
            // VFP leaves the element out completely when the field is NULL
            Assert.Equal(1, SELF:CountOf(cXml, "<nombre>"))
            Assert.Equal(1, SELF:CountOf(cXml, "<fecha>"))
            Assert.Equal(2, SELF:CountOf(cXml, "<id>"))
        END METHOD

        #endregion

        #region record selection and cursor position

        [Fact, Trait("Category", "CursorToXml")];
        METHOD NRecordsLimitsTheNumberOfRows AS VOID
            SELF:CreateTestCursor()
            VAR cXml := SELF:ToXml("curxml", 512, 2, "")
            Assert.Equal(2, SELF:CountOf(cXml, "<curxml>"))
            Assert.DoesNotContain("<nombre>tres</nombre>", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD WritingEveryRecordLeavesThePointerAtEof AS VOID
            SELF:CreateTestCursor()
            SELF:ToXml("curxml")
            // VFP does not restore the record pointer
            Assert.True(EOF())
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD NRecordsLeavesThePointerOnTheLastRecordWritten AS VOID
            SELF:CreateTestCursor()
            SELF:ToXml("curxml", 512, 2, "")
            Assert.False(EOF())
            Assert.Equal((DWORD) 2, RECNO())
        END METHOD

        #endregion

        #region output targets

        [Fact, Trait("Category", "CursorToXml")];
        METHOD FileOutputReturnsTheNumberOfBytesWritten AS VOID
            LOCAL nBytes AS LONG
            SELF:CreateTestCursor()
            VAR cFile := SELF:TempFile()
            nBytes := (LONG) CursorToXml("curxml", cFile, 1, 512, 0, "")
            Assert.True(File.Exists(cFile))
            Assert.Equal(nBytes, (LONG) FileInfo{cFile}:Length)
            File.Delete(cFile)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD MemVarOutputCreatesTheVariable AS VOID
            PRIVATE cXmlOut
            LOCAL nBytes AS LONG
            SELF:CreateTestCursor()
            // without the file flag cOutput names a memory variable
            nBytes := (LONG) CursorToXml("curxml", "cXmlOut", 1, 0, 0, "")
            Assert.True(nBytes > 0)
            Assert.Contains("<VFPData>", cXmlOut)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD CurrentWorkAreaIsUsedWhenNoAreaIsGiven AS VOID
            LOCAL cXml AS STRING
            SELF:CreateTestCursor()
            VAR cFile := SELF:TempFile()
            CursorToXml(0, cFile, 1, 512, 0, "")
            cXml := File.ReadAllText(cFile)
            File.Delete(cFile)
            Assert.Equal(3, SELF:CountOf(cXml, "<curxml>"))
        END METHOD

        #endregion

        #region flags and schema

        [Fact, Trait("Category", "CursorToXml")];
        METHOD ContinuousFlagRemovesTheLineBreaks AS VOID
            SELF:CreateTestCursor()
            // 512 = to file, 1 = one continuous string
            VAR cXml := SELF:ToXml("curxml", 513, 0, "")
            Assert.DoesNotContain(e"\n  <curxml>", cXml)
            Assert.Contains("<VFPData><curxml>", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD NoSchemaIsWrittenByDefault AS VOID
            SELF:CreateTestCursor()
            VAR cXml := SELF:ToXml("curxml")
            Assert.DoesNotContain("xs:schema", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD InlineSchemaIsEmittedWhenSchemaNameIsOne AS VOID
            SELF:CreateTestCursor()
            VAR cXml := SELF:ToXml("curxml", 512, 0, "1")
            Assert.Contains("schema", cXml)
        END METHOD

        [Fact, Trait("Category", "CursorToXml")];
        METHOD ExternalSchemaFileIsCreated AS VOID
            SELF:CreateTestCursor()
            VAR cXsd  := Path.Combine(Path.GetTempPath(), "CursorToXml_" + Guid.NewGuid():ToString("N") + ".xsd")
            VAR cFile := SELF:TempFile()
            CursorToXml("curxml", cFile, 1, 512, 0, cXsd)
            Assert.True(File.Exists(cXsd))
            File.Delete(cXsd)
            File.Delete(cFile)
        END METHOD

        #endregion

    END CLASS

END NAMESPACE
