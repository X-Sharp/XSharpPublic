// AUsedTests.prg
// Created by    : irwin
// Creation Date : 2/21/2026 12:36:46 PM
// Created for   :
// WorkStation   : IRWINPC


USING System
USING XUnit
USING XSharp.RDD
USING XSharp.RDD.Support

BEGIN NAMESPACE XSharp.VFP.Tests

	CLASS AUsedTests

        CONSTRUCTOR()
            CoreDb.CloseAll()
        RETURN

        METHOD Dispose() AS VOID
            CoreDb.CloseAll()
            TRY; System.IO.File.Delete("TEST_A.DBF"); END TRY
            TRY; System.IO.File.Delete("TEST_B.DBF"); END TRY
            RETURN

        [Fact, Trait("Category", "AUsed")];
        METHOD BasicAUsedTest() AS VOID
            SELECT 0
            CREATE TABLE TEST_A (ID N(10, 0))
            USE IN TEST_A

            SELECT 0
            CREATE TABLE TEST_B (ID N(10, 0))
            USE IN TEST_B

            SELECT 0
            USE TEST_A ALIAS ALIAS_A

            SELECT 0
            USE TEST_B ALIAS ALIAS_B

            LOCAL ARRAY aTest[1]

            VAR nCount := AUsed(aTest)
            Assert.Equal(2, (INT)nCount)

            Assert.Equal("ALIAS_B", aTest[1, 1])
            Assert.Equal("ALIAS_A", aTest[2, 1])

            CoreDb.CloseAll()
            System.IO.File.Delete("TEST_A.DBF")
            System.IO.File.Delete("TEST_B.DBF")
        RETURN

        [Fact, Trait("Category", "AUsed")];
        METHOD EmptySessionTest() AS VOID
            CoreDb.CloseAll()
            LOCAL ARRAY aTest[1]
            aTest[1] := "Original"

            VAR nCount := AUsed(aTest)

            Assert.Equal(0, (INT)nCount)
            Assert.Equal("Original", aTest[1])
        RETURN

	END CLASS
END NAMESPACE // XSharp.VFP.Tests
