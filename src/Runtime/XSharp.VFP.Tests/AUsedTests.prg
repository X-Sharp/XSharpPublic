// AUsedTests.prg
// Created by    : irwin
// Creation Date : 2/21/2026 12:36:46 PM
// Created for   :
// WorkStation   : IRWINPC


USING System
USING XUnit
USING XSharp.RDD
// USING XSharp.VFP
// USING XSharp.RDD
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

            // Usamos sintaxis LOCAL ARRAY para evitar líos con DIMENSION
            LOCAL ARRAY aTest[1]

            // Llamamos a la función
            VAR nCount := AUsed(aTest)

            // --- Validaciones ---

            Assert.Equal(2, (INT)nCount)

            // Verificamos contenido (LIFO)
            Assert.Equal("ALIAS_B", aTest[1, 1])
            Assert.Equal("ALIAS_A", aTest[2, 1])

            // Limpieza
            CoreDb.CloseAll()
            System.IO.File.Delete("TEST_A.DBF")
            System.IO.File.Delete("TEST_B.DBF")
        RETURN

	END CLASS
END NAMESPACE // XSharp.VFP.Tests
