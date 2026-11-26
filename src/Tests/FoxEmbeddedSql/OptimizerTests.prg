// OptimizerTests.prg
USING System
USING System.Diagnostics
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.Parsers
USING XSharp.RDD.Enums
USING XSharp.Data
USING XSharp.Data.Query
USING XSharp.VFP

#include "foxcmd.xh"

FUNCTION TestQueryOptimizer() AS VOID
    ? "Testing Query Optimizer functionality..."

    CREATE CURSOR test_optimizer_table ;
        (Id N(5), Name Character(20), Age N(3), City Character(20))

    IF !Used()
        THROW Exception{"Failed: Failed to create optimizer test table"}
    ENDIF

    IF Verbose
        ? "Created test table for optimizer testing"
    ENDIF

    INSERT INTO test_optimizer_table VALUES (1, "John Doe", 30, "New York")
    INSERT INTO test_optimizer_table VALUES (2, "Jane Smith", 25, "Boston")
    INSERT INTO test_optimizer_table VALUES (3, "Bob Johnson", 35, "New York")
    INSERT INTO test_optimizer_table VALUES (4, "Alice Brown", 30, "Chicago")
    INSERT INTO test_optimizer_table VALUES (5, "Charlie Wilson", 40, "New York")

    // Test simple WHERE clause first
    IF Verbose
        ? "Testing simple WHERE clause..."
    ENDIF

    SELECT * FROM test_optimizer_table WHERE Age > 25
    VerifyTable(4, "Failed: Expected 4 records from simple WHERE", TRUE)

    // Test complex WHERE clause that would benefit from optimization
    IF Verbose
        ? "Testing complex WHERE clause with optimizer..."
    ENDIF

    SELECT * FROM test_optimizer_table WHERE Age > 25 .AND. City = "New York"
    VerifyTable(3, "Failed: Expected 3 records from complex WHERE", TRUE)

    // Test with index optimization - create an index on Age field
    IF Verbose
        ? "Testing index optimization..."
    ENDIF

    DbSelectArea("test_optimizer_table")

    // Create an index on the Age field
    LOCAL lIndexCreated AS LOGIC
    lIndexCreated := DbCreateOrder("AgeIdx",, "Age")

    IF !lIndexCreated
        THROW Exception{"Failed: Failed to create index on Age field"}
    ENDIF

    // Set focus to the new index
    OrdSetFocus("AgeIdx")

    // Test SELECT with WHERE clause that can use the index
    IF Verbose
        ? "SELECT * FROM test_optimizer_table WHERE Age > 25"
    ENDIF

    SELECT * FROM test_optimizer_table WHERE Age > 25
    VerifyTable(4, "Failed: Expected 4 records from indexed WHERE", TRUE)

    // Test with equality condition on indexed field
    IF Verbose
        ? "Testing equality condition with index..."
        ? "SELECT * FROM test_optimizer_table WHERE Age = 30"
    ENDIF

    SELECT * FROM test_optimizer_table WHERE Age = 30
    VerifyTable(2, "Failed: Expected 2 records from equality WHERE", TRUE)

    // Test with multiple conditions where one uses index
    IF Verbose
        ? "Testing multiple conditions with index..."
        ? "SELECT * FROM test_optimizer_table WHERE Age = 30 .AND. City = 'New York'"
    ENDIF

    SELECT * FROM test_optimizer_table WHERE Age = 30 .AND. City = "New York"
    VerifyTable(1, "Failed: Expected 1 record from indexed multi-condition WHERE", TRUE)

    // Clear the index and test without optimization for comparison
    IF Verbose
        ? "Testing without index (clearing index)..."
        ? "SELECT * FROM test_optimizer_table WHERE Age > 25 (without index)"
    ENDIF

    DbSetIndex(NULL)

    SELECT * FROM test_optimizer_table WHERE Age > 25
    VerifyTable(4, "Failed: Expected 4 records from simple WHERE (no index)", TRUE)

    USE IN test_optimizer_table

    RETURN

FUNCTION TestOptimizerBitmap() AS VOID
    CREATE CURSOR optimizer_bitmap_table (Age N(3), City C(20))
    INSERT INTO optimizer_bitmap_table VALUES (30, "New York")
    INSERT INTO optimizer_bitmap_table VALUES (25, "Boston")
    INSERT INTO optimizer_bitmap_table VALUES (35, "New York")
    INSERT INTO optimizer_bitmap_table VALUES (30, "Chicago")
    INSERT INTO optimizer_bitmap_table VALUES (40, "New York")

    CREATE CURSOR optimizer_bitmap_other (Age N(3))
    INSERT INTO optimizer_bitmap_other VALUES (30)

    LOCAL tableAliases := Dictionary<STRING, STRING>{StringComparer.OrdinalIgnoreCase} AS Dictionary<STRING, STRING>
    tableAliases["optimizer_bitmap_table"] := "optimizer_bitmap_table"
    tableAliases["optimizer_bitmap_other"] := "optimizer_bitmap_other"

    VAR andBitmap := QueryOptimizer.OptimizeQuery( ;
        ParseSqlExpressionForTest("optimizer_bitmap_table.Age > 25 AND optimizer_bitmap_table.City = 'New York'"), ;
        5, tableAliases)
    IF andBitmap:IsAllMatch() .OR. andBitmap:CountMatches() != 3
        THROW Exception{"Single-table AND bitmap should match exactly 3 rows"}
    ENDIF
    IF andBitmap:GetMatchState(1) != RecordBitmap.RecordMatchState.Match .OR. ;
       andBitmap:GetMatchState(2) != RecordBitmap.RecordMatchState.NoMatch .OR. ;
       andBitmap:GetMatchState(3) != RecordBitmap.RecordMatchState.Match .OR. ;
       andBitmap:GetMatchState(4) != RecordBitmap.RecordMatchState.NoMatch .OR. ;
       andBitmap:GetMatchState(5) != RecordBitmap.RecordMatchState.Match
        THROW Exception{"Single-table AND bitmap contains incorrect record numbers"}
    ENDIF

    VAR reversedRangeBitmap := QueryOptimizer.OptimizeQuery( ;
        ParseSqlExpressionForTest("25 < optimizer_bitmap_table.Age"), 5, tableAliases)
    IF reversedRangeBitmap:CountMatches() != 4 .OR. ;
       reversedRangeBitmap:GetMatchState(1) != RecordBitmap.RecordMatchState.Match .OR. ;
       reversedRangeBitmap:GetMatchState(2) != RecordBitmap.RecordMatchState.NoMatch .OR. ;
       reversedRangeBitmap:GetMatchState(3) != RecordBitmap.RecordMatchState.Match .OR. ;
       reversedRangeBitmap:GetMatchState(4) != RecordBitmap.RecordMatchState.Match .OR. ;
       reversedRangeBitmap:GetMatchState(5) != RecordBitmap.RecordMatchState.Match
        THROW Exception{"Constant-on-left range bitmap contains incorrect record numbers"}
    ENDIF

    VAR orBitmap := QueryOptimizer.OptimizeQuery( ;
        ParseSqlExpressionForTest("optimizer_bitmap_table.Age = 25 OR optimizer_bitmap_table.City = 'Chicago'"), ;
        5, tableAliases)
    IF orBitmap:IsAllMatch() .OR. orBitmap:CountMatches() != 2
        THROW Exception{"Single-table OR bitmap should match exactly 2 rows"}
    ENDIF
    IF orBitmap:GetMatchState(2) != RecordBitmap.RecordMatchState.Match .OR. ;
       orBitmap:GetMatchState(4) != RecordBitmap.RecordMatchState.Match
        THROW Exception{"Single-table OR bitmap omitted an expected record"}
    ENDIF

    VAR multiTableBitmap := QueryOptimizer.OptimizeQuery( ;
        ParseSqlExpressionForTest("optimizer_bitmap_table.Age = optimizer_bitmap_other.Age"), ;
        5, tableAliases)
    IF !multiTableBitmap:IsAllMatch()
        THROW Exception{"Multi-table predicate must fall back instead of combining unrelated record domains"}
    ENDIF

    LOCAL tableRecordCounts := Dictionary<STRING, LONG>{StringComparer.OrdinalIgnoreCase} AS Dictionary<STRING, LONG>
    tableRecordCounts["optimizer_bitmap_table"] := 5
    tableRecordCounts["optimizer_bitmap_other"] := 1
    VAR candidatePlan := QueryOptimizer.OptimizeMultiTableQuery( ;
        ParseSqlExpressionForTest("optimizer_bitmap_table.Age > 25 AND optimizer_bitmap_table.City = 'New York' AND optimizer_bitmap_other.Age = 30"), ;
        tableAliases, tableRecordCounts)
    IF candidatePlan:CandidateBitmaps:Count != 2
        THROW Exception{"Independent AND predicates should produce one candidate bitmap per table"}
    ENDIF
    IF candidatePlan:CandidateBitmaps["optimizer_bitmap_table"]:CountMatches() != 3 .OR. ;
       candidatePlan:CandidateBitmaps["optimizer_bitmap_other"]:CountMatches() != 1
        THROW Exception{"Per-table candidate bitmaps contain incorrect matches"}
    ENDIF
    IF candidatePlan:CandidateBitmaps["optimizer_bitmap_table"]:GetMatchState(1) != RecordBitmap.RecordMatchState.Match .OR. ;
       candidatePlan:CandidateBitmaps["optimizer_bitmap_table"]:GetMatchState(2) != RecordBitmap.RecordMatchState.NoMatch .OR. ;
       candidatePlan:CandidateBitmaps["optimizer_bitmap_table"]:GetMatchState(3) != RecordBitmap.RecordMatchState.Match .OR. ;
       candidatePlan:CandidateBitmaps["optimizer_bitmap_table"]:GetMatchState(4) != RecordBitmap.RecordMatchState.NoMatch .OR. ;
       candidatePlan:CandidateBitmaps["optimizer_bitmap_table"]:GetMatchState(5) != RecordBitmap.RecordMatchState.Match .OR. ;
       candidatePlan:CandidateBitmaps["optimizer_bitmap_other"]:GetMatchState(1) != RecordBitmap.RecordMatchState.Match
        THROW Exception{"Per-table candidate bitmap has incorrect physical record matches"}
    ENDIF

    VAR orCandidatePlan := QueryOptimizer.OptimizeMultiTableQuery( ;
        ParseSqlExpressionForTest("optimizer_bitmap_table.Age = 30 OR optimizer_bitmap_other.Age = 30"), ;
        tableAliases, tableRecordCounts)
    IF orCandidatePlan:CandidateBitmaps:Count != 0
        THROW Exception{"Cross-table OR must remain on the full WHERE evaluation path"}
    ENDIF

    DbCloseArea("optimizer_bitmap_other")
    DbCloseArea("optimizer_bitmap_table")
    RETURN


FUNCTION TestMultiTableOptimizer() AS VOID STRICT
    ? ""
    ? "=== Testing Multi-Table Query Optimizer ==="
    ? ""

    CREATE CURSOR TableA (ID N(5), Value C(10))
    CREATE CURSOR TableB (SKU N(5), Price N(8,2))

    IF Verbose
        ? "Created test tables:"
    ENDIF

    INSERT INTO TableA VALUES (1, "Item A1")
    INSERT INTO TableA VALUES (2, "Item A2")
    INSERT INTO TableA VALUES (3, "Item A3")

    INSERT INTO TableB VALUES (103, 9.99)
    INSERT INTO TableB VALUES (105, 19.99)
    INSERT INTO TableB VALUES (403, 29.99)
    INSERT INTO TableB VALUES (1, 39.99)
    INSERT INTO TableB VALUES (2, 49.99)

    IF Verbose
        ? "TableA:", DbSelectArea("TableA"), "records:", RecCount()
        ? "TableB:", DbSelectArea("TableB"), "records:", RecCount()
    ENDIF

    // Test 1: Single-table WHERE (baseline)
    Title("Test 1: Single-table WHERE")
    SELECT * FROM TableA WHERE ID = 2
    VerifyTable(1, "Test 1 failed", FALSE)

    // Test 2: AND of independent single-table conditions (KEY TEST FOR PHASE 1)
    Title("Test 2: AND of independent single-table conditions")
    SELECT * FROM TableA, TableB WHERE TableA.ID = 2 .AND. TableB.SKU = 103
    VerifyTable(1, "Test 2 failed", FALSE)

    // Test 3: OR of independent single-table conditions
    Title("Test 3: OR of independent single-table conditions")
    // Explanation: Cross join creates 15 rows. TableA.ID=1 matches 5 rows, TableB.SKU=105 matches 3 rows, overlap is 1 row
    // Total matching = 5 + 3 - 1 (overlap) = 7 unique rows
    SELECT * FROM TableA, TableB WHERE TableA.ID = 1 .OR. TableB.SKU = 105
    VerifyTable(7, "Test 3 failed", FALSE)

    // Test 4: Multiple AND conditions across tables
    Title("Test 4: Multiple independent single-table conditions")
    SELECT * FROM TableA, TableB WHERE TableA.ID = 2 .AND. TableA.Value = "Item A2" .AND. TableB.SKU = 103
    VerifyTable(1, "Test 4 failed", FALSE)

    // Test 5: Cross-table comparison (not optimizable)
    Title("Test 5: Cross-table comparison")
    SELECT * FROM TableA, TableB WHERE TableA.ID = INT(TableB.SKU / 100)
    VerifyTable(2, "Test 5 failed", FALSE)

    // Test 6: Complex nested conditions
    Title("Test 6: Complex nested conditions")
    SELECT * FROM TableA, TableB WHERE (TableA.ID = 1 .OR. TableA.ID = 2) .AND. TableB.SKU >= 103
    VerifyTable(6, "Test 6 failed", FALSE)

    // Test 7: Range operators with independent tables
    Title("Test 7: Range operators")
    SELECT * FROM TableA, TableB WHERE TableA.ID > 1 .AND. TableB.Price < 25
    VerifyTable(4, "Test 7 failed", FALSE)

    Title("All tests completed successfully")

    DbCloseAll()
RETURN

FUNCTION TestSparseOptimizationBenchmark() AS VOID
    CREATE CURSOR sparse_left (Id N(5), MatchFlag N(1))
    CREATE CURSOR sparse_right (Id N(5), MatchFlag N(1))

    FOR LOCAL rowIndex := 1 AS INT TO 100
        INSERT INTO sparse_left VALUES (rowIndex, IIF(rowIndex <= 2, 1, 0))
        INSERT INTO sparse_right VALUES (rowIndex, IIF(rowIndex <= 2, 1, 0))
    NEXT

    LOCAL query := "SELECT COUNT(*) AS PairCount FROM sparse_left a CROSS JOIN sparse_right b WHERE a.MatchFlag = 1 AND b.MatchFlag = 1 INTO CURSOR SparseResult" AS STRING
    LOCAL switchName := "XSharp.VFP.EmbeddedSql.DisableOptimizations" AS STRING
    LOCAL previousSwitch := FALSE AS LOGIC
    LOCAL switchWasSet := AppContext.TryGetSwitch(switchName, OUT previousSwitch) AS LOGIC
    LOCAL optimizedMilliseconds AS INT64
    LOCAL baselineMilliseconds AS INT64
    LOCAL timer := Stopwatch{} AS Stopwatch

    TRY
        AppContext.SetSwitch(switchName, FALSE)
        timer:Restart()
        __SqlSelect(query)
        timer:Stop()
        optimizedMilliseconds := timer:ElapsedMilliseconds
        GO TOP
        IF (LONG)FieldGet(1) != 4
            THROW Exception{"Optimized sparse join returned an incorrect count"}
        ENDIF

        AppContext.SetSwitch(switchName, TRUE)
        timer:Restart()
        __SqlSelect(query)
        timer:Stop()
        baselineMilliseconds := timer:ElapsedMilliseconds
        GO TOP
        IF (LONG)FieldGet(1) != 4
            THROW Exception{"Baseline sparse join returned an incorrect count"}
        ENDIF

        IF Verbose
            ? "Sparse join milliseconds (optimized, baseline):", optimizedMilliseconds, baselineMilliseconds
        ENDIF
    FINALLY
        AppContext.SetSwitch(switchName, IIF(switchWasSet, previousSwitch, FALSE))
        IF Used("SparseResult")
            USE IN SparseResult
        ENDIF
        IF Used("sparse_left")
            USE IN sparse_left
        ENDIF
        IF Used("sparse_right")
            USE IN sparse_right
        ENDIF
    END TRY
    RETURN

