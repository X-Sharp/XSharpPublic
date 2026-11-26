// SelectTests.prg
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

FUNCTION SqlOptimizationCount(name AS STRING) AS INT
    VAR diagnosticType := System.Reflection.Assembly.Load("XSharp.VFP"):GetType("XSharp.VFP.SqlOptimizationDiagnostics", TRUE)
    VAR field := diagnosticType:GetField(name, System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.NonPublic)
    RETURN (INT)field:GetValue(NULL)

FUNCTION VerifySqlOptimization(name AS STRING, previous AS INT) AS VOID
    LOCAL disabled := FALSE AS LOGIC
    AppContext.TryGetSwitch("XSharp.VFP.EmbeddedSql.DisableOptimizations", OUT disabled)
    IF (SqlOptimizationCount(name) > previous) == disabled
        THROW Exception{"Unexpected optimizer selection: " + name}
    ENDIF
    RETURN

FUNCTION TestSelectFunctionality() AS VOID
    ? "Testing SELECT functionality..."

    // Create a test table
    CREATE CURSOR test_select_table ;
        (ID N(5), Name C(20), Age N(3))

    IF !Used()
        THROW Exception{"Failed: Failed to create test table for SELECT testing"}
    ENDIF

    IF Verbose
        ? "Created test table for SELECT testing"
    ENDIF

    // Insert some test data
    INSERT INTO test_select_table VALUES (1, "John Doe", 30)
    INSERT INTO test_select_table VALUES (2, "Jane Smith", 25)
    INSERT INTO test_select_table VALUES (3, "Bob Johnson", 35)
    INSERT INTO test_select_table VALUES (4, "Alice Brown", 30)  // Duplicate age for DISTINCT test

    IF Verbose
        ? "Inserted test data"
    ENDIF

// Test basic SELECT
    IF Verbose
        ? "Attempting basic SELECT..."
    ENDIF

    SELECT * FROM test_select_table
    VerifyTable(4, "Failed: Expected 4 records from SELECT *", TRUE, ;
        { {1, "John Doe", 30}, ;
          {2, "Jane Smith", 25}, ;
          {3, "Bob Johnson", 35}, ;
          {4, "Alice Brown", 30} } )

// Test SELECT with WHERE
    IF Verbose
        ? "Attempting SELECT with WHERE..."
    ENDIF

    SELECT * FROM test_select_table WHERE Age > 25
    VerifyTable(3, "Failed: Expected 3 records from SELECT with WHERE", TRUE, ;
            { {1, "John Doe", 30}, ;
              {3, "Bob Johnson", 35}, ;
              {4, "Alice Brown", 30} } )

// Test SELECT with specific columns
    IF Verbose
        ? "Attempting SELECT with specific columns..."
    ENDIF

    SELECT Name, Age FROM test_select_table WHERE Age > 25
    VerifyTable(3, "Failed: Expected 3 records from SELECT specific columns", TRUE, ;
            { {"John Doe", 30}, ;
              {"Bob Johnson", 35}, ;
              {"Alice Brown", 30} } )

// Test SELECT with DISTINCT
    IF Verbose
        ? "Attempting SELECT with DISTINCT..."
    ENDIF

    SELECT DISTINCT Age FROM test_select_table
    VerifyTable(3, "Failed: Expected 3 distinct ages", TRUE, ;
            { {30}, ;
              {25}, ;
              {35} } )

// Test SELECT with TOP
    IF Verbose
        ? "Attempting SELECT with TOP..."
    ENDIF

    SELECT TOP 2 * FROM test_select_table
    VerifyTable(2, "Failed: Expected 2 records from SELECT TOP 2", TRUE, ;
            { {1, "John Doe", 30}, ;
              {2, "Jane Smith", 25} } )

    // ORDER BY supports multiple keys and applies TOP after sorting.
    SELECT Id, Age FROM test_select_table ORDER BY Age DESC, Id ASC
    VerifyTable(4, "Failed: Expected all records from multi-key ORDER BY", TRUE, ;
            { {3, 35}, ;
              {1, 30}, ;
              {4, 30}, ;
              {2, 25} } )

    SELECT TOP 2 Id, Age FROM test_select_table ORDER BY Age DESC, Id ASC
    VerifyTable(2, "Failed: TOP should be applied after ORDER BY", TRUE, ;
            { {3, 35}, ;
              {1, 30} } )

    SELECT Id, Age FROM test_select_table ORDER BY Age DESC, Id ASC INTO CURSOR OrderedCursor
    VerifyTable(4, "Failed: Ordered INTO CURSOR should preserve sorted rows", FALSE, ;
            { {3, 35}, ;
              {1, 30}, ;
              {4, 30}, ;
              {2, 25} } )
    USE IN OrderedCursor

    SELECT DISTINCT Age FROM test_select_table ORDER BY Age DESC
    VerifyTable(3, "Failed: DISTINCT rows should be ordered", TRUE, ;
            { {35}, ;
              {30}, ;
              {25} } )

    SELECT Id FROM test_select_table ORDER BY Age + Id DESC
    VerifyTable(4, "Failed: ORDER BY should accept unprojected expressions", TRUE, ;
            { {3}, ;
              {4}, ;
              {1}, ;
              {2} } )

    // GROUP BY, aggregate execution, HAVING aliases, and aggregate schema.
    SELECT Age, COUNT(*) AS OrderCount, SUM(Id) AS TotalId ;
        FROM test_select_table GROUP BY Age HAVING OrderCount >= 2 ORDER BY TotalId DESC
    VerifyTable(1, "Failed: GROUP BY/HAVING should return the matching age group", FALSE, ;
        { {30, 2, 5} } )
    VAR groupedStruct := DbStruct()
    IF FCount() != 3 .OR. groupedStruct[2][1]:ToString():Trim():ToUpperInvariant() != "ORDERCOUNT" .OR. ;
       groupedStruct[3][1]:ToString():Trim():ToUpperInvariant() != "TOTALID" .OR. ;
       groupedStruct[2][2]:ToString():Trim():ToUpperInvariant() != "N" .OR. ;
       groupedStruct[3][2]:ToString():Trim():ToUpperInvariant() != "N"
        THROW Exception{"Aggregate aliases and numeric DBF field types should be preserved"}
    ENDIF
    USE IN QUERYRESULT

    SELECT Age, SUM(Id) / COUNT(*) + Age AS Adjusted ;
        FROM test_select_table GROUP BY Age HAVING COUNT(*) > 1 ORDER BY Adjusted
    VerifyTable(1, "Failed: composed aggregate expressions should use grouped values", TRUE, ;
        { {30, 32.5} } )

    SELECT AVG(Id), MIN(Name), MAX(Name), CNT(Name) FROM test_select_table
    VerifyTable(1, "Failed: AVG/MIN/MAX/CNT should calculate over the global group", FALSE, ;
        { {2.5, "Alice Brown", "John Doe", 4} } )
    VAR aggregateStruct := DbStruct()
    IF FCount() != 4 .OR. aggregateStruct[1][2] != "N" .OR. ;
       aggregateStruct[2][2] != "C" .OR. aggregateStruct[3][2] != "C" .OR. aggregateStruct[4][2] != "N"
        THROW Exception{"Implicit aggregate columns should retain aggregate result types"}
    ENDIF
    USE IN QUERYRESULT

    LOCAL rejectedAggregateQuery := FALSE AS LOGIC
    TRY
        SELECT SUM(COUNT(Id)) FROM test_select_table
    CATCH aggregateError AS Exception
        rejectedAggregateQuery := TRUE
    END TRY
    IF !rejectedAggregateQuery
        THROW Exception{"Nested aggregate functions should be rejected"}
    ENDIF

    rejectedAggregateQuery := FALSE
    TRY
        SELECT Name, COUNT(*) FROM test_select_table GROUP BY Age
    CATCH groupingError AS Exception
        rejectedAggregateQuery := TRUE
    END TRY
    IF !rejectedAggregateQuery
        THROW Exception{"Ungrouped projected fields should be rejected"}
    ENDIF

    SELECT Age AS GroupAge, COUNT(*) AS RowCount FROM test_select_table GROUP BY GroupAge ORDER BY GroupAge
    VerifyTable(3, "Failed: GROUP BY should resolve a SELECT-list alias", TRUE, ;
        { {25, 1}, {30, 2}, {35, 1} } )

    SELECT Age, COUNT(*) AS RowCount FROM test_select_table GROUP BY 1 ORDER BY Age
    VerifyTable(3, "Failed: GROUP BY should resolve a SELECT-list ordinal", TRUE, ;
        { {25, 1}, {30, 2}, {35, 1} } )

    SELECT Name FROM test_select_table HAVING Id > 2 ORDER BY Id
    VerifyTable(2, "Failed: HAVING without grouping should filter source rows", TRUE, ;
        { {"Bob Johnson"}, {"Alice Brown"} } )

    SELECT Age, Name, COUNT(*) AS RowCount FROM test_select_table GROUP BY Age, Name ORDER BY Age, Name
    VerifyTable(4, "Failed: multiple GROUP BY keys should form distinct groups", TRUE, ;
        { {25, "Jane Smith", 1}, ;
          {30, "Alice Brown", 1}, ;
          {30, "John Doe", 1}, ;
          {35, "Bob Johnson", 1} } )

    SELECT COUNT(*), SUM(Id) FROM test_select_table
    VerifyTable(1, "Failed: global aggregate should return one row", TRUE, { {4, 10} } )

    CREATE CURSOR empty_aggregate_source (Id N(5))
    SELECT COUNT(*), SUM(Id) FROM empty_aggregate_source
    IF RecCount() != 1
        THROW Exception{"Failed: empty global aggregate should return one result row"}
    ENDIF
    GO TOP
    IF !IsNull(FieldGet(1)) .OR. !IsNull(FieldGet(2))
        THROW Exception{"Failed: empty global aggregate fields should be NULL"}
    ENDIF
    DbCloseArea()
    USE IN empty_aggregate_source

    // Test cross join with multiple tables
    IF Verbose
        ? "Creating additional tables for cross join test..."
    ENDIF

    CREATE CURSOR test_table_a (Id N(5), Name C(20))
    CREATE CURSOR test_table_b (Id N(5), Desc C(30))

    IF !Used("test_table_a") .OR. !Used("test_table_b")
        THROW Exception{"Failed: Failed to create additional test tables"}
    ENDIF

    IF Verbose
        ? "Created additional test tables"
    ENDIF

    // Insert test data for cross join
    INSERT INTO test_table_a VALUES (1, "Item A1")
    INSERT INTO test_table_a VALUES (2, "Item A2")

    INSERT INTO test_table_b VALUES (1, "Description B1")
    INSERT INTO test_table_b VALUES (2, "Description B2")
    INSERT INTO test_table_b VALUES (3, "Description B3")

    IF Verbose
        ? "Inserted test data for cross join"
    ENDIF

    SELECT a.Id, COUNT(*) AS MatchCount FROM test_table_a a ;
        INNER JOIN test_table_b b ON a.Id = b.Id GROUP BY 1 ORDER BY a.Id
    VerifyTable(2, "Failed: aggregate execution should consume joined rows", TRUE, ;
        { {1, 1}, {2, 1} } )

// Test cross join (Cartesian product)
    IF Verbose
        ? "Attempting cross join (SELECT from multiple tables)..."
    ENDIF

    SELECT test_table_a.Name, test_table_b.Desc FROM test_table_a, test_table_b
    VerifyTable(6, "Failed: Expected 6 records from cross join", TRUE, ;
            { {"Item A1", "Description B1"}, ;
              {"Item A1", "Description B2"}, ;
              {"Item A1", "Description B3"}, ;
              {"Item A2", "Description B1"}, ;
              {"Item A2", "Description B2"}, ;
              {"Item A2", "Description B3"} } )

    // Close the additional test tables
    USE IN test_table_a
    USE IN test_table_b

    // Test field name resolution and aliasing
    IF Verbose
        ? "Creating table for field name resolution test..."
    ENDIF

    CREATE CURSOR test_field_resolution (Id N(5), Name C(20), Value N(10,2))

    IF !Used("test_field_resolution")
        THROW Exception{"Failed: Failed to create test table for field name resolution"}
    ENDIF

    IF Verbose
        ? "Created test table for field name resolution"
    ENDIF

    // Insert test data
    INSERT INTO test_field_resolution VALUES (1, "Test Item", 100.50)
    INSERT INTO test_field_resolution VALUES (2, "Another Item", 200.75)

    IF Verbose
        ? "Inserted test data for field name resolution"
    ENDIF

// Test field aliasing with AS
    IF Verbose
        ? "Testing field aliasing with AS..."
    ENDIF

    SELECT Id AS Identifier, Name AS ItemName, Value AS Amount FROM test_field_resolution
    VerifyTable(2, "Failed: Expected 2 records from field aliasing", TRUE, ;
            { {1, "Test Item", 100.50}, ;
              {2, "Another Item", 200.75} } )

    // Test expression aliasing with AS
    IF Verbose
        ? "Testing expression aliasing with AS..."
    ENDIF

    SELECT Id, Name, Value, Value * 1.1 AS TaxedValue FROM test_field_resolution
    VerifyTable(2, "Failed: Expected 2 records from expression aliasing", TRUE)

// Test field name conflict resolution
    IF Verbose
        ? "Creating additional table for conflict resolution test..."
    ENDIF

    CREATE CURSOR test_conflict (Id N(5), Name C(20), Desc C(30))

    IF !Used("test_conflict")
        THROW Exception{"Failed: Failed to create test table for conflict resolution"}
    ENDIF

    INSERT INTO test_conflict VALUES (1, "Conflict Name", "Description 1")
    INSERT INTO test_conflict VALUES (2, "Another Name", "Description 2")

    IF Verbose
        ? "Testing field name conflict resolution..."
    ENDIF

    SELECT test_field_resolution.Name, test_conflict.Name ;
        FROM test_field_resolution, test_conflict ;
        WHERE test_field_resolution.Id = 1 AND test_conflict.Id = 1
    VerifyTable(1, "Failed: Expected 1 record from conflict resolution", TRUE, { {"Test Item", "Conflict Name"} } )

    USE IN test_conflict

    // Close the test table
    USE IN test_field_resolution

    // Test INTO CURSOR functionality
    IF Verbose
        ? "Testing INTO CURSOR functionality..."
    ENDIF

    CREATE CURSOR test_cursor_target (Id N(5), Name C(20), Value N(10,2))

    IF !Used("test_cursor_target")
        THROW Exception{"Failed: Failed to create test table for INTO CURSOR test"}
    ENDIF

    IF Verbose
        ? "Created test table for INTO CURSOR test"
    ENDIF

    // Insert test data
    INSERT INTO test_cursor_target VALUES (1, "Test Item 1", 100.50)
    INSERT INTO test_cursor_target VALUES (2, "Test Item 2", 200.75)
    INSERT INTO test_cursor_target VALUES (3, "Test Item 3", 300.25)

    IF Verbose
        ? "Inserted test data for INTO CURSOR test"
    ENDIF

// Test SELECT INTO CURSOR
    IF Verbose
        ? "Testing SELECT INTO CURSOR..."
    ENDIF

    SELECT * FROM test_cursor_target INTO CURSOR MyTestCursor
    VerifyTable(3, "Failed: Expected 3 records in MyTestCursor", FALSE, ;
            { {1, "Test Item 1", 100.50}, ;
              {2, "Test Item 2", 200.75}, ;
              {3, "Test Item 3", 300.25} } )

    // Verify the cursor exists and has the correct data
    IF !Used("MyTestCursor")
        THROW Exception{"Failed: MyTestCursor was not created!"}
    ENDIF

    SELECT MyTestCursor
    GO TOP
    LOCAL verifiedCount AS DWORD
    verifiedCount := 0
    DO WHILE !EOF()
        verifiedCount++
        SKIP
    ENDDO

    VerifyTable(verifiedCount, "Failed: Verified record count mismatch in MyTestCursor", TRUE)

// Test SELECT with field aliasing INTO CURSOR
    IF Verbose
        ? "Testing SELECT with field aliasing INTO CURSOR..."
    ENDIF

    SELECT Id AS ItemId, Name AS ItemName, Value AS ItemValue FROM test_cursor_target INTO CURSOR AliasedCursor
    VerifyTable(3, "Failed: Expected 3 records in AliasedCursor", FALSE, ;
            { {1, "Test Item 1", 100.50}, ;
              {2, "Test Item 2", 200.75}, ;
              {3, "Test Item 3", 300.25} } )

    // Verify the aliased cursor exists and has the correct data
    IF !Used("AliasedCursor")
        THROW Exception{"Failed: AliasedCursor was not created!"}
    ENDIF

    SELECT AliasedCursor
    GO TOP

    LOCAL aliasCount AS DWORD
    aliasCount := 0
    LOCAL fieldCount AS DWORD
    fieldCount := FCount()

    DO WHILE !EOF()
        aliasCount++
        SKIP
    ENDDO

    VerifyTable(aliasCount, "Failed: Verified record count mismatch in AliasedCursor", TRUE)

    // Close the test table
    USE IN test_cursor_target

    // Close the test table
    USE IN test_select_table

    RETURN

FUNCTION TestCrossJoinFunctionality() AS VOID
    ? "Testing CROSS JOIN functionality..."

    // Create test tables for CROSS JOIN
    CREATE CURSOR cross_join_table_a (Id N(5), Name C(20))
    CREATE CURSOR cross_join_table_b (Id N(5), Desc C(30))
    CREATE CURSOR cross_join_table_c (Id N(5), Label C(20))

    IF !Used("cross_join_table_a") .OR. !Used("cross_join_table_b") .OR. !Used("cross_join_table_c")
        THROW Exception{"Failed: Failed to create test tables for CROSS JOIN"}
    ENDIF

    IF Verbose
        ? "Created test tables for CROSS JOIN"
    ENDIF

    // Insert test data
    INSERT INTO cross_join_table_a VALUES (1, "Item A1")
    INSERT INTO cross_join_table_a VALUES (2, "Item A2")

    INSERT INTO cross_join_table_b VALUES (1, "Description B1")
    INSERT INTO cross_join_table_b VALUES (2, "Description B2")
    INSERT INTO cross_join_table_b VALUES (3, "Description B3")
    INSERT INTO cross_join_table_c VALUES (2, "Label C2")
    INSERT INTO cross_join_table_c VALUES (3, "Label C3")

    IF Verbose
        ? "Inserted test data for CROSS JOIN"
    ENDIF

// Test explicit CROSS JOIN syntax
    IF Verbose
        ? "Testing explicit CROSS JOIN syntax..."
    ENDIF

    SELECT cross_join_table_a.Name, cross_join_table_b.Desc ;
        FROM cross_join_table_a ;
        CROSS JOIN cross_join_table_b
    VerifyTable(6, "Failed: Expected 6 records from explicit CROSS JOIN", TRUE, ;
            { {"Item A1", "Description B1"}, ;
              {"Item A1", "Description B2"}, ;
              {"Item A1", "Description B3"}, ;
              {"Item A2", "Description B1"}, ;
              {"Item A2", "Description B2"}, ;
              {"Item A2", "Description B3"} } )

// Test CROSS JOIN with aliases
    IF Verbose
        ? "Testing CROSS JOIN with aliases..."
    ENDIF

    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b
    VerifyTable(6, "Failed: Expected 6 records from CROSS JOIN with aliases", TRUE, ;
            { {"Item A1", "Description B1"}, ;
              {"Item A1", "Description B2"}, ;
              {"Item A1", "Description B3"}, ;
              {"Item A2", "Description B1"}, ;
              {"Item A2", "Description B2"}, ;
              {"Item A2", "Description B3"} } )

// Test CROSS JOIN with WHERE clause
    IF Verbose
        ? "Testing CROSS JOIN with WHERE clause..."
    ENDIF

    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE a.Id = 1
    VerifyTable(3, "Failed: Expected 3 records from CROSS JOIN with WHERE", TRUE, ;
            { {"Item A1", "Description B1"}, ;
              {"Item A1", "Description B2"}, ;
              {"Item A1", "Description B3"} } )

    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE b.Id >= 2
    VerifyTable(4, "Failed: Inner-table-only candidates changed cross-join results", TRUE)

    // Multi-table candidate bitmaps must prune both sides without changing
    // the nested-loop result set. The full WHERE expression remains authoritative.
    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE a.Id = 2 AND b.Id >= 2
    VerifyTable(2, "Failed: Per-table candidates changed multi-table AND results", TRUE, ;
            { {"Item A2", "Description B2"}, ;
              {"Item A2", "Description B3"} } )

    SELECT a.Name, b.Desc, c.Label ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        CROSS JOIN cross_join_table_c c ;
        WHERE a.Id = 2 AND b.Id >= 2 AND c.Id >= 2
    VerifyTable(4, "Failed: Three-table candidate carry/reset changed rows", TRUE)

    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE a.Id = 99 AND b.Id = 2
    VerifyTable(0, "Failed: Empty candidate list should produce no rows", TRUE)

    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE a.Id = 2 AND b.Id = 99
    VerifyTable(0, "Failed: Empty inner candidate list should produce no rows", TRUE)

    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE a.Id = 1 OR b.Id = 3
    VerifyTable(4, "Failed: Cross-table OR candidate fallback returned incorrect rows", TRUE, ;
            { {"Item A1", "Description B1"}, ;
              {"Item A1", "Description B2"}, ;
              {"Item A1", "Description B3"}, ;
              {"Item A2", "Description B3"} } )

    // Bare JOIN and INNER JOIN both apply their ON predicate.
    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        JOIN cross_join_table_b b ON a.Id = b.Id
    VerifyTable(2, "Failed: Expected matching rows from JOIN ON", TRUE, ;
            { {"Item A1", "Description B1"}, ;
              {"Item A2", "Description B2"} } )

    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        INNER JOIN cross_join_table_b b ON a.Id = b.Id AND b.Id > 1 ;
        WHERE a.Id = 2
    VerifyTable(1, "Failed: Expected ON predicates and WHERE to both apply", TRUE, ;
            { {"Item A2", "Description B2"} } )

    SELECT a.Name, c.Label ;
        FROM cross_join_table_a a ;
        JOIN cross_join_table_b b ON a.Id = b.Id ;
        INNER JOIN cross_join_table_c c ON b.Id = c.Id
    VerifyTable(1, "Failed: Expected chained JOIN predicates to apply", TRUE, ;
            { {"Item A2", "Label C2"} } )

    // Duplicate source rows must remain distinct result combinations.
    INSERT INTO cross_join_table_b VALUES (2, "Description B2 duplicate")
    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE a.Id = 1 AND b.Id = 2
    VerifyTable(2, "Failed: Duplicate candidate rows were collapsed", TRUE)

    // Existing RDD filters must remain in effect through candidate generation.
    DbSelectArea("cross_join_table_b")
    DbSetFilter("Id = 2")
    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE b.Id >= 2
    VerifyTable(4, "Failed: SELECT ignored an active source-table filter", TRUE)
    DbSelectArea("cross_join_table_b")
    IF String.IsNullOrWhiteSpace((STRING)DbInfo(DBI_DBFILTER))
        THROW Exception{"SELECT cleared the source-table filter"}
    ENDIF
    DbSetFilter(NULL)

    // Candidate record lists must preserve the source's active RDD order.
    INSERT INTO cross_join_table_b VALUES (4, "Description B4")
    INSERT INTO cross_join_table_b VALUES (0, "Description B0")
    DbSelectArea("cross_join_table_b")
    IF !DbCreateOrder("IdOrder",, "Id")
        THROW Exception{"Could not create order for candidate traversal test"}
    ENDIF
    OrdSetFocus("IdOrder")
    SELECT a.Name, b.Id ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE a.Id = 1 AND b.Id >= 0
    VerifyTable(6, "Failed: Candidate lists changed active-index traversal order", TRUE, ;
        { {"Item A1", 0}, ;
          {"Item A1", 1}, ;
          {"Item A1", 2}, ;
          {"Item A1", 2}, ;
          {"Item A1", 3}, ;
          {"Item A1", 4} } )
    DbSelectArea("cross_join_table_b")
    OrdSetFocus(0)

    // Close the test tables
    USE IN cross_join_table_a
    USE IN cross_join_table_b
    USE IN cross_join_table_c

    RETURN

FUNCTION TestDeletedCandidateRows() AS VOID
    CREATE CURSOR deleted_candidate_left (Id N(5))
    CREATE CURSOR deleted_candidate_right (Id N(5))
    INSERT INTO deleted_candidate_left VALUES (1)
    INSERT INTO deleted_candidate_left VALUES (2)
    INSERT INTO deleted_candidate_right VALUES (1)
    INSERT INTO deleted_candidate_right VALUES (2)
    DbSelectArea("deleted_candidate_right")
    GO 2
    __DbDelete()
    INSERT INTO deleted_candidate_right VALUES (3)

    LOCAL oldDeleted := SetDeleted() AS LOGIC
    TRY
        SetDeleted(TRUE)

        SELECT a.Id, b.Id ;
            FROM deleted_candidate_left a ;
            CROSS JOIN deleted_candidate_right b ;
            WHERE b.Id >= 1
        VerifyTable(4, "Failed: Candidate scan included a deleted source row", TRUE)
    FINALLY
        SetDeleted(oldDeleted)
        IF Used("deleted_candidate_left")
            USE IN deleted_candidate_left
        ENDIF
        IF Used("deleted_candidate_right")
            USE IN deleted_candidate_right
        ENDIF
    END TRY
    RETURN

FUNCTION TestUnionFunctionality() AS VOID
    CREATE CURSOR union_left (Id N(5) NULL, Label C(20) NULL)
    CREATE CURSOR union_middle (Id N(5) NULL, Label C(20) NULL)
    CREATE CURSOR union_right (Id N(5) NULL, Label C(20) NULL)
    CREATE CURSOR union_empty (Id N(5) NULL, Label C(20) NULL)
    CREATE CURSOR union_integer (Id I NULL, Label C(20) NULL)
    CREATE CURSOR union_null_left (Id N(5))
    CREATE CURSOR union_null_right (Id N(5))

    INSERT INTO union_left VALUES (1, "one")
    INSERT INTO union_left VALUES (2, "shared")
    INSERT INTO union_left VALUES (2, "duplicate value")
    INSERT INTO union_middle VALUES (2, "shared")
    INSERT INTO union_middle VALUES (3, "three")
    INSERT INTO union_right VALUES (1, "one")
    INSERT INTO union_right VALUES (4, "four")
    INSERT INTO union_integer VALUES (5, "five")

    SELECT Id, Label FROM union_left UNION ALL SELECT Id, Label FROM union_middle INTO CURSOR UnionAllResult
    VerifyTable(5, "UNION ALL should preserve duplicates and branch order", TRUE, ;
        { {1, "one"}, {2, "shared"}, {2, "duplicate value"}, {2, "shared"}, {3, "three"} } )

    SELECT Id, Label FROM union_left UNION SELECT Id, Label FROM union_middle INTO CURSOR UnionDistinctResult
    VerifyTable(4, "UNION should remove duplicates across branches", TRUE, ;
        { {1, "one"}, {2, "shared"}, {2, "duplicate value"}, {3, "three"} } )

    SELECT Id, Label FROM union_left UNION ALL SELECT Id, Label FROM union_middle UNION SELECT Id, Label FROM union_right
    VerifyTable(5, "Mixed UNION operators should apply left to right", TRUE)

    SELECT Id, Label FROM union_left UNION ALL SELECT Id, Label FROM union_empty
    VerifyTable(3, "An empty UNION ALL branch should add no rows", TRUE)

    SELECT Id, Label FROM union_left UNION ALL SELECT Id, Label FROM union_integer
    VerifyTable(4, "Compatible numeric branch types should combine", TRUE)

    SELECT TOP 1 Id, Label FROM union_left UNION ALL SELECT TOP 1 Id, Label FROM union_middle
    VerifyTable(2, "TOP should apply independently to each UNION branch", TRUE)

    SELECT Id, Label FROM union_left UNION ALL SELECT Id, Label FROM union_middle ORDER BY Id DESC INTO CURSOR UnionOrderedResult
    VerifyTable(5, "Final ORDER BY should sort the combined UNION result", TRUE, ;
        { {3, "three"}, {2, "shared"}, {2, "duplicate value"}, {2, "shared"}, {1, "one"} } )

    SELECT SUM(Id) AS Total FROM union_null_left UNION ALL SELECT SUM(Id) AS Total FROM union_null_right
    VerifyTable(2, "UNION ALL should preserve NULL values", TRUE, { {NULL}, {NULL} } )

    SELECT SUM(Id) AS Total FROM union_null_left UNION SELECT SUM(Id) AS Total FROM union_null_right
    VerifyTable(1, "UNION should treat NULL result rows as duplicates", TRUE, { {NULL} } )

    LOCAL incompatibleSchemaError := FALSE AS LOGIC
    TRY
        SELECT Label FROM union_left UNION ALL SELECT Id FROM union_integer
    CATCH ex AS Exception
        incompatibleSchemaError := ex:Message:Contains("incompatible types")
    END TRY
    IF !incompatibleSchemaError
        THROW Exception{"UNION should reject incompatible column types"}
    ENDIF

    IF Used("union_left")
        USE IN union_left
    ENDIF
    IF Used("union_middle")
        USE IN union_middle
    ENDIF
    IF Used("union_right")
        USE IN union_right
    ENDIF
    IF Used("union_empty")
        USE IN union_empty
    ENDIF
    IF Used("union_integer")
        USE IN union_integer
    ENDIF
    IF Used("union_null_left")
        USE IN union_null_left
    ENDIF
    IF Used("union_null_right")
        USE IN union_null_right
    ENDIF
    RETURN

FUNCTION TestNestedSubqueryFunctionality() AS VOID
    CREATE CURSOR nested_parent (Id N(5) NULL)
    CREATE CURSOR nested_parent_duplicates (Id N(5) NULL)
    CREATE CURSOR nested_child (Id N(5) NULL, Label C(20))
    CREATE CURSOR nested_empty (Id N(5) NULL)
    CREATE CURSOR nested_repeat (Id N(5) NULL)
    INSERT INTO nested_parent VALUES (1)
    INSERT INTO nested_parent VALUES (2)
    INSERT INTO nested_parent VALUES (3)
    INSERT INTO nested_parent VALUES (4)
    INSERT INTO nested_parent VALUES (NULL)
    INSERT INTO nested_parent_duplicates VALUES (2)
    INSERT INTO nested_parent_duplicates VALUES (2)
    INSERT INTO nested_parent_duplicates VALUES (4)
    INSERT INTO nested_parent_duplicates VALUES (NULL)
    INSERT INTO nested_child VALUES (2, "first")
    INSERT INTO nested_child VALUES (2, "duplicate")
    INSERT INTO nested_child VALUES (4, "four")
    INSERT INTO nested_child VALUES (NULL, "null")

    LOCAL openAreas := XSharp.__FoxArray{} AS ARRAY
    LOCAL workAreaCountBeforeDerived := AUsed(openAreas) AS DWORD
    SELECT d.Id, d.Label FROM (SELECT c.Id, c.Label FROM nested_child c WHERE c.Id > 1) d ;
        WHERE d.Id >= 2 ORDER BY d.Id, d.Label
    VerifyTable(3, "Derived table fields should support outer filtering and ordering", TRUE, ;
        {{2, "duplicate"}, {2, "first"}, {4, "four"}})
    IF AUsed(openAreas) != workAreaCountBeforeDerived
        THROW Exception{"Derived table cursor should close after the outer SELECT completes"}
    ENDIF

    SELECT d.Id, COUNT(*) AS Quantity FROM (SELECT c.Id FROM nested_child c WHERE c.Id > 1) d ;
        GROUP BY d.Id ORDER BY d.Id
    VerifyTable(2, "Outer grouping should work over a derived table", TRUE, {{2, 2}, {4, 1}})
    IF AUsed(openAreas) != workAreaCountBeforeDerived
        THROW Exception{"Derived table cursor should close after grouped SELECT execution"}
    ENDIF

    LOCAL hashJoinBeforeDerived := SqlOptimizationCount("HashJoins") AS INT
    SELECT p.Id, d.Label FROM nested_parent p INNER JOIN ;
        (SELECT c.Id, c.Label FROM nested_child c WHERE c.Id > 1) d ON p.Id = d.Id ;
        ORDER BY p.Id, d.Label
    VerifyTable(3, "INNER JOIN should resolve fields from a derived right source", TRUE, ;
        {{2, "duplicate"}, {2, "first"}, {4, "four"}})
    VerifySqlOptimization("HashJoins", hashJoinBeforeDerived)
    IF AUsed(openAreas) != workAreaCountBeforeDerived
        THROW Exception{"Joined derived table cursor should close after execution"}
    ENDIF

    SELECT d.Id, p.Id FROM (SELECT c.Id FROM nested_child c WHERE c.Id > 1) d ;
        INNER JOIN nested_parent p ON d.Id = p.Id ORDER BY d.Id
    VerifyTable(3, "INNER JOIN should resolve a derived left source", TRUE, {{2, 2}, {2, 2}, {4, 4}})

    SELECT p.Id FROM nested_parent p INNER JOIN (SELECT c.Id FROM nested_empty c) d ON p.Id = d.Id
    VerifyTable(0, "An empty derived source should produce no INNER JOIN matches", TRUE)
    IF AUsed(openAreas) != workAreaCountBeforeDerived
        THROW Exception{"Empty joined derived table cursor should close after execution"}
    ENDIF

    LOCAL twoDerivedHashBefore := SqlOptimizationCount("HashJoins") AS INT
    LOCAL twoDerivedExecutionsBefore := SqlOptimizationCount("DerivedTableExecutions") AS INT
    SELECT a.Id, b.Label FROM (SELECT c.Id FROM nested_child c WHERE c.Id > 1) a ;
        INNER JOIN (SELECT x.Id, x.Label FROM nested_child x WHERE x.Id > 1) b ON a.Id = b.Id ;
        ORDER BY a.Id, b.Label
    VerifyTable(5, "Two derived inputs should preserve duplicate INNER JOIN matches", TRUE, ;
        {{2, "duplicate"}, {2, "duplicate"}, {2, "first"}, {2, "first"}, {4, "four"}})
    VerifySqlOptimization("HashJoins", twoDerivedHashBefore)
    IF SqlOptimizationCount("DerivedTableExecutions") - twoDerivedExecutionsBefore != 2 .OR. ;
       AUsed(openAreas) != workAreaCountBeforeDerived
        THROW Exception{"Each derived source should materialize once and close after the joined SELECT"}
    ENDIF

    LOCAL chainDerivedExecutionsBefore := SqlOptimizationCount("DerivedTableExecutions") AS INT
    SELECT p.Id, a.Id, b.Label FROM nested_parent p ;
        INNER JOIN (SELECT c.Id FROM nested_child c WHERE c.Id > 1) a ON p.Id = a.Id ;
        INNER JOIN (SELECT x.Id, x.Label FROM nested_child x WHERE x.Id > 1) b ON a.Id = b.Id ;
        ORDER BY p.Id, b.Label
    VerifyTable(5, "Multiple derived sources should work in a three-source INNER JOIN chain", TRUE, ;
        {{2, 2, "duplicate"}, {2, 2, "duplicate"}, {2, 2, "first"}, {2, 2, "first"}, {4, 4, "four"}})
    IF SqlOptimizationCount("DerivedTableExecutions") - chainDerivedExecutionsBefore != 2 .OR. ;
       AUsed(openAreas) != workAreaCountBeforeDerived
        THROW Exception{"Derived sources in a longer join chain should execute once and close"}
    ENDIF

    LOCAL failedDerivedExecutionRejected := FALSE AS LOGIC
    LOCAL derivedFailureCallerArea := VoDbGetSelect() AS DWORD
    TRY
        SELECT d.Id FROM (SELECT c.Id FROM nested_child c) d ;
            WHERE d.Id = (SELECT c.Id FROM nested_child c WHERE c.Id = 2)
    CATCH ex AS Exception
        failedDerivedExecutionRejected := ex:Message:Contains("more than one record")
    END TRY
    IF !failedDerivedExecutionRejected .OR. AUsed(openAreas) != workAreaCountBeforeDerived .OR. ;
       VoDbGetSelect() != derivedFailureCallerArea
        THROW Exception{"Failing outer SELECT should close the derived cursor and restore workarea"}
    ENDIF

    LOCAL secondDerivedFailureRejected := FALSE AS LOGIC
    LOCAL secondDerivedFailureCallerArea := VoDbGetSelect() AS DWORD
    LOCAL secondDerivedExecutionsBefore := SqlOptimizationCount("DerivedTableExecutions") AS INT
    TRY
        SELECT a.Id FROM (SELECT c.Id FROM nested_child c) a ;
            INNER JOIN (SELECT x.Id FROM nested_child x WHERE x.Id = ;
                (SELECT y.Id FROM nested_child y WHERE y.Id = 2)) b ON a.Id = b.Id
    CATCH ex AS Exception
        secondDerivedFailureRejected := ex:Message:Contains("more than one record")
    END TRY
    IF !secondDerivedFailureRejected .OR. ;
       SqlOptimizationCount("DerivedTableExecutions") - secondDerivedExecutionsBefore != 2 .OR. ;
       AUsed(openAreas) != workAreaCountBeforeDerived .OR. VoDbGetSelect() != secondDerivedFailureCallerArea
        THROW Exception{"Failure while materializing a later derived source should clean prior cursors and restore workarea"}
    ENDIF

    LOCAL correlatedDerivedRejected := FALSE AS LOGIC
    TRY
        SELECT d.Id FROM (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id) d
    CATCH ex AS Exception
        correlatedDerivedRejected := ex:Message:Contains("cannot be correlated")
    END TRY
    IF !correlatedDerivedRejected .OR. AUsed(openAreas) != workAreaCountBeforeDerived
        THROW Exception{"Correlated derived tables should be rejected without leaking a workarea"}
    ENDIF

    LOCAL projectionScalarBefore := SqlOptimizationCount("ScalarExecutions") AS INT
    SELECT p.Id, (SELECT c.Id FROM nested_child c WHERE c.Label = "four") AS ScalarId ;
        FROM nested_parent p WHERE p.Id <= 2 ORDER BY p.Id
    VerifyTable(2, "Uncorrelated scalar projection should repeat its value for outer rows", TRUE, {{1, 4}, {2, 4}})
    IF SqlOptimizationCount("ScalarExecutions") - projectionScalarBefore != 1
        THROW Exception{"Uncorrelated projection subquery should execute once per outer SELECT"}
    ENDIF

    SELECT (SELECT c.Label FROM nested_child c WHERE c.Label = "four") AS LabelValue FROM nested_parent p WHERE p.Id = 1
    VerifyTable(1, "Character scalar projections should preserve their value", FALSE, {{"four"}})
    VAR scalarProjectionStruct := DbStruct()
    IF scalarProjectionStruct[1][2]:ToString():Trim():ToUpperInvariant() != "C"
        THROW Exception{"Character scalar projection should preserve the subquery field type"}
    ENDIF
    USE IN QUERYRESULT

    SELECT (SELECT c.Id FROM nested_empty c) AS MissingValue FROM nested_parent p WHERE p.Id = 1
    VerifyTable(1, "Empty scalar projection should produce NULL", FALSE, {{NULL}})
    VAR emptyScalarProjectionStruct := DbStruct()
    IF emptyScalarProjectionStruct[1][2]:ToString():Trim():ToUpperInvariant() != "N" .OR. ;
       ((LONG)emptyScalarProjectionStruct[1][6] & (LONG)DBFFieldFlags.Nullable) == 0
        THROW Exception{"Empty scalar projection should retain numeric type and allow NULL"}
    ENDIF
    USE IN QUERYRESULT

    SELECT p.Id + (SELECT c.Id FROM nested_child c WHERE c.Label = "four") AS Total ;
        FROM nested_parent p WHERE p.Id <= 2 ORDER BY p.Id
    VerifyTable(2, "Numeric scalar subqueries should work inside projection expressions", TRUE, {{5}, {6}})

    SELECT p.Label + (SELECT c.Label FROM nested_child c WHERE c.Label = "four") AS Joined ;
        FROM nested_child p WHERE p.Id = 2 ORDER BY p.Label
    VerifyTable(2, "Character scalar subqueries should work inside concatenation expressions", TRUE, ;
        {{"duplicate           four                "}, {"first               four                "}})

    SELECT COUNT(*) + (SELECT c.Id FROM nested_child c WHERE c.Label = "four") AS Total ;
        FROM nested_parent p
    VerifyTable(1, "Scalar projections should compose with outer aggregates", TRUE, {{9}})

    SELECT p.Id FROM nested_parent p WHERE p.Id = ANY (SELECT c.Id FROM nested_child c WHERE c.Id > 0)
    VerifyTable(2, "ANY should match a value occurring in the subquery set", TRUE, {{2}, {4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id == SOME (SELECT c.Id FROM nested_child c WHERE c.Id > 0)
    VerifyTable(2, "SOME should be an alias for ANY", TRUE, {{2}, {4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id < ALL (SELECT c.Id FROM nested_child c WHERE c.Id > 0)
    VerifyTable(1, "ALL should require the comparison to match every non-NULL set value", TRUE, {{1}})
    SELECT p.Id FROM nested_parent p WHERE p.Id >= ANY (SELECT c.Id FROM nested_child c WHERE c.Label = "four")
    VerifyTable(1, "ANY should support ordered comparisons", TRUE, {{4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id > ANY (SELECT c.Id FROM nested_child c WHERE c.Id = 2)
    VerifyTable(2, "ANY should support greater-than comparisons", TRUE, {{3}, {4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id <= ANY (SELECT c.Id FROM nested_child c WHERE c.Id = 2)
    VerifyTable(2, "ANY should support less-than-or-equal comparisons", TRUE, {{1}, {2}})
    SELECT p.Id FROM nested_parent p WHERE p.Id != ALL (SELECT c.Id FROM nested_child c WHERE c.Id = 2)
    VerifyTable(3, "ALL should support inequality comparisons", TRUE, {{1}, {3}, {4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id = ANY (SELECT c.Id FROM nested_empty c)
    VerifyTable(0, "ANY over an empty set should be false", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id = ALL (SELECT c.Id FROM nested_empty c)
    VerifyTable(5, "ALL over an empty set should be true, including for a NULL left operand", TRUE, ;
        {{1}, {2}, {3}, {4}, {NULL}})
    SELECT p.Id FROM nested_parent p WHERE p.Id = ANY (SELECT c.Id FROM nested_child c WHERE c.Label = "null")
    VerifyTable(0, "ANY comparisons with only NULL values should be unknown", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id < ANY (SELECT c.Id FROM nested_child c WHERE c.Id = 4 OR c.Label = "null")
    VerifyTable(3, "ANY should propagate NULL only when no comparison is true", TRUE, {{1}, {2}, {3}})
    SELECT p.Id FROM nested_parent p WHERE p.Id < ALL (SELECT c.Id FROM nested_child c WHERE c.Id = 4 OR c.Label = "null")
    VerifyTable(0, "ALL should propagate NULL when no comparison is false", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id < ANY (SELECT c.Id FROM nested_child c WHERE c.Id >= p.Id)
    VerifyTable(3, "Correlated ANY should evaluate its set for each outer row", TRUE, {{1}, {2}, {3}})
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = ANY (SELECT c.Id FROM nested_child c WHERE c.Label = "null"))
    VerifyTable(0, "NOT should preserve UNKNOWN from a quantified comparison", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id = 1 OR p.Id = ANY (SELECT c.Id FROM nested_child c WHERE c.Id = 4)
    VerifyTable(2, "Quantified comparisons should compose with OR", TRUE, {{1}, {4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id = ANY (SELECT c.Id FROM nested_child c WHERE c.Id > 0) AND EXISTS (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id)
    VerifyTable(2, "Quantified predicates should compose with correlated EXISTS", TRUE, {{2}, {4}})

    LOCAL scalarBefore := SqlOptimizationCount("ScalarExecutions") AS INT
    SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT c.Id FROM nested_child c WHERE c.Label = "four")
    VerifyTable(1, "Scalar comparison should match the single value", TRUE, {{4}})
    IF SqlOptimizationCount("ScalarExecutions") - scalarBefore != 1
        THROW Exception{"Uncorrelated scalar subquery should execute once"}
    ENDIF
    SELECT p.Id FROM nested_parent p WHERE p.Id < (SELECT c.Id FROM nested_child c WHERE c.Label = "four")
    VerifyTable(3, "Scalar less-than comparison should match", TRUE, {{1}, {2}, {3}})
    SELECT p.Id FROM nested_parent p WHERE p.Id <= (SELECT c.Id FROM nested_child c WHERE c.Label = "four")
    VerifyTable(4, "Scalar less-than-or-equal comparison should match", TRUE, {{1}, {2}, {3}, {4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id > (SELECT c.Id FROM nested_child c WHERE c.Label = "four")
    VerifyTable(0, "Scalar greater-than comparison should match", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id >= (SELECT c.Id FROM nested_child c WHERE c.Label = "four")
    VerifyTable(1, "Scalar greater-than-or-equal comparison should match", TRUE, {{4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id == (SELECT c.Id FROM nested_child c WHERE c.Label = "four")
    VerifyTable(1, "Scalar exact equality should match", TRUE, {{4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id != (SELECT c.Id FROM nested_child c WHERE c.Label = "four")
    VerifyTable(3, "Scalar inequality should exclude NULL", TRUE, {{1}, {2}, {3}})
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT c.Id FROM nested_child c WHERE c.Label = "four"))
    VerifyTable(3, "Negated scalar equality should exclude NULL", TRUE, {{1}, {2}, {3}})
    SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT c.Id FROM nested_empty c)
    VerifyTable(0, "Empty scalar subquery should compare as NULL", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id != (SELECT c.Id FROM nested_empty c)
    VerifyTable(0, "Empty scalar inequality should compare as NULL", TRUE)
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT c.Id FROM nested_empty c))
    VerifyTable(0, "Negated NULL scalar comparison should not match", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT c.Id FROM nested_child c WHERE c.Label = "null")
    VerifyTable(0, "NULL scalar value should not match", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id AND c.Label = "four")
    VerifyTable(1, "Correlated scalar subquery should use the current outer row", TRUE, {{4}})
    scalarBefore := SqlOptimizationCount("ScalarExecutions")
    SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id AND c.Label = "four") AND p.Id <= (SELECT c.Id FROM nested_child c WHERE c.Label = "four")
    VerifyTable(1, "Mixed scalar subqueries should retain correlation", TRUE, {{4}})
    IF SqlOptimizationCount("ScalarExecutions") - scalarBefore != 6
        THROW Exception{"Uncorrelated scalar subqueries should execute once during correlated refresh"}
    ENDIF
    SELECT p.Id FROM nested_parent p WHERE p.Id < (SELECT COUNT(*) FROM nested_child)
    VerifyTable(3, "Scalar subquery should allow aggregate values", TRUE, {{1}, {2}, {3}})
    SELECT p.Id FROM nested_parent p WHERE (p.Id = (SELECT c.Id FROM nested_child c WHERE c.Label = "four") OR p.Id = 1) AND EXISTS (SELECT Id FROM nested_child)
    VerifyTable(2, "Scalar comparisons should preserve logical precedence", TRUE, {{1}, {4}})
    SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT c.Id FROM nested_child c WHERE c.Id IN (SELECT e.Id FROM nested_empty e))
    VerifyTable(0, "Scalar subqueries should support nested IN", TRUE)
    SELECT c.Id FROM nested_child c WHERE c.Label = (SELECT p.Label FROM nested_child p WHERE p.Label = "four")
    VerifyTable(1, "Scalar comparisons should preserve character values", TRUE, {{4}})
    SELECT nested_parent
    LOCAL scalarCallerArea := VoDbGetSelect() AS DWORD
    LOCAL scalarRowsRejected := FALSE AS LOGIC
    TRY
        SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT c.Id FROM nested_child c WHERE c.Id = 2)
    CATCH ex AS Exception
        scalarRowsRejected := ex:Message:Contains("more than one record")
    END TRY
    IF !scalarRowsRejected .OR. VoDbGetSelect() != scalarCallerArea .OR. Used("QUERYRESULT")
        THROW Exception{"Scalar subquery should reject duplicate rows and restore workarea and result state"}
    ENDIF
    LOCAL scalarColumnsRejected := FALSE AS LOGIC
    TRY
        SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT c.Id, c.Label FROM nested_child c WHERE c.Id = 4)
    CATCH ex AS Exception
        scalarColumnsRejected := ex:Message:Contains("one column")
    END TRY
    IF !scalarColumnsRejected
        THROW Exception{"Scalar subquery should reject multiple columns"}
    ENDIF
    LOCAL projectionRowsRejected := FALSE AS LOGIC
    TRY
        SELECT p.Id, (SELECT c.Id FROM nested_child c WHERE c.Id = 2) AS ChildId ;
            FROM nested_parent p
    CATCH ex AS Exception
        projectionRowsRejected := ex:Message:Contains("more than one record")
    END TRY
    IF !projectionRowsRejected
        THROW Exception{"Scalar projection should reject multiple result rows"}
    ENDIF
    LOCAL quantifiedColumnsRejected := FALSE AS LOGIC
    TRY
        SELECT p.Id FROM nested_parent p WHERE p.Id >= ANY (SELECT c.Id, c.Label FROM nested_child c)
    CATCH ex AS Exception
        quantifiedColumnsRejected := ex:Message:Contains("one column")
    END TRY
    IF !quantifiedColumnsRejected
        THROW Exception{"Quantified subqueries should reject multiple columns"}
    ENDIF
    LOCAL correlatedProjectionRejected := FALSE AS LOGIC
    TRY
        SELECT p.Id, (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id) AS ChildId ;
            FROM nested_parent p
    CATCH ex AS Exception
        correlatedProjectionRejected := ex:Message:Contains("SELECT list")
    END TRY
    IF !correlatedProjectionRejected
        THROW Exception{"Correlated scalar projections should be rejected in this increment"}
    ENDIF
    LOCAL scalarDeepRejected := FALSE AS LOGIC
    TRY
        SELECT p.Id FROM nested_parent p WHERE EXISTS (SELECT c.Id FROM nested_child c WHERE c.Id = (SELECT e.Id FROM nested_empty e WHERE e.Id = p.Id))
    CATCH ex AS Exception
        scalarDeepRejected := ex:Message:Contains("immediate outer query")
    END TRY
    IF !scalarDeepRejected
        THROW Exception{"Scalar subqueries should reject grandparent correlation"}
    ENDIF
    // Reference: VFP 9 keeps UNKNOWN through NOT, AND and OR.
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = 1 OR p.Id = (SELECT Id FROM nested_empty))
    VerifyTable(0, "NOT (true OR unknown) and NOT (false OR unknown) should not match", TRUE)
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = 1 AND p.Id = (SELECT Id FROM nested_empty))
    VerifyTable(3, "NOT (false AND unknown) should match", TRUE, {{2}, {3}, {4}})
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT Id FROM nested_empty) AND p.Id = 1)
    VerifyTable(3, "Unknown AND false should be false in either operand order", TRUE, {{2}, {3}, {4}})
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT Id FROM nested_empty) OR p.Id IN (SELECT Id FROM nested_child))
    VerifyTable(0, "Unknown OR IN containing NULL should not match under NOT", TRUE)
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT Id FROM nested_empty) AND p.Id IN (SELECT Id FROM nested_child))
    VerifyTable(0, "Unknown AND IN containing NULL should not match under NOT", TRUE)
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT Id FROM nested_empty) AND p.Id NOT IN (SELECT Id FROM nested_child))
    VerifyTable(2, "Unknown AND NOT IN should distinguish matches from unknown", TRUE, {{2}, {4}})
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT Id FROM nested_empty) AND p.Id NOT IN (2, NULL))
    VerifyTable(1, "NOT IN list should distinguish matches from unknown", TRUE, {{2}})
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT Id FROM nested_empty) AND EXISTS (SELECT Id FROM nested_empty))
    VerifyTable(5, "Unknown AND false EXISTS should be false", TRUE)
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT c.Id FROM nested_child c WHERE c.Label = "four") AND p.Id NOT IN (2, NULL))
    VerifyTable(3, "NOT IN with NULL should preserve unknown in a compound predicate", TRUE, {{1}, {2}, {3}})
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT c.Id FROM nested_child c WHERE c.Label = "four") AND p.Id IN (SELECT Id FROM nested_empty))
    VerifyTable(5, "IN an empty set should be false even for NULL outer values", TRUE)
    SELECT p.Id FROM nested_parent p WHERE NOT (p.Id = (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id AND c.Label = "four") OR EXISTS (SELECT Id FROM nested_empty))
    VerifyTable(0, "Correlated scalar NULL results should remain unknown under NOT", TRUE)

    SELECT p.Id FROM nested_parent p WHERE EXISTS (SELECT c.Id, c.Label FROM nested_child c WHERE c.Id = p.Id)
    VerifyTable(2, "Correlated EXISTS should find matching outer rows", TRUE, {{2}, {4}})
    SELECT p.Id FROM nested_parent p WHERE NOT EXISTS (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id)
    VerifyTable(3, "Correlated NOT EXISTS should include unmatched and NULL outer rows", TRUE, {{1}, {3}, {NULL}})
    SELECT Id FROM nested_parent WHERE EXISTS (SELECT Id FROM nested_empty)
    VerifyTable(0, "EXISTS with an empty result should be false", TRUE)
    SELECT Id FROM nested_parent WHERE NOT EXISTS (SELECT Id FROM nested_empty)
    VerifyTable(5, "NOT EXISTS with an empty result should be true", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id > 1 AND EXISTS (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id)
    VerifyTable(2, "EXISTS should work within a logical expression", TRUE, {{2}, {4}})
    SELECT p.Id FROM nested_parent p WHERE EXISTS (SELECT c.Id FROM nested_child c WHERE c.Id > p.Id)
    VerifyTable(3, "Correlated EXISTS should support inequalities", TRUE, {{1}, {2}, {3}})
    SELECT p.Id FROM nested_parent p WHERE EXISTS (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id AND EXISTS (SELECT e.Id FROM nested_empty e))
    VerifyTable(0, "Nested EXISTS should respect an empty inner result", TRUE)
    LOCAL existsAggregateRejected := FALSE AS LOGIC
    TRY
        SELECT Id FROM nested_parent WHERE EXISTS (SELECT COUNT(*) FROM nested_empty)
    CATCH ex AS Exception
        existsAggregateRejected := TRUE
    END TRY
    IF !existsAggregateRejected
        THROW Exception{"VFP rejects aggregate SELECT lists in EXISTS subqueries"}
    ENDIF
    SELECT Id FROM nested_parent WHERE EXISTS (SELECT c.Id FROM nested_child c GROUP BY c.Id)
    VerifyTable(5, "EXISTS should see grouped subquery results", TRUE)
    SELECT p.Id FROM nested_parent p WHERE EXISTS (SELECT p.Id FROM nested_child p WHERE p.Id = 2)
    VerifyTable(5, "An inner alias should shadow the outer alias", TRUE)
    LOCAL existsBefore := SqlOptimizationCount("ExistsExecutions") AS INT
    SELECT p.Id FROM nested_parent p WHERE EXISTS (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id) AND EXISTS (SELECT Id FROM nested_child)
    VerifyTable(2, "Mixed correlated and uncorrelated EXISTS predicates should match", TRUE, {{2}, {4}})
    IF SqlOptimizationCount("ExistsExecutions") - existsBefore != 6
        THROW Exception{"Uncorrelated EXISTS should execute once per outer query"}
    ENDIF

    SELECT p.Id FROM nested_parent p WHERE p.Id IN (SELECT c.Id FROM nested_child c) AND NOT EXISTS (SELECT e.Id FROM nested_empty e)
    VerifyTable(2, "Mixed IN and EXISTS predicates should match", TRUE, {{2}, {4}})
    SELECT Id FROM nested_parent WHERE EXISTS (SELECT Id FROM nested_repeat)
    VerifyTable(0, "Empty EXISTS should be false before insertion", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT Id FROM nested_repeat)
    VerifyTable(0, "Empty scalar subquery should not match before insertion", TRUE)
    INSERT INTO nested_repeat VALUES (4)
    SELECT Id FROM nested_parent WHERE EXISTS (SELECT Id FROM nested_repeat)
    VerifyTable(5, "EXISTS should be reevaluated after data changes", TRUE)
    SELECT p.Id FROM nested_parent p WHERE p.Id = (SELECT Id FROM nested_repeat)
    VerifyTable(1, "Scalar subquery should be reevaluated after data changes", TRUE, {{4}})

    LOCAL deepCorrelationRejected := FALSE AS LOGIC
    TRY
        SELECT p.Id FROM nested_parent p WHERE EXISTS (SELECT c.Id FROM nested_child c WHERE EXISTS (SELECT e.Id FROM nested_empty e WHERE e.Id = p.Id))
    CATCH ex AS Exception
        deepCorrelationRejected := ex:Message:Contains("immediate outer query")
    END TRY
    IF !deepCorrelationRejected
        THROW Exception{"Grandparent correlation should be rejected clearly"}
    ENDIF

    SELECT nested_parent
    LOCAL callerArea := VoDbGetSelect() AS DWORD
    LOCAL failureRejected := FALSE AS LOGIC
    TRY
        SELECT p.Id FROM nested_parent p WHERE EXISTS (SELECT c.Id FROM nested_child c WHERE c.Id IN (SELECT Id, Label FROM nested_child))
    CATCH ex AS Exception
        failureRejected := TRUE
    END TRY
    IF !failureRejected .OR. VoDbGetSelect() != callerArea .OR. Used("QUERYRESULT")
        THROW Exception{"Failed inner SELECT should restore the workarea and close its result"}
    ENDIF

    SELECT p.Id FROM nested_parent p WHERE EXISTS (SELECT c.Id FROM nested_child c WHERE EXISTS (SELECT e.Id FROM nested_parent e WHERE e.Id = c.Id))
    VerifyTable(5, "Innermost correlation should resolve the immediate parent", TRUE)

    SELECT Id FROM nested_parent WHERE Id IN (SELECT Id FROM nested_child)
    VerifyTable(2, "IN subquery should return matching outer values once", TRUE, {{2}, {4}})

    SELECT Id FROM nested_parent WHERE Id IN (SELECT Id FROM nested_child WHERE Id = 2)
    VerifyTable(1, "IN subquery should execute its own WHERE clause", TRUE, {{2}})

    SELECT Id FROM nested_parent WHERE Id IN (2, 4)
    VerifyTable(2, "Literal IN lists should remain executable", TRUE, {{2}, {4}})

    SELECT Id FROM nested_parent WHERE Id IN (SELECT Id FROM nested_child WHERE Id = 2 UNION SELECT Id FROM nested_child WHERE Id = 4)
    VerifyTable(2, "IN subquery should support a UNION result", TRUE, {{2}, {4}})

    SELECT Id FROM nested_parent WHERE Id NOT IN (SELECT Id FROM nested_child WHERE Id > 0)
    VerifyTable(2, "NOT IN should exclude matching values and NULL outer values", TRUE, {{1}, {3}})

    SELECT Id FROM nested_parent WHERE Id NOT IN (SELECT Id FROM nested_child)
    VerifyTable(0, "NOT IN should match no rows when the subquery contains NULL", TRUE)

    SELECT Id FROM nested_parent WHERE Id IN (SELECT Id FROM nested_empty)
    VerifyTable(0, "IN an empty subquery should match no rows", TRUE)

    SELECT Id FROM nested_parent WHERE Id NOT IN (SELECT Id FROM nested_empty)
    VerifyTable(5, "NOT IN an empty subquery should match every outer row", TRUE)

    LOCAL invalidArityRejected := FALSE AS LOGIC
    TRY
        SELECT Id FROM nested_parent WHERE Id IN (SELECT Id, Label FROM nested_child)
    CATCH ex AS Exception
        invalidArityRejected := ex:Message:Contains("exactly one column")
    END TRY
    IF !invalidArityRejected
        THROW Exception{"IN subquery should reject a result with multiple columns"}
    ENDIF

    LOCAL correlatedPlansBefore := SqlOptimizationCount("CorrelatedInPlans") AS INT
    SELECT p.Id FROM nested_parent p WHERE p.Id IN (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id)
    VerifySqlOptimization("CorrelatedInPlans", correlatedPlansBefore)
    VerifyTable(2, "Correlated IN subqueries should evaluate against the current outer row", TRUE, {{2}, {4}})

    SELECT p.Id FROM nested_parent p WHERE p.Id IN (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id AND c.Label = "four")
    VerifyTable(1, "Correlated IN should retain inner-only filters", TRUE, {{4}})

    SELECT p.Id FROM nested_parent p WHERE p.Id NOT IN (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id)
    VerifyTable(3, "Correlated NOT IN should complement per-row matches, including NULL outer values", TRUE, {{1}, {3}, {NULL}})

    SELECT p.Id FROM nested_parent_duplicates p WHERE p.Id IN (SELECT c.Id FROM nested_child c WHERE c.Id = p.Id)
    VerifyTable(3, "Correlated IN should preserve outer duplicates without multiplying inner duplicates", TRUE, {{2}, {2}, {4}})

    LOCAL unsupportedCorrelationRejected := FALSE AS LOGIC
    TRY
        SELECT Id FROM nested_parent p WHERE Id IN (SELECT p.Id FROM nested_child c WHERE c.Id > 0)
    CATCH ex AS Exception
        unsupportedCorrelationRejected := ex:Message:Contains("immediate outer query")
    END TRY
    IF !unsupportedCorrelationRejected
        THROW Exception{"Correlated IN references outside the subquery WHERE clause should fail clearly"}
    ENDIF

    IF Used("nested_parent")
        USE IN nested_parent
    ENDIF
    IF Used("nested_parent_duplicates")
        USE IN nested_parent_duplicates
    ENDIF
    IF Used("nested_child")
        USE IN nested_child
    ENDIF
    IF Used("nested_empty")
        USE IN nested_empty
    ENDIF
    IF Used("nested_repeat")
        USE IN nested_repeat
    ENDIF
    RETURN

FUNCTION TestHashJoin() AS VOID
    ? "Testing hash join functionality..."

    CREATE CURSOR hash_join_left (JoinId N(8,2) NULL, LeftName C(20))
    CREATE CURSOR hash_join_right (JoinId N(8,2) NULL, RightName C(20), Active L)

    INSERT INTO hash_join_left VALUES (1, "Left A")
    INSERT INTO hash_join_left VALUES (1, "Left B")
    INSERT INTO hash_join_left VALUES (2, "Left C")
    INSERT INTO hash_join_left VALUES (4, "Left D")
    INSERT INTO hash_join_left VALUES (NULL, "Left Null")

    INSERT INTO hash_join_right VALUES (1, "Right A", .T.)
    INSERT INTO hash_join_right VALUES (1, "Right inactive", .F.)
    INSERT INTO hash_join_right VALUES (1, "Right B", .T.)
    INSERT INTO hash_join_right VALUES (2, "Right C", .T.)
    INSERT INTO hash_join_right VALUES (3, "Right D", .T.)
    INSERT INTO hash_join_right VALUES (NULL, "Right Null", .T.)

    // Duplicate keys must produce every matching pair, while residual ON and WHERE
    // predicates still run after the hash lookup.
    LOCAL hashJoinsBefore := SqlOptimizationCount("HashJoins") AS INT
    SELECT lhs.LeftName, rhs.RightName ;
        FROM hash_join_left lhs ;
        INNER JOIN hash_join_right rhs ON lhs.JoinId = rhs.JoinId AND rhs.Active = .T. ;
        WHERE lhs.JoinId <= 2 ;
        ORDER BY lhs.LeftName, rhs.RightName
    VerifySqlOptimization("HashJoins", hashJoinsBefore)
    VerifyTable(5, "Failed: Hash join lost duplicate matches or residual predicates", TRUE, ;
        { {"Left A", "Right A"}, ;
          {"Left A", "Right B"}, ;
          {"Left B", "Right A"}, ;
          {"Left B", "Right B"}, ;
          {"Left C", "Right C"} } )

    SELECT DISTINCT lhs.JoinId ;
        FROM hash_join_left lhs ;
        JOIN hash_join_right rhs ON rhs.JoinId = lhs.JoinId AND rhs.Active = .T. ;
        ORDER BY lhs.JoinId DESC
    VerifyTable(2, "Failed: DISTINCT should apply after hash join matches", TRUE, ;
        { {2}, {1} } )

    SELECT TOP 2 lhs.LeftName, rhs.RightName ;
        FROM hash_join_left lhs ;
        JOIN hash_join_right rhs ON lhs.JoinId = rhs.JoinId AND rhs.Active = .T. ;
        ORDER BY lhs.LeftName DESC, rhs.RightName DESC INTO CURSOR HashJoinCursor
    VerifyTable(2, "Failed: TOP/ORDER BY/INTO CURSOR failed with hash join", FALSE, ;
        { {"Left C", "Right C"}, {"Left B", "Right B"} } )
    USE IN HashJoinCursor

    // A non-equality ON predicate must retain the nested-loop fallback.
    SELECT lhs.LeftName, rhs.RightName ;
        FROM hash_join_left lhs ;
        JOIN hash_join_right rhs ON lhs.JoinId < rhs.JoinId ;
        WHERE lhs.JoinId <= 2
    VerifyTable(5, "Failed: Nested-loop JOIN candidates changed result rows", TRUE)

    USE IN hash_join_left
    USE IN hash_join_right

    CREATE CURSOR hash_fraction_left (Id N(8,2), TextKey C(4))
    CREATE CURSOR hash_fraction_right (Id N(10,3), TextKey C(4))
    INSERT INTO hash_fraction_left VALUES (1.25, "one")
    INSERT INTO hash_fraction_left VALUES (-2.50, "two")
    INSERT INTO hash_fraction_right VALUES (1.25, "one")
    INSERT INTO hash_fraction_right VALUES (-2.50, "two")
    hashJoinsBefore := SqlOptimizationCount("HashJoins")
    SELECT a.Id FROM hash_fraction_left a JOIN hash_fraction_right b ON a.Id = b.Id
    VerifySqlOptimization("HashJoins", hashJoinsBefore)
    VerifyTable(2, "Fractional numeric keys should match across field widths", TRUE, {{1.25}, {-2.50}})

    hashJoinsBefore := SqlOptimizationCount("HashJoins")
    SELECT a.TextKey FROM hash_fraction_left a JOIN hash_fraction_right b ON a.TextKey = b.TextKey
    IF SqlOptimizationCount("HashJoins") != hashJoinsBefore
        THROW Exception{"Character join unexpectedly used hash optimization"}
    ENDIF
    VerifyTable(2, "Character join fallback should preserve matches", TRUE)
    USE IN hash_fraction_left
    USE IN hash_fraction_right
    RETURN

FUNCTION TestLeftOuterJoin() AS VOID
    CREATE CURSOR outer_left (Id I, LeftName C(20))
    CREATE CURSOR outer_right (Id I, RightName C(20), Active L)
    CREATE CURSOR outer_empty (Id I, RightName C(20))
    CREATE CURSOR outer_no_left (Id I, LeftName C(20))
    CREATE CURSOR outer_detail (Id I, DetailName C(20))
    CREATE CURSOR outer_inner (Id I, InnerName C(20))

    INSERT INTO outer_left VALUES (1, "left one")
    INSERT INTO outer_left VALUES (2, "left two")
    INSERT INTO outer_left VALUES (3, "left three")
    INSERT INTO outer_right VALUES (1, "right one a", .T.)
    INSERT INTO outer_right VALUES (1, "right one b", .T.)
    INSERT INTO outer_right VALUES (2, "right two inactive", .F.)
    INSERT INTO outer_detail VALUES (1, "detail one")
    INSERT INTO outer_detail VALUES (2, "detail two")
    INSERT INTO outer_inner VALUES (1, "inner one")
    INSERT INTO outer_inner VALUES (3, "inner three")

    // ON filtering must not discard preserved left rows. Matching right duplicates remain distinct.
    SELECT l.Id, r.RightName ;
        FROM outer_left l ;
        LEFT OUTER JOIN outer_right r ON l.Id = r.Id AND r.Active = .T. ;
        ORDER BY l.Id, r.RightName
    VerifyTable(4, "LEFT JOIN should preserve unmatched rows and matching duplicates", TRUE, ;
        { {1, "right one a"}, {1, "right one b"}, {2, NULL}, {3, NULL} } )

    // A WHERE predicate on the nullable side applies after NULL extension.
    SELECT l.Id, r.RightName ;
        FROM outer_left l ;
        LEFT JOIN outer_right r ON l.Id = r.Id ;
        WHERE r.Active = .T. ;
        ORDER BY l.Id, r.RightName
    VerifyTable(2, "WHERE on the nullable side should filter unmatched rows", TRUE, ;
        { {1, "right one a"}, {1, "right one b"} } )

    // The internal NULL-extension row is not a real ON candidate when ON is true for every input pair.
    SELECT l.Id, r.RightName ;
        FROM outer_left l ;
        LEFT JOIN outer_right r ON l.Id > 0
    VerifyTable(9, "LEFT JOIN should not expose its synthetic row when real matches exist", TRUE)

    // An empty right input still yields one NULL-extended row per left row.
    SELECT l.Id, e.RightName ;
        FROM outer_left l ;
        LEFT JOIN outer_empty e ON l.Id = e.Id ;
        ORDER BY l.Id
    VerifyTable(3, "LEFT JOIN to an empty table should preserve every left row", TRUE, ;
        { {1, NULL}, {2, NULL}, {3, NULL} } )

    SELECT l.Id, e.RightName ;
        FROM outer_no_left l ;
        LEFT JOIN outer_empty e ON l.Id = e.Id
    VerifyTable(0, "LEFT JOIN with an empty preserved side should return no rows", TRUE)

    // Chained outer joins retain the accumulated left side at each stage.
    SELECT l.Id, r.RightName, d.DetailName ;
        FROM outer_left l ;
        LEFT JOIN outer_right r ON l.Id = r.Id AND r.Active = .T. ;
        LEFT OUTER JOIN outer_detail d ON l.Id = d.Id ;
        ORDER BY l.Id, r.RightName
    VerifyTable(4, "Chained LEFT JOIN should preserve NULL-extended combinations", TRUE, ;
        { {1, "right one a", "detail one"}, ;
          {1, "right one b", "detail one"}, ;
          {2, NULL, "detail two"}, ;
          {3, NULL, NULL} } )

    SELECT l.Id, r.RightName, d.DetailName ;
        FROM outer_left l ;
        LEFT JOIN outer_right r ON l.Id = r.Id AND r.Active = .T. ;
        CROSS JOIN outer_detail d
    VerifyTable(8, "CROSS JOIN after LEFT JOIN should retain all produced combinations", TRUE)

    // An INNER JOIN after LEFT JOIN filters rows only at its own stage.
    SELECT l.Id, r.RightName, i.InnerName ;
        FROM outer_left l ;
        LEFT JOIN outer_right r ON l.Id = r.Id AND r.Active = .T. ;
        INNER JOIN outer_inner i ON l.Id = i.Id ;
        ORDER BY l.Id, r.RightName
    VerifyTable(3, "LEFT JOIN followed by INNER JOIN should preserve join order semantics", TRUE, ;
        { {1, "right one a", "inner one"}, ;
          {1, "right one b", "inner one"}, ;
          {3, NULL, "inner three"} } )

    USE IN outer_left
    USE IN outer_right
    USE IN outer_empty
    USE IN outer_no_left
    USE IN outer_detail
    USE IN outer_inner
    RETURN

