// Program.prg
// Created by    : robert
// Creation Date : 11/8/2023 5:05:33 PM
// Created for   :
// WorkStation   : NYX

using System.Diagnostics
USING SYstem
USING SYstem.Collections.Generic
USING SYstem.Linq
USING SYstem.Text
USING XSharp.Parsers
USING XSharp.RDD.Enums
USING XSharp.Data
USING XSharp.Data.Query
USING XSharp.VFP

#include "foxcmd.xh"

// comments:
// insert into <table> opens the table when it is not already open
// select * from <table> opens the table when it is not already open

GLOBAL AllTests := List<TestInfo>{} AS List<TestInfo>
GLOBAL Verbose := FALSE AS LOGIC

CLASS TestInfo
    PUBLIC Name AS STRING
    PUBLIC TestFunc AS Action
    PUBLIC Result AS STRING
    PUBLIC DisableSqlOptimizations AS LOGIC

    CONSTRUCTOR(name AS STRING, testFunc AS Action, disableSqlOptimizations := FALSE AS LOGIC)
        ::Name := name
        ::TestFunc := testFunc
        ::Result := "NotRun"
        ::DisableSqlOptimizations := disableSqlOptimizations
END CLASS

FUNCTION RegisterTest(name AS STRING, testFunc AS Action, disableSqlOptimizations := FALSE AS LOGIC) AS VOID
    AllTests:Add(TestInfo{name, testFunc, disableSqlOptimizations})
    RETURN
FUNCTION SetVerbose(v AS LOGIC) AS VOID
    Verbose := v
FUNCTION RunTests(tests := NULL AS List<TestInfo>) AS TUPLE(INT, INT)
    LOCAL passed := 0 AS INT
    LOCAL failed := 0 AS INT

    IF tests == NULL
        tests := AllTests
    ENDIF

    FOREACH VAR test IN tests
        TestRunner(test)
        IF test:Result == "Passed"
            passed++
        ELSE
            failed++
        ENDIF
    NEXT

    RETURN TUPLE(passed, failed)

FUNCTION TestRunner(test AS TestInfo) AS VOID
    LOCAL switchName := "XSharp.VFP.EmbeddedSql.DisableOptimizations" AS STRING
    LOCAL previousSwitch := FALSE AS LOGIC
    LOCAL switchWasSet := AppContext.TryGetSwitch(switchName, OUT previousSwitch) AS LOGIC
    AppContext.SetSwitch(switchName, test:DisableSqlOptimizations)
    TRY
        TRY
            ?
            ? "Starting test:", test:Name
            test:TestFunc()
            test:Result := "Passed"
        CATCH e AS Exception
            test:Result := "Failed: " + e:ToString()
        END TRY
    FINALLY
        AppContext.SetSwitch(switchName, IIF(switchWasSet, previousSwitch, FALSE))
    END

    IF test:Result != "Passed" .OR. Verbose
        ? "Result:", test:Result
    ENDIF

    RETURN

FUNCTION Start(args AS STRING[]) AS VOID STRICT
    // Check for /verbose option
    FOREACH VAR arg IN args
        arg = Upper(arg):Replace(c'-', c'/')
        IF arg == "/VERBOSE" .OR. arg == "/V"
            SetVerbose(TRUE)
        ENDIF
    NEXT

    RegisterTest("TestCreate", { => TestCreate() })
    RegisterTest("TestSqlParser", { => TestSqlParser() })
    RegisterTest("TestValueComparison", { => TestValueComparison() })
    RegisterTest("TestFieldResolution", { => TestFieldResolution() })
    RegisterTest("TestTableResolution", { => TestTableResolution() })
    RegisterTest("TestNewSqlFeatures", { => TestNewSqlFeatures() })
    RegisterTest("TestSelectFunctionality [optimized]", { => TestSelectFunctionality() })
    RegisterTest("TestSelectFunctionality [baseline]", { => TestSelectFunctionality() }, TRUE)
    RegisterTest("TestCrossJoinFunctionality [optimized]", { => TestCrossJoinFunctionality() })
    RegisterTest("TestCrossJoinFunctionality [baseline]", { => TestCrossJoinFunctionality() }, TRUE)
    RegisterTest("TestDeletedCandidateRows [optimized]", { => TestDeletedCandidateRows() })
    RegisterTest("TestDeletedCandidateRows [baseline]", { => TestDeletedCandidateRows() }, TRUE)
    RegisterTest("TestHashJoin [optimized]", { => TestHashJoin() })
    RegisterTest("TestHashJoin [baseline]", { => TestHashJoin() }, TRUE)
    RegisterTest("TestQueryOptimizer", { => TestQueryOptimizer() })
    RegisterTest("TestOptimizerBitmap", { => TestOptimizerBitmap() })
    RegisterTest("TestMultiTableOptimizer [optimized]", { => TestMultiTableOptimizer() })
    RegisterTest("TestMultiTableOptimizer [baseline]", { => TestMultiTableOptimizer() }, TRUE)
    RegisterTest("TestSparseOptimizationBenchmark", { => TestSparseOptimizationBenchmark() })

    LOCAL results := RunTests()

    ? ""
    ? "=== Test Summary ==="
    ? "Passed:", results:Item1
    ? "Failed:", results:Item2

    Environment.ExitCode := IIF(results.Item2 > 0, 1, 0)

    IF results.Item2 > 0
        ? ""
        ? "=== Failed Tests ==="
        FOREACH VAR test IN AllTests
            IF test:Result != "Passed"
                ? test:Name + ": " + test:Result
            ENDIF
        NEXT
    ENDIF

    RETURN

FUNCTION TestCreate() AS VOID
    local a,b,c as int
    a := b := c := 42
    CREATE TABLE Salesman ;
        (SalesID c(6) PRIMARY KEY, ;
        SaleName Character(20) )

    IF Used()
        INSERT INTO Salesman VALUES ("1", "Robert")
        INSERT INTO Salesman VALUES ("2", "Fabrice")
        INSERT INTO Salesman VALUES ("3", "Chris")
        INSERT INTO Salesman VALUES ("4", "Nikos")

        // Verify data was inserted
        GO TOP
        IF Alias() != "SALESMAN"
            THROW Exception{"Failed: Expected alias SALESMAN"}
        ENDIF
        VerifyTable(4, "Failed: Expected 4 records in Salesman table", TRUE)
    ELSE
        THROW Exception{"Failed: Table not open"}
    ENDIF

    CREATE CURSOR employee ;
        (EmpID N(5), Name Character(20), DOB D, Address c( 30 ) , City c(30), ;
        PostalCode c(10), OfficeNo c(8) NULL, Salary Numeric(10 , 2 ),  Specialty Memo)

    IF Used()
        INSERT INTO Employee(EmpId, Name) VALUES (1, "Nikos")
        Local Address as usual
        m.Address = "Zuukerweg"
        m.EmpId = 10
        m.Name  = "Robert"
        INSERT INTO employee FROM Memvar

        LOCAL obj as object
        OBJ = CREATEOBJECT("Empty")
        = AddProperty(obj,"EmpId", 42)
        = AddProperty(obj,"Name", "Fabrice")
        INSERT INTO employee FROM NAME obj

        Dimension values[3]
        values[1] = 100
        values[2] = "Chris"
        values[3] = ToDay()
        INSERT INTO employee FROM ARRAY values

        // Verify data
        GO TOP
        IF Alias() != "EMPLOYEE"
            THROW Exception{"Failed: Expected alias EMPLOYEE"}
        ENDIF
        VerifyTable(4, "Failed: Expected 4 records in Employee table", TRUE)
    ELSE
        THROW Exception{"Failed: Table not open"}
    ENDIF

    RETURN

FUNCTION TestDelete() AS VOID
/*
        DELETE FROM employee where _FIELD->EmpID = 1

        //DELETE FROM Database!employee where EmpID = 1

        DELETE  MyProducts FROM MSRPList; 
            WHERE MSRPList.ProdID = MyProducts.ProdID;
            AND MSRPList.discontinued = .t.
*/

    //         DELETE  DB!MyProducts FROM DB!MSRPList ;
    //            WHERE MSRPList.ProdID = MyProducts.ProdID;
    //             AND MSRPList.discontinued = .t.

    /*
        UPDATE MyProducts SET MSRP=MyUpdates.MSRP FROM MyUpdates WHERE MyProducts.ProdID=MyUpdates.ProdID
        UPDATE products ;
            SET unitprice = ;
            (SELECT (msrp*.90) ;
            FROM mfg_msrp ;
            WHERE mfg_msrp.productID = products.productID ;
            AND mfg_msrp.discontinued = .f.)

        UPDATE products ;
            SET unitprice = mfg_msrp.msrp*.90 ;
            FROM mfg_msrp ;
            WHERE mfg_msrp.productID = products.productID;
            AND mfg_msrp.discontinued = .f.
*/

FUNCTION TestCreateTables()
        CREATE TABLE Customers ;
            (CustId i PRIMARY KEY, ;
            CustName c(20) )
        INSERT INTO Customers(CustId,CustName) VALUES (1, "Microsoft")
        INSERT INTO Customers(CustId,CustName) VALUES (2, "Google")

        IF Verbose
            ? ALIAS()
        ENDIF

        CREATE TABLE Orders ;
            (OrderId i PRIMARY KEY, ;
            CustId i REFERENCES customer TAG CustId, ;
            CustName c(10), ;
            OrderAmt y(4), ;
            OrderQty i ;
            DEFAULT 10 ;
            CHECK (OrderQty > 9) ;
            ERROR "Order Quantity must be at least 10", ;
            DiscPercent n(6,2) NULL ;
            DEFAULT .NULL., ;
            CHECK (OrderAmt > 0) ERROR "Order Amount must be > 0" )
        local oTest := XSharp.VFP.Empty{}
        AddProperty(oTest, "LastName", "Hulst")
        AddProperty(oTest, "FirstName", "Robert")
        INSERT INTO Orders(CustId,OrderAmt, OrderQty,OrderCode) VALUES (1, 100, 10,"19010101")
        INSERT INTO Orders(CustId,OrderAmt, OrderQty,OrderCode) VALUES (1, 200, 20,"19010202")
        INSERT INTO Orders(CustId,OrderAmt, OrderQty,OrderCode) VALUES (2, 200, 20,"19010202")

        IF Verbose
            ? ALIAS()
        ENDIF

        UPDATE Orders ;
            SET OrderAmt = OrderAmt*1.1 ;
            FROM Customers ;
            WHERE Orders->CustId = Customers->CustId AND Customers->CustName = "Microsoft"

        IF Verbose
            Browse()
        ENDIF

        ALTER TABLE Orders Add OrderDate Date NULL

        IF Verbose
            Browse()
        ENDIF

        ALTER TABLE Orders Add COLUMN DeliveryDate DateTime

        IF Verbose
            Browse()
        ENDIF

        ALTER TABLE Orders DROP COLUMN OrderAmt

        IF Verbose
            Browse()
        ENDIF

        ALTER TABLE Orders Alter COLUMN OrderCode C(5)

        IF Verbose
            Browse()
        ENDIF

//         CREATE SQL VIEW MyView AS SELECT * FROM Northwind!Customers;
//             WHERE Country="Mexico

FUNCTION TestNewSqlFeatures() AS VOID
    ? "Testing new SQL features..."

    // Test SELECT parsing
    TestSelectParsing()

    // Test INSERT parsing
    TestInsertParsing()

    // Test SELECT UDC functionality
    TestSelectUDC()

    RETURN

FUNCTION TestSelectUDC() AS VOID
    LOCAL result := "Passed" AS STRING

    ? "Testing SELECT UDC functionality..."

    // This would test the actual UDC functionality if the table existed
    // For now, we'll just show what would happen
    IF Verbose
        ? "SELECT UDCs have been defined in foxcmd.xh"
        ? "- Basic SELECT: SELECT field1, field2 FROM table WHERE condition"
        ? "- SELECT with DISTINCT: SELECT DISTINCT field1 FROM table"
        ? "- SELECT with TOP: SELECT TOP 10 field1 FROM table"
        ? "These UDCs map to the __SqlSelect function"
    ENDIF

    RETURN

FUNCTION TestSelectParsing() AS VOID
    ? "Testing SELECT parsing..."

    LOCAL selectSql AS STRING
    selectSql := "SELECT DISTINCT TOP 10 CustomerID, CompanyName, ContactName FROM Customers WHERE City = 'New York' ORDER BY CompanyName DESC, CustomerID ASC"

    VAR lexer := XSqlLexer{selectSql}
    VAR tokens := lexer:AllTokens()
    VAR parser := SqlParser{XTokenList{tokens}}
    LOCAL selectCtx AS FoxSelectContext

    IF parser:ParseSelectStatement(OUT selectCtx)
        IF selectCtx:OrderByClause:Count != 2 .OR. ;
           !selectCtx:OrderByClause[0]:Descending .OR. ;
           selectCtx:OrderByClause[1]:Descending
            THROW Exception{"ORDER BY should preserve both keys and their directions"}
        ENDIF
        IF Verbose
            ? "Successfully parsed SELECT statement"
            ? "Top Count:", selectCtx:TopCount
            ? "Is Distinct:", selectCtx:IsDistinct
            ? "Select List Count:", selectCtx:SelectList:Count
            ? "Table List Count:", selectCtx:TableList:Count
            ? "Where Clause:", selectCtx:WhereClause
            ? "Order By Clause:", selectCtx:OrderByClause

            FOREACH VAR col IN selectCtx:SelectList
                ? "  Column:", col:ToString()
            NEXT
        ENDIF
    ELSE
        THROW Exception{ "Failed to parse SELECT statement: " + parser:Error }
    ENDIF

    VAR joinLexer := XSqlLexer{"SELECT a.Id FROM TableA a JOIN TableB b ON a.Id = b.Id INNER JOIN TableC c ON b.Id = c.Id"}
    VAR joinParser := SqlParser{XTokenList{joinLexer:AllTokens()}}
    LOCAL joinCtx AS FoxSelectContext
    IF !joinParser:ParseSelectStatement(OUT joinCtx) .OR. joinCtx:Joins:Count != 2
        THROW Exception{"Failed to parse chained INNER JOIN metadata: " + joinParser:Error}
    ENDIF

    VAR aggregateLexer := XSqlLexer{"SELECT Region, SUM(Amount) / COUNT(*) + Tax AS Adjusted FROM Sales GROUP BY Region HAVING SUM(Amount) > 0"}
    VAR aggregateParser := SQLParser{XTokenList{aggregateLexer:AllTokens()}}
    LOCAL aggregateCtx AS FoxSelectContext
    IF !aggregateParser:ParseSelectStatement(OUT aggregateCtx)
        THROW Exception{"Failed to parse composed aggregate expression: " + aggregateParser:Error}
    ENDIF
    IF aggregateCtx:SelectList:Count != 2 .OR. aggregateCtx:GroupByItems:Count != 1
        THROW Exception{"Aggregate SELECT should preserve select and grouping items"}
    ENDIF
    IF !aggregateCtx:SelectList[1]:ToString():Contains("SUM(Amount)") .OR. ;
       !aggregateCtx:SelectList[1]:ToString():Contains("COUNT(*)")
        THROW Exception{"Aggregate expression should preserve COUNT and SUM calls"}
    ENDIF

    VAR outerJoinLexer := XSqlLexer{"SELECT a.Id FROM TableA a LEFT OUTER JOIN TableB b ON a.Id = b.Id"}
    VAR outerJoinParser := SqlParser{XTokenList{outerJoinLexer:AllTokens()}}
    LOCAL outerJoinCtx AS FoxSelectContext
    IF outerJoinParser:ParseSelectStatement(OUT outerJoinCtx)
        THROW Exception{"LEFT OUTER JOIN should be rejected until outer join execution is supported"}
    ENDIF

    VAR missingOnLexer := XSqlLexer{"SELECT a.Id FROM TableA a JOIN TableB b"}
    VAR missingOnParser := SqlParser{XTokenList{missingOnLexer:AllTokens()}}
    LOCAL missingOnCtx AS FoxSelectContext
    IF missingOnParser:ParseSelectStatement(OUT missingOnCtx)
        THROW Exception{"JOIN without ON should be rejected"}
    ENDIF

    RETURN

FUNCTION TestInsertParsing() AS VOID
    ? "Testing INSERT parsing..."

    LOCAL insertSql AS STRING
    insertSql := "INSERT INTO Employees (FirstName, LastName, Salary) VALUES ('John', 'Doe', 50000)"

    VAR lexer := XSqlLexer{insertSql}
    VAR tokens := lexer:AllTokens()
    VAR parser := SqlParser{XTokenList{tokens}}
    LOCAL insertCtx AS FoxInsertContext

    IF !parser:ParseInsertStatement(OUT insertCtx)
        THROW Exception{"Failed to parse INSERT statement: " + parser:Error}
    ENDIF

    IF Verbose
        ? "Successfully parsed INSERT statement"
        ? "Table Name:", insertCtx:TableName
        ? "Column List Count:", insertCtx:ColumnList:Count
        ? "Value List Count:", insertCtx:ValueList:Count

        FOREACH VAR col IN insertCtx:ColumnList
            ? "  Column:", col
        NEXT

        FOREACH VAR val IN insertCtx:ValueList
            ? "  Value:", val:ToString()
        NEXT
    ENDIF

    RETURN

FUNCTION TestFieldResolution() AS VOID
    // Test the field resolution functionality
    VAR sqlText := "SELECT field1, table2.field2 FROM table1, table2 WHERE table1.id = table2.id"
    VAR lexer := XSqlLexer{sqlText}
    VAR tokens := lexer:AllTokens()
    VAR parser := SQLParser{XTokenList{tokens}}
    VAR selectCtx := FoxSelectContext{}

    CREATE CURSOR table1 (ID N(5), field1 C(20))
    CREATE CURSOR table2 (ID N(5), field2 C(20))

    IF !parser:ParseSelectStatement(out selectCtx)
        THROW Exception{"Failed to parse: " + parser:Error}
    ENDIF

    IF Verbose
        ? "Successfully parsed SELECT statement"

        ? "Original SELECT expressions:"
        FOR VAR i := 0 TO selectCtx:SelectList:Count - 1
            ? i"  {selectCtx:SelectList[i]:ToString()}"
        NEXT

        IF selectCtx:WhereClause != NULL
            ? i"Original WHERE clause: {selectCtx:WhereClause:ToString()}"
        ENDIF

        ? "Available tables:"
        FOREACH VAR table IN selectCtx:TableList
            ? i"  {table}"
        NEXT

        ? "Resolved SELECT expressions:"
        FOR VAR i := 0 TO selectCtx:SelectList:Count - 1
            ? "  ", selectCtx:SelectList[i]:ToResolvedString(selectCtx:TableAliases)
        NEXT

        IF selectCtx:WhereClause != NULL
            ? i"Resolved WHERE clause:", selectCtx:WhereClause:ToResolvedString(selectCtx:TableAliases)
        ENDIF
    ENDIF

    USE IN table1
    USE IN table2

    RETURN

FUNCTION TestTableResolution() AS VOID
    // Test the table resolution mechanism
    LOCAL sql AS STRING
    sql := "SELECT Customers.Name, Orders.Total FROM Customers, Orders WHERE Customers.Id = Orders.CustomerId"
    VAR lexer := XSqlLexer{sql}
    VAR tokens := lexer:AllTokens()
    VAR parser := SQLParser{XTokenList{tokens}}
    VAR selectCtx := FoxSelectContext{}

    CREATE CURSOR customers (ID N(5), Name c(20))
    CREATE CURSOR orders (Id N(5), CustomerId N(5), TotalVal C(20))

    IF !parser:ParseSelectStatement(out selectCtx)
        THROW Exception{"Failed to parse: " + parser:Error}
    ENDIF

    IF Verbose
        ? "Successfully parsed SELECT statement"
        ? "Tables in FROM clause: " + String.Join(", ", selectCtx:TableList)

        // Test individual expression dependencies
        FOR LOCAL i := 0 AS INT TO selectCtx:SelectList:Count - 1
            VAR expr := selectCtx:SelectList[i]
            VAR exprDeps := expr:GetTableDependencies(selectCtx:TableAliases)
            ? "Expression " + i:ToString() + " (" + expr:ToString() + ") depends on tables: " + String.Join(", ", exprDeps)
        NEXT

        // Test WHERE clause dependencies
        IF selectCtx:WhereClause != NULL
            VAR whereDeps := selectCtx:WhereClause:GetTableDependencies(selectCtx:TableAliases)
            ? "WHERE clause depends on tables: " + String.Join(", ", whereDeps)
        ENDIF
    ENDIF

    // Verify parsing succeeded
    IF selectCtx:TableList:Count == 0 .OR. selectCtx:SelectList:Count == 0
        THROW Exception{"Failed: Parsing did not produce expected results"}
    ENDIF

    lexer := XSqlLexer{"SELECT CustomerId, ProductId FROM Sales GROUP BY CustomerId, ProductId HAVING CustomerId > 0"}
    parser := SQLParser{XTokenList{lexer:AllTokens()}}
    selectCtx := FoxSelectContext{}
    IF !parser:ParseSelectStatement(OUT selectCtx)
        THROW Exception{"Failed to parse multi-key GROUP BY: " + parser:Error}
    ENDIF
    IF selectCtx:GroupByItems:Count != 2 .OR. selectCtx:GroupByClause != selectCtx:GroupByItems[0]
        THROW Exception{"GROUP BY should preserve all keys and the legacy first-key property"}
    ENDIF
    IF selectCtx:HavingClause == NULL
        THROW Exception{"HAVING should be parsed after a multi-key GROUP BY"}
    ENDIF

    // Test another SQL statement with different constructs
    sql := "SELECT c.Name, o.Total FROM Customers c, Orders o WHERE c.Id = o.CustomerId AND o.Total > 100"

    IF Verbose
        ? ""
        ? "Parsing SQL: " + sql
    ENDIF

    lexer := XSqlLexer{sql}
    tokens := lexer:AllTokens()
    parser := SQLParser{XTokenList{tokens}}
    selectCtx := FoxSelectContext{}

    IF !parser:ParseSelectStatement(out selectCtx)
        THROW Exception{"Failed to parse: " + parser:Error}
    ENDIF

    IF Verbose
        ? "Successfully parsed SELECT statement"
        ? "Tables in FROM clause: " + String.Join(", ", selectCtx:TableList)

        // Test WHERE clause dependencies
        IF selectCtx:WhereClause != NULL
            VAR whereDeps := selectCtx:WhereClause:GetTableDependencies(selectCtx:TableAliases)
            ? "WHERE clause depends on tables: " + String.Join(", ", whereDeps)
        ENDIF
    ENDIF

    // Verify parsing succeeded with aliases
    IF selectCtx:TableList:Count == 0 .OR. selectCtx:SelectList:Count == 0
        THROW Exception{"Failed: Parsing did not produce expected results"}
    ENDIF

    USE IN customers
    USE IN orders

    RETURN

FUNCTION PrintTable() AS DWORD
    LOCAL count := 0 AS DWORD

    IF Verbose
        ? "RECORDS:"
        LOCAL resultTable AS STRING
        LOCAL fields AS ARRAY
        resultTable = DbDataSource():Name
        fields := DbStruct()
        GO TOP
        DO WHILE !EOF()
            count++
            ? "  ", "| Rec:", count:ToString(), "| "
            FOREACH VAR f IN fields
                ?? f[1]+":", &(resultTable+"->"+f[1]), "| "
            NEXT
            SKIP
        ENDDO
    ENDIF

    RETURN count

FUNCTION VerifyTable(expectedRecords AS DWORD, message AS STRING, closeTable := FALSE AS LOGIC, expectedData := NULL AS ARRAY) AS VOID
    VAR  actualRecords := RecCount()
    PrintTable()
    IF expectedRecords != actualRecords
        IF closeTable
            DbCloseArea()
        ENDIF
        THROW Exception{message + " (Expected records: " + expectedRecords:ToString() + ", Got: " + actualRecords:ToString() + ")"}
    ENDIF
    IF expectedData != NULL
        LOCAL fieldCount := FCount() AS DWORD
        GO TOP
        FOR VAR row := 1 TO expectedData:Count
            IF EOF()
                IF closeTable
                    DbCloseArea()
                ENDIF
                THROW Exception{message + " (Expected row " + row:ToString() + " but reached end of file)"}
            ENDIF
            LOCAL aRow := expectedData[row] AS ARRAY
            FOR VAR col := 1 TO aRow:Count
                IF col > fieldCount
                    IF closeTable
                        DbCloseArea()
                    ENDIF
                    THROW Exception{message + " (Row " + row:ToString() + ", Field " + col:ToString() + ": Expected field does not exist)"}
                ENDIF
                LOCAL expectedValue := aRow[col]
                LOCAL actualValue := FieldGet(col)
                IF !ValueSame(expectedValue, actualValue)
                    IF closeTable
                        DbCloseArea()
                    ENDIF
                    THROW Exception{message + " (Row " + row:ToString() + ", Field " + col:ToString() + ": Expected '" + expectedValue:ToString() + "', Got '" + actualValue:ToString() + "')"}
                ENDIF
            NEXT
            SKIP
        NEXT
        IF !EOF()
            IF closeTable
                DbCloseArea()
            ENDIF
            THROW Exception{message + " (Unexpected additional records in table)"}
        ENDIF
    ENDIF
    IF closeTable
        DbCloseArea()
    ENDIF
    RETURN

FUNCTION ValueSame(value1 AS USUAL, value2 AS USUAL) AS LOGIC
    IF value1 == NULL .OR. value2 == NULL
        RETURN value1 == value2
    ENDIF
    IF IsNumericValue(value1) .AND. IsNumericValue(value2)
        RETURN value1 == value2
    ENDIF
    // For nonnumeric values, ignore type-specific formatting and padding.
    RETURN AllTrim(value1:ToString()) == AllTrim(value2:ToString())

FUNCTION IsNumericValue(value AS USUAL) AS LOGIC
    SWITCH UsualType(value)
    CASE __UsualType.Long
    CASE __UsualType.Float
    CASE __UsualType.Fixed
    CASE __UsualType.Byte
    CASE __UsualType.ShortInt
    CASE __UsualType.Word
    CASE __UsualType.DWord
    CASE __UsualType.Real4
    CASE __UsualType.Real8
    CASE __UsualType.Int64
    CASE __UsualType.UInt64
    CASE __UsualType.Decimal
    CASE __UsualType.Currency
        RETURN TRUE
    OTHERWISE
        RETURN FALSE
    END SWITCH

FUNCTION TestValueComparison() AS VOID
    IF !ValueSame(100.50, 100.5)
        THROW Exception{"Equivalent numeric values should compare equal"}
    ENDIF
    IF ValueSame(100.50, 100.51)
        THROW Exception{"Different numeric values should not compare equal"}
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
        __SqlSelect("SELECT SUM(COUNT(Id)) FROM test_select_table")
    CATCH aggregateError AS Exception
        rejectedAggregateQuery := TRUE
    END TRY
    IF !rejectedAggregateQuery
        THROW Exception{"Nested aggregate functions should be rejected"}
    ENDIF

    rejectedAggregateQuery := FALSE
    TRY
        __SqlSelect("SELECT Name, COUNT(*) FROM test_select_table GROUP BY Age")
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
    SELECT lhs.LeftName, rhs.RightName ;
        FROM hash_join_left lhs ;
        INNER JOIN hash_join_right rhs ON lhs.JoinId = rhs.JoinId AND rhs.Active = .T. ;
        WHERE lhs.JoinId <= 2 ;
        ORDER BY lhs.LeftName, rhs.RightName
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
    RETURN

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


FUNCTION __SqlDelete (sCommand as STRING)
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}
    IF Verbose
        ? sCommand
        IF ! parser:ParseDeleteStatement(out var table)
            ? parser:Error
        ENDIF
        ? table:TableName
        foreach var t in table:TableList
            ? t
        next
        foreach var j in table:JoinList
            ? j
        next
        ? table:WhereClause
    ENDIF
    RETURN

FUNCTION __SqlUpdate (sCommand as STRING)
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}
    VAR result := parser:ParseUpdateStatement(out var table)
    IF Verbose
        ? sCommand
        IF ! result
            ? parser:Error
        ENDIF
        ? "TableName", table:TableName
        foreach var c in table:ColumnList
            ? "Column", c
        next
        foreach var t in table:ValueList
            ? "Value", t:ToString()
        next
        foreach var t in table:TableList
            ? "From Table", t
        next
        foreach var j in table:JoinList
            ? "Join Table", j
        next
        ? "Where", table:WhereClause:ToString()
    ENDIF

    VAR o := XSharp.MacroCompiler.MacroOptions.FoxPro
    o:AllowOldStyleAssignments := False
    VAR mc := XSharp.Runtime.MacroCompiler{o}
    // VAR mc := XSharp.Runtime.MacroCompiler.GetScriptCompiler(XSharpDialect.FoxPro)
    VAR cbWhere := mc:CompileCodeblock(table:WhereClause:ToString())
    //VAR res := (OBJECT) cb:Eval(args)

    VAR values := Dictionary<STRING,CODEBLOCK>{}
    FOR VAR i := 0 TO table:ColumnList:Count-1
        values[table:ColumnList[i]] := mc:CompileCodeblock(table:ValueList[i]:ToString())
    NEXT

    LOCAL FUNCTION UpdateRow() AS VOID
        FOREACH VAR item IN values
            __FieldSet(item:key, item:value:Eval())
        NEXT
    END FUNCTION

    LOCAL FUNCTION FilterRow() AS LOGIC
        FOREACH VAR t IN table:TableList
            IF (t)->DbLocate( cbWhere )
                RETURN TRUE
            ENDIF
        NEXT
        RETURN FALSE
    END FUNCTION

    (table:TableName)->(DbEval( {||DbAutoLock(), UpdateRow(), DbAutoUnLock()}, {||FilterRow()} ))

    RETURN

FUNCTION PrintContext(ctx AS XSharp.Parsers.SqlExpressionContext, depth := 0 AS INT)
    IF Verbose
        IF ctx IS SqlNameExpressionContext VAR s
            ? STRING{c" ", depth*2} + "NAME: " + s:ToString()
        ELSEIF ctx IS SqlSimpleExpressionContext VAR s
            ? STRING{c" ", depth*2} + "SIMPLE: " + s:ToString()
        ELSEIF ctx IS SqlCompositeExpressionContext VAR c
            ? STRING{c" ", depth*2} + "COMPOSITE: " + c:ToString() + " NAMES: " + c:Names:Count:ToString()
        ELSEIF ctx IS SqlParenExpressionContext VAR p
            ? STRING{c" ", depth*2} + "PAREN: "
            PrintContext(p:Expr, depth + 1)
        ELSEIF ctx IS SqlPrefixExpressionContext VAR pr
            ? STRING{c" ", depth*2} + "PREFIX: " + pr:Op:Text
            PrintContext(pr:Expr, depth + 1)
        ELSEIF ctx IS SqlLogicExpressionContext VAR l
            ? STRING{c" ", depth*2} + "LOGIC: " + l:Op:Text
            PrintContext(l:Left, depth + 1)
            PrintContext(l:Right, depth + 1)
        ELSEIF ctx IS SqlCompareExpressionContext VAR co
            ? STRING{c" ", depth*2} + "COMPARE: " + co:Op:Text
            PrintContext(co:Left, depth + 1)
            PrintContext(co:Right, depth + 1)
        ENDIF
    ENDIF

    RETURN

FUNCTION __SqlInsertFromSQL( cUdc AS STRING, cTable AS STRING, aFields AS STRING[], cSelect AS STRING ) AS VOID
    IF Verbose
        ? "Executing INSERT FROM SQL:", cUdc
        ? "Target table:", cTable
        ? "Fields:", Atoa(aFields)
        ? "Source SQL:", cSelect
    ENDIF

    // This would normally execute the SELECT statement and insert the results
    // For now, we'll just show the parsed information
    RETURN

FUNCTION Atoa(a AS ARRAY) AS STRING
    LOCAL sb AS StringBuilder
    LOCAL i AS DWORD
    sb := StringBuilder{}
    sb:Append("[")
    FOR i := 0 UPTO a:Count-1
        IF i > 0
            sb:Append(", ")
        ENDIF
        sb:Append(a[i]:ToString())
    NEXT
    sb:Append("]")
    RETURN sb:ToString()


FUNCTION TestSqlParser () AS VOID
    VAR sCommand := "xi = 1 AND (yi+1)+table.zed = 2"
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}

    IF Verbose
        ? sCommand
        VAR ctx := parser:ParseExpressionContext()
        ? ctx:ToString()
        PrintContext(ctx)
    ELSE
        // Parse silently when not verbose
        VAR ctx := parser:ParseExpressionContext()
    ENDIF

    VAR chainedAnd := ParseSqlExpressionForTest("a = 1 AND b = 2 AND c = 3")
    IF ! (chainedAnd IS SqlLogicExpressionContext)
        THROW Exception{"Parser did not return a logical expression for chained AND"}
    ENDIF
    VAR chainedAndRoot := (SqlLogicExpressionContext)chainedAnd
    IF chainedAndRoot:Op:Type != XTokenType.AND .OR. !(chainedAndRoot:Left IS SqlLogicExpressionContext)
        THROW Exception{"Parser did not consume all chained AND conditions"}
    ENDIF

    VAR chainedOr := ParseSqlExpressionForTest("a = 1 OR b = 2 OR c = 3")
    IF ! (chainedOr IS SqlLogicExpressionContext)
        THROW Exception{"Parser did not return a logical expression for chained OR"}
    ENDIF
    VAR chainedOrRoot := (SqlLogicExpressionContext)chainedOr
    IF chainedOrRoot:Op:Type != XTokenType.OR .OR. !(chainedOrRoot:Left IS SqlLogicExpressionContext)
        THROW Exception{"Parser did not consume all chained OR conditions"}
    ENDIF

    VAR mixedLogic := ParseSqlExpressionForTest("a = 1 OR b = 2 AND c = 3")
    IF ! (mixedLogic IS SqlLogicExpressionContext)
        THROW Exception{"Parser did not return a logical expression for mixed AND/OR"}
    ENDIF
    VAR mixedRoot := (SqlLogicExpressionContext)mixedLogic
    IF mixedRoot:Op:Type != XTokenType.OR .OR. !(mixedRoot:Right IS SqlLogicExpressionContext)
        THROW Exception{"AND must bind more tightly than OR"}
    ENDIF
    VAR mixedRight := (SqlLogicExpressionContext)mixedRoot:Right
    IF mixedRight:Op:Type != XTokenType.AND
        THROW Exception{"Mixed AND/OR expression has incorrect precedence"}
    ENDIF

    VAR groupedLogic := ParseSqlExpressionForTest("a = 1 AND (b = 2 OR c = 3)")
    IF ! (groupedLogic IS SqlLogicExpressionContext)
        THROW Exception{"Parser did not return a logical expression for grouped conditions"}
    ENDIF
    VAR groupedRight := (SqlLogicExpressionContext)groupedLogic
    IF !(groupedRight:Right IS SqlParenExpressionContext)
        THROW Exception{"Parser did not preserve grouped logical conditions"}
    ENDIF
    VAR groupedExpr := (SqlParenExpressionContext)groupedRight:Right
    IF !(groupedExpr:Expr IS SqlLogicExpressionContext)
        THROW Exception{"Parser did not parse the logical expression inside parentheses"}
    ENDIF
    VAR groupedInner := (SqlLogicExpressionContext)groupedExpr:Expr
    IF groupedInner:Op:Type != XTokenType.OR
        THROW Exception{"Parentheses did not preserve the inner OR expression"}
    ENDIF

    LOCAL rejectedDanglingOperator := FALSE AS LOGIC
    TRY
        ParseSqlExpressionForTest("a = 1 AND")
    CATCH e AS Exception
        rejectedDanglingOperator := TRUE
    END TRY
    IF !rejectedDanglingOperator
        THROW Exception{"Parser accepted a dangling AND operator"}
    ENDIF

    rejectedDanglingOperator := FALSE
    TRY
        ParseSqlExpressionForTest("a = 1 OR")
    CATCH e AS Exception
        rejectedDanglingOperator := TRUE
    END TRY
    IF !rejectedDanglingOperator
        THROW Exception{"Parser accepted a dangling OR operator"}
    ENDIF
RETURN

FUNCTION ParseSqlExpressionForTest(expression AS STRING) AS SqlExpressionContext
    VAR lexer := XSqlLexer{expression}
    RETURN SqlParser{XTokenList{lexer:AllTokens()}}:ParseExpressionContext()


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

FUNCTION Title(title AS STRING) AS VOID
    IF Verbose
        ? ""
        ? "===", title, "==="
    ENDIF

FUNCTION VerifyTable(expectedRecords AS DWORD, message AS STRING, closeTable := TRUE AS LOGIC) AS VOID STRICT
    LOCAL actualCount AS DWORD
    actualCount := RecCount()

    IF actualCount != expectedRecords
        ? "FAILED:", message
        ? "  Expected:", expectedRecords, "records"
        ? "  Actual:", actualCount, "records"
        THROW Exception{message + " Expected: " + expectedRecords:ToString() + " Actual: " + actualCount:ToString()}
    ENDIF

    IF Verbose
        ? "PASSED: Expected", expectedRecords, "records - Got", actualCount
    ENDIF

    IF closeTable
        DbCloseArea()
    ENDIF
RETURN


    RETURN
