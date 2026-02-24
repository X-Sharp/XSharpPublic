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
USING XSharp.MacroCompiler

#include "foxcmd.xh"
#command __NOFOXPROSQL__ <any> => #error This Embedded SQL command is not (yet) supported: <(any)>
// SQL Delete, mapped to normal delete
#command DELETE [<*clauses*>] WHERE [<*moreclauses*>] => DELETE <clauses> FOR <moreclauses>
#command DELETE <*target*> FROM <(a)> [<*clauses*>] => DELETE <clauses> IN <target>
#command DELETE FROM <(a)> [<*clauses*>] => DELETE <clauses> IN <a>
#command DELETE [<*clauses*>] <s:SELECT,JOIN> <*moreclauses*> => __NOFOXPROSQL__ <(udc)>
// Subselect is not supported

// comments:
// insert into <table> opens the table when it is not already open
// select * from <table> opens the table when it is not already open

GLOBAL AllTests := List<TestInfo>{} AS List<TestInfo>
GLOBAL Verbose := FALSE AS LOGIC

CLASS TestInfo
    PUBLIC Name AS STRING
    PUBLIC TestFunc AS Action
    PUBLIC Result AS STRING

    CONSTRUCTOR(name AS STRING, testFunc AS Action)
        ::Name := name
        ::TestFunc := testFunc
        ::Result := "NotRun"
END CLASS

FUNCTION RegisterTest(name AS STRING, testFunc AS Action) AS VOID
    AllTests:Add(TestInfo{name, testFunc})
    RETURN

FUNCTION SetVerbose(v AS LOGIC) AS VOID
    Verbose := v
    RETURN

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
    TRY
        ?
        ? "Starting test:", test:Name
        test:TestFunc()
        test:Result := "Passed"
    CATCH e AS Exception
        test:Result := "Failed: " + e:ToString()
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

    RegisterTest("TestCreate", TestCreate)
    RegisterTest("TestSqlParser", TestSqlParser)
    RegisterTest("TestFieldResolution", TestFieldResolution)
    RegisterTest("TestTableResolution", TestTableResolution)
    RegisterTest("TestNewSqlFeatures", TestNewSqlFeatures)
    RegisterTest("TestSelectFunctionality", TestSelectFunctionality)
    RegisterTest("TestQueryOptimizer", TestQueryOptimizer)

    LOCAL results := RunTests()

    ? ""
    ? "=== Test Summary ==="
    ? "Passed:", results:Item1
    ? "Failed:", results:Item2

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
            OrderCode c(10), ;
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
    selectSql := "SELECT DISTINCT TOP 10 CustomerID, CompanyName, ContactName FROM Customers WHERE City = 'New York' ORDER BY CompanyName"

    VAR lexer := XSqlLexer{selectSql}
    VAR tokens := lexer:AllTokens()
    VAR parser := SqlParser{XTokenList{tokens}}
    LOCAL selectCtx AS FoxSelectContext

    IF parser:ParseSelectStatement(OUT selectCtx)
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

FUNCTION VerifyTable(expectedRecords AS DWORD, message AS STRING, closeTable := FALSE AS LOGIC) AS VOID
    VAR  actualRecords := RecCount()
    PrintTable()
    IF expectedRecords != actualRecords
        IF closeTable
            DbCloseArea()
        ENDIF
        THROW Exception{message + " (Expected records: " + expectedRecords:ToString() + ", Got: " + actualRecords:ToString() + ")"}
    ENDIF
    IF closeTable
        DbCloseArea()
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
    VerifyTable(4, "Failed: Expected 4 records from SELECT *", TRUE)

    // Test SELECT with WHERE
    IF Verbose
        ? "Attempting SELECT with WHERE..."
    ENDIF

    SELECT * FROM test_select_table WHERE Age > 25
    VerifyTable(3, "Failed: Expected 3 records from SELECT with WHERE", TRUE)

    // Test SELECT with specific columns
    IF Verbose
        ? "Attempting SELECT with specific columns..."
    ENDIF

    SELECT Name, Age FROM test_select_table WHERE Age > 25
    VerifyTable(3, "Failed: Expected 3 records from SELECT specific columns", TRUE)

// Test SELECT with DISTINCT
    IF Verbose
        ? "Attempting SELECT with DISTINCT..."
    ENDIF

    SELECT DISTINCT Age FROM test_select_table
    VerifyTable(3, "Failed: Expected 3 distinct ages", TRUE)

    // Test SELECT with TOP
    IF Verbose
        ? "Attempting SELECT with TOP..."
    ENDIF

    SELECT TOP 2 * FROM test_select_table
    VerifyTable(2, "Failed: Expected 2 records from SELECT TOP 2", TRUE)

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

    // Test cross join (Cartesian product)
    IF Verbose
        ? "Attempting cross join (SELECT from multiple tables)..."
    ENDIF

    SELECT test_table_a.Name, test_table_b.Desc FROM test_table_a, test_table_b
    VerifyTable(6, "Failed: Expected 6 records from cross join", TRUE)

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
    VerifyTable(2, "Failed: Expected 2 records from field aliasing", TRUE)

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
    VerifyTable(1, "Failed: Expected 1 record from conflict resolution", TRUE)

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
    VerifyTable(3, "Failed: Expected 3 records in MyTestCursor", TRUE)

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
    VerifyTable(3, "Failed: Expected 3 records in AliasedCursor", TRUE)

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

    // Test CROSS JOIN functionality
    TestCrossJoinFunctionality()

    // Close the test table
    USE IN test_select_table

    RETURN

FUNCTION TestCrossJoinFunctionality() AS VOID
    ? "Testing CROSS JOIN functionality..."

    // Create test tables for CROSS JOIN
    CREATE CURSOR cross_join_table_a (Id N(5), Name C(20))
    CREATE CURSOR cross_join_table_b (Id N(5), Desc C(30))

    IF !Used("cross_join_table_a") .OR. !Used("cross_join_table_b")
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
    VerifyTable(6, "Failed: Expected 6 records from explicit CROSS JOIN", TRUE)

    // Test CROSS JOIN with aliases
    IF Verbose
        ? "Testing CROSS JOIN with aliases..."
    ENDIF

    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b
    VerifyTable(6, "Failed: Expected 6 records from CROSS JOIN with aliases", TRUE)

    // Test CROSS JOIN with WHERE clause
    IF Verbose
        ? "Testing CROSS JOIN with WHERE clause..."
    ENDIF

    SELECT a.Name, b.Desc ;
        FROM cross_join_table_a a ;
        CROSS JOIN cross_join_table_b b ;
        WHERE a.Id = 1
    VerifyTable(3, "Failed: Expected 3 records from CROSS JOIN with WHERE", TRUE)

    // Close the test tables
    USE IN cross_join_table_a
    USE IN cross_join_table_b

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

FUNCTION OpenArea(sTable as STRING) AS LOGIC
    IF ! DbSelectArea(sTable)
        DbUseArea(TRUE, "DBFVFP", sTable, sTable, TRUE, FALSE)
    ENDIF
    RETURN Used()

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

    RETURN
