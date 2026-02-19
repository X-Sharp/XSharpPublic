// Program.prg
// Created by    : robert
// Creation Date : 11/8/2023 5:05:33 PM
// Created for   :
// WorkStation   : NYX

using SYstem.Diagnostics
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

GLOBAL AllTests := List<STRING>{} AS List<STRING>
GLOBAL TestFuncs := Dictionary<STRING,Action>{} AS Dictionary<STRING,Action>

FUNCTION RegisterTest(name AS STRING, testFunc AS Action) AS VOID
    IF NOT TestFuncs:ContainsKey(name)
        TestFuncs[name] := testFunc
        AllTests:Add(name)
    ENDIF
    RETURN

FUNCTION RunTests(tests := NULL AS List<STRING>) AS VOID
    IF tests == NULL
        tests := AllTests
    ENDIF
    FOREACH VAR test IN tests
        TRY
            ?
            ? "Starting test:", test
            testFuncs[test]()
        CATCH e AS Exception
            ? "Error:", e:Message
            RETURN
        END
    NEXT
    RETURN

FUNCTION Start() AS VOID STRICT
    RegisterTest("TestCreate", TestCreate)
    RegisterTest("TestSqlParser", TestSqlParser)
    RegisterTest("TestFieldResolution", TestFieldResolution)
    RegisterTest("TestTableResolution", TestTableResolution)
    RegisterTest("TestNewSqlFeatures", TestNewSqlFeatures)
    RegisterTest("TestSelectFunctionality", TestSelectFunctionality)
    RegisterTest("TestQueryOptimizer", TestQueryOptimizer)

    RunTests()
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
        //Browse()
    ELSE
        ? "Table not open"
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

        //Browse()

    ELSE
        ? "Table not open"
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
        ? ALIAS()

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
        ? ALIAS()
        UPDATE Orders ;
            SET OrderAmt = OrderAmt*1.1 ;
            FROM Customers ;
            WHERE Orders->CustId = Customers->CustId AND Customers->CustName = "Microsoft"
        Browse()
        ALTER TABLE Orders Add OrderDate Date NULL
        Browse()
        ALTER TABLE Orders Add COLUMN DeliveryDate DateTime
        Browse()
        ALTER TABLE Orders DROP COLUMN OrderAmt
        Browse()
        ALTER TABLE Orders Alter COLUMN OrderCode C(5)
        Browse()

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
    ? "Testing SELECT UDC functionality..."

    // This would test the actual UDC functionality if the table existed
    // For now, we'll just show what would happen
    ? "SELECT UDCs have been defined in foxcmd.xh"
    ? "- Basic SELECT: SELECT field1, field2 FROM table WHERE condition"
    ? "- SELECT with DISTINCT: SELECT DISTINCT field1 FROM table"
    ? "- SELECT with TOP: SELECT TOP 10 field1 FROM table"
    ? "These UDCs map to the __SqlSelect function"
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
    ELSE
        ? "Failed to parse SELECT statement:", parser:Error
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

    IF parser:ParseInsertStatement(OUT insertCtx)
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
    ELSE
        ? "Failed to parse INSERT statement:", parser:Error
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

    IF parser:ParseSelectStatement(out selectCtx)
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
    ELSE
        ? i"Failed to parse: {parser:Error}"
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

    ? "Parsing SQL: " + sql
    IF parser:ParseSelectStatement(out selectCtx)
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
    ELSE
        ? "Failed to parse: " + parser:Error
    ENDIF

    // Test another SQL statement with different constructs
    ? ""
    sql := "SELECT c.Name, o.Total FROM Customers c, Orders o WHERE c.Id = o.CustomerId AND o.Total > 100"
    ? "Parsing SQL: " + sql
    lexer := XSqlLexer{sql}
    tokens := lexer:AllTokens()
    parser := SQLParser{XTokenList{tokens}}
    selectCtx := FoxSelectContext{}

    IF parser:ParseSelectStatement(out selectCtx)
        ? "Successfully parsed SELECT statement"
        ? "Tables in FROM clause: " + String.Join(", ", selectCtx:TableList)

        // Test WHERE clause dependencies
        IF selectCtx:WhereClause != NULL
            VAR whereDeps := selectCtx:WhereClause:GetTableDependencies(selectCtx:TableAliases)
            ? "WHERE clause depends on tables: " + String.Join(", ", whereDeps)
        ENDIF
    ELSE
        ? "Failed to parse: " + parser:Error
    ENDIF

    USE IN customers
    USE IN orders

    RETURN

FUNCTION PrintFields() AS VOID
    ? "FIELDS IN TABLE:", Alias()
    FOREACH VAR f IN DbStruct()
        ? "  ", f[1], f[2], f[3]
    NEXT

FUNCTION PrintTable() AS DWORD
    ? "RECORDS:"
    LOCAL resultTable AS STRING
    LOCAL fields AS ARRAY
    LOCAL Count AS DWORD
    resultTable = DbDataSource():Name
    fields := DbStruct()
    count := 0
    GO TOP
    DO WHILE !EOF()
        count++
        ? "  ", "| Rec:", count:ToString(), "| "
        FOREACH VAR f IN fields
            ?? f[1]+":", &(resultTable+"->"+f[1]), "| "
        NEXT
        SKIP
    ENDDO
    RETURN count


FUNCTION TestSelectFunctionality() AS VOID
    ? "Testing SELECT functionality..."

    // Create a test table
    CREATE CURSOR test_select_table ;
        (ID N(5), Name C(20), Age N(3))

    IF Used()
        ? "Created test table for SELECT testing"

        // Insert some test data
        INSERT INTO test_select_table VALUES (1, "John Doe", 30)
        INSERT INTO test_select_table VALUES (2, "Jane Smith", 25)
        INSERT INTO test_select_table VALUES (3, "Bob Johnson", 35)
        INSERT INTO test_select_table VALUES (4, "Alice Brown", 30)  // Duplicate age for DISTINCT test

        ? "Inserted test data"

        // Test basic SELECT
        ? "Attempting basic SELECT..."
        SELECT * FROM test_select_table
        ? "Records in QUERYRESULT after SELECT *: ", RecCount("QUERYRESULT")
        PrintFields()
        PrintTable()
        DbCloseArea()

        // Test SELECT with WHERE
        ? "Attempting SELECT with WHERE..."
        SELECT * FROM test_select_table WHERE Age > 25
        ? "Records in QUERYRESULT after SELECT with WHERE: ", RecCount("QUERYRESULT")
        PrintFields()
        PrintTable()
        DbCloseArea()

        // Test SELECT with specific columns
        ? "Attempting SELECT with specific columns..."
        SELECT Name, Age FROM test_select_table WHERE Age > 25
        ? "Records in QUERYRESULT after SELECT specific columns: ", RecCount("QUERYRESULT")
        PrintFields()
        PrintTable()
        DbCloseArea()

        // Test SELECT with DISTINCT
        ? "Attempting SELECT with DISTINCT..."
        SELECT DISTINCT Age FROM test_select_table
        ? "Records in QUERYRESULT after SELECT DISTINCT: ", RecCount("QUERYRESULT")
        PrintFields()
        PrintTable()
        DbCloseArea()

        // Test SELECT with TOP
        ? "Attempting SELECT with TOP..."
        SELECT TOP 2 * FROM test_select_table
        ? "Records in QUERYRESULT after SELECT TOP 2: ", RecCount("QUERYRESULT")
        PrintFields()
        PrintTable()
        DbCloseArea()

        // Test cross join with multiple tables
        ? "Creating additional tables for cross join test..."
        CREATE CURSOR test_table_a (Id N(5), Name C(20))
        CREATE CURSOR test_table_b (Id N(5), Desc C(30))

        IF Used("test_table_a") .AND. Used("test_table_b")
            ? "Created additional test tables"

            // Insert test data for cross join
            INSERT INTO test_table_a VALUES (1, "Item A1")
            INSERT INTO test_table_a VALUES (2, "Item A2")

            INSERT INTO test_table_b VALUES (1, "Description B1")
            INSERT INTO test_table_b VALUES (2, "Description B2")
            INSERT INTO test_table_b VALUES (3, "Description B3")

            ? "Inserted test data for cross join"

            // Test cross join (Cartesian product)
            ? "Attempting cross join (SELECT from multiple tables)..."
            SELECT test_table_a.Name, test_table_b.Desc FROM test_table_a, test_table_b
            ? "Records in QUERYRESULT after cross join: ", RecCount("QUERYRESULT")
            ? "Expected: 6 records (2 from A × 3 from B)"

            PrintFields()
            PrintTable()
            DbCloseArea()

            // Close the additional test tables
            USE IN test_table_a
            USE IN test_table_b
        ELSE
            ? "Failed to create additional test tables"
        ENDIF

        // Test field name resolution and aliasing
        ? "Creating table for field name resolution test..."
        CREATE CURSOR test_field_resolution (Id N(5), Name C(20), Value N(10,2))

        IF Used("test_field_resolution")
            ? "Created test table for field name resolution"

            // Insert test data
            INSERT INTO test_field_resolution VALUES (1, "Test Item", 100.50)
            INSERT INTO test_field_resolution VALUES (2, "Another Item", 200.75)

            ? "Inserted test data for field name resolution"

            // Test field aliasing with AS
            ? "Testing field aliasing with AS..."
            SELECT Id AS Identifier, Name AS ItemName, Value AS Amount FROM test_field_resolution
            ? "Records in QUERYRESULT after field aliasing: ", RecCount("QUERYRESULT")

            PrintFields()
            PrintTable()
            DbCloseArea()

            // Test expression aliasing with AS
            ? "Testing expression aliasing with AS..."
            SELECT Id, Name, Value, Value * 1.1 AS TaxedValue FROM test_field_resolution
            ? "Records in QUERYRESULT after expression aliasing: ", RecCount("QUERYRESULT")

            // Display the results to verify expression alias
            PrintFields()
            PrintTable()
            DbCloseArea()

            // Test field name conflict resolution
            ? "Creating additional table for conflict resolution test..."
            CREATE CURSOR test_conflict (Id N(5), Name C(20), Desc C(30))

            IF Used("test_conflict")
                INSERT INTO test_conflict VALUES (1, "Conflict Name", "Description 1")
                INSERT INTO test_conflict VALUES (2, "Another Name", "Description 2")

                ? "Testing field name conflict resolution..."
                SELECT test_field_resolution.Name, test_conflict.Name ;
                    FROM test_field_resolution, test_conflict ;
                    WHERE test_field_resolution.Id = 1 AND test_conflict.Id = 1
                ? "Records in QUERYRESULT after conflict resolution: ", RecCount("QUERYRESULT")

                // Display the results to verify conflict resolution
                PrintFields()
                PrintTable()
                DbCloseArea()

                USE IN test_conflict
            ENDIF

            // Close the test table
            USE IN test_field_resolution
        ELSE
            ? "Failed to create test table for field name resolution"
        ENDIF

        // Test INTO CURSOR functionality
        ? "Testing INTO CURSOR functionality..."
        CREATE CURSOR test_cursor_target (Id N(5), Name C(20), Value N(10,2))

        IF Used("test_cursor_target")
            ? "Created test table for INTO CURSOR test"

            // Insert test data
            INSERT INTO test_cursor_target VALUES (1, "Test Item 1", 100.50)
            INSERT INTO test_cursor_target VALUES (2, "Test Item 2", 200.75)
            INSERT INTO test_cursor_target VALUES (3, "Test Item 3", 300.25)

            ? "Inserted test data for INTO CURSOR test"

            // Test SELECT INTO CURSOR
            ? "Testing SELECT INTO CURSOR..."
            SELECT * FROM test_cursor_target INTO CURSOR MyTestCursor
            ? "Records in MyTestCursor: ", RecCount("MyTestCursor")

            // Verify the cursor exists and has the correct data
            IF Used("MyTestCursor")
                SELECT MyTestCursor
                GO TOP
                LOCAL count AS DWORD
                count := 0
                DO WHILE !EOF()
                    count++
                    ? count:ToString() + ".", "Id:", MyTestCursor->Id, "Name:", MyTestCursor->Name, "Value:", MyTestCursor->Value
                    SKIP
                ENDDO
                ? "Verified MyTestCursor has", count, "records"
                USE IN MyTestCursor
            ELSE
                ? "ERROR: MyTestCursor was not created!"
            ENDIF

            // Test SELECT with field aliasing INTO CURSOR
            ? "Testing SELECT with field aliasing INTO CURSOR..."
            SELECT Id AS ItemId, Name AS ItemName, Value AS ItemValue FROM test_cursor_target INTO CURSOR AliasedCursor
            ? "Records in AliasedCursor: ", RecCount("AliasedCursor")

            // Verify the aliased cursor exists and has the correct data
            IF Used("AliasedCursor")
                SELECT AliasedCursor
                GO TOP
                LOCAL aliasCount AS DWORD
                aliasCount := 0
                LOCAL fieldCount AS DWORD
                fieldCount := FCount()
                ? "AliasedCursor has", fieldCount, "fields"
                FOR LOCAL i := 1 AS DWORD TO fieldCount
                    ? "Field " + i:ToString() + ": " + FieldName(i)
                NEXT
                DO WHILE !EOF()
                    aliasCount++
                    ? aliasCount:ToString() + ".", "ItemId:", AliasedCursor->ItemId, "ItemName:", AliasedCursor->ItemName, "ItemValue:", AliasedCursor->ItemValue
                    SKIP
                ENDDO
                ? "Verified AliasedCursor has", aliasCount, "records"
                USE IN AliasedCursor
            ELSE
                ? "ERROR: AliasedCursor was not created!"
            ENDIF

            // Close the test table
            USE IN test_cursor_target
        ELSE
            ? "Failed to create test table for INTO CURSOR test"
        ENDIF

        // Test CROSS JOIN functionality
        TestCrossJoinFunctionality()

        // Close the test table
        USE IN test_select_table
    ELSE
        ? "Failed to create test table"
    ENDIF

    RETURN

FUNCTION TestCrossJoinFunctionality() AS VOID
    ? "Testing CROSS JOIN functionality..."

    // Create test tables for CROSS JOIN
    CREATE CURSOR cross_join_table_a (Id N(5), Name C(20))
    CREATE CURSOR cross_join_table_b (Id N(5), Desc C(30))

    IF Used("cross_join_table_a") .AND. Used("cross_join_table_b")
        ? "Created test tables for CROSS JOIN"

        // Insert test data
        INSERT INTO cross_join_table_a VALUES (1, "Item A1")
        INSERT INTO cross_join_table_a VALUES (2, "Item A2")

        INSERT INTO cross_join_table_b VALUES (1, "Description B1")
        INSERT INTO cross_join_table_b VALUES (2, "Description B2")
        INSERT INTO cross_join_table_b VALUES (3, "Description B3")

        ? "Inserted test data for CROSS JOIN"

        // Test explicit CROSS JOIN syntax
        ? "Testing explicit CROSS JOIN syntax..."
        SELECT cross_join_table_a.Name, cross_join_table_b.Desc ;
            FROM cross_join_table_a ;
            CROSS JOIN cross_join_table_b
        ? "Records in QUERYRESULT after explicit CROSS JOIN: ", RecCount("QUERYRESULT")
        ? "Expected: 6 records (2 from A × 3 from B)"

        PrintFields()
        PrintTable()
        DbCloseArea()

        // Test CROSS JOIN with aliases
        ? "Testing CROSS JOIN with aliases..."
        SELECT a.Name, b.Desc ;
            FROM cross_join_table_a a ;
            CROSS JOIN cross_join_table_b b
        ? "Records in QUERYRESULT after CROSS JOIN with aliases: ", RecCount("QUERYRESULT")
        ? "Expected: 6 records (2 from A × 3 from B)"

        PrintFields()
        PrintTable()
        DbCloseArea()

        // Test CROSS JOIN with WHERE clause
        ? "Testing CROSS JOIN with WHERE clause..."
        SELECT a.Name, b.Desc ;
            FROM cross_join_table_a a ;
            CROSS JOIN cross_join_table_b b ;
            WHERE a.Id = 1
        ? "Records in QUERYRESULT after CROSS JOIN with WHERE: ", RecCount("QUERYRESULT")
        ? "Expected: 3 records (only rows where a.Id = 1)"

        PrintFields()
        PrintTable()
        DbCloseArea()

        // Close the test tables
        USE IN cross_join_table_a
        USE IN cross_join_table_b
    ELSE
        ? "Failed to create test tables for CROSS JOIN"
    ENDIF

    RETURN

FUNCTION TestQueryOptimizer() AS VOID
    ? "Testing Query Optimizer functionality..."

    CREATE CURSOR test_optimizer_table ;
        (Id N(5), Name Character(20), Age N(3), City Character(20))

    IF Used()
        ? "Created test table for optimizer testing"

        INSERT INTO test_optimizer_table VALUES (1, "John Doe", 30, "New York")
        INSERT INTO test_optimizer_table VALUES (2, "Jane Smith", 25, "Boston")
        INSERT INTO test_optimizer_table VALUES (3, "Bob Johnson", 35, "New York")
        INSERT INTO test_optimizer_table VALUES (4, "Alice Brown", 30, "Chicago")
        INSERT INTO test_optimizer_table VALUES (5, "Charlie Wilson", 40, "New York")

        // Test complex WHERE clause that would benefit from optimization
        ? "Testing complex WHERE clause with optimizer..."
        SELECT * FROM test_optimizer_table WHERE Age > 25 .AND. City = "New York"
        ? "Records in QUERYRESULT after complex WHERE: ", RecCount("QUERYRESULT")

        // Display the results
        PrintFields()
        PrintTable()
        DbCloseArea()

        USE IN test_optimizer_table
    ELSE
        ? "Failed to create optimizer test table"
    ENDIF
RETURN


FUNCTION __SqlDelete (sCommand as STRING)
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}
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
    ? sCommand
    IF ! parser:ParseUpdateStatement(out var table)
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
    RETURN

FUNCTION __SqlInsertFromSQL( cUdc AS STRING, cTable AS STRING, aFields AS STRING[], cSelect AS STRING ) AS VOID
    LOCAL i AS DWORD
    LOCAL uValue AS USUAL
    LOCAL cFieldName AS STRING

    ? "Executing INSERT FROM SQL:", cUdc
    ? "Target table:", cTable
    ? "Fields:", Atoa(aFields)
    ? "Source SQL:", cSelect

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
    ? sCommand
    VAR ctx := parser:ParseExpressionContext()
    ? ctx:ToString()
    PrintContext(ctx)






