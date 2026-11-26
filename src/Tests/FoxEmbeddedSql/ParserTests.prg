// ParserTests.prg
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

    VAR arrayOutputParser := SQLParser{XTokenList{XSqlLexer{"SELECT Id FROM Customers INTO ARRAY CustomerRows"}:AllTokens()}}
    LOCAL arrayOutputCtx AS FoxSelectContext
    IF !arrayOutputParser:ParseSelectStatement(OUT arrayOutputCtx) .OR. ;
       arrayOutputCtx:TargetArray != "CustomerRows" .OR. !String.IsNullOrEmpty(arrayOutputCtx:TargetCursor)
        THROW Exception{"SELECT INTO ARRAY should retain its target and not set a cursor target: " + arrayOutputParser:Error}
    ENDIF

    VAR invalidArrayOutputParser := SQLParser{XTokenList{XSqlLexer{"SELECT Id FROM Customers INTO ARRAY"}:AllTokens()}}
    IF invalidArrayOutputParser:ParseSelectStatement(OUT VAR _)
        THROW Exception{"SELECT INTO ARRAY without a target should be rejected"}
    ENDIF

    VAR tableOutputParser := SQLParser{XTokenList{XSqlLexer{"SELECT Id FROM Customers INTO TABLE customer_copy"}:AllTokens()}}
    LOCAL tableOutputCtx AS FoxSelectContext
    IF !tableOutputParser:ParseSelectStatement(OUT tableOutputCtx) .OR. ;
       tableOutputCtx:TargetTable != "customer_copy" .OR. ;
       !String.IsNullOrEmpty(tableOutputCtx:TargetCursor) .OR. !String.IsNullOrEmpty(tableOutputCtx:TargetArray)
        THROW Exception{"SELECT INTO TABLE should retain a persistent table target: " + tableOutputParser:Error}
    ENDIF

    VAR cursorOptionsParser := SQLParser{XTokenList{XSqlLexer{"SELECT Id FROM Customers INTO CURSOR customer_rows NOFILTER READWRITE"}:AllTokens()}}
    LOCAL cursorOptionsCtx AS FoxSelectContext
    IF !cursorOptionsParser:ParseSelectStatement(OUT cursorOptionsCtx) .OR. ;
       cursorOptionsCtx:TargetCursor != "customer_rows" .OR. ;
       !cursorOptionsCtx:TargetCursorNoFilter .OR. !cursorOptionsCtx:TargetCursorReadWrite
        THROW Exception{"INTO CURSOR should retain NOFILTER and READWRITE options: " + cursorOptionsParser:Error}
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
    IF !outerJoinParser:ParseSelectStatement(OUT outerJoinCtx) .OR. ;
       outerJoinCtx:Joins:Count != 1 .OR. outerJoinCtx:Joins[0]:Kind != FoxSelectJoinKind.Left
        THROW Exception{"LEFT OUTER JOIN should parse as a left join: " + outerJoinParser:Error}
    ENDIF

    VAR chainedOuterLexer := XSqlLexer{"SELECT a.Id FROM TableA a LEFT JOIN TableB b ON a.Id = b.Id LEFT OUTER JOIN TableC c ON b.Id = c.Id"}
    VAR chainedOuterParser := SqlParser{XTokenList{chainedOuterLexer:AllTokens()}}
    LOCAL chainedOuterCtx AS FoxSelectContext
    IF !chainedOuterParser:ParseSelectStatement(OUT chainedOuterCtx) .OR. ;
       chainedOuterCtx:Joins:Count != 2 .OR. ;
       chainedOuterCtx:Joins[0]:Kind != FoxSelectJoinKind.Left .OR. ;
       chainedOuterCtx:Joins[1]:Kind != FoxSelectJoinKind.Left
        THROW Exception{"Chained LEFT JOIN forms should parse with left-join metadata: " + chainedOuterParser:Error}
    ENDIF

    VAR missingOnLexer := XSqlLexer{"SELECT a.Id FROM TableA a JOIN TableB b"}
    VAR missingOnParser := SqlParser{XTokenList{missingOnLexer:AllTokens()}}
    IF missingOnParser:ParseSelectStatement(OUT VAR _)
        THROW Exception{"JOIN without ON should be rejected"}
    ENDIF

    VAR missingOuterOnLexer := XSqlLexer{"SELECT a.Id FROM TableA a LEFT JOIN TableB b"}
    VAR missingOuterOnParser := SqlParser{XTokenList{missingOuterOnLexer:AllTokens()}}
    IF missingOuterOnParser:ParseSelectStatement(OUT VAR _)
        THROW Exception{"LEFT JOIN without ON should be rejected"}
    ENDIF

    VAR unionLexer := XSqlLexer{"SELECT Id FROM FirstTable UNION ALL SELECT Id FROM SecondTable UNION SELECT Id FROM ThirdTable ORDER BY Id DESC INTO CURSOR UnionResult"}
    VAR unionParser := SQLParser{XTokenList{unionLexer:AllTokens()}}
    LOCAL unionCtx AS FoxSelectContext
    IF !unionParser:ParseSelectStatement(OUT unionCtx)
        THROW Exception{"Failed to parse UNION chain: " + unionParser:Error}
    ENDIF
    IF unionCtx:UnionBranches:Count != 2 .OR. unionCtx:UnionAll:Count != 2 .OR. ;
       !unionCtx:UnionAll[0] .OR. unionCtx:UnionAll[1] .OR. ;
       unionCtx:OrderByClause:Count != 1 .OR. unionCtx:TargetCursor != "UnionResult"
        THROW Exception{"UNION should preserve operators and final ORDER BY/INTO clauses"}
    ENDIF

    VAR derivedLexer := XSqlLexer{"SELECT d.Id FROM (SELECT c.Id FROM Child c WHERE c.Id > 1) d WHERE d.Id < 5"}
    VAR derivedParser := SQLParser{XTokenList{derivedLexer:AllTokens()}}
    LOCAL derivedCtx AS FoxSelectContext
    IF !derivedParser:ParseSelectStatement(OUT derivedCtx) .OR. ;
       derivedCtx:DerivedTables:Count != 1 .OR. ;
       !derivedCtx:DerivedTables:ContainsKey("d") .OR. ;
       derivedCtx:DerivedTables["d"]:SelectList:Count != 1
        THROW Exception{"Aliased FROM subqueries should parse and retain their SELECT context: " + derivedParser:Error}
    ENDIF
    FOREACH VAR derivedJoinSql IN <STRING>{ ;
        "SELECT p.Id, d.Id FROM Parent p INNER JOIN (SELECT c.Id FROM Child c) d ON p.Id = d.Id", ;
        "SELECT d.Id, p.Id FROM (SELECT c.Id FROM Child c) AS d JOIN Parent p ON d.Id = p.Id" }
        VAR derivedJoinParser := SQLParser{XTokenList{XSqlLexer{derivedJoinSql}:AllTokens()}}
        LOCAL derivedJoinCtx AS FoxSelectContext
        IF !derivedJoinParser:ParseSelectStatement(OUT derivedJoinCtx) .OR. ;
           derivedJoinCtx:DerivedTables:Count != 1 .OR. derivedJoinCtx:Joins:Count != 1 .OR. ;
           derivedJoinCtx:Joins[0]:Kind != FoxSelectJoinKind.Inner .OR. derivedJoinCtx:TableList:Count != 2
            THROW Exception{"INNER JOIN should accept one derived source in either position: " + derivedJoinParser:Error}
        ENDIF
    NEXT
    VAR multipleDerivedSql := "SELECT a.Id, b.Id, p.Id FROM (SELECT c.Id FROM Child c) a " + ;
        "INNER JOIN (SELECT x.Id FROM Other x) b ON a.Id = b.Id " + ;
        "INNER JOIN Parent p ON b.Id = p.Id"
    VAR multipleDerivedParser := SQLParser{XTokenList{XSqlLexer{multipleDerivedSql}:AllTokens()}}
    LOCAL multipleDerivedCtx AS FoxSelectContext
    IF !multipleDerivedParser:ParseSelectStatement(OUT multipleDerivedCtx) .OR. ;
       multipleDerivedCtx:DerivedTables:Count != 2 .OR. multipleDerivedCtx:TableList:Count != 3 .OR. ;
       multipleDerivedCtx:Joins:Count != 2 .OR. multipleDerivedCtx:Joins[0]:Kind != FoxSelectJoinKind.Inner .OR. ;
       multipleDerivedCtx:Joins[1]:Kind != FoxSelectJoinKind.Inner
        THROW Exception{"Multiple derived sources should parse in an explicit INNER JOIN chain: " + multipleDerivedParser:Error}
    ENDIF
    FOREACH VAR derivedJoinSql IN <STRING>{ ;
        "SELECT d.Id FROM (SELECT c.Id FROM Child c) d, Other o", ;
        "SELECT d.Id FROM (SELECT c.Id FROM Child c) d CROSS JOIN Other o", ;
        "SELECT d.Id FROM Other o CROSS JOIN (SELECT c.Id FROM Child c) d", ;
        "SELECT d.Id, o.Id FROM (SELECT c.Id FROM Child c) d LEFT JOIN Other o ON d.Id = o.Id", ;
        "SELECT d.Id, o.Id FROM Other o LEFT JOIN (SELECT c.Id FROM Child c) d ON o.Id = d.Id", ;
        "SELECT d.Id, o.Id FROM Other o, (SELECT c.Id FROM Child c) d LEFT JOIN Third t ON d.Id = t.Id" }
        VAR mixedDerivedParser := SQLParser{XTokenList{XSqlLexer{derivedJoinSql}:AllTokens()}}
        LOCAL mixedDerivedCtx AS FoxSelectContext
        IF !mixedDerivedParser:ParseSelectStatement(OUT mixedDerivedCtx) .OR. ;
           mixedDerivedCtx:TableList:Count != mixedDerivedCtx:Joins:Count + 1 .OR. ;
           mixedDerivedCtx:DerivedTables:Count != 1
            THROW Exception{"Derived tables should parse in comma, CROSS, LEFT, and mixed source lists: " + mixedDerivedParser:Error}
        ENDIF
    NEXT
    FOREACH VAR invalidDerivedSql IN <STRING>{ ;
        "SELECT d.Id FROM (SELECT c.Id FROM Child c)", ;
        "SELECT d.Id FROM (SELECT c.Id FROM Child c", ;
        "SELECT a.Id FROM (SELECT c.Id FROM Child c) a INNER JOIN (SELECT x.Id FROM Other x) a ON a.Id = a.Id", ;
        "SELECT d.Id FROM Parent p INNER JOIN (SELECT c.Id FROM Child c)", ;
        "SELECT d.Id FROM Parent p INNER JOIN (SELECT c.Id FROM Child c) d LEFT JOIN Other o ON d.Id = o.Id", ;
        "SELECT a.Id FROM Parent p INNER JOIN (SELECT c.Id FROM Child c) a LEFT JOIN (SELECT x.Id FROM Other x) b ON a.Id = b.Id" }
        VAR invalidDerivedParser := SQLParser{XTokenList{XSqlLexer{invalidDerivedSql}:AllTokens()}}
        IF invalidDerivedParser:ParseSelectStatement(OUT VAR _)
            THROW Exception{"Invalid or unsupported derived table form should be rejected: " + invalidDerivedSql}
        ENDIF
    NEXT

    VAR inLexer := XSqlLexer{"SELECT a.Id FROM Parent a WHERE a.Id IN (SELECT b.Id FROM Child b WHERE b.Active = .T.)"}
    VAR scalarLexer := XSqlLexer{"SELECT a.Id FROM Parent a WHERE a.Id >= (SELECT b.Id FROM Child b WHERE b.Active = .T.)"}
    VAR scalarParser := SQLParser{XTokenList{scalarLexer:AllTokens()}}
    LOCAL scalarCtx AS FoxSelectContext
    IF !scalarParser:ParseSelectStatement(OUT scalarCtx) .OR. ;
       !(scalarCtx:WhereClause IS SqlCompareExpressionContext VAR scalarComparison) .OR. ;
       !(scalarComparison:Right IS SqlScalarSubqueryExpressionContext)
        THROW Exception{"Scalar comparison should parse into a dedicated subquery expression"}
    ENDIF
    VAR scalarProjectionParser := SQLParser{XTokenList{XSqlLexer{"SELECT (SELECT b.Id FROM Child b) AS ChildId FROM Parent"}:AllTokens()}}
    LOCAL scalarProjectionCtx AS FoxSelectContext
    IF !scalarProjectionParser:ParseSelectStatement(OUT scalarProjectionCtx) .OR. ;
       scalarProjectionCtx:SelectList:Count != 1 .OR. ;
       !(scalarProjectionCtx:SelectList[0] IS SqlScalarSubqueryExpressionContext) .OR. ;
       scalarProjectionCtx:FieldSelectionList[0]:Alias != "ChildId"
        THROW Exception{"Scalar SELECT-list subquery should parse as a named projection"}
    ENDIF
    VAR scalarExpressionParser := SQLParser{XTokenList{XSqlLexer{"SELECT a.Id + (SELECT b.Id FROM Child b) AS Total FROM Parent a"}:AllTokens()}}
    LOCAL scalarExpressionCtx AS FoxSelectContext
    IF !scalarExpressionParser:ParseSelectStatement(OUT scalarExpressionCtx) .OR. ;
       scalarExpressionCtx:SelectList:Count != 1 .OR. ;
       scalarExpressionCtx:SelectList[0] IS NOT SqlCompositeExpressionContext
        THROW Exception{"Scalar subqueries should parse inside projection expressions"}
    ENDIF
    VAR scalarProjectionExpression := (SqlCompositeExpressionContext)scalarExpressionCtx:SelectList[0]
    LOCAL projectionHasScalar := FALSE AS LOGIC
    FOREACH VAR projectionTerm IN scalarProjectionExpression:Exprs
        IF projectionTerm IS SqlScalarSubqueryExpressionContext
            projectionHasScalar := TRUE
            EXIT
        ENDIF
    NEXT
    IF !projectionHasScalar
        THROW Exception{"Projection expression should retain its scalar subquery node"}
    ENDIF
    FOREACH VAR quantifier IN <STRING>{"ANY", "SOME", "ALL"}
        VAR quantifiedParser := SQLParser{XTokenList{XSqlLexer{"SELECT a.Id FROM Parent a WHERE a.Id >= " + quantifier + " (SELECT b.Id FROM Child b)"}:AllTokens()}}
        LOCAL quantifiedCtx AS FoxSelectContext
        IF !quantifiedParser:ParseSelectStatement(OUT quantifiedCtx) .OR. ;
           !(quantifiedCtx:WhereClause IS SqlQuantifiedSubqueryExpressionContext VAR quantifiedExpression) .OR. ;
           quantifiedExpression:Quantifier != quantifier .OR. quantifiedExpression:Op:Type != XTokenType.GTE .OR. ;
           quantifiedExpression:Subquery:SelectList:Count != 1
            THROW Exception{"Quantified comparison should preserve its operator and subquery: " + quantifier}
        ENDIF
    NEXT
    VAR existsLexer := XSqlLexer{"SELECT a.Id FROM Parent a WHERE NOT EXISTS (SELECT b.Id FROM Child b WHERE b.Id = a.Id)"}
    VAR existsParser := SQLParser{XTokenList{existsLexer:AllTokens()}}
    LOCAL existsCtx AS FoxSelectContext
    IF !existsParser:ParseSelectStatement(OUT existsCtx) .OR. ;
       !(existsCtx:WhereClause IS SqlPrefixExpressionContext VAR existsPrefix) .OR. ;
       !(existsPrefix:Expr IS SqlExistsSubqueryExpressionContext)
        THROW Exception{"NOT EXISTS should parse as a negated subquery predicate"}
    ENDIF
    VAR badExistsLexer := XSqlLexer{"SELECT Id FROM Parent WHERE EXISTS (SELECT Id FROM Child"}
    VAR badExistsParser := SQLParser{XTokenList{badExistsLexer:AllTokens()}}
    LOCAL badExistsRejected := FALSE AS LOGIC
    TRY
        badExistsRejected := !badExistsParser:ParseSelectStatement(OUT VAR _)
    CATCH AS ArgumentException
        badExistsRejected := TRUE
    END TRY
    IF !badExistsRejected
        THROW Exception{"Unclosed EXISTS subquery should be rejected"}
    ENDIF
    FOREACH VAR malformed IN <STRING>{ ;
        "SELECT Id FROM Parent WHERE EXISTS ()", ;
        "SELECT Id FROM Parent WHERE EXISTS SELECT Id FROM Child", ;
        "SELECT Id FROM Parent WHERE EXISTS (SELECT Id FROM Child INTO CURSOR c WHERE Id = 1)", ;
        "SELECT Id FROM Parent WHERE Id IN (SELECT Id FROM Child INTO CURSOR c WHERE Id = 1)", ;
        "SELECT Id FROM Parent WHERE Id = (SELECT Id FROM Child", ;
        "SELECT Id FROM Parent WHERE Id = (SELECT Id FROM Child INTO CURSOR c WHERE Id = 1)", ;
        "SELECT Id FROM Parent WHERE Id >= ANY ()", ;
        "SELECT Id FROM Parent WHERE Id >= SOME SELECT Id FROM Child" }
        LOCAL rejected := FALSE AS LOGIC
        TRY
            VAR malformedParser := SQLParser{XTokenList{XSqlLexer{malformed}:AllTokens()}}
            rejected := !malformedParser:ParseSelectStatement(OUT VAR _)
        CATCH AS Exception
            rejected := TRUE
        END TRY
        IF !rejected
            THROW Exception{"Malformed subquery should be rejected: " + malformed}
        ENDIF
    NEXT
    VAR inParser := SQLParser{XTokenList{inLexer:AllTokens()}}
    LOCAL inCtx AS FoxSelectContext
    IF !inParser:ParseSelectStatement(OUT inCtx) .OR. ;
       !(inCtx:WhereClause IS SqlInSubqueryExpressionContext)
        THROW Exception{"IN subquery predicate should parse into a dedicated expression"}
    ENDIF
    VAR inExpression := (SqlInSubqueryExpressionContext)inCtx:WhereClause
    IF inExpression:Subquery:TableList:Count != 1 .OR. inExpression:Subquery:WhereClause == NULL
        THROW Exception{"IN subquery should retain its SELECT source and WHERE clause"}
    ENDIF

    VAR notInLexer := XSqlLexer{"SELECT Id FROM Parent WHERE Id NOT IN (SELECT Id FROM Child)"}
    VAR notInParser := SQLParser{XTokenList{notInLexer:AllTokens()}}
    LOCAL notInCtx AS FoxSelectContext
    IF !notInParser:ParseSelectStatement(OUT notInCtx) .OR. ;
       !(notInCtx:WhereClause IS SqlInSubqueryExpressionContext VAR notInExpression) .OR. ;
       !notInExpression:IsNegated
        THROW Exception{"NOT IN subquery should preserve negation"}
    ENDIF

    VAR inListLexer := XSqlLexer{"SELECT Id FROM Parent WHERE Id IN (1, 2, 3)"}
    VAR inListParser := SQLParser{XTokenList{inListLexer:AllTokens()}}
    LOCAL inListCtx AS FoxSelectContext
    IF !inListParser:ParseSelectStatement(OUT inListCtx) .OR. ;
       !(inListCtx:WhereClause IS SqlInListExpressionContext VAR inListExpression) .OR. ;
       inListExpression:Items:Count != 3
        THROW Exception{"Existing literal IN lists should remain parseable"}
    ENDIF

    VAR incompleteUnionLexer := XSqlLexer{"SELECT Id FROM FirstTable UNION"}
    VAR incompleteUnionParser := SQLParser{XTokenList{incompleteUnionLexer:AllTokens()}}
    IF incompleteUnionParser:ParseSelectStatement(OUT VAR _)
        THROW Exception{"UNION without a following SELECT should be rejected"}
    ENDIF

    VAR misplacedUnionLexer := XSqlLexer{"SELECT Id FROM FirstTable ORDER BY Id UNION ALL SELECT Id FROM SecondTable"}
    VAR misplacedUnionParser := SQLParser{XTokenList{misplacedUnionLexer:AllTokens()}}
    IF misplacedUnionParser:ParseSelectStatement(OUT VAR _)
        THROW Exception{"ORDER BY before the UNION chain should be rejected"}
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

FUNCTION TestSqlParser () AS VOID
    VAR sCommand := "xi = 1 AND (yi+1)+table.zed = 2"
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}

    IF Verbose
        ? sCommand
    ENDIF

    VAR ctx := parser:ParseExpressionContext()
    IF Verbose
        ? ctx:ToString()
        PrintContext(ctx)
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
    CATCH AS Exception
        rejectedDanglingOperator := TRUE
    END TRY
    IF !rejectedDanglingOperator
        THROW Exception{"Parser accepted a dangling AND operator"}
    ENDIF

    rejectedDanglingOperator := FALSE
    TRY
        ParseSqlExpressionForTest("a = 1 OR")
    CATCH AS Exception
        rejectedDanglingOperator := TRUE
    END TRY
    IF !rejectedDanglingOperator
        THROW Exception{"Parser accepted a dangling OR operator"}
    ENDIF
RETURN

FUNCTION ParseSqlExpressionForTest(expression AS STRING) AS SqlExpressionContext
    VAR lexer := XSqlLexer{expression}
    RETURN SqlParser{XTokenList{lexer:AllTokens()}}:ParseExpressionContext()


