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


FUNCTION Start() AS VOID STRICT
    try
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
        TestSqlParser("x = 1 AND y = 2")
        wait

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
        wait
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
//             WHERE Country="Mexico"
    catch e as exception
        ? "Error", e:Message
    end try
    WAIT



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
        ? "Value", t
    next
    foreach var t in table:TableList
        ? "From Table", t
    next
    foreach var j in table:JoinList
        ? "Join Table", j
    next
    ? "Where", table:WhereClause

    VAR o := XSharp.MacroCompiler.MacroOptions.FoxPro
    o:AllowOldStyleAssignments := False
    VAR mc := XSharp.Runtime.MacroCompiler{o}
    // VAR mc := XSharp.Runtime.MacroCompiler.GetScriptCompiler(XSharpDialect.FoxPro)
    VAR cbWhere := mc:CompileCodeblock(table:WhereClause)
    //VAR res := (OBJECT) cb:Eval(args)

    VAR values := Dictionary<STRING,CODEBLOCK>{}
    FOR VAR i := 0 TO table:ColumnList:Count-1
        values[table:ColumnList[i]] := mc:CompileCodeblock(table:ValueList[i])
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

FUNCTION PrintContext(ctx AS SqlExpressionContext, depth := 0 AS INT)
    IF ctx IS SqlSimpleExpressionContext VAR s
        ? STRING{c" ", depth*2} + "SIMPLE: " + s:ToString()
    ELSEIF ctx IS SqlCompsiteExpressionContext VAR c
        ? STRING{c" ", depth*2} + "COMPOSITE: " + c:ToString()
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

FUNCTION TestSqlParser (sCommand as STRING)
    VAR lexer := XSqlLexer{sCommand}
    VAR tokens := lexer:AllTokens()
    var parser := SqlParser{XTokenList{tokens}}
    ? sCommand
    VAR ctx := parser.ParseExpressionContext()
    ? ctx:ToString()
    PrintContext(ctx)

