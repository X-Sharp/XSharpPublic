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

#include "foxcmd.xh"

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

        DELETE FROM employee where EmpID = 1
        DELETE FROM Database!employee where EmpID = 1
        DELETE  MyProducts FROM MSRPList ;
           WHERE MSRPList.ProdID = MyProducts.ProdID;
            AND MSRPList.discontinued = .t.

        DELETE  DB!MyProducts FROM DB!MSRPList ;
           WHERE MSRPList.ProdID = MyProducts.ProdID;
            AND MSRPList.discontinued = .t.

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
        INSERT INTO Orders(CustId,OrderAmt, OrderQty,CustName) VALUES (1, 100, 10,"19010101")
        INSERT INTO Orders(CustId,OrderAmt, OrderQty,CustName) VALUES (1, 200, 20,"19010202")
    Browse()
    ALTER TABLE Orders Add OrderDate Date NULL
    Browse()
    ALTER TABLE Orders Add COLUMN DeliveryDate DateTime
    Browse()
    ALTER TABLE Orders DROP COLUMN OrderAmt
    Browse()
    ALTER TABLE Orders Alter COLUMN CustName C(5)
    Browse()

    CREATE SQL VIEW MyView AS SELECT * FROM Northwind!Customers;
    WHERE Country="Mexico"
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

    RETURN
