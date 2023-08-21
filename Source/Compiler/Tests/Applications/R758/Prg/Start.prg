USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.VFP
// Test program for FoxPro Sql commands
// compile with /nostddef



#command SELECT <*columnClauses*> FROM <*TableList*>           ;
    [WITH (BUFFERING = <lExpr> ) ]                             ;
    [WHERE <*WhereConditions*> ]                               ;
    [GROUP BY <*ColumnList*> [HAVING <*HavingConditions*>] ]   ;
    [UNION [ALL] <*SelectCommand*>]                            ;
    [ORDER BY <*orderByList*>]                                 ;
    [TO <dest:FILE,PRINTER,SCREEN> <*extraoptions*>]        => ;
    __FoxSqlSelect(<"udc">)                



#command SELECT <*columnClauses*> FROM <*TableList*>           ;
    [WITH (BUFFERING = <lExpr> ) ]                             ;
    [WHERE <*WhereConditions*> ]                               ;
    [GROUP BY <*ColumnList*> [HAVING <*HavingConditions*>] ]   ;
    [UNION [ALL] <*SelectCommand*>]                            ;
    [ORDER BY <*orderByList*>]                                 ;
    INTO ARRAY <ArrayName>                                     => ;
    __FoxSqlSelectArray(<"udc">,<ArrayName>)                

            
#command SELECT <*columnClauses*> FROM <*TableList*>           ;
    [WITH (BUFFERING = <lExpr> ) ]                             ;
    [WHERE <*WhereConditions*> ]                               ;
    [GROUP BY <*ColumnList*> [HAVING <*HavingConditions*>] ]   ;
    [UNION [ALL] <*SelectCommand*>]                            ;
    [ORDER BY <*orderByList*>]                                 ;
    INTO CURSOR <CursorName>  [<cursoropt:NOFILTER,READWRITE> ]=> ;
    __FoxSqlSelectCursor(<"udc">,<"CursorName">)     

#command SELECT <*columnClauses*> FROM <*TableList*>           ;
    [WITH (BUFFERING = <lExpr> ) ]                             ;
    [WHERE <*WhereConditions*> ]                               ;
    [GROUP BY <*ColumnList*> [HAVING <*HavingConditions*>] ]   ;
    [UNION [ALL] <*SelectCommand*>]                            ;
    [ORDER BY <*orderByList*>]                                 ;
    INTO TABLE <TableName> [DATABASE <DatabaseName> [NAME <LongName>] ]  => ;
    __FoxSqlSelectTable(<"udc">,<"TableName">, <"DatabaseName">, <"LongName">)     


// SQL Insert commands
    
#command INSERT INTO <*dbfAndFields*> VALUES <*values*>    => ;
    __FoxSqlCommand(<"udc">)

#command INSERT INTO <*dbfAndFields*> SELECT <*selectClause*>   => ;
    __FoxSqlCommand(<"udc">)

    
#command INSERT INTO <*dbfAndFields*> FROM MEMVAR => ;
    __FoxSqlInsertMemVar(<"udc">)

#command INSERT INTO <*dbfAndFields*> FROM ARRAY <arrayName>   => ;
    __FoxSqlInsertArray(<"udc">,<arrayName>,<"arrayname">)

#command INSERT INTO <*dbfAndFields*> FROM NAME <objectName>   => ;
    __FoxSqlInsertObject(<"udc">,<objectName>,<"objectName">)



// SQL Delete commands

#command DELETE [<*target*>] FROM <moreClauses,...> => ;
    __FoxSqlDelete(<"udc"> )

// SQL Update Commands

#command UPDATE <*target*> SET <otherClauses,...> => ;
    __FoxSqlUpdate(<"udc">)


// SQL Create Commands

#command CREATE SQL VIEW <*clauses*> => ;
    __FoxSqlCreateView(<"udc">)

#command CREATE TABLE <*clauses*> => ;
    __FoxSqlCreateTable(<"udc">)

#command CREATE CURSOR <*clauses*> => ;
    __FoxSqlCreateCursor(<"udc">)

#command DROP TABLE <*clauses*> => ;
    __FoxSqlCommand(<"udc">)

#command DROP VIEW <*clauses*> => ;
    __FoxSqlCommand(<"udc">)

// SQL Alter Commands

#command ALTER TABLE <*clauses*> => ;
    __FoxSqlCreateTable(<"udc">)


// Unsupported commands

#command UPDATE SET      <*clauses*> => #error "Table name missing in UPDATE statement"
#command CREATE TRIGGER  <*clauses*> => #error "CREATE TRIGGER is not supported "
#command DELETE TRIGGER  <*clauses*> => #error "DELETE TRIGGER is not supported "
    

FUNCTION Start( ) AS VOID                              
    LOCAL aValues := {} AS ARRAY                   
    LOCAL objectName := NULL AS OBJECT        
    DROP TABLE Customer
    DROP VIEW CustomerContacts
    CREATE TABLE Customer NAME Customers FREE    
    ALTER TABLE Customer ADD COLUMN Fax c(20) NULL

    CREATE SQL VIEW MyView AS SELECT * FROM Northwind!Customers WHERE Country="Mexico"

    CREATE CURSOR mycursor (char1 C(10), char2 C(10) NOCPTRANS,  memo1 M, memo2 M NOCPTRANS)

    SELECT * FROM Customer INTO CURSOR Customer
    SELECT 1 FROM Customer TO SCREEN
    SELECT 1 FROM Customer TO SCREEN NOCONSOLE PLAIN

      SELECT a.cust_id, a.company, b.amount ;
      FROM customer a, payments b ;
      GROUP BY a.company      ;
      HAVING a.company != 'a' ;
      INTO ARRAY aValues
      
      
      
    SELECT FirstName, LastName FROM Customer  INTO Table LocalCust DATABASE MyDb NAME LocalCustomers
    SELECT FirstName, LastName FROM Customer  INTO Table LocalCust DATABASE MyDb 
    SELECT FirstName, LastName FROM Customer  INTO Table LocalCust.DBF 
    SELECT FirstName, LastName FROM Customer WHERE Lastname ="Hulst" TO SCREEN
    SELECT FirstName, LastName FROM Customer,City WHERE Lastname ="Hulst" TO PRINTER PROMPT
    SELECT FirstName, LastName FROM Customer,State WHERE Lastname ="Hulst" TO FILE "CustomerList.txt" 
    INSERT INTO employee (emp_no, fname, lname, officeno) ;
       VALUES (3022, "John", "Smith", 2101)
    INSERT INTO Cust2 FROM MEMVAR
    INSERT INTO OrdersArchive (order_id, order_date, ship_name) ;
    SELECT order_id, order_date, ship_name FROM Orders ;
      WHERE order_date >= (DATE()-30)

    aValues := {"Robert","van der Hulst"}
    INSERT INTO Customer(FirstName, LastName) FROM ARRAY aValues
    objectName := Custom{}     
    AddProperty(objectName,"FirstName","Robert")
    AddProperty(objectName,"LastName","van der Hulst")
    INSERT INTO Customer(FirstName, LastName) FROM NAME objectName     
    DELETE  MyProducts FROM MSRPList ;
          WHERE MSRPList.ProdID = MyProducts.ProdID;
          AND MSRPList.discontinued = .t.

    DELETE FROM Customers WHERE Lastname = "Hulst"
    DELETE FROM FORCE Customers WHERE Lastname = "Hulst"
    DELETE NorthWind!Customers FROM Customers WHERE Lastname = "Hulst"
    UPDATE Customers SET FirstName = "Robert", Dob = 8-6-1959 where Lastname = "Hulst"
    //UPDATE SET FirstName = "Robert", Dob = 8-6-1959 where Lastname = "Hulst"
    //CREATE TRIGGER ON customer FOR UPDATE AS maxordamt <= 50
    //DELETE TRIGGER ON customer FOR UPDATE  && Remove the trigger

RETURN


FUNCTION __FoxSqlCommand(cCommand AS STRING) AS VOID
    ? "FoxSqlCommand", cCommand
    RETURN     

FUNCTION __FoxSqlSelect(cCommand AS STRING) AS VOID
    ? "FoxSqlSelect", cCommand
    RETURN     

FUNCTION __FoxSqlSelectArray(cCommand AS STRING, aData AS ARRAY) AS VOID
    ? "__FoxSqlSelectArray", cCommand, aData
    RETURN   

FUNCTION __FoxSqlSelectCursor(cCommand AS STRING, cCursor AS STRING) AS VOID
    ? "__FoxSqlSelectCursor", cCursor
    ? "__FoxSqlSelectCursor", cCommand
    RETURN   


FUNCTION __FoxSqlSelectTable(cCommand AS STRING, cTableName AS STRING, cDatabaseName := "" AS STRING, cLongName := "" AS STRING) AS VOID
    ? "__FoxSqlSelectTable", cTableName, cDatabaseName, cLongName
    ? "__FoxSqlSelectTable", cCommand
    RETURN   


FUNCTION __FoxSqlInsertMemVar(cDbfAndFields AS STRING)  AS VOID
    ? "FoxSqlInsertMemVar", cDbfAndFields
FUNCTION __FoxSqlInsertArray(cDbfAndFields AS STRING, aValues AS ARRAY,cArrayName AS STRING) AS VOID
    ? "FoxSqlInsertArray", cDbfAndFields, ALen(aValues), cArrayName
FUNCTION __FoxSqlInsertObject(cDbfAndFields AS STRING, oObject AS OBJECT,cObjectName AS STRING) AS VOID
    ? "FoxSqlInsertObject", cDbfAndFields, oObject:ToString(), cObjectName
    
FUNCTION __FoxSqlDelete(deleteCommand AS STRING) AS VOID
    ? "FoxSqlDelete", deleteCommand

FUNCTION __FoxSqlUpdate(updateCommand AS STRING) AS VOID
    ? "FoxSqlUpdate", updateCommand

FUNCTION __FoxSqlCreateCursor(createCommand AS STRING) AS VOID
    ? "FoxSqlCreateCursor", createCommand    


FUNCTION __FoxSqlCreateTable(createCommand AS STRING) AS VOID
    ? "FoxSqlCreateTable", createCommand    
    
FUNCTION __FoxSqlCreateView(createCommand AS STRING) AS VOID
    ? "FoxSqlCreateView", createCommand        

