﻿using XSharp.RDD.SqlRDD
using XSharp.RDD.SqlRDD.Providers
using System.Data
using System.Diagnostics
using System.Data.Common
using MySql.Data.MySqlClient
using System.Collections.Generic

global options := "TrimTrailingSpaces=False;UseNulls=False;" as STRING

global SqlConnStr := "Server=(local);Initial catalog=Northwind;Trusted_Connection=True;LegacyFieldTypes=False;"+options as STRING
global ODBCConnStr := "Driver={SQL Server};Server=LEDA;Database=Northwind;Trusted_Connection=Yes;"+options as STRING
global OleDbConnStr := "Provider=sqloledb;Data Source=LEDA;Initial Catalog=Northwind;Integrated Security=SSPI;"+options as STRING
global showEvents := true as logic

function Start as void
    TestBeta3()
    //TestProviders()
    //TestSqlServer()
    //TestODBC()
    //TestOLEDB()
    //     TestRDDODBC()
    //     TestRDDOLEDB()
    //TestRDDSql()

//     TestCommandSql()
//     TestCommandODBC()
//     TestCommandOLEDB()
//     TestParametersODBC()
//     TestParametersSQL()
//     TestParametersOLEDB()
    //TestTable()
    //TestCreateIndex()
    //TestServerFilter()
    //TestTableRecno()
    //TestTransaction()
    //testCreate()
    //FillGsTutor()
    //TestGsTutor()
    wait
    return

    function TestBeta3() as void
        SqlDbSetProvider("SQLSERVER")
        var handle := SqlDbOpenConnection(SqlConnStr)
        DbUseArea(TRUE,"SQLRDD","CUSTOMER")
        DbZap()
        DbAppend()
        FieldPut(1,"AAA")
        DbAppend()
        FieldPut(1,"BBB")
        DbSkip()
        ? LastRec()
        DbCloseArea()

        DbUseArea(TRUE,"SQLRDD","CUSTOMER")
        ? FieldGet(1) // AAA, OK
        FieldPut(1,"CCC")
        DbSkip()
        ? FieldGet(1) // Elizabeth, wrong!
        FieldPut(1,"DDD")
        DbCloseArea()
        wait
        return

function TestCreate() as void
    local aStruct as array
    SqlDbSetProvider("SQLSERVER")
    var handle := SqlDbOpenConnection(SqlConnStr)

    ? handle
    aStruct := {{"Key","I:+",4,0},{"FirstName","C",10,0},{"LastName","C",10,0},{"DOB","D",8,0}, {"Salary","Y",10,2},{"Married","L",1,0},{"Notes","M",10,0}}
    ? DbCreate("TEST",aStruct,"SQLRDD")
    VoDbUseArea(true, "SQLRDD","TEST","TEST",true, false)
    ? DbAppend()
    FieldPut(2, "John")
    FieldPut(3, "Doe")
    FieldPut(4, 1960.01.01)
    FieldPut(5, $12345)
    FieldPut(6, true)
    FieldPut(7, "Some Notes")
    ? "Key before",FieldGet(1)
    DbCommit()
    ? "Key after",FieldGet(1)
    DbGoTop()
    FieldPut(2, "Jane")
    FieldPut(4, 1962.02.02)
    DbCommit()
    for var i:= 1 to 10
        DbAppend()
        FieldPut(2, "F"+NTrim(i))
        FieldPut(3, "L"+NTrim(i))
        FieldPut(4, 1970.01.01+i)
        FieldPut(7, "Notes "+Str(i))
        ? "Key before",FieldGet(1)
        DbCommit()
        ? "Key after",FieldGet(1)
    next
    ? "LastRec",LastRec()
    for var i:= 1 to 10 step 2
        DbGoto(i)
        ? "Delete Recno", Recno()
        RLock()
        DbDelete()
        ? DbCommit()
    next
    ? "LastRec",LastRec()
    DbCloseArea()
    wait

function TestTable() as void
    SqlDbSetProvider("SQLSERVER")
    var handle := SqlDbOpenConnection(SqlConnStr)
    var conn   := SqlDbGetConnection(handle)
    //conn:MetadataProvider := SqlMetaDataProviderDatabase{conn}
    conn:CallBack += @@EventHandler
    ? handle
    VoDbUseArea(true, "SQLRDD","Customers","Customers",true, true)
    ? Cdx(1)
    DbSetIndex("Customers.sdx")
    ? Cdx(2)
    DumpIndexes()

    ? "SetDeleted(TRUE)"
    SetDeleted(true)
    DbGoTop()
    do while ! Eof() .and. Recno() < 10
        ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3)
        DbSkip(1)
    enddo
    wait
    Cls()
    ? "SetDeleted(FALSE)"
    SetDeleted(false)
    DbGoTop()
    do while ! Eof() .and. Recno() < 10
        ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3)
        DbSkip(1)
    enddo
    wait
    Cls()
    ? "Order by address"
    DbSetOrder("Address")
    DbGoTop()
    do while ! Eof() .and. Recno() < 10
        ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3),  FieldGetSym(#Country),  FieldGetSym(#City)
        DbSkip(1)
    enddo
    wait
    Cls()
    ListSeek()
    wait
    Cls()
    ListBrazil()
    wait
    // Test creating an index
    ? "Create Index on City"
    DbCreateOrder("City",,"City")
    DbCreateOrder("Name",,"ContactName")
    DbSetIndex("Customers.sdx")
    DumpIndexes()
    ? DbSetOrder("City")
    DbGoTop()
    var nI := 0
    ? "By city"
    DO WHILE ! Eof() .and. ++nI < 10
        ? Recno(), FieldGetSym(#City), FieldGetSym(#CustomerID), FieldGetSym(#ContactName)
        DbSkip(1)
    ENDDO
    ? "By name"
    ? DbSetOrder("Name")
    DbGoTop()
    nI := 0
    DO WHILE ! Eof() .and. ++nI < 10
        ? Recno(), FieldGetSym(#City), FieldGetSym(#CustomerID), FieldGetSym(#ContactName)
        DbSkip(1)
    ENDDO

    VODbCloseArea()


function TestCreateIndex() as void
    SqlDbSetProvider("SQLSERVER")
    var handle := SqlDbOpenConnection(SqlConnStr)
    var conn   := SqlDbGetConnection(handle)
    conn:MetadataProvider := SqlMetaDataProviderDatabase{conn}
    conn:CallBack += @@EventHandler
    ? handle
    VoDbUseArea(true, "SQLRDD","Customers","Customers",true, true)
    DbCreateOrder("Name",,"Upper(ContactName)")
    DbCreateOrder("City",,"Upper(City)")
    //DbDeleteOrder("Name")
    //DbDeleteOrder("City")
    VODbCloseArea()

function ListSeek as void
    ? "Seek Customer with key ALFKI"
    DbSetOrder("PK")
    DbSeek("ALFKI")
    var iCounter := 0
    do while ! Eof() .and. iCounter++ < 5
        ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3)
        DbSkip(1)
    enddo
    ? "Seek Customer with key GOURL"
    DbSeek("GOURL")
    iCounter := 0
    do while ! Eof() .and. iCounter++ < 5
        ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3)
        DbSkip(1)
    enddo


function ListBrazil as void
    ? "Scope Customers in Brazil and Canada"
    DbSetOrder("Address")
    DbOrderInfo(DBOI_SCOPETOP,,,"BRAZIL")
    DbOrderInfo(DBOI_SCOPEBOTTOM,,,"CANADA")
    DbGoTop()
    do while ! Eof()
        ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3), FieldGetSym(#Country)
        DbSkip(1)
    enddo
    return


function TestParametersODBC() AS VOID
    local conn := null as SqlDbConnection
    local cmd  := null as SqlDbCommand
    TRY
        ? "Testing Parameters with ODBC"
        var stopWatch := Stopwatch{}
        stopWatch:Start()

        SqlDbSetProvider("ODBC")
        var handle := SqlDbOpenConnection(ODBCConnStr, EventHandler)
        conn := SqlDbGetConnection(handle)
        cmd := SqlDbCommand{"TEST", conn}
        cmd:CommandText := "Select * from Customers where CustomerId like ? or Country = ?"
        cmd:AddParameter(1, "A%")
        cmd:AddParameter(2, "Germany")
        var oTable := cmd:GetDataTable("Customers")
        ? "Command   :",cmd:CommandText
        ? "Parameters:",cmd:ParameterList
        ? "Rows      :",oTable:Rows:Count
        if oTable:Rows:Count > 0
            var row := oTable:Rows[0]
            foreach col as DataColumn in oTable:Columns
                ? col:ColumnName, row[col]
            next
        else
            foreach col as DataColumn in oTable:Columns
                ? col:ColumnName
            next
        endif
        ? stopWatch:Elapsed:ToString()
        stopWatch:Stop()
    CATCH e as Exception
        ? e:Message
        if e:InnerException != null
            ? e:InnerException:Message
        endif

    FINALLY
        IF cmd != NULL
            cmd:Close()
        ENDIF
        IF conn != NULL
            conn:Close()
        ENDIF


    END TRY
    WAIT
    RETURN
function TestParametersOleDb() AS VOID
    local conn := null as SqlDbConnection
    local cmd  := null as SqlDbCommand
    TRY
        ? "Testing Parameters with OLEDB"
        var stopWatch := System.Diagnostics.Stopwatch{}
        stopWatch:Start()

        SqlDbSetProvider("OLEDB")
        var handle := SqlDbOpenConnection(OleDbConnStr, EventHandler)
        conn := SqlDbGetConnection(handle)
        cmd := SqlDbCommand{"TEST", conn}
        cmd:CommandText := "Select * from Customers where CustomerId like ? or Country = ?"
        cmd:AddParameter(1, "A%")
        cmd:AddParameter(2, "Germany")
        var oTable := cmd:GetDataTable("Customers")
        ? "Command   :",cmd:CommandText
        ? "Parameters:",cmd:ParameterList
        ? "Rows      :",oTable:Rows:Count
        if oTable:Rows:Count > 0
            var row := oTable:Rows[0]
            foreach col as DataColumn in oTable:Columns
                ? col:ColumnName, row[col]
            next
        else
            foreach col as DataColumn in oTable:Columns
                ? col:ColumnName
            next
        endif
        ? stopWatch:Elapsed:ToString()
        stopWatch:Stop()
    CATCH e as Exception
        ? e:Message
        if e:InnerException != null
            ? e:InnerException:Message
        endif

    FINALLY
        IF cmd != NULL
            cmd:Close()
        ENDIF
        IF conn != NULL
            conn:Close()
        ENDIF


    END TRY
    WAIT
    RETURN
function TestParametersSQL() AS VOID
    local conn := NULL as SqlDbConnection
    local cmd  := NULL as SqlDbCommand
    TRY
        ? "Testing Parameters with SQLSERVER"
        var stopWatch := System.Diagnostics.Stopwatch{}
        stopWatch:Start()
        SqlDbSetProvider("SQLServer")
        var handle := SqlDbOpenConnection(SqlConnStr, EventHandler)
        conn := SqlDbGetConnection(handle)
        cmd := SqlDbCommand{"TEST", conn}
        cmd:CommandText := "Select * from Customers where Customerid like @id or Country = @country"
        cmd:AddParameter("@id", "A%")
        cmd:AddParameter("@country", "Germany")
        cmd:BindParameters()
        var oTable := cmd:GetDataTable("Customers")
        ? "Command   :",cmd:CommandText
        ? "Parameters:",cmd:ParameterList
        ? "Rows      :",oTable:Rows:Count
        if oTable:Rows:Count > 0
            var row := oTable:Rows[0]
            foreach col as DataColumn in oTable:Columns
                ? col:ColumnName, row[col]
            next
        else
            foreach col as DataColumn in oTable:Columns
                ? col:ColumnName
            next
        endif
        ? stopWatch:Elapsed:ToString()
        stopWatch:Stop()

    CATCH e as Exception
        ? e:Message
        if e:InnerException != null
            ? e:InnerException:Message
        endif
    FINALLY
        IF cmd != NULL
            cmd:Close()
        ENDIF
        IF conn != NULL
            conn:Close()
        ENDIF


    END TRY
    WAIT
    RETURN


function TestCommandSql() as void
    TestCommand("SqlServer", SqlConnStr)

function TestCommandODBC() as void
    TestCommand("ODBC", ODBCConnStr)

function TestCommandOLEDB() as void
    TestCommand("OLEDB", OleDbConnStr)

FUNCTION TestCommand(cDriver as string, cConn as STRING) AS VOID
    local conn as SqlDbConnection
    TRY
        var stopWatch := System.Diagnostics.Stopwatch{}
        ? "Testing Command object with ", cDriver
        stopWatch:Start()
        SqlDbSetProvider(cDriver)
        var handle := SqlDbOpenConnection(cConn, EventHandler)
        conn := SqlDbGetConnection(handle)
        ? stopWatch:Elapsed:ToString()
        ? conn:ConnectionString
        ? conn:DbConnection:ConnectionString
        var hCommand := SqlDbCreateSqlCommand(handle)
        var res := SqlDbExecuteSQLDirect(hCommand, "Select count(*) from Customers")
        ? res
        SqlDbCloseCommand(hCommand)
        // now use command object directly
        using var oCmd := SqlDbCommand{"TEST", conn}
        oCmd:CommandText := "Select count(*) from Customers"
        res := oCmd:ExecuteScalar()
        ? res
        SqlDbCloseConnection(handle)
        ? stopWatch:Elapsed:ToString()
        stopWatch:Stop()
    CATCH e as Exception
        ? e:Message
        if e:InnerException != null
            ? e:InnerException:Message
        endif
    END TRY
    WAIT
    RETURN
Function TestRDD(cDriver as string, cConn as STRING) as void
    //Console.Clear()
    var secs := Seconds()
    SqlDbSetProvider(cDriver)
    var handle := SqlDbOpenConnection(cConn, EventHandler)
    DumpCustomers()
    ? Seconds() - secs
    //WAIT
    VODbCloseArea()
    SqlDbCloseConnection(handle)

function TestRDDSql() as void
    TestRDD("SqlServer", SqlConnStr)

function TestRDDODBC() as void
    TestRDD("ODBC", ODBCConnStr)

function TestRDDOLEDB() as void
    TestRDD("OLEDB", OleDbConnStr)

function DumpCustomers() as VOID
    VoDbUseArea(true, "SQLRDD","select * from Customers","Customers",true, true)
    ? FCount(), RecCount()
    local nI as DWORD
    For nI  := 1 to FCount()
        ? FieldName(nI), DbFieldInfo(DBS_ALIAS, nI)
    next
    DbGoTop()
    DO WHILE ! Eof()
        ? Recno()
        For nI  := 1 to Min(FCount(),5)
            ?? "; ", FieldGet(nI)
        next
        DbSkip(1)
    ENDDO
    // Test creating an index
    ? "Create Index on City", DbCreateOrder("City",,"City")
    ? "Create Index on name", DbCreateOrder("Name",,"ContactName")
    DumpIndexes()
    OrdSetFocus("City")
    DbGoTop()
    nI := 0
    ? "By city"
    DO WHILE ! Eof() .and. ++nI < 10
        ? Recno(), FieldGetSym(#City), FieldGetSym(#CustomerID), FieldGetSym(#ContactName)
        DbSkip(1)
    ENDDO
    ? "By name"
    OrdSetFocus("Name")
    DbGoTop()
    nI := 0
    DO WHILE ! Eof() .and. ++nI < 10
        ? Recno(), FieldGetSym(#City), FieldGetSym(#CustomerID), FieldGetSym(#ContactName)
        DbSkip(1)
    ENDDO


function TestSqlServer as void
    Console.Clear()
    SqlDbSetProvider("SqlServer")
    var handle := SqlDbOpenConnection(SqlConnStr)
    var conn := SqlDbGetConnection(handle)
    DumpConnection(conn)
    SqlDbCloseConnection(handle)

function TestODBC as void
    Console.Clear()
    SqlDbSetProvider("ODBC")
    var handle := SqlDbOpenConnection(ODBCConnStr)
    var conn := SqlDbGetConnection(handle)
    DumpConnection(conn)
    SqlDbCloseConnection(handle)
function TestOLEDB as void
    Console.Clear()
    SqlDbSetProvider("OLEDB")
    var handle := SqlDbOpenConnection(OleDbConnStr)
    var conn := SqlDbGetConnection(handle)
    DumpConnection(conn)
    SqlDbCloseConnection(handle)

function DumpMetadataCollection(cName as STRING, table as DataTable) as void
    ? "Collection", cName, "Rows", table:Rows:Count
    ?
    if table:Rows:Count > 0
        var colNo:=0
        foreach col as DataColumn in table:Columns
            if colNo > 0
                ?? " "
            endif
            ?? col:ColumnName
            if ++colNo > 10
                exit
            endif

        next
        ?
        foreach row as DataRow in table:Rows
            colNo:=0
            foreach col as DataColumn in table:Columns
                if colNo > 0
                    ?? " "
                endif
                ?? row[col]
                if ++colNo > 10
                    exit
                endif
            next
            ?
        next
    endif

Function DumpConnection(conn as SqlDbConnection) as void
    var secs := Seconds()
    if conn != NULL
        ? "Provider",conn:Provider:Name
        ? "Handle  ",conn:Handle
        ? "ConnStr ",conn:DbConnection:ConnectionString
        ? "State   ",conn:DbConnection:State:ToString()
        ? "IsOpen  ", conn:IsOpen
        //var coll := conn:GetMetaDataCollections()
//         ? "Collections: ", coll:Count
//         foreach var name in coll
//             var table := conn:GetMetaDataCollection(Name)
//             DumpMetadataCollection(Name, table)
//             Console.ReadLine()
//         next
        var tables := conn:GetTables("TABLE")
        ? "# of Tables : ", tables:Count
        foreach var Name in tables
            try
                DumpStructure(conn:GetStructureForTable(Name, null,"*"))
            catch
                nop
            end try
        next
    ENDIF
    ? Seconds() - secs
function DumpStructure(oTd as SqlDbTableInfo) as VOID
    ? "Table", oTd:Name, "Columns", oTd:Columns:Count
    foreach oCol as SqlDbColumnDef in oTd:Columns
        ? oCol:Name, oCol:ColumnInfo:ColumnName, oCol:ColumnInfo:FieldTypeFlags, oCol:Type:Name, oCol:Length, oCol:Scale, oCol:Precision
    next
    ?

FUNCTION EventHandler(oSender AS Object, e AS XSharp.RDD.SqlRDD.SqlRddEventArgs) AS OBJECT
    // Tags default
    switch e:Reason
        
    case SqlRDDEventReason.Condition
        e:Value := ""
    case SqlRDDEventReason.Unique
        e:Value := FALSE
    case SqlRDDEventReason.Expression
        switch e:Name
        case "Tag:Customers:PK"
            e:Value := "CustomerID"
        case "Tag:Customers:CompanyName"
            e:Value := "UPPER(CompanyName)"
        case "Tag:Customers:ContactName"
            e:Value := "UPPER(ContactName)"
        case "Tag:Customers:Address"
            e:Value := "Upper(Country)+Upper(City)"
        end switch
    case SqlRDDEventReason.Indexes
        switch e:Name
        case "Customers"
            e:Value := "Customers"
        end switch
    case SqlRDDEventReason.Tags
        switch e:Name
        case "Index:Customers"
            e:Value := "PK,CompanyName,ContactName,Address"
        end switch
     case SqlRDDEventReason.SeekReturnsSubset
         e:Value := TRUE
    end switch
    if showEvents
        ? "Event", e:Name, e:Reason:ToString(), e:Value
    endif
    return e:Value

function TestProviders as void
    local oProv as ISqlDbProvider
    local oProvider := MySqlClientFactory.Instance AS DbProviderFactory
    ? oProvider:CreateConnection():GetType():FullName
    local aTest := {"ODBC","OLEDB","SQLSERVER","MySql","ORACLE", "PostgreSql"} // "Advantage",
    foreach strProv as STRING in aTest
        if SqlDbSetProvider(strProv)
            oProv := SqlDbGetProvider()
            ? "Name      ", oProv:Name
            ? "TopStmt   ", oProv:SelectTopStatement
            ? "Left()    ", oProv:GetFunction("LEFT(%1%,%2%)")
            ? "Alltrim() ", oProv:GetFunction("ALLTRIM(%1%)")
            ? "DTOS()    ", oProv:GetFunction("DTOS(%1%)")
            try
                ? "Quotes    ", oProv:QuoteIdentifier("somename")
            catch e as Exception
                ? "NO Quotes for ", strProv, e:Message
            end try
            ?
        else
            ? "Could not load provider "+strProv
            wait
        endif
    next
    wait

    // FUNCTION Start1(args AS STRING[]) AS VOID
    //     local oExpr as SqlDbExpression
    //     SqlDbExpression.SqlTranslator := SqlTranslator
    //     oExpr := SqlDbExpression{NULL, "LASTNAME+FIRSTNAME"}
    //     DumpExpression(oExpr)
    //     SqlDbExpression{NULL, "STR(CUSTNO)+DESCEND(DTOS(ORDERDATE))"}
    //     DumpExpression(oExpr)
    //     oExpr := SqlDbExpression{NULL, "STR(CUSTNO)+IIF(EMPTY(LASTNAME),REPL('Z',10), LEFT(LASTNAME,10))"}
    //     DumpExpression(oExpr)
    //

    //    WAIT


FUNCTION SqlTranslator(cFunc as String) as String
    var oProv := SqlDbProvider.GetProvider("ORACLE")
    var sqlFunc := oProv:GetFunction(cFunc)
    if String.IsNullOrEmpty(sqlFunc)
        return cFunc
    endif
    return sqlFunc


// Function DumpExpression(oExpr as SqlDbExpression) AS VOID
//     ? "Expression", oExpr:XsKey
//     foreach var oSeg in oExpr:Segments
//         ? "Seg", oSeg:Key
//     next
//     ? "Order", oExpr:OrderListString
//     ? "Cols ",oExpr:ColumnListString
//     ? "SqlKey", oExpr:SQLKey
//     ?
function DumpIndexes as Void
    // Dump the indexes and orders
    ? "Bags", DbOrderInfo(DBOI_BAGCOUNT)
    ? "Indexes", DbOrderInfo(DBOI_ORDERCOUNT)
    var focus := OrdSetFocus()
    OrdSetFocus(0)
    var Count := DbOrderInfo(DBOI_ORDERCOUNT)
    FOR var i := 1 TO Count
        OrdSetFocus(i)
        ? "Order", i, DbOrderInfo(DBOI_FULLPATH), DbOrderInfo(DBOI_BAGNAME), DbOrderInfo(DBOI_NAME), DbOrderInfo(DBOI_EXPRESSION), DbOrderInfo(DBOI_CONDITION)
    NEXT
    OrdSetFocus(focus)
    WAIT

function TestServerFilter as Void
    SqlDbSetProvider("SQLSERVER")
    var handle := SqlDbOpenConnection(SqlConnStr)
    var conn   := SqlDbGetConnection(handle)
    conn:MetadataProvider := SqlMetaDataProviderDatabase{conn}
    conn:CallBack += @@EventHandler
    ? handle
    VoDbUseArea(true, "SQLRDD","OpenOrders","OpenOrders",true, true)

    SetDeleted(true)
    DbGoTop()
    do while ! Eof()
        ? Recno(),FieldGetSym(#OrderID), FieldGetSym(#CustomerID), FieldGetSym(#OrderDate), FieldGetSym(#ShippedDate)
        DbSkip(1)
    enddo
    DbCloseArea()
    SqlDbCloseConnection(handle)
    RETURN
function TestTableRecno as Void
    SqlDbSetProvider("SQLSERVER")
    var handle := SqlDbOpenConnection(SqlConnStr)
    var conn   := SqlDbGetConnection(handle)
    conn:MetadataProvider := SqlMetaDataProviderDatabase{conn}
    conn:CallBack += @@EventHandler
    VoDbUseArea(true, "SQLRDD","Orders","Orders",true, true)

    DbGoTop()
    var recnos := List<dword>{}
    do while ! Eof()
        recnos:Add(Recno())
        //? Recno(),FieldGetSym(#OrderID), FieldGetSym(#CustomerID), FieldGetSym(#OrderDate), FieldGetSym(#ShippedDate)
        VoDbSkip(1)
    enddo
    ? LastRec(), "Records found"
    var Count := 0
    foreach var Recno in recnos
        VoDbGoto(Recno)
        ? Recno, FieldGetSym(#OrderID), FieldGetSym(#CustomerID), FieldGetSym(#OrderDate), FieldGetSym(#ShippedDate)
        if ++Count == 20
            exit
        endif
    next

    DbCloseArea()
    RETURN
function TestTransaction()
    /*
    // Tell the RDD to use the SQLServer SqlDbProvider class
    // store the handle as hConn1
    SqlDbSetProvider("SQLSERVER")
    RddSetDefault("SQLRDD")
    // Open a connection to the local Northwind sample database for SqlServer
    // This connection will be the 'default' connection
    // store the handle as hConn1
    var hConn1 := SqlDbOpenConnection("Server=(local);Initial catalog=Northwind;Trusted_Connection=True;")
    var oConn1 := SqlDbGetConnection(hConn1)
    oConn1:CallBack += @@MyEventHandler

    // open a server
    var oServer := DbServer{"Customers"}
    oServer:GoTop()
    oConn1:BeginTrans()
    // write to the server
    oServer:FieldPut(#ContactName, "Jones")
    // commit and close
    oServer:Commit()
    oServer:Close()
    ? "Count Before Rollback"
    ? oConn1:ExecuteScalar("Select count(*) from Customers where ContactName = 'Jones'")
    wait

    // Rollback the changes
    oConn1:RollBackTrans()
    // Close the connection
    ? "Count After Rollback"
    ? oConn1:ExecuteScalar("Select count(*) from Customers where ContactName = 'Jones'")
    wait
    SqlDbCloseConnection(hConn1)
    */
    return true

    FUNCTION MyEventHandler(oSender AS Object, e AS XSharp.RDD.SqlRDD.SqlRddEventArgs) AS OBJECT
	? "Event", e:Name, e:Reason:ToString(), e:Value
	RETURN e:Value



    FUNCTION FillGsTutor() AS VOID
        local aOrders as array
        aOrders := {}
        TRY
            SqlDbSetProvider("SQLSERVER")
            SqlDbOpenConnection("Server=(local);Initial catalog=GsTutor;Trusted_Connection=True;", EventHandler)
            var oConn  := SqldbGetConnection("DEFAULT")
            oConn:MetadataProvider := SqlMetaDataProviderDatabase{oConn}
            oConn:UseNulls := FALSE
            RddSetDefault("SQLRDD")
            if !oConn:DoesTableExist("Customer")
                aOrders := {}
                USE ("c:\cavo28SP3\Samples\Gstutor\customer.dbf") via "DBFNTX"
                DbSetIndex("CustNum.ntx")
                aadd(aOrders, {"Custnum", OrdKey()})
                DbSetIndex("CustName.ntx")
                aadd(aOrders, {"Custname", OrdKey()})

                DbCopy("Customer")
                DbCloseAll()
                USE Customer
                foreach var item in aOrders
                    OrdCreate(,item[1], item[2])
                next
                DbCloseArea()
            endif
            if !oConn:DoesTableExist("Orders")
                aOrders := {}
                USE ("c:\cavo28SP3\Samples\Gstutor\Orders.dbf") via "DBFNTX"
                DbSetIndex("OrdCust.ntx")
                aadd(aOrders, {"OrdCust", OrdKey()})
                DbSetIndex("OrderNum.ntx")
                aadd(aOrders, {"OrderNum", OrdKey()})

                DbCopy("Orders")
                DbCloseAll()
                USE Orders
                foreach var item in aOrders
                    OrdCreate(,item[1], item[2])
                next
                DbCloseAll()
            endif
        CATCH e as Exception
            ? e:Message
        END TRY
        RETURN

FUNCTION TestGsTutor() AS VOID
//     local aOrders as array
//     aOrders := {}
    TRY
        SqlDbSetProvider("SQLSERVER")
        RddSetDefault("SQLRDD")
        SqlDbOpenConnection("Server=(local);Initial catalog=GsTutor;Trusted_Connection=True;", EventHandler)
        var conn  := SqldbGetConnection("DEFAULT")
        conn:MetadataProvider := SqlMetaDataProviderDatabase{conn}
        local oSrv := dbServer{"Customer"} as DbServer
        ShowArray(oSrv:DbStruct)
        oSrv:SetOrder("CustNum")
        var stopWatch := StopWatch{}
        stopWatch:Start()
        oSrv:Seek(10)
        ? stopWatch:Elapsed:ToString()
        stopWatch:Restart()
        ? "Recno", oSrv:Recno, oSrv:Eof, "Keycount",oSrv:OrderInfo(DBOI_KEYCOUNT), oSrv:OrderKeyVal
        ? stopWatch:Elapsed:ToString()
        stopWatch:Restart()
        oSrv:Seek(10.0)
        ? "Recno", oSrv:Recno, oSrv:Eof, "Keycount",oSrv:OrderInfo(DBOI_KEYCOUNT), oSrv:OrderKeyVal
        ? stopWatch:Elapsed:ToString()
        stopWatch:Restart()
        oSrv:Seek((INT64) 20)
        ? "Recno", oSrv:Recno, oSrv:Eof, "Keycount",oSrv:OrderInfo(DBOI_KEYCOUNT), oSrv:OrderKeyVal
        ? stopWatch:Elapsed:ToString()
        stopWatch:Restart()
        oSrv:Seek(20,TRUE)
        ? "Recno", oSrv:Recno, oSrv:Eof, "Keycount",oSrv:OrderInfo(DBOI_KEYCOUNT), oSrv:OrderKeyVal
        wait
        oSrv:CreateOrder("CustName","Customer","LastName+FirstName")
        oSrv:CreateOrder("CustName","CustNum","CustNum")
        oSrv:Close()
        DbUseArea(,,"orders")
        ShowArray(DbStruct())
        OrdSetFocus("Order_Date")
        stopWatch:Restart()
        DbSeek(DateTime.Now)
        ? stopWatch:Elapsed:ToString()
        ? "Recno", Recno(), Eof(), "Keycount", DbOrderInfo(DBOI_KEYCOUNT), DbOrderInfo(DBOI_KEYVAL)
        stopWatch:Restart()
        DbSeek(1993.10.11)
        ? stopWatch:Elapsed:ToString()
        ? "Recno", Recno(), Eof(), "Keycount", DbOrderInfo(DBOI_KEYCOUNT), DbOrderInfo(DBOI_KEYVAL)
        OrdSetFocus("OrdCust")
        OrdScope(TOPSCOPE, 10)
        OrdScope(BOTTOMSCOPE, 10)
        stopWatch:Restart()
        DbSeek(10,FALSE, FALSE)
        ? stopWatch:Elapsed:ToString()
        ? "Recno", Recno(), Eof(), "Keycount", DbOrderInfo(DBOI_KEYCOUNT), DbOrderInfo(DBOI_KEYVAL)
        stopWatch:Restart()
        DbSeek(10,FALSE, TRUE)
        ? stopWatch:Elapsed:ToString()
        ? "Recno", Recno(), Eof(), "Keycount", DbOrderInfo(DBOI_KEYCOUNT), DbOrderInfo(DBOI_KEYVAL)
        DbCloseArea()

    CATCH e as Exception
        ? e:ToString()
    END TRY
    RETURN



    FUNCTION GetGlobal() AS usual CLIPPER
        RETURN NULL_OBJECT
