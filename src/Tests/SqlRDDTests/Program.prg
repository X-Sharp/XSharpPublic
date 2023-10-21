using XSharp.RDD.SqlRDD
using System.Data
using MySql.Data.MySqlClient
using Advantage.Data
using System.Collections.Generic

global options := "TrimTrailingSpaces=False;UseNulls=False;" as STRING

global SqlConnStr := "Server=(local);Initial catalog=Northwind;Trusted_Connection=True;"+options as STRING
global ODBCConnStr := "Driver={SQL Server};Server=(local);Database=Northwind;Trusted_Connection=Yes;"+options as STRING
global OleDbConnStr := "Provider=sqloledb;Data Source=(local);Initial Catalog=Northwind;Integrated Security=SSPI;"+options as STRING
global showEvents := true as logic

function Start as void
    RegisteredRDD.Add( RegisteredRDD{"SQLRDD", typeof(SQLRDD)})
    //TestProviders()
    //TestSqlServer()
    //TestODBC()
    //TestOLEDB()
    //     TestRDDODBC()
    //     TestRDDOLEDB()
    //TestRDDSql()
    //TestCommandSql()
    //TestCommandODBC()
    //TestCommandOLEDB()
    //TestParametersODBC()
    //TestParametersSQL()
    //TestTable()
    //testCreate()
    wait
    return

function TestCreate() as void
    local aStruct as array
    SqlDbSetProvider("SQLSERVER")
    var handle := SqlDbOpenConnection(SqlConnStr, EventHandler)
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
        var handle := SqlDbOpenConnection(SqlConnStr, EventHandler)
        VoDbUseArea(true, "SQLRDD","Customers","Customers",true, true)
        ? "SetDeleted(TRUE)"
        SetDeleted(true)
        dbGoTop()
        do while ! Eof() .and. Recno() < 10
            ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3)
            DbSkip(1)
        enddo
        wait
        ? "SetDeleted(FALSE)"
        SetDeleted(false)
        dbGoTop()
        do while ! Eof() .and. Recno() < 10
            ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3)
            DbSkip(1)
        enddo
        wait
        ? "Order by address"
        DbSetOrder("Address")
        dbGoTop()
        do while ! Eof() .and. Recno() < 10
            ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3),  FieldGetSym(#Country),  FieldGetSym(#City)
            DbSkip(1)
        enddo
        ListSeek()
        wait
        ListBrazil()
        VODbCloseArea()


function ListSeek as void
        ? "Seek Customer with key ALFKI"
        DbSetOrder("Customers_Pk")
        DbSeek("ALFKI")
        do while ! Eof() .and. Recno() < 10
            ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3)
            DbSkip(1)
        enddo
        ? "Seek Customer with key GOURL"
        DbSeek("GOURL")
        do while ! Eof() .and. Recno() < 10
            ? Recno(), Deleted(), FieldGet(1), FieldGet(2), FieldGet(3)
            DbSkip(1)
        enddo


function ListBrazil as void
        ? "Scope Customers in Brazil and Canada"
        DbSetOrder("Address")
        DBOrderInfo(DBOI_SCOPETOP,,,"BRAZIL")
        DBOrderInfo(DBOI_SCOPEBOTTOM,,,"CANADA")
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
        SqlDbSetProvider("ODBC")
        var handle := SqlDbOpenConnection(OdbcConnStr, EventHandler)
        conn := SqlDbGetConnection(handle)
        cmd := SqlDbCommand{"TEST", conn}
        cmd:CommandText := "Select * from Customers where Country = ?"
        cmd:AddParameter(1, "Germany1")
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
    local conn as SqlDbConnection
    local cmd  as SqlDbCommand
    TRY
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
        SqlDbSetProvider(cDriver)
        var handle := SqlDbOpenConnection(cConn, EventHandler)
        conn := SqlDbGetConnection(handle)
        var stmt := SqlDbCreateSqlStatement(handle)
        var res := SqlDbExecuteSQLDirect(stmt, "Select count(*) from Customers")
        ? res
        SqlDbCloseStatement(stmt)
        SqlDbCloseConnection(handle)
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
    VoDbCloseArea()
    SqlDbCloseConnection(handle)

function TestRDDSql() as void
    testRDD("SqlServer", SqlConnStr)

function TestRDDODBC() as void
    testRDD("ODBC", ODBCConnStr)

function TestRDDOLEDB() as void
    testRDD("OLEDB", OleDbConnStr)

function DumpCustomers() as VOID
    VoDbUseArea(true, "SQLRDD","Select * from Customers","Customers",true, true)
    ? FCount(), RecCount()
    local nI as DWORD
    For nI  := 1 to FCount()
        ? FieldName(nI), DbFieldInfo(DBS_ALIAS, nI)
    next
    DO WHILE ! Eof()
        ? Recno()
        For nI  := 1 to Min(FCount(),5)
            ?? "; ", FieldGet(nI)
        next
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

Function DumpConnection(conn as SqlDbConnection) as void
    var secs := Seconds()
    if conn != NULL
        ? "Provider",conn:Provider:Name
        ? "Handle  ",conn:Handle
        ? "ConnStr ",conn:DbConnection:ConnectionString
        ? "State   ",conn:DbConnection:State:ToString()
        ? "IsOpen  ", conn:IsOpen
        //         var coll := conn:GetMetaDataCollections()
        //         ? "Collections: ", coll:Count
        //         foreach var name in coll
        //             ? Name
        //         next
        var tables := conn:GetTables("TABLE")
        ? "# of Tables : ", tables:Count
        foreach var name in tables
            try
                DumpStructure(conn:GetStructureForTable(name, true,"*"))
            end try
        next
    ENDIF
    ? Seconds() - secs
function DumpStructure(oTd as SqlDbTableDef) as VOID
        ? "Table", oTd:Name, "Columns", oTd:Columns:Count
        foreach oCol as SqlDbColumnDef in oTd:Columns
            ? oCol:Name, oCol:ColumnInfo:ColumnName, oCol:ColumnInfo:FieldTypeFlags, oCol:Type:Name, oCol:Length, oCol:Scale, oCol:Precision
        next
        ?

FUNCTION EventHandler(oSender AS Object, e AS XSharp.RDD.SqlRDD.SqlRddEventArgs) AS OBJECT
    local strValue as string
    if e:Value is IList<string>  var listValue
        strValue := List2String(listValue)
    else
        strValue := e:Value:ToString()
    endif
    if showEvents
        ? "Event", e:Reason:ToString(), e:Table, strValue
    endif
    return e:Value

function TestProviders as void
    local oProv as SqlDbProvider
    local oProvider := MySqlClientFactory.Instance
    oProvider := Advantage.Data.Provider.AdsFactory.Instance
    local aTest := {"Advantage","ODBC","OLEDB","SQLSERVER","MySql","ORACLE"}
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
            catch e as exception
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


Function DumpExpression(oExpr as SqlDbExpression) AS VOID
    ? "Expression", oExpr:XsKey
    foreach var oSeg in oExpr:Segments
        ? "Seg", oSeg:Key
    next
    ? "Order", oExpr:OrderListString
    ? "Cols ",oExpr:ColumnListString
    ? "SqlKey", oExpr:SQLKey
    ?






