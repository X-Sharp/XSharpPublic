using XSharp.RDD.SqlRDD
using System.Data
using MySql.Data.MySqlClient
using Advantage.Data
using System.Collections.Generic

global SqlConnStr := "Server=(local);Initial catalog=Northwind;Trusted_Connection=True;TrimTrailingSpaces=True;" as STRING
global ODBCConnStr := "Driver={SQL Server};Server=(local);Database=Northwind;Trusted_Connection=Yes;TrimTrailingSpaces=True;" as STRING
global OleDbConnStr := "Provider=sqloledb;Data Source=(local);Initial Catalog=Northwind;Integrated Security=SSPI;TrimTrailingSpaces=True;" as STRING


function Start as void
    //TestProviders()
    //TestSqlServer()
    //TestODBC()
    //TestOLEDB()
    //     TestRDDODBC()
    //     TestRDDOLEDB()
    //     TestRDDSql()
    TestCommandSql()
    TestCommandODBC()
    TestCommandOLEDB()
    wait
    return

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
    VoDbUseArea(TRUE, typeof(SQLRDD),"Select * from tblProjecten","tblProjecten",TRUE, TRUE)
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
        ? "Tables: ", tables:Count
        foreach var name in tables
            DumpStructure(conn:GetStructure(name))
        next
    ENDIF
    ? Seconds() - secs
function DumpStructure(oTd as SqlDbTableDef) as VOID
    //     ? "Table", oTd:Name, "Columns", oTd:Columns:Count
    //     Foreach oCol as SqlDbColumnDef in oTd:Columns
    //         ? oCol:Name, oCol:Type:Name, oCol:Length, oCol:Scale, oCol:Precision
    //     next
    //     ?

FUNCTION EventHandler(oSender AS Object, e AS XSharp.RDD.SqlRDD.SqlRddEventArgs) AS OBJECT
    local strValue as string
    if e:Value is IList<String>  var listValue
        strValue := List2String(listValue)
    else
        strValue := e:Value:ToString()
    endif

    ? "Event", e:Reason:ToString(), e:Table, strValue
    RETURN e:Value

function TestProviders as void
    local oProv as SqlDbProvider
    local oProvider := MySqlClientFactory.Instance
    oProvider := Advantage.Data.Provider.AdsFactory.Instance
    local aTest := {"Advantage","ODBC","OLEDB","SQLSERVER","MySql"}
    foreach strProv as STRING in aTest
        SqlDbSetProvider(strProv)
        oProv := SqlDbGetProvider()
        ? "Name      ", oProv:Name
        ? "TopStmt   ", oProv:SelectTopStatement
        ? "Left()    ", oProv:GetFunction("LEFT(%1%,%2%)")
        ? "Alltrim() ", oProv:GetFunction("ALLTRIM(%1%)")
        ? "DTOS()    ", oProv:GetFunction("DTOS(%1%)")
        ? "Delimiters", oProv:QuotePrefix, oProv:QuoteSuffix
    next
    wait

FUNCTION Start1(args AS STRING[]) AS VOID
    local oExpr as SqlDbExpression
    SqlDbExpression.SqlTranslator := SqlTranslator
    oExpr := SqlDbExpression{NULL, "LASTNAME+FIRSTNAME"}
    DumpExpression(oExpr)
    SqlDbExpression{NULL, "STR(CUSTNO)+DESCEND(DTOS(ORDERDATE))"}
    DumpExpression(oExpr)
    oExpr := SqlDbExpression{NULL, "STR(CUSTNO)+IIF(EMPTY(LASTNAME),REPL('Z',10), LEFT(LASTNAME,10))"}
    DumpExpression(oExpr)


    WAIT


FUNCTION SqlTranslator(cFunc as String) as String
    var oProv := SqlDbProvider.GetProvider("ORACLE")
    var sqlFunc := oProv:GetFunction(cFunc)
    if String.IsNullOrEmpty(sqlFunc)
        return cFunc
    endif
    return sqlFunc


Function DumpExpression(oExpr as SqlDbExpression) AS VOID
    ? "Expression", oExpr:VoKey
    foreach var oSeg in oExpr:Segments
        ? "Seg", oSeg:Key
    next
    ? "Order", oExpr:OrderListString
    ? "Cols ",oExpr:ColumnListString
    ? "SqlKey", oExpr:SQLKey
    ?




