using XSharp.RDD.SqlRDD
using System.Data
using MySql.Data.MySqlClient
using Advantage.Data

function Start as void
    //TestProviders()
    TestSqlServer()
    wait
    TestODBC()
    wait
    TestOLEDB()
    wait
    return

function TestSqlServer as void
    SqlDbSetProvider("SqlServer")
    var handle := SqlDbOpenConnection("Server=(local);Initial catalog=NorthWind;Trusted_Connection=True;")
    var conn := SqlDbGetConnection(handle)
    DumpConnection(conn)
    SqlDbCloseConnection(handle)

function TestODBC as void
    SqlDbSetProvider("ODBC")
    var handle := SqlDbOpenConnection("Driver={SQL Server};Server=(local);Database=NorthWind;Trusted_Connection=Yes;")
    var conn := SqlDbGetConnection(handle)
    DumpConnection(conn)
    SqlDbCloseConnection(handle)
function TestOLEDB as void
    SqlDbSetProvider("OLEDB")
    var handle := SqlDbOpenConnection("Provider=sqloledb;Data Source=(local);Initial Catalog=NorthWind;Integrated Security=SSPI;")
    var conn := SqlDbGetConnection(handle)
    DumpConnection(conn)
    SqlDbCloseConnection(handle)

Function DumpConnection(conn as SqlDbConnection) as void
    var secs := Seconds()
    if conn != NULL
        Console.Clear()
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

