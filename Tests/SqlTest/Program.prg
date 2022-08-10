using System
using System.Collections.Generic
using System.Linq
using System.Text
using XSharp.Data
using System.Data
using XSharp.VO

FUNCTION Start() AS VOID STRICT
    LOCAL oConn AS SqlConnection
    local oSel as MySqlSelect
    try
    //oConn := SqlOpenConnection()
    SetSqlFactory(SqlServerFactory{})
    //oConn := SqlConnection{"Driver=SQL Server;Trusted_Connection=yes;Server=(local);Database=CursAdm"}
    oConn := SqlConnection{"Server=(local);Database=Cursadm;Trusted_Connection=True;"}
    //Conn:DriverConnect()
    ? oConn:ConnectString
    if ! oConn:Connected
        ? oConn:Connect()
    ENDIF
    ? oConn:Connected
    IF oConn:COnnected
        oSel := MySqlSelect{"Select top 1 * from Fakturen",oConn}
        //oSel:ReadOnly := TRUE
        //oSel:BatchUpdates := true
        oSel:Execute()

        do while ! oSel:EOF
            ? oSel:RecNo, oSel:FieldGet(1)
            oSel:Skip()
        enddo
        oSel:Skip(-1)

        oSel:SetPrimaryKey(1)
        ? "Count", oSel:NumSuccessfulRows

        //TestFieldPut(oSel)
        TestAppend(oSel)
        //TestDelete(oSel)
        oSel:Update(true)
        oSel:Close()
        ? oConn:Disconnect()
    endif
    catch e as Exception
        ? e:ToString()
    end try
    WAIT
    return

class MySqlSelect inherit SqlSelect
    constructor( cSQLSelect, oSQLConnection )
        super(cSQLSelect, oSQLConnection )
    method PreExecute(cSqlString as string) as string
        ? cSqlString
        return super:PreExecute(cSqlString)
    METHOD __CreateDataAdapter AS VOID
        SUPER:__CreateDataAdapter()

    METHOD __GoCold(lUpdateBatch AS LOGIC) AS LOGIC STRICT
        LOCAL lOk as LOGIC
        lOk := SUPER:__GoCold(lUpdateBatch)
        SELF:nRowCount := SELF:oTable:Rows:Count
        RETURN lOk

METHOD __InitColumnDesc() AS LOGIC STRICT

        local nScaleColumn := -1 as LONG

        foreach oColumn AS DataColumn in oSchema:Columns

            if oColumn:ColumnName == "NumericScale"

                nScaleColumn := oColumn:Ordinal

                oColumn:ReadOnly := FALSE

                EXIT

            endif

        next

        foreach oColumn AS DataColumn in oTable:Columns

            if oColumn:DataType == typeof(Decimal)

                var oRow := oSchema:Rows[oColumn:Ordinal]

                oRow[nScaleColumn] := 4

            endif

        next

        RETURN SUPER:__InitColumnDesc()

end class


function TestFieldPut(oSel as SQLSelect) as void
        var cName := oSel:FieldGet(2 )
        oSel:FIELDPUT(2, "ABC")
        oSel:Skip(0)
        oSel:FIELDPUT(2, cName)

function TestAppend(oSel as SQLSelect) as void
        ? oSel:Append(true)
        ? oSel:FieldGet(1)
        ? oSel:FIELDPUT(2, "ZZZ")
        ? oSel:AppendRow()
        return
function TestDelete(oSel as SQLSelect) as void
        ? oSel:Delete()
        return




function DumpTable(oSel as SqlSelect) as void
    ?
    FOR VAR i := 1 TO oSel:FCount
        ?? oSel:FieldName(i),""
    NEXT
    ?
    DO WHILE ! oSel:EOF
        FOR VAR i := 1 TO oSel:FCount
            ?? Trim(AsString(oSel:FIELDGET(i))),""
        NEXT
        ?
        oSel:Skip(1)
    ENDDO
    ?
