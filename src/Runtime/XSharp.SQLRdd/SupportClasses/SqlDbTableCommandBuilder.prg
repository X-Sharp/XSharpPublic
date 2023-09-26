// SqlDbTableCommandBuilder.prg
// Created by    : robert
// Creation Date : 9/25/2023 2:54:39 PM
// Created for   :
// WorkStation   : NYX


using System
using System.Collections.Generic
using System.Text

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The SqlDbTableCommandBuilder class.
/// </summary>
class SqlDbTableCommandBuilder
    protected _cTable as string
    protected _oInfo as SqlTableInfo
    protected _connection as SqlDbConnection
    protected _provider as SqlDbProvider

    constructor(cTable as string, oconn as SqlDbConnection)
        self:_cTable     := cTable
        self:_connection := oconn
        self:_provider   := oconn:Provider
        return
    method FetchInfo() as SqlDbTableDef
        // build initial information needed for SQL Query for a table
        local oInfo as SqlTableInfo
        local cTable as string
        cTable := self:_cTable
        oInfo := SqlTableInfo{_cTable}
        // Ask the client for MaxRecords
        oInfo:MaxRecords        := _connection:RaiseIntEvent(_connection, SqlRDDEventReason.MaxRecords,cTable, oInfo:MaxRecords)
        oInfo:RecnoColumn       := _connection:RaiseStringEvent(_connection, SqlRDDEventReason.RecnoColumn,cTable, oInfo:RecnoColumn)
        oInfo:DeletedColumn     := _connection:RaiseStringEvent(_connection, SqlRDDEventReason.DeletedColumn,cTable, oInfo:DeletedColumn)
        oInfo:LongFieldNames    := _connection:RaiseLogicEvent(_connection, SqlRDDEventReason.LongFieldNames,cTable, oInfo:LongFieldNames)
        var oTd := _connection:GetStructureForTable(cTable, oInfo:LongFieldNames)
        oInfo:CopyFromTd(oTd)
        self:AdjustSelects(oInfo)
        self:_oInfo := oInfo
        return oInfo

    method AdjustSelects(oInfo as SqlTableInfo) as void
        var sb := StringBuilder{}
        sb:Append(SqlDbProvider.SelectClause)
        var first := true
        var list  := Dictionary<string, SqlDbColumnDef>{StringComparer.OrdinalIgnoreCase}
        foreach var c in oInfo:Columns
            if first
                first := false
            else
                sb:Append(", ")
            endif
            sb:Append(_provider.QuotePrefix)
            sb:Append(c:ColumnInfo:ColumnName)
            sb:Append(_provider.QuoteSuffix)

            list:Add(c:ColumnInfo:ColumnName, c)
        next
        if !String.IsNullOrEmpty(oInfo:RecnoColumn)
            if !list:ContainsKey(oInfo:RecnoColumn)
                sb:Append(", ")
                sb:Append(_provider.QuotePrefix)
                sb:Append(oInfo:RecnoColumn)
                sb:Append(_provider.QuoteSuffix)
                list:Add(oInfo:RecnoColumn, null)
            else
                var col := list[oInfo:RecnoColumn]
                col:ColumnFlags |= SqlDbColumnFlags.Recno
            endif

        endif
        if !String.IsNullOrEmpty(oInfo:DeletedColumn)
            if !list:ContainsKey(oInfo:RecnoColumn)
                sb:Append(", ")
                sb:Append(_provider.QuotePrefix)
                sb:Append(oInfo:DeletedColumn)
                sb:Append(_provider.QuoteSuffix)
                list:Add(oInfo:DeletedColumn,null)
            else
                var col := list[oInfo:DeletedColumn]
                col:ColumnFlags |= SqlDbColumnFlags.Deleted
            endif
        endif
        sb:Append(SqlDbProvider.FromClause)
        sb:Append(_provider.QuotePrefix)
        sb:Append(oInfo:Name)
        sb:Append(_provider.QuoteSuffix)
        oInfo:SelectStatement := sb:ToString()
        sb:Append(SqlDbProvider.WhereClause)
        sb:Append("1=0")
        oInfo:EmptySelectStatement :=sb:ToString()
        return



end class
end namespace // XSharp.SQLRdd.SupportClasses
