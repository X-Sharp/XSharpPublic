//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



using System
using System.Collections.Generic
using System.Text
using System.Data
using System.Data.Common

public delegate XSharp.SqlRDDEventHandler(oSender as object, e as XSharp.RDD.SqlRDD.SqlRddEventArgs) as object

begin namespace XSharp.RDD.SqlRDD


/// <summary>
/// Connection class.
/// </summary>
class SqlDbConnection inherit SqlDbEventObject implements IDisposable
    // Constants for the Metadata collections
    internal const COLLECTIONNAME := "CollectionName" as string
    internal const TABLECOLLECTION := "Tables" as string
    internal const TABLENAME := "TABLE_NAME" as string
    internal const TABLETYPE := "TABLE_TYPE" as string
    // Constants for the columns in a schema

#region Properties
    property IsOpen             as logic get DbConnection != null .and. DbConnection:State == ConnectionState.Open
    property Provider           as SqlDbProvider auto
    property DbConnection       as DbConnection auto
    property ConnectionString   as string auto
    property Schema             as Dictionary<string, SqlDbTableDef> auto
    property RDDs               as IList<SQLRDD> auto
    property KeepOpen           as logic auto
    property TimeOut            as long auto
    property DbTransaction      as DbTransaction auto
    property UseNulls           as logic auto
    property UseLongNames       as logic auto
    property TrimTrailingSpaces as logic auto
    private _lastException as Exception

#endregion

#region static properties and methods
    const DefaultConnection := "DEFAULT" as string
    static Connections      as List<SqlDbConnection>
    static internal property DefaultCached  as logic auto
    static constructor()
        Connections     := List<SqlDbConnection>{}
        DefaultCached   := true

    static method FindByHandle(id as IntPtr) as SqlDbConnection
        if SqlDbHandles.FindById(id) is SqlDbConnection var oConn
            return oConn
        endif
        return null

    static method FindByName(name as string) as SqlDbConnection
        foreach var oConn in Connections
            if String.Compare(oConn:Name, name, true) == 0
                return oConn
            endif
        next
        return null

#endregion

    private method AnalyzeConnectionString(cConnectionString as string) as string
        var options := DbConnectionStringBuilder{self:Provider:Name:ToLower() == "odbc"}
        options:ConnectionString := cConnectionString
        var builder := Provider:CreateConnectionStringBuilder()
        foreach key as string in options:Keys
            var value := options[key]:ToString()
            switch key:ToLower()
            case "trimtrailingspaces"
                self:TrimTrailingSpaces := (value:ToLower() == "true")
            case "usenulls"
                self:UseNulls := (value:ToLower() == "true")
            case "uselongnames"
                self:UseLongNames := (value:ToLower() == "true")
            otherwise
                builder:Add(key, value)
            end switch
        next
        return builder:ConnectionString
    constructor(cName as string, cConnectionString as string, @@Callback := null as SqlRDDEventHandler)
        super(cName)
        RDDs            := List<SQLRDD>{}
        Schema          := Dictionary<string, SqlDbTableDef>{StringComparer.OrdinalIgnoreCase}
        Provider        := SqlDbProvider.Current
        cConnectionString := self:AnalyzeConnectionString(cConnectionString)
        self:ConnectionString := cConnectionString
        DbConnection    := Provider:CreateConnection()
        TimeOut         := 15
        KeepOpen        := DefaultCached
        if @@Callback != null
            self:CallBack += @@Callback
        endif
        Connections.Add(self)
        self:ForceOpen()
        return
    method Close() as logic
        if self:RDDs:Count > 0
            return false
        endif
        self:CloseConnection()
        Connections.Remove(self)
        return true
    private method CloseConnection as void
        if self:DbConnection:State == ConnectionState.Open
            self:DbConnection:Close()
        endif
        return
    internal method ForceOpen as void
        if self:DbConnection:State != ConnectionState.Open
            var connStr := RaiseStringEvent(self, SqlRDDEventReason.ConnectionString, "", self:ConnectionString)
            self:DbConnection:ConnectionString  := connStr
            self:DbConnection:Open()
        endif
#region RDD registration
    method AddRdd(oRDD as SQLRDD) as logic
        RDDs:Add(oRDD)
        return true
    method RemoveRdd(oRDD as SQLRDD) as logic
        if RDDs:Contains(oRDD)
            RDDs:Remove(oRDD)
        endif
        if RDDs:Count == 0 .and. ! self:KeepOpen
            self:CloseConnection()
        endif
        return true
#endregion
#region Transactions
    method BeginTrans as logic
        try
            self:DbTransaction := self:DbConnection:BeginTransaction()
        catch e as Exception
            _lastException := e
            self:DbTransaction := null
        end try
        return self:DbTransaction != null
    method BeginTrans(isolationLevel as System.Data.IsolationLevel)  as logic
        try
            self:DbTransaction := self:DbConnection:BeginTransaction(isolationLevel)
        catch e as Exception
            _lastException := e
            self:DbTransaction := null
        end try
        return self:DbTransaction != null
    method CommitTrans() as logic
        if self:DbTransaction != null
            self:DbTransaction:Commit()
            self:DbTransaction := null
            return true
        endif
        return false
#endregion
    method RollBackTrans as logic
        if self:DbTransaction != null
            self:DbTransaction:Rollback()
            self:DbTransaction := null
            return true
        endif
        return false


#region Schema Info
    method DeleteTableDef(sTableName as string) as logic
        if self:Schema:ContainsKey(sTableName)
            self:Schema:Remove(sTableName)
            return true
        endif
        return false
#endregion
#region MetaData
    method GetStructureForQuery(cQuery as string, TableName as string, longFieldNames as logic) as SqlDbTableDef
        cQuery := RaiseStringEvent(self, SqlRDDEventReason.CommandText, TableName, cQuery)
        longFieldNames := RaiseLogicEvent(self,SqlRDDEventReason.LongFieldNames, TableName, longFieldNames)
        var cmd   := SqlDbCommand{TableName, self}
        cmd:CommandText := cQuery
        var schema := cmd:GetSchemaTable()
        var oCols := List<SqlDbColumnDef>{}
        var fieldNames := List<string>{}
        foreach row as DataRow in schema:Rows
            local colInfo  := SQLHelpers.GetColumnInfoFromSchemaRow(row, fieldNames, longFieldNames) as DbColumnInfo
            oCols:Add(SqlDbColumnDef{ colInfo })
        next
        var oTd   := SqlDbTableDef{TableName, oCols}
        return oTd

    method GetStructureForTable(TableName as string,longFieldNames as logic, cColumnNames as string) as SqlDbTableDef
        if self:Schema:TryGetValue(TableName, out var result)
            return result
        endif
        try
            var table := self:Provider:QuoteIdentifier(TableName)
            local list as IList<string>
            if ! String.IsNullOrEmpty(cColumnNames)
                list := String2List(cColumnNames)
            else
                list := List<string>{}{"*"}
            endif
            list  := RaiseListEvent(self, SqlRDDEventReason.ColumnList, TableName, list)
            var columnList := List2String(list)
            var selectStmt := SqlDbProvider.SelectClause+columnList+SqlDbProvider.FromClause+table
            var query := selectStmt+SqlDbProvider.WhereClause+"0=1"
            query := RaiseStringEvent(self, SqlRDDEventReason.CommandText, TableName, query)
            var oTd := GetStructureForQuery(query,TableName, longFieldNames)
            oTd:SelectStatement := selectStmt
            oTd:EmptySelectStatement := query
            self:Schema:Add(TableName, oTd)
            return oTd
        catch e as Exception
            ? "Error reading table ",TableName, e:Message
        end try
        return null
    method DoesTableExist(cTableName as string) as logic
        local aTableRestrictions := string[]{4} as string[]
        aTableRestrictions[2] := cTableName
        var dt := self:DbConnection:GetSchema(TABLECOLLECTION, aTableRestrictions)
        return dt:Rows:Count > 0

    method GetTables(filter := "" as string) as List<string>
        var dt := self:DbConnection:GetSchema(TABLECOLLECTION)
        var result := List<string>{}
        foreach row as DataRow in dt:Rows
            if String.IsNullOrEmpty(filter)
                result:Add(row[TABLENAME]:ToString())
            else
                var type := row[TABLETYPE]:ToString()
                if type:IndexOf(filter, StringComparison.OrdinalIgnoreCase) >= 0
                    result:Add(row[TABLENAME]:ToString())
                endif
            endif
        next
        return result
    method GetMetaDataCollections() as List<string>
        var dt := self:DbConnection:GetSchema(DbMetaDataCollectionNames.MetaDataCollections)
        var result := List<string>{}
        foreach row as DataRow in dt:Rows
            result:Add(row[COLLECTIONNAME]:ToString())
        next
        return result
#endregion


#region Implement IDisposable

    public method Dispose() as void
        self:Close()

#endregion
end class
end namespace // XSharp.RDD.SqlRDD
