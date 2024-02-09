//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



using System
using System.Linq
using System.Collections.Generic
using System.Text
using System.Data
using System.Data.Common
using XSharp.RDD.SqlRDD.Providers
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
    private _lastException as Exception
    private _command       as SqlDbCommand
    private _commands      as List<SqlDbCommand>

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
    property TrimTrailingSpaces as logic auto
    property UseLongNames       as logic auto
    property MetadataProvider   as IMetadataProvider auto
    property LastException      as Exception auto get internal set
    PROPERTY State              as ConnectionState get iif(self:DbConnection == null, ConnectionState.Closed, self:DbConnection:State)


#endregion

#region static properties and methods
    const DefaultConnection := "DEFAULT" as string
    static Connections      as List<SqlDbConnection>
    static internal property DefaultCached  as logic auto
    static constructor()
        Connections     := List<SqlDbConnection>{}
        DefaultCached   := true
        AppDomain.CurrentDomain:ProcessExit += EventHandler{CurrentDomain_ProcessExit}
    end constructor

    static method CurrentDomain_ProcessExit(sender as object, e as EventArgs) as void
        foreach var oConn in Connections
            oConn:Close()
        next
    end method

    static method FindByHandle(id as IntPtr) as SqlDbConnection
        if SqlDbHandles.FindById(id) is SqlDbConnection var oConn
            return oConn
        endif
        return null
    end method

    static method FindByName(name as string) as SqlDbConnection
        foreach var oConn in Connections
            if String.Compare(oConn:Name, name, true) == 0
                return oConn
            endif
        next
        return null
    end method

#endregion

    internal method RegisterCommand(oCmd as SqlDbCommand) AS VOID
        if _commands:Contains(oCmd)
            return
        endif
        _commands:Add(oCmd)

    internal method UnRegisterCommand(oCmd as SqlDbCommand) AS VOID
        if _commands:Contains(oCmd)
            _commands:Remove(oCmd)
        endif

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
#ifdef DEBUG
        private method DbConnection_StateChange( sender as object, oStateChange as System.Data.StateChangeEventArgs) as void
            ? "StateChange", oStateChange:OriginalState, oStateChange:CurrentState
        end method
#endif
    constructor(cName as string, cConnectionString as string, @@Callback := null as SqlRDDEventHandler)
        super(cName)
        RDDs            := List<SQLRDD>{}
        Schema          := Dictionary<string, SqlDbTableDef>{StringComparer.OrdinalIgnoreCase}
        Provider        := SqlDbProvider.Current
        cConnectionString := self:AnalyzeConnectionString(cConnectionString)
        self:ConnectionString := cConnectionString
        DbConnection    := Provider:CreateConnection()
#ifdef DEBUG        
        SELF:DbConnection:StateChange += DbConnection_StateChange
#endif        
        TimeOut         := 15
        KeepOpen        := DefaultCached
        if @@Callback != null
            self:CallBack += @@Callback
            SELF:MetadataProvider := CallBackMetaDataProvider{SELF}
        ELSE
            SELF:MetadataProvider := IniMetaDataProvider{SELF}
        endif
        Connections.Add(self)
        _commands := List<SqlDbCommand>{}
        _command  := SqlDbCommand{"Worker", self}
        self:ForceOpen()
        return
    end constructor

    method Close() as logic
        if self:RDDs:Count > 0
            return false
        endif
        _command:Dispose()
        foreach var cmd in SELF:_commands:ToArray()
            if cmd:Connection == self
                cmd:Dispose()
            endif
        next
        self:CloseConnection()
        Connections.Remove(self)
        return true
    end method

    private method CloseConnection as void
        if self:DbConnection:State == ConnectionState.Open
            self:DbConnection:Close()
        endif
        return
    end method

    internal async method ForceOpen as void
        if self:DbConnection:State != ConnectionState.Open
            var connStr := RaiseStringEvent(self, SqlRDDEventReason.ConnectionString, "", self:ConnectionString)
            self:DbConnection:ConnectionString  := connStr
            self:DbConnection:Open()
        endif
    end method
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
    end method

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
    end method

    method BeginTrans(isolationLevel as System.Data.IsolationLevel)  as logic
        try
            self:DbTransaction := self:DbConnection:BeginTransaction(isolationLevel)
        catch e as Exception
            _lastException := e
            self:DbTransaction := null
        end try
        return self:DbTransaction != null
    end method

    method CommitTrans() as logic
        if self:DbTransaction != null
            self:DbTransaction:Commit()
            self:DbTransaction := null
            return true
        endif
        return false
    end method

    method RollBackTrans as logic
        if self:DbTransaction != null
            self:DbTransaction:Rollback()
            self:DbTransaction := null
            return true
        endif
        return false
    end method
#endregion

    method ExecuteScalar(cCommand as string) as OBJECT
        local result := null as object
        try
            _command:CommandText := cCommand
            result := _command:ExecuteScalar()
        catch e as Exception
            _lastException := e
            result := null
        end try
        return result
    end method

    method ExecuteNonQuery(cCommand as string) as LOGIC
        local result := true as logic
        try
            _command:CommandText := cCommand
            result := _command:ExecuteNonQuery()
        catch e as Exception
            _lastException := e
            result := false
        end try
        return result
    end method

    method ExecuteReader(cCommand as string) as DbDataReader
       local result := null as DbDataReader
        try
            _command:CommandText := cCommand
            result := _command:ExecuteReader()
        catch e as Exception
            _lastException := e
            result := null
        end try
        return result
    end method

    method GetDataTable(cCommand as string, cName as STRING) as DataTable
       local result := null as DataTable
        try
            _command:CommandText := cCommand
            result := _command:GetDataTable(cName)
        catch e as Exception
            _lastException := e
            result := null
        end try
        return result
    end method



#region Schema Info
    method DeleteTableDef(sTableName as string) as logic
        if self:Schema:ContainsKey(sTableName)
            self:Schema:Remove(sTableName)
            return true
        endif
        return false
    end method
#endregion
#region MetaData
    method GetStructureForQuery(cQuery as string, TableName as string, longFieldNames as LOGIC) as SqlTableInfo
        cQuery := RaiseStringEvent(self, SqlRDDEventReason.CommandText, TableName, cQuery)
        longFieldNames := SELF:MetadataProvider:LongFieldNames
        _command:CommandText := cQuery
        var schema := _command:GetSchemaTable()
        var oCols := List<SqlDbColumnDef>{}
        var fieldNames := List<string>{}
        foreach row as DataRow in schema:Rows
            local colInfo  := SQLHelpers.GetColumnInfoFromSchemaRow(row, fieldNames, longFieldNames) as DbColumnInfo
            oCols:Add(SqlDbColumnDef{ colInfo })
        next
        var oTd   := SqlTableInfo{TableName, SELF}
        foreach var oCol in oCols
            oTd:Columns:Add(oCol)
        next
        return oTd
    end method

    method GetStructureForTable(TableName as string, oTable as SqlTableInfo, cColumnNames as string) as SqlDbTableDef
        if self:Schema:TryGetValue(TableName, out var result)
            return result
        endif
        try
            var table := self:Provider:QuoteIdentifier(TableName)
            local longFieldNames := TRUE as logic
            if oTable != null
                longFieldNames := oTable:LongFieldNames
            endif
            var columnList := cColumnNames
            var selectStmt := SqlDbProvider.SelectClause+columnList+SqlDbProvider.FromClause+table
            var query := selectStmt+SqlDbProvider.WhereClause+"0=1"
            var oTd := GetStructureForQuery(query,TableName, longFieldNames)
            oTd:SelectStatement := selectStmt
            oTd:EmptySelectStatement := query
            self:Schema:Add(TableName, oTd)
            return oTd
        catch e as Exception
            ? "Error reading table ",TableName, e:Message
        end try
        return null
    end method

    method DoesTableExist(cTableName as string) as logic
        local aTableRestrictions := string[]{4} as string[]
        aTableRestrictions[2] := cTableName
        var dt := self:DbConnection:GetSchema(TABLECOLLECTION, aTableRestrictions)
        return dt:Rows:Count > 0
    end method

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
    end method

    method GetMetaDataCollections() as List<string>
        var dt := self:DbConnection:GetSchema(DbMetaDataCollectionNames.MetaDataCollections)
        var result := List<string>{}
        foreach row as DataRow in dt:Rows
            result:Add(row[COLLECTIONNAME]:ToString())
        next
        return result
    end method
#endregion


#region Implement IDisposable

    public override method Dispose() as void
        self:Close()
        super:Dispose()
    end method

#endregion
end class
end namespace
