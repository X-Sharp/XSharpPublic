﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



using System
using System.Linq
using System.Diagnostics
using System.Collections.Generic
using System.Text
using System.Data
using System.Data.Common
using XSharp.RDD.Enums
using XSharp.RDD.SqlRDD.Providers

begin namespace XSharp.RDD.SqlRDD
    /// <summary>
    /// Delegate that describes the event handler for the RDD events
    /// </summary>
    /// <param name="oSender">Object that raised the event</param>
    /// <param name="e">Event arguments</param>
public delegate SqlRDDEventHandler(oSender as object, e as SqlRddEventArgs) as object


/// <summary>
/// Connection class.
/// </summary>
[DebuggerDisplay("{Name,nq} ({ProductName,nq})")];
class SqlDbConnection inherit SqlDbHandleObject implements IDisposable
    // Constants for the Metadata collections
    internal const COLLECTIONNAME := "CollectionName" as string
    internal const TABLECOLLECTION := "Tables" as string
    internal const COLUMNCOLLECTION := "Columns" as string
    internal const DATABASECOLLECTION := "Databases" as string
    internal const DATASOURCECOLLECTION := "DataSourceInformation" as string
    internal const TABLENAME := "TABLE_NAME" as string
    internal const TABLETYPE := "TABLE_TYPE" as string
    // Constants for the columns in a schema
    private _lastException as Exception
    private _command       as SqlDbCommand
    private _commands      as List<SqlDbCommand>
    private _datasourceProperties as Dictionary<string, string>
    private _metadataCollections as List<string>
    private _databaseRestrictions as int
    private _tableRestrictions as int

    #region Properties
    /// <summary>Dictionary with properties defined by the Ado.Net provider</summary>
    property DataSourceProperties as Dictionary<string, string> get _datasourceProperties

    /// <summary>Is the connection open</summary>
    property IsOpen             as logic get DbConnection != null .and. DbConnection:State == ConnectionState.Open
    /// <summary>ISqlDbProvider object used by the connection</summary>
    property Provider           as ISqlDbProvider auto
    /// <summary>Ado.Net DbConnection object used by the connection</summary>
    property DbConnection       as DbConnection auto
    /// <summary>Connection String</summary>
    property ConnectionString   as string auto
    /// <summary>Cache for the Table Schemas</summary>
    property Schema             as Dictionary<string, SqlDbTableInfo> auto
    /// <summary>Open RDDs for the connection</summary>
    property RDDs               as IList<SQLRDD> auto
    /// <summary>Should the connection stay open</summary>
    property KeepOpen           as logic auto
    /// <summary>Timeout when Opening</summary>
    property TimeOut            as long auto
    /// <summary>Ado.Net DbTransaction object when a transaction is running</summary>
    property DbTransaction      as DbTransaction auto

    /// <summary>Provider for the Metadata, such as columnlist, maxrecords etc.</summary>
    property MetadataProvider   as ISqlMetadataProvider auto
    /// <summary>Last exception that occurred in the RDD</summary>
    property LastException      as Exception auto get internal set
    /// <summary>Connection State</summary>
    PROPERTY State              as ConnectionState get iif(self:DbConnection == null, ConnectionState.Closed, self:DbConnection:State)

    /// <summary>Should field types from SQL be translated to the 'old' field types (CDLMN) or should also FoxPro types (BCDFGILMNPQTVWY0) be allowed?</summary>
    property LegacyFieldTypes   as logic auto

    /// <summary>Should the phantom record have Null values or empty values?</summary>
    property UseNulls           as logic auto

    /// <summary>Identifier Case as returned by the Ado.Net provider in the DataSourceInformation metadata collection.</summary>
    property IdentifierCase     as System.Data.Common.IdentifierCase auto get private set
    /// <summary>Quoted Identifier Case as returned by the Ado.Net provider in the DataSourceInformation metadata collection.</summary>
    property QuotedIdentifierCase     as System.Data.Common.IdentifierCase auto get private set
    /// <summary>ProductName as returned by the Ado.Net provider in the DataSourceInformation metadata collection.</summary>
    property ProductName        as string auto get private set


#endregion

#region Metadata Defaults


    /// <summary>
    /// Can the table be updated ?
    /// </summary>
    /// <remarks>This is the default value for the AllowUpdates property for tables opened with the RDD. This can be overridden at the table level</remarks>
    PROPERTY AllowUpdates      as LOGIC auto
    /// <summary>
    /// Specifies whether memo fields of type Long text or Long binary are included in the WHERE clause when using automatic updating. This defaults to TRUE
    /// </summary>
    /// <remarks>This is the default value for the CompareMemo property for tables opened with the RDD. This can be overridden at the table level</remarks>
    PROPERTY CompareMemo       as LOGIC auto
    /// <summary>
    /// Name of the Deleted column. When empty then rows will be physically deleted from the server
    /// </summary>
    /// <remarks>This is the default value for the DeletedColumn property for tables opened with the RDD. This can be overridden at the table level</remarks>
    PROPERTY DeletedColumn      as STRING auto
    /// <summary>
    /// Can field names longer than 10 characters be used (true) or should they be truncated (false)
    /// </summary>
    /// <remarks>This is the default value for the LongFieldNames property for tables opened with the RDD. This can be overridden at the table level</remarks>
    PROPERTY LongFieldNames     as LOGIC auto
    /// <summary>
    /// What is the maximum number of records that the RDD should fetch when unfiltered.
    /// </summary>
    /// <remarks>This is the default value for the MaxRecords property for tables opened with the RDD. This can be overridden at the table level</remarks>
    PROPERTY MaxRecords         as INT auto
    /// <summary>
    /// Name of the Recno column. When empty then the relative row number is the record number
    /// </summary>
    /// <remarks>This is the default value for the RecnoColumn property for tables opened with the RDD. This can be overridden at the table level</remarks>
    PROPERTY RecnoColumn        as STRING auto
    /// <summary>
    /// Should trailing spaces for string columns be trimmed?
    /// </summary>
    /// <remarks>This is the default value for the CompareMemo property for tables opened with the RDD. This can be overridden at the table level</remarks>
    PROPERTY TrimTrailingSpaces as LOGIC auto
    /// <summary>
    /// Should all columns be updated when a record is updated?
    /// </summary>
    /// <remarks>This is the default value for the CompareMemo property for tables opened with the RDD. This can be overridden at the table level</remarks>
    PROPERTY UpdateAllColumns as LOGIC auto
    /// <summary>
    /// Specifies whether the maximum value in the Recno column should be used as the RecCount property.
    /// This defaults to FALSE which means that the RecCount property returns the number of records in the table
    /// </summary>
    property MaxRecnoAsRecCount       as logic auto

#endregion




#region static properties and methods
    internal const DefaultConnection := "DEFAULT" as string
    internal static Connections      as List<SqlDbConnection>
    static internal property DefaultCached  as logic auto
    static constructor()
        Connections     := List<SqlDbConnection>{}
        DefaultCached   := true
        AppDomain.CurrentDomain:ProcessExit += EventHandler{CurrentDomain_ProcessExit}
    end constructor

    internal static method CurrentDomain_ProcessExit(sender as object, e as EventArgs) as void
        foreach var oConn in Connections:ToArray()
            oConn:Close()
        next
    end method

    /// <summary>
    /// Find a connection object by its handle
    /// </summary>
    /// <param name="id">Unique Handle returned when creating the connection</param>
    /// <returns>The matching Connection object, or NULL when the handle is invalid</returns>

    static method FindByHandle(id as IntPtr) as SqlDbConnection
        if SqlDbHandles.FindById(id) is SqlDbConnection var oConn
            return oConn
        endif
        return null
    end method

   /// <summary>
   /// Find a connection object by its name
   /// </summary>
   /// <param name="name">Unique name of the connection</param>
   /// <returns>The matching Connection object, or NULL when the name is not found</returns>
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
            case "allowupdates"
                self:AllowUpdates := (value:ToLower() == "true")
            case "comparememo"
                self:CompareMemo := (value:ToLower() == "true")
            case "deletedcolumn"
                self:DeletedColumn := value
            case "legacyfieldtypes"
                self:LegacyFieldTypes := (value:ToLower() == "true")
            case "longfieldnames"
                self:LongFieldNames := (value:ToLower() == "true")
            case "maxrecords"
                if Int32.TryParse(value, out var max)
                    self:MaxRecords := max
                endif
            case "recnocolumn"
                self:RecnoColumn := value
            case "updateallcolumns"
                self:UpdateAllColumns := (value:ToLower() == "true")
           case "trimtrailingspaces"
                self:TrimTrailingSpaces := (value:ToLower() == "true")
            case "usenulls"
                self:UseNulls := (value:ToLower() == "true")
            case "maxrecnoasreccount"
                self:MaxRecnoAsRecCount := (value:ToLower() == "true")
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

    private method _FillDataSourceProperties() as VOID

        IF ! SELF:HasCollection(DATASOURCECOLLECTION)
            return
        endif
        var tbl := SELF:GetMetaDataCollection(DATASOURCECOLLECTION)
        if tbl:Rows:Count > 0
            var row := tbl:Rows[0]
            foreach col as DataColumn in tbl:Columns
                SELF:_datasourceProperties:Add(col:ColumnName, row[col]:ToString())
            next
        endif

        if _datasourceProperties:TryGetValue("IdentifierCase", out var id)
            if Enum.TryParse<IdentifierCase>(id, true, out var idCase)
                self:IdentifierCase := idCase
            endif
        endif
        if _datasourceProperties:TryGetValue("QuotedIdentifierCase", out id)
            if Enum.TryParse<IdentifierCase>(id, true, out var idCase)
                self:QuotedIdentifierCase := idCase
            endif
        endif
        if _datasourceProperties:TryGetValue("DataSourceProductName", out id)
            self:ProductName := id
        endif

    private method _FillMetadataCollections() as void
        var coll := SELF:GetMetaDataCollections()
        self:_metadataCollections := List<string>{}
        foreach var c in coll
            _metadataCollections:Add(c:ToLower())
        next
        return
    end method

    private method HasCollection(cCollection as string) as logic
        return _metadataCollections != null .and. _metadataCollections:Contains(cCollection:ToLower())
    end method

    /// <summary>
    /// Create a new connection object
    /// </summary>
    /// <param name="cName">Connection Name</param>
    /// <param name="cConnectionString">Connection String</param>
    /// <param name="Callback">(Optional) CallBack</param>
    constructor(cName as string, cConnectionString as string, @@Callback := null as SqlRDDEventHandler)
        super(cName)
        RDDs             := List<SQLRDD>{}
        Schema           := Dictionary<string, SqlDbTableInfo>{StringComparer.OrdinalIgnoreCase}
        Provider         := SqlDbProvider.Current
        LegacyFieldTypes   := DEFAULT_LEGACYFIELDTYPES
        UseNulls           := DEFAULT_USENULLS
        LongFieldNames     := DEFAULT_LONGFIELDNAMES
        TrimTrailingSpaces := DEFAULT_TRIMTRAILINGSPACES
        MaxRecords         := DEFAULT_MAXRECORDS
        DeletedColumn      := DEFAULT_DELETEDCOLUMN
        RecnoColumn        := DEFAULT_RECNOCOLUMN
        cConnectionString  := self:AnalyzeConnectionString(cConnectionString)
        self:ConnectionString := cConnectionString
        DbConnection    := Provider:CreateConnection()
#ifdef DEBUG
        SELF:DbConnection:StateChange += DbConnection_StateChange
#endif
        TimeOut         := 15
        KeepOpen        := DefaultCached
        if @@Callback != null
            self:CallBack += @@Callback
            SELF:MetadataProvider := SqlMetadataProviderCallBack{SELF}
        ELSE
            SELF:MetadataProvider := SqlMetadataProviderIni{SELF}
        endif
        Connections.Add(self)
        _datasourceProperties := Dictionary<string, string>{StringComparer.OrdinalIgnoreCase}
        _commands := List<SqlDbCommand>{}
        _command  := SqlDbCommand{"Worker", self}
        self:ForceOpen()
        SELF:_FillMetadataCollections()
        SELF:_FillDataSourceProperties()
        SELF:_CheckLicenseTables()
        SELF:_Login()
        // Todo: Check for # of open users and close the connection when no users are left and then throw an exception
        return
    end constructor

    /// <summary>
    /// Close the connection.
    /// </summary>
    /// <returns>TRUE when the connection was closed.</returns>
    /// <remarks>
    /// The connection will only be closed when there are no open RDDs for this connection.
    /// </remarks>
    method Close() as logic
        if self:RDDs:Count > 0
            return false
        endif
        self:_Logout()
        // Logout the workstation from the Open Connections table
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

    internal method ForceOpen as void
        if self:DbConnection:State != ConnectionState.Open
            var connStr := SELF:RaiseStringEvent(self, SqlRDDEventReason.ConnectionString, "", self:ConnectionString)
            self:DbConnection:ConnectionString  := connStr
            self:DbConnection:Open()
        endif
    end method
#region RDD registration
    /// <summary>
    /// Register a RDD with its connection object.
    /// </summary>
    /// <param name="oRDD">Object to register</param>
    method RegisterRdd(oRDD as SQLRDD) as void
        RDDs:Add(oRDD)
        return

    /// <summary>
    /// Unregister a RDD from its connection object.
    /// </summary>
    /// <param name="oRDD">Object to unregister</param>
    /// <returns></returns>
    method UnregisterRdd(oRDD as SQLRDD) as logic
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
    /// <summary>
    /// Begin a transaction
    /// </summary>
    /// <returns>TRUE when transaction was succesfully started</returns>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="LastException"/>
    method BeginTrans as logic
        try
            if SELF:DbTransaction == NULL
                self:DbTransaction := self:DbConnection:BeginTransaction()
            else
                return false
            endif
        catch e as Exception
            _lastException := e
            self:DbTransaction := null
        end try
        return self:DbTransaction != null
    end method

    /// <summary>
    /// Begin a transaction
    /// </summary>
    /// <returns>TRUE when transaction was succesfully started</returns>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="LastException"/>
    method BeginTrans(isolationLevel as System.Data.IsolationLevel)  as logic
        try
            if SELF:DbTransaction == NULL
                self:DbTransaction := self:DbConnection:BeginTransaction(isolationLevel)
            else
                return false
            endif
        catch e as Exception
            _lastException := e
            self:DbTransaction := null
        end try
        return self:DbTransaction != null
    end method

    /// <summary>
    /// Commit the current transaction
    /// </summary>
    /// <returns>TRUE when transaction was succesfully committed</returns>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="LastException"/>
    method CommitTrans() as logic
        if self:DbTransaction != null
            try
                self:DbTransaction:Commit()
                self:DbTransaction := null
                return true
            catch e as Exception
                SELF:LastException := e
                return false
            end try
        endif
        return false
    end method
   /// <summary>
    /// Roll back the current transaction
    /// </summary>
    /// <returns>TRUE when transaction was succesfully rolled back</returns>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="LastException"/>
    method RollBackTrans as logic
        if self:DbTransaction != null
            try
                self:DbTransaction:Rollback()
                self:DbTransaction := null
                return true
            catch e as Exception
                SELF:LastException := e
                return false
            end try
        endif
        return false
    end method
#endregion

    /// <summary>
    /// Execute a SQL command and return the value returned by the command
    /// </summary>
    /// <param name="cCommand">SQL Statement</param>
    /// <param name="cTable">Table name to display for Event Handler</param>
    /// <returns>Result of the command or NULL when an exception occurred</returns>

    method ExecuteScalar(cCommand as string, cTable := __FUNCTION__ as STRING) as OBJECT
        local result := null as object
        try
            _command:CommandText := cCommand
            result := _command:ExecuteScalar(cTable)
        catch e as Exception
            _lastException := e
            result := null
        end try
        return result
    end method

   /// <summary>
   /// Execute a SQL command
   /// </summary>
   /// <param name="cCommand">SQL Statement</param>
    /// <param name="cTable">Table name to display for Event Handler</param>
   /// <returns>TRUE when succesfull or FALSE when an exception occurred</returns>
    method ExecuteNonQuery(cCommand as string, cTable := __FUNCTION__ as STRING) as LOGIC
        local result := true as logic
        try
             _command:CommandText := cCommand
            result := _command:ExecuteNonQuery(cTable)
        catch e as Exception
            _lastException := e
            result := false
        end try
        return result
    end method

    /// <summary>
    /// Execute a SQL command and return the DbDataReader returned by the command
    /// </summary>
    /// <param name="cCommand">SQL Statement</param>
    /// <param name="cTable">Table name to display for Event Handler</param>
    /// <returns>DbDataReader or NULL when an exception occurred</returns>
    method ExecuteReader(cCommand as string, cTable := __FUNCTION__ as STRING) as DbDataReader
       local result := null as DbDataReader
        try
             _command:CommandText := cCommand
            result := _command:ExecuteReader(cTable)
        catch e as Exception
            _lastException := e
            result := null
        end try
        return result
    end method

    /// <summary>
    /// Execute a SQL command and read the DataTable returned by the command
    /// </summary>
    /// <param name="cCommand">SQL Statement</param>
    /// <param name="cTable">Table name for the Table</param>
    /// <returns>DataTable or NULL when an exception occurred</returns>
    method GetDataTable(cCommand as string, cTable as STRING) as DataTable
       local result := null as DataTable
        try
            _command:CommandText := cCommand
            result := _command:GetDataTable(cTable)
        catch e as Exception
            _lastException := e
            result := null
        end try
        return result
    end method



    #region Schema Info
    /// <summary>
    /// Delete a Table from the schema information
    /// </summary>
    /// <param name="sTableName">Table to delete</param>
    /// <returns></returns>
    method DeleteTableDef(sTableName as string) as logic
        if self:Schema:ContainsKey(sTableName)
            self:Schema:Remove(sTableName)
            return true
        endif
        return false
    end method
#endregion
#region MetaData
    /// <summary>
    /// Read the table structure for a query
    /// </summary>
    /// <param name="cQuery">Select statement </param>
    /// <param name="TableName">Name of the table that will be used</param>
    /// <param name="longFieldNames">Should long field names be returned</param>
    /// <returns>Object with Table Information or NULL when an error occurs</returns>
    method GetStructureForQuery(cQuery as string, TableName as string, longFieldNames as LOGIC) as SqlDbTableInfo
         try
            cQuery := SELF:RaiseStringEvent(self, SqlRDDEventReason.CommandText, TableName, cQuery)
            longFieldNames := SELF:LongFieldNames
            _command:CommandText := cQuery
            var schema := _command:GetSchemaTable()
            var oCols := List<SqlDbColumnDef>{}
            var fieldNames := List<string>{}
            foreach row as DataRow in schema:Rows
                local colInfo  := SQLHelpers.GetColumnInfoFromSchemaRow(row, fieldNames, longFieldNames) as DbColumnInfo
                if SELF:LegacyFieldTypes
                    // Map back to the old field types
                    switch colInfo:FieldType
                    case DbFieldType.Integer
                    case DbFieldType.Currency
                        if colInfo:FieldType == DbFieldType.Currency
                            colInfo:Decimals := Math.Min(4, colInfo:Decimals)
                        endif
                        colInfo:FieldType := DbFieldType.Number
                        colInfo:Length := 10
                    case DbFieldType.Double
                    case DbFieldType.Float
                        colInfo:FieldType := DbFieldType.Number
                        colInfo:Length := 20
                    case DbFieldType.DateTime
                        colInfo:FieldType := DbFieldType.Date
                        colInfo:Length := 8
                    case DbFieldType.Blob
                    case DbFieldType.General
                    case DbFieldType.VarBinary
                    case DbFieldType.VarChar
                    case DbFieldType.Picture
                        colInfo:FieldType := DbFieldType.Memo
                        colInfo:Length := 10
                    CASE DbFieldType.Character
                    CASE DbFieldType.Date
                    CASE DbFieldType.Number
                    CASE DbFieldType.Logic
                    CASE DbFieldType.Memo
                        nop
                    otherwise
                        nop
                    end switch
                endif
                oCols:Add(SqlDbColumnDef{ colInfo })
            next
            var oTd   := SqlDbTableInfo{TableName, SELF}
            foreach var oCol in oCols
                oTd:Columns:Add(oCol)
            next
            return oTd
        catch e as Exception
            _lastException := e
        end try
        return null
    end method

    /// <summary>
    /// Read the table structure for a table
    /// </summary>
    /// <param name="TableName">Name of the table</param>
    /// <param name="oTable">Table Info (from the metadata provider)</param>
    /// <param name="cColumnNames">List of column names that we're interested in.</param>
    /// <returns>Object with Table Information or NULL when an error occurs</returns>
    method GetStructureForTable(TableName as string, oTable as SqlDbTableInfo, cColumnNames as string) as SqlDbTableInfo
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
            var oTd := SELF:GetStructureForQuery(query,TableName, longFieldNames)
            oTd:SelectStatement := selectStmt
            self:Schema:Add(TableName, oTd)
            return oTd
        catch e as Exception
            _lastException := e
        end try
        return null
    end method

    /// <summary>
    /// Check to see if a table exists
    /// </summary>
    /// <param name="cTableName">Name of the table to check</param>
    /// <returns>TRUE when the table exists. </returns>
    method DoesTableExist(cTableName as string) as logic
        try
            if !SELF:HasCollection(TABLECOLLECTION)
                return false
            endif
            if self:_tableRestrictions != 0
                local aTableRestrictions := string[]{4} as string[]
                aTableRestrictions[2] := cTableName
                var dt := self:DbConnection:GetSchema(TABLECOLLECTION, aTableRestrictions)
                return dt:Rows:Count > 0
            else
                var dt := self:DbConnection:GetSchema(TABLECOLLECTION)
                foreach row as DataRow in dt:Rows
                    var tbl := row["TABLE_NAME"]:ToString()
                    if String.Compare(tbl, cTableName, true) == 0
                        return true
                    endif
                next
            endif
        catch e as Exception
            _lastException := e
        end try
        return false
    end method


    /// <summary>
    /// Check to see if a database exists
    /// </summary>
    /// <param name="cDatabase">Name of the database to check</param>
    method DoesDatabaseExist(cDatabase as string) as logic
        try
            if !SELF:HasCollection(DATABASECOLLECTION)
                    return false
            endif
            if self:_databaseRestrictions != 0
                local aRestrictions := string[]{1} as string[]
                aRestrictions[0] := cDatabase
                var dt := self:DbConnection:GetSchema(DATABASECOLLECTION, aRestrictions)
                if dt:Rows:Count > 0
                    return true
                endif
            else
                var dt := self:DbConnection:GetSchema(DATABASECOLLECTION)
                foreach row as DataRow in dt:Rows
                    var db := row[0]:ToString()
                    if String.Compare(db, cDatabase, true) == 0
                        return true
                    endif
                next
            endif
        catch e as Exception
            _lastException := e
        end try
        return false
    end method
    /// <summary>
    /// Return the table names from the database
    /// </summary>
    /// <param name="filter">filter to apply</param>
    /// <returns>List of table names that match the filter</returns>
    method GetTables(filter := "" as string) as List<string>
        try
            var result := List<string>{}
            if self:HasCollection(TABLECOLLECTION)
                var dt := self:DbConnection:GetSchema(TABLECOLLECTION)
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
            endif
            return result
        catch e as Exception
            _lastException := e
        end try
        return null
    end method

 /// <summary>
 /// Return the list of metadata collections supported by the provider
 /// </summary>
 /// <returns>List of metadata collections</returns>
    method GetMetaDataCollections() as List<string>
        var dt := self:DbConnection:GetSchema(DbMetaDataCollectionNames.MetaDataCollections)
        var result := List<string>{}
        foreach row as DataRow in dt:Rows
            var cColl := row[COLLECTIONNAME]:ToString()
            result:Add(cColl)
            if String.Compare(cColl, TABLECOLLECTION, true) == 0
                _tableRestrictions := Convert.ToInt32(row["NumberOfRestrictions"])
            elseif String.Compare(cColl, DATABASECOLLECTION, true) == 0
                _databaseRestrictions := Convert.ToInt32(row["NumberOfRestrictions"])
            endif
        next
        return result
    end method
/// <summary>
/// Return the list of metadata collections supported by the provider
/// </summary>
/// <returns>List of metadata collections</returns>
   method GetMetaDataCollection(cCollection as string) as DataTable
       var dt := self:DbConnection:GetSchema(cCollection)
       return dt
   end method
#endregion



    private method _CheckLicenseTables() as void
        if SELF:DoesTableExist(CONNECTIONSTABLE) .and. self:DoesTableExist(LICENSETABLE)
            return
        endif
        var sb := StringBuilder{}
        sb:Append(SELF:Provider:CreateTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro,LICENSETABLE)
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, "name varchar(50), value varchar(50)")
        using var cmd := SqlDbCommand{"Licenses", self, false}
        cmd:CommandText := sb:ToString()
        cmd:ExecuteNonQuery("License")
        sb:Clear()
        sb:Append(SELF:Provider:CreateTableStatement)
        sb:Replace(SqlDbProvider.TableNameMacro,CONNECTIONSTABLE)
        sb:Replace(SqlDbProvider.FieldDefinitionListMacro, "station varchar(50), username varchar(50), lastlogin varchar(10), refcount int")
        cmd:CommandText := sb:ToString()
        cmd:ExecuteNonQuery("License")

        sb:Clear()
        sb:Append("Insert into "+LICENSETABLE+"( name, value) values(@p1, @p2)")
        cmd:CommandText := sb:ToString()
        cmd:ClearParameters()
        cmd:AddParameter("@p1","version")
        cmd:AddParameter("@p2","SQLRDD Beta 2")
        cmd:ExecuteNonQuery("License")
        cmd:ClearParameters()
        cmd:AddParameter("@p1","serial")
        cmd:AddParameter("@p2","1234567890")
        cmd:ExecuteNonQuery("License")
        cmd:ClearParameters()
        cmd:AddParameter("@p1","users")
        cmd:AddParameter("@p2","999")
        cmd:ExecuteNonQuery("License")
        cmd:ClearParameters()
        cmd:AddParameter("@p1","validationcode")
        cmd:AddParameter("@p2","AAAA-BBBB-CCCC-DDDD")
        cmd:ExecuteNonQuery("License")


    private method _Login() as void
        SELF:_LoginWorker(true)
    private method _Logout() as void
        SELF:_LoginWorker(false)
    private method _LoginWorker(lIn as LOGIC) as void
        var user    := Environment.UserName
        var station := Environment.MachineName
        var dDate   := DateTime.Now
        var today   := dDate:Year:ToString()+"-"+dDate:Month:ToString("0#")+"-"+dDate:Day:ToString("0#")
        SELF:BeginTrans()
        using var cmd := SqlDbCommand{"Licenses", self, false}
        cmd:CommandText := "Delete from "+CONNECTIONSTABLE+" where lastlogin < @p3"
        cmd:ClearParameters()
        cmd:AddParameter("@p1",user)
        cmd:AddParameter("@p2",station)
        cmd:AddParameter("@p3",today:ToString())
        cmd:ExecuteNonQuery("License")
        var sWhere := "where username = @p1 and station = @p2 and lastlogin = @p3"
        if lIn
            cmd:CommandText := "select count(*) from "+CONNECTIONSTABLE+" "+sWhere
            var result := Convert.ToInt64(cmd:ExecuteScalar())
            if result == 0
                cmd:CommandText := "Insert into "+CONNECTIONSTABLE+"(username, station, lastlogin, refcount) values(@p1, @p2, @p3, 0)"
                cmd:ExecuteNonQuery("License")
            endif
            cmd:CommandText := "Update "+CONNECTIONSTABLE+" set refcount = refcount + 1 "+sWhere
            cmd:ExecuteNonQuery("License")
        else
            cmd:CommandText := "Update "+CONNECTIONSTABLE+" set refcount = refcount - 1 "+sWhere
            cmd:ExecuteNonQuery("License")
            cmd:CommandText := "delete from "+CONNECTIONSTABLE+" "+sWhere+" and refcount <= 0"
            cmd:ExecuteNonQuery("License")

        endif
        SELF:CommitTrans()
        #region Implement IDisposable
    /// <inheritdoc/>
    public override method Dispose() as void
        self:Close()
        Super:Dispose()
    end method

    #endregion

    INTERNAL CONST LICENSETABLE := "xs_license" as string
    INTERNAL CONST CONNECTIONSTABLE := "xs_connections" as string
    INTERNAL CONST DEFAULT_ALLOWUPDATES := TRUE AS LOGIC
    INTERNAL CONST DEFAULT_COMPAREMEMO := TRUE AS LOGIC
    INTERNAL CONST DEFAULT_DELETEDCOLUMN := "xs_deleted" AS STRING
    INTERNAL CONST DEFAULT_LEGACYFIELDTYPES := TRUE AS LOGIC
    INTERNAL CONST DEFAULT_LONGFIELDNAMES := FALSE AS LOGIC
    INTERNAL CONST DEFAULT_MAXRECORDS := 1000 AS INT
    INTERNAL CONST DEFAULT_RECNOCOLUMN := "xs_recno" AS STRING
    INTERNAL CONST DEFAULT_TRIMTRAILINGSPACES := TRUE AS LOGIC
    INTERNAL CONST DEFAULT_UPDATEALLCOLUMNS := FALSE AS LOGIC
    INTERNAL CONST DEFAULT_USENULLS := TRUE AS LOGIC
    #region Events
    /// <summary>
    /// Event handler for the SqlRDD events
    /// </summary>
    public event CallBack as SqlRDDEventHandler
    internal method RaiseEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as object) as object
        if String.IsNullOrEmpty(cTable)
            cTable := oObject:Name
        endif
        var oArgs := SqlRddEventArgs{ nEvent, cTable, oValue}
        if @@CallBack != null
            var result := SELF:CallBack ( oObject, oArgs )
            return result
        endif
        return oArgs:Value
    internal method RaiseStringEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as string) as string
        var result := SELF:RaiseEvent(oObject, nEvent, cTable, oValue)
        if result is string var strValue
            return strValue
        endif
        return oValue
    internal method RaiseIntEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as int) as int
        var result := SELF:RaiseEvent(oObject, nEvent, cTable, oValue)
        if result is int var intValue
            return intValue
        endif
        return oValue
    internal method RaiseListEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as IList<string>) as IList<string>
        var result := SELF:RaiseEvent(oObject, nEvent, cTable, oValue)
        if result is IList<string> var listValue
            return listValue
        endif
        return oValue
    internal method RaiseLogicEvent(oObject as SqlDbObject, nEvent as SqlRDDEventReason, cTable as string, oValue as logic) as logic
        var result := SELF:RaiseEvent(oObject, nEvent, cTable, oValue)
        if result is logic var logValue
            return logValue
        endif
        return oValue
    #endregion
end class
end namespace
