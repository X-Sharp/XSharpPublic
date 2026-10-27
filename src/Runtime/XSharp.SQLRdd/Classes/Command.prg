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
using XSharp.RDD.SqlRDD.Providers

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// Connection class.
/// </summary>
class SqlDbCommand inherit SqlDbHandleObject implements IDisposable

#region Properties
    /// <summary>The Connection object for this command.</summary>
    property Connection     as SqlDbConnection auto get private set
    /// <summary>The Ado.Net DbCommand object for this command.</summary>
    property DbCommand      as DbCommand auto
    /// <summary>The Ado.Net DbTransaction object for this command.</summary>
    property DbTransaction  as DbTransaction get DbCommand:Transaction set DbCommand:Transaction := value
    /// <summary>The List of Parameters.</summary>
    property Parameters     as List<SqlDbParameter> auto
    /// <summary>The DBMS Provider class.</summary>
    property Provider       as ISqlDbProvider get iif(Connection == null, null, Connection:Provider)
    /// <summary>The text of the Command object.</summary>
    property CommandText    as string get iif(DbCommand == null, "", DbCommand:CommandText) set DbCommand:CommandText := value
    /// <summary>The table the RDD reads with this command: its read statements may be answered from the
    /// connection's result cache (see SqlDbConnection.ResultCacheMs). NULL = never.</summary>
    internal property CacheTable as string auto

#endregion

    /// <summary>The statement as cache key, or NULL when its result may not be reused: no cache, not an RDD
    /// read (CacheTable), parameters, or not a single SELECT.</summary>
    private method _CacheKey() as string
        if String.IsNullOrEmpty(SELF:CacheTable) .or. SELF:Connection:ResultCache == null .or. SELF:Parameters?:Count > 0
            return null
        endif
        var cText := SELF:CommandText
        if cText == null .or. cText:IndexOf(';') >= 0 .or. !cText:TrimStart():StartsWith("select", StringComparison.OrdinalIgnoreCase)
            return null
        endif
        return cText
    end method

    /// <summary>Before a statement that is not a SELECT runs: drop the cached results it may change, those of
    /// CacheTable for a write of the RDD, all of them for a statement of somebody else.</summary>
    private method _InvalidateCache() as void
        var oCache := SELF:Connection:ResultCache
        if oCache == null
            return
        endif
        var cText := SELF:CommandText
        if cText == null .or. cText:TrimStart():StartsWith("select", StringComparison.OrdinalIgnoreCase) .and. cText:IndexOf(';') < 0
            return
        endif
        if !String.IsNullOrEmpty(SELF:CacheTable)
            oCache:Invalidate(SELF:CacheTable)
        elseif cText:IndexOf(SqlDbConnection.LockTableName, StringComparison.OrdinalIgnoreCase) < 0
            // the lock table refresh runs on a timer and changes no data
            oCache:Clear()
        endif
        return
    end method

    private withEvents  as LOGIC

#region static properties and methods
    private static Commands         as List<SqlDbCommand>
    static constructor
        Commands := List<SqlDbCommand>{}
    end constructor

    /// <summary>
    /// Find a command object by its handle
    /// </summary>
    /// <param name="id">Unique Handle returned when creating the command</param>
    /// <returns>The matching Command object, or NULL when the handle is invalid</returns>
    static method FindByHandle(id as IntPtr) as SqlDbCommand
        if SqlDbHandles.FindById(id) is SqlDbCommand var oCmd
            return oCmd
        endif
        return null
    end method

#endregion

    /// <summary>
    /// Create a SqlDbCommand object
    /// </summary>
    /// <param name="cName">Command name</param>
    /// <param name="oConn">Connection object</param>
    constructor(cName as string, oConn as SqlDbConnection, withEvents := TRUE as LOGIC)
        super(cName)
        self:Connection := oConn
        self:withEvents := withEvents
        oConn:RegisterCommand(SELF)
        self:DbCommand := oConn:Provider:CreateCommand()
        self:DbCommand:Connection := oConn:DbConnection
        Commands:Add(self)
        return
    end constructor

    private method CallBack(cTable as STRING) as VOID
        if withEvents
            self:CommandText := SELF:Connection:RaiseStringEvent(self, SqlRDDEventReason.CommandText, cTable, self:CommandText)
        endif
        RETURN

    /// <summary>
    /// Close a SqlDbCommand object
    /// </summary>
    /// <returns>Nothing</returns>
    method Close() as void
        Commands:Remove(self)
        SELF:Connection:UnRegisterCommand(SELF)
        if SELF:DbCommand != NULL
            SELF:DbCommand:Dispose()
            SELF:DbCommand := NULL
        endif
        return
    end method

    /// <summary>
    /// Get the schematable for the current Command
    /// </summary>
    /// <param name="cTable">Table name to display for Event Handler</param>
    /// <returns>An Ado.Net DataTable</returns>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="SqlDbConnection.LastException"/>
    method GetSchemaTable() as DataTable
        try
            self:Connection:ForceOpen()
            if ! SELF:_TryBindParameters()
                return null
            endif
            using var reader := DbCommand:ExecuteReader(CommandBehavior.SchemaOnly)
            var schema := reader:GetSchemaTable()
            return schema
        catch e as Exception
            Connection:LastException := e
            return null
        end try
    end method

    /// <summary>
    /// Get the datatable for the current Command
    /// </summary>
    /// <returns>An Ado.Net DataTable</returns>
    /// <param name="cTable">The name of the DataTable</param>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="SqlDbConnection.LastException"/>
    method GetDataTable(cTable := "" as string) as DataTable
        try
            local oDataTable as DataTable
            if String.IsNullOrEmpty(cTable)
                cTable := SELF:Name
            endif
            using var reader := SELF:ExecuteReader(cTable)
            if reader == null
                return null
            endif
            oDataTable := DataTable{cTable}
            // Load() takes NOT NULL etc. from the reader's schema and checks the constraints when
            // it finishes, which builds a sort index for every non-nullable column. Those indexes
            // stay attached to the table and are maintained for every row added or removed later
            // (the RDD's buffer is such a table): with 154 NOT NULL columns paging was dominated by
            // index maintenance. A DataSet that does not enforce constraints skips that check.
            var oDataSet := DataSet{}
            oDataSet:EnforceConstraints := false
            oDataSet:Tables:Add(oDataTable)
            oDataTable:Load(reader)
            return oDataTable
        catch e as Exception
            Connection:LastException := e
            return null
        end try
    end method

    /// <summary>
    /// Execute the command and return the rows in a copy of oTemplate's structure.
    /// </summary>
    /// <param name="cTable">The name of the DataTable (when the template cannot be used)</param>
    /// <param name="oTemplate">An empty table with the columns the command returns, or NULL</param>
    /// <param name="oTarget">An empty table with the structure of oTemplate to load the rows into instead
    /// of a copy of oTemplate (copying the structure of a wide table is not free), or NULL.</param>
    /// <remarks>
    /// Building the structure from the reader's schema (DataTable.Load()) is the expensive part of
    /// a query that returns one or a few rows of a wide table: 1.5 ms against 0.5 ms for 178
    /// columns. The RDD runs every query of a work area with the same column list, so it passes
    /// the structure of its buffer. When the reader's columns do not match the template, the
    /// rows are loaded as by GetDataTable(cTable).
    /// </remarks>
    method GetDataTable(cTable as string, oTemplate as DataTable, oTarget := null as DataTable) as DataTable
        if oTemplate == null
            return SELF:GetDataTable(cTable)
        endif
        try
            local oCached as SqlDbResultCacheEntry
            var cKey := SELF:_CacheKey()
            if cKey != null .and. SELF:Connection:ResultCache:TryGet(cKey, out oCached) .and. !oCached:IsScalar ;
                .and. oCached:Columns == oTemplate:Columns:Count
                var oResult := iif(oTarget != null, oTarget, oTemplate:Clone())
                oResult:BeginLoadData()
                foreach var aRow in oCached:Rows
                    oResult:LoadDataRow(aRow, true)
                next
                oResult:EndLoadData()
                return oResult
            endif
            using var reader := SELF:ExecuteReader(cTable)
            if reader == null
                return null
            endif
            var lMatch := reader:FieldCount == oTemplate:Columns:Count
            for var i := 0 to reader:FieldCount-1
                if !lMatch
                    exit
                endif
                lMatch := String.Equals(reader:GetName(i), oTemplate:Columns[i]:ColumnName, StringComparison.OrdinalIgnoreCase)
            next
            local oDataTable as DataTable
            if lMatch
                oDataTable := iif(oTarget != null, oTarget, oTemplate:Clone())
                var aValues := object[]{reader:FieldCount}
                var aRows := iif(cKey != null, List<object[]>{}, null)
                oDataTable:BeginLoadData()
                do while reader:Read()
                    reader:GetValues(aValues)
                    oDataTable:LoadDataRow(aValues, true)
                    if aRows != null
                        if aRows:Count < SqlDbResultCache.MaxRows
                            aRows:Add((object[]) aValues:Clone())
                        else
                            aRows := null
                            cKey  := null
                        endif
                    endif
                enddo
                oDataTable:EndLoadData()
                if aRows != null
                    SELF:Connection:ResultCache:Put(cKey, SqlDbResultCacheEntry{}{Table := SELF:CacheTable, Rows := aRows, Columns := reader:FieldCount})
                endif
            else
                oDataTable := DataTable{iif(String.IsNullOrEmpty(cTable), SELF:Name, cTable)}
                var oDataSet := DataSet{}
                oDataSet:EnforceConstraints := false
                oDataSet:Tables:Add(oDataTable)
                oDataTable:Load(reader)
            endif
            return oDataTable
        catch e as Exception
            Connection:LastException := e
            return null
        end try
    end method

    /// <summary>
    /// Execute a scalar command
    /// </summary>
    /// <param name="cTable">Table name to display for Event Handler</param>
    /// <returns>Return value of the command</returns>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="SqlDbConnection.LastException"/>
    method ExecuteScalar(cTable := "" as string) as object
        try
            self:Connection:ForceOpen()
            local oCached as SqlDbResultCacheEntry
            var cKey := SELF:_CacheKey()
            if cKey != null .and. SELF:Connection:ResultCache:TryGet(cKey, out oCached) .and. oCached:IsScalar
                return oCached:Scalar
            endif
            SELF:_InvalidateCache()
            if String.IsNullOrEmpty(cTable)
                cTable := SELF:Name
            endif
            SELF:CallBack(cTable)
            if self:Connection:DbTransaction != null
                self:DbTransaction := self:Connection:DbTransaction
            endif
            if ! SELF:_TryBindParameters()
                return null
            endif
            var oResult := self:DbCommand:ExecuteScalar()
            if cKey != null
                SELF:Connection:ResultCache:Put(cKey, SqlDbResultCacheEntry{}{Table := SELF:CacheTable, Scalar := oResult, IsScalar := TRUE})
            endif
            return oResult
        catch e as Exception
            Connection:LastException := e
            return null
        end try
    end method

    /// <summary>
    /// Execute a command and return the DataReader
    /// </summary>
    /// <param name="cTable">Table name to display for Event Handler</param>
    /// <returns>An Ado.Net DbDataReader object</returns>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="SqlDbConnection.LastException"/>
    method ExecuteReader(cTable := "" as string) as DbDataReader
        try
            self:Connection:ForceOpen()
            SELF:_InvalidateCache()
            if String.IsNullOrEmpty(cTable)
                cTable := SELF:Name
            endif
            SELF:CallBack(cTable)
            if self:Connection:DbTransaction != null
                self:DbTransaction := self:Connection:DbTransaction
            endif
            if ! SELF:_TryBindParameters()
                return null
            endif
            return self:DbCommand:ExecuteReader()
        catch e as Exception
            Connection:LastException := e
            return null
        end try
    end method

    /// <summary>
    /// Execute a command that does not return data
    /// </summary>
    /// <param name="cTable">Table name to display for Event Handler</param>
    /// <returns>TRUE when executed successfully</returns>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="SqlDbConnection.LastException"/>
    method ExecuteNonQuery(cTable := "" as string) as LOGIC
        try
            self:Connection:ForceOpen()
            SELF:_InvalidateCache()
            if String.IsNullOrEmpty(cTable)
                cTable := SELF:Name
            endif
            SELF:CallBack(cTable)
            if self:Connection:DbTransaction != null
                self:DbTransaction := self:Connection:DbTransaction
            endif
            if ! SELF:_TryBindParameters()
                return false
            endif
            self:DbCommand:ExecuteNonQuery()
            return true
        catch e as Exception
            Connection:LastException := e
            return false
        end try
    end method

#region Parameters
    private method AddParameter(oParam as SqlDbParameter) as void
        if self:Parameters == null
            self:Parameters := List<SqlDbParameter>{}
        endif
        self:Parameters:Add(oParam)
    end method

    /// <summary>
    /// Add a parameter to the command
    /// </summary>
    /// <param name="nId">Ordinal of the parameter</param>
    /// <param name="oValue">Value for the parameter</param>
    /// <param name="cName">Name of the parameter</param>
    /// <returns>The SqlDbParameter object</returns>
    method AddParameter(nId as long, oValue as object) as SqlDbParameter
        var oParam := SqlDbParameter{nId, oValue}
        self:AddParameter(oParam)
        return oParam
    end method

    /// <inheritdoc cref="AddParameter(System.Int32,System.Object)" />
    method AddParameter(cName as string, oValue as object) as SqlDbParameter
        var oParam := SqlDbParameter{cName, oValue}
        self:AddParameter(oParam)
        return oParam
    end method

    /// <summary>
    /// Remove all the parameters from the command
    /// </summary>
    /// <returns>Nothing</returns>
    method ClearParameters() as void
        self:DbCommand:Parameters:Clear()
        if self:Parameters != null
            self:Parameters:Clear()
        endif
        return
    end method

    /// <summary>
    /// Remove a parameter object from the command
    /// </summary>
    /// <param name="oParam"></param>
    /// <returns>TRUE when the parameter was part of the command Parameters collection</returns>
    method RemoveParameter(oParam as SqlDbParameter) as logic
        if self:Parameters != null .and. self:Parameters:Contains(oParam)
            self:Parameters:Remove(oParam)
            return true
        endif
        return false
    end method

    private method _TryBindParameters() as logic
        try
            if self:Parameters?:Count > 0
                return self:BindParameters()
            else
                DbCommand:Parameters:Clear()
            endif
            return true
        catch e as Exception
            Connection:LastException := e
            return false
        end try
    end method

    /// <summary>Lowest value of the legacy SQL `datetime` type.</summary>
    static internal initonly MinLegacyDateTime := DateTime{1753, 1, 1} as DateTime

    /// <summary>
    /// Bind the parameters to the underlying DbCommand object
    /// </summary>
    /// <returns>TRUE when succesfull</returns>
    /// <remarks> <note type='tip'>When an error occurs then the error is registered in the LastException property of the Connection</note></remarks>
    /// <seealso cref="SqlDbConnection.LastException"/>
    method BindParameters() as logic
        try
            self:DbCommand:Parameters:Clear()
            if self:Parameters != null
                foreach var oParam in self:Parameters
                    var dbParam := Provider:CreateParameter()
                    dbParam:Direction := oParam:Direction
                    dbParam:Value     := oParam:Value
                    if oParam:Value is DateTime var dtValue .and. dtValue < SqlDbCommand.MinLegacyDateTime
                        // A DateTime parameter is inferred as the legacy `datetime` type, which
                        // starts at 1753-01-01 - so DateTime.MinValue, the value an empty DBF date
                        // is stored as in a NOT NULL `date` column, overflowed. datetime2 covers
                        // the full range and converts to `date` on the server.
                        dbParam:DbType := DbType.DateTime2
                    endif
                    if oParam:Ordinal == -1
                        dbParam:ParameterName := oParam:Name
                    endif
                    oParam:DbParameter:= dbParam
                    self:DbCommand:Parameters:Add(dbParam)
                    if oParam:Ordinal != -1
                        oParam:SetName(dbParam:ParameterName)
                    endif
                next
            endif
            return true
        catch e as Exception
            Connection:LastException := e
            return false
        end try
    end method

    /// <summary>
    /// Return the parameterlist as a string
    /// </summary>
    /// <value>A list with Name/Value pairs</value>
    property ParameterList as string
        get
            if self:Parameters == null
                return ""
            endif
            var sb := StringBuilder{}
            var first := true
            foreach var param in self:Parameters
                if (first)
                    first := false
                else
                    sb:Append(", ")
                endif
                sb:Append(param:Name)
                sb:Append("=")
                sb:Append(param:Value)
            next
            return sb:ToString()

        end get
    end property

#endregion

#region Implement IDisposable
    /// <inheritdoc/>
    public override method Dispose() as void
        self:Close()
        super:Dispose()
    end method

#endregion
end class
end namespace
