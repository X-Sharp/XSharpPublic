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
    property CommandText    as string get DbCommand:CommandText set DbCommand:CommandText := value

#endregion

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
    constructor(cName as string, oConn as SqlDbConnection)
        super(cName)
        self:Connection := oConn
        oConn:RegisterCommand(SELF)
        self:DbCommand := oConn:Provider:CreateCommand()
        self:DbCommand:Connection := oConn:DbConnection
        Commands.Add(self)
        return
    end constructor

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
    method GetDataTable(cTable as string) as DataTable
        try
            local oDataTable as DataTable
            using var reader := SELF:ExecuteReader(cTable)
            if reader == null
                return null
            endif
            oDataTable := DataTable{cTable}
            oDataTable:Load(reader)
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
    method ExecuteScalar(cTable := __FUNCTION__ as string) as object
        try
            self:CommandText := SELF:Connection:RaiseStringEvent(self, SqlRDDEventReason.CommandText, cTable, self:CommandText)
            if self:Connection:DbTransaction != null
                self:DbTransaction := self:Connection:DbTransaction
            endif
            if ! SELF:_TryBindParameters()
                return null
            endif
            return self:DbCommand:ExecuteScalar()
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
    method ExecuteReader(cTable := __FUNCTION__ as string) as DbDataReader
        try
            self:CommandText := SELF:Connection:RaiseStringEvent(self, SqlRDDEventReason.CommandText, cTable, self:CommandText)
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
    method ExecuteNonQuery(cTable := __FUNCTION__ as string) as LOGIC
        try
            self:CommandText := SELF:Connection:RaiseStringEvent(self, SqlRDDEventReason.CommandText, cTable, self:CommandText)

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
