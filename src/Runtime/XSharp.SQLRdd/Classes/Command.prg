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


begin namespace XSharp.RDD.SqlRDD


/// <summary>
/// Connection class.
/// </summary>
class SqlDbCommand inherit SqlDbHandleObject implements IDisposable

#region Properties
    property Connection     as SqlDbConnection auto
    property DbCommand      as DbCommand auto
    property DbTransaction  as DbTransaction get DbCommand:Transaction set DbCommand:Transaction := value
    property Parameters     as List<SqlDbParameter> auto
    property Provider       as SqlDbProvider get iif(Connection == null, null, Connection:Provider)
#endregion

#region static properties and methods
    static Commands      as List<SqlDbCommand>
    static constructor
        Commands := List<SqlDbCommand>{}
    static method FindByHandle(id as IntPtr) as SqlDbCommand
        if SqlDbHandles.FindById(id) is SqlDbCommand var oCmd
            return oCmd
        endif
        return null
#endregion

    constructor(cName as string, oConn as SqlDbConnection)
        super(cName)
        self:Connection := oConn
        self:DbCommand := oConn:Provider:CreateCommand()
        self:DbCommand:Connection := oConn:DbConnection
        Commands.Add(self)
        return
    method Close() as logic
        Commands.Remove(self)
        return true

    property CommandText as string get DbCommand:CommandText set DbCommand:CommandText := value

    method GetSchemaTable() as DataTable
        using var reader := DbCommand:ExecuteReader(CommandBehavior.SchemaOnly)
        var schema := reader:GetSchemaTable()
        return schema
    method GetDataTable(cName as string) as DataTable
        local oDataTable as DataTable
        using var reader := DbCommand:ExecuteReader()
        if self:Connection:DbTransaction != null
            self:DbTransaction := self:Connection:DbTransaction
        endif
        oDataTable := DataTable{cName}
        oDataTable:Load(reader)
        return oDataTable

    method ExecuteScalar() as object
        if self:Connection:DbTransaction != null
            self:DbTransaction := self:Connection:DbTransaction
        endif
        return self:DbCommand:ExecuteScalar()

#region Parameters
    private method AddParameter(oParam as SqlDbParameter) as void
        if self:Parameters == null
            self:Parameters := List<SqlDbParameter>{}
        endif
        oParam:Command := self
        self:Parameters:Add(oParam)
    method AddParameter(nId as long, oValue as object) as SqlDbParameter
        var oParam := SqlDbParameter{nId, oValue}
        self:AddParameter(oParam)
        return oParam

    method AddParameter(cName as string, oValue as object) as SqlDbParameter
        var oParam := SqlDbParameter{cName, oValue}
        self:AddParameter(oParam)
        return oParam

    method ClearParameters() as logic
        self:DbCommand:Parameters:Clear()
        if self:Parameters != null
            self:Parameters:Clear()
        endif
        return self:Parameters != null

    method RemoveParameter(oParam as SqlDbParameter) as logic
        if self:Parameters != null .and. self:Parameters:Contains(oParam)
            self:Parameters:Remove(oParam)
            return true
        endif
        return false
    method BindParameters() as logic
        self:DbCommand:Parameters:Clear()
        if self:Parameters == null
            return false
        endif
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
        return true
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

    public method Dispose() as void
        self:Close()

#endregion
end class
end namespace // XSharp.RDD.SqlRDD
