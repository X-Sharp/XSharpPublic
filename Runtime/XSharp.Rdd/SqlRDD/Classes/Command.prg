//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



USING System
USING System.Collections.Generic
USING System.Text
USING System.Data
USING System.Data.Common


BEGIN NAMESPACE XSharp.RDD.SqlRDD


/// <summary>
/// Connection class.
/// </summary>
CLASS SqlDbCommand INHERIT SqlDbEventObject IMPLEMENTS IDisposable

#region Properties
    PROPERTY Connection     AS SqlDbConnection AUTO
    PROPERTY DbCommand      as DbCommand AUTO
    PROPERTY DbTransaction  AS DbTransaction GET DbCommand:Transaction SET DbCommand:Transaction := Value
    PROPERTY Parameters     AS List<SqlDbParameter> AUTO
    PROPERTY Provider       AS SqlDbProvider GET IIF(Connection == NULL, NULL, Connection:Provider)
#endregion

#region static properties and methods
    STATIC Commands      AS List<SqlDbCommand>
    STATIC CONSTRUCTOR
        Commands := List<SqlDbCommand>{}
    STATIC METHOD FindByHandle(id as IntPtr) AS SqlDbCommand
        IF SqlDbHandles.FindById(id) IS SqlDbCommand VAR oCmd
            return oCmd
        endif
        RETURN NULL
#endregion

    CONSTRUCTOR(cName AS STRING, oConn as SqlDbConnection)
        SUPER(cName)
        SELF:Connection := oConn
        SELF:DbCommand := oConn:Provider:CreateCommand()
        SELF:DbCommand:Connection := oConn:DbConnection
        Commands.Add(SELF)
        RETURN
    METHOD Close() AS LOGIC
        Commands.Remove(SELF)
        RETURN TRUE

    PROPERTY CommandText AS STRING GET DbCommand:CommandText SET DbCommand:CommandText := value

    METHOD GetSchemaTable() AS DataTable
        using var reader := DbCommand:ExecuteReader(CommandBehavior.SchemaOnly)
        var schema := reader:GetSchemaTable()
        RETURN schema
    METHOD GetDataTable(cName as STRING) AS DataTable
        local oDataTable as DataTable
        using var reader := DbCommand:ExecuteReader()
        if SELF:Connection:DbTransaction != NULL
            SELF:DbTransaction := SELF:Connection:DbTransaction
        ENDIF
        oDataTable := DataTable{cName}
        oDataTable:Load(reader)
        return oDataTable

    METHOD ExecuteScalar() AS OBJECT
        if SELF:Connection:DbTransaction != NULL
            SELF:DbTransaction := SELF:Connection:DbTransaction
        ENDIF
        RETURN SELF:DbCommand:ExecuteScalar()

        #region Parameters
    PRIVATE METHOD AddParameter(oParam as SqlDbParameter) AS VOID
        IF SELF:Parameters == NULL
            SELF:Parameters := List<SqlDbParameter>{}
        ENDIF
        oParam:Command := SELF
        SELF:Parameters:Add(oParam)
    METHOD AddParameter(nId as LONG, oValue as OBJECT) AS SqlDbParameter
        VAR oParam := SqlDbParameter{nId, oValue}
        SELF:AddParameter(oParam)
        RETURN oParam

    METHOD AddParameter(cName as STRING, oValue as OBJECT) AS SqlDbParameter
        VAR oParam := SqlDbParameter{cName, oValue}
        SELF:AddParameter(oParam)
        RETURN oParam

    METHOD ClearParameters() AS LOGIC
        SELF:DbCommand:Parameters:Clear()
        IF SELF:Parameters != NULL
            SELF:Parameters:Clear()
        ENDIF
        RETURN SELF:Parameters != NULL

    METHOD RemoveParameter(oParam as SqlDbParameter) AS LOGIC
        IF SELF:Parameters != NULL .and. SELF:Parameters:Contains(oParam)
            SELF:Parameters:Remove(oParam)
            RETURN TRUE
        ENDIF
        RETURN FALSE
    METHOD BindParameters() AS LOGIC
        SELF:DbCommand:Parameters:Clear()
        IF SELF:Parameters == NULL
            RETURN FALSE
        ENDIF
        FOREACH var oParam in SELF:Parameters
            var dbParam := Provider:CreateParameter()
            dbParam:Direction := oParam:Direction
            dbParam:Value     := oParam:Value
            IF oParam:Ordinal == -1
                dbParam:ParameterName := oParam:Name
            ENDIF
            oParam:DbParameter:= dbParam
            SELF:DbCommand:Parameters:Add(dbParam)
            IF oParam:Ordinal != -1
                oParam:SetName(dbParam:ParameterName)
            endif
        NEXT
        RETURN TRUE
    PROPERTY ParameterList as STRING
        GET
            if SELF:Parameters == NULL
                RETURN ""
            ENDIF
            var sb := StringBuilder{}
            var first := TRUE
            FOREACH var param in SELF:Parameters
                if (first)
                    first := FALSE
                else
                    sb:Append(", ")
                endif
                sb:Append(param:Name)
                sb:Append("=")
                sb:Append(param:Value)
            NEXT
            RETURN sb:ToString()

        END GET
    END PROPERTY

        #endregion

    #region Implement IDisposable

    PUBLIC METHOD Dispose() AS VOID
        SELF:Close()

    #endregion
END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
