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
    PROTECTED _commandBuilder as DbCommandBuilder

#region Properties
    PROPERTY Connection     AS SqlDbConnection AUTO
    PROPERTY DbCommand      as DbCommand AUTO
    PROPERTY DbTransaction  AS DbTransaction GET DbCommand:Transaction SET DbCommand:Transaction := Value
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

    #region Implement IDisposable

    PUBLIC METHOD Dispose() AS VOID
        SELF:Close()

    #endregion
END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
