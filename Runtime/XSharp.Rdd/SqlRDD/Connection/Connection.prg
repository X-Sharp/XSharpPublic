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
CLASS SqlDbConnection INHERIT SqlDbObject
#region Properties
    PROPERTY IsOpen             AS LOGIC GET DbConnection != NULL .AND. DbConnection:State == ConnectionState.Open
    PROPERTY Provider           AS SqlDbProvider AUTO
    PROPERTY DbConnection       AS DbConnection AUTO
    PROPERTY UserName		    AS STRING AUTO
    PROPERTY PassWord		    AS STRING AUTO
    PROPERTY ConnectionString   AS STRING AUTO
    PROPERTY Schema             AS Dictionary<string, SqlDbTableDef> AUTO
    PROPERTY RDDs               AS IList<SQLRDD> AUTO
    PROPERTY Cached             AS LOGIC AUTO
    PROPERTY Handle             AS IntPtr AUTO
    PROPERTY TimeOut            AS LONG AUTO
#endregion

#region static properties and methods
    CONST DefaultConnection := "DEFAULT" AS STRING
    STATIC Connections      AS List<SqlDbConnection>
    STATIC RandomGenerator  AS Random
    STATIC INTERNAL PROPERTY DefaultCached  AS LOGIC AUTO
    STATIC Constructor()
        Connections     := List<SqlDbConnection>{}
        RandomGenerator := Random{ 12345 }
        DefaultCached   := TRUE
    STATIC METHOD GetId() as IntPtr
        local ok := TRUE as LOGIC
        local id as IntPtr
        REPEAT
            id := IntPtr{RandomGenerator:Next()}
            FOREACH var connection in Connections
                if connection:Handle == id
                    ok := FALSE
                    EXIT
                endif
            NEXT
        UNTIL ok
        return id
    STATIC METHOD FindByHandle(id as IntPtr) AS SqlDbConnection
        FOREACH var connection in Connections
            if connection:Handle == id
                return connection
            endif
        NEXT
        RETURN NULL
    STATIC METHOD FindByName(name as String) AS SqlDbConnection
        FOREACH var connection in Connections
            if String.Compare(connection:Name, name, true) == 0
                return connection
            endif
        NEXT
        RETURN NULL
#endregion

    CONSTRUCTOR(cName AS STRING, cConnectionString as STRING, cUser as STRING, cPassword as STRING)
        SUPER(cName)
        SELF:ConnectionString := cConnectionString
        SELF:UserName         := cUser
        SELF:PassWord         := cPassword
        RDDs            := List<SQLRDD>{}
        Schema          := Dictionary<string, SqlDbTableDef>{StringComparer.OrdinalIgnoreCase}
        Provider        := SqlDbProvider.Current
        DbConnection    := Provider:CreateConnection()
        Handle          := GetId()
        TimeOut         := 15
        Cached          := DefaultCached
        Connections.Add(SELF)
        SELF:ForceOpen()
        RETURN

    METHOD Close() AS LOGIC
        IF SELF:RDDs:Count > 0
            RETURN FALSE
        ENDIF
        SELF:CloseConnection()
        Connections.Remove(SELF)
        RETURN TRUE
    PRIVATE METHOD CloseConnection AS VOID
        IF SELF:DbConnection:State == ConnectionState.Open
            SELF:DbConnection:Close()
        ENDIF
        RETURN
    INTERNAL METHOD ForceOpen AS VOID
        IF SELF:DbConnection:State != ConnectionState.Open
            SELF:DbConnection:ConnectionString  := SELF:ConnectionString
            SELF:DbConnection:Open()
        ENDIF
    METHOD AddRdd(oRDD AS SQLRDD) AS LOGIC
        RDDs:Add(oRDD)
        RETURN TRUE
    METHOD RemoveRdd(oRDD AS SQLRDD) AS LOGIC
        IF RDDs:Contains(oRDD)
            RDDs:Add(oRDD)
        ENDIF
        IF RDDs:Count == 0 .and. ! SELF:Cached
        ENDIF
        RETURN TRUE
    METHOD BeginTrans AS LOGIC
        RETURN FALSE
    METHOD Close(nWorkArea AS LONG) AS LOGIC
        RETURN FALSE
    METHOD CloseAll() AS VOID
        RETURN
    METHOD CommitTrans() AS LOGIC
        RETURN FALSE
    METHOD DeleteTableDef(sTableName AS STRING) AS LOGIC
        RETURN FALSE
    INTERNAL METHOD GetConfigFile(sPath AS STRING) AS IniFile
        RETURN NULL
    INTERNAL METHOD GetCurrentConfigFile(cPath AS STRING, cFileName AS STRING) AS IniFile
        RETURN NULL
    INTERNAL METHOD GetDefaultConnectionName(oIni AS IniFile) AS STRING
        RETURN NULL
    METHOD GetStructure(TableName AS STRING) AS SqlDbTableDef
        if SELF:Schema:ContainsKey(TableName)
            RETURN SELF:Schema[TableName]
        ENDIF
        try
            var table := SELF:Provider:QuotePrefix+TableName+SELF:Provider:QuoteSuffix
            if String.IsNullOrEmpty(SELF:Provider:QuotePrefix) .and. TableName:IndexOf(" ") > 0
                table := """"+TableName+""""
            endif
            var query := "Select * from "+table+SELF:Provider:WhereClause+" 0=1 "
            var cmd   := Self:Provider:CreateCommand()
            cmd:CommandText := query
            cmd:Connection := SELF:DbConnection
            using var reader := cmd:ExecuteReader(CommandBehavior.SchemaOnly)
            var schema := reader:GetSchemaTable()
            var oCols := List<SqlDbColumnDef>{}
            foreach row as DataRow in schema:Rows
                var oCol            := SqlDbColumnDef{row["ColumnName"]:ToString()}
                oCol:Type           := (System.Type) row["DataType"]
                oCol:Scale          := Convert.ToInt32(row["NumericScale"])
                oCol:Precision      := Convert.ToInt32(row["NumericPrecision"])
                oCol:OrdinalPosition:= Convert.ToInt32(row["ColumnOrdinal"])
                oCol:Length         := Convert.ToInt32(row["ColumnSize"])
                oCol:Updatable      := !((LOGIC) row["IsReadOnly"])
                if oCol:Scale == 255
                    oCol:Scale := 0
                endif
                if oCol:Precision == 255
                    oCol:Precision := 0
                endif
                oCols:Add(oCol)

            next
            var oTd   := SqlDbTableDef{TableName, oCols}
            SELF:Schema:Add(TableName, oTd)
            RETURN oTd
        CATCH e as Exception
            ? "Error reading table ",TableName, e:Message
        END TRY
        RETURN NULL
    METHOD GetTables(filter := "" as STRING) AS List<STRING>
        var dt := SELF:DbConnection:GetSchema("Tables")
        var result := List<String>{}
        foreach row as DataRow in dt:Rows
            if String.IsNullOrEmpty(filter)
                result:Add(row["TABLE_NAME"]:ToString())
            else
                var type := row["TABLE_TYPE"]:ToString()
                if type:IndexOf(filter, StringComparison.OrdinalIgnoreCase) >= 0
                    result:Add(row["TABLE_NAME"]:ToString())
                endif
            endif
        next
        RETURN result
    METHOD GetMetaDataCollections() as List<String>
        var dt := SELF:DbConnection:GetSchema(DbMetaDataCollectionNames.MetaDataCollections)
        var result := List<String>{}
        foreach row as DataRow in dt:Rows
            result:Add(row["CollectionName"]:ToString())
        next
        RETURN result

    METHOD RollBackTrans AS LOGIC
        RETURN FALSE

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD

DEFINE strConnections 	:= "Connections"
DEFINE strConString	  	:= "ConnectionString"
DEFINE strPassWord		:= "Password"
DEFINE strUserId		:= "UserId"



