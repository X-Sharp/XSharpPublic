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

PUBLIC DELEGATE XSharp.SqlRDDEventHandler(oSender AS Object, e AS XSharp.RDD.SqlRDD.SqlRddEventArgs) AS OBJECT

BEGIN NAMESPACE XSharp.RDD.SqlRDD


/// <summary>
/// Connection class.
/// </summary>
CLASS SqlDbConnection INHERIT SqlDbObject
    // Constants for the Metadata collections
    INTERNAL CONST COLLECTIONNAME := "CollectionName" AS STRING
    INTERNAL CONST TABLECOLLECTION := "Tables" as STRING
    INTERNAL CONST TABLENAME := "TABLE_NAME" as STRING
    INTERNAL CONST TABLETYPE := "TABLE_TYPE" as STRING
    // Constants for the columns in a schema

#region Properties
    PROPERTY IsOpen             AS LOGIC GET DbConnection != NULL .AND. DbConnection:State == ConnectionState.Open
    PROPERTY Provider           AS SqlDbProvider AUTO
    PROPERTY DbConnection       AS DbConnection AUTO
    PROPERTY ConnectionString   AS STRING AUTO
    PROPERTY Schema             AS Dictionary<string, SqlDbTableDef> AUTO
    PROPERTY RDDs               AS IList<SQLRDD> AUTO
    PROPERTY KeepOpen           AS LOGIC AUTO
    PROPERTY Handle             AS IntPtr AUTO
    PROPERTY TimeOut            AS LONG AUTO

#endregion

    PUBLIC EVENT CallBack AS SqlRDDEventHandler
    PRIVATE METHOD RaiseEvent(oConn AS SqlDbConnection, nEvent AS SqlRDDEventReason, cTable as string, oValue AS Object) AS Object
        VAR oArgs := SqlRddEventArgs{ nEvent, cTable, oValue}
        IF @@CallBack != NULL
            @@CallBack ( oConn, oArgs )
        ENDIF
        RETURN oArgs:Value
    PRIVATE METHOD RaiseStringEvent(oConn AS SqlDbConnection, nEvent AS SqlRDDEventReason, cTable as string, oValue AS STRING) as String
        var result := RaiseEvent(oConn, nEvent, cTable, oValue)
        if result IS String var strValue
            RETURN strValue
        ENDIF
        return oValue
    PRIVATE METHOD RaiseIntEvent(oConn AS SqlDbConnection, nEvent AS SqlRDDEventReason, cTable as string, oValue AS INT) as INT
        var result := RaiseEvent(oConn, nEvent, cTable, oValue)
        if result IS Int var intValue
            RETURN intValue
        ENDIF
        return oValue
    PRIVATE METHOD RaiseListEvent(oConn AS SqlDbConnection, nEvent AS SqlRDDEventReason, cTable as string, oValue AS IList<String>) as IList<String>
        var result := RaiseEvent(oConn, nEvent, cTable, oValue)
        if result IS IList<String> VAR listValue
            RETURN listValue
        ENDIF
        return oValue
    PRIVATE METHOD RaiseLogicEvent(oConn AS SqlDbConnection, nEvent AS SqlRDDEventReason, cTable as string, oValue AS LOGIC) as LOGIC
        var result := RaiseEvent(oConn, nEvent, cTable, oValue)
        if result IS Logic var logValue
            RETURN logValue
        ENDIF
        return oValue

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

    CONSTRUCTOR(cName AS STRING, cConnectionString as STRING, @@Callback := NULL as SqlRDDEventHandler)
        SUPER(cName)
        SELF:ConnectionString := cConnectionString
        RDDs            := List<SQLRDD>{}
        Schema          := Dictionary<string, SqlDbTableDef>{StringComparer.OrdinalIgnoreCase}
        Provider        := SqlDbProvider.Current
        DbConnection    := Provider:CreateConnection()
        Handle          := GetId()
        TimeOut         := 15
        KeepOpen        := DefaultCached
        IF @@Callback != NULl
            SELF:CallBack += @@Callback
        ENDIF
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
            var connStr := RaiseStringEvent(SELF, SqlRDDEventReason.ConnectionString, "", SELF:ConnectionString)
            SELF:DbConnection:ConnectionString  := connStr
            SELF:DbConnection:Open()
        ENDIF
#region RDD registration
    METHOD AddRdd(oRDD AS SQLRDD) AS LOGIC
        RDDs:Add(oRDD)
        RETURN TRUE
    METHOD RemoveRdd(oRDD AS SQLRDD) AS LOGIC
        IF RDDs:Contains(oRDD)
            RDDs:Remove(oRDD)
        ENDIF
        IF RDDs:Count == 0 .and. ! SELF:KeepOpen
            SELF:CloseConnection()
        ENDIF
        RETURN TRUE
#endregion
#region Transactions
    METHOD BeginTrans AS LOGIC
        RETURN FALSE
    METHOD CommitTrans() AS LOGIC
        RETURN FALSE
#endregion
    METHOD RollBackTrans AS LOGIC
        RETURN FALSE


#region Schema Info
    METHOD DeleteTableDef(sTableName AS STRING) AS LOGIC
        IF SELF:Schema:ContainsKey(sTableName)
            SELF:Schema:Remove(sTableName)
            RETURN TRUE
        ENDIF
        RETURN FALSE
#endregion
#region MetaData
    METHOD GetStructureForQuery(cQuery as STRING, TableName as STRING) AS SqlDbTableDef
        cQuery := RaiseStringEvent(SELF, SqlRDDEventReason.CommandText, TableName, cQuery)
        var longFieldNames := TRUE
        longFieldNames := RaiseLogicEvent(SELF,SqlRDDEventReason.LongFieldNames, TableName, longFieldNames)
        var cmd   := Self:Provider:CreateCommand()
        cmd:CommandText := cQuery
        cmd:Connection := SELF:DbConnection
        using var reader := cmd:ExecuteReader(CommandBehavior.SchemaOnly)
        var schema := reader:GetSchemaTable()
        var oCols := List<SqlDbColumnDef>{}
        var fieldNames := List<String>{}
        foreach row as DataRow in schema:Rows
            local colInfo  := SQLHelpers.GetColumnInfoFromSchemaRow(row, fieldNames, longFieldNames) AS DbColumnInfo
            oCols:Add(SqlDbColumnDef{ colInfo })
        next
        var oTd   := SqlDbTableDef{TableName, oCols}
        RETURN oTd

    METHOD GetStructure(TableName AS STRING) AS SqlDbTableDef
        if SELF:Schema:ContainsKey(TableName)
            RETURN SELF:Schema[TableName]
        ENDIF
        try
            var table := SELF:Provider:QuotePrefix+TableName+SELF:Provider:QuoteSuffix
            if String.IsNullOrEmpty(SELF:Provider:QuotePrefix) .and. TableName:IndexOf(" ") > 0
                table := SqlDbProvider.DefaultQuotePrefix+TableName+SqlDbProvider.DefaultQuoteSuffix
            endif
            var list  := RaiseListEvent(SELF, SqlRDDEventReason.ColumnList, TableName, List<String>{}{"*"})
            var columnList := List2String(list)
            var query := SqlDbProvider.SelectClause+columnList+SqlDbProvider.FromClause+table+SqlDbProvider.WhereClause+"0=1"
            query := RaiseStringEvent(SELF, SqlRDDEventReason.CommandText, TableName, query)
            var oTd := GetStructureForQuery(query,TableName)
            SELF:Schema:Add(TableName, oTd)
            RETURN oTd
        CATCH e as Exception
            ? "Error reading table ",TableName, e:Message
        END TRY
        RETURN NULL
    METHOD GetTables(filter := "" as STRING) AS List<STRING>
        var dt := SELF:DbConnection:GetSchema(TABLECOLLECTION)
        var result := List<String>{}
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
        RETURN result
    METHOD GetMetaDataCollections() as List<String>
        var dt := SELF:DbConnection:GetSchema(DbMetaDataCollectionNames.MetaDataCollections)
        var result := List<String>{}
        foreach row as DataRow in dt:Rows
            result:Add(row[COLLECTIONNAME]:ToString())
        next
        RETURN result
#endregion

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD
