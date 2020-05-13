//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.Data.Common
USING System.Data
USING System.Diagnostics
USING System.Threading
USING XSharp.VFP
USING System.Reflection
USING XSharp.RDD
USING XSharp.RDD.Enums


INTERNAL CLASS XSharp.VFP.SQLStatement
    PROTECT _oConnection     AS SQLConnection
    PROTECT _oNetCommand     AS DbCommand
    PROTECT _oLastDataReader AS DbDataReader
    PROTECT _nextCursorNo    AS LONG
    PROTECT _aSyncState      AS AsyncState
    PROTECT _oThread         AS Thread
    PROTECT _aQueryResult    AS ARRAY
    PROTECT _aResult         AS IList<IRdd>
    PROTECT _aParams         AS IList<SQLParameter>
    PROTECT _returnsRows     AS LOGIC
    PROTECT _hasOutParams    AS LOGIC
    PROTECT _lastException   AS System.Exception
    
    PROPERTY Connected          AS LOGIC GET Connection:State == System.Data.ConnectionState.Open
    PROPERTY Connection         AS SQLConnection GET _oConnection
    PROPERTY Asynchronous       AS LOGIC  AUTO GET SET
    PROPERTY BatchMode          AS LOGIC  AUTO GET SET
    PROPERTY ConnectBusy        AS LOGIC  GET Connection:ConnectBusy
    PROPERTY ConnectionString   AS STRING GET Connection:ConnectionString   
    PROPERTY ConnectionTimeOut  AS LONG   GET Connection:ConnectionTimeOut  SET Connection:ConnectionTimeOut := Value
    PROPERTY CursorName         AS STRING AUTO GET SET
    PROPERTY DataSource         AS STRING GET Connection:DataSource         SET Connection:DataSource := Value
    PROPERTY DisconnectRollback AS LOGIC AUTO GET SET
    PROPERTY DispWarnings       AS LOGIC AUTO GET SET
    PROPERTY Handle             AS LONG   AUTO GET SET
    PROPERTY IdleTimeout        AS LONG  AUTO GET SET
    PROPERTY LastException      AS System.Exception GET _lastException
    PROPERTY ODBChdbc           AS DbConnection GET SELF:Connection:NetConnection
    PROPERTY ODBChstmt          AS DbCommand    GET _oNetCommand
    PROPERTY Password           AS STRING GET Connection:Password   
    PROPERTY Shared             AS LOGIC  GET Connection:Shared             SET Connection:Shared := Value
    PROPERTY UserId             AS STRING GET Connection:UserId     
    PROPERTY PacketSize         AS LONG   GET Connection:PacketSize SET Connection:PacketSize := Value
    PROPERTY Prepared           AS LOGIC AUTO GET PRIVATE SET
    PROPERTY QueryTimeOut       AS LONG  GET _oNetCommand:CommandTimeout SET _oNetCommand:CommandTimeout := Value
    PROPERTY TransactionMode    AS LONG  AUTO GET SET
    PROPERTY UsesTransaction    AS LOGIC GET SELF:TransactionMode == DB_TRANSMANUAL
    PROPERTY WaitTime           AS LONG  AUTO GET SET

    PROPERTY ParamsObject       AS OBJECT AUTO GET SET


    METHOD SetDefaults() AS VOID
        SELF:Asynchronous       := (LOGIC) SQLSupport.GetDefault(SQLProperty.Asynchronous)
        SELF:BatchMode          := (LOGIC) SQLSupport.GetDefault(SQLProperty.BatchMode)
        SELF:DisconnectRollback := (LOGIC) SQLSupport.GetDefault(SQLProperty.DisconnectRollback)
        SELF:DispWarnings       := (LOGIC) SQLSupport.GetDefault(SQLProperty.DispWarnings)
        SELF:IdleTimeout        := (LONG) SQLSupport.GetDefault(SQLProperty.IdleTimeout)
        SELF:PacketSize         := (LONG) SQLSupport.GetDefault(SQLProperty.PacketSize)
        SELF:Prepared           := FALSE
        SELF:TransactionMode    := (LONG) SQLSupport.GetDefault(SQLProperty.Transactions)
        SELF:WaitTime           := (LONG) SQLSupport.GetDefault(SQLProperty.WaitTime)
        SELF:_aSyncState        := AsyncState.Idle

    PRIVATE METHOD _AllocateCommand() AS VOID
        SELF:_oNetCommand := SELF:Connection:Factory:CreateCommand()
        SELF:_oNetCommand:Connection := SELF:Connection:NetConnection
        SELF:_oNetCommand:CommandTimeout := SELF:QueryTimeOut
        _oConnection:AddStatement(SELF)
        RETURN
        
    PRIVATE METHOD _CloseReader() AS VOID
        IF _oLastDataReader != NULL
            _oLastDataReader:Close()
            _oLastDataReader := NULL
        ENDIF


    PRIVATE METHOD CreateFile(oSchema AS DataTable, cCursorName AS STRING) AS IRdd
        LOCAL nFields AS LONG
        LOCAL aStruct AS ARRAY
        LOCAL aFieldNames AS List<STRING>
        nFields := oSchema:Rows:Count
        aFieldNames := List<STRING>{}
        aStruct := ArrayNew( (DWORD) nFields)
        nFields := 1
        FOREACH schemaRow AS DataRow IN oSchema:Rows
            VAR fieldInfo     := FromSchema(schemaRow, aFieldNames)
            fieldInfo:Ordinal := nFields
            aStruct[nFields]  := {fieldInfo:Name, fieldInfo:FieldTypeStr, fieldInfo:Length, fieldInfo:Decimals, fieldInfo:ColumnName, fieldInfo:Flags, fieldInfo}
            nFields++
        NEXT
        nFields := aStruct:Count
        VAR cTemp := System.IO.Path.GetTempFileName()
        DbCreate(cTemp, aStruct, "DBFVFP")
        VAR nArea := _SelectString(cCursorName)
        IF nArea != 0
            DbCloseArea()
        ENDIF
        VoDbUseArea(TRUE, "DBFVFPSQL",cTemp,cCursorName,FALSE,FALSE)
        LOCAL oRDD AS IRdd
        oRDD := (IRdd) DbInfo(DbInfo.DBI_RDD_OBJECT)

        FOR VAR nI := 1 TO ALen(aStruct)
            LOCAL fieldInfo AS DbColumnInfo
            fieldInfo := aStruct[nI, 7]
            oRDD:FieldInfo(nI, DBS_COLUMNINFO, fieldInfo)
        NEXT
        RETURN oRDD
       




    PRIVATE METHOD _CopyFromReaderToRDD(oDataReader AS DbDataReader, oRDD AS IRdd) AS LOGIC
        LOCAL oMIGet := NULL AS MethodInfo 
        VAR nFields := oRDD:FieldCount
        VAR nCounter := 0 
        // DBFVFPSQL has a method SetData to set all the values of the current row.
        oMIGet := oRDD:GetType():GetMethod("GetData", BindingFlags.Instance+BindingFlags.IgnoreCase+BindingFlags.Public)
        IF oMIGet != NULL
            VAR GetData := (SqlGetData) oMIGet:CreateDelegate(typeof(SqlGetData), oRDD) 
            DO WHILE oDataReader:Read()
                oRDD:Append(TRUE)
                // Get the data array from the workarea
                VAR data := GetData()
                // and fetch its values. This automatically updates the array that is owned by the RDD
                oDataReader:GetValues(data)
                IF SELF:Asynchronous .AND. ++nCounter % 100 == 0
                    IF SELF:_aSyncState == AsyncState.Cancelling
                        EXIT
                    ENDIF
                ENDIF
            ENDDO
        ELSE
            VAR data := OBJECT[]{nFields+1}  // 1 extra for the NullFlags
            DO WHILE oDataReader:Read()
                oRDD:Append(TRUE)
                // use our local data array
                oDataReader:GetValues(data)
                // and write the values to the RDD
                FOR VAR nFld := 1 UPTO nFields
                    oRDD:PutValue(nFld, data[nFld])
                NEXT
                IF SELF:Asynchronous .AND. ++nCounter % 100 == 0
                    IF SELF:_aSyncState == AsyncState.Cancelling
                        EXIT
                    ENDIF
                ENDIF
            ENDDO
        ENDIF
        RETURN TRUE


    PRIVATE METHOD _CreateWorkarea(oSchema AS DataTable, oDataReader AS DbDataReader, cCursorName AS STRING) AS LOGIC
        LOCAL oRDD AS IRdd
        oRDD := SELF:CreateFile(oSchema, cCursorName)
        SELF:_CopyFromReaderToRDD(oDataReader, oRDD) 
        RETURN TRUE


    PRIVATE METHOD _ReturnsRows(cCommand AS STRING) AS LOGIC
        LOCAL aParts := cCommand:Split(" ()":ToCharArray()) AS STRING[]
        IF aParts:Length > 0
            LOCAL cWord := aParts[1]:ToLower() AS STRING
            SWITCH cWord
            CASE "select"
            CASE "execute"
                RETURN TRUE
            // dml
            CASE "insert"
            CASE "delete"
            CASE "update"
            // ddl
            CASE "create"
            CASE "drop"
            CASE "alter"
                RETURN FALSE
            OTHERWISE
                RETURN TRUE  //? 
            END SWITCH
        ENDIF
        RETURN FALSE


    
    CONSTRUCTOR(cDataSource AS STRING, cUser AS STRING, cPassword AS STRING, lShared AS LOGIC)
        _oConnection    := SQLConnection{cDataSource, cUser, cPassword, lShared}
        SELF:SetDefaults()
        SELF:_AllocateCommand()
        RETURN 
        

    CONSTRUCTOR(cConnectionString AS STRING, lShared AS LOGIC)
        _oConnection := SQLConnection{cConnectionString, lShared}
        SELF:SetDefaults()
        SELF:_AllocateCommand()
        RETURN

    CONSTRUCTOR(oConnection AS SQLConnection)
        _oConnection := oConnection
        SELF:SetDefaults()
        SELF:_AllocateCommand()
        RETURN


    METHOD BeginTransaction AS LOGIC
        IF SELF:UsesTransaction
            IF SELF:Connection:Transaction == NULL
                SELF:Connection:BeginTransaction()
            ENDIF
            SELF:_oNetCommand:Transaction := SELF:Connection:Transaction
        ENDIF
        SELF:_lastException := NULL
        RETURN TRUE



    METHOD Cancel AS LOGIC
        IF SELF:_aSyncState == AsyncState.Executing
            BEGIN LOCK SELF
                SELF:_aSyncState := AsyncState.Cancelling
            END LOCK
            DO WHILE SELF:_aSyncState == AsyncState.Cancelling
                System.Threading.Thread.Sleep(500)
            ENDDO
            SELF:ThreadComplete() 
            BEGIN LOCK SELF
                SELF:_aSyncState := AsyncState.Idle
            END LOCK
            RETURN TRUE
        ENDIF
        RETURN FALSE
        
    METHOD Commit AS LOGIC
        SELF:_CloseReader()
        RETURN SELF:Connection:CommitTransaction()

    METHOD Rollback AS LOGIC
        SELF:_CloseReader()
        RETURN SELF:Connection:RollbackTransaction()


    METHOD DisConnect AS LOGIC
        SELF:_CloseReader()
        IF SELF:Connected
            IF SELF:DisconnectRollback
                SELF:Rollback()
            ELSE
                SELF:Commit()
            ENDIF
            _oNetCommand:Dispose()
            IF ! SELF:Connection:Shared
                SELF:Connection:Close()
            ENDIF
        ENDIF
        _oConnection:RemoveStatement(SELF)
        _oNetCommand := NULL
        _oConnection := NULL
        RETURN TRUE

    METHOD Prepare(cCommand AS STRING, cCursorName AS STRING) AS LONG
        SELF:_CloseReader()
        SELF:_oNetCommand:CommandText := cCommand
        SELF:_oNetCommand:Prepare()
        SELF:Prepared := TRUE
        SELF:CursorName := cCursorName
        RETURN 1


    PRIVATE METHOD _SaveResult(aInfo AS ARRAY) AS LONG
        CopyToInfo(_aQueryResult, aInfo)
        IF SELF:_lastException != NULL
            RETURN -1
        ENDIF
        IF ALen(_aQueryResult) > 0
            RETURN (LONG) ALen(_aQueryResult)
        ENDIF
        RETURN 0

    METHOD _AsyncExecuteResult(aInfo AS ARRAY) AS LONG
        IF SELF:Asynchronous
            SWITCH SELF:_aSyncState
            CASE AsyncState.Executing
                RETURN 0
            CASE AsyncState.Ready
                SELF:ThreadComplete()
                RETURN SELF:_SaveResult(aInfo)
            CASE AsyncState.Idle
            CASE AsyncState.Cancelled
                SELF:CopyToCursorAsync()
                RETURN SELF:_SaveResult(aInfo)
            CASE AsyncState.Exception
                RETURN -1
            CASE AsyncState.Cancelling
                RETURN 0
            END SWITCH
        ENDIF
        RETURN 0

    METHOD _CreateParameters() AS LOGIC
        SELF:_oNetCommand:Parameters:Clear()
        IF SELF:_aParams:Count == 0
            RETURN TRUE
        ENDIF
        IF SELF:ParamsObject == NULL
            // Parameters but no values
            RETURN FALSE
        ENDIF
        SELF:_hasOutParams := FALSE
        FOREACH oParam AS SQLParameter IN SELF:_aParams
            LOCAL oDbParam AS DbParameter
            oDbParam := SELF:Connection:Factory:CreateParameter()
            oParam:DbParameter := oDbParam
            oDbParam:ParameterName := oParam:Name
            IF oParam:ByRef
                oDbParam:Direction := ParameterDirection.InputOutput
                SELF:_hasOutParams := TRUE
            ELSE
                oDbParam:Direction := ParameterDirection.Input
            ENDIF
            LOCAL oValue        AS OBJECT
            IF SQLReflection.GetPropertyValue(SELF:ParamsObject,oParam:Name, OUT oValue)
                IF oValue IS USUAL 
                    VAR uType := typeof(USUAL)
                    VAR mi    := uType:GetMethod("ToObject")
                    oValue    := mi:Invoke(NULL,<OBJECT>{oValue})
                ENDIF
                oDbParam:Value         := oValue
                IF oValue IS STRING .AND. oParam:ByRef 
                    oDbParam:Size := 4096
                ENDIF
            ELSE
                // Can't get value
                RETURN FALSE
            ENDIF
            SELF:_oNetCommand:Parameters:Add(oDbParam)
        NEXT
        RETURN TRUE

    METHOD _WriteOutParameters() AS LOGIC
        IF SELF:_hasOutParams
            FOREACH oParam AS SQLParameter IN SELF:_aParams
                IF oParam:ByRef
                    LOCAL oValue := oParam:DbParameter:Value AS OBJECT
                    IF ! SQLReflection.SetPropertyValue(SELF:ParamsObject,oParam:Name, oValue)
                        RETURN FALSE
                    ENDIF
                ENDIF
            NEXT
        ENDIF
        RETURN TRUE

    METHOD Execute(cCommand AS STRING, cCursorName AS STRING, aInfo AS ARRAY) AS LONG
        SELF:BeginTransaction()
        cCommand := SELF:ParseCommand(cCommand, SELF:Connection:Factory:ParameterPrefix, SELF:Connection:Factory:ParameterNameInQuery)
        SELF:_oNetCommand:CommandText := cCommand
        IF ! SELF:_CreateParameters()
           RETURN -1
        ENDIF
        SELF:CursorName := cCursorName
        IF SELF:Asynchronous
            RETURN SELF:_AsyncExecuteResult(aInfo)
        ELSE
            SELF:CopyToCursor()
        ENDIF
        RETURN SELF:_SaveResult(aInfo)        

        
    METHOD Execute(aInfo AS ARRAY) AS LONG
        SELF:BeginTransaction()
        IF SELF:Asynchronous
            RETURN SELF:_AsyncExecuteResult(aInfo)
        ELSE
            SELF:CopyToCursor()
        ENDIF
        RETURN SELF:_SaveResult(aInfo)        

    PRIVATE METHOD CopyToInfo(aResult AS ARRAY, aInfo AS ARRAY) AS VOID
        IF aResult != NULL_ARRAY
            ASize(aInfo, ALen(aResult))
            IF ALen(aInfo) > 0
                ACopy(aResult, aInfo)
            ENDIF
        ELSE
            ASize(aInfo, 0)
        ENDIF
        RETURN


    METHOD BackgroundWorker(o AS OBJECT) AS VOID STRICT
        LOCAL aResults := List<IRdd>{} AS List<IRdd>
        TRY
            VAR oDataReader := SELF:_oNetCommand:ExecuteReader()
            SELF:_WriteOutParameters()
            LOCAL cursorNo := 0 AS LONG
            BEGIN LOCK SELF
                SELF:_oLastDataReader := oDataReader
            END LOCK
            DO WHILE TRUE
                VAR oSchema := oDataReader:GetSchemaTable()
                VAR cursorName := SELF:CursorName
                IF cursorNo != 0
                    cursorName += cursorNo:ToString()
                ENDIF
                LOCAL oRDD AS IRdd
                oRDD := SELF:CreateFile(oSchema, cursorName)
                SELF:_CopyFromReaderToRDD(oDataReader, oRDD)
                aResults:Add(oRDD)
                IF SELF:_aSyncState == AsyncState.Cancelling
                    EXIT
                ENDIF
                IF ! oDataReader:NextResult()
                    EXIT
                ENDIF
            ENDDO
            BEGIN LOCK SELF
                SELF:_aResult    := aResults
                IF SELF:_aSyncState == AsyncState.Executing
                    SELF:_aSyncState := AsyncState.Ready
                ELSEIF SELF:_aSyncState == AsyncState.Cancelling
                    SELF:_aSyncState := AsyncState.Cancelled
                ENDIF
            END LOCK
        CATCH e AS Exception
            SELF:_lastException := e
            RuntimeState.LastRddError := Error{e}
            BEGIN LOCK SELF
                SELF:_lastException := e
                SELF:_CloseReader()
                SELF:_aSyncState := AsyncState.Exception
                IF SELF:_oThread:ThreadState == System.Threading.ThreadState.Running
                    SELF:_oThread:Abort()
                ENDIF
                SELF:_oThread := NULL
            END LOCK
        END TRY
         RETURN

    METHOD ThreadComplete() AS VOID STRICT
        SELF:_aQueryResult := {}
        IF SELF:_aResult == NULL
            SELF:_aSyncState := AsyncState.Idle
            RETURN
        ENDIF
        VAR aRDDs := SELF:_aResult
        SELF:_aResult := NULL
        FOREACH oRdd AS IRdd IN aRDDs
            VAR name := oRdd:Alias
            VAR count := oRdd:RecCount
            AAdd(SELF:_aQueryResult, {name, count})
            VAR nArea := RuntimeState.Workareas.FindAlias(name)
            IF nArea > 0
                RuntimeState.Workareas.CloseArea(nArea)
            ELSE
                nArea := RuntimeState.Workareas.FindEmptyArea(TRUE)
            ENDIF
            RuntimeState.Workareas.SetArea(nArea, oRdd)
        NEXT
        IF SELF:_aSyncState == AsyncState.Ready
            SELF:_aSyncState := AsyncState.Idle
        ENDIF
        
    PRIVATE METHOD CopyToCursorAsync() AS VOID
        IF SELF:_returnsRows
            _aQueryResult := {}
            IF SELF:_aSyncState == AsyncState.Idle
                BEGIN LOCK SELF
                    SELF:_CloseReader()
                    _oThread := Thread{BackgroundWorker}
                    SELF:_aSyncState := AsyncState.Executing
                    _oThread:Start(SELF)
                END LOCK
            ENDIF
        ELSE
            VAR result := SELF:_oNetCommand:ExecuteNonQuery()
            SELF:_WriteOutParameters()
            _aQueryResult := {{"", result}}
        ENDIF

    
    PRIVATE METHOD CopyToCursor() AS VOID
        TRY
            SELF:_CloseReader()
            SELF:_lastException := NULL
            IF SELF:_ReturnsRows(SELF:_oNetCommand:CommandText)
                VAR oDataReader := SELF:_oNetCommand:ExecuteReader()
                SELF:_WriteOutParameters()
                CopyToCursor(oDataReader, 0)
            ELSE
                VAR result := SELF:_oNetCommand:ExecuteNonQuery()
                SELF:_WriteOutParameters()
                _aQueryResult := {{"", result}}
            ENDIF
        CATCH e AS Exception
            SELF:_lastException := e
            RuntimeState.LastRddError := Error{e}
            _aQueryResult := {{"", -1}}
        END TRY

    PRIVATE METHOD CopyToCursor(oDataReader AS DbDataReader, cursorNo AS LONG) AS VOID
        LOCAL result   := {} AS ARRAY
        _oLastDataReader := oDataReader
        DO WHILE TRUE
            VAR cursorName := SELF:CursorName
            IF cursorNo != 0
                cursorName += cursorNo:ToString()
            ENDIF
    		oDataReader := SELF:Connection:Factory:AfterOpen(oDataReader)
            VAR oSchema := oDataReader:GetSchemaTable()
            SELF:_CreateWorkarea(oSchema, oDataReader, cursorName)
            AAdd(result, {cursorName, RecCount()})
            cursorNo += 1
            IF ! SELF:BatchMode
                _oLastDataReader := oDataReader
                _nextCursorNo    := cursorNo
                EXIT
            ENDIF
            IF ! oDataReader:NextResult()
                EXIT
            ENDIF
        ENDDO
        _aQueryResult := result
        RETURN 




    METHOD MoreResults(cursorName AS STRING, aInfo AS ARRAY) AS LONG
        IF !String.IsNullOrEmpty(cursorName)
            SELF:CursorName := cursorName
            SELF:_nextCursorNo := 0
        ENDIF
        IF _oLastDataReader != NULL .AND. _oLastDataReader:NextResult()
            CopyToCursor(_oLastDataReader, _nextCursorNo)
            RETURN SELF:_SaveResult(aInfo)
        ENDIF
        ASize(aInfo,1)
        aInfo[1] := {{0, -1}}
        RETURN 0
    

    PRIVATE METHOD GetNumRestrictions(cCollectionName AS STRING) AS LONG
        LOCAL nRestrictions := 0 AS LONG
        LOCAL oTable := SELF:Connection:NetConnection:GetSchema("MetadataCollections",NULL) AS DataTable
        FOREACH oRow AS DataRow IN oTable:Rows
			IF String.Compare( (STRING)oRow:Item["CollectionName"], cCollectionName, StringComparison.OrdinalIgnoreCase) == 0
				nRestrictions := (INT) oRow:Item["NumberOfRestrictions"] 
				EXIT
			ENDIF
        NEXT        
        RETURN nRestrictions  


    #region MetaData
   METHOD GetTables(cType AS STRING, cCursorName AS STRING) AS LOGIC
        SELF:CursorName := cCursorName
        VAR types := cType:Split(",":ToCharArray(),StringSplitOptions.RemoveEmptyEntries)
        VAR list  := List<STRING>{}
        FOREACH VAR type IN types
            VAR sType := type
            IF sType:StartsWith("'")
                sType := sType:Replace("'","")
            ELSEIF sType:StartsWith(e"\"")
                sType := sType:Replace(e"\"","")
            ENDIF
            list:Add(sType)
        NEXT
        
        LOCAL filter AS STRING[]
        LOCAL oTables AS List<DataTable>
        LOCAL oTable AS DataTable
        LOCAL nRestrictions AS LONG
        oTables := List<DataTable>{}
        
        nRestrictions := GetNumRestrictions("Tables") 
        filter := STRING[]{nRestrictions}
        oTable := SELF:Connection:NetConnection:GetSchema("Tables", filter)
        oTables:Add(oTable)
        
        nRestrictions := GetNumRestrictions("Views")
        filter := STRING[]{nRestrictions}
        oTable := SELF:Connection:NetConnection:GetSchema("Views", filter)
        oTables:Add(oTable)
        
        LOCAL aStruct AS ARRAY
        aStruct := ArrayNew(5)
        aStruct[1] := {"TABLE_CAT","C:0",128,0}
        aStruct[2] := {"TABLE_SCHEM","C:0",128,0}
        aStruct[3] := {"TABLE_NAME","C:0",128,0}
        aStruct[4] := {"TABLE_TYPE","C:0",32,0}
        aStruct[5] := {"REMARKS","C:0",254,0}
        VAR cTemp := System.IO.Path.GetTempFileName()
        DbCreate(cTemp, aStruct, "DBFVFP")
        VAR nArea := _SelectString(cCursorName)
        IF nArea != 0
            DbCloseArea()
        ENDIF
        VoDbUseArea(TRUE, "DBFVFPSQL",cTemp,cCursorName,FALSE,FALSE)
        LOCAL oRDD AS IRdd
        oRDD := (IRdd) DbInfo(DbInfo.DBI_RDD_OBJECT)
        FOREACH VAR oT IN oTables
            FOREACH oRow AS DataRow IN oT:Rows
                VAR oValues := SELF:Connection:Factory:GetMetaDataTableValues(oRow)
                VAR match := TRUE
                IF list:Count > 0
                    match := list:Contains((STRING) oValues[4])
                ENDIF
                IF match
                    oRDD:Append(TRUE)
                    FOR VAR nFld := 1 TO 5
                        oRDD:PutValue(nFld, oValues[nFld])
                    NEXT
                ENDIF
            NEXT
       NEXT
       RETURN TRUE

   METHOD GetColumnsFox(cTableName AS STRING, cCursorName AS STRING) AS LOGIC
        SELF:_oNetCommand:CommandText := "Select * from "+cTableName+" where 1 = 0"
        SELF:_CloseReader()
        VAR oDataReader := SELF:_oNetCommand:ExecuteReader()
        VAR oSchema := oDataReader:GetSchemaTable()
        VAR aFieldNames := List<STRING>{}
        VAR aStruct := ArrayNew(4)
        aStruct[1] := {"FIELD_NAME", "C", 14,0}
        aStruct[2] := {"FIELD_TYPE", "C", 1,0}
        aStruct[3] := {"FIELD_LEN", "N", 3,0}
        aStruct[4] := {"FIELD_DEC", "N", 3,0}
        VAR cTemp := System.IO.Path.GetTempFileName()
        DbCreate(cTemp, aStruct, "DBFVFP")
        VoDbUseArea(TRUE, "DBFVFPSQL",cTemp,cCursorName,FALSE,FALSE)
        LOCAL oRDD AS IRdd
        oRDD := (IRdd) DbInfo(DbInfo.DBI_RDD_OBJECT)
        FOREACH schemaRow AS DataRow IN oSchema:Rows
            VAR fieldInfo    := FromSchema(schemaRow, aFieldNames)
            oRDD:Append(TRUE)
            oRDD:PutValue(1, fieldInfo:Name)
            oRDD:PutValue(2, Left(fieldInfo:FieldTypeStr,1))
            oRDD:PutValue(3, fieldInfo:Length)
            oRDD:PutValue(4, fieldInfo:Decimals)
            oRDD:GoCold()
        NEXT
        RETURN TRUE        
        
   METHOD GetColumnsNative(cTableName AS STRING, cCursorName AS STRING) AS LOGIC
        LOCAL filter AS STRING[]
        LOCAL oTable AS DataTable
        LOCAL nRestrictions AS LONG
        nRestrictions := GetNumRestrictions("Columns") 
        filter := STRING[]{nRestrictions}
        filter[3] := cTableName
        SELF:_CloseReader()
        oTable := SELF:Connection:NetConnection:GetSchema("Columns", filter)
        LOCAL aStruct AS ARRAY
        aStruct := ArrayNew(19)
        aStruct[1] := {"TABLE_CAT","C:0",128,0}
        aStruct[2] := {"TABLE_SCHE","C:0",128,0}
        aStruct[3] := {"TABLE_NAME","C",128,0}
        aStruct[4] := {"COLUMN_NAM","C",128,0}
        aStruct[5] := {"DATA_TYPE","I",4,0}
        aStruct[6] := {"TYPE_NAME","C",128,0}
        aStruct[7] := {"COLUMN_SIZ","I:0",4,0}
        aStruct[8] := {"BUFFER_LEN","I:0",4,0}
        aStruct[9] := {"DECIMAL_DI","I:0",4,0}
        aStruct[10] := {"NUM_PREC_R","I:0",4,0}
        aStruct[11] := {"NULLABLE","I",4,0}
        aStruct[12] := {"REMARKS","C:0",254,0}
        aStruct[13] := {"COLUMN_DEF","M:0",4,0}
        aStruct[14] := {"SQL_DATA_T","I",4,0}
        aStruct[15] := {"SQL_DATETI","I:0",4,0}
        aStruct[16] := {"CHAR_OCTET","I:0",4,0}
        aStruct[17] := {"ORDINAL_PO","I",4,0}
        aStruct[18] := {"IS_NULLABL","C:0",254,0}
        aStruct[19] := {"SS_DATA_TY","I:0",4,0}
        VAR cTemp := System.IO.Path.GetTempFileName()
        DbCreate(cTemp, aStruct, "DBFVFP")
        VAR nArea := _SelectString(cCursorName)
        IF nArea != 0
            DbCloseArea()
        ENDIF
        VoDbUseArea(TRUE, "DBFVFPSQL",cTemp,cCursorName,FALSE,FALSE)
        LOCAL oRDD AS IRdd
        oRDD := (IRdd) DbInfo(DbInfo.DBI_RDD_OBJECT)
        FOREACH oRow AS DataRow IN oTable:Rows
            oRDD:Append(TRUE)
            VAR oValues := SELF:Connection:Factory:GetMetaDataColumnValues(oRow)
            FOR VAR i := 1 TO oValues:Length
                oRDD:PutValue(i, oValues[i])
            NEXT
         NEXT
       RETURN TRUE        

   METHOD GetColumns(cTableName AS STRING, cType AS STRING, cCursorName AS STRING) AS LOGIC
        SELF:CursorName := cCursorName
        IF String.IsNullOrEmpty(cTableName) .OR. String.IsNullOrEmpty(cType)
            RETURN FALSE
        ENDIF
        SWITCH cType:ToUpper()
        CASE "FOXPRO"
            RETURN GetColumnsFox(cTableName, cCursorName)
        CASE "NATIVE"
            RETURN GetColumnsNative(cTableName, cCursorName)
        END SWITCH
        RETURN FALSE
    #endregion


   
    STATIC METHOD FromSchema(schemaRow AS DataRow, aFieldNames AS IList<STRING>) AS XSharp.RDD.DbColumnInfo
		LOCAL nLen, nDec AS LONG
		LOCAL cType AS STRING
		LOCAL oType	AS System.Type
		LOCAL TC AS TypeCode
        LOCAL result AS DbColumnInfo
        LOCAL columnName AS STRING
        columnName   := schemaRow["ColumnName"]:ToString( )
        oType   := (Type) schemaRow["DataType"]
		TC      := Type.GetTypeCode(oType)
		nDec    := 0
		SWITCH TC
		CASE TypeCode.String 
			cType   := "C"
			nLen    := (Int32)schemaRow["ColumnSize"]
			// Automatically Convert Long Strings to Memos
			IF nLen  > 255 .OR. nLen < 0
				nLen    := 4
				cType   := "M"
            ENDIF
            result := DbColumnInfo{columnName,cType,nLen ,0 }
			
		CASE TypeCode.Boolean
			result := DbColumnInfo{columnName,"L",1 ,0 }

        CASE TypeCode.Decimal
		    result := DbColumnInfo{columnName,"Y",16 ,4 }
            result:NumericScale     := 16
            result:NumericPrecision := 4
            
		CASE TypeCode.Double
        CASE TypeCode.Single
			nDec := 1
			nLen := 10
            VAR nScale := Convert.ToInt32(schemaRow:Item["NumericScale"])
            VAR nPrec  := Convert.ToInt32(schemaRow:Item["NumericPrecision"])
            IF nScale == 255
                nScale := 2
            ENDIF
			nDec := nScale
			nLen := nPrec
            IF nLen == 0
				// I have seen a case where nDec == 31 and nLen == 0
				// Fix this to something usefull
				nDec := 2
				nLen := 10
			ELSEIF nDec == 127
				//IF nLen == 38 .AND. oProviderType = ProviderType.Oracle // Standardvalue for calculated fields in oracle-queries, cutting decimals leads to wrong results in that case
				//	nDec := 10
				//ELSE
					nDec := 0 // Overflow abfangen
				//ENDIF
            ENDIF
            result := DbColumnInfo{columnName,"N",nLen ,nDec }
            result:NumericScale     := nScale
            result:NumericPrecision := nPrec
			
		CASE TypeCode.Int32		// -2147483647 - 2147483648 (2^31)
            result := DbColumnInfo{columnName,"I",4 ,0}
            IF (LOGIC)schemaRow["IsAutoIncrement"]
                result:Flags |= DBFFieldFlags.AutoIncrement
            ENDIF
               
		CASE TypeCode.Int64		// - 9223372036854775807 - 9223372036854775808 (2^63)
            result := DbColumnInfo{columnName,"N",21 ,0}
                
		CASE TypeCode.Int16	// -32767 - 32768 (2^15)
            result := DbColumnInfo{columnName,"N",6 ,0}
                
		CASE TypeCode.Byte
            result := DbColumnInfo{columnName,"N",4 ,0}
                
		CASE TypeCode.SByte	// 0 - 255 	(2^8)
            result := DbColumnInfo{columnName,"N",3 ,0}
                
		CASE TypeCode.UInt16	// 0 - 65535 (2^16)
            result := DbColumnInfo{columnName,"N",5 ,0}
                
		CASE TypeCode.UInt32		// 0 - 4294836225 (2^32)
            result := DbColumnInfo{columnName,"N",10 ,0}
                
		CASE TypeCode.UInt64	// 0 - 18445618199572250625 (2^64)
			nLen := 20
            result := DbColumnInfo{columnName,"N",nLen ,0}
			
        CASE TypeCode.DateTime
            VAR nPrec  := Convert.ToInt32(schemaRow:Item["NumericPrecision"])
			nLen 	:= 8
            IF nPrec <= 10
			    cType   := "D"
            ELSE
			    cType   := "T"
            ENDIF
            result  := DbColumnInfo{columnName,cType,nLen ,0}
			
        CASE TypeCode.Object
            IF oType == typeof(BYTE[])
			    cType   := "P"
			    nLen 	:= 4
                result  := DbColumnInfo{columnName,cType,nLen ,0}
           
            ELSE
			    LOCAL lIsDate := FALSE AS LOGIC
			    LOCAL oMems AS MethodInfo[]
			    LOCAL lFound := FALSE AS LOGIC
			    // check to see if the datatype has a dbType
			    oMems := oType:GetMethods(BindingFlags.Public|BindingFlags.Static)
			    FOREACH oMem AS MethodInfo IN oMems
				    IF oMem:ReturnType == TypeOf(System.DateTime)  .AND. String.Compare(oMem:Name, "op_Explicit", StringComparison.OrdinalIgnoreCase) == 0
					    lIsDate := TRUE
					    lFound  := TRUE
					    EXIT
				    ENDIF
			    NEXT
			    IF ! lFound
				    LOCAL cTypeName AS STRING
				    cTypeName := oType:Name:ToUpperInvariant()
				    lIsDate     := cTypeName:Contains("DATE") 
			    ENDIF
			    IF lIsDate
				    cType   := "D"
				    nLen 	:= 8
			    ELSE
				    cType 	:= "C"
				    nLen 	:= 10
			    ENDIF
                result := DbColumnInfo{columnName,cType,nLen ,0}
            ENDIF			
		OTHERWISE
			cType := "C"
			nLen 	:= (Int32)schemaRow["ColumnSize"]
			IF nLen <= 0
				cType := "M"
                nLen  := 4
			ENDIF
            result := DbColumnInfo{columnName,cType,nLen ,0}
			
        END SWITCH
        IF (LOGIC)schemaRow["AllowDBNull"]
            result:Flags |= DBFFieldFlags.Nullable
        ENDIF
        VAR cFldName        := SQLSupport.CleanupColumnName(columnName)
        result:ColumnName   := columnName
        result:Name         := MakeFieldNameUnique(cFldName, aFieldNames)
        result:Alias        := result:Name
        result:DotNetType   := oType
		RETURN result

   STATIC METHOD MakeFieldNameUnique(cName AS STRING, aFldNames AS IList<STRING> ) AS STRING
        LOCAL dwPos, dwFld AS LONG
        LOCAL cNewname		 AS STRING
        IF String.IsNullOrEmpty(cName)
            dwFld := 0
            dwPos := 1
            DO WHILE dwPos >= 0
                ++dwFld
                cName := "FLD"+dwFld:ToString():PadLeft(3,c'0')
                dwPos := aFldNames:IndexOf(cName:ToUpper())
            ENDDO
        ELSE
            // remove column prefixes
            dwPos := cName:IndexOf(".")+1 
            IF dwPos > 0
                cName := cName:Substring(dwPos)
            ENDIF
            // remove embedded spaces
            cName 	:= cName:Replace(" ", "_"):ToUpper()
            cNewname := Left(cName,10)
            dwFld 	:= 1
            DO WHILE aFldNames:IndexOf(cNewname) >= 0
                ++dwFld
                VAR tmp := dwFld:ToString()
                VAR len := tmp:Length
                IF cName:Length + len <= 10
                    cNewname := cName + tmp
                ELSE
                    cNewname := cName:Substring(0, 10 - tmp:Length)+tmp
                ENDIF
            ENDDO
            cName 	:= cNewname
        ENDIF
        aFldNames:Add(cName)
    RETURN cName            

    METHOD ParseCommand(cCommand AS STRING, cParamChar AS CHAR, lIncludeParameterNameInQuery AS LOGIC) AS STRING
        LOCAL statements := List<STRING>{} AS List<STRING>
        LOCAL aParams    := List<SQLParameter>{} AS List<SQLParameter>
        LOCAL inString := FALSE AS LOGIC
        LOCAL inParam  := FALSE AS LOGIC
        LOCAL sb       := StringBuilder{cCommand:Length} AS StringBuilder
        LOCAL sbParam  := StringBuilder{cCommand:Length} AS StringBuilder
        LOCAL lParamByRef := FALSE AS LOGIC
        FOREACH ch AS CHAR IN cCommand
            SWITCH ch
            CASE c'\''
                inString := ! inString
                sb:Append(ch)
            CASE c';'
                IF (! inString)
                    IF inParam
                        IF lIncludeParameterNameInQuery
                            sb:Append(sbParam:ToString())
                        ENDIF
                        aParams:Add( SQLParameter{sbParam:ToString(), lParamByRef})
                        inParam := FALSE
                        sbParam:Clear()
                    ENDIF
                    statements:Add(sb:ToString())
                    statements:Clear()
                    LOOP
                ENDIF
                sb:Append(ch)
            CASE c'@'
                IF inParam
                    IF sbParam:Length == 0
                        lParamByRef := TRUE
                    ENDIF
                ELSE
                    sb:Append(ch)    
                ENDIF
                
            CASE c'?'
            CASE c':'
                inParam := TRUE
                lParamByRef := FALSE
                sb:Append(cParamChar)
            OTHERWISE
                IF inParam
                    IF Char.IsLetterOrDigit(ch) .OR. ch == c'_'
                        sbParam:Append(ch)
                    ELSE
                        IF lIncludeParameterNameInQuery
                            sb:Append(sbParam:ToString())
                        ENDIF
                        aParams:Add( SQLParameter{sbParam:ToString(), lParamByRef})
                        inParam     := FALSE
                        sbParam:Clear()
                    ENDIF
                ELSE
                    sb:Append(ch)
                ENDIF
            END SWITCH
        NEXT
        IF inParam
            IF lIncludeParameterNameInQuery
                sb:Append(sbParam:ToString())
            ENDIF
            aParams:Add( SQLParameter{sbParam:ToString(), lParamByRef})
        ENDIF
        statements:Add(sb:ToString())
        SELF:_aParams := aParams
        VAR result := ""
        VAR returns := FALSE
        FOREACH VAR stmt IN statements
            IF result:Length > 0
                result += ';'
            ENDIF
            result += stmt
            returns := SELF:_ReturnsRows(stmt)
        NEXT
        _returnsRows := returns
        RETURN result
        



END CLASS

INTERNAL DELEGATE SqlGetData() AS OBJECT[]

INTERNAL ENUM XSharp.VFP.AsyncState
    MEMBER Idle         := 0
    MEMBER Executing    := 1
    MEMBER Ready        := 2
    MEMBER Cancelling   := 3
    MEMBER Cancelled    := 4
    MEMBER Exception    := 5
END ENUM
    
INTERNAL CLASS XSharp.VFP.SQLParameter
    PROPERTY Name   AS STRING AUTO GET PRIVATE SET
    PROPERTY ByRef  AS LOGIC  AUTO GET PRIVATE SET
    PROPERTY DbParameter AS DbParameter AUTO GET SET
    CONSTRUCTOR(cName AS STRING, lByRef AS LOGIC)
        SELF:Name  := cName
        SELF:ByRef := lByRef
END CLASS

