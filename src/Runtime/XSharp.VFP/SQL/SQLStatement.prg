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
        SELF:Asynchronous       := SQLSupport.GetDefault<LOGIC>(SQLProperty.Asynchronous)
        SELF:BatchMode          := SQLSupport.GetDefault<LOGIC>(SQLProperty.BatchMode)
        SELF:DisconnectRollback := SQLSupport.GetDefault<LOGIC>(SQLProperty.DisconnectRollback)
        SELF:DispWarnings       := SQLSupport.GetDefault<LOGIC>(SQLProperty.DispWarnings)
        SELF:IdleTimeout        := SQLSupport.GetDefault<LONG>(SQLProperty.IdleTimeout)
        SELF:PacketSize         := SQLSupport.GetDefault<LONG>(SQLProperty.PacketSize)
        SELF:Prepared           := FALSE
        SELF:TransactionMode    := SQLSupport.GetDefault<LONG>(SQLProperty.Transactions)
        SELF:WaitTime           := SQLSupport.GetDefault<LONG>(SQLProperty.WaitTime)
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
            VAR fieldInfo     := SQLHelpers.GetColumnInfoFromSchemaRow(schemaRow, aFieldNames)
            fieldInfo:Ordinal := nFields
            aStruct[nFields]  := {fieldInfo:Name, fieldInfo:FieldTypeStr, fieldInfo:Length, fieldInfo:Decimals, fieldInfo:ColumnName, fieldInfo:Flags, fieldInfo}
            nFields++
        NEXT
        nFields := aStruct:Count
        VAR cTemp := System.IO.Path.GetTempFileName()
        SELF:CloseArea(cCursorName)
        // We do not use DBFVFPSQL to create the file because that driver deletes the file when it is closed.

        DbCreate(cTemp, aStruct, "DBFVFP", TRUE, cCursorName)
        SELF:CloseArea(cCursorName)
        VoDbUseArea(TRUE, "DBFVFPSQL",cTemp,cCursorName,FALSE,FALSE)
        LOCAL oRDD AS IRdd
        oRDD := (IRdd) DbInfo(DbInfo.DBI_RDD_OBJECT)

        FOR VAR nI := 1 TO ALen(aStruct)
            LOCAL fieldInfo AS DbColumnInfo
            fieldInfo := aStruct[nI, 7]
            oRDD:FieldInfo(nI, DBS_COLUMNINFO, fieldInfo)
        NEXT
        RETURN oRDD




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
        SELF:CopyToInfo(_aQueryResult, aInfo)
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
        SELF:_hasOutParams := FALSE
        FOREACH oParam AS SQLParameter IN SELF:_aParams
            LOCAL oDbParam AS DbParameter
            oDbParam := SELF:Connection:Factory:CreateParameter()
            oParam:DbParameter := oDbParam
            IF oParam:ByRef
                oDbParam:Direction := ParameterDirection.Output //.InputOutput
                SELF:_hasOutParams := TRUE
            ELSE
                oDbParam:Direction := ParameterDirection.Input
            ENDIF
            LOCAL oValue        AS OBJECT
            if oParam:Name[0]=c'('
               oValue:= Evaluate(oParam:Name)
            else
               oValue:= MemVarGet(oParam:Name)
            endif
            IF oValue IS USUAL
                VAR uType := typeof(USUAL)
                VAR mi    := uType:GetMethod("ToObject")
                oValue    := mi:Invoke(NULL,<OBJECT>{oValue})
                oDbParam:Value:= oValue
            elseif oValue is Int32
                oDbParam:Value:= oValue
            elseif oValue is Int64
                oDbParam:Value:= oValue
            elseif oValue IS DATE
                oDbParam:DbType:= DbType.Date
                oDbParam:Value := oValue
            elseif oValue is DateTime
                oDbParam:DbType:= DbType.DateTime
                if oParam:ByRef
                    oDbParam:Value:= oValue
                else
                    var dtValue:= (DateTime)oValue
                    oDbParam:Value:= System.DateTime{dtValue:Year, dtValue:Month, dtValue:Day, dtValue:Hour, dtValue:Minute, dtValue:Second}
                endif
            elseif oValue is Decimal
                oDbParam:DbType:=DbType.String
                // Ugly Hack : Some ODBC don't like decimal value, let's give them a String
                // We could go Double, but may loose some accuracy (when related to currency)
                oDbParam:Value:= oValue:ToString()
            elseif oValue is Currency
                oDbParam:DbType:=DbType.Currency
                oDbParam:Value:= oValue
            elseif oValue is float
                oDbParam:DbType:= DbType.Double
                oDbParam:Value:= oValue
            elseif oValue IS STRING
                if oParam:ByRef
                    oDbParam:Size := 256
                    oDbParam:DbType:= DbType.String
                else
                    oDbParam:Size:= oValue.ToString():Length
                    oDbParam:DbType:= DbType.AnsiString
                endif
                oDbParam:Value:= oValue
            elseif oValue=null
                oDbParam:Value:= DBNull.Value
            else
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
                    MemVarPut(oParam.Name,oValue)
                    //IF ! SQLReflection.SetPropertyValue(SELF:ParamsObject,oParam:Name, oValue)
                    //    RETURN FALSE
                    //ENDIF
                ENDIF
            NEXT
        ENDIF
        RETURN TRUE

    METHOD Execute(cCommand AS STRING, cCursorName AS STRING, aInfo AS ARRAY) AS LONG
        try
            SELF:BeginTransaction()
            cCommand := SELF:ParseCommand(cCommand, SELF:Connection:Factory:ParameterPrefix, SELF:Connection:Factory:ParameterNameInQuery)
            SELF:_oNetCommand:CommandText := cCommand
            if  self:_aParams:Count>0
               IF ! SELF:_CreateParameters()
                  RETURN -1
               ENDIF
            endif
            SELF:CursorName := cCursorName
            if self._hasOutParams
                self:_oNetCommand:ExecuteNonQuery()
                SELF:_WriteOutParameters()
                return 1
            else
               IF SELF:Asynchronous
                  RETURN SELF:_AsyncExecuteResult(aInfo)
               ELSE
                   SELF:CopyToCursor()
               ENDIF
            endif
            RETURN SELF:_SaveResult(aInfo)
        catch e as Exception
            if self:_lastException== null
               self:_lastException:= Error{e}
            endif
            return -1
        end try



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
                VAR cursorName := SELF:CursorName
                IF cursorNo != 0
                    cursorName += cursorNo:ToString()
                ENDIF
                VAR oRDD := SELF:CreateArea(oDataReader, cursorName)
                aResults:Add(oRDD)
                IF SELF:_aSyncState == AsyncState.Cancelling
                    EXIT
                ENDIF
                //IF ! oDataReader:NextResult()
                    EXIT
                //ENDIF
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

    PRIVATE METHOD OnRowChanged(sender as OBJECT, e as DataRowChangeEventArgs) AS VOID
        IF SELF:_aSyncState == AsyncState.Cancelling
            SELF:_oLastDataReader:Close()   // this aborts the load
        ENDIF
        RETURN

    PRIVATE METHOD CreateArea(oDataReader AS DbDataReader, cCursorName as STRING) AS IRdd
        VAR oSchema := oDataReader:GetSchemaTable()
        VAR oRDD := SELF:CreateFile(oSchema, cCursorName)
        local oDataTable as DbDataTable
        TRY
            oDataTable := DbDataTable{}
            oDataTable:RowChanged += OnRowChanged
            oDataTable:Load(oDataReader)
            oDataTable:RowChanged -= OnRowChanged
            IF ! SQLReflection.SetPropertyValue(oRDD,"DataTable",oDataTable)
                RuntimeState.LastRddError := Error{"Internal error: Could not locate the DataTable property of the SQL RDD"}
                oRDD := NULL
            ENDIF
        CATCH e as Exception
            IF SELF:_aSyncState == AsyncState.Cancelling
                RuntimeState.LastRddError := Exception{"SQLExec Cancelled"}
            ELSE
                RuntimeState.LastRddError := e
            ENDIF
            oRDD := NULL
        END TRY
        RETURN oRDD


    PRIVATE METHOD CopyToCursor() AS VOID
        TRY
            SELF:_CloseReader()
            SELF:_lastException := NULL
            IF SELF:_returnsRows
               begin using VAR oDataReader:= SELF:_oNetCommand:ExecuteReader()
                SELF:_WriteOutParameters()
                IF (oDataReader== null) or (oDataReader!=null and  oDataReader:FieldCount=0)
                   _aQueryResult := {{"", 0}}
                else
                   SELF:CopyToCursor(oDataReader, 0)
                endif
                end using
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
            VAR oRDD := SELF:CreateArea(oDataReader, cursorName)
            AAdd(result, {oRDD:Alias, oRDD:RecCount})
            cursorNo += 1
            IF ! SELF:BatchMode
                _oLastDataReader := oDataReader
                _nextCursorNo    := cursorNo
                EXIT
            ENDIF
            IF oDataReader:IsClosed .or. ! oDataReader:HasRows
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
        IF _oLastDataReader != NULL .and. ! _oLastDataReader:IsClosed .AND. _oLastDataReader:HasRows
            SELF:CopyToCursor(_oLastDataReader, _nextCursorNo)
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

    PRIVATE METHOD CloseArea(cArea as STRING) AS VOID
        var nArea := VoDb.SymSelect(cArea)
        IF nArea > 0
            DbCloseArea()
        ENDIF

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

        nRestrictions := SELF:GetNumRestrictions("Tables")
        filter := STRING[]{nRestrictions}
        oTable := SELF:Connection:NetConnection:GetSchema("Tables", filter)
        oTables:Add(oTable)

        nRestrictions := SELF:GetNumRestrictions("Views")
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
        SELF:CloseArea(cCursorName)
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
            VAR fieldInfo    := SQLHelpers.GetColumnInfoFromSchemaRow(schemaRow, aFieldNames)
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
        nRestrictions := SELF:GetNumRestrictions("Columns")
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
        SELF:CloseArea(cCursorName)
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
            RETURN SELF:GetColumnsFox(cTableName, cCursorName)
        CASE "NATIVE"
            RETURN SELF:GetColumnsNative(cTableName, cCursorName)
        END SWITCH
        RETURN FALSE
    #endregion




/// Foxpro detect the parameter with the quotation character '?'
/// if the character followed is '@', It is an output parameter
/// if the character followed is '(', It is an VFP expression closed between  parenthesis
/// If the parameter are variables, they need to be private or publics and initialized before
METHOD ParseCommand(cCommand AS STRING, cParamChar AS CHAR, lIncludeParameterNameInQuery AS LOGIC) AS STRING
	LOCAL statements := List<STRING>{} AS List<STRING>
	LOCAL aParams    := List<SQLParameter>{} AS List<SQLParameter>
	LOCAL inString := FALSE AS LOGIC
	LOCAL inParam  := FALSE AS LOGIC
	LOCAL sb       := StringBuilder{cCommand:Length} AS StringBuilder
	LOCAL sbParam  := StringBuilder{cCommand:Length} AS StringBuilder
	LOCAL lParamByRef := FALSE AS LOGIC
	FOREACH ch AS CHAR IN cCommand
		IF !inString .AND. ch == cParamChar
			inParam := TRUE
			lParamByRef := FALSE
			sb:Append(cParamChar)
			LOOP
		ENDIF
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
				sb:Clear()
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
					sb:Append(ch)
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
	VAR result := StringBuilder{}
	VAR returns := FALSE
	FOREACH VAR stmt IN statements
		IF result:Length > 0
			result:Append( ';' )
		ENDIF
        result:Append( stmt )
		returns := XSharp.SQLHelpers.ReturnsRows(stmt)
	NEXT
	//
    SELF:_returnsRows := returns
    RETURN result:ToString()


END CLASS

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

