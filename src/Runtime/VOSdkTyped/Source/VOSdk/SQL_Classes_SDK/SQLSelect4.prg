//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//




USING System.Data.Common
USING System.Data
USING System.Linq
using System.Collections.Generic

PARTIAL CLASS SQLSelect INHERIT DataServer


/// <include file="Sql.xml" path="doc/SQLSelect.ctor/*" />
    CONSTRUCTOR( cSQLSelect, oSQLConnection ) CLIPPER


        SUPER()


        SELF:oStmt 	    := SQLStatement{ cSQLSelect, oSQLConnection }
        SELF:oConn 	    := SELF:oStmt:Connection
        SELF:oNetConn   := oConn:ConnectionHandle

        SELF:lReadOnly          := FALSE
        SELF:lBatchUpdates      := FALSE
        SELF:lFetchFlag      	:= FALSE
        SELF:lCsrOpenFlag    	:= FALSE
        SELF:lBof            	:= TRUE
        SELF:lEof            	:= FALSE
        SELF:nNotifyCount    	:= 0
        SELF:lAppendFlag     	:= FALSE
        SELF:aIndexCol       	:= List<INT>{}
        SELF:aLastArgs          := {}
        SELF:nRowCount       	:= -1
        SELF:lTimeStampAsDate   := TRUE
        SELF:lNullAsBlank		:= TRUE
        IF !IsNil(cSQLSelect)
            SELF:__FindTableName()
        ENDIF


        RETURN


/// <include file="Sql.xml" path="doc/SQLSelect.NoIVarGet/*" />
    METHOD NoIVarGet(symVar  AS USUAL) AS USUAL
        RETURN SELF:FieldGet(symVar)


/// <include file="Sql.xml" path="doc/SQLSelect.NoIVarPut/*" />
    METHOD NoIVarPut(symVar AS USUAL, uValue  AS USUAL) AS USUAL
        RETURN SELF:FieldPut(symVar, uValue)


/// <include file="Sql.xml" path="doc/SQLSelect.Notify/*" />
    METHOD Notify( kNotification AS LONG, uDescription := NIL AS USUAL) AS USUAL
        LOCAL lRetValue AS LOGIC
        LOCAL uRetValue	AS USUAL
        LOCAL nClient	AS DWORD
        LOCAL laClients AS ARRAY
        //
        // If notification is enabled
        // Call DataServer:Notify. This will call all the clients
        //
        lRetValue := TRUE
        IF nSuspendNot == 0 .and. aClients:Length > 0
            // Make a clone, to prevent problems when a client unregisters itself
            laClients := ACloneShallow(aClients)
            IF kNotification <= NOTIFYCOMPLETION
                FOREACH oClient AS OBJECT IN AClone(laClients)
                    Send(oClient,#Notify, kNotification, uDescription )
                NEXT


            ELSEIF kNotification = NOTIFYINTENTTOMOVE
                nClient := 0
                DO WHILE lRetValue .AND. nClient < ALen(laClients)
                    nClient++


                    uRetValue := Send(laClients[nClient],#Notify,kNotification)
                    IF IsLogic(uRetValue)
                        lRetValue := uRetValue
                    ENDIF
                ENDDO
                RETURN lRetValue
            ELSEIF kNotification <= NOTIFYFILECHANGE
                FOREACH oClient AS OBJECT IN AClone(aClients)
                    Send(oClient,#Notify, kNotification, uDescription )
                NEXT
            ELSEIF kNotification = NOTIFYRELATIONCHANGE
                FOREACH oClient AS OBJECT IN AClone(aClients)
                    Send(oClient,#Notify, NOTIFYFILECHANGE )
                NEXT
            ELSEIF kNotification != NOTIFYCLEARRELATION
                FOREACH oClient AS OBJECT IN AClone(aClients)
                    Send(oClient,#Notify, kNotification, uDescription )
                NEXT
            ENDIF
        ENDIF
        RETURN lRetValue


/// <include file="Sql.xml" path="doc/SQLSelect.NumResultCols/*" />
    METHOD NumResultCols() AS LONG
        //  Make sure we have a stmt
        IF ( oStmt:StatementHandle = NULL )
            SELF:__AllocStmt()
        ENDIF
        //RvdH 050413 Centralize opening of cursor
        IF ! SELF:__ForceOpen()
            RETURN -1
        ENDIF
        RETURN nNumCols


/// <include file="Sql.xml" path="doc/SQLSelect.PreExecute/*" />
    METHOD PreExecute( cSQLString AS STRING) AS STRING
        RETURN cSQLString


/// <include file="Sql.xml" path="doc/SQLSelect.Prepare/*" />
    METHOD Prepare() AS LOGIC
        RETURN oStmt:Prepare()


/// <include file="Sql.xml" path="doc/SQLSelect.Refresh/*" />
    METHOD Refresh() AS LOGIC STRICT
        RETURN SELF:Update( FALSE )


/// <include file="Sql.xml" path="doc/SQLSelect.RejectChanges/*" />
    METHOD RejectChanges() AS LOGIC
        oTable:RejectChanges()
        SELF:lChanges := FALSE
        RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.Requery/*" />
    METHOD Requery() AS LOGIC
        LOCAL lOk		AS LOGIC
        // ask the clients if they want to move
        SELF:lErrorFlag := FALSE
        TRY
            lOk := SELF:Execute(SELF:aLastArgs)
            IF lOk
                SELF:Notify(NOTIFYFILECHANGE)
            ENDIF
        CATCH e AS Exception
            SELF:Error(Error{e}, #Requery)
            lOk := FALSE
        END TRY
        RETURN lOk


/// <include file="Sql.xml" path="doc/SQLSelect.ReReadRow/*" />
    METHOD ReReadRow() AS LOGIC
        IF oCurrentRow != NULL
            oCurrentRow:RejectChanges()
        ENDIF
        RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.ResetCursor/*" />
    METHOD ResetCursor( nUpdateType ) AS LOGIC  CLIPPER
        SELF:oTable:RejectChanges()
        SELF:GoTo( SELF:RecNo )
        RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.ResetNotification/*" />
    METHOD ResetNotification() AS LONG STRICT
        --nNotifyCount
        RETURN nNotifyCount


/// <include file="Sql.xml" path="doc/SQLSelect.RLock/*" />
    METHOD RLock(nRecord AS LONG ) AS LOGIC
        RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.RLockVerify/*" />
    METHOD RLockVerify() AS LOGIC STRICT
        RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.Rollback/*" />
    METHOD Rollback() AS LOGIC STRICT
        RETURN SELF:oStmt:Connection:Rollback()


/// <include file="Sql.xml" path="doc/SQLSelect.SetColumnAttributes/*" />
    METHOD SetColumnAttributes(uFieldPos AS USUAL, oColAttributes AS SQLColumnAttributes) AS LOGIC
        LOCAL nIndex    AS DWORD
        LOCAL lOk		 AS LOGIC
        nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
        IF ( nIndex = 0 .OR. nIndex > nNumCols )
            oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetColumnAttributes )
            SELF:Error( oStmt:ErrInfo ,#SetColumnAttributes)
            lOk := FALSE
        ELSE
            SELF:aSQLColumns[nIndex] 		:= oColAttributes
            lOk := TRUE
        ENDIF
        RETURN lOk


/// <include file="Sql.xml" path="doc/SQLSelect.SetDataField/*" />
    METHOD SetDataField( nFieldPosition AS DWORD, oDataField AS DataField) AS LOGIC
        // override method in DataServer class because it does not allow
        // to change size/decimals for numeric columns
        LOCAL lRetVal := FALSE AS LOGIC
        LOCAL wFieldPosition := nFieldPosition AS DWORD
        LOCAL oField := oDataField AS DataField
        LOCAL oDF   AS DataField
        LOCAL oFS   AS FieldSpec
        IF SELF:aDataFields = NULL_ARRAY
            BREAK DbError{ SELF, #SetDataField, EG_SEQUENCE,  __CavoStr( __CAVOSTR_SQLCLASS__NODATAFIELDSEXIST )  }


        ELSEIF IsNil(nFieldPosition) .OR. !IsNumeric(nFieldPosition) .OR. wFieldPosition<1 .OR. wFieldPosition>ALen( SELF:aDataFields )
            BREAK DbError{ SELF, #SetDataField, EG_ARG, __CavoStr( __CAVOSTR_SQLCLASS__BADFIELDPOSITION ),nFieldPosition, "nFieldPosition" }


        ELSEIF IsNil(oDataField) .OR. !IsInstanceOfUsual(oDataField,#DataField)
            BREAK DbError{ SELF, #SetDataField, EG_ARG,__CavoStr( __CAVOSTR_SQLCLASS__BADFIELDPOSITION ), nFieldPosition, "nFieldPosition" }


        ELSE
            oDF := SELF:aDataFields[ wFieldPosition ]
            IF oField:Name == oDF:Name
                // Check for ValType only
                // No checks for Length/Decimals
                // Also allow to override Memo/Char
                oFS := oDF:FieldSpec
                IF oFS:ValType == oFS:ValType
                    SELF:aDataFields[ wFieldPosition ] := oField
                    lRetVal := TRUE
                ELSEIF oFS:ValType $ "CM" .AND. oFS:ValType $ "CM"
                    SELF:aDataFields[ wFieldPosition ] := oField
                    lRetVal := TRUE
                ENDIF
            ENDIF
        ENDIF
        RETURN lRetVal


/// <include file="Sql.xml" path="doc/SQLSelect.SetPos/*" />
    [Obsolete];
    METHOD SetPos( nPos, nOption, nLock ) AS LOGIC CLIPPER
        RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.SetPrimaryKey/*" />
    METHOD SetPrimaryKey( uFieldPos ) AS LOGIC CLIPPER
        LOCAL nIndex    AS DWORD
        LOCAL lRet      := FALSE AS LOGIC


        nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
        IF nIndex = 0
            // clear the index array
            aIndexCol := List<Int>{}
            lRet := TRUE
        ELSE
            IF nIndex > nNumCols
                oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetPrimaryKey )
            ELSE
                aIndexCol:Add( (INT) nIndex )
                var cols := List<DataColumn>{}
                foreach var num in aIndexCol
                    cols:Add( SELF:oTable:Columns[num-1])
                next
                self:oTable:PrimaryKey := cols:ToArray()

                oStmt:ErrInfo:ErrorFlag := FALSE
                lRet := TRUE
            ENDIF
        ENDIF
        RETURN lRet




/// <include file="Sql.xml" path="doc/SQLSelect.SetStatementOption/*" />
    [Obsolete];
    METHOD SetStatementOption( fOption, uValue ) AS LOGIC  CLIPPER
        //RETURN oStmt:SetStatementOption( fOption, uValue, TRUE )
        RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.SetTimeStamp/*" />
    METHOD SetTimeStamp( uFieldPos AS USUAL, cTimeStamp AS STRING) AS VOID
        LOCAL nIndex AS DWORD
        LOCAL oErr		AS Error
        LOCAL oColumn   AS SQLColumn
        nIndex := SELF:__GetColIndex( uFieldPos, TRUE )


        IF ( nIndex = 0 .OR. nIndex > nNumCols )
            oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetTimeStamp )
            oErr := oStmt:ErrInfo
            oErr:ArgNum := 1
            oErr:Args := {uFieldPos, cTimeStamp}
            SELF:Error( oErr , #SetTimeStamp)
            RETURN
        ENDIF


        oColumn := aSQLColumns[nIndex]
        IF  oColumn:Type != typeof(DateTime)
            oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetTimeStamp  )
            oErr := oStmt:ErrInfo
            oErr:ArgNum := 1
            oErr:Args := {uFieldPos, cTimeStamp}
            SELF:Error( oErr , #SetTimeStamp)
            RETURN
        ENDIF


        IF ! IsString(cTimeStamp )
            oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #SetTimeStamp  )
            oErr := oStmt:ErrInfo
            oErr:ArgNum := 2
            oErr:Args := {uFieldPos, cTimeStamp}
            SELF:Error( oErr , #SetTimeStamp)
            RETURN
        ENDIF


        LOCAL oDT AS DateTime
        oDT := DateTime.Parse(cTimeStamp)
        SELF:FieldPut(nIndex, oDT)
        RETURN




/// <include file="Sql.xml" path="doc/SQLSelect.Skip/*" />
    METHOD Skip( nSkip := 1 AS LONG) AS LOGIC
        LOCAL lOk AS LOGIC
        LOCAL nRow AS LONG
        SELF:lErrorFlag := FALSE
        TRY
            lOk := SELF:__PrepareForRecordMovement()
            IF lOk
                nRow := SELF:nCurrentRow
                IF IsNil(nSkip)
                    nSkip := 1
                ENDIF
                IF nSkip < 0 .AND. SELF:lBof
                    // already on BOF
                    // don't move !


                    lOk := FALSE
                ELSEIF nSkip < 0 .AND. SELF:lEof
                    // On EOF, handle that
                    SELF:nCurrentRow := SELF:nRowCount -1
                    lOk := TRUE
                ELSEIF nSkip > 0 .AND. SELF:lEof
                    // already on EOF, so don't move
                    lOk := FALSE
                ELSE
                    lOk := TRUE
                ENDIF
                IF lOk
                    // Not yet on a boundary
                    SELF:nCurrentRow := nRow + nSkip
                    SELF:__CheckEOF()
                    // Notify clients of record change
                    SELF:Notify( NOTIFYRECORDCHANGE ,nSkip)
                ENDIF
            ENDIF
        CATCH e AS Exception
            SELF:Error(Error{e}, #Skip)
            lOk := FALSE
        END TRY
        RETURN lOk


/// <include file="Sql.xml" path="doc/SQLSelect.SuspendNotification/*" />
    METHOD SuspendNotification() AS LONG STRICT
        SELF:nNotifyCount := SELF:nNotifyCount + 1
        RETURN nNotifyCount


/// <include file="Sql.xml" path="doc/SQLSelect.UnLock/*" />
    METHOD UnLock( nRecordNumber := 0 AS LONG) AS LOGIC
        RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.Update/*" />
    METHOD Update(lUpdateFlag) AS LOGIC CLIPPER
        LOCAL lRet          AS LOGIC
        IF ! IsLogic(lUpdateFlag)
            lUpdateFlag := TRUE
        ENDIF
		IF lUpdateFlag
            lRet := SELF:__GoCold(TRUE)
        ELSE
            lRet := SELF:RejectChanges()
        ENDIF
        SELF:lChanges := FALSE
        RETURN lRet




    #region Dummy Methods
/// <include file="Sql.xml" path="doc/SQLSelect.UpdateCursor/*" />
    [Obsolete];
    METHOD UpdateCursor() AS LOGIC
        RETURN TRUE
/// <include file="Sql.xml" path="doc/SQLSelect.UpdateKey/*" />
    [Obsolete];
    METHOD UpdateKey() AS LOGIC
        RETURN TRUE
/// <include file="Sql.xml" path="doc/SQLSelect.UpdateVal/*" />
    [Obsolete];
    METHOD UpdateVal() AS LOGIC
        RETURN TRUE
    #endregion


END CLASS
