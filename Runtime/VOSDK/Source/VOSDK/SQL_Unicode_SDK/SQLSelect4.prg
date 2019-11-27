//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Data.Common
USING System.Data
using System.Collections.Generic
PARTIAL CLASS SqlSelect INHERIT DataServer

	CONSTRUCTOR( cSQLSelect, oSQLConnection )

		SUPER()

		SELF:oStmt 	    := SQLStatement{ cSQLSelect, oSQLConnection }
		SELF:oConn 	    := SELF:oStmt:Connection
		SELF:oNetConn   := oConn:ConnectionHandle

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
		IF cSQLSelect != NIL
			SELF:__FindTableName()
		ENDIF

		RETURN

	METHOD NoIVarGet(symVar)
		RETURN SELF:FIELDGET(symVar)

	METHOD NoIVarPut(symVar, uValue)
		RETURN SELF:FIELDPUT(symVar, uValue)

	METHOD Notify( uNotification, uDescription )
		LOCAL lRetValue AS LOGIC
		LOCAL uRetValue	AS USUAL
		LOCAL nClient	AS DWORD
		LOCAL laClients AS ARRAY
        LOCAL kNotification := uNotification AS LONG
		//
		// If notification is enabled
		// Call DataServer:Notify. This will call all the clients
		//
		lRetValue := TRUE
		IF nSuspendNot == 0 .and. aClients:Length > 0
			// Make a clone, to prevent problems when a client unregisters itself
			laClients := ACloneShallow(aClients)
			IF kNotification <= NOTIFYCOMPLETION
				FOREACH oClient AS OBJECT IN Aclone(laClients)
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
				FOREACH oClient AS OBJECT IN Aclone(aClients)
					Send(oClient,#Notify, kNotification, uDescription )
				NEXT
			ELSEIF kNotification = NOTIFYRELATIONCHANGE
				FOREACH oClient AS OBJECT IN Aclone(aClients)
					Send(oClient,#Notify, NOTIFYFILECHANGE )
				NEXT
			ELSEIF kNotification != NOTIFYCLEARRELATION
				FOREACH oClient AS OBJECT IN Aclone(aClients)
					Send(oClient,#Notify, kNotification, uDescription )
				NEXT
			ENDIF
		ENDIF
		RETURN lRetValue

	METHOD NumResultCols()
		//  Make sure we have a stmt
		IF ( oStmt:StatementHandle = NULL )
			SELF:__AllocStmt()
		ENDIF
		//RvdH 050413 Centralize opening of cursor
		IF ! SELF:__ForceOpen()
			RETURN -1
		ENDIF
		RETURN nNumCols

	METHOD PreExecute( cSQLString )
		RETURN cSQLString

	METHOD Prepare()
		RETURN oStmt:Prepare()

	METHOD Refresh() CLIPPER
		RETURN SELF:Update( FALSE )

	METHOD RejectChanges()
		oTable:RejectChanges()
		SELF:lChanges := FALSE
		RETURN TRUE

	METHOD Requery()
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

	METHOD ReReadRow()
		IF oCurrentRow != NULL
			oCurrentRow:RejectChanges()
		ENDIF
		RETURN TRUE

	METHOD ResetCursor( nUpdateType )
		SELF:oTable:RejectChanges()
		SELF:GoTo( SELF:RecNo )
		RETURN TRUE

	METHOD ResetNotification() 
		--nNotifyCount
		RETURN nNotifyCount

	METHOD RLOCK(nRecno) 
		RETURN TRUE

	METHOD RLockVerify() 
		RETURN TRUE

	METHOD Rollback() 
		RETURN SELF:oStmt:Connection:RollBack()

	METHOD SetColumnAttributes(uFieldPos, oColAttributes)
		LOCAL nIndex    AS DWORD
		LOCAL lOk		 AS LOGIC
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF ( nIndex = 0 .OR. nIndex > nNumCols )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetColumnAttributes )
			SELF:Error( oStmt:ErrInfo )
			lOk := FALSE
		ELSEIF IsInstanceOfUsual(oColAttributes, #SQLColumnAttributes)
			SELF:aSQLColumns[nIndex] 		:= oColAttributes
			lOk := TRUE
		ELSE
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__INVALIDCOLATT ), #SetColumnAttributes )
			SELF:Error( oStmt:ErrInfo )
			lOk := FALSE
		ENDIF
		RETURN lOk

	METHOD SetDataField( nFieldPosition , oDataField ) 
		// override method in DataServer class because it does not allow
		// to change size/decimals for numeric columns
		LOCAL lRetVal AS LOGIC
		LOCAL wFieldPosition := nFieldPosition AS WORD
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

	[Obsolete];
	METHOD SetPos( nPos, nOption, nLock )
		RETURN TRUE



	METHOD SetPrimaryKey( uFieldPos )
		LOCAL nIndex    AS DWORD
		LOCAL lRet      AS LOGIC

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
				oStmt:ErrInfo:ErrorFlag := FALSE
				lRet := TRUE
			ENDIF
		ENDIF
		RETURN lRet


	[Obsolete];
	METHOD SetStatementOption( fOption, uValue )
		//RETURN oStmt:SetStatementOption( fOption, uValue, TRUE )
		RETURN TRUE

	METHOD SetTimeStamp( uFieldPos, cTimestamp )
		LOCAL nIndex AS DWORD
		LOCAL oErr		AS Error
		LOCAL oColumn   AS SqlColumn
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )

		IF ( nIndex = 0 .OR. nIndex > nNumCols )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetTimeStamp )
			oErr := oStmt:ErrInfo
			oErr:ArgNum := 1
			oErr:Args := {uFieldPos, cTimestamp}
			SELF:Error( oErr )
			RETURN NIL
		ENDIF

		oColumn := aSQLColumns[nIndex]
		IF  oColumn:Type != typeof(DateTime)
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #SetTimeStamp  )
			oErr := oStmt:ErrInfo
			oErr:ArgNum := 1
			oErr:Args := {uFieldPos, cTimestamp}
			SELF:Error( oErr )
			RETURN NIL
		ENDIF

		IF ! IsString(cTimeStamp )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #SetTimeStamp  )
			oErr := oStmt:ErrInfo
			oErr:ArgNum := 2
			oErr:Args := {uFieldPos, cTimestamp}
			SELF:Error( oErr )
			RETURN NIL
		ENDIF

		LOCAL oDT AS DateTime
		oDT := DateTime.Parse(cTimeStamp)
		SELF:FieldPut(nIndex, oDT)
		RETURN cTimestamp


	METHOD Skip( nSkip ) AS USUAL CLIPPER
		LOCAL lOk AS LOGIC
		LOCAL nRow AS LONG
		SELF:lErrorFlag := FALSE
		TRY
			lOk := SELF:__PrepareForRecordMovement()
			IF lOk
				nRow := SELF:nCurrentRow
				IF nSkip == NIL
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
					SELF:__CheckEof()
					// Notify clients of record change
					SELF:Notify( NOTIFYRECORDCHANGE ,nSkip)
				ENDIF
			ENDIF
		CATCH e AS Exception
			SELF:Error(Error{e}, #Skip)
			lOk := FALSE
		END TRY
		RETURN lOk

	METHOD SuspendNotification() AS USUAL CLIPPER
		SELF:nNotifyCount := SELF:nNotifyCount + 1
		RETURN nNotifyCount

	METHOD UnLock( nRecordNumber ) 
		RETURN TRUE

	METHOD Update(lUpdateFlag) 
		LOCAL lRet          AS LOGIC
		IF ! IsLogic(lUpdateFlag)
			lUpdateFlag := TRUE
		ENDIF
		IF lUpdateFlag
			lRet := SELF:__GoCold()
		ELSE
			lRet := SELF:Rejectchanges()
		ENDIF
		SELF:lChanges := FALSE
		RETURN lRet


	#region Dummy Methods
	[Obsolete];
	METHOD UpdateCursor()
		RETURN TRUE
	[Obsolete];
	METHOD UpdateKey()
		RETURN TRUE
	[Obsolete];
	METHOD UpdateVal()
		RETURN TRUE
	#endregion

END CLASS
