//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//



USING System.Data.Common
using System.Data

PARTIAL CLASS SqlSelect INHERIT DataServer

	METHOD AddDateVal( uFieldPos, dDate )
		//  Adds time string to Timestamp field
		LOCAL cVal  AS STRING
		LOCAL cDate AS STRING
		LOCAL cTime AS STRING
		IF !IsDate( dDate )
			dDate := Today()
		ENDIF

		cVal := SELF:GetTimestamp( uFieldPos )
		IF IsNil( cVal )
			RETURN NIL
		ENDIF

		cDate := DToCSQL( dDate )
		IF SLen( cVal ) = 0
			cTime := Time()
		ELSE
			cTime := SubStr2( cVal, 12 )
		ENDIF
		cVal := cDate + " " + cTime
		RETURN SELF:SetTimeStamp( uFieldPos, cVal )

	METHOD AddTimeString( uFieldPos, cTime )
		//  Adds time string to Timestamp field
		LOCAL cVal  AS STRING
		LOCAL cDate AS STRING

		IF !IsString( cTime )
			cTime := Time()
		ENDIF

		IF At2( ".", cTime ) == 0
			cTime := cTime + ".000000"
		ENDIF
		cVal := SELF:GetTimestamp( uFieldPos )
		IF IsNil( cVal )
			RETURN NIL
		ENDIF
		IF SLen( cVal ) = 0
			cDate := DToCSQL( Today() )
		ELSE
			cDate := SubStr3( cVal, 1, 11 )
		ENDIF
		cVal := cDate + " " + cTime
		RETURN SELF:SetTimeStamp( uFieldPos, cVal )




	METHOD Append() 
		LOCAL oRow AS DataRow
		LOCAL lOk AS LOGIC
		IF SELF:__PrepareForRecordMovement()
			SELF:lChanges := TRUE
			oRow := oTable:NewRow()
			oTable:Rows:Add(oRow)
			oCurrentRow := oRow
			oCurrentRow:AcceptChanges()
			nCurrentRow := oTable:Rows:Count-1
			nRowCount	:= oTable:Rows:Count-1
			SELF:lEof	:= FALSE
			SELF:lBof	:= FALSE
			SELF:lAppendFlag := TRUE
			lOk := TRUE
		ENDIF
		RETURN lOk

	METHOD AppendRow( lForce )
		RETURN SELF:Update(lForce)


	METHOD Error( oError ) 
	//  Method for handling error conditions raised during database processing
	//
	//  The standard Error handling method passes the problem to its clients
	//  if there are any, in their standard Exception handing method.
	//
	//  If there is no client who wants to deal with the problem, the user
	//  defined error handler gets called.
	//
	//  Note: if an error comes in while one is being handled, the Error
	//  method immediately breaks without any fancy stuff.

	STATIC LOCAL    lErrorProcessingSemaphor    AS LOGIC
	IF lErrorProcessingSemaphor
			IF ErrorBlock() != NULL
				Eval( ErrorBlock(), oError )
			ENDIF
	ELSE
		lErrorProcessingSemaphor := TRUE
		IF IsArray( aClients ) .AND. ALen( aClients ) != 0 .AND. IsMethod( aClients[1], #Error )
			Send(aClients[1],#Error, oError )
		ELSE
			lErrorProcessingSemaphor := FALSE
			IF ErrorBlock() != NULL
				Eval( ErrorBlock(), oError )
			ENDIF
		ENDIF
		lErrorProcessingSemaphor := FALSE
	ENDIF
	RETURN NIL

/*
	METHOD Error( uError, symMethod )
		STATIC LOCAL    lErrorProcessingSemaphor    AS LOGIC
		LOCAL           oError                      AS Error
		oError := uError
		IF lErrorProcessingSemaphor
			THROW oError
		ELSE
			lErrorProcessingSemaphor := TRUE

			IF ! IsInstanceOfUsual( oError, #Error )
				oError := CreateError(EG_ERRORBUILD, "Wrong Error Object ")
				oError:FuncSym          := __ENTITY__
			ENDIF
			IF IsNil(symMethod) .OR. !IsSymbol( symMethod )
				oError:FuncSym := #Unknown
			ELSE
				oError:FuncSym := symMethod
			ENDIF
			oError:MethodSelf := SELF
			SELF:lErrorFlag := TRUE
			SELF:oStmt:ErrInfo := oError

			IF IsArray( SELF:aClients ) .AND. ALen(SELF:aClients) != 0 .AND. IsMethod( aClients[1], #Error )
				Send(aClients[1],#Error, oError )
			ELSE
				lErrorProcessingSemaphor := FALSE
				THROW oError
			ENDIF

			lErrorProcessingSemaphor := FALSE

		ENDIF

		RETURN SELF
*/

	[Obsolete];
	METHOD BindColumn( i )
		RETURN TRUE
 
	METHOD Close() 
		LOCAL lOk AS LOGIC
		SELF:lErrorFlag := FALSE
		TRY
			lOk := SELF:__GoCold()
			// Notify the clients that we closed down
			SELF:__Notify( NOTIFYCLOSE )
			IF lOk
				SELF:__FreeStmt( SQL_DROP )
				SELF:oTable				:= NULL_OBJECT
				SELF:oSchema			:= NULL_OBJECT
				SELF:oCurrentRow 		:= NULL_OBJECT
				SELF:lCsrOpenFlag 	    := FALSE
				SELF:lBof        	    := TRUE
				SELF:lEof         	    := FALSE
			ENDIF


		CATCH e AS Exception
			SELF:Error(Error{e},#Close)
			lOk := FALSE
		END TRY
		RETURN lOk


	METHOD Column( siCol )
		LOCAL nIndex    AS DWORD
		LOCAL oColumn   AS OBJECT
		nIndex := SELF:__GetColIndex( siCol, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADCOL ), #Column )
		ELSE
			oStmt:ErrInfo:ErrorFlag := FALSE
			oColumn := aSQLColumns[nIndex]
		ENDIF
		RETURN oColumn

	METHOD ColumnAttributes( siCol )
		LOCAL nIndex            AS DWORD
		LOCAL oSQLColAtt        AS SQLColumnAttributes
		LOCAL oSqlCol			AS SQLColumn

		nIndex := SELF:__GetColIndex( siCol, TRUE )

		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADCOL ), #ColumnAttributes )
			RETURN NULL_OBJECT
		ENDIF

		IF SELF:aColumnAttributes:Length >= nIndex
			IF IsObject( SELF:aColumnAttributes[nIndex] )
				RETURN SELF:aColumnAttributes[nIndex]
			ENDIF
		ENDIF

		oSqlCol		:= aSQLColumns[nIndex] 
		oSQLColAtt 	:= SQLColumnAttributes{ (HyperLabel) oSqlCol:HyperLabel , ;
											(FieldSpec) oSqlCol:FieldSpec,  ;
											oSqlCol:Type,   ;
											oSqlCol:Scale,      ;
											oSqlCol:Nullable,   ;
											oSqlCol:Index,      ;
											oSqlCol:ColName,    ;
											oSqlCol:AliasName }

		oSQLColAtt:DisplaySize		:= oSqlCol:DisplaySize
		LOCAL oDataColumn AS DataColumn
		oDataColumn := SELF:oTable:Columns[(INT) nIndex]
		IF oDataColumn:MaxLength == -1
			LOCAL oFs AS FieldSpec
			LOCAL oDf AS DataField
			oDf := SELF:aDataFields[nIndex]
			oFs := oDf:FieldSpec
			oSQLColAtt:Length 			:= (LONG) oFs:Length
		ELSE
			oSQLColAtt:Length 			:= oDataColumn:MaxLength 
		ENDIF
		oSQLColAtt:Unsigned 		:= InList(oDataColumn:DataType, typeof(WORD), typeof(DWORD), typeof(UINT64))
		oSQLColAtt:Money 			:= oDataColumn:DataType == Typeof(System.Decimal)
		oSQLColAtt:Updatable 		:= ! oDataColumn:ReadOnly
		oSQLColAtt:AutoIncrement 	:= oDataColumn:AutoIncrement

		oStmt:ErrInfo:ErrorFlag 	:= FALSE
		
		SELF:aColumnAttributes[nIndex] := oSQLColAtt
		RETURN oSQLColAtt

	METHOD Commit() 
		SELF:Update(TRUE)
		RETURN SELF:oConn:Commit()

	METHOD DataField( uFieldPos ) 
		LOCAL nIndex        AS DWORD
		LOCAL oRet          AS OBJECT

		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #DataField )
		ELSE
			oStmt:ErrInfo:ErrorFlag := FALSE

			oRet := aDataFields[nIndex]
		ENDIF
		RETURN oRet

	METHOD Delete() 
		LOCAL lOk AS LOGIC
		IF SELF:__PrepareForRecordMovement()
			IF oCurrentRow != NULL
				SELF:lChanges := TRUE
				oTable:Rows:Remove(oCurrentRow)
				nRowCount	:= oTable:Rows:Count-1
				IF nCurrentRow >= nROwCount
					nCurrentRow := nRowCount-1
				ENDIF
				lOk := TRUE
			ENDIF
		ENDIF
		RETURN lOk

METHOD DirectSkip( nSkip )
	RETURN SELF:Skip(nSkip)

METHOD Execute( uParam )
	LOCAL nCount        AS DWORD
	LOCAL lRet          AS LOGIC
	LOCAL i             AS DWORD
	LOCAL aArgs         AS ARRAY

	IF oStmt:StatementHandle == NULL_OBJECT
		SELF:__AllocStmt()
	ENDIF

	oStmt:SQLString := SELF:PreExecute( oStmt:SQLString )
	nCount := (DWORD) PCount()
	IF nCount != 0
		IF nCount = 1 .AND. UsualType( uParam ) = ARRAY
			aArgs  := uParam
		ELSE
			aArgs := ArrayCreate( nCount )
			FOR i:=1 TO nCount
				aArgs[i] := _GETMPARAM( i )
			NEXT
		ENDIF
	ENDIF
	SELF:aLastArgs := aArgs
	lRet := oStmt:Execute( aArgs)

	IF lRet
		SELF:__Open(oStmt:oDataTable, oStmt:oSchema)
	ELSE
		SELF:lBof         := TRUE
		SELF:lEof         := TRUE
		SELF:nRowCount    := -1
	ENDIF
	
	RETURN lRet


	METHOD ExtendedFetch( nFetchType, nRow ) 
		LOCAL lResult AS LOGIC
		EnForceType(REF nFetchType, LONG)
		DEFAULT ( REF nRow, 0)
		SWITCH (LONG) nFetchType 
		CASE SQL_FETCH_LAST
			lResult := SELF:GoBottom()
		CASE SQL_FETCH_FIRST
			lResult := SELF:GoTop()
		CASE SQL_FETCH_NEXT
			lResult := SELF:Skip(1)
		CASE SQL_FETCH_PREV
			lResult := SELF:Skip(-1)
		CASE SQL_FETCH_RELATIVE
			lResult := SELF:Skip(nRow)
		CASE SQL_FETCH_ABSOLUTE
			lResult := SELF:Goto(nRow)
		OTHERWISE
			lResult := FALSE
		END SWITCH
		SELF:lFetchFlag := TRUE
		RETURN lResult


	METHOD Fetch( )
		IF SELF:lFetchFlag
			SELF:Skip(1)
		ELSE
			SELF:lFetchFlag := TRUE
		ENDIF
		RETURN !SELF:Eof

	METHOD FIELDGET( uFieldID  ) 
		LOCAL uValue   AS USUAL
		LOCAL cType    AS STRING
		LOCAL wField   AS DWORD
		LOCAL oFs      AS FieldSpec
		LOCAL oCol     AS SqlColumn
		wField	:= SELF:__GetColIndex(uFieldID, TRUE)
		IF wField > 0 .AND. wField <= SELF:nNumCols
			uValue  := SELF:GetData(uFieldID)
			IF (uValue == NIL .OR. uValue == NULL) .AND. SELF:lNullAsBlank 
				oCol    := SELF:aSqlColumns[wField]
				IF oCol != NULL_OBJECT
					oFs		:= oCol:FieldSpec
					cType	:= oFs:ValType
					SWITCH cType
					CASE "C"
						uValue := STRING.Empty
					CASE "N"
						uValue := 0
					CASE "L"
						uValue := FALSE
					CASE "D"
						IF lTimeStampAsDate
							uValue := NULL_DATE
						ELSE
							uValue := DateTime{0}
						ENDIF
					CASE "M"
						uValue := STRING.Empty
					CASE "O"
						uValue := NULL_OBJECT
					END SWITCH
				ENDIF
			ENDIF
		ENDIF
		RETURN uValue

	METHOD FieldGetFormatted( uFieldPos ) 
		LOCAL nIndex    AS DWORD
		LOCAL xRet      AS USUAL
		LOCAL oFs      AS FieldSpec
		LOCAL oCol     AS SqlColumn
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldGetFormatted )
			SELF:Error( oStmt:ErrInfo )
		ELSE
			xRet := SELF:FIELDGET( nIndex )
			oStmt:ErrInfo:ErrorFlag := FALSE
			oCol := aSqlColumns[nIndex]
			oFs  := oCol:FieldSpec
			xRet := oFs:Transform( xRet )
		ENDIF

		RETURN xRet
END CLASS
