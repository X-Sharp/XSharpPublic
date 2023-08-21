//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System.Data.Common
using System.Data


PARTIAL CLASS SQLSelect INHERIT DataServer


/// <include file="Sql.xml" path="doc/SQLSelect.AddDateVal/*" />
	METHOD AddDateVal( uFieldPos AS USUAL, dDate AS DATE) AS VOID
		//  Adds time string to Timestamp field
		LOCAL cVal  AS STRING
		LOCAL cDate AS STRING
		LOCAL cTime AS STRING
		IF !IsDate( dDate )
			dDate := Today()
		ENDIF


		cVal := SELF:GetTimeStamp( uFieldPos )
		IF IsNil( cVal )
			RETURN
		ENDIF


		cDate := DToCSQL( dDate )
		IF SLen( cVal ) = 0
			cTime := Time()
		ELSE
			cTime := SubStr2( cVal, 12 )
		ENDIF
		cVal := cDate + " " + cTime
		SELF:SetTimeStamp( uFieldPos, cVal )


/// <include file="Sql.xml" path="doc/SQLSelect.AddTimeString/*" />
	METHOD AddTimeString( uFieldPos AS USUAL, cTime AS STRING) AS VOID
		//  Adds time string to Timestamp field
		LOCAL cVal  AS STRING
		LOCAL cDate AS STRING


		IF !IsString( cTime )
			cTime := Time()
		ENDIF


		IF At2( ".", cTime ) == 0
			cTime := cTime + ".000000"
		ENDIF
		cVal := SELF:GetTimeStamp( uFieldPos )
		IF IsNil( cVal )
			RETURN
		ENDIF
		IF SLen( cVal ) = 0
			cDate := DToCSQL( Today() )
		ELSE
			cDate := SubStr3( cVal, 1, 11 )
		ENDIF
		cVal := cDate + " " + cTime
		SELF:SetTimeStamp( uFieldPos, cVal )




/// <include file="Sql.xml" path="doc/SQLSelect.Append/*" />
	METHOD Append(lReleaseLocks AS LOGIC)  AS LOGIC
		LOCAL oRow AS DataRow
        LOCAL lOk := FALSE AS LOGIC
        TRY
            self:__CheckReadOnly()
        CATCH e as Exception
            SELF:ShowSQLError(e:Message, #Append, e)
            return FALSE
        END TRY
		IF SELF:__PrepareForRecordMovement()
			SELF:lChanges := TRUE
			oRow := oTable:NewRow()
			oTable:Rows:Add(oRow)
			oCurrentRow := oRow
			nCurrentRow := oTable:Rows:Count-1
			nRowCount	:= oTable:Rows:Count-1
			SELF:lEof	:= FALSE
			SELF:lBof	:= FALSE
			SELF:lAppendFlag := TRUE
			lOk := TRUE
		ENDIF
		RETURN lOk


/// <include file="Sql.xml" path="doc/SQLSelect.AppendRow/*" />
	METHOD AppendRow( lForce ) AS LOGIC CLIPPER
        if self:lBatchUpdates
            return true
        endif
		RETURN SELF:Update(lForce)




/// <include file="Sql.xml" path="doc/SQLSelect.Error/*" />
	METHOD Error( oError as Error, symMethod as Symbol) AS VOID
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
	RETURN


/*
/// <include file="Sql.xml" path="doc/SQLSelect.Error/*" />
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


/// <include file="Sql.xml" path="doc/SQLSelect.BindColumn/*" />
	[Obsolete];
	METHOD BindColumn( i ) AS LOGIC CLIPPER
		RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.Close/*" />
	METHOD Close() AS LOGIC STRICT
		LOCAL lOk AS LOGIC
		SELF:lErrorFlag := FALSE
		TRY
			lOk := SELF:__GoCold(TRUE)
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




/// <include file="Sql.xml" path="doc/SQLSelect.Column/*" />
	METHOD Column( siCol AS USUAL ) AS SQLColumn
		LOCAL nIndex    AS DWORD
		LOCAL oColumn   := NULL AS SQLColumn
		nIndex := SELF:__GetColIndex( siCol, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADCOL ), #Column )
		ELSE
			oStmt:ErrInfo:ErrorFlag := FALSE
			oColumn := aSQLColumns[nIndex]
		ENDIF
		RETURN oColumn


/// <include file="Sql.xml" path="doc/SQLSelect.ColumnAttributes/*" />
	METHOD ColumnAttributes( siCol AS USUAL) AS SQLColumnAttributes
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


/// <include file="Sql.xml" path="doc/SQLSelect.Commit/*" />
	METHOD Commit() AS LOGIC STRICT
		SELF:Update(TRUE)
		RETURN SELF:oConn:Commit()


/// <include file="Sql.xml" path="doc/SQLSelect.DataField/*" />
	METHOD DataField( uFieldPos AS USUAL) AS DataField
		LOCAL nIndex        AS DWORD
		LOCAL oRet         := NULL  AS DataField


		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #DataField )
		ELSE
			oStmt:ErrInfo:ErrorFlag := FALSE


			oRet := aDataFields[nIndex]
		ENDIF
		RETURN oRet


/// <include file="Sql.xml" path="doc/SQLSelect.Delete/*" />
	METHOD Delete() AS LOGIC CLIPPER
		LOCAL lOk := FALSE AS LOGIC
        TRY
            SELF:__CheckReadOnly()
        CATCH e as Exception
            SELF:ShowSQLError(e:Message, #Delete, e)
            return FALSE
        END TRY

		IF SELF:__PrepareForRecordMovement()
			IF oCurrentRow != NULL
				SELF:lChanges := TRUE
				oCurrentRow:Delete()
				nRowCount	:= oTable:Rows:Count-1
				IF nCurrentRow >= nRowCount
					nCurrentRow := nRowCount-1
				ENDIF
				lOk := TRUE
			ENDIF
		ENDIF
		RETURN lOk


/// <include file="Sql.xml" path="doc/SQLSelect.DirectSkip/*" />
METHOD DirectSkip( nSkip ) AS LOGIC CLIPPER
	RETURN SELF:Skip(nSkip)


/// <include file="Sql.xml" path="doc/SQLSelect.Execute/*" />
METHOD Execute( uParam ) AS LOGIC CLIPPER
	LOCAL nCount        AS DWORD
	LOCAL lRet          AS LOGIC
	LOCAL i             AS DWORD
	LOCAL aArgs       := NULL_ARRAY   AS ARRAY


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




/// <include file="Sql.xml" path="doc/SQLSelect.ExtendedFetch/*" />
	METHOD ExtendedFetch( nFetchType, nRow ) AS LOGIC CLIPPER
		LOCAL lResult AS LOGIC
		EnforceType(REF nFetchType, LONG)
		@@Default ( REF nRow, 0)
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
			lResult := SELF:GoTo(nRow)
		OTHERWISE
			lResult := FALSE
		END SWITCH
		SELF:lFetchFlag := TRUE
		RETURN lResult




/// <include file="Sql.xml" path="doc/SQLSelect.Fetch/*" />
	METHOD Fetch( ) AS LOGIC
		IF SELF:lFetchFlag
			SELF:Skip(1)
		ELSE
			SELF:lFetchFlag := TRUE
		ENDIF
		RETURN !SELF:EoF


/// <include file="Sql.xml" path="doc/SQLSelect.FieldGet/*" />
	METHOD FieldGet( uFieldID AS USUAL ) AS USUAL
		LOCAL uValue   := NIL AS USUAL
		LOCAL cType    AS STRING
		LOCAL wField   AS DWORD
		LOCAL oFs      AS FieldSpec
		LOCAL oCol     AS SQLColumn
		wField	:= SELF:__GetColIndex(uFieldID, TRUE)
		IF wField > 0 .AND. wField <= SELF:nNumCols
			uValue  := SELF:GetData(uFieldID)
			IF IsNil(uValue)  .AND. SELF:lNullAsBlank
				oCol    := SELF:aSQLColumns[wField]
				IF oCol != NULL_OBJECT
					oFs		:= oCol:FieldSpec
					cType	:= oFs:ValType
					SWITCH cType
					CASE "C"
						uValue := String.Empty
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
						uValue := String.Empty
					CASE "O"
						uValue := NULL_OBJECT
					END SWITCH
				ENDIF
			ENDIF
		ENDIF
		RETURN uValue


/// <include file="Sql.xml" path="doc/SQLSelect.FieldGetFormatted/*" />
	METHOD FieldGetFormatted( uFieldPos AS USUAL ) AS USUAL
		LOCAL nIndex    AS DWORD
		LOCAL xRet     := NIL  AS USUAL
		LOCAL oFs      AS FieldSpec
		LOCAL oCol     AS SQLColumn
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldGetFormatted )
			SELF:Error( oStmt:ErrInfo , #FieldGetFormatted)
		ELSE
			xRet := SELF:FieldGet( nIndex )
			oStmt:ErrInfo:ErrorFlag := FALSE
			oCol := aSQLColumns[nIndex]
			oFs  := oCol:FieldSpec
			xRet := oFs:Transform( xRet )
		ENDIF


		RETURN xRet
END CLASS
