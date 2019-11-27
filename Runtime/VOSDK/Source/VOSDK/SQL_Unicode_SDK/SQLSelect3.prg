//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


USING System.Data.Common
USING System.Data

PARTIAL CLASS SqlSelect INHERIT DataServer

	METHOD FieldHyperLabel  ( uFieldPos ) 
		LOCAL nIndex    AS DWORD
		LOCAL oRet      AS OBJECT

		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF ( nIndex = 0 .OR. nIndex > nNumCols )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldHyperLabel )
		ELSE
			oStmt:ErrInfo:ErrorFlag := FALSE
			oRet := ((SqlColumn)aSQLColumns[nIndex]):HyperLabel
		ENDIF
		RETURN oRet 

	METHOD FieldInfo( kFieldInfoType, uFieldPos, uFieldVal )
		//
		//  Retrieves information about fields
		//  uFieldPos is numeric, symbol or string
		//  kFieldInfoType is one of a set of manifest constants:
		//  DBS_NAME    := 1
		//  DBS_TYPE    := 2
		//  DBS_LEN     := 3
		//  DBS_DEC     := 4
		//  DBS_STRUCT  := 5
		//  DBS_ALIAS   := 6
		//
		LOCAL nIndex    AS DWORD
		LOCAL xRet      AS USUAL
		LOCAL oColumn	AS SQLColumn
		LOCAL oFs       AS FieldSpec
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )

		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldInfo )
		ELSE
			oStmt:ErrInfo:ErrorFlag := FALSE
			oColumn := aSQLColumns[nIndex]
			oFs     := oColumn:FieldSpec
			SWITCH (LONG) kFieldInfoType
			CASE DBS_NAME
				xRet := oColumn:ColName

			CASE DBS_TYPE
				xRet := oFs:ValType

			CASE DBS_LEN
				xRet := oFs:Length

			CASE DBS_DEC
				xRet := oFs:Decimals

			CASE DBS_ALIAS
				IF IsSymbol( uFieldVal )
					oColumn:AliasName := AsString( uFieldVal )
				ELSEIF IsString( uFieldVal )
					oColumn:AliasName := uFieldVal
				ELSE
					IF !IsNil( uFieldVal )
						oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #FieldInfo )
					ENDIF
				ENDIF
				xRet := oColumn:AliasName

			OTHERWISE
				oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #FieldInfo )
			END SWITCH
		ENDIF
		RETURN xRet

	METHOD FieldName( uFieldID) 
		LOCAL nIndex    AS DWORD
		LOCAL cRet      AS STRING
		nIndex := SELF:__GetColIndex( uFieldID, TRUE )
		IF ( nIndex = 0 .OR. nIndex > nNumCols )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldName )
		ELSE
			LOCAL oColumn	AS SQLColumn
			oColumn := aSQLColumns[nIndex]
			cRet    := oColumn:ColName
			oStmt:ErrInfo:ErrorFlag := FALSE
		ENDIF
		RETURN cRet


	METHOD FieldPos( uFieldID ) 
		LOCAL nIndex    AS DWORD
		LOCAL nRet      AS DWORD

		nIndex := SELF:__GetColIndex( uFieldID, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			nRet := 0
		ELSE
			nRet := nIndex
			oStmt:ErrInfo:ErrorFlag := FALSE
		ENDIF
		RETURN (LONG)nRet

	METHOD FIELDPUT( uFieldID , uValue ) AS USUAL CLIPPER

		LOCAL wField AS INT
		LOCAL oError AS Error
		LOCAL oColumn AS DataColumn
		LOCAL oDf AS DataField
		 TRY
			IF ! SELF:EoF .and. oCurrentRow != NULL_OBJECT
				wField := (INT) SELF:__GetColIndex( uFieldID ,TRUE)
				IF wField <= 0 .OR. wField > SELF:nNumCols
					oError := SqlFunctions.CreateError(EG_ARG, "Bad field name/position")
					oError:FuncSym      := __ENTITY__
					oError:Severity     := ES_WARNING
					oError:ArgType      := DWORD
					oError:Arg          := AsString(wField)
					THROW oError
				ENDIF
				IF IsString(uValue)
					uValue 	:= Trim(uValue)
					IF Len(uValue) == 0
						oColumn := oTable:Columns[wField-1]
						IF oColumn:AllowDBNull
							uValue := NULL
						ELSE
							uValue := " "
						ENDIF
					ENDIF
				ENDIF
				IF IsDate(uValue)
					// Dates need special handling. Dotnet expects System.DateTime. MySql expects its own type
					LOCAL oDT AS DateTime
//					LOCAL oDColumn AS DataColumn
					oDT     := ((DATE) uValue):ToDateTime()
//					oDColumn := oTable:Columns[wField-1] 
//					IF oDColumn:DataType == typeof(MySql.Data.Types.MySqlDateTime)
//						LOCAL mysqlDate AS MySql.Data.Types.MySqlDateTime
//						mysqlDate := MySql.Data.Types.MySqlDateTime{oDT}
//						oCurrentRow:Item[wField-1]	:= mysqlDate
//					ELSE
						oCurrentRow:Item[wField-1]	:= oDT
//					ENDIF
				ELSE
					oCurrentRow:Item[wField-1]	:= uValue
				ENDIF
				SELF:lChanges := TRUE
				oDf := SELF:aDataFields[ wField ]
				SELF:Notify( NOTIFYFIELDCHANGE, oDf:NameSym )
			ENDIF
		CATCH e AS Exception
			SELF:Error(Error{e}, #FIELDPUT)
		END TRY
		RETURN uValue

	METHOD FieldSpec( uFieldPos ) 
		LOCAL nIndex AS DWORD
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF ( nIndex = 0 .OR. nIndex > SELF:FCount )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldSpec )
			RETURN NULL_OBJECT
		ENDIF
		oStmt:ErrInfo:ErrorFlag := FALSE
		RETURN ((DataField) aDataFields[nIndex]):FieldSpec


	METHOD FieldStatus( uFieldPos ) 
		LOCAL nIndex AS DWORD
		LOCAL oRet   AS OBJECT
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldStatus )
		ELSE
			LOCAL oColumn	AS SQLColumn
			oColumn := aSQLColumns[nIndex]
			oRet := oColumn:HyperLabel
			oStmt:ErrInfo:ErrorFlag := FALSE
		ENDIF
		RETURN oRet

	METHOD FieldSym( uFieldPos ) 
		LOCAL nIndex        AS DWORD
		LOCAL symRet        AS SYMBOL
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldSym )
		ELSE
			LOCAL oColumn	AS SQLColumn
			oColumn := aSQLColumns[nIndex]
			symRet := oColumn:NameSym
			oStmt:ErrInfo:ErrorFlag := FALSE
		ENDIF
		RETURN symRet

	METHOD FieldValidate( uFieldPos , uValue  ) 
		LOCAL nIndex    AS DWORD
		LOCAL lRet      AS LOGIC

		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF nIndex = 0 .OR. nIndex > nNumCols
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldValidate )
		ELSE
			oStmt:ErrInfo:ErrorFlag := FALSE
			LOCAL oDF AS DataField
			LOCAL oFs AS FieldSpec
			oDF := aDataFields[nIndex]
			oFs := oDF:FieldSpec
			lRet := oFs:PerformValidations( uValue )
			IF !lRet
				IF oFs:Status != NULL_OBJECT
					//  Get description from hyperlabel
					LOCAL oHL AS HyperLabel
					oHL := oFs:HyperLabel
					oStmt:__GenerateSQLError( oHL:Description ,  #FieldValidate )
				ELSE
					oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADVALID ), #FieldValidate )
				ENDIF
			ENDIF
		ENDIF
		RETURN lRet

	METHOD FLOCK() 
		// Dummy, no locking available for SQL
		RETURN TRUE

	METHOD FreeStmt( fOption )
		IF PCount() == 0
			fOption := SQL_CLOSE
		ENDIF
		RETURN oStmt:FreeStmt( fOption )

	METHOD GetData( uFieldID ) 
		LOCAL wField AS LONG
		LOCAL uValue AS USUAL
		SELF:lErrorFlag := FALSE
		TRY
			uValue := NIL
			wField := (INT) SELF:__GetColIndex( uFieldID ,TRUE)
			IF wField <= 0 .OR. wField > SELF:nNumCols
				oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADCOL ), #GetData )
				SELF:Error( oStmt:ErrInfo )
				RETURN NIL
			ENDIF
			IF SELF:lEof .OR. SELF:lBof
				// if on deleted record or at boundary, value is NIL
				uValue := NIL
			ELSEIF oCurrentRow != NULL
				LOCAL oFs AS FieldSpec
				LOCAL oDf AS DataField
				oDf := SELF:aDataFields[wField] 
				oFs := oDf:FieldSpec
                
				uValue := SELF:oConn:Factory:HandleSpecialValue(oCurrentRow:Item[wField-1], oFs,SELF:lTimeStampAsDate)
			ENDIF
		CATCH e AS Exception
			// Vulcan.NET Suppress errors for unknown field types
			// but return NIL in stead
			IF wField <= 0 .OR. wField > SELF:nNumCols
				SELF:Error(Error{e}, #FIELDGET)
			ENDIF
		END TRY
		RETURN uValue

	METHOD GetdateVal( uFieldPos )
		LOCAL cVal AS STRING
		LOCAL dVal AS DATE

		cVal := SELF:GetTimestamp( uFieldPos )
		IF !IsNil( cVal )
			dVal := CToDAnsi( SubStr3( cVal, 1, 12 ) )
		ENDIF
		RETURN dVal

	METHOD GetLookupTable(nMaxRows,uField1,uField2)
		LOCAL aResult 	:= {}         AS ARRAY
		LOCAL nRows 	:= 32767      AS DWORD
		IF nMaxRows == NIL
			nRows := 100
		ELSEIF nMaxRows<nRows
			nRows := nMaxRows
		ENDIF
		IF IsNil( uField1 )
			uField1 := 1
		ENDIF
		IF IsNil( uField2 )
			uField2 := 2
		ENDIF
		DO WHILE nRows > 0 .AND. !SELF:lEof
			AAdd( aResult, { SELF:FIELDGET( uField1 ), SELF:FIELDGET( uField2 ) } )
			--nRows
			SELF:Skip(1)
		ENDDO
		SELF:Notify( NOTIFYRECORDCHANGE )
		RETURN aResult

	
	[Obsolete];
	METHOD GetStatementOption( fOption )
		//RETURN oStmt:GetStatementOption( fOption )
		RETURN NIL

	METHOD GetTimeStamp( uFieldPos )
		LOCAL nIndex    AS DWORD
		LOCAL oType     AS System.Type
		LOCAL cVal      AS STRING
		LOCAL oColumn	 AS SQLColumn
		LOCAL oDT       AS System.DateTime
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF ( nIndex = 0 .OR. nIndex > nNumCols )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #GetTimeStamp )
			SELF:Error( oStmt:ErrInfo )
			RETURN NIL
		ENDIF
		oColumn   := aSQLColumns[nIndex]
		oType    := oColumn:Type
		IF ( oType != typeof(System.DateTime))
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #GetTimeStamp )
			SELF:Error( oStmt:ErrInfo )
			RETURN NIL
		ENDIF
		oDT := (DateTime) oCurrentRow:Item[(INT)nIndex-1]
		cVal := oDT:ToString("yyyy-MM-dd HH:mm:ss.fff")
		RETURN cVal

	METHOD GetTimeString( uFieldPos )
		LOCAL cVal AS STRING
		cVal := SELF:GetTimestamp( uFieldPos )
		IF Slen(cVal) > 0
			cVal := Left( cVal, 12 )
		ENDIF
		RETURN cVal

	METHOD GoBottom() 
		LOCAL lOk AS LOGIC
		SELF:lErrorFlag := FALSE
		TRY
			lOk := SELF:__PrepareForRecordMovement()
			IF lOk
				SELF:nCurrentRow := SELF:nRowCount -1
				SELF:__CheckEof()
				SELF:Notify( NOTIFYGOBOTTOM )
			ENDIF
		CATCH e AS Exception
			SELF:Error(Error{e}, #GoBottom)
			lOk := FALSE
		END TRY
		RETURN lOk

	METHOD GoTo( nNewRec) 
		LOCAL lOk AS LOGIC
		SELF:lErrorFlag := FALSE
		TRY
			lOk := SELF:__PrepareForRecordMovement()
			IF lOk
				IF IsNumeric(nNewRec)

					// Before we move we assume we won't land at BOF or EOF
					SELF:lEof := SELF:lBof := FALSE
					IF nNewRec >= 0 .and. nNewRec <= SELF:nRowCount
						SELF:nCurrentRow := nNewRec -1
						SELF:__CheckEOF()
					ENDIF
					SELF:Notify( NOTIFYRECORDCHANGE )
				ENDIF
			ENDIF
		CATCH e AS Exception
			SELF:Error(Error{e}, #Goto)
			lOk := FALSE
		END  TRY
		RETURN lOk

	METHOD GoTop() 
		LOCAL lOk AS LOGIC
		SELF:lErrorFlag := FALSE
		TRY
			lOk := SELF:__PrepareForRecordMovement()
			IF lOk
				// Move the recordset
				SELF:nCurrentRow := 0
				SELF:__CheckEof()
				SELF:Notify( NOTIFYGOTOP )
			ENDIF
		CATCH e AS Exception
			SELF:Error(Error{e}, #GoTop)
			lOk := FALSE
		END TRY
		RETURN lOk

  
END CLASS

