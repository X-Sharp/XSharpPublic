//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//




USING System.Data.Common
USING System.Data



PARTIAL CLASS SQLSelect INHERIT DataServer


/// <include file="Sql.xml" path="doc/SQLSelect.FieldHyperLabel/*" />
	METHOD FieldHyperLabel  ( uFieldPos AS USUAL) AS HyperLabel
		LOCAL nIndex    AS DWORD
		LOCAL oRet      := NULL AS HyperLabel


		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF ( nIndex = 0 .OR. nIndex > nNumCols )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldHyperLabel )
		ELSE
			oStmt:ErrInfo:ErrorFlag := FALSE
			oRet := ((SQLColumn)aSQLColumns[nIndex]):HyperLabel
		ENDIF
		RETURN oRet


/// <include file="Sql.xml" path="doc/SQLSelect.FieldInfo/*" />
	METHOD FieldInfo( kFieldInfoType, uFieldPos, uFieldVal ) AS USUAL CLIPPER
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
		LOCAL xRet      := NIL AS USUAL
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


/// <include file="Sql.xml" path="doc/SQLSelect.FieldName/*" />
	METHOD FieldName( uFieldID AS USUAL ) AS STRING
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




/// <include file="Sql.xml" path="doc/SQLSelect.FieldPos/*" />
	METHOD FieldPos( uFieldID  AS USUAL)  AS DWORD STRICT
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


/// <include file="Sql.xml" path="doc/SQLSelect.FieldPut/*" />
	METHOD FieldPut( uFieldID AS USUAL , uValue AS USUAL) AS USUAL


		LOCAL wField AS INT
		LOCAL oError AS Error
		LOCAL oColumn AS DataColumn
		LOCAL oDf AS DataField
        TRY
        	SELF:__CheckReadOnly()
        CATCH e as Exception
            SELF:ShowSQLError(e:Message, #FieldPut,e )
        END TRY
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
                else
                    var old := oCurrentRow:Item[wField-1]
                    var newType := UsualType(uValue)
                    var nullType := (dword) __UsualType.Null
                    local changed as logic
                    // you can't compare DBNull.Value !
                    if old == DBNull.Value
                        changed := newType != nullType
                    elseif newType ==nullType
                        changed := true
                    else
                        changed := old != uValue
                    endif
                    if changed
					    oCurrentRow:Item[wField-1]	:= uValue
                    endif
				ENDIF
                SELF:lChanges := TRUE
				oDf := SELF:aDataFields[ wField ]
				SELF:Notify( NOTIFYFIELDCHANGE, oDf:NameSym )
			ENDIF
		CATCH e AS Exception
			SELF:Error(Error{e}, #FIELDPUT)
		END TRY
		RETURN uValue


/// <include file="Sql.xml" path="doc/SQLSelect.FieldSpec/*" />
	METHOD FieldSpec( uFieldPos AS USUAL)  AS FieldSpec
		LOCAL nIndex AS DWORD
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF ( nIndex = 0 .OR. nIndex > SELF:FCount )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #FieldSpec )
			RETURN NULL_OBJECT
		ENDIF
		oStmt:ErrInfo:ErrorFlag := FALSE
		RETURN ((DataField) aDataFields[nIndex]):FieldSpec




/// <include file="Sql.xml" path="doc/SQLSelect.FieldStatus/*" />
	METHOD FieldStatus( uFieldPos AS USUAL) AS HyperLabel
		LOCAL nIndex AS DWORD
		LOCAL oRet   := NULL AS HyperLabel
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


/// <include file="Sql.xml" path="doc/SQLSelect.FieldSym/*" />
	METHOD FieldSym( uFieldPos AS USUAL)  AS SYMBOL
		LOCAL nIndex        AS DWORD
		LOCAL symRet    := NULL_SYMBOL   AS SYMBOL
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


/// <include file="Sql.xml" path="doc/SQLSelect.FieldValidate/*" />
	METHOD FieldValidate( uField AS USUAL, uValue AS USUAL) AS LOGIC
		LOCAL nIndex    AS DWORD
		LOCAL lRet    := FALSE  AS LOGIC


		nIndex := SELF:__GetColIndex( uField, TRUE )
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


/// <include file="Sql.xml" path="doc/SQLSelect.FLock/*" />
	METHOD FLock( ) AS LOGIC STRICT
		// Dummy, no locking available for SQL
		RETURN TRUE


/// <include file="Sql.xml" path="doc/SQLSelect.FreeStmt/*" />
	METHOD FreeStmt( fOption := SQL_CLOSE AS WORD ) AS LOGIC
		RETURN oStmt:FreeStmt( fOption )


/// <include file="Sql.xml" path="doc/SQLSelect.GetData/*" />
	METHOD GetData( uFieldID AS USUAL )  AS USUAL
		LOCAL wField := 0 AS LONG
		LOCAL uValue := NIL AS USUAL
		SELF:lErrorFlag := FALSE
		TRY
			uValue := NIL
			wField := (INT) SELF:__GetColIndex( uFieldID ,TRUE)
			IF wField <= 0 .OR. wField > SELF:nNumCols
				oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADCOL ), #GetData )
				SELF:Error( oStmt:ErrInfo ,#GetData)
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


/// <include file="Sql.xml" path="doc/SQLSelect.GetdateVal/*" />
	METHOD GetdateVal( uFieldPos AS USUAL) AS DATE
		LOCAL cVal  AS STRING
		LOCAL dVal := NULL_DATE AS DATE


		cVal := SELF:GetTimeStamp( uFieldPos )
		IF !IsNil( cVal )
			dVal := CToDAnsi( SubStr3( cVal, 1, 12 ) )
		ENDIF
		RETURN dVal


/// <include file="Sql.xml" path="doc/SQLSelect.GetLookupTable/*" />
	METHOD GetLookupTable(nMaxRows,uField1,uField2, uSearch) AS ARRAY CLIPPER
		LOCAL aResult 	:= {}         AS ARRAY
		LOCAL nRows 	:= 32767      AS DWORD
		IF IsNil(nMaxRows)
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
			AAdd( aResult, { SELF:FieldGet( uField1 ), SELF:FieldGet( uField2 ) } )
			--nRows
			SELF:Skip(1)
		ENDDO
		SELF:Notify( NOTIFYRECORDCHANGE )
		RETURN aResult




/// <include file="Sql.xml" path="doc/SQLSelect.GetStatementOption/*" />
	[Obsolete];
	METHOD GetStatementOption( fOption ) AS USUAL CLIPPER
		//RETURN oStmt:GetStatementOption( fOption )
		RETURN NIL


/// <include file="Sql.xml" path="doc/SQLSelect.GetTimeStamp/*" />
	METHOD GetTimeStamp( uFieldPos AS USUAL) AS STRING
		LOCAL nIndex    AS DWORD
		LOCAL oType     AS System.Type
		LOCAL cVal      AS STRING
		LOCAL oColumn	 AS SQLColumn
		LOCAL oDT       AS System.DateTime
		nIndex := SELF:__GetColIndex( uFieldPos, TRUE )
		IF ( nIndex = 0 .OR. nIndex > nNumCols )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #GetTimeStamp )
			SELF:Error( oStmt:ErrInfo , #GetTimeStamp )
			RETURN NIL
		ENDIF
		oColumn   := aSQLColumns[nIndex]
		oType    := oColumn:Type
		IF ( oType != typeof(System.DateTime))
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADFLD ), #GetTimeStamp )
			SELF:Error( oStmt:ErrInfo , #GetTimeStamp )
			RETURN NIL
		ENDIF
		oDT := (DateTime) oCurrentRow:Item[(INT)nIndex-1]
		cVal := oDT:ToString("yyyy-MM-dd HH:mm:ss.fff")
		RETURN cVal


/// <include file="Sql.xml" path="doc/SQLSelect.GetTimeString/*" />
	METHOD GetTimeString( uFieldPos AS USUAL ) AS STRING
		LOCAL cVal AS STRING
		cVal := SELF:GetTimeStamp( uFieldPos )
		if SLen(cVal) > 0
			cVal := Left( cVal, 12 )
		ENDIF
		RETURN cVal


/// <include file="Sql.xml" path="doc/SQLSelect.GoBottom/*" />
	METHOD GoBottom() AS LOGIC STRICT
		LOCAL lOk AS LOGIC
		SELF:lErrorFlag := FALSE
		TRY
			lOk := SELF:__PrepareForRecordMovement()
			IF lOk
				SELF:nCurrentRow := SELF:nRowCount -1
				SELF:__CheckEOF()
				SELF:Notify( NOTIFYGOBOTTOM )
			ENDIF
		CATCH e AS Exception
			SELF:Error(Error{e}, #GoBottom)
			lOk := FALSE
		END TRY
		RETURN lOk


/// <include file="Sql.xml" path="doc/SQLSelect.GoTo/*" />
	METHOD GoTo( nNewRec AS LONG ) AS LOGIC
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


/// <include file="Sql.xml" path="doc/SQLSelect.GoTop/*" />
	METHOD GoTop() AS LOGIC STRICT
		LOCAL lOk AS LOGIC
		SELF:lErrorFlag := FALSE
		TRY
			lOk := SELF:__PrepareForRecordMovement()
			IF lOk
				// Move the recordset
				SELF:nCurrentRow := 0
				SELF:__CheckEOF()
				SELF:Notify( NOTIFYGOTOP )
			ENDIF
		CATCH e AS Exception
			SELF:Error(Error{e}, #GoTop)
			lOk := FALSE
		END TRY
		RETURN lOk




END CLASS


