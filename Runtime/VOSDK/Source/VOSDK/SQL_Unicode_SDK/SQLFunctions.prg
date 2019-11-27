//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Reflection
using System.Data
[Obsolete];
FUNCTION __SQLMaxStringSize( nValue )
	RETURN 0

[Obsolete];
FUNCTION __SQLMaxDisplaySize( nValue )
	RETURN 0

FUNCTION __GetSymString( uString AS USUAL ) AS STRING STRICT
	LOCAL nType AS DWORD
	LOCAL cRet  AS STRING
	nType := UsualType( uString )
	SWITCH nType
	CASE SYMBOL
		cRet := Symbol2String( uString )
	CASE STRING
		cRet := uString
	OTHERWISE
		cRet := NULL_STRING
	END SWITCH
	RETURN cRet

[Obsolete];
FUNCTION __AdjustString( c AS STRING ) AS STRING STRICT
	LOCAL nLen	AS DWORD
	LOCAL nSize	AS DWORD
	
	nLen  := SLen( c )
	nSize := PszLen( String2Psz( c ) )
	IF nSize < nLen
		c := SubStr3( c, 1, nSize )
	ENDIF
	RETURN c

[Obsolete];
FUNCTION __GetNULLDataValue( nODBCType AS SHORTINT, lNullAsBlank AS LOGIC ) AS USUAL PASCAL
	LOCAL xRet AS USUAL
	IF lNullAsBlank
		SWITCH nODBCType 
		CASE SQL_DATE
			xRet := NULL_DATE
			
		CASE SQL_VARCHAR
			xRet := NULL_STRING
		END SWITCH
	ENDIF
	
	RETURN xRet

[Obsolete];
FUNCTION __CheckNULLDataValue( xValue AS USUAL, nODBCType AS SHORTINT ) AS LOGIC PASCAL
	LOCAL lRet AS LOGIC
	IF IsNil( xValue )
		lRet := TRUE
	ELSE
		IF IsString( xValue ) .AND. NULL_STRING = xValue
			lRet := TRUE
			
		ELSEIF IsDate( xValue ) .AND. NULL_DATE = xValue
			lRet := TRUE
		ENDIF
	ENDIF
	
	RETURN lRet


[Obsolete];
FUNCTION __CheckHandles( hEnv AS USUAL, hDbc AS USUAL, hStmt AS USUAL ) AS LOGIC STRICT
	LOCAL lRet  AS LOGIC
	IF IsPtr( hEnv ) .AND. IsPtr( hDbc ) .AND. IsPtr( hStmt )
		lRet := TRUE
	ENDIF
	RETURN lRet

FUNCTION DToCSQL( dDate AS DATE ) AS STRING
	LOCAL dDT           AS DateTime
	dDT := dDate
	RETURN dDT:ToString("yyyy-MM-dd")

[Obsolete];
FUNCTION MakeTimeStamp( dDate, nSeconds )
	RETURN ""

[Obsolete];
FUNCTION  SQL_LEN_DATA_AT_EXEC( nLength AS INT ) AS INT STRICT
	RETURN -nLength + SQL_LEN_DATA_AT_EXEC_OFFSET



[Obsolete];
FUNCTION SqlSetStmtConcurrency( nNew ) AS INT
	STATIC nValue := __CAVO_SQL_CONCURRENCY AS INT
	LOCAL nResult AS INT
	nResult := nValue
	IF IsNumeric( nNew )
		nValue := nNew
	ENDIF
	RETURN nResult

[Obsolete];
FUNCTION SqlSetStmtCursorType( nNew ) AS INT
	STATIC nValue := __CAVO_SQL_CURSOR_TYPE AS INT
	LOCAL nResult AS INT
	nResult := nValue
	IF IsNumeric( nNew )
		nValue := nNew
	ENDIF
	RETURN nResult

[Obsolete];
FUNCTION SqlSetStmtSimulateCursor( nNew ) AS INT
	STATIC nValue := __CAVO_SQL_SIMULATE_CURSOR  AS INT
	LOCAL nResult AS INT
	nResult := nValue
	IF IsNumeric( nNew )
		nValue := nNew
	ENDIF
	RETURN nResult


[Obsolete];
FUNCTION SqlDeleteWhiteSpace( cString AS STRING ) AS STRING STRICT
	//LOCAL cResult AS STRING
	//LOCAL pString AS PSZ
	//LOCAL pTarget AS PTR
	//LOCAL pWalk   AS BYTE PTR
	//LOCAL pTWalk  AS BYTE PTR
	//LOCAL nSlen   AS DWORD
	//LOCAL i       AS DWORD
	//LOCAL cChar   AS STRING
	//LOCAL lCopy   AS LOGIC
	//LOCAL cCloseDelim AS STRING
	//LOCAL cDelims     AS STRING
	//LOCAL cWhitespace AS STRING
	
	//LOCAL lWhiteSpace AS LOGIC
	
	//lWhiteSpace := FALSE
	//lCopy       := FALSE
	
	//cDelims := "'"+chr(34)+"["
	////Stripping out CRLF may be dangerous if they used end of line comments...
	//cWhitespace := " "+_CHR(9) //+CHR(10)+CHR(13)+CHR(141)     
	
	//nSlen   := SLen(cString)
	//pString := StringAlloc(cString)
	//pWalk   := PTR(_CAST,pString)
	//IF (pWalk == NULL_PTR)
	//	SQLThrowOutOfMemoryError()
	//ENDIF
	
	//pTWalk  := MemAlloc( nSLen+1 )                                 // fürs zero terminated
	//pTarget := PSZ(_CAST,pTWalk)                                   // merken zum freigeben
	
	//FOR i:=1 UPTO nSlen+1
	//   cChar := CHR(BYTE(pWalk))                              // get character
	//   IF lCopy                                                   // innerhalb von delimitern wird einfach blind kopiert
	//      BYTE(pTWalk) := BYTE(pWalk)                             // kopieren
	//      pWalk  += 1
	//      pTWalk += 1
	//      lCopy := ( cChar != cCloseDelim )                       // wenn wir  den schließenden delimiter erreicht haben , kopieren aus
	//   ELSE
	//      DO CASE
	//      CASE At2(cChar,cDelims) > 0                          // wir kommen an einen delimitierten STRING
	//         lCopy := TRUE                                   // kopieren einschalten
	//         IF cChar == "["                                 //  Sonderfall, asymetrischer delimiter
	//            cCloseDelim := "]"
	//         ELSE
	//            cCloseDelim := cChar
	//         ENDIF
	//         BYTE(pTWalk) := BYTE(pWalk)                     // kopieren
	//         pWalk  += 1
	//         pTWalk += 1
	//         lWhiteSpace := FALSE
	//      CASE !lWhiteSpace .AND. At2(cChar,cWhitespace ) > 0  // erster Whitespace, als Space kopieren !
	//         BYTE(pTWalk) := Asc(" ")
	//         pWalk  += 1
	//         pTWalk += 1
	//         lWhiteSpace := TRUE
	//      CASE lWhiteSpace .AND. At2(cChar,cWhitespace ) > 0   // weiter  Whitspaces ignorieren
	//         pWalk += 1
	//      OTHERWISE                                            // Normalfall, einfach kopieren
	//         lWhiteSpace := FALSE
	//         BYTE(pTWalk) := BYTE(pWalk)                     // kopieren
	//         pWalk  += 1
	//         pTWalk += 1
	//      ENDCASE
	//   ENDIF
	//NEXT
	//cResult := Psz2String(pTarget)
	//MemFree( pTarget )
	//MemFree( pString )
	RETURN cString



FUNCTION SQLThrowOutOfMemoryError() AS VOID STRICT
	LOCAL oError AS Error
	oError:=Error{}
	oError:SubSystem := __CavoStr( __CAVOSTR_SQLCLASS_SUBSYS )
	oError:Description:="Out of memory"
	THROW oError
	//RETURN

[Obsolete];
FUNCTION _SLen( c AS STRING ) AS SHORTINT STRICT
	RETURN (SHORT) SLen( c ) 




STATIC CLASS SqlFunctions
/*
	STATIC METHOD SqlObject2Usual(oValue AS OBJECT, oFS := NULL AS FieldSpec, lDateTimeAsDate := TRUE AS LOGIC) AS USUAL
	LOCAL uValue AS USUAL  
	LOCAL oType  AS System.Type
	LOCAL oTC    AS TypeCode
	LOCAL fValue AS FLOAT
	TRY      
		
		IF oValue == NULL
			RETURN NIL
		ENDIF
		oType := oValue:GetType()
		oTC := Type.GetTypeCode(oType)
		DO CASE
		CASE oTC == TypeCode.DBNull
			uValue := NIL
		CASE oTC == TypeCode.Decimal
			LOCAL oDec AS System.Decimal
			oDec := (System.Decimal) oValue                
			IF oFS != NULL .and. oFS:Decimals == 0
				TRY
					IF oDec > (System.Decimal) System.Int32.MaxValue .or. oDec < (System.Decimal) System.Int32.MinValue
						LOCAL oInt64 AS System.Int64
						oInt64 := (System.Int64) oDec
						uValue := oInt64
					ELSE
						uValue := (System.Int32) oDec
					ENDIF
				CATCH E AS OverflowException
					fValue := (System.Double) oDec
					fValue:Decimals := 0
					uValue := fValue
				END TRY
			ELSE
				fValue := (System.Double) oDec
				IF oFS != NULL_OBJECT
					fValue:Decimals := (int) oFs:Decimals
				ENDIF
				uValue := fValue
			ENDIF
		CASE oTC == TypeCode.Int64
			LOCAL oInt64 AS System.Int64
			oInt64 := (System.Int64) oValue                
			TRY
				IF oInt64 > (System.Int64) System.Int32.MaxValue .or. oInt64 < (System.Int64) System.Int32.MinValue
					fValue := (System.Double) oInt64
					fValue:Decimals := 0
					uValue := fValue
				ELSE
					uValue := (System.Int32) oInt64
				ENDIF
			CATCH E AS OverflowException
				fValue := (System.Double) oInt64
				fValue:Decimals := 0
				uValue := fValue
			END TRY
            CASE oTC == TypeCode.Object
//                uValue := oFactory:HandleSpecialValue(oValue)
//			    IF oType == typeof(MySql.Data.Types.MySqlDateTime)
//				    LOCAL oMyDT AS MySql.Data.Types.MySqlDateTime
//				    LOCAL oDT AS System.DateTime
//				    oMyDT := (MySql.Data.Types.MySqlDateTime) oValue
//				    IF oMyDT:IsValidDateTime
//					    oDT   := oMyDT:GetDateTime()
//					    IF lDateTimeAsDate
//						    uValue := DATE{oDT}
//					    ELSE
//						    uValue := oDT:ToString("yyyy-MM-dd HH:mm:ss.fff")
//					    ENDIF
//				    ELSE
//					    IF lDateTimeAsDate
//						    uValue := NULL_DATE
//					    ELSE
//						    uValue := STRING.Empty
//					    ENDIF
//				    ENDIF
//			    ELSE
//				    uValue := oValue
//			    ENDIF
		OTHERWISE
			uValue := oValue
		ENDCASE
		IF oFS != NULL
			IF oFS:ValType == "D" 
				IF ! IsDate(uValue)
					LOCAL oDT AS DateTime
					oDT := Convert.ToDateTime((OBJECT) uValue)
					IF lDateTimeAsDate
						uValue := (DATE) oDT
					ELSE
						uValue := oDT:ToString("yyyy-MM-dd HH:mm:ss.fff")
					ENDIF
				ENDIF
			ELSEIF oFS:ValType == "L"
				IF IsNumeric(uValue)
					uValue := uValue != 0  
				ELSEIF IsString(uValue)
					uValue := Val(uValue) != 0
				ENDIF
			ENDIF
		ENDIF
	CATCH E AS Exception
		#ifdef VEWA7
			BusyMessageBox.breakBusyMessage()
			DevExpress.XtraEditors.XtraMessageBox.Show("Error converting SQL Data type to VO type: "+E:Message,__ENTITY__)
			BusyMessageBox.unbreakBusyMessage()
		#else
			System.Windows.Forms.MessageBox.Show("Error converting SQL Data type to VO type: "+E:Message,__ENTITY__)
		#endif
		uValue := NIL
	END TRY
	RETURN uValue
*/
	STATIC METHOD FieldNameCheck(cName AS STRING, aFldNames AS ARRAY ) AS STRING
		LOCAL dwPos, dwFld AS DWORD
		LOCAL cNewname		 AS STRING
		IF Empty(cName)
			dwFld := 0
			dwPos := 1
			DO WHILE dwPos > 0
				++dwFld
				cName := "FLD"+StrZero(dwFld,3,0)
				dwPos := AScanExact(aFldNames, Upper(cName))
			ENDDO
		ELSE
			// remove column prefixes
			dwPos := At2(".", cName)
			IF dwPos > 0
				cName := SubStr2(cName, dwPos+1)
			ENDIF
			// remove embedded spaces
			cName 	:= StrTran(cName, " ", "_")
			cNewname := cName
			dwFld 	:= 1
			dwPos 	:= AScanExact(aFldNames, Upper(cNewname))
			DO WHILE dwPos > 0
				#IFDEF __DEBUG__
				System.Diagnostics.Debug.WriteLine("Column name", cNewname, " Found")
				#ENDIF
				++dwFld
				cNewname := Left(cName, 10 - SLen(NTrim(dwFld))) + NTrim(dwFld)
				dwPos := AScanExact(aFldNames, Upper(cNewname))
			ENDDO
			cName 	:= cNewname
		ENDIF
		AAdd(aFldNames, Upper(cName))
		RETURN cName


       
	STATIC METHOD CreateError(nErrCode AS DWORD, cMessage AS STRING)
		LOCAL oError AS ERROR
		oError 				:= Error{}
		oError:FuncSym      := ProcName(1)
		oError:Description  := cMessage
		oError:SubSystem 	:= "NetServer"
		oError:Severity		:= nErrCode
		oError:Tries 	 	:= 1
		RETURN oError
	
	STATIC METHOD CreateError(e AS Exception) AS Error
		LOCAL oError AS ERROR
		oError 				:= Error{e}
		oError:FuncSym      := ProcName(1)
		oError:Description  := e:Message
		oError:SubSystem 	:= "NetServer"
		oError:Severity		:= ES_ERROR
		oError:Tries 	 	:= 1
		RETURN oError
	STATIC PRIVATE INITONLY cErlaubt AS STRING
	STATIC PRIVATE INITONLY aFieldNames AS System.Collections.Generic.Dictionary<STRING, STRING>
	STATIC CONSTRUCTOR
		cErlaubt   := "ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789_abcdefghijklmnopqrstuvwxyz"	
		aFieldNames := System.Collections.Generic.Dictionary<STRING, STRING>{}

    STATIC METHOD  CleanupColumnName( cColumnName AS  STRING ) AS STRING
		LOCAL sb AS System.Text.StringBuilder
		LOCAL lLastWasOk AS LOGIC
		LOCAL cResult AS STRING
		LOCAL cWork		AS STRING
		IF aFieldNames:ContainsKey(cColumnName)
			RETURN aFieldNames[cColumnName]
		ENDIF		
		// Gibt nur erlaubte zeichen zurück. Alles andere wird als Unterstrich dargestellt.
		sb  := System.Text.StringBuilder{}
		// When the column is an expresion like CONCAT( foo, bar)
		// Then remove the function name and the last closing param
		// when there is more than one function we remove all functions
		cWork := cColumnName
		DO WHILE cWork:Contains("(") .and. cWork:Contains(")") .and. cWork:IndexOf("(") < cWork:IndexOf(")")
			cWork := cWork:Substring(cWork:IndexOf('(')+1)
			cWork := cWork:Substring(0, cWork:LastIndexOf(')'))
		ENDDO
		// Remove the paramter delimiters
		cWork := cWork:Replace(",", "")
		lLastWasOk := FALSE
		FOREACH IMPLIED cZeichen IN cWork
			IF cErlaubt:IndexOf(cZeichen) >= 0
				sb:Append(cZeichen)
				lLastWasOk := TRUE
			ELSEIF lLastWasOk
				sb:Append('_')
				lLastWasOk := FALSE
			ENDIF
		NEXT
		IF sb:Length == 0
			// Something like "Count(*)" was passed in
			cWork := cColumnName
			cWork := cWork:Replace("(", "")
			cWork := cWork:Replace(")", "")
			FOREACH IMPLIED cZeichen IN cWork
				IF cErlaubt:IndexOf(cZeichen) >= 0
					sb:Append(cZeichen)
					lLastWasOk := TRUE
				ELSEIF lLastWasOk
					sb:Append('_')
					lLastWasOk := FALSE
				ENDIF
			NEXT
		ENDIF	
		IF sb:Length > 1
			DO WHILE sb[sb:Length-1] == '_'
				sb:Remove(sb:Length-1,1)
			ENDDO
		ENDIF	
		IF sb:Length == 0
			cResult := "EXPR" 
		ELSE	
			cResult := sb:ToString()
		ENDIF
		aFieldNames:Add(cColumnName, cResult)
		RETURN cResult
	
END CLASS
