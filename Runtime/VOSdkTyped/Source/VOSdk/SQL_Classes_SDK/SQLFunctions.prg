//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Reflection
using System.Data

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



FUNCTION DToCSQL( dDate AS DATE ) AS STRING
	LOCAL dDT           AS DateTime
	dDT := dDate
	RETURN dDT:ToString("yyyy-MM-dd")




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
	STATIC METHOD SqlObject2Usual(oValue AS OBJECT, oFs := NULL AS FieldSpec, lDateTimeAsDate := TRUE AS LOGIC) AS USUAL
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
			IF oFs != NULL .and. oFs:Decimals == 0
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
				IF oFs != NULL_OBJECT
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
		IF oFs != NULL
			IF oFs:ValType == "D" 
				IF ! IsDate(uValue)
					LOCAL oDT AS DateTime
					oDT := Convert.ToDateTime((OBJECT) uValue)
					IF lDateTimeAsDate
						uValue := (DATE) oDT
					ELSE
						uValue := oDT:ToString("yyyy-MM-dd HH:mm:ss.fff")
					ENDIF
				ENDIF
			ELSEIF oFs:ValType == "L"
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
		LOCAL oError AS Error
		oError 				:= Error{}
		oError:FuncSym      := ProcName(1)
		oError:Description  := cMessage
		oError:SubSystem 	:= "NetServer"
		oError:Severity		:= nErrCode
		oError:Tries 	 	:= 1
		RETURN oError
	
	STATIC METHOD CreateError(e AS Exception) AS Error
		LOCAL oError AS Error
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

FUNCTION DotNetType2VOType(oSchema AS DataTable, oColumn AS DataColumn, cFieldName AS STRING) AS FieldSpec 
		LOCAL cName AS STRING
		LOCAL oHL AS HyperLabel
		LOCAL nLen, nDec AS LONG
		LOCAL cType AS STRING
		LOCAL oType	AS System.Type
		LOCAL TC AS TypeCode
		LOCAL oRow AS DataRow
        LOCAL oFs AS FieldSpec
		cName   := cFieldName
		oType   := oColumn:DataType
		TC      := Type.GetTypeCode(oType)
		oHL     := HyperLabel{String2Symbol(cName),cName,cName}
		nDec    := 0
		SWITCH TC
		CASE TypeCode.String 
				
			cType := "C"
			nLen := oColumn:MaxLength
			// Automatically Convert Long Strings to Memos
			IF nLen  > 255 .OR. nLen < 0
				nLen 	:= 10
				cType := "M"
			ENDIF
			oFs	:= FieldSpec{oHL,cType,nLen ,0 }
			
		CASE TypeCode.Boolean
			cType 	:= "L"
			nLen 		:= 1
			oFs 		:= LogicFS{oHL}
			oFs:Picture := "Y"
			
			
		CASE TypeCode.Double
        CASE TypeCode.Single
        CASE TypeCode.Decimal
            // .OR. (lTypeCodeIsInt .AND. oProviderType = ProviderType.Oracle)
				
				
			cType		 := "N"
			// 2.72 Changed size calculation:
			// SQL handles precision like this
			// Num 6,2 allows numbers -9999.99 thru 9999.99. So we need a VO Field N 8,2
			// Num 3,0 allows numbers -999 thru 999 So we need a VO field N 4,0.
			// So if Scale = 0 Length = Precision + 1
			// So if Scale <> 0 Length = Precision + 2
			// ...
			// In Vewa führt diese Modifikation zu Abweichungen, da hier mit den Längen der SQL-Datentypen
			// gerechnet wird. So kommt es zum Beispiel vor, dass ein Feld, was in einer SQL-Struktur mit N,5,2
			// definiert ist, in einer Makse nur mit XX,XX beschrieben werden kann. Dieses Problem ist so alt,
			// das in einigen Einrichtungen sogar damit gerechnet wird, weswegen hier nichts verändert
			// werden sollte (siehe dazu auch Ticket #4126).
			nDec := 1
			IF oSchema:Rows:Count >= oColumn:Ordinal 
				oRow := oSchema:Rows[oColumn:Ordinal]
				nDec := Convert.ToInt32(oRow:Item["NumericScale"])
				nLen := Convert.ToInt32(oRow:Item["NumericPrecision"])
//					lVersionLow := FALSE
//					IF oProviderType == ProviderType.MySql
//						iMyVersion := SupportFunctions.GetMySQLDataVersion()
//						IF iMyVersion != NULL
//							lVersionLow :=  !((iMyVersion[1] > 6) .OR. (iMyVersion[1] = 6 .AND. iMyVersion[2] > 9) .OR. (iMyVersion[1] = 6 .AND. iMyVersion[2] = 9 .AND. iMyVersion[3] >= 11))
//						ENDIF
//					ENDIF
					// Längen von Zahlen ohne Dezimalstellen kommen um eine Stelle zu kurz an
					//IF nDec = 0 .AND. oProviderType = ProviderType.MySql .AND. lVersionLow
						// Dies ist ab 6.9.11 gefixt
					//	nLen++
					//ELSEIF nLen == 0
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
			ELSE
				nDec := 2
				nLen := 10
			ENDIF
			oFs	:= NumberFS{oHL,nLen, nDec}
			
		CASE TypeCode.Int32		// -2147483647 - 2147483648 (2^31)
			nLen := 11
			oFs	:= FieldSpec{oHL,"N",nLen,0}
                
		CASE TypeCode.Int64		// - 9223372036854775807 - 9223372036854775808 (2^63)
			nLen := 21
			oFs	:= FieldSpec{oHL,"N",nLen,0}
                
		CASE TypeCode.Int16	// -32767 - 32768 (2^15)
			nLen := 6
			oFs	:= FieldSpec{oHL,"N",nLen,0}
                
		CASE TypeCode.Byte
			nLen := 4         		// -128  - 127 (2^7)
			oFs	:= FieldSpec{oHL,"N",nLen,0}
                
		CASE TypeCode.SByte	// 0 - 255 	(2^8)
			nLen := 3
			oFs	:= FieldSpec{oHL,"N",nLen,0}
                
		CASE TypeCode.UInt16	// 0 - 65535 (2^16)
			nLen := 5
			oFs	:= FieldSpec{oHL,"N",nLen,0}
                
		CASE TypeCode.UInt32		// 0 - 4294836225 (2^32)
			nLen := 10
			oFs	:= FieldSpec{oHL,"N",nLen,0}
                
		CASE TypeCode.UInt64	// 0 - 18445618199572250625 (2^64)
			nLen := 20
			oFs	:= FieldSpec{oHL,"N",nLen,0}
			
		CASE TypeCode.DateTime
				
			cType   := "D"
			nLen 	:= 8
			oFs	    := DateFS{oHL}
			
		CASE TypeCode.Object
			LOCAL lIsDate AS LOGIC
			LOCAL oMems AS MethodInfo[]
			LOCAL lFound AS LOGIC
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
					
				cType := "D"
				nLen 	:= 8
				oFs	:= DateFS{oHL}
			ELSE
				cType 	:= "C"
				nLen 	:= 10
				oFs		:= FieldSpec{oHL,"C",10,0} 
			ENDIF
			
		OTHERWISE
			cType := "C"
			nLen 	:= oColumn:MaxLength
			IF (nLen > 0)
				oFs	:= FieldSpec{oHL,"C",nLen,0}
			ELSE
				cType := "M"
				oFs := oFs	:= FieldSpec{oHL,"M",10,0}
			ENDIF
			
        END SWITCH
		RETURN oFs
