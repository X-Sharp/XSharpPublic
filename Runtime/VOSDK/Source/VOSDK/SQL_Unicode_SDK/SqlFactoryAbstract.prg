//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Data
USING System.Data.Odbc
USING System.Data.Common
USING System.Reflection
USING System.Text
USING System.Runtime.InteropServices





ABSTRACT CLASS AbstractSqlFactory IMPLEMENTS ISqlFactory
    PROTECTED oInstance AS System.Data.Common.DbProviderFactory

    PROPERTY QuoteChar AS STRING GET ""

    PROPERTY CanCreateDataSourceEnumerator AS LOGIC GET oInstance:CanCreateDataSourceEnumerator

    CONSTRUCTOR
        oInstance := NULL

    METHOD CreateConnection AS DbConnection
        RETURN oInstance:CreateConnection()
        
    METHOD CreateCommand    AS DbCommand
        RETURN oInstance:CreateCommand()
    METHOD CreateParameter  AS DbParameter
        RETURN oInstance:CreateParameter()
    METHOD CreateDataAdapter    AS DbDataAdapter
        RETURN oInstance:CreateDataAdapter()
    METHOD CreateConnectionStringBuilder AS DbConnectionStringBuilder
        RETURN oInstance:CreateConnectionStringBuilder()

    METHOD CreateDataSourceEnumerator() AS DbDataSourceEnumerator
        RETURN oInstance:CreateDataSourceEnumerator()
    METHOD AfterConnect(oConnection AS DbConnection) AS VOID
        RETURN


    METHOD BeforeConnect(cString AS STRING, cUser AS STRING, cPassword AS STRING) AS STRING
	    IF !cString:Contains("=") .AND. ! STRING.IsNullOrEmpty(cString)
			cString := "DSN="+cString+";" 
		ENDIF
		IF !STRING.IsNullOrEmpty(cUser)
			cString += "UID="+cUser+";"
		ENDIF
		IF !STRING.IsNullOrEmpty(cPassword)
			cString += "PWD="+cPassword+";"
		ENDIF
        RETURN cString
        
    METHOD BeforeDisConnect(oConnection AS DbConnection) AS VOID
        RETURN

    METHOD AfterDisConnect(oConnection AS DbConnection) AS VOID
        RETURN

    METHOD BeforeRollBack(oConnection AS DbTransaction) AS VOID
        RETURN

    METHOD AfterRollBack(oConnection AS DbTransaction) AS VOID
        RETURN

    METHOD BeforeCommit(oConnection AS DbTransaction) AS VOID
        RETURN

    METHOD AfterCommit(oConnection AS DbTransaction) AS VOID
        RETURN

    METHOD GetName(oConn AS DbConnection) AS STRING
        RETURN "ODBC"

    METHOD DriverConnect(hWindow AS USUAL, uCompletion AS USUAL, cConnectionString AS USUAL) AS STRING
        LOCAL nRetCode      AS INT
        LOCAL cConnect      AS STRING
        LOCAL sbResult      AS StringBuilder
        LOCAL cResult       AS STRING
        LOCAL nCompletion   AS WORD
        LOCAL nSize		  	AS SHORTINT
        LOCAL hWnd          AS IntPtr
        LOCAL hEnv          AS IntPtr
        LOCAL hDBc          AS IntPtr
        LOCAL nBytes        AS SHORT
        IF IsNumeric( uCompletion )
            nCompletion := uCompletion
        ELSE
            nCompletion := SQL_DRIVER_PROMPT
        ENDIF
        sbResult := StringBuilder{SQL_MAX_MESSAGE_LENGTH}
        IF hWnd ==  IntPtr.Zero
            hWnd := GetActiveWindow()
        ENDIF
        IF hWnd = NULL_PTR
            hWnd := GetDeskTopWindow()
        ENDIF
        
        IF IsString( cConnectionString )
            cConnect  := cConnectionString
            nSize     := (SHORT) SLen( cConnect ) + 1 
        ELSE
            cConnect  := ""
            nSize     := 0
        ENDIF
        
        nRetCode := OdbcFactory.SQLAllocEnv( OUT hEnv ) 
        nRetCode := OdbcFactory.SQLAllocConnect( hEnv, OUT hDBc )
        nRetCode := OdbcFactory.SQLDriverConnect( 	hDBc,                    ;
                                        hWnd,                    ;
                                        cConnect,               ;
                                        nSize    , ;
                                        sbResult,              ;
                                        SQL_MAX_MESSAGE_LENGTH,  ;
                                        OUT nBytes,                 ;
                                        nCompletion  )
        IF nRetCode == SQL_SUCCESS .OR. nRetCode == SQL_SUCCESS_WITH_INFO .AND. nBytes > 0
            cResult  := sbResult:ToString()
        ELSE
            cResult := ""
        ENDIF
        
        nRetCode := OdbcFactory.SQLDisconnect( hDBc )
        nRetCode := OdbcFactory.SQLFreeConnect(hDBc)
        nRetCode := OdbcFactory.SQLFreeEnv( hEnv )
        RETURN cResult
 

    METHOD EnhanceException(oEx AS SYstem.Exception)  AS SYstem.Exception
        RETURN oEx

    METHOD HandleSpecialValue(oValue AS OBJECT, oFS AS FieldSpec, lDateTimeAsDate AS LOGIC) AS USUAL
        RETURN oValue

    METHOD TranslateStatement(cStatement AS STRING) AS STRING
        RETURN cStatement


    METHOD AfterOpen(oDataReader AS DbDataReader) AS DbDataReader
        RETURN oDataReader

    METHOD DotNetType2VOType(oSchema AS DataTable, oColumn AS DataColumn, cFieldName AS STRING, oFS REF FieldSpec) AS ARRAY
		LOCAL cName AS STRING
		LOCAL oHL AS Hyperlabel
		LOCAL aField AS ARRAY
		LOCAL nLen, nDec AS LONG
		LOCAL cType AS STRING
		LOCAL oType	AS System.Type
		LOCAL TC AS TypeCode
		LOCAL oRow AS DataRow
		cName   := cFieldName
		oType   := oColumn:DataType
		TC      := Type.GetTypeCode(oType)
		aField	:= ArrayNew(4)
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
				oFS	:= FieldSpec{oHL,cType,nLen ,0 }
			
			CASE TypeCode.Boolean
				cType 	:= "L"
				nLen 		:= 1
				oFS 		:= LogicFS{oHL}
				oFS:Picture := "Y"
			
			
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
				oFS	:= NumberFS{oHL,nLen, nDec}
			
			CASE TypeCode.Int32		// -2147483647 - 2147483648 (2^31)
				nLen := 11
				oFS	:= FieldSpec{oHL,"N",nLen,0}
                
			CASE TypeCode.Int64		// - 9223372036854775807 - 9223372036854775808 (2^63)
				nLen := 21
				oFS	:= FieldSpec{oHL,"N",nLen,0}
                
			CASE TypeCode.Int16	// -32767 - 32768 (2^15)
				nLen := 6
				oFS	:= FieldSpec{oHL,"N",nLen,0}
                
			CASE TypeCode.Byte
				nLen := 4         		// -128  - 127 (2^7)
				oFS	:= FieldSpec{oHL,"N",nLen,0}
                
			CASE TypeCode.SByte	// 0 - 255 	(2^8)
				nLen := 3
				oFS	:= FieldSpec{oHL,"N",nLen,0}
                
			CASE TypeCode.Uint16	// 0 - 65535 (2^16)
				nLen := 5
				oFS	:= FieldSpec{oHL,"N",nLen,0}
                
			CASE TypeCode.Uint32		// 0 - 4294836225 (2^32)
				nLen := 10
				oFS	:= FieldSpec{oHL,"N",nLen,0}
                
			CASE TypeCode.Uint64	// 0 - 18445618199572250625 (2^64)
				nLen := 20
				oFS	:= FieldSpec{oHL,"N",nLen,0}
			
			CASE TypeCode.DateTime
				
				cType   := "D"
				nLen 	:= 8
				oFS	    := DateFS{oHL}
			
			CASE TypeCode.Object
				LOCAL lIsDate AS LOGIC
				LOCAL oMems AS MethodInfo[]
				LOCAL lFound AS LOGIC
				// check to see if the datatype has a dbType
				oMems := oType:GetMethods(BindingFlags.Public|BindingFlags.Static)
				FOREACH oMem AS MethodInfo IN oMems
					IF oMem:ReturnType == TypeOf(System.DateTime)  .AND. STRING.Compare(oMem:Name, "op_Explicit", StringComparison.OrdinalIgnoreCase) == 0
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
					oFS	:= DateFS{oHL}
				ELSE
					cType 	:= "C"
					nLen 	:= 10
					oFS		:= FieldSpec{oHL,"C",10,0} 
				ENDIF
			
			OTHERWISE
				cType := "C"
				nLen 	:= oColumn:MaxLength
				IF (nLen > 0)
					oFS	:= FieldSpec{oHL,"C",nLen,0}
				ELSE
					cType := "M"
					oFS := oFS	:= FieldSpec{oHL,"M",10,0}
				ENDIF
			
		END SWITCH
		aField[DBS_NAME] 	:= cName
		aField[DBS_TYPE] 	:= cType
		aField[DBS_LEN ]	:= nLen
		aField[DBS_DEC]	:= nDec
		RETURN aField        

    [DllImport("USER32.dll", CharSet := CharSet.Ansi)];
    STATIC METHOD GetActiveWindow() AS IntPtr PASCAL
        
    [DllImport("USER32.dll", CharSet := CharSet.Ansi)];
    STATIC METHOD GetDesktopWindow() AS PTR PASCAL

    [DllImport("ODBC32.dll", EntryPoint := "SQLAllocEnv")];
    STATIC METHOD SQLAllocEnv(phenv OUT IntPtr) AS SHORTINT PASCAL

    [DllImport("ODBC32.dll")];
    STATIC METHOD SQLAllocConnect(henv AS IntPtr, phdbc OUT IntPtr) AS SHORTINT PASCAL

    [DllImport("ODBC32.dll", CharSet := CharSet.Unicode)];
    STATIC METHOD SQLDriverConnect(hdbc AS IntPtr, hwnd AS IntPtr,szConnStrIn AS STRING,cbConnStrIn AS SHORT,;
	    szConnStrOut AS StringBuilder, cbConnStrOutMax AS SHORT,pcbConnStrOut OUT SHORT ,;
	    fDriverCompletion AS WORD) AS SHORTINT PASCAL
        
    [DllImport("ODBC32.dll")];
    STATIC METHOD SQLDisconnect(hdbc AS IntPtr) AS SHORTINT PASCAL

    [DllImport("ODBC32.dll")];
    STATIC METHOD SQLFreeConnect(hdbc AS IntPtr) AS SHORTINT PASCAL

    [DllImport("ODBC32.dll")];
    STATIC METHOD SQLFreeEnv(henv AS IntPtr) AS SHORTINT PASCAL


END CLASS

