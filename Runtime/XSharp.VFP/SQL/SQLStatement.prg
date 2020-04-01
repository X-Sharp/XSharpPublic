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
USING System.Data.Odbc
USING XSharp.VFP
USING System.Reflection
USING XSharp.RDD
USING XSharp.RDD.Enums


INTERNAL CLASS XSharp.VFP.SQLStatement
    PROTECT _oConnection    AS SQLConnection
    PROTECT _oNetCommand    AS DbCommand
    PROTECT _oTransaction   AS DbTransaction
    
    PROPERTY Connected          AS LOGIC GET Connection:State == System.Data.ConnectionState.Open
    PROPERTY Connection         AS SQLConnection GET _oConnection
    PROPERTY Asynchronous       AS LOGIC  AUTO GET SET
    PROPERTY BatchMode          AS LOGIC  AUTO GET SET
    PROPERTY ConnectBusy        AS LOGIC  GET Connection:ConnectBusy
    PROPERTY ConnectionTimeOut  AS LONG   GET Connection:ConnectionTimeOut  SET Connection:ConnectionTimeOut := Value
    PROPERTY DataSource         AS STRING GET Connection:DataSource         SET Connection:DataSource := Value
    PROPERTY UserId             AS STRING GET Connection:UserId     
    PROPERTY Password           AS STRING GET Connection:Password   
    PROPERTY Shared             AS LOGIC  GET Connection:Shared             SET Connection:Shared := Value
    PROPERTY ConnectionString   AS STRING GET Connection:ConnectionString   
    PROPERTY ODBChdbc           AS IntPtr GET SELF:Connection:ODBChdbc
    
    PROPERTY DisconnectRollback AS LOGIC AUTO GET SET
    PROPERTY DispWarnings       AS LOGIC AUTO GET SET
    PROPERTY IdleTimeout        AS LONG  AUTO GET SET
    PROPERTY PacketSize         AS LONG  AUTO GET SET
    PROPERTY QueryTimeOut       AS LONG  GET _oNetCommand:CommandTimeout SET _oNetCommand:CommandTimeout := Value
    PROPERTY TransactionMode    AS LONG  AUTO GET SET
    PROPERTY WaitTime           AS LONG  AUTO GET SET
    PROPERTY Prepared           AS LOGIC AUTO GET PRIVATE SET
    PROPERTY CursorName         AS STRING AUTO GET SET

    PROPERTY ODBChstmt AS IntPtr
        GET
            IF SELF:_oNetCommand IS OdbcCommand
                LOCAL oDCommand := (OdbcCommand) _oNetCommand AS OdbcCommand
                LOCAL oFld AS FieldInfo
                LOCAL oType AS System.Type
                oType := oDCommand:GetType():UnderlyingSystemType
                oFld := oType:GetField("_cmdWrapper", BindingFlags.NonPublic | BindingFlags.Instance) 
                IF oFld != NULL_OBJECT
                    LOCAL oWrap AS OBJECT
                    LOCAL oWrapType AS System.Type
                    oWrap := oFld:GetValue(oDCommand)
                    IF oWrap != NULL
                        oWrapType  := oWrap:GetType()
                        LOCAL oProp := oWrapType:GetProperty("StatementHandle", BindingFlags.NonPublic | BindingFlags.Instance) AS PropertyInfo
                        IF oProp != NULL
                            LOCAL oHandle AS OBJECT
                            LOCAL oHandleType AS System.Type
                            oHandle := oProp:GetValue(oWrap, NULL)
                            oHandleType := oHandle:GetType()
                            LOCAL oMethod AS MethodInfo
                            oMethod := oHandleType:GetMethod("DangerousGetHandle")
                            IF oMethod != NULL
                                RETURN oMethod:Invoke(oHandle,NULL)
                            ENDIF
                        ENDIF
                    ENDIF                        
                ENDIF
            ENDIF
            RETURN IntPtr.Zero    
        END GET
    END PROPERTY



    METHOD SetDefaults() AS VOID
        SELF:Shared             := FALSE
        SELF:Asynchronous       := FALSE
        SELF:BatchMode          := FALSE
        SELF:DisconnectRollback := FALSE
        SELF:DispWarnings       := FALSE
        SELF:IdleTimeout        := 0
        SELF:PacketSize         := 4096
        SELF:Prepared           := FALSE
        SELF:QueryTimeOut       := 0
        SELF:TransactionMode    := DB_TRANSAUTO
        SELF:WaitTime           := 100
        SELF:_oNetCommand       := NULL
        SELF:_oConnection       := NULL
        SELF:_oTransaction      := NULL


    PRIVATE METHOD _AllocateCommand() AS VOID
        SELF:_oNetCommand := SELF:Connection:Factory:CreateCommand()
        SELF:_oNetCommand:Connection := SELF:Connection:NetConnection
        SELF:_oNetCommand:CommandText := "Select 1"
        SELF:_oNetCommand:ExecuteNonQuery()
        RETURN 

    
    CONSTRUCTOR(cDataSource AS STRING, cUser AS STRING, cPassword AS STRING, lShared AS LOGIC)
        SELF:SetDefaults()
        _oConnection    := SQLConnection{cDataSource, cUser, cPassword, lShared}
        SELF:_AllocateCommand()
        RETURN 
        

    CONSTRUCTOR(cConnectionString AS STRING, lShared AS LOGIC)
        _oConnection := SQLConnection{cConnectionString, lShared}
        SELF:_AllocateCommand()
        RETURN

    CONSTRUCTOR(oConnection AS SQLConnection)
        _oConnection := oConnection
        SELF:_AllocateCommand()
        RETURN

    METHOD Commit AS LOGIC
        IF SELF:_oTransaction != NULL
            SELF:_oTransaction:Commit()
            SELF:_oTransaction := NULL
            RETURN TRUE
        ENDIF
        RETURN FALSE

    METHOD Rollback AS LOGIC
        IF SELF:_oTransaction != NULL
            SELF:_oTransaction:Rollback()
            SELF:_oTransaction := NULL
            RETURN TRUE
        ENDIF
        RETURN FALSE


    METHOD DisConnect AS LOGIC
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
        _oNetCommand := NULL
        _oConnection := NULL
        RETURN TRUE

    METHOD Prepare(cCommand AS STRING, cCursorName AS STRING) AS LONG
        SELF:_oNetCommand:CommandText := cCommand
        SELF:_oNetCommand:Prepare()
        SELF:Prepared := TRUE
        SELF:CursorName := cCursorName
        RETURN 1

    METHOD Execute(cCommand AS STRING, cCursorName AS STRING, aInfo AS ARRAY) AS LONG
        SELF:_oNetCommand:CommandText := cCommand
        SELF:CursorName := cCursorName
        VAR tables := SELF:CopyToCursor()
        CopyToInfo(tables, aInfo)
        RETURN 1
        
    METHOD Execute(aInfo AS ARRAY) AS LONG
        VAR tables := SELF:CopyToCursor()
        CopyToInfo(tables, aInfo)
        RETURN 1

    PRIVATE METHOD CopyToInfo(aResult, aInfo AS ARRAY) AS VOID
        ASize(aInfo, ALen(aResult))
        ACopy(aResult, aInfo)
        RETURN

    METHOD CopyToCursor() AS ARRAY
        VAR oDataReader := SELF:_oNetCommand:ExecuteReader()
        VAR cursorName := SELF:CursorName
        LOCAL nAreas   := 0 AS LONG
        LOCAL result   := {} AS ARRAY
        REPEAT
    		oDataReader := SELF:Connection:Factory:AfterOpen(oDataReader)
            VAR oSchema := oDataReader:GetSchemaTable()
            CreateWorkarea(oSchema, oDataReader, cursorName)
            AAdd(result, {cursorName, RecCount()})
            nAreas += 1
            cursorName := SELF:CursorName+nAreas:ToString()
        UNTIL !oDataReader:NextResult()
        
        RETURN result

    METHOD CreateWorkarea(oSchema AS DataTable, oDataReader AS DbDataReader, cCursorName AS STRING) AS LOGIC
        LOCAL aStruct AS ARRAY
        LOCAL nFields AS LONG
        LOCAL aFieldNames AS List<STRING>
        nFields := oSchema:Rows:Count
        aFieldNames := List<STRING>{}
        aStruct := ArrayNew( (DWORD) nFields)
        nFields := 1
        FOREACH schemaRow AS DataRow IN oSchema:Rows
            aStruct[nFields] := SchemaRowToFieldInfo(schemaRow,aFieldNames)
            nFields++
        NEXT
        nFields := aStruct:Count
        VAR cTemp := System.IO.Path.GetTempFileName()
        DbCreate(cTemp, aStruct, "DBFVFP")
        VoDbUseArea(TRUE, "DBFVFPSQL",cTemp,cCursorName,FALSE,FALSE)
        LOCAL oRDD AS IRdd
        oRDD := (IRdd) DbInfo(DbInfo.DBI_RDD_OBJECT)
        // Set column Aliases to the original column name
//        FOR VAR nFld := 1 TO oRDD:FieldCount
//            oRDD:FieldInfo(nFld, DBS_ALIAS, aStruct[nFld, DBS_ALIAS])
//        NEXT
        VAR data := OBJECT[]{oSchema:Rows:Count}
        DO WHILE oDataReader:Read()
            oRDD:Append(TRUE)
            oDataReader:GetValues(data)
            FOR VAR nFld := 1 UPTO nFields
                oRDD:PutValue(nFld, data[nFld])
            NEXT
        ENDDO
        RETURN TRUE
   



    METHOD SchemaRowToFieldInfo(schemaRow AS DataRow,aFieldNames AS IList<STRING>) AS ARRAY
        VAR cColumnName := schemaRow["ColumnName"]:ToString( )
        VAR fieldType  := DotNetType2DbfType(schemaRow)
        VAR cFldName   := SQLSupport.CleanupColumnName(cColumnName)
	    cFldName	    := SQLSupport.FieldNameCheck(cFldName, aFieldNames)
        RETURN {cFldName, fieldType:Item1, fieldType:Item2,fieldType:Item3, cColumnName}
    
            
    STATIC METHOD DotNetType2DbfType(schemaRow AS DataRow) AS Tuple<STRING,LONG,LONG>
		LOCAL nLen, nDec AS LONG
		LOCAL cType AS STRING
		LOCAL oType	AS System.Type
		LOCAL TC AS TypeCode
        LOCAL result AS Tuple<STRING,LONG,LONG>
		oType   := (Type) schemaRow["DataType"]
		TC      := Type.GetTypeCode(oType)
		nDec    := 0
		SWITCH TC
		CASE TypeCode.String 
			cType   := "C"
			nLen    := (Int32)schemaRow["ColumnSize"]
			// Automatically Convert Long Strings to Memos
			IF nLen  > 255 .OR. nLen < 0
				nLen    := 4
				cType   := "M"
            ENDIF
            result := Tuple<STRING,LONG,LONG>{cType,nLen ,0 }
			
		CASE TypeCode.Boolean
			result := Tuple<STRING,LONG,LONG>{"L",1 ,0 }

        CASE TypeCode.Decimal
		    result := Tuple<STRING,LONG,LONG>{"Y",16 ,4 }
		CASE TypeCode.Double
        CASE TypeCode.Single
			nDec := 1
			nLen := 10
			nDec := Convert.ToInt32(schemaRow:Item["NumericScale"])
			nLen := Convert.ToInt32(schemaRow:Item["NumericPrecision"])
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
        result := Tuple<STRING,LONG,LONG>{"N",nLen ,nDec }
			
		CASE TypeCode.Int32		// -2147483647 - 2147483648 (2^31)
            IF (LOGIC)schemaRow["IsAutoIncrement"]
                result := Tuple<STRING,LONG,LONG>{"I:+",4 ,0}
            ELSE
                result := Tuple<STRING,LONG,LONG>{"I",4 ,0}
            ENDIF
               
		CASE TypeCode.Int64		// - 9223372036854775807 - 9223372036854775808 (2^63)
            result := Tuple<STRING,LONG,LONG>{"N",21 ,0}
                
		CASE TypeCode.Int16	// -32767 - 32768 (2^15)
            result := Tuple<STRING,LONG,LONG>{"N",6 ,0}
                
		CASE TypeCode.Byte
            result := Tuple<STRING,LONG,LONG>{"N",4 ,0}
                
		CASE TypeCode.SByte	// 0 - 255 	(2^8)
            result := Tuple<STRING,LONG,LONG>{"N",3 ,0}
                
		CASE TypeCode.UInt16	// 0 - 65535 (2^16)
            result := Tuple<STRING,LONG,LONG>{"N",5 ,0}
                
		CASE TypeCode.UInt32		// 0 - 4294836225 (2^32)
            result := Tuple<STRING,LONG,LONG>{"N",10 ,0}
                
		CASE TypeCode.UInt64	// 0 - 18445618199572250625 (2^64)
			nLen := 20
            result := Tuple<STRING,LONG,LONG>{"N",nLen ,0}
			
		CASE TypeCode.DateTime
			cType   := "T"
			nLen 	:= 8
            result  := Tuple<STRING,LONG,LONG>{cType,nLen ,0}
			
		CASE TypeCode.Object
			LOCAL lIsDate := FALSE AS LOGIC
			LOCAL oMems AS MethodInfo[]
			LOCAL lFound := FALSE AS LOGIC
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
				cType   := "D"
				nLen 	:= 8
			ELSE
				cType 	:= "C"
				nLen 	:= 10
			ENDIF
            result := Tuple<STRING,LONG,LONG>{cType,nLen ,0}
			
		OTHERWISE
			cType := "C"
			nLen 	:= (Int32)schemaRow["ColumnSize"]
			IF nLen <= 0
				cType := "M"
                nLen  := 4
			ENDIF
            result := Tuple<STRING,LONG,LONG>{cType,nLen ,0}
			
        END SWITCH
        IF (LOGIC)schemaRow["AllowDBNull"]
            cType := result:Item1
            IF cType:Contains(":")
                cType += "0"
            ELSE
                cType += ":0"
            ENDIF
            result := Tuple<STRING,LONG,LONG>{cType,result:Item2 ,result:Item3}
            
        ENDIF
		RETURN result
        
END CLASS
