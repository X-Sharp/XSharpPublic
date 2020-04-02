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
    PROTECT _oLastDataReader AS DbDataReader
    PROTECT _nextCursorNo    AS LONG
    
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
    PROPERTY ODBChdbc           AS DbConnection GET SELF:Connection:NetConnection
    PROPERTY ODBChstmt          AS DbCommand    GET _oNetCommand
    PROPERTY DisconnectRollback AS LOGIC AUTO GET SET
    PROPERTY DispWarnings       AS LOGIC AUTO GET SET
    PROPERTY IdleTimeout        AS LONG  AUTO GET SET
    PRIVATE _PacketSize         AS LONG
    PROPERTY PacketSize         AS LONG
        GET
            IF SQLReflection.GetPropertyValue(SELF:Connection:NetConnection, "PacketSize", OUT VAR result)
                RETURN (LONG) result
            ENDIF
            RETURN _PacketSize
            
        END GET
        SET
            _PacketSize := value
            SQLReflection.SetPropertyValue(SELF:Connection:NetConnection, "PacketSize", value)
            RETURN

    END SET
    END PROPERTY
    PROPERTY QueryTimeOut       AS LONG  GET _oNetCommand:CommandTimeout SET _oNetCommand:CommandTimeout := Value
    PROPERTY TransactionMode    AS LONG  AUTO GET SET
    PROPERTY WaitTime           AS LONG  AUTO GET SET
    PROPERTY Prepared           AS LOGIC AUTO GET PRIVATE SET
    PROPERTY CursorName         AS STRING AUTO GET SET


    METHOD SetDefaults() AS VOID
        SELF:Shared             := FALSE
        SELF:Asynchronous       := FALSE
        SELF:BatchMode          := TRUE
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
        IF ALen(tables) > 0
            RETURN (LONG) ALen(tables)
        ENDIF
        RETURN 0
        
    METHOD Execute(aInfo AS ARRAY) AS LONG
        VAR tables := SELF:CopyToCursor()
        CopyToInfo(tables, aInfo)
        IF ALen(tables) > 0
            RETURN (LONG) ALen(tables)
        ENDIF
        RETURN 0

    PRIVATE METHOD CopyToInfo(aResult, aInfo AS ARRAY) AS VOID
        ASize(aInfo, ALen(aResult))
        ACopy(aResult, aInfo)
        RETURN

    
    METHOD CopyToCursor() AS ARRAY
        VAR oDataReader := SELF:_oNetCommand:ExecuteReader()
        RETURN CopyToCursor(oDataReader, 0)

    METHOD CopyToCursor(oDataReader AS DbDataReader, cursorNo AS LONG) AS ARRAY
        LOCAL result   := {} AS ARRAY
        _oLastDataReader := NULL_OBJECT
        DO WHILE TRUE
            VAR cursorName := SELF:CursorName
            IF cursorNo != 0
                cursorName += cursorNo:ToString()
            ENDIF
    		oDataReader := SELF:Connection:Factory:AfterOpen(oDataReader)
            VAR oSchema := oDataReader:GetSchemaTable()
            CreateWorkarea(oSchema, oDataReader, cursorName)
            AAdd(result, {cursorName, RecCount()})
            cursorNo += 1
            IF ! SELF:BatchMode
                _oLastDataReader := oDataReader
                _nextCursorNo    := cursorNo
                EXIT
            ENDIF
            oDataReader:NextResult()
        ENDDO
        RETURN result

    METHOD MoreResults(cursorName AS STRING, aInfo AS ARRAY) AS LONG
        IF !String.IsNullOrEmpty(cursorName)
            SELF:CursorName := cursorName
            SELF:_nextCursorNo := 0
        ENDIF
        IF _oLastDataReader != NULL .AND. _oLastDataReader:NextResult()
            VAR tables := CopyToCursor(_oLastDataReader, _nextCursorNo)
            CopyToInfo(tables, aInfo)
            IF ALen(tables) > 0
                RETURN (LONG) ALen(tables)
            ENDIF
        ENDIF
        RETURN 0
    

    METHOD GetNumRestrictions(cCollectionName AS STRING) AS LONG
        LOCAL nRestrictions := 0 AS LONG
        LOCAL oTable := SELF:Connection:NetConnection:GetSchema("MetadataCollections",NULL) AS DataTable
        FOREACH oRow AS DataRow IN oTable:Rows
			IF String.Compare( (STRING)oRow:Item["CollectionName"], cCollectionName, StringComparison.OrdinalIgnoreCase) == 0
				nRestrictions := (INT) oRow:Item["NumberOfRestrictions"] 
				EXIT
			ENDIF
        NEXT        
        RETURN nRestrictions  

    METHOD GetTables(cType AS STRING, cCursorName AS STRING) AS LOGIC
        SELF:CursorName := cCursorName
        VAR types := cType:Split(",":ToCharArray(),StringSplitOptions.RemoveEmptyEntries)
        VAR list  := List<STRING>{}
        FOREACH VAR type IN types
            VAR sType := type
            IF sType:StartsWith("'")
                sType := sType:Replace("'","")
            ELSEIF sType:StartsWith(e"\"")
                sType := sType:Replace(e"\"","")
            ENDIF
            list:Add(sType)
        NEXT

        
        LOCAL filter AS STRING[]
        LOCAL oTables AS List<DataTable>
        LOCAL oTable AS DataTable
        LOCAL nRestrictions AS LONG
        oTables := List<DataTable>{}
        
        nRestrictions := GetNumRestrictions("Tables") 
        filter := STRING[]{nRestrictions}
        oTable := SELF:Connection:NetConnection:GetSchema("Tables", filter)
        oTables:Add(oTable)
        
        nRestrictions := GetNumRestrictions("Views")
        filter := STRING[]{nRestrictions}
        oTable := SELF:Connection:NetConnection:GetSchema("Views", filter)
        oTables:Add(oTable)
        
        LOCAL aStruct AS ARRAY
        aStruct := ArrayNew(5)
        aStruct[1] := {"TABLE_CAT","C:0",128,0}
        aStruct[2] := {"TABLE_SCHEM","C:0",128,0}
        aStruct[3] := {"TABLE_NAME","C:0",128,0}
        aStruct[4] := {"TABLE_TYPE","C:0",32,0}
        aStruct[5] := {"REMARKS","C:0",254,0}
        VAR cTemp := System.IO.Path.GetTempFileName()
        DbCreate(cTemp, aStruct, "DBFVFP")
        VAR nArea := _SelectString(cCursorName)
        IF nArea != 0
            DbCloseArea()
        ENDIF
        VoDbUseArea(TRUE, "DBFVFPSQL",cTemp,cCursorName,FALSE,FALSE)
        LOCAL oRDD AS IRdd
        oRDD := (IRdd) DbInfo(DbInfo.DBI_RDD_OBJECT)
        
        LOCAL nCol := ((DataColumn) oTable:Columns["Table_Type"]):Ordinal
        FOREACH VAR oT IN oTables
            FOREACH oRow AS DataRow IN oT:Rows
                LOCAL match AS LOGIC
                VAR data := oRow:ItemArray
                IF list:Count == 0
                    match := TRUE
                ELSE
                    match := FALSE
                    LOCAL element := (STRING) data[nCol+1] AS STRING
                    FOREACH VAR type IN list
                        IF String.Compare(element, type, TRUE) == 0
                            match := TRUE
                            EXIT
                        ENDIF
                    NEXT
                ENDIF
                IF match
                    oRDD:Append(TRUE)
                    FOR VAR nFld := 1 TO 5
                        oRDD:PutValue(nFld, data[nFld])
                    NEXT
                ENDIF
            NEXT
       NEXT
       RETURN TRUE        
            

    
    STATIC METHOD CreateWorkarea(oSchema AS DataTable, oDataReader AS DbDataReader, cCursorName AS STRING) AS LOGIC
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
        VAR nArea := _SelectString(cCursorName)
        IF nArea != 0
            DbCloseArea()
        ENDIF
        VoDbUseArea(TRUE, "DBFVFPSQL",cTemp,cCursorName,FALSE,FALSE)
        LOCAL oRDD AS IRdd
        oRDD := (IRdd) DbInfo(DbInfo.DBI_RDD_OBJECT)
        LOCAL oMIGet := NULL AS MethodInfo 
        // DBFVFPSQL has a method SetData to set all the values of the current row.
        oMIGet := oRDD:GetType():GetMethod("GetData", BindingFlags.Instance+BindingFlags.IgnoreCase+BindingFlags.Public)
        IF oMIGet != NULL
            VAR GetData := (SqlGetData) oMIGet:CreateDelegate(typeof(SqlGetData), oRDD) 
            DO WHILE oDataReader:Read()
                oRDD:Append(TRUE)
                // Get the data array from the workarea
                VAR data := GetData()
                // and fetch its values. This automatically updates the array that is owned by the RDD
                oDataReader:GetValues(data)
            ENDDO
        ELSE
            VAR data := OBJECT[]{oSchema:Rows:Count+1}  // 1 extra for the NullFlags
            DO WHILE oDataReader:Read()
                oRDD:Append(TRUE)
                // use our local data array
                oDataReader:GetValues(data)
                // and write the values to the RDD
                FOR VAR nFld := 1 UPTO nFields
                    oRDD:PutValue(nFld, data[nFld])
                NEXT
            ENDDO
        ENDIF
        RETURN TRUE
   



    STATIC METHOD SchemaRowToFieldInfo(schemaRow AS DataRow,aFieldNames AS IList<STRING>) AS ARRAY
        VAR cColumnName := schemaRow["ColumnName"]:ToString( )
        VAR fieldType  := DotNetType2DbfType(schemaRow)
        VAR cFldName   := SQLSupport.CleanupColumnName(cColumnName)
	    cFldName	   := MakeFieldNameUnique(cFldName, aFieldNames)
        RETURN {cFldName, fieldType:Item1, fieldType:Item2,fieldType:Item3, cColumnName}

    STATIC METHOD MakeFieldNameUnique(cName AS STRING, aFldNames AS IList<STRING> ) AS STRING
		LOCAL dwPos, dwFld AS LONG
		LOCAL cNewname		 AS STRING
		IF Empty(cName)
			dwFld := 0
			dwPos := 1
			DO WHILE dwPos >= 0
				++dwFld
				cName := "FLD"+StrZero(dwFld,3,0)
				dwPos := aFldNames:IndexOf(cName:ToUpper())
			ENDDO
		ELSE
			// remove column prefixes
			dwPos := cName:IndexOf(".")+1
			IF dwPos > 0
				cName := cName:Substring(dwPos)
			ENDIF
			// remove embedded spaces
			cName 	:= StrTran(cName, " ", "_"):ToUpper()
			cNewname := Left(cName,10)
			dwFld 	:= 1
			DO WHILE aFldNames:IndexOf(cNewname) >= 0
				++dwFld
                VAR tmp := dwFld:ToString()
				cNewname := cName:Substring(0, 10 - tmp:Length)+tmp
			ENDDO
			cName 	:= cNewname
		ENDIF
		aFldNames:Add(cName)
		RETURN cName

            
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
    DELEGATE SqlGetData() AS OBJECT[]
        
END CLASS
