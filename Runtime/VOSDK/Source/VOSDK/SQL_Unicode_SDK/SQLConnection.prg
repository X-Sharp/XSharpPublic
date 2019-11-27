
//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING System.Data
USING System.Data.Common
USING System.Reflection
USING System.Diagnostics
USING System.ComponentModel
USING System.Runtime.ConstrainedExecution
USING System.Runtime.InteropServices
USING System.Collections.Generic

CLASS SqlConnection
	#region IVars
	// VO Compatible Ivars
	PROTECT cServer        AS STRING
	PROTECT cDSN            AS STRING
	PROTECT cUser          AS STRING
	PROTECT cAuthString    AS STRING
	PROTECT cDatabase      AS STRING
	PROTECT cDriver        AS STRING
	PROTECT cVersion       AS STRING
	PROTECT cConnectString AS STRING
	PROTECT oErrInfo       AS SQLErrorInfo
	PROTECT aStmts         AS List<SqlStatement>
	// New properties
	PROTECT cDataSourceName     AS STRING
	PROTECT cDataSourceVersion  AS STRING
	PROTECT lOrderByColumnsInSelect AS LOGIC
	PROTECT nGroupByBehavior       AS Int32
	PROTECT nIdentifierCase        AS Int32
	PROTECT nQuotedIdentifierCase  AS Int32
	PROTECT nSupportedJoinOperators as Int32
	
	// DotNet Specific Ivars
	PROTECT oNetConn         AS DbConnection
	PROTECT oTransaction     AS DbTransaction
	PRIVATE oFactory         AS ISqlFactory 
	#endregion
	#region static ivars
	
	STATIC PROTECT aInfoString  AS List<INT>
	STATIC PROTECT aInfoWord    AS List<INT>
	STATIC PROTECT aInfoDWord   AS List<INT>
	STATIC INTERNAL lConnErrMsg := TRUE    AS LOGIC   
	STATIC INTERNAL oDefConnection AS SqlConnection 
	
	#endregion
	
	#region Constructors and Destructors
	CONSTRUCTOR ( cConnStr, cUserID, cPassword )
        SELF:oFactory := GetSqlFactory()
		aStmts := List<SqlStatement>{}
		oErrInfo := SQLErrorInfo{}
		IF IsString( cUserID )
			SELF:cUser := cUserID
		ENDIF
		IF IsString( cPassword )
			SELF:cAuthString := cPassword
		ENDIF
		IF IsString( cConnStr )
			SELF:connect( cConnStr, cUser, cAuthString )
			SELF:_ParseConnectionString()
		ENDIF
	RETURN 
	
	DESTRUCTOR
		SELF:DisConnect()
		IF SELF:oNetConn != NULL_OBJECT
			SELF:oNetConn:Dispose()
			SELF:oNetConn := NULL_OBJECT
		ENDIF
	RETURN
	#endregion
	
	#region Transactions
	
	METHOD BeginTransaction() 
		LOCAL lOk AS LOGIC
		TRY
			lOk := FALSE
			IF SELF:oTransaction == NULL_OBJECT
				SELF:oTransaction := SELF:NetConn:BeginTransaction()
				lOk := TRUE
			ENDIF
		CATCH
			lOk := FALSE
		END TRY
		RETURN lOk
	
	METHOD EndTransaction() 
		LOCAL lOk AS LOGIC
		TRY
			lOk := FALSE
			IF SELF:oTransaction != NULL_OBJECT
				SELF:oFactory:BeforeCommit(SELF:oTransaction)
				SELF:oTransaction:Commit()
                SELF:oFactory:AfterCommit(SELF:oTransaction)
				SELF:oTransaction:Dispose()
				SELF:oTransaction := NULL_OBJECT
				lOk := TRUE
			ENDIF
		CATCH
			lOk := FALSE
		END TRY
		RETURN lOk
	
	
	METHOD Commit() 
		LOCAL lOk AS LOGIC
		TRY
			lOk := FALSE
			IF SELF:oTransaction != NULL_OBJECT
                SELF:oFactory:BeforeCommit(SELF:oTransaction)
				SELF:oTransaction:Commit()
                SELF:oFactory:AfterCommit(SELF:oTransaction)
				SELF:oTransaction:Dispose()
				SELF:oTransaction := NULL_OBJECT
				lOk := TRUE
				
			ENDIF
		CATCH
			lOk := FALSE
		END TRY
		RETURN lOk
	
	METHOD Rollback() 
		LOCAL lOk AS LOGIC
		TRY
			lOk := FALSE
			IF SELF:oTransaction != NULL_OBJECT
                SELF:oFactory:BeforeRollBack(SELF:oTransaction)
				SELF:oTransaction:Rollback()
                SELF:oFactory:AfterRollBack(SELF:oTransaction)
				SELF:oTransaction:Dispose()
				SELF:oTransaction := NULL_OBJECT
				lOk := TRUE
			ENDIF
		CATCH
			lOk := FALSE
		END TRY
		RETURN lOk
	#endregion
	
	#region Connect & Disconnect

	METHOD Connect( uConnStr, uUserID, uPassword ) 
		
		LOCAL lRet        AS LOGIC
		LOCAL ex          AS Exception
		LOCAL cConnStr AS STRING
		LOCAL cUserID AS STRING
		LOCAL cPassword  AS STRING
		DEFAULT( REF uConnStr, SELF:ConnectString )
		DEFAULT( REF uUserID, SELF:UserID )
		DEFAULT( REF uPassword, SELF:Password )
		cConnStr := (STRING) uConnStr
		cUserID  := (STRING) uUserID
		cPassword := (STRING) uPassword        
		IF !SELF:Connected
			cConnStr := oFactory:BeforeConnect(cConnStr, cUserID, cPassword)
			SELF:cConnectString := cConnStr
			SELF:cUser       := cUserID
			SELF:cAuthString := cPassword
			ex := SELF:NetConnect()
			IF ex != NULL .OR. !SELF:Connected
				oErrInfo := SQLErrorInfo{SELF, #Connect, ex}
				IF lConnErrMsg
					oErrInfo:ShowErrorMsg()
				ENDIF
			ELSE
				SELF:cDatabase   := NetConn:Database
				SELF:cServer     := NetConn:DataSource
				SELF:cVersion    := NetConn:ServerVersion
				oErrInfo:ErrorFlag := FALSE
				lRet := TRUE
				SELF:_ReadProperties()
			ENDIF
		ELSE
			SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CONNECTED ), #connect )
		ENDIF
		RETURN lRet

	METHOD NetConnect() AS Exception
		LOCAL ex := NULL AS Exception
		TRY
			LOCAL oBuilder AS DbConnectionStringBuilder
			oBuilder := Factory:CreateConnectionStringBuilder()
			oBuilder:ConnectionString := cConnectString
//			IF oBuilder IS MySQL.Data.MySqlClient.MySqlConnectionStringBuilder
//				LOCAL oBuilder2 AS MySQL.Data.MySqlClient.MySQLConnectionStringBuilder
//				oBuilder2 := (MySQL.Data.MySqlClient.MySQLConnectionStringBuilder) oBuilder
//				oBuilder2:AllowLoadLocalInfile := TRUE		
//			ENDIF
			SELF:NetConn:ConnectionString := oBuilder:ToString()
			SELF:NetConn:Open()
            SELF:oFactory:AfterConnect(SELF:NetConn)
		CATCH e AS Exception
			ex := e
			LOCAL w AS System.IO.TextWriter
			w := System.IO.File.AppendText("SQLNetLog.txt")
			w:Write(DToC(Today()) + " "+Time() + " " + e:Message+chr(13)+chr(10)+chr(13)+chr(10))
			w:Close()
		END TRY
		RETURN ex

	
	PROPERTY DataSource AS STRING GET cDSN      SET cDSN := value
	PROPERTY Server     AS STRING GET cServer   SET cServer     := value
	
	METHOD Disconnect() 
		LOCAL lRet      AS LOGIC
		
		FOREACH VAR oStmt IN aStmts:ToArray()
    		oStmt:Destroy()
		NEXT
		
		IF SELF:Connected 
			TRY
				SELF:Rollback()
                oFactory:BeforeDisConnect(SELF:NetConn)
				SELF:NetConn:Close()
				// A bug in MySql prevents the connections from being closed properly.
				// To properly close the connection it has to be removed from the connection pool
//				IF SELF:ProviderType == ProviderType.MySql
//					MySql.Data.MySqlClient.MySqlConnection.ClearPool((MySql.Data.MySqlClient.MySqlConnection)SELF:NetConn)
//				ENDIF
                oFactory:AfterDisConnect(SELF:NetConn)
				lRet := TRUE
			CATCH e AS Exception
				oErrInfo := SQLErrorInfo{SELF, #Disconnect,  e}
				lRet := FALSE
			END TRY
			
			IF lRet
				oErrInfo:ErrorFlag := FALSE
			ENDIF
		ELSE
			lRet := TRUE
		ENDIF
		
		UnRegisterAxit( SELF )
		
		RETURN TRUE
	
	
	
	
	METHOD DriverConnect( hWindow, nDriverCompletion, cConnStrIn )
        LOCAL cResult AS STRING
        LOCAL lRet    AS LOGIC
		cResult  := SELF:oFactory:DriverConnect(hWindow, nDriverCompletion, cConnStrIn)
        IF ! empty(cResult)
            SELF:cConnectString := cResult        
            LOCAL oBuilder AS DbConnectionStringBuilder
            oBuilder  := oFactory:CreateConnectionStringBuilder()
            oBuilder:ConnectionString := cResult
            IF oBuilder:ContainsKey("DataSource")
                cDSN := oBuilder["DataSource"]
            ELSEIF oBuilder:ContainsKey("DSN")
                cDSN := oBuilder["DSN"]
            ENDIF            
            IF oBuilder:ContainsKey("UID" )
                cUser := oBuilder["UID"]
            ENDIF
            IF oBuilder:ContainsKey( "PWD")
                cAuthString := oBuilder["PWD"]
            ENDIF
            IF oBuilder:ContainsKey( "Driver")
                cDriver := oBuilder["Driver"]
            ENDIF
        ENDIF
        IF !string.IsNullOrEmpty(cResult)
            TRY
                VAR oTempConn := SELF:oFactory:CreateConnection()
                oTempConn:ConnectionString := cResult
                oTempConn:Open()
                cServer         := oTempConn:DataSource
                cConnectString  := oTempConn:ConnectionString
                cDatabase       := oTempConn:Database
                
                cVersion        := oTempConn:ServerVersion
                IF SELF:oNetConn != NULL_OBJECT .AND. SELF:oNetConn:State == ConnectionState.Open
                    SELF:oNetConn:Close()
                    SELF:oNetConn:Dispose()
                ENDIF
                SELF:oNetConn   := oTempConn
                SELF:_ReadProperties()
                
            CATCH AS Exception
                lRet := FALSE
            END TRY
        ELSE
            lRet := FALSE
        ENDIF
		RETURN lRet
	
	METHOD Reconnect() 
		IF SELF:Connected
			SELF:Disconnect()
		ENDIF
		RETURN SELF:Connect()  
	
	#endregion
	
	#region .NET Extensions
	METHOD GetSchemaTable(cSchema AS STRING, aFilter AS STRING[]) AS DataTable
		LOCAL oTable AS DataTable
		LOCAL lFound AS LOGIC
		LOCAL nRestrictions AS LONG
		TRY
			SELF:oErrInfo:ErrorFlag := FALSE
			oTable := SELF:NetConn:GetSchema("MetadataCollections",NULL)
			FOREACH oRow AS DataRow IN oTable:Rows
				IF STRING.Compare( (STRING)oRow:Item["CollectionName"], cSchema, StringComparison.OrdinalIgnoreCase) == 0
					lFound := TRUE
					nRestrictions := (INT) oRow:Item["NumberOfRestrictions"] 
					EXIT
				ENDIF
			NEXT
			IF lFound
				// Adjust size of restrictions array
				IF aFilter != NULL .and. aFilter:Length != nRestrictions
					LOCAL aNewFilter AS STRING[]
					LOCAL nFilter AS INT
					LOCAL nMax    AS INT
					aNewFilter := STRING[]{nRestrictions}
					nMax := nRestrictions
					IF aFilter:Length < nRestrictions
						nMax := aFilter:Length
					ENDIF
					FOR nFilter := 1 TO nMax
						aNewFilter[nFilter] := aFilter[nFilter]
					NEXT
					aFilter := aNewFilter
				ENDIF
				oTable  := SELF:NetConn:GetSchema(cSchema , aFilter)
				RETURN oTable
			ENDIF
			SELF:__GenerateSqlError( "Collection "+cSchema+" is not supported by SQL Connection",#GetSchema)
		CATCH e AS Exception
			SELF:__GenerateSqlError( e:Message, #GetSchema )
		END TRY
		RETURN NULL
	
	METHOD GetSchema(cSchema AS STRING, aFilter AS STRING[]) AS SqlCatalogQuery
		LOCAL oSqlCatalog AS SqlCatalogQuery
		LOCAL oTable AS DataTable
		SELF:oErrInfo:ErrorFlag := FALSE
		TRY
			oTable := SELF:GetSchemaTable(cSchema, aFilter)
			IF oTable != NULL .and. ! SELF:oErrInfo:ErrorFlag
				oSqlCatalog  := SqlCatalogQuery{SELF}
				oSqlCatalog:_Open(oTable)
				oSqlCatalog:Execute()
				RETURN oSqlCatalog
			ENDIF
		CATCH AS Exception
			THROW 
		END TRY
		RETURN NULL_OBJECT
	
	
	METHOD _CreateConnection() AS DbConnection STRICT
		IF SELF:Factory == NULL
			THROW NullReferenceException{"Factory has not been set"}
		ENDIF
		LOCAL oConn AS DbConnection
		oConn := SELF:Factory:CreateConnection()
		RETURN oConn
	
	METHOD _CreateCommand() AS DbCommand STRICT
		IF SELF:Factory == NULL
			THROW NullReferenceException{"Factory has not been set"}
		ENDIF
		LOCAL oCmd AS DbCommand
		oCmd := SELF:Factory:CreateCommand()
		oCmd:Connection := SELF:NetConn
		oCmd:CommandTimeout := 0
		RETURN oCmd
	
	METHOD _CreateParameter() AS DbParameter STRICT
		IF SELF:Factory == NULL
			THROW NullReferenceException{"Factory has not been set. Use MySqlConnection, OracleConnection or MsSqlConnection class"}
		ENDIF
		LOCAL oPar AS DbParameter
		oPar := SELF:Factory:CreateParameter()
		RETURN oPar
	
	METHOD _CreateDataAdapter() AS DbDataAdapter STRICT
		IF SELF:Factory == NULL
			THROW NullReferenceException{"Factory has not been set. Use MySqlConnection, OracleConnection or MsSqlConnection class"}
		ENDIF
		LOCAL oDba AS DbDataAdapter
		oDba := SELF:Factory:CreateDataAdapter()
		RETURN oDba
	
	
	METHOD _ParseConnectionString() AS VOID
		LOCAL oBuilder AS DbConnectionStringBuilder
		oBuilder := Factory:CreateConnectionStringBuilder()
		IF ! SELF:cConnectString:Contains(";")
			RETURN
		ENDIF
		oBuilder:ConnectionString := SELF:cConnectString
		IF STRING.IsNullOrEmpty(SELF:cUser)
			IF oBuilder:ContainsKey("UID")
				cUser := oBuilder["UID"]
			ELSEIF oBuilder:ContainsKey("User Id")
				cUser := oBuilder["User ID"]
			ENDIF
		ENDIF
		IF STRING.IsNullOrEmpty(SELF:cAuthString)
			IF oBuilder:ContainsKey("PWD")
				cAuthString := oBuilder["PWD"]
			ELSEIF oBuilder:ContainsKey("Password")
				cAuthString := oBuilder["Password"]
			ENDIF
		ENDIF
		IF oBuilder:ContainsKey("Server")
			cServer := oBuilder["Server"]
		ENDIF
		IF oBuilder:ContainsKey("DSN")
			cDSN := oBuilder["DSN"]
		ENDIF
		IF oBuilder:ContainsKey("Database")
			cDatabase := oBuilder["Database"]
		ENDIF
		IF oBuilder:ContainsKey("Driver")
			cDriver := oBuilder["Driver"]
		ENDIF
		RETURN     
				   
	METHOD _ReadProperties AS VOID STRICT
		// Read Some values from the provider
		LOCAL oTable AS DataTable
		LOCAL oRow AS DataRow
		LOCAL oBuilder AS DbConnectionStringBuilder
		oTable := GetSchemaTable("DataSourceInformation",NULL)
		oRow := oTable:Rows[0]
		SELF:cDataSourceName            := (STRING) oRow["DataSourceProductName"]
		SELF:cDataSourceVersion         := (STRING) oRow["DataSourceProductVersion"]
		SELF:lOrderByColumnsInSelect    := (LOGIC)  oRow["OrderByColumnsInSelect"]
		SELF:nGroupByBehavior           := Convert.ToInt32(oRow["GroupByBehavior"])
		SELF:nIdentifierCase            := Convert.ToInt32(oRow["IdentifierCase"])
		SELF:nQuotedIdentifierCase      := Convert.ToInt32(oRow["QuotedIdentifierCase"])
		SELF:nSupportedJoinOperators    := Convert.ToInt32(oRow["SupportedJoinOperators"])
		oTable:Dispose()
		oBuilder := SELF:Factory:CreateConnectionStringBuilder()
		oBuilder:ConnectionString := SELF:NetConn:ConnectionString
		IF STRING.IsNullOrEmpty(SELF:cUser) 
			IF oBuilder:ContainsKey("UID")
				SELF:cUser := oBuilder["UID"]
			ELSEIF oBuilder:ContainsKey("UserId")
				SELF:cUser := oBuilder["UserId"]
			ENDIF
		ENDIF
		IF STRING.IsNullOrEmpty(SELF:cAuthString) 
			IF oBuilder:ContainsKey("PWD")
				SELF:cAuthString := oBuilder["PWD"]
			ELSEIF oBuilder:ContainsKey("PASSWORD")
				SELF:cAuthString := oBuilder["PASSWORD"]
			ENDIF
		ENDIF
		RETURN     
	#endregion
	
	#region Obsolete Properties
	[Obsolete];
	[DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PROPERTY IsolationOption  AS LONG GET 0 
	[Obsolete];
	[DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PROPERTY ODBCCursors AS LONG GET 0 
	[Obsolete];
	[DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PROPERTY PositionOps AS LOGIC  GET TRUE
	[Obsolete];
	[DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PROPERTY ScrollConcurrency AS LONG GET 0
	[Obsolete];
	[DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PROPERTY ScrollCsr AS LOGIC GET TRUE SET Today()
	[Obsolete];
	[DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PROPERTY ConnHandle AS IntPtr GET NULL
	
	#endregion
	
	#region Obsolete methods
	
	[Obsolete];
	METHOD GetConnectOption( nOption AS DWORD ) 
		//LOCAL oHandle AS OBJECT
		//LOCAL oMethodInfo AS MethodInfo
		//LOCAL oType       AS System.Type
		//LOCAL IMPLIED buffer := BYTE[]{256}
		//LOCAL IMPLIED params := OBJECT[]{3}
		//LOCAL length    AS LONG
		//LOCAL siRet     AS SHORTINT
		//LOCAL uRet   AS USUAL
		//oHandle := SELF:__GetConnectionHandle()
		//oType   := oHandle:GetType()
		//oMethodInfo := oType:GetMethod("GetConnectionAttribute",BindingFlags.Instance+ BindingFlags.NonPublic)
		//params[1] :=  (LONG) nOption
		//params[2] := buffer
		//params[3] := length
		//siRet := (SHORTINT) oMethodInfo:Invoke(oHandle, params)
		//SELF:oErrInfo:ErrorFlag := siRet < 0
		//IF nOption = SQL_OPT_TRACEFILE .OR. ;
		//    nOption = SQL_TRANSLATE_DLL .or. ;
		//    nOption == SQL_ATTR_TRANSLATE_LIB .or.;
		//    nOption == SQL_ATTR_CURRENT_CATALOG 
		//    LOCAL sRet AS STRING
		//    sRet := System.Text.Encoding.Unicode:GetString(buffer,0, (LONG) params[3])
		//    IF sRet:IndexOf('\0') >= 0
		//        sRet := Left(sRet, (DWORD) sRet:IndexOf('\0'))
		//    ENDIF
		//    uRet := sRet
		//ELSE
		//    uRet := BitConverter.ToInt32(buffer,0)
		//ENDIF
		//RETURN uRet
		RETURN NIL    
	
	
	METHOD Info( nInfoType AS WORD) 
		SWITCH nInfoType
        CASE SQL_DRIVER_NAME	// 6
            RETURN oFactory:GetName(oNetConn)
		CASE SQL_USER_NAME
			RETURN SELF:cUser
		CASE SQL_DATABASE_NAME	// 16
			RETURN SELF:cDatabase
		CASE SQL_DBMS_NAME		// 17
			RETURN SELF:cDataSourceName
		CASE SQL_DBMS_VER		// 18
			RETURN cVersion
        CASE SQL_IDENTIFIER_QUOTE_CHAR // 29
            RETURN oFactory:QuoteChar       
        OTHERWISE
		    IF aInfoDWord:Contains( nInfoType ) 
			    RETURN 0U
		    ELSEIF aInfoString:Contains(  nInfoType ) 
			    RETURN STRING.Empty
		    ELSEIF aInfoWord:Contains(  nInfoType ) 
			    RETURN (WORD) 0
			ENDIF
			
		END SWITCH
		
		RETURN NIL
	
	[Obsolete];
	METHOD isFunction( nFunction AS WORD ) 
		//LOCAL oHandle AS OBJECT
		//LOCAL oMethodInfo AS MethodInfo
		//LOCAL oType       AS System.Type
		//LOCAL IMPLIED params := OBJECT[]{2}
		//LOCAL fExists     AS SHORTINT
		//LOCAL siRet AS SHORTINT
		//oHandle := SELF:__GetConnectionHandle()
		//oType   := oHandle:GetType()
		//oMethodInfo := oType:GetMethod("GetFunctions",BindingFlags.Instance+ BindingFlags.NonPublic)
		//params[1] := (WORD)  nFunction
		//params[2] := fExists
		//siRet := (SHORTINT) oMethodInfo:Invoke(oHandle, params)
		//SELF:oErrInfo:ErrorFlag := (siRet < 0)
		//RETURN ((SHORTINT) params[2]) != 0
		RETURN TRUE    
	
	[Obsolete];
	METHOD SetConnectOption( nOption AS WORD, uValue AS USUAL) 
		//LOCAL oHandle AS OBJECT
		//LOCAL oMethodInfo AS MethodInfo
		//LOCAL oType       AS System.Type
		//LOCAL IMPLIED params := OBJECT[]{3}
		//LOCAL siRet     AS SHORTINT
		//oHandle := SELF:__GetConnectionHandle()
		//IF (oHandle != NULL)
		//oType   := oHandle:GetType()
		//IF IsString(uValue)
		//    oMethodInfo := oType:GetMethod("SetConnectionAttribute3",BindingFlags.Instance+ BindingFlags.NonPublic)
		//    params[2] := (STRING) uValue
		//    params[3] := (INT) (SLen(uValue)*2)
		//ELSE
		//    oMethodInfo := oType:GetMethod("SetConnectionAttribute2",BindingFlags.Instance+ BindingFlags.NonPublic)
		//    params[2] := (IntPtr) (LONG) uValue
		//    params[3] := 4
		//ENDIF
		//params[1] := (WORD)  nOption
		//siRet := (SHORTINT) oMethodInfo:Invoke(oHandle, params)
		//    SELF:oErrInfo:ErrorFlag := siRet < 0
		//RETURN TRUE
		//ENDIF
		//RETURN FALSE    
		RETURN FALSE    

	[Obsolete];
	METHOD __AllocConnect() AS LOGIC 
		RETURN TRUE
	
	
	[Obsolete];
	METHOD __AllocEnv() AS LOGIC 
		RETURN TRUE
	
	[Obsolete];
	METHOD __CheckActiveStmts() AS LOGIC 
		RETURN TRUE
	
	[Obsolete];
	METHOD __CheckIdentQuoteChar() AS LOGIC 
		RETURN TRUE
	
	[Obsolete];
	METHOD __CheckPositionOps() AS LOGIC 
		RETURN TRUE
	
	[Obsolete];
	METHOD __CheckQE() AS LOGIC 
		RETURN TRUE
	
	[Obsolete];
	METHOD __CheckScrollable() AS LOGIC 
		RETURN TRUE
	
	[Obsolete];
	METHOD __CloseExtraStmt(oStmt AS SQLStatement)  AS VOID 
		RETURN 
	[Obsolete];
	METHOD __Free() AS LOGIC 
		RETURN TRUE
	[Obsolete];
	METHOD __FreeConnect() AS LOGIC 
		RETURN TRUE
	[Obsolete];
	METHOD __FreeEnv () AS LOGIC
		RETURN TRUE
	#endregion
	
	METHOD __GenerateSqlError( cErrorString AS STRING, symMethod AS SYMBOL) AS SQLErrorInfo 
		SELF:oErrInfo:ErrorMessage := __CavoStr( __CAVOSTR_SQLCLASS__ODBC_VO ) +   ;
		Symbol2String( ClassName( SELF ) ) +   ;
		":" + Symbol2String( symMethod ) +    ;
		" " + cErrorString
		SELF:oErrInfo:NativeError := 0
		SELF:oErrInfo:SQLState    := __CavoStr( __CAVOSTR_SQLCLASS__GENERAL_ERR )
		SELF:oErrInfo:ErrorFlag   := TRUE
		SELF:oErrInfo:SubSystem   := __CavoStr( __CAVOSTR_SQLCLASS_SUBSYS )
		SELF:oErrInfo:MethodSelf  := SELF
		SELF:oErrInfo:FuncSym     := symMethod
		SELF:oErrInfo:CallFuncSym := symMethod
		SELF:oErrInfo:Severity    := ES_ERROR
		SELF:oErrInfo:ReturnCode  := SQL_SUCCESS
		RETURN oErrInfo
	
	METHOD __GetExtraStmt(cStmtText AS STRING) AS SqlStatement 
		LOCAL oNewStmt AS SqlStatement
		oNewStmt := SQLStatement{ cStmtText, SELF}
		RETURN oNewStmt
	
	METHOD __RegisterStmt( oStmt AS SQLStatement ) AS LOGIC 
		IF ! aStmts:Contains(oStmt)
			aStmts:Add(oStmt)
		ENDIF
		RETURN TRUE
	
	METHOD __UnregisterStmt( oStmt AS SQLStatement ) AS LOGIC
        LOCAL lRet := FALSE AS LOGIC
		IF aStmts:Contains(oStmt)
            aStmts:Remove(oStmt)
            lRet := TRUE
		ENDIF
		RETURN lRet
	
	
	#region Properties
	PROPERTY DataSourceName                         AS STRING GET cDataSourceName
	PROPERTY DataSourceVersion                      AS STRING GET SELF:cDataSourceVersion
	PROPERTY OrderByColumnsInSelect                 AS LOGIC GET SELF:lOrderByColumnsInSelect
	PROPERTY GroupByBehavior                        AS GroupByBehavior GET (GroupByBehavior) nGroupByBehavior
	PROPERTY IdentifierCase                         AS IdentifierCase GET (IdentifierCase) nIdentifierCase
	PROPERTY QuotedIdentifierCase                   AS IdentifierCase GET (IdentifierCase) nQuotedIdentifierCase
	PROPERTY SupportedJoinOperators                 AS SupportedJoinOperators GET (SupportedJoinOperators) nSupportedJoinOperators
	
	PROPERTY AccessMode                             AS LONG GET SQL_MODE_READ_WRITE

	PROPERTY NetConn AS DbConnection 
		GET
			IF SELF:oNetConn == NULL_OBJECT
				SELF:oNetConn := SELF:_CreateConnection()
			ENDIF
			RETURN oNetConn
		END GET
    END PROPERTY
    
	PROPERTY Connected AS LOGIC
		GET 
			IF SELF:oNetConn != NULL_OBJECT
				RETURN SELF:oNetConn:State == ConnectionState.Open
			ENDIF
			RETURN FALSE
		END GET
	END PROPERTY
	
	PROPERTY ConnectionHandle AS DbConnection GET SELF:NetConn
	PROPERTY ConnectString AS STRING 
		GET 
			RETURN SELF:cConnectString 
		END GET
		SET 
			SELF:DisConnect()
			SELF:cConnectString := VALUE
			SELF:_ParseConnectionString()
		END SET
	END PROPERTY
	ACCESS ErrInfo 
		IF oErrInfo:ErrorFlag
			RETURN oErrInfo
		ENDIF
		RETURN NIL
	
	ACCESS HyperLabel 
		LOCAL oHL AS HyperLabel
		IF SLen( SELF:cDatabase ) > 0
			oHL := HyperLabel{  cDatabase, ;
			cDatabase, ;
			Symbol2String( ClassName( SELF )  )+ ": " + cDatabase, ;
			Symbol2String( ClassName( SELF ) )+ "_" + cDatabase  }
		ENDIF
		RETURN oHL

	
	PROPERTY IdentifierQuoteChar AS STRING GET oFactory:QuoteChar
	
	PROPERTY Password   AS STRING
		GET 
			RETURN SELF:cAuthString
		END GET
		SET 
			IF  !SELF:Connected
				SELF:cAuthString := VALUE
			ELSE
				SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CONNECTED ), #Password )
				oErrInfo:Throw()
			ENDIF
		END SET
	END PROPERTY
	
	PROPERTY Factory    AS ISqlFactory 
		GET
			RETURN oFactory
		END GET
	END PROPERTY
	
	PROPERTY Status AS HyperLabel
		GET
			LOCAL oStatus AS HyperLabel
			 
			IF oErrInfo:ErrorFlag
				oStatus := HyperLabel{  oErrInfo:FuncSym,  ;
				oErrInfo:SQLState, ;
				Symbol2String( ClassName( oErrInfo:MethodSelf ) ) + ": " + ;
				oErrInfo:ErrorMessage }
			ENDIF
			RETURN oStatus
		END GET
	END PROPERTY


	PROPERTY UserID AS STRING
		GET 
			RETURN SELF:cUser
		END GET
		SET 
			IF !SELF:Connected
				SELF:cUser := value
			ELSE
				SELF:__GenerateSqlError( __CavoStr( __CAVOSTR_SQLCLASS__CONNECTED ), #UserID )
				oErrInfo:Throw()
			ENDIF
			RETURN 
		END SET
	END PROPERTY
	

	#endregion
	
	STATIC METHOD GetODBCDataSources AS ARRAY
		LOCAL aSources  AS ARRAY
		LOCAL oEnum     AS DbDataSourceEnumerator
		LOCAL oTable    AS DataTable
		LOCAL oConn     AS SqlConnection
		oConn := SqlConnection{}
		aSources := {}
		IF oConn:Factory:CanCreateDataSourceEnumerator
			oEnum := oConn:Factory:CreateDataSourceEnumerator()
			oTable := oEnum:GetDataSources()
			FOREACH IMPLIED oRow IN oTable:Rows
				LOCAL oR := oRow AS DataRow
				aadd(aSources, oR[0])
			NEXT
		ELSE
			LOCAL oKey AS Microsoft.Win32.RegistryKey
			LOCAL aNames AS STRING[]
			oKey := Microsoft.Win32.Registry.CurrentUser
			oKey := oKey:OpenSubKey("Software\ODBC\ODBC.INI\ODBC Data Sources")
            IF oKey != NULL
			    aNames := oKey:GetValueNames()
			    FOREACH IMPLIED sName IN aNames
				    aadd(aSources, sName)
			    NEXT
			    oKey:Close()
            ENDIF
			oKey := Microsoft.Win32.Registry.LocalMachine
			oKey := oKey:OpenSubKey("Software\ODBC\ODBC.INI\ODBC Data Sources")
            IF oKey != NULL
			    aNames := oKey:GetValueNames()
			    FOREACH IMPLIED sName IN aNames
				    IF ascan(aSources, sName) == 0
					    aadd(aSources, sName)
				    ENDIF
                NEXT
			oKey:Close()
            ENDIF
		ENDIF
		
		RETURN aSources
	
	
	/*
	STATIC METHOD OpenConnection()                  AS SQLConnection
	
	//
	// Displays a dialog box that displays a list of available ODBC data sources.
	// Note: if only one ODBC driver is installed on the system, it immediately
	//  opens it without prompting the developer.
	// returns an SQLConnection object
	//
	LOCAL oConn              AS SQLConnection
	oConn := SQLConnection{}
	oConn:DriverConnect()
	RETURN oConn
	
	*/    
	STATIC INTERNAL METHOD SetConnection( oNewConnection := NULL ) AS SQLConnection STRICT
		LOCAL oDefConn      AS SQLConnection
		oDefConn    := SqlConnection.oDefConnection
		IF oNewConnection != NULL
			SqlConnection.oDefConnection := oNewConnection            
		ENDIF
		RETURN oDefConn
	
	
	STATIC CONSTRUCTOR 
		// Initialize
		aInfoString := List<INT>{}{     ;
		SQL_ACCESSIBLE_PROCEDURES,      ;
		SQL_ACCESSIBLE_TABLES,          ;
		SQL_CATALOG_NAME,               ;
		SQL_CATALOG_NAME_SEPARATOR,     ;
		SQL_CATALOG_TERM,               ;
		SQL_COLLATION_SEQ,              ;
		SQL_COLUMN_ALIAS,               ;
		SQL_DATA_SOURCE_NAME,           ;
		SQL_DATA_SOURCE_READ_ONLY,      ;
		SQL_DATABASE_NAME,              ;
		SQL_DBMS_NAME,                  ;
		SQL_DBMS_VER,                   ;
		SQL_DESCRIBE_PARAMETER,         ;
		SQL_DM_VER,                     ;
		SQL_DRIVER_NAME,                ;
		SQL_DRIVER_ODBC_VER,            ;
		SQL_DRIVER_VER,                 ;
		SQL_EXPRESSIONS_IN_ORDERBY,     ;
		SQL_IDENTIFIER_QUOTE_CHAR,      ;
		SQL_INTEGRITY,                  ;
		SQL_KEYWORDS,                   ;
		SQL_LIKE_ESCAPE_CLAUSE,         ;
		SQL_MAX_ROW_SIZE_INCLUDES_LONG, ;
		SQL_MULT_RESULT_SETS,         ;
		SQL_MULTIPLE_ACTIVE_TXN,        ;
		SQL_NEED_LONG_DATA_LEN,         ;
		SQL_ODBC_SQL_OPT_IEF,           ;
		SQL_ODBC_VER,                   ;
		SQL_ORDER_BY_COLUMNS_IN_SELECT, ;
		SQL_OUTER_JOINS,                ;
		SQL_OWNER_TERM,                 ;
		SQL_PROCEDURE_TERM,             ;
		SQL_PROCEDURES,                 ;
		SQL_QUALIFIER_TERM,             ;
		SQL_QUALIFIER_NAME_SEPARATOR,   ;
		SQL_ROW_UPDATES,                ;
		SQL_SCHEMA_TERM,                ;
		SQL_SEARCH_PATTERN_ESCAPE,      ;
		SQL_SERVER_NAME,                ;
		SQL_SPECIAL_CHARACTERS,         ;
		SQL_TABLE_TERM,                 ;
		SQL_USER_NAME ,                  ;
		SQL_XOPEN_CLI_YEAR}
		
		aInfoWord := List<INT>{}{                   ;
		SQL_AGGREGATE_FUNCTIONS,         ;
		SQL_ACTIVE_ENVIRONMENTS,         ;
		SQL_CATALOG_LOCATION,;
		SQL_CONCAT_NULL_BEHAVIOR,;
		SQL_CORRELATION_NAME,;
		SQL_CURSOR_COMMIT_BEHAVIOR,     ;
		SQL_CURSOR_ROLLBACK_BEHAVIOR,   ;
		SQL_FILE_USAGE,;
		SQL_GROUP_BY,;
		SQL_IDENTIFIER_CASE,            ;
		SQL_MAX_CATALOG_NAME_LEN,;
		SQL_MAX_COLUMN_NAME_LEN,        ;
		SQL_MAX_COLUMNS_IN_GROUP_BY,;
		SQL_MAX_COLUMNS_IN_INDEX,;
		SQL_MAX_COLUMNS_IN_SELECT,;
		SQL_MAX_COLUMNS_IN_TABLE,;
		SQL_MAX_CONCURRENT_ACTIVITIES,;
		SQL_MAX_CURSOR_NAME_LEN,        ;
		SQL_MAX_DRIVER_CONNECTIONS,;
		SQL_MAX_IDENTIFIER_LEN,;
		SQL_MAX_PROCEDURE_NAME_LEN,     ;
		SQL_MAX_SCHEMA_NAME_LEN,;
		SQL_MAX_TABLE_NAME_LEN, ;
		SQL_MAX_TABLES_IN_SELECT,;
		SQL_MAX_USER_NAME_LEN,;
		SQL_NON_NULLABLE_COLUMNS,;
		SQL_NULL_COLLATION,;
		SQL_QUOTED_IDENTIFIER_CASE,;
		SQL_TXN_CAPABLE ;
		}
		
		aInfoDWord  := List<INT>{}{        ;
		SQL_ACTIVE_CONNECTIONS,          ;
		SQL_ACTIVE_STATEMENTS,           ;
		SQL_ALTER_DOMAIN,                ;
		SQL_ALTER_TABLE,                 ;
		SQL_ASYNC_MODE,                  ;
		SQL_BATCH_ROW_COUNT ,;
		SQL_BATCH_SUPPORT,;
		SQL_BOOKMARK_PERSISTENCE,;
		SQL_CATALOG_USAGE,;
		SQL_CONVERT_BIGINT   ,;
		SQL_CONVERT_BINARY   ,;
		SQL_CONVERT_BIT      ,;
		SQL_CONVERT_CHAR     ,;
		SQL_CONVERT_DATE     ,;
		SQL_CONVERT_DECIMAL  ,;
		SQL_CONVERT_DOUBLE   ,;
		SQL_CONVERT_FLOAT    ,;
		SQL_CONVERT_INTEGER  ,;
		SQL_CONVERT_INTERVAL_DAY_TIME ,;
		SQL_CONVERT_INTERVAL_YEAR_MONTH,;
		SQL_CONVERT_LONGVARBINARY,;
		SQL_CONVERT_LONGVARCHAR,;
		SQL_CONVERT_NUMERIC    ,;
		SQL_CONVERT_REAL       ,;
		SQL_CONVERT_SMALLINT   ,;
		SQL_CONVERT_TIME       ,;
		SQL_CONVERT_TIMESTAMP  ,;
		SQL_CONVERT_TINYINT    ,;
		SQL_CONVERT_VARBINARY  ,;
		SQL_CONVERT_VARCHAR    ,;
		SQL_CONVERT_INTERVAL_YEAR_MONTH,;
		SQL_CONVERT_WLONGVARCHAR   ,;
		SQL_CONVERT_WVARCHAR ,;
		SQL_CONVERT_FUNCTIONS,;
		SQL_CREATE_ASSERTION,;
		SQL_CREATE_CHARACTER_SET,;
		SQL_CREATE_COLLATION,;
		SQL_CREATE_DOMAIN,;
		SQL_CREATE_SCHEMA,;
		SQL_CREATE_TABLE,;
		SQL_CREATE_TRANSLATION,;
		SQL_CREATE_VIEW,;
		SQL_DATETIME_LITERALS,;
		SQL_DDL_INDEX,;
		SQL_DEFAULT_TXN_ISOLATION,;
		SQL_DRIVER_HDBC,;
		SQL_DRIVER_HENV,;
		SQL_DRIVER_HLIB,;
		SQL_DRIVER_HSTMT,;
		SQL_DROP_ASSERTION,;
		SQL_DROP_CHARACTER_SET,;
		SQL_DROP_COLLATION,;
		SQL_DROP_DOMAIN,;
		SQL_DROP_SCHEMA,;
		SQL_DROP_TABLE,;
		SQL_DROP_TRANSLATION,;
		SQL_DROP_VIEW,;
		SQL_DYNAMIC_CURSOR_ATTRIBUTES1,;
		SQL_DYNAMIC_CURSOR_ATTRIBUTES2,;
		SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1,;
		SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2,;
		SQL_GETDATA_EXTENSIONS,;
		SQL_INDEX_KEYWORDS,;
		SQL_INFO_SCHEMA_VIEWS,;
		SQL_INSERT_STATEMENT,;
		SQL_KEYSET_CURSOR_ATTRIBUTES1,;
		SQL_KEYSET_CURSOR_ATTRIBUTES2,;
		SQL_MAX_ASYNC_CONCURRENT_STATEMENTS,;
		SQL_MAX_BINARY_LITERAL_LEN,;
		SQL_MAX_CHAR_LITERAL_LEN,;
		SQL_MAX_INDEX_SIZE,;
		SQL_MAX_OWNER_NAME_LEN,         ;
		SQL_MAX_QUALIFIER_NAME_LEN,     ;
		SQL_MAX_ROW_SIZE,;
		SQL_MAX_STATEMENT_LEN,;
		SQL_NUMERIC_FUNCTIONS,;
		SQL_ODBC_API_CONFORMANCE,       ;
		SQL_ODBC_INTERFACE_CONFORMANCE,;
		SQL_ODBC_SAG_CLI_CONFORMANCE,   ;
		SQL_ODBC_SQL_CONFORMANCE,       ;
		SQL_OJ_CAPABILITIES,;
		SQL_PARAM_ARRAY_ROW_COUNTS,;
		SQL_PARAM_ARRAY_SELECTS,;
		SQL_POS_OPERATIONS,;
		SQL_SCHEMA_USAGE,;
		SQL_SCROLL_OPTIONS,;
		SQL_SQL_CONFORMANCE,;
		SQL_SQL92_DATETIME_FUNCTIONS,;
		SQL_SQL92_FOREIGN_KEY_DELETE_RULE,;
		SQL_SQL92_FOREIGN_KEY_UPDATE_RULE,;
		SQL_SQL92_GRANT,;
		SQL_SQL92_NUMERIC_VALUE_FUNCTIONS,;
		SQL_SQL92_PREDICATES,;
		SQL_SQL92_RELATIONAL_JOIN_OPERATORS,;
		SQL_SQL92_REVOKE,;
		SQL_SQL92_ROW_VALUE_CONSTRUCTOR,;
		SQL_SQL92_STRING_FUNCTIONS,;
		SQL_SQL92_VALUE_EXPRESSIONS,;
		SQL_STANDARD_CLI_CONFORMANCE ,;
		SQL_STATIC_CURSOR_ATTRIBUTES1,;
		SQL_STATIC_CURSOR_ATTRIBUTES2,;
		SQL_STRING_FUNCTIONS,;
		SQL_SUBQUERIES,;
		SQL_SYSTEM_FUNCTIONS,;
		SQL_TIMEDATE_ADD_INTERVALS,;
		SQL_TIMEDATE_DIFF_INTERVALS,;
		SQL_TIMEDATE_FUNCTIONS,;
		SQL_TXN_ISOLATION_OPTION,;
		SQL_UNION}
		
	RETURN
	
	// Wird nicht geloggt, das Logging ist in der Schicht drüber
	METHOD DoSimpleSelect(cSelect AS STRING)  AS USUAL
	// Schickt ein SQL-Select an den Server und gibt das erste Feld zurück
	LOCAL oRetVal AS OBJECT
	LOCAL uRetVal AS USUAL
	LOCAL oCmd AS DbCommand
	TRY
		oCmd := SELF:_CreateCommand()
		oCmd:CommandText := cSelect
		oCmd:Connection := SELF:ConnectionHandle
		oRetVal := oCmd:ExecuteScalar()
		uRetVal := oFactory:HandleSpecialValue(oRetVal,NULL , FALSE)
	CATCH
		uRetVal := NIL
	END TRY
	IF oCmd != NULL
		oCmd:Dispose()
	ENDIF

	RETURN uRetVal

	
	
END CLASS


#region Obsolete Functions

[Obsolete];
FUNCTION SQLDropMyConnection( cMySourceName, cMyUserID, cMyPassword ) AS LOGIC
	RETURN TRUE

[Obsolete];
FUNCTION GetMyConnection( cMySourceName, cMyUserID, cMyPassword )
	RETURN NULL_OBJECT

[Obsolete];
FUNCTION SQLOpenConnection()  AS SQLConnection
	LOCAL oConn              AS SQLConnection
	oConn := SQLConnection{}
	oConn:DriverConnect()
	RETURN oConn
	


#endregion

FUNCTION SQLSetConnection( oSQLConnection ) AS SQLConnection
	RETURN SqlConnection.SetConnection(oSQLConnection)

FUNCTION SQLGetDataSources() AS ARRAY
	RETURN SqlConnection.GetODBCDataSources()


FUNCTION SQLConnectErrorMsg( lValue ) AS LOGIC
	DEFAULT( @lValue, FALSE )
	SqlConnection.lConnErrMsg := lValue
	RETURN SqlConnection.lConnErrMsg



