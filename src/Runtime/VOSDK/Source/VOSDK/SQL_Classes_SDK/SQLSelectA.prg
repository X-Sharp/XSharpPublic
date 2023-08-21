
PARTIAL CLASS SQLSelect




/// <include file="SQL.xml" path="doc/SQLSelect.AppendFlag/*" />
ACCESS AppendFlag 


	RETURN SELF:lAppendFlag


/// <include file="SQL.xml" path="doc/SQLSelect.AppendFlag/*" />
ASSIGN AppendFlag( lSetAppendFlag ) 


	IF IsLogic( lSetAppendFlag )
		SELF:lAppendFlag := lSetAppendFlag
	ENDIF


	RETURN SELF:lAppendFlag


/// <include file="SQL.xml" path="doc/SQLSelect.BoF/*" />
ACCESS BoF 
 	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:BOF returns " + AsString(lBof) )
	#ENDIF
   IF SELF:lAppendFlag
      RETURN FALSE
   ENDIF
	RETURN SELF:lBof


/// <include file="SQL.xml" path="doc/SQLSelect.BoundedColumns/*" />
ACCESS BoundedColumns() 


	RETURN SELF:nMaxBindColumn


/// <include file="SQL.xml" path="doc/SQLSelect.Connection/*" />
ACCESS Connection 


	RETURN SELF:oConn


/// <include file="SQL.xml" path="doc/SQLSelect.CursorName/*" />
ACCESS CursorName 
	LOCAL   cRet    AS STRING


	IF( SELF:__GetCursorName() )
		cRet := SELF:cCursor
	ENDIF
	RETURN cRet


/// <include file="SQL.xml" path="doc/SQLSelect.CursorName/*" />
ASSIGN CursorName( cCursorName ) 
	LOCAL   cRet AS STRING


	IF( SELF:__SetCursorName( cCursorName ) )
		cRet := SELF:cCursor
	ENDIF


	RETURN cRet


/// <include file="SQL.xml" path="doc/SQLSelect.CursorType/*" />
ACCESS CursorType 


	RETURN SELF:oStmt:CursorType


/// <include file="SQL.xml" path="doc/SQLSelect.CursorType/*" />
ASSIGN CursorType( nVal ) 


	RETURN SELF:oStmt:CursorType := nVal


/// <include file="SQL.xml" path="doc/SQLSelect.DBStruct/*" />
ACCESS DBStruct 
	LOCAL aStruct       AS ARRAY
	LOCAL nIndex        AS DWORD
	LOCAL oCol			  AS SQLColumn
	LOCAL oFs			  AS FieldSpec
	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN {}
	ENDIF


	// create an array for the dbstructs
	aStruct := ArrayNew( SELF:nNumCols )


	// fill it...
	FOR nIndex := 1 TO SELF:nNumCols
		oCol := SELF:aSQLColumns[nIndex]
		oFs := oCol:FieldSpec
		aStruct[nIndex] :=    ;
			{  oCOl:ColName,   ;
				OFS:ValType,    ;
				OFS:Length,     ;
				OFS:Decimals    ;
			}
	NEXT


	RETURN aStruct


/// <include file="SQL.xml" path="doc/SQLSelect.Deleted/*" />
ACCESS Deleted 
	LOCAL lRet AS LOGIC


	lRet := SELF:lDeleteFlag
// 	IF SELF:nBuffIndex != 0
// 		lRet := SELF:aSQLDataBuffer[nBuffIndex, SQL_DATA_DELETE]
// 	ENDIF


	RETURN lRet


/// <include file="SQL.xml" path="doc/SQLSelect.EoF/*" />
ACCESS EoF 
 	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:EOF returns " + AsString(lEof ) )
	#ENDIF
   IF SELF:lAppendFlag
      RETURN FALSE
   ENDIF


	RETURN SELF:lEof


/// <include file="SQL.xml" path="doc/SQLSelect.ErrInfo/*" />
ACCESS ErrInfo 


	RETURN SELF: oStmt:ErrInfo


/// <include file="SQL.xml" path="doc/SQLSelect.FCount/*" />
ACCESS FCount 
	LOCAL nRet  AS INT


	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		nRet := -1
	ELSE
		nRet := INT( _CAST, SELF:nNumCols )


	ENDIF


	RETURN nRet


/// <include file="SQL.xml" path="doc/SQLSelect.FOUND/*" />
ACCESS FOUND () 


	RETURN !SELF:Eof


/// <include file="SQL.xml" path="doc/SQLSelect.LASTREC/*" />
ACCESS LASTREC() 
	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN 0
	ENDIF


	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:LastRec() returns " + NTrim( nLastRecNum ) )
	#ENDIF


	RETURN SELF:nLastRecNum


/// <include file="SQL.xml" path="doc/SQLSelect.MoreResults/*" />
ACCESS MoreResults 


	RETURN !SELF:lEof


/// <include file="SQL.xml" path="doc/SQLSelect.NativeSQL/*" />
ACCESS NativeSQL 


	RETURN SELF:oStmt:NativeSQL


/// <include file="SQL.xml" path="doc/SQLSelect.NullAsBlank/*" />
ACCESS NullAsBlank 


	RETURN SELF:lNullAsBlank


/// <include file="SQL.xml" path="doc/SQLSelect.NullAsBlank/*" />
ASSIGN NullAsBlank( lNew )                              	


	IF IsLogic( lNew )
		SELF:lNullAsBlank := lNew
	ENDIF


	RETURN SELF:lNullAsBlank


/// <include file="SQL.xml" path="doc/SQLSelect.NumParameters/*" />
ACCESS NumParameters 


	RETURN SELF:oStmt:NumParameters


/// <include file="SQL.xml" path="doc/SQLSelect.NumResultColumns/*" />
ACCESS NumResultColumns 
	//RvdH 050413 Make equal to FCount
	RETURN SELF:Fcount


/// <include file="SQL.xml" path="doc/SQLSelect.NumSuccessfulRows/*" />
ACCESS NumSuccessfulRows 


	RETURN SELF:nRowCount


/// <include file="SQL.xml" path="doc/SQLSelect.PrepFlag/*" />
ACCESS PrepFlag 


	RETURN SELF:oStmt:PrepFlag


/// <include file="SQL.xml" path="doc/SQLSelect.RecCount/*" />
ACCESS RecCount 


	RETURN SELF:__RecCount()


/// <include file="SQL.xml" path="doc/SQLSelect.Recno/*" />
ACCESS Recno 


	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN 0
	ENDIF


	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Recno="+AsString( nRecNum ) )
	#ENDIF
	RETURN SELF:nRecNum


/// <include file="SQL.xml" path="doc/SQLSelect.ScrollConcurrency/*" />
ACCESS ScrollConcurrency 


	RETURN SELF:oStmt:ScrollConcurrency


/// <include file="SQL.xml" path="doc/SQLSelect.ScrollConcurrency/*" />
ASSIGN ScrollConcurrency( nVal ) 


	RETURN SELF:oStmt:ScrollConcurrency := nVal


/// <include file="SQL.xml" path="doc/SQLSelect.ScrollUpdateType/*" />
ACCESS ScrollUpdateType 


	RETURN SELF:nScrollUpdateType


/// <include file="SQL.xml" path="doc/SQLSelect.ScrollUpdateType/*" />
ASSIGN ScrollUpdateType( uType ) 


	IF uType == SQL_SC_UPD_AUTO   .OR.           ;
		uType == SQL_SC_UPD_CURSOR .OR.           ;
		uType == SQL_SC_UPD_KEY    .OR.           ;
		uType == SQL_SC_UPD_VALUE
		SELF:nScrollUpdateType := uType
	ENDIF


	RETURN SELF:nScrollUpdateType


/// <include file="SQL.xml" path="doc/SQLSelect.SimulateCursor/*" />
ACCESS SimulateCursor 


	RETURN SELF:oStmt:SimulateCursor


/// <include file="SQL.xml" path="doc/SQLSelect.SimulateCursor/*" />
ASSIGN SimulateCursor( nVal ) 


	RETURN SELF:oStmt:SimulateCursor := nVal


/// <include file="SQL.xml" path="doc/SQLSelect.SQLColumns/*" />
ACCESS SQLColumns 


	RETURN SELF:ASQLColumns


//ACCESS BuffIndex CLASS SqlSelect


//	RETURN nBuffIndex


//ACCESS SqlDataBuffer CLASS SqlSelect


//	RETURN aSqlDataBuffer


/// <include file="SQL.xml" path="doc/SQLSelect.SqlData/*" />
ACCESS SqlData 


	RETURN aSqlData


/// <include file="SQL.xml" path="doc/SQLSelect.SQLData/*" />
ASSIGN SQLData( aNew ) 


	IF IsArray( aNew )
		SELF:aSqlData := aNew
	ENDIF
	RETURN 


/// <include file="SQL.xml" path="doc/SQLSelect.SQLString/*" />
ACCESS SQLString 


	RETURN SELF:oStmt:SQLString


/// <include file="SQL.xml" path="doc/SQLSelect.SQLString/*" />
ASSIGN SQLString( uVal ) 


	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:SQLString uVal="+AsString( uVal ) )
	#ENDIF


	IF SELF:oStmt:PrepFlag
		IF !SELF:Close()
			RETURN NIL
		ENDIF
	ENDIF


	IF IsString( uVal )
		SELF:oStmt:SQLString := uVal
	ENDIF


	//  Have a statement?
	IF SELF:oStmt:SQLString != NIL
		SELF:__FindTableName()
	ENDIF


	RETURN SELF:oStmt:SQLString


/// <include file="SQL.xml" path="doc/SQLSelect.Statement/*" />
ACCESS Statement 


	RETURN SELF:oStmt


/// <include file="SQL.xml" path="doc/SQLSelect.StatementHandle/*" />
ACCESS StatementHandle 


	RETURN SELF:oStmt:StatementHandle


/// <include file="SQL.xml" path="doc/SQLSelect.Status/*" />
ACCESS Status 


	RETURN SELF:oStmt:Status


/// <include file="SQL.xml" path="doc/SQLSelect.TableName/*" />
ACCESS TableName 


	RETURN SELF:cTableName


/// <include file="SQL.xml" path="doc/SQLSelect.TimeStampAsDate/*" />
ACCESS TimeStampAsDate	()			               			
	RETURN SELF:lTimeStampAsDate


/// <include file="SQL.xml" path="doc/SQLSelect.TimeStampAsDate/*" />
ASSIGN TimeStampAsDate	( lNew )		               			


	IF IsLogic( lNew )
		SELF:lTimeStampAsDate := lNew
	ENDIF
	RETURN SELF:lTimeStampAsDate




/// <include file="SQL.xml" path="doc/SQLSelect.Used/*" />
ACCESS Used 


	RETURN SELF:lCsrOpenFlag




//RvdH 2010-12-03: Some extra properties
/// <include file="SQL.xml" path="doc/SQLSelect.AppendData/*" />
ACCESS AppendData 


	RETURN aAppendData


/// <include file="SQL.xml" path="doc/SQLSelect.AppendData/*" />
ASSIGN AppendData( aData ) 
	
	
	aAppendData := aData


	RETURN 


/// <include file="SQL.xml" path="doc/SQLSelect.DELETED/*" />
ASSIGN DELETED( lValue ) 
	
	
	IF UsualType( lValue ) == LOGIC
		lDeleteFlag := lValue
	ENDIF
	RETURN 


/// <include file="SQL.xml" path="doc/SQLSelect.IndexColumns/*" />
ACCESS IndexColumns 


	RETURN aIndexCol


/// <include file="SQL.xml" path="doc/SQLSelect.Modified/*" />
ACCESS Modified 
	
	
	RETURN lRowModified


/// <include file="SQL.xml" path="doc/SQLSelect.Modified/*" />
ASSIGN Modified( lValue ) 
	
	
	IF UsualType( lValue ) == LOGIC
		lRowModified := lValue
	ENDIF
	RETURN 


/// <include file="SQL.xml" path="doc/SQLSelect.NumCols/*" />
ACCESS NumCols


	RETURN nNumCols


/// <include file="SQL.xml" path="doc/SQLSelect.RowCount/*" />
ASSIGN RowCount( nVal )
	
	
	RETURN nRowCount := nVal




END CLASS


