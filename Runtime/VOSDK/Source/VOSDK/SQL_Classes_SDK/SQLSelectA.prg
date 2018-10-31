PARTIAL CLASS SQLSelect


ACCESS AppendFlag 

	RETURN SELF:lAppendFlag

ASSIGN AppendFlag( lSetAppendFlag ) 

	IF IsLogic( lSetAppendFlag )
		SELF:lAppendFlag := lSetAppendFlag
	ENDIF

	RETURN SELF:lAppendFlag

ACCESS BoF 
 	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:BOF returns " + AsString(lBof) )
	#ENDIF
   IF SELF:lAppendFlag
      RETURN FALSE
   ENDIF
	RETURN SELF:lBof

ACCESS BoundedColumns() 

	RETURN SELF:nMaxBindColumn

ACCESS Connection 

	RETURN SELF:oConn

ACCESS CursorName 
	LOCAL   cRet    AS STRING

	IF( SELF:__GetCursorName() )
		cRet := SELF:cCursor
	ENDIF
	RETURN cRet

ASSIGN CursorName( cCursorName ) 
	LOCAL   cRet AS STRING

	IF( SELF:__SetCursorName( cCursorName ) )
		cRet := SELF:cCursor
	ENDIF

	RETURN cRet

ACCESS CursorType 

	RETURN SELF:oStmt:CursorType

ASSIGN CursorType( nVal ) 

	RETURN SELF:oStmt:CursorType := nVal

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

ACCESS Deleted 
	LOCAL lRet AS LOGIC

	lRet := SELF:lDeleteFlag
// 	IF SELF:nBuffIndex != 0
// 		lRet := SELF:aSQLDataBuffer[nBuffIndex, SQL_DATA_DELETE]
// 	ENDIF

	RETURN lRet

ACCESS EoF 
 	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:EOF returns " + AsString(lEof ) )
	#ENDIF
   IF SELF:lAppendFlag
      RETURN FALSE
   ENDIF

	RETURN SELF:lEof

ACCESS ErrInfo 

	RETURN SELF: oStmt:ErrInfo

ACCESS FCount 
	LOCAL nRet  AS INT

	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		nRet := -1
	ELSE
		nRet := INT( _CAST, SELF:nNumCols )

	ENDIF

	RETURN nRet

ACCESS FOUND () 

	RETURN !SELF:Eof

ACCESS LASTREC() 
	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN 0
	ENDIF

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:LastRec() returns " + NTrim( nLastRecNum ) )
	#ENDIF

	RETURN SELF:nLastRecNum

ACCESS MoreResults 

	RETURN !SELF:lEof

ACCESS NativeSQL 

	RETURN SELF:oStmt:NativeSQL

ACCESS NullAsBlank 

	RETURN SELF:lNullAsBlank

ASSIGN NullAsBlank( lNew )                              	

	IF IsLogic( lNew )
		SELF:lNullAsBlank := lNew
	ENDIF

	RETURN SELF:lNullAsBlank

ACCESS NumParameters 

	RETURN SELF:oStmt:NumParameters

ACCESS NumResultColumns 
	//RvdH 050413 Make equal to FCount
	RETURN SELF:Fcount

ACCESS NumSuccessfulRows 

	RETURN SELF:nRowCount

ACCESS PrepFlag 

	RETURN SELF:oStmt:PrepFlag

ACCESS RecCount 

	RETURN SELF:__RecCount()

ACCESS Recno 

	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN 0
	ENDIF

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Recno="+AsString( nRecNum ) )
	#ENDIF
	RETURN SELF:nRecNum

ACCESS ScrollConcurrency 

	RETURN SELF:oStmt:ScrollConcurrency

ASSIGN ScrollConcurrency( nVal ) 

	RETURN SELF:oStmt:ScrollConcurrency := nVal

ACCESS ScrollUpdateType 

	RETURN SELF:nScrollUpdateType

ASSIGN ScrollUpdateType( uType ) 

	IF uType == SQL_SC_UPD_AUTO   .OR.           ;
		uType == SQL_SC_UPD_CURSOR .OR.           ;
		uType == SQL_SC_UPD_KEY    .OR.           ;
		uType == SQL_SC_UPD_VALUE
		SELF:nScrollUpdateType := uType
	ENDIF

	RETURN SELF:nScrollUpdateType

ACCESS SimulateCursor 

	RETURN SELF:oStmt:SimulateCursor

ASSIGN SimulateCursor( nVal ) 

	RETURN SELF:oStmt:SimulateCursor := nVal

ACCESS SQLColumns 

	RETURN SELF:ASQLColumns

//ACCESS BuffIndex CLASS SqlSelect

//	RETURN nBuffIndex

//ACCESS SqlDataBuffer CLASS SqlSelect

//	RETURN aSqlDataBuffer

ACCESS SqlData 

	RETURN aSqlData

ASSIGN SQLData( aNew ) 

	IF IsArray( aNew )
		SELF:aSqlData := aNew
	ENDIF
	RETURN 

ACCESS SQLString 

	RETURN SELF:oStmt:SQLString

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

ACCESS Statement 

	RETURN SELF:oStmt

ACCESS StatementHandle 

	RETURN SELF:oStmt:StatementHandle

ACCESS Status 

	RETURN SELF:oStmt:Status

ACCESS TableName 

	RETURN SELF:cTableName

ACCESS TimeStampAsDate	()			               			
	RETURN SELF:lTimeStampAsDate

ASSIGN TimeStampAsDate	( lNew )		               			

	IF IsLogic( lNew )
		SELF:lTimeStampAsDate := lNew
	ENDIF
	RETURN SELF:lTimeStampAsDate


ACCESS Used 

	RETURN SELF:lCsrOpenFlag


//RvdH 2010-12-03: Some extra properties
ACCESS AppendData 

	RETURN aAppendData

ASSIGN AppendData( aData ) 
	
	aAppendData := aData

	RETURN 

ASSIGN DELETED( lValue ) 
	
	IF UsualType( lValue ) == LOGIC
		lDeleteFlag := lValue
	ENDIF
	RETURN 

ACCESS IndexColumns 

	RETURN aIndexCol

ACCESS Modified 
	
	RETURN lRowModified

ASSIGN Modified( lValue ) 
	
	IF UsualType( lValue ) == LOGIC
		lRowModified := lValue
	ENDIF
	RETURN 

ACCESS NumCols

	RETURN nNumCols

ASSIGN RowCount( nVal )
	
	RETURN nRowCount := nVal


END CLASS

