/// <include file="SQL.xml" path="doc/SQLCatalogQuery/*" />
CLASS SQLCatalogQuery INHERIT SQLSelect


/// <include file="SQL.xml" path="doc/SQLCatalogQuery.CursorName/*" />
ACCESS CursorName


	oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__INV_OP ), #CursorName )
	RETURN NIL


/// <include file="SQL.xml" path="doc/SQLCatalogQuery.CursorName/*" />
ASSIGN CursorName( cCursor )


	oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__INV_OP ), #CursorName )
	RETURN


/// <include file="SQL.xml" path="doc/SQLCatalogQuery.Delete/*" />
METHOD Delete()


	oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__INV_OP ), #Delete )
	RETURN FALSE


/// <inheritdoc />
METHOD Execute()


	lCsrOpenFlag := TRUE
	lRowModified := FALSE
	lBof := FALSE
	lEof := FALSE
	lFetchFlag := FALSE
	lDeleteFlag := FALSE


	RETURN SELF:__InitColumnDesc()


/// <include file="SQL.xml" path="doc/SQLCatalogQuery.FIELDPUT/*" />
METHOD FIELDPUT()


	///RvdH Changed #FieldSet to #FieldPut
	oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__INV_OP ), #FieldPut )
	RETURN FALSE


/// <include file="SQL.xml" path="doc/SQLCatalogQuery.GoTop/*" />
METHOD GoTop()
	//RvdH 051216 Added method, because Some ODBC Drivers don't support scrollable Catalog queries
	LOCAL lRet AS LOGIC
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:GoTop()" )
	#ENDIF
	IF !SELF:Notify( NOTIFYINTENTTOMOVE )
		oStmt:__GenerateSQLError( "IntentToMove returned false.", #GoTop )
		RETURN FALSE
	ENDIF
	// RvdH 050413 Centralize GoCold behavior
	IF !SELF:__GoCold(FALSE, TRUE)
		RETURN FALSE
	ENDIF


	// Some ODBC Drivers don't support scrollable Catalog queries, so use regular Fetch here,
	// And make sure we are on top by checking the Record number
	IF SELF:nRecNum > 1
		SELF:Execute()
	ENDIF
	lRet := SELF:Fetch()


	IF !lRet
		SELF:nLastRecnum := 0
		SELF:nRecnum := 1
		SELF:__SetRecordFlags( TRUE, TRUE )
	ENDIF
	RETURN lRet


/// <include file="SQL.xml" path="doc/SQLCatalogQuery.ctor/*" />
CONSTRUCTOR( oSQLConnection )


	SUPER( NIL, oSQLConnection )
	RETURN


/// <exclude />
METHOD Prepare()
	RETURN TRUE




/// <include file="SQL.xml" path="doc/SQLCatalogQuery.Skip/*" />
METHOD Skip( nRecordCount )
	LOCAL iRecCount, i       AS INT
	LOCAL lRet					  AS LOGIC
	//RvdH 051216 Added method, because Some ODBC Drivers don't support scrollable Catalog queries


	IF ! SELF:Notify( NOTIFYINTENTTOMOVE )
		oStmt:__GenerateSQLError( "IntentToMove returned false.", #Skip )
		RETURN FALSE
	ENDIF


	IF nRecordCount = NIL
		iRecCount := 1
	ELSE
		iRecCount := nRecordCount
	ENDIF


	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLSelect:Skip( "+NTrim( iRecCount )+" )" )
	#ENDIF


	IF ( iRecCount = 0 )
		RETURN TRUE
	ENDIF
	// Some ODBC Drivers don't support scrollable Catalog queries, so use regular Fetch here.
	IF (iRecCount > 0)
		FOR i := 1 TO iRecCount
			lRet := SELF:Fetch()
		NEXT
	ENDIF
	RETURN lRet




/// <include file="SQL.xml" path="doc/SQLCatalogQuery.TableName/*" />
ACCESS TableName


	oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__INV_OP ), #TableName )
	RETURN NIL


/// <include file="SQL.xml" path="doc/SQLCatalogQuery.Update/*" />
METHOD Update()


	oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__INV_OP ), #Update )
	RETURN FALSE
END CLASS


