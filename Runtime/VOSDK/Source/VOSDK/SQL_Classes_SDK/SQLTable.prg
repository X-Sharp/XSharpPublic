CLASS SQLTable INHERIT SQLSelect

	PROTECT cTblStmt            AS STRING
	PROTECT cTable              AS STRING
	PROTECT cFields             AS STRING
	PROTECT cWhereSeek          AS STRING
	PROTECT cWhere              AS STRING
	PROTECT cOrderSeek          AS STRING
	PROTECT cOrder              AS STRING
	PROTECT cSuffix             AS STRING
	PROTECT lRelationsActive    AS LOGIC
	PROTECT aRelationChildren   AS ARRAY
	PROTECT lSelectionSeek      AS LOGIC
	PROTECT lSelectionActive    AS LOGIC
	PROTECT oSelectionParent    AS OBJECT
	PROTECT aParentRelationCols AS ARRAY
	PROTECT cRelationExpression AS STRING
	PROTECT lSoft               AS LOGIC
	PROTECT SymCol              AS USUAL // Array of Symbols or Symbol

	METHOD __AcceptSelectiveRelation( oParent AS OBJECT, uRelation AS USUAL, cRelation AS USUAL) AS LOGIC STRICT 
	LOCAL wFieldNo, wFieldCount AS DWORD

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:__AcceptSelectiveRelation()" )
	#ENDIF

	// save parent & relation
	lSelectionActive := TRUE
	oSelectionParent := oParent

	IF IsCodeBlock( uRelation )
		cRelationExpression := IIF( cRelation = NIL, "", cRelation )
		aParentRelationCols := {}
		AAdd( aParentRelationCols, cRelationExpression )

	ELSEIF IsSymbol( uRelation ) .OR. IsString( uRelation )
		cRelationExpression := AsString( uRelation )
		aParentRelationCols := {}
		AAdd( aParentRelationCols, cRelationExpression )

	ELSEIF IsArray( uRelation )
		wFieldCount := ALen( uRelation )
		IF wFieldCount < 1
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NOFLDS ), #AcceptSelectiveRelation )
			RETURN FALSE
		ELSE
			cRelationExpression := ""
			aParentRelationCols := {}
			FOR wFieldNo := 1 UPTO wFieldCount
				IF !IsNil( uRelation[wFieldNo] )
					IF IsNumeric( uRelation[wFieldNo] )
						SWITCH (LONG) uRelation[wFieldNo]
						CASE  SQL_RELOP_AND
							cRelationExpression += " AND "
							AAdd( aParentRelationCols, uRelation[wFieldNo] )

						CASE SQL_RELOP_OR
							cRelationExpression += " OR "
							AAdd( aParentRelationCols, uRelation[wFieldNo] )

						CASE SQL_RELOP_NOT
							cRelationExpression += " NOT "
							AAdd( aParentRelationCols, uRelation[wFieldNo] )

						CASE SQL_RELOP_OPENP
							cRelationExpression += " ( "
							AAdd( aParentRelationCols, uRelation[wFieldNo] )

						CASE SQL_RELOP_CLOSEP
							cRelationExpression += " ) "
							AAdd( aParentRelationCols, uRelation[wFieldNo] )

						END SWITCH
					ELSE
						IF wFieldNo = 1
							cRelationExpression += AsString( uRelation[wFieldNo] )
						ELSE
							cRelationExpression += "+" + AsString( uRelation[wFieldNo] )
						ENDIF

						AAdd( aParentRelationCols, AsString( uRelation[wFieldNo] ) )
					ENDIF  	// IsNumeric( uRelation[wFieldNo] )
				ENDIF		// !IsNil( uRelation[wFieldNo] )
			NEXT
		ENDIF				// wFieldCount < 1
	ELSE
		oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__NOFLDS ), #AcceptSelectiveRelation )
		RETURN FALSE
	ENDIF

	//RvdH 050413 Centralize opening of cursor
	IF ! SELF:__ForceOpen()
		RETURN FALSE
	ENDIF

	RETURN TRUE

METHOD __BuildSQLString() AS VOID STRICT 
	LOCAL nIndex        AS DWORD
	LOCAL cWhereSep  := ""   AS STRING
	LOCAL cOrderSep  := ""   AS STRING
	LOCAL cOperator     AS STRING
	LOCAL nMax          AS DWORD
	LOCAL uTempValue    AS USUAL
	LOCAL lNeedOP       AS LOGIC
	LOCAL oFldSpec      AS FieldSpec
	LOCAL cVal          AS STRING
	LOCAL cParentField  AS STRING
	LOCAL cChildField   AS STRING
	LOCAL nFPos         AS DWORD
	LOCAL cQuote		  AS STRING
	LOCAL symColumn	  AS SYMBOL

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** NewSQLTable:__BuildSQLString()" )
	#ENDIF

	IF lSoft
		cOperator := " >= ? "
	ELSE
		cOperator := " = ? "
	ENDIF
	cQuote := oConn:IdentifierQuoteChar
	IF ! lSelectionSeek
		// build seek where list...
		cWhereSeek := NULL_STRING
		cOrderSeek := NULL_STRING
		IF symCol != NIL
			IF UsualType( symCol ) = ARRAY
				FOR nIndex := 1 TO ALen( symCol )
					symColumn := symCol[nIndex]

					cWhereSeek += cWhereSep + cQuote + __GetSymString( symColumn) + cQuote + cOperator
					cWhereSep  := " and "

					cOrderSeek +=   cOrderSep + cQuote + __GetSymString( symColumn ) + cQuote
					cOrderSep := ","

				NEXT
			ELSE

				cWhereSeek := cQuote + __GetSymString( symCol ) + cQuote + cOperator
				cOrderSeek := cQuote + __GetSymString( symCol ) + cQuote
			ENDIF
		ENDIF
	ENDIF

	IF lCsrOpenFlag
		lSuppressNotify := TRUE
		SELF:Close()

		lSuppressNotify := FALSE
	ENDIF

	//
	cTblStmt := "select " + cFields + " from "+ cTable

	IF Len( cWhereSeek ) != 0 .OR. Len( cWhere ) != 0
		cTblStmt += " where "

		IF SLen( cWhereSeek ) != 0
			cTblStmt += cWhereSeek + " "

			IF SLen( cWhere ) != 0
				cTblStmt += " and "
			ENDIF
		ENDIF

		IF SLen( cWhere ) != 0
			cTblStmt += cWhere + " "
		ENDIF
	ENDIF

	// selective child?
	IF lSelectionActive
		IF Len( cWhereSeek ) = 0 .AND. Len( cWhere ) = 0
			cTblStmt += " where "
		ELSE
			cTblStmt += " and "
		ENDIF

		//  Put in parenthesis
		cTblStmt += " ( "

		// now add selective child restriction,
		// using value of parent's relation columns...
		nMax := Len( aParentRelationCols )
		lNeedOP := FALSE

		FOR nIndex := 1 UPTO nMax
			IF IsNumeric( aParentRelationCols[nIndex] )
				SWITCH (LONG) aParentRelationCols[nIndex]
				CASE   SQL_RELOP_AND
					cTblStmt += " AND "
					lNeedOP := FALSE

				CASE SQL_RELOP_OR
					cTblStmt += " OR "
					lNeedOP := FALSE

				CASE SQL_RELOP_NOT
					cTblStmt += " NOT "
					lNeedOP := FALSE

				CASE SQL_RELOP_OPENP
					cTblStmt += " ( "
					lNeedOP := FALSE

				CASE SQL_RELOP_CLOSEP
					cTblStmt += " ) "
					lNeedOP := FALSE

				END SWITCH
			ELSE
				IF lNeedOP
					cTblStmt += " and "
				ENDIF

				// Enhancement to allow a reflexive relationship!
				// That means that you can have a relationship from Table1:FieldName1 to Tablex:Fieldname2
				// and not only Table1:FieldName1 to Table2:Fieldname1
				// Now accepting the following Relation syntax:
				//
				//      "<FieldName1> = <FieldName2>" or
				//      "<FieldName1> | <FieldName2>" .... prefered Syntax
				//
				//      <FieldName1> ... ParentField
				//      <FieldName2> ... ChildField in which to seek
				//      this means: The Value from oSelectionParent:FieldName1 is used to search
				//                  the field FieldName2 in the Childtable.
				//
				// This is working very well when the field in the Childtable have the same datatype as the
				// field in the parenttable.

				IF ( nFPos := At2( "=",aParentRelationCols[nIndex] ) ) > 0          // Syntax 1
					cParentField := AllTrim( SubStr3( aParentRelationCols[nIndex], 1, nFPos-1 ) )
					cChildField  := AllTrim( SubStr2( aParentRelationCols[nIndex], nFPos + 1 ) )

				ELSEIF ( nFPos := At2( "|",aParentRelationCols[nIndex] ) ) > 0      // Syntax 2
					cParentField := AllTrim( SubStr3( aParentRelationCols[nIndex], 1, nFPos-1 ) )
					cChildField  := AllTrim( SubStr2( aParentRelationCols[nIndex], nFPos + 1 ) )

				ELSE
					cParentField := aParentRelationCols[nIndex]
					cChildField  := cParentField
				ENDIF

				//  Use access instead of fieldget
				uTempValue := IVarGet( oSelectionParent, AsSymbol( cParentField ) )

				//  Get fieldspec ( for type )
				oFldSpec := Send( oSelectionParent, #FieldSpec, AsSymbol( cParentField ) )

				//  Check NIL value
				IF IsNil( uTempValue )
					cTblStmt += cQuote + SELF:__GetFieldName( cChildField ) + ;
								cQuote + __CAVOSTR_SQLCLASS__EQ_NULL
				ELSE
					IF ! IsNil( oFldSpec )
						SWITCH (STRING) oFldSpec:ValType
						CASE  "N"
							cTblStmt += cQuote + SELF:__GetFieldName( cChildField ) + ;
										cQuote + " = " + AsString( uTempValue )

						CASE "L"
							IF uTempValue
								cVal := "1"
							ELSE
								cVal := "0"
							ENDIF

							cTblStmt += cQuote + SELF:__GetFieldName( cChildField ) + ;
										cQuote + " = " +cVal

						CASE "D"
							cVal := DToS( uTempValue )
							cVal := SubStr3( cVal, 1, 4 ) + "-" +        ;
									SubStr3( cVal, 5, 2 ) + "-" +        ;
									SubStr3( cVal, 7, 2 )
							cTblStmt += cQuote + ;
								SELF:__GetFieldName( cChildField ) + ;
								cQuote + " = {d '" + cVal + "'}"
						OTHERWISE
							cTblStmt += cQuote + SELF:__GetFieldName( cChildField ) + ;
								cQuote + " = '"  + AsString( uTempValue ) + "'"
						END SWITCH
					ELSE
						cTblStmt += cQuote + SELF:__GetFieldName( cChildField ) + ;
							cQuote + " ='" + AsString( uTempValue ) + "'"
					ENDIF
				ENDIF

				lNeedOP := TRUE

			ENDIF
		NEXT

		//  Put in parenthesis
		cTblStmt += " ) "

	ENDIF

	IF SLen( cSuffix ) != 0
		cTblStmt += " " + cSuffix
	ENDIF

	IF SLen( cOrderSeek ) != 0 .OR. Len( cOrder ) != 0
		cTblStmt += __CAVOSTR_SQLCLASS__ORDER_BY
		IF Len( cOrderSeek ) != 0
			cTblStmt += cOrderSeek + " "

			IF Len( cOrder ) != 0
				cTblStmt += ","
			ENDIF
		ENDIF

		IF Len( cOrder ) != 0
			cTblStmt += cOrder + " "
		ENDIF
	ENDIF

	#IFDEF __DEBUG__
		__SQLOutputDebug( "  __BuildSQLString() cTblStmt="+AsString( cTblStmt ) )
	#ENDIF

	//  add the statement to oStmt
	//  UH 06/05/2000
	//  oStmt:SQLString := cTblStmt
	SELF:oStmt:SQLString := SELF:__StripStatement( cTblStmt )

	RETURN


METHOD __StripStatement( cStat AS STRING ) AS STRING STRICT 
	LOCAL cChar AS STRING
	LOCAL nPos AS DWORD

	cChar := e"\""
	nPos := At2( cChar, cStat )
	DO WHILE nPos > 0
		IF nPos = 1
			cStat := SubStr2( cStat, 2 )
		ELSE
			cStat := SubStr3( cStat, 1, nPos -1 ) + SubStr2( cStat, nPos +1 )
		ENDIF
		nPos := At2( cChar, cStat )
	ENDDO

	RETURN cStat

METHOD ClearRelation( nRelation ) 
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:ClearRelation()" )
	#ENDIF

	IF lRelationsActive
		//  nRelation means clear all relations
		IF IsNil( nRelation )
			ASend( aRelationChildren, #Notify, NOTIFYCLEARRELATION )
			lRelationsActive := FALSE
			aRelationChildren := {}
		ELSE
			// find the child
			IF ! lRelationsActive .OR. nRelation = 0 .OR. ;
				nRelation > ALen( aRelationChildren )
				oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #ClearRelation )
				RETURN FALSE
			ENDIF
			Send( aRelationChildren[nRelation], #Notify, NOTIFYCLEARRELATION )
			ADel( aRelationChildren, nRelation )
			ASize( aRelationChildren, ALen( aRelationChildren ) - 1 )

			IF ALen( aRelationChildren ) = 0
				lRelationsActive := FALSE
				aRelationChildren := {}
			ENDIF
		ENDIF
		RETURN TRUE
	ELSE
		RETURN FALSE
	ENDIF

METHOD Condition( cOtherConditions ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:Condition( "+AsString( cOtherConditions )+" )" )
	#ENDIF
	///RvdH Add Typecheck ?
	IF cOtherConditions != NIL
		SELF:cSuffix := cOtherConditions
	ENDIF

	SELF:__BuildSQLString()
	RETURN NIL

CONSTRUCTOR( symTableName, aFieldList, oSQLConnection ) 

	// You can specify a table name as a symbol ( or string ), and an
	//  optional array of field names ( as symbols or strings ).
	// If the field list is omitted ( NIL or {} ) the class uses SELECT * ...
	//
	// You can specify additional constraints through the Where, OrderBy,
	//  GroupBy methods.
	// In addition, the Seek method will generate additional Where and
	//  OrderBy methods, and SetFilter is a synonym for OrderBy.
	//
	// A SQL statement is generated something like this:
	//      SELECT *        or SELECT aFieldList
	//      FROM TableName
	//      WHERE where-clause generated by SEEK
	//           .AND. where-clause1
	//           .AND. where-clause2 ...
	//      ORDER BY order-by-clause generated by SEEK,
	//                                          order-by-clause1,
	//                                          order-by-clause2 ...
	//      cOtherConditions
	//
	LOCAL nIndex            AS DWORD
	LOCAL cSeparator  := ""	AS STRING

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:Init( "+AsString( symTableName )+" )" )
	#ENDIF

	SELF:aRelationChildren   := {}
	SELF:aParentRelationCols := {}
	cTable := __GetSymString( symTableName )
	IF aFieldList = NIL
		cFields := "*"
	ELSE
		IF UsualType( aFieldList ) = ARRAY
			cFields := NULL_STRING
			FOR nIndex := 1 TO ALen( aFieldList )
				cFields += cSeparator + __GetSymString( aFieldList[nIndex] )
				cSeparator := ","
			NEXT
		ELSE
			cFields := __GetSymString( aFieldList )
		ENDIF
	ENDIF

	SUPER( NIL , oSQLConnection )

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:Init() cFields="+AsString( cFields ) )
	#ENDIF

	cTableName := cTable
	lRelationsActive    := FALSE
	aRelationChildren   := {}
	lSelectionActive    := FALSE
	lSelectionSeek      := FALSE
	//oSelectionParent  := NIL_OBJECT
	aParentRelationCols := {}
	cRelationExpression := ""
	lSoft               := FALSE

	SELF:__BuildSQLString()
	oHyperLabel := HyperLabel{  cTable, ;
								cTable, ;
								Symbol2String( ClassName( SELF ) )+": "+cTable, ;
								Symbol2String( ClassName( SELF ) )+"_"+cTable }

	RETURN 

METHOD Notify ( kNotification, uDescription ) 
	LOCAL lRetValue AS LOGIC
	LOCAL nChild := 0 AS DWORD

	lRetValue := TRUE

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:Notify( "+AsString( kNotification )+          ;
						  " ) Count="+AsString( nNotifyCount ) )
	#ENDIF

	// NOT suspended & NOT suppressed?
	IF nNotifyCount = 0 .AND. ! lSuppressNotify
		IF kNotification <= NOTIFYCOMPLETION
			SUPER:Notify( kNotification, uDescription )

		ELSEIF kNotification = NOTIFYINTENTTOMOVE
			lRetValue := SUPER:Notify( kNotification, uDescription )
			IF lRetValue .AND. lRelationsActive
				DO WHILE lRetValue .AND. nChild < ALen( aRelationChildren )
					nChild++
					lRetValue := Send(aRelationChildren[nChild],#Notify, kNotification )
				ENDDO
			ENDIF
			RETURN lRetValue

		ELSEIF kNotification <= NOTIFYFILECHANGE
			SUPER:Notify( kNotification, uDescription )
			IF lRelationsActive
				// file changed, need to reset any child relations
				ASend( aRelationChildren, #Notify, NOTIFYRELATIONCHANGE )
			ENDIF

		ELSEIF kNotification = NOTIFYRELATIONCHANGE
			IF lSelectionActive
				// re-issue the select using the parent's relation value...
				lSelectionSeek := TRUE
				SELF:SuspendNotification()
				SELF:Seek()
				SELF:ResetNotification()
				lSelectionSeek := FALSE

				IF oStmt:__ErrInfo:ErrorFlag
					RETURN NIL
				ENDIF
			ENDIF

			SUPER:Notify( kNotification, uDescription )

			IF lRelationsActive
				// pass it on to my children
				ASend( aRelationChildren, #Notify, NOTIFYRELATIONCHANGE )
			ENDIF

		ELSEIF kNotification = NOTIFYCLEARRELATION
			lSelectionActive := FALSE

		ELSE   // event I don't know about
			SUPER:Notify( kNotification, uDescription )
			IF lRelationsActive
				ASend( aRelationChildren, #Notify, kNotification,uDescription )
			ENDIF
		ENDIF
	ENDIF

	RETURN lRetValue


METHOD OrderBy( /* symColName1,symColName2,... */ ) CLIPPER
	LOCAL wCount AS DWORD
	LOCAL cSeparator := ""  AS STRING

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:OrderBy()" )
	#ENDIF

	cOrder := NULL_STRING
	// any parameters?
	IF ( PCount() != 0 )
		FOR wCount:=1 UPTO PCount()
			cOrder += cSeparator + __GetSymString( _GETMPARAM( wCount ) )
			cSeparator := ","
		NEXT
	ENDIF

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:OrderBy() cOrder="+AsString( cOrder ) )
	#ENDIF

	SELF:__BuildSQLString()
	RETURN NIL


METHOD Relation( nRelation ) 
	//
	// returns a string representation of the relation; or NULL_STRING
	//
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:Relation()" )
	#ENDIF

	IF IsNil( nRelation )
		RETURN cRelationExpression
	ELSE
		// find the child
		IF ! lRelationsActive .OR. nRelation > ALen( aRelationChildren )
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #Relation )
			RETURN NULL_STRING
		ENDIF

		// then return the childs's relationship to the parent
		RETURN Send(aRelationChildren[nRelation],#Relation)
	ENDIF


METHOD Seek( symColumn, uValue, lSoftSeek ) 

	LOCAL aArgs                              AS ARRAY
	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:Seek()" )
	#ENDIF
	IF lSoftSeek != NIL
		lSoft := lSoftSeek
	ENDIF

	symCol := symColumn

	SELF:__BuildSQLString()

	//  Pre-execute any stmt override
	//  UH 07/17/2000
	//  oStmt:SQLString := SELF:PreExecute( oStmt:SQLString )

	aArgs := {}
	IF uValue != NIL
		IF UsualType( uValue ) = ARRAY
			aArgs := uValue
		ELSE
			AAdd( aArgs, uValue )
		ENDIF
	ENDIF
	IF !SELF:Execute( aArgs )
		RETURN .F. 
	ENDIF

	SELF:__GetColIndex( 1, TRUE )
	//  If empty, set bof
	IF SELF:lEof
		SELF:__SetRecordFlags( TRUE, NIL )
	ENDIF

	SELF:Notify( NOTIFYFILECHANGE )
	RETURN !SELF:lEof

METHOD SetRelation( oChild, uRelation, cRelation ) 

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:SetRelation()" )
	#ENDIF

	IF oChild = NIL
		SELF:ClearRelation()
	ELSE
		//  Prevent infinite loop child=parent
		IF oChild = SELF
			oStmt:__GenerateSQLError( __CavoStr( __CAVOSTR_SQLCLASS__BADPAR ), #SetRelation )
			RETURN FALSE
		ENDIF

		// is this our child?
		IF AScan( aRelationChildren, oChild ) = 0
			// no, adopt it
			AAdd( aRelationChildren, oChild )
		ENDIF
		lRelationsActive := TRUE

		IF ! Send(oChild,#__AcceptSelectiveRelation, SELF, uRelation, cRelation )
			RETURN FALSE
		ENDIF
		Send(oChild,#Notify, NOTIFYRELATIONCHANGE )
	ENDIF

	RETURN TRUE



METHOD SetSelectiveRelation( oChild, uRelation, cRelation ) 
	RETURN SELF:SetRelation( oChild, uRelation, cRelation )

METHOD Where( )  CLIPPER
	LOCAL wCount                AS DWORD
	LOCAL cSeparator := ""      AS STRING

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:Where()" )
	#ENDIF

	cWhere := NULL_STRING
	// any parameters?
	IF ( PCount() != 0 )
		FOR wCount:=1 UPTO PCount()
			cWhere += cSeparator + __GetSymString( _GETMPARAM( wCount ) )
			cSeparator := " and "
		NEXT
	ENDIF

	#IFDEF __DEBUG__
		__SQLOutputDebug( "** SQLTable:Where() cWhere="+AsString( cWhere ) )
	#ENDIF

	SELF:__BuildSQLString()
	RETURN NIL

//RvdH 2010-12-03: Some extra properties
ACCESS Fields 
	RETURN cFields

ACCESS OrderByClause 
	RETURN cOrder
	
ACCESS OrderSeek
	RETURN cOrderSeek

ACCESS Suffix
	RETURN cSuffix

ACCESS Table 
	RETURN cTable
	
ACCESS WhereClause
	RETURN cWhere

ACCESS WhereSeek
	RETURN cWhereSeek

END CLASS

