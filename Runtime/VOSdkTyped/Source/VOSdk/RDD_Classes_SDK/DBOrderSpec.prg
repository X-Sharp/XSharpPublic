CLASS OrderSpec
	PROTECT oDBF            AS DbFileSpec

	// index order items
	PROTECT cFileName       AS STRING
	PROTECT cOrdName        AS STRING
	PROTECT cOrdExpr        AS STRING
	PROTECT cbOrdExpr       AS USUAL
	PROTECT lUnique         AS LOGIC
	PROTECT aKeyInfo        AS ARRAY

	// conditional index items
	PROTECT lIsCond          AS LOGIC
	PROTECT uForCond        AS STRING   // RvdH 070310 Was ForCond which is the same as an Access....
	PROTECT uForBlock       AS USUAL		// RvdH 070310 Was ForBlock which is the same as an Access....
	PROTECT uWhileBlock     AS USUAL		// RvdH 070310 Was WhileBlock which is the same as an Access....
	PROTECT uEvalBlock      AS USUAL		// RvdH 070310 Was EvalBlock which is the same as an Access....
	PROTECT nStep           AS DWORD
	PROTECT nStart          AS DWORD
	PROTECT nNext           AS DWORD
	PROTECT nRecno          AS DWORD
	PROTECT lRest           AS LOGIC
	PROTECT lDescend        AS LOGIC
	PROTECT lAll            AS LOGIC
	PROTECT lAdd            AS LOGIC
	PROTECT lCustom         AS LOGIC
	PROTECT lCurrent        AS LOGIC
	PROTECT lNoOpt          AS LOGIC

	// NTX order items
	PROTECT lHPLock         AS LOGIC
	PROTECT nLockOffSet     AS LONGINT

	// CDX order items
	PROTECT lAutoOpen       AS LOGIC
	PROTECT nAutoOrder      AS LONGINT
	PROTECT nAutoShare      AS LONGINT
	PROTECT lStrictRead     AS LOGIC

	METHOD __OpenDb      ( cAlias AS STRING )	AS LOGIC STRICT	

	IF SLen( cAlias ) > 0
		cAlias := Symbol2String( __ConstructUniqueAlias( SELF:oDBF:FileName ) )
	ENDIF

	RETURN DbUseArea( TRUE, SELF:oDBF:RDDs, SELF:oDBF:FullPath, cAlias, FALSE, FALSE )

PROPERTY __DBF AS DbFileSpec GET oDBF
METHOD __OrderSetInfo( cAlias AS STRING ) AS VOID STRICT  
	// Gets order information and assigns info into the OrderSpec Object.
	// Assumes that the workarea defined by cAlias is already opened and the order
	// has been set.
	//
	LOCAL aKeyInfo      AS ARRAY
	LOCAL cRDD          AS STRING

	IF SLen( cAlias )  > 0
		aKeyInfo := ArrayNew( 4 )

		cRDD := SELF:__DBF:RDD_Name

		aKeyInfo[ORD_KEYTYPE] := ( cAlias )->( DbOrderInfo( DBOI_KEYTYPE ) )
		DO CASE
		CASE aKeyInfo[ORD_KEYTYPE] == 7
			aKeyInfo[ORD_KEYTYPE] := "STRING"

		CASE aKeyInfo[ORD_KEYTYPE] == 3
			aKeyInfo[ORD_KEYTYPE] := "NUMERIC"

		CASE aKeyInfo[ORD_KEYTYPE] == 2
			aKeyInfo[ORD_KEYTYPE] := "DATE"

		CASE aKeyInfo[ORD_KEYTYPE] == 8
			aKeyInfo[ORD_KEYTYPE] := "LOGIC"
		ENDCASE

		aKeyInfo[ORD_KEYCOUNT]      := ( cAlias )->( DbOrderInfo( DBOI_KEYCOUNT ) )
		aKeyInfo[ORD_KEYSIZE]       := ( cAlias )->( DbOrderInfo( DBOI_KEYSIZE ) )
		aKeyInfo[ORD_KEYDEC]        := ( cAlias )->( DbOrderInfo( DBOI_KEYDEC ) )

		// non-conditional order information
		SELF:FileName           := ( cAlias )->( DbOrderInfo( DBOI_FULLPATH ) )
		SELF:__DBF:IndexNames     := ( cAlias )->( DbOrderInfo( DBOI_FULLPATH ) )
		SELF:OrderName          := ( cAlias )->( DbOrderInfo( DBOI_NAME ) )
		SELF:OrderExpr          := ( cAlias )->( DbOrderInfo( DBOI_EXPRESSION ) )
		SELF:OrderBlock         := &( "{||" + SELF:OrderExpr + "}" )
		SELF:Unique             := ( cAlias )->( DbOrderInfo( DBOI_UNIQUE ) )

		SELF:__DBF:Orders         := SELF

		// key info
		SELF:KeyInfo            := aKeyInfo

		// conditional index info
		SELF:lIsCond            := ( cAlias )->( DbOrderInfo( DBOI_ISCOND ) )
		IF SELF:lIsCond
			SELF:uForCond        := ( cAlias )->( DbOrderInfo( DBOI_CONDITION ) )
			SELF:uForBlock       := &( "{||" + SELF:uForCond + "}" )
		ELSE
			SELF:uForCond        := NULL_STRING
			SELF:uForBlock       := NIL
		ENDIF

		SELF:Descend            := ( cAlias )->( DbOrderInfo( DBOI_ISDESC ) )

		// NTX specific items
		IF cRDD == "DBFNTX"
			SELF:Custom        := FALSE
			SELF:NoOptimize    := FALSE
			SELF:lHPLock        := ( cAlias )->( DbOrderInfo( DBOI_HPLOCKING ) )
			SELF:nLockOffSet    := ( cAlias )->( DbOrderInfo( DBOI_LOCKOFFSET ) )

		ENDIF

		// CDX specific items
		//	IF cRDD == "DBFCDX"
		IF At2( "CDX", cRDD ) > 0
			SELF:Custom        := ( cAlias )->( DbOrderInfo( DBOI_CUSTOM ) )
			SELF:NoOptimize     := ( cAlias )->( RddInfo( _SET_OPTIMIZE ) )
			SELF:lAutoOpen      := ( cAlias )->( RddInfo( _SET_AUTOOPEN ) )
			SELF:nAutoOrder     := ( cAlias )->( RddInfo( _SET_AUTOORDER ) )
			SELF:nAutoShare     := ( cAlias )->( RddInfo( _SET_AUTOSHARE ) )
			SELF:lStrictRead    := ( cAlias )->( RddInfo( _SET_STRICTREAD ) )
		ENDIF

	ELSE
		// null everything out if no alias
		SELF:FileName       := NULL_STRING
		SELF:OrderName      := NULL_STRING
		SELF:OrderExpr      := NULL_STRING
		SELF:OrderBlock     := NIL
		SELF:Unique         := FALSE

		SELF:KeyInfo        := NULL_ARRAY

		SELF:lIsCond         := FALSE
		SELF:uForCond        := NULL_STRING
		SELF:uForBlock       := NIL
		SELF:uWhileBlock     := NIL
		SELF:uEvalBlock      := NIL
		SELF:Interval       := 0
		SELF:Start          := 0
		SELF:Records        := 0
		SELF:RecNo          := 0
		SELF:Rest           := FALSE
		SELF:Descend        := FALSE
		SELF:All            := FALSE
		SELF:Add            := FALSE
		SELF:Custom         := FALSE
		SELF:Current        := FALSE
		SELF:NoOptimize     := FALSE

		SELF:lHPLock        := FALSE
		SELF:nLockOffSet    := 0

		SELF:lAutoOpen      := FALSE
		SELF:nAutoOrder     := 0
		SELF:nAutoShare     := 0
		SELF:lStrictRead    := FALSE

	ENDIF

	RETURN

ACCESS Add          
	RETURN SELF:lAdd

ASSIGN Add( lLogic )      
	// conditional index
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lAdd := FALSE
	ELSE
		SELF:lAdd := lLogic
		SELF:lIsCond := TRUE
	ENDIF
	RETURN 

ACCESS All          
	RETURN SELF:lAll

ASSIGN All( lLogic )      
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lAll := FALSE

	ELSE
		SELF:lAll := lLogic
		SELF:lIsCond := TRUE

	ENDIF

	RETURN 

ACCESS AutoOpen     
	RETURN SELF:lAutoOpen

ACCESS AutoOrder    
	RETURN SELF:nAutoOrder

ACCESS AutoShare    
	RETURN SELF:nAutoShare

ACCESS Current          
	RETURN SELF:lCurrent

ASSIGN Current( lLogic )      
	// conditional index
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lCurrent := FALSE

	ELSE
		SELF:lCurrent := lLogic
		SELF:lIsCond := TRUE

	ENDIF

	RETURN 

ACCESS Custom           
	RETURN SELF:lCustom

ASSIGN Custom( lLogic )       
	// conditional index
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lCustom := FALSE

	ELSE
		SELF:lCustom := lLogic
		SELF:lIsCond := TRUE

	ENDIF

	RETURN 

ACCESS DBF          
	//
	// Returns the DBFileSpec object that this OrderSpec object
	// belongs to.
	//
	RETURN SELF:oDBF

ACCESS Descend          
	RETURN SELF:lDescend

ASSIGN Descend( lLogic )      
	// conditional index
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lDescend := FALSE

	ELSE
		SELF:lDescend := lLogic
		SELF:lIsCond := TRUE

	ENDIF

	RETURN 

ACCESS EvalBlock    
	RETURN SELF:uEvalBlock

ASSIGN EvalBlock( cbCodeBlock )   
	// conditional index
	IF Empty( cbCodeBlock ) .OR. !__CanEval( cbCodeBlock )
		SELF:uEvalBlock := NIL

	ELSE
		SELF:uEvalBlock := cbCodeBlock
		SELF:lIsCond := TRUE

	ENDIF

	RETURN 

ACCESS FileName         
	RETURN SELF:cFileName

ASSIGN FileName( cName )          
	LOCAL aFullPath AS ARRAY
	LOCAL cFileName AS STRING
	LOCAL oDBFS     AS DbFileSpec

	IF Empty( cName ) .OR. !IsString( cName )
		SELF:cFileName := NULL_STRING

	ELSE
		aFullPath := ArrayNew( 4 )
		oDBFS := SELF:oDBF
		// clean up the drive+path+filename
		__SplitPath( oDBFS, cName, aFullPath )

		IF SubStr2( aFullPath[ 2 ], SLen( aFullPath[ 2 ] ) ) == "\"
			cFileName := aFullPath[ 1 ] + aFullPath[ 2 ] + ;
				aFullPath[ 3 ] + aFullPath[ 4 ]

		ELSE
			cFileName := aFullPath[ 1 ] + aFullPath[ 2 ] + "\" + ;
				aFullPath[ 3 ] + aFullPath[ 4 ]

		ENDIF

		SELF:cFileName := Upper( cFileName )

	ENDIF

	RETURN 

ACCESS ForBlock     
	RETURN SELF:uForBlock

ASSIGN ForBlock( cbCodeBlock )    
	// conditional index
	IF Empty( cbCodeBlock ) .OR. !__CanEval( cbCodeBlock )
		SELF:uForBlock := NIL

	ELSE
		SELF:uForBlock := cbCodeBlock
		SELF:lIsCond := TRUE

	ENDIF

	RETURN 

ACCESS ForCond      
	RETURN SELF:uForCond

ASSIGN ForCond( cForCondition )   
	// conditional index
	IF Empty( cForCondition ) .OR. !IsString( cForCondition )
		SELF:uForCond := NULL_STRING

	ELSE
		SELF:uForCond := cForCondition
		SELF:lIsCond := TRUE

	ENDIF

	RETURN 

ACCESS HPLock       
	RETURN SELF:lHPLock

ASSIGN HPLock( lLogic )   

	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		lLogic := FALSE

	ENDIF

	SELF:lHPLock := lLogic

	RETURN 

CONSTRUCTOR( oDBFS )      
	//
	// oDBFS is an existing DBFileSpec object
	//
    IF IsObject(oDBFS) .and. __Usual.ToObject(oDBFS) IS DbFileSpec  
		SELF:oDBF := oDBFS
		IF SELF:__DBF:Orders == NULL_ARRAY
			SELF:__DBF:Orders := {}

		ENDIF

		IF SELF:__DBF:IndexNames == NULL_ARRAY
			SELF:__DBF:IndexNames := {}

		ENDIF

		// init the USUALs to NILs
		SELF:cbOrdExpr  	:= NIL
		SELF:uForBlock   := NIL
		SELF:uWhileBlock := NIL
		SELF:uEvalBlock  := NIL
		SELF:aKeyInfo   	:= {}

	ELSE
		SELF:oDBF := NULL_OBJECT

	ENDIF
	RETURN 

ACCESS Interval         
	RETURN SELF:nStep

ASSIGN Interval( nDWord )     
	// conditional index
	IF Empty( nDWord ) .OR. !IsNumeric( nDWord )
		SELF:nStep := 0
	ELSE
		SELF:nStep := nDWord
		SELF:lIsCond := TRUE
	ENDIF
	RETURN 

ACCESS IsCond           
	RETURN SELF:lIsCond

ASSIGN IsCond( lLogic )       
	// conditional index
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lIsCond := FALSE
	ELSE
		SELF:lIsCond := lLogic
	ENDIF
	RETURN 

ACCESS KeyInfo      
	RETURN SELF:aKeyInfo

ASSIGN KeyInfo( aKeyInfo )        

	IF Empty( aKeyInfo ) .OR. !IsArray( aKeyInfo )
		SELF:aKeyInfo := NULL_ARRAY
	ELSE
		SELF:aKeyInfo := aKeyInfo
	ENDIF
	RETURN 

ACCESS LockOffSet   
	RETURN SELF:nLockOffSet

ACCESS NoOptimize       
	RETURN SELF:lNoOpt

ASSIGN NoOptimize( lLogic )   
	// conditional index
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lNoOpt := FALSE
	ELSE
		SELF:lNoOpt := lLogic
		SELF:lIsCond := TRUE
	ENDIF
	RETURN 

METHOD OrderAdd( oFS, uOrder )    
	LOCAL lRetCode      AS LOGIC
	LOCAL aFullPath     AS ARRAY
	LOCAL cDrive        AS STRING
	LOCAL cPath         AS STRING
	LOCAL cFile         AS STRING
	LOCAL cExt          AS STRING
	LOCAL cRDD          AS STRING
	LOCAL oDBFSpec      AS DbFileSpec
	//LOCAL oSelf         AS OrderSpec
	LOCAL cAlias		AS STRING

	IF IsObject(oFS) .and. __Usual.ToObject(oFS) IS FileSpec VAR oFS2
		cDrive      := oFS2:Drive
		cPath       := oFS2:Path
		cFile       := oFS2:FileName
		cExt        := oFS2:Extension

		// default drive and path to where DBF file is
		IF Empty( cDrive )
			cDrive := SELF:oDBF:Drive

		ENDIF

		IF Empty( cPath )
			cPath := SELF:oDBF:Path

		ENDIF


		// build full path
		IF SubStr2( cPath, SLen( cPath ) ) == "\"
			cFile := cDrive + cPath + cFile + cExt

		ELSE
			cFile := cDrive + cPath + "\" + cFile + cExt

		ENDIF

	ELSE
		IF Empty( oFS ) .OR. !IsString( oFS )
			RETURN FALSE
		ENDIF

		oDBFSpec := SELF:oDBF
		cFile := Upper( oFS )
		aFullPath := ArrayNew( 4 )
		__SplitPath( oDBFSpec, cFile, aFullPath )

		cDrive      := aFullPath[ 1 ]
		cPath       := aFullPath[ 2 ]
		cFile       := aFullPath[ 3 ]
		cExt        := aFullPath[ 4 ]

		// build full path
		IF SubStr2( cPath, SLen( cPath ) ) == "\"
			cFile := cDrive + cPath + cFile + cExt

		ELSE
			cFile := cDrive + cPath + "\" + cFile + cExt

		ENDIF

	ENDIF

	cRDD  := SELF:oDBF:RDD_Name
	//oSelf := SELF

	cAlias := Symbol2String( __ConstructUniqueAlias( SELF:oDBF:FileName ) )

	IF cRDD != "DBFNTX"

		IF SELF:__OpenDb( cAlias )
			// auto-open index file
			IF ( cAlias )->( DbOrderInfo( DBOI_FILEHANDLE ) ) > 0
				lRetCode := ( cAlias )->( DbSetOrder ( uOrder ) )

			ELSE
				( cAlias )->( OrdListAdd( cFile ) )
				lRetCode := ( cAlias )->( DbSetOrder( uOrder ) ) 

			ENDIF

			IF lRetCode
				SELF:__OrderSetInfo( cAlias )

			ENDIF

			( cAlias )->( DbCloseArea() ) 

		ENDIF

	ELSE
		IF SELF:__OpenDb( NULL_STRING )

			lRetCode := ( cAlias )->( DbSetIndex( cFile ) )

			IF lRetCode
				SELF:__OrderSetInfo( cAlias )

			ENDIF

			( cAlias )->( DbCloseArea() ) 

		ENDIF

	ENDIF

	RETURN lRetCode

ACCESS OrderBlock       
	RETURN SELF:cbOrdExpr

ASSIGN OrderBlock( cbCodeBlock )          

	IF Empty( cbCodeBlock ) .OR. !__CanEval( cbCodeBlock )
		SELF:cbOrdExpr := NIL
	ELSE
		SELF:cbOrdExpr := cbCodeBlock
	ENDIF
	RETURN

METHOD OrderCreate( oFS, cOrder, cKeyValue, cbKeyValue, lUnique ) 
	LOCAL lRetCode      AS LOGIC
	LOCAL aFullPath     AS ARRAY
	LOCAL cDrive        AS STRING
	LOCAL cPath         AS STRING
	LOCAL cFile         AS STRING
	LOCAL cExt          AS STRING
	LOCAL cRDD          AS STRING
	LOCAL lOldHPLock    AS LOGIC
	LOCAL oDBFSpec      AS DbFileSpec
	LOCAL cAlias		AS STRING
	LOCAL nNext         AS USUAL
	LOCAL nRec          AS USUAL

	IF IsObject(oFS) .and. __Usual.ToObject(oFS) IS FileSpec VAR oFS2
		cDrive      :=oFS2:Drive
		cPath       :=oFS2:Path
		cFile       :=oFS2:FileName
		cExt        :=oFS2:Extension

		// default drive and path to where DBF file is
		IF Empty( cDrive )
			cDrive := SELF:oDBF:Drive

		ENDIF

		IF Empty( cPath )
			cPath := SELF:oDBF:Path

		ENDIF
		// build full path
		IF SubStr2( cPath, SLen( cPath ) ) == "\"
			cFile := cDrive + cPath + cFile + cExt
		ELSE
			cFile := cDrive + cPath + "\" + cFile + cExt
		ENDIF
	ELSE
		IF Empty( oFS ) .OR. !IsString( oFS )
			oFS := SELF:cFileName
			IF oFS == NULL_STRING
				RETURN FALSE
			ENDIF
		ENDIF

		oDBFSpec := SELF:oDBF
		cFile := Upper( oFS )
		aFullPath := ArrayNew( 4 )
		__SplitPath( oDBFSpec, cFile, aFullPath )

		cDrive      := aFullPath[ 1 ]
		cPath       := aFullPath[ 2 ]
		cFile       := aFullPath[ 3 ]
		cExt        := aFullPath[ 4 ]

		// build full path
		IF SubStr2( cPath, SLen( cPath ) ) == "\"
			cFile := cDrive + cPath + cFile + cExt
		ELSE
			cFile := cDrive + cPath + "\" + cFile + cExt
		ENDIF
	ENDIF

	cRDD := SELF:oDBF:RDD_Name

	// NTX only has single orders
	IF cRDD == "DBFNTX"
		cOrder := NIL
	ELSEIF Empty( cOrder ) .OR. !IsString( cOrder )
		cOrder := SELF:cOrdName
	ENDIF

	IF Empty( cKeyValue ) .OR. !IsString( cKeyValue )
		cKeyValue := SELF:cOrdExpr
	ENDIF

	IF IsNil( cbKeyValue ) .OR. !__CanEval( cbKeyValue )
		cbKeyValue := SELF:cbOrdExpr
	ENDIF

	IF IsNil( cbKeyValue )
		IF Empty( cKeyValue )
			RETURN FALSE
		ENDIF
		cbKeyValue := &( "{||" + cKeyValue + "}" )
	ENDIF

	IF Empty( lUnique )
		lUnique := SELF:lUnique
	ENDIF

	IF SELF:uForCond != NULL_STRING
		IF IsNil( SELF:uForBlock )
			SELF:uForBlock := &( "{||" + SELF:uForCond + "}" )
		ENDIF
	ENDIF

	IF SELF:nNext = 0
		nNext := NIL
	ELSE
		nNext := SELF:nNext
	ENDIF

	IF SELF:nRecno = 0
		nRec := NIL
	ELSE
		nRec := SELF:nRecno
	ENDIF

	cAlias := Symbol2String( __ConstructUniqueAlias( SELF:oDBF:FileName ) )
	IF SELF:__OpenDb( cAlias )

		// if any condition ASSIGN was made, then call OrdCondSet()
		IF SELF:lIsCond
			lRetCode := ( cAlias )->( OrdCondSet( SELF:uForCond,    ;
				SELF:uForBlock,   ;
				SELF:lAll,       ;
				SELF:uWhileBlock, ;
				SELF:uEvalBlock,  ;
				SELF:nStep,      ;
				SELF:nStart,     ;
				nNext,           ;
				nRec,            ;
				SELF:lRest,      ;
				SELF:lDescend,   ;
				SELF:lAdd,       ;
				SELF:lCurrent,   ;
				SELF:lCustom,    ;
				SELF:lNoOpt      ) )

		ENDIF

		// set HP locking for this order
		IF cRDD == "DBFNTX"
			lOldHPLock := ( cAlias )->( IndexHPLock( SELF:lHPLock ) )
		ENDIF

		lRetCode := ( cAlias )->( VoDbOrdCreate ( cFile, cOrder, cKeyValue, cbKeyValue, lUnique, NULL ) )

		IF cRDD == "DBFNTX"
			( cAlias )->( IndexHPLock( lOldHPLock ) )
		ENDIF
		IF lRetCode
			SELF:__OrderSetInfo( cAlias )
		ENDIF
		( cAlias )->( DbCloseArea() )
	ENDIF
	RETURN lRetCode

METHOD OrderDelete( uOrder )  
	LOCAL lRetCode  AS LOGIC
	LOCAL cFullPath AS STRING
	LOCAL cRDD      AS STRING
	LOCAL cAlias	AS STRING
	// LOCAL oSelf     AS OrderSpec      dcaton 070430 never used
	LOCAL i         AS DWORD
	LOCAL nOrders, nFiles   AS DWORD
	LOCAL nOrdCount, nHandle AS DWORD

	cRDD := SELF:oDBF:RDD_Name
	// oSelf := SELF         // dcaton 070430 never used

	IF cRDD != "DBFNTX"
		cAlias := Symbol2String( __ConstructUniqueAlias( SELF:oDBF:FileName ) )
		cFullPath := SELF:cFileName
		IF Empty( uOrder )
			uOrder := 1
		ENDIF
		IF SELF:__OpenDb( cAlias )
			nHandle := DbOrderInfo( DBOI_FILEHANDLE )
			IF nHandle > 0
				lRetCode := ( cAlias )->( DbDeleteOrder( uOrder, cFullPath ) )
			ELSE
				( cAlias )->( OrdListAdd( cFullPath ) )//, uOrder ) )
				lRetCode := ( cAlias )->( DbDeleteOrder( uOrder, cFullPath ) )
			ENDIF

			IF lRetCode
				nOrders := ALen( SELF:__DBF:Orders )
				FOR i := 1 UPTO nOrders
                    VAR os := (OrderSpec) SELF:__DBF:Orders[ i ]
					IF os:FileName == SELF:cFileName
						ADel( SELF:__DBF:Orders, i )
						ASize( SELF:__DBF:Orders, nOrders - 1 )
						EXIT
					ENDIF
				NEXT
			ENDIF

			nOrdCount := ( cAlias )->( DbOrderInfo( DBOI_ORDERCOUNT, SELF:cFileName ) )
			( cAlias )->( DbCloseArea() )

			// DBFCDX deletes index file if order count == 0
			IF nOrdCount = 0
				nFiles := ALen( SELF:oDBF:IndexNames )
				FOR i := 1 UPTO nFiles
					IF SELF:oDBF:IndexNames[ i ] == SELF:cFileName
						ADel( SELF:oDBF:IndexNames, i )
						ASize( SELF:oDBF:IndexNames, nFiles - 1 )
						EXIT
					ENDIF
				NEXT
			ENDIF
		ENDIF
	ELSE
		// TODO: Make OrdDestroy() delete NTX like CDX does.
		lRetCode := FErase( SELF:cFileName  )
		IF lRetCode
			nFiles := ALen( SELF:oDBF:IndexNames )
			FOR i := 1 UPTO nFiles
				IF SELF:oDBF:IndexNames[ i ] == SELF:cFileName
					ADel( SELF:oDBF:IndexNames, i )
					ASize( SELF:oDBF:IndexNames, nFiles - 1 )
					EXIT
				ENDIF
			NEXT
			nOrders := ALen( SELF:oDBF:Orders )
			FOR i := 1 UPTO nOrders
                VAR os := (OrderSpec) SELF:__DBF:Orders[ i ]
	            IF os:FileName == SELF:cFileName
					ADel( SELF:oDBF:Orders, i )
					ASize( SELF:oDBF:Orders, nFiles - 1 )
					EXIT
				ENDIF
			NEXT
		ENDIF
	ENDIF

	// clear everything out
	IF lRetCode
		SELF:__OrderSetInfo( NULL_STRING )
	ENDIF
	RETURN lRetCode


ACCESS OrderExpr        

	RETURN SELF:cOrdExpr

ASSIGN OrderExpr( cExpression )           

	IF Empty( cExpression ) .OR. !IsString( cExpression )
		SELF:cOrdExpr := NULL_STRING
	ELSE
		SELF:cOrdExpr := cExpression
	ENDIF
	RETURN 

ACCESS OrderName        
	RETURN SELF:cOrdName

ASSIGN OrderName( cOrderName )            

	IF Empty( cOrderName ) .OR. !IsString( cOrderName )
		SELF:cOrdName := NULL_STRING
	ELSE
		SELF:cOrdName := cOrderName
	ENDIF
	RETURN 

ACCESS RecNo  AS DWORD      
	RETURN SELF:nRecno

ASSIGN RecNo ( nDWord AS DWORD)        
	// conditional index
	IF Empty( nDWord ) 
		SELF:nRecno := 0
	ELSE
		SELF:nRecno := nDWord
		SELF:lIsCond := TRUE
	ENDIF
	RETURN 

ACCESS Records      
	RETURN SELF:nNext

ASSIGN Records( nDWord )      
	// conditional index
	IF Empty( nDWord ) .OR. !IsNumeric( nDWord )
		SELF:nNext := 0
	ELSE
		SELF:nNext := nDWord
		SELF:lIsCond := TRUE
	ENDIF
	RETURN 

ACCESS Rest         
	RETURN SELF:lRest

ASSIGN Rest( lLogic )     
	// conditional index
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lRest := FALSE
	ELSE
		SELF:lRest := lLogic
		SELF:lIsCond := TRUE
	ENDIF
	RETURN 

ACCESS Start        
	RETURN SELF:nStart

ASSIGN Start( nDWord )        
	// conditional index
	IF Empty( nDWord ) .OR. !IsNumeric( nDWord )
		SELF:nStart := 0
	ELSE
		SELF:nStart := nDWord
		SELF:lIsCond := TRUE
	ENDIF
	RETURN 

ACCESS StrictRead   
	RETURN SELF:lStrictRead

ACCESS Unique       
	RETURN SELF:lUnique

ASSIGN Unique( lLogic )           

	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lUnique := FALSE
	ELSE
		SELF:lUnique := lLogic
	ENDIF
	RETURN 

ACCESS WhileBlock   
	RETURN SELF:uWhileBlock

ASSIGN WhileBlock( cbCodeBlock )  
	// conditional index
	IF Empty( cbCodeBlock ) .OR. !__CanEval( cbCodeBlock )
		SELF:uWhileBlock := NIL
	ELSE
		SELF:uWhileBlock := cbCodeBlock
		SELF:lIsCond := TRUE
	ENDIF
	RETURN 
END CLASS

