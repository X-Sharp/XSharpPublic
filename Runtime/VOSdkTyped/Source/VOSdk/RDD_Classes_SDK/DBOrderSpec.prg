//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


#pragma warnings(165, off)
/// <include file="Rdd.xml" path="doc/OrderSpec/*" />
[XSharp.Internal.TypesChanged];
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
	PROTECT uForCond        AS STRING
	PROTECT uForBlock       AS USUAL
	PROTECT uWhileBlock     AS USUAL
	PROTECT uEvalBlock      AS USUAL
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


 /// <exclude />
	METHOD __OpenDb      ( cAlias AS STRING )	AS LOGIC STRICT


	IF SLen( cAlias ) > 0
		cAlias := Symbol2String( __ConstructUniqueAlias( SELF:oDBF:FileName ) )
	ENDIF


	RETURN DbUseArea( TRUE, SELF:oDBF:RDDs, SELF:oDBF:FullPath, cAlias, FALSE, FALSE )


 /// <exclude />
METHOD __OrderSetInfo( cAlias AS STRING ) AS VOID STRICT
	// Gets order information and assigns info into the OrderSpec Object.
	// Assumes that the workarea defined by cAlias is already opened and the order
	// has been set.
	//
	LOCAL aKeyInfo      AS ARRAY
	LOCAL cRDD          AS STRING


	IF SLen( cAlias )  > 0
		aKeyInfo := ArrayNew( 4 )


		cRDD := SELF:DBF:RDD_Name


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
		SELF:DBF:IndexNames     := ( cAlias )->( DbOrderInfo( DBOI_FULLPATH ) )
		SELF:OrderName          := ( cAlias )->( DbOrderInfo( DBOI_NAME ) )
		SELF:OrderExpr          := ( cAlias )->( DbOrderInfo( DBOI_EXPRESSION ) )
		SELF:OrderBlock         := &( "{||" + SELF:OrderExpr + "}" )
		SELF:Unique             := ( cAlias )->( DbOrderInfo( DBOI_UNIQUE ) )


		SELF:DBF:Orders         := SELF


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


/// <include file="Rdd.xml" path="doc/OrderSpec.Add/*" />
ACCESS Add   AS LOGIC
	RETURN SELF:lAdd


/// <include file="Rdd.xml" path="doc/OrderSpec.Add/*" />
ASSIGN Add( lLogic  AS LOGIC)
	// conditional index
	SELF:lAdd := lLogic
	SELF:lIsCond := TRUE
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.All/*" />
ACCESS All  AS LOGIC
	RETURN SELF:lAll


/// <include file="Rdd.xml" path="doc/OrderSpec.All/*" />
ASSIGN All( lLogic AS LOGIC)
	SELF:lAll := lLogic
	SELF:lIsCond := TRUE
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.AutoOpen/*" />
ACCESS AutoOpen AS LOGIC
	RETURN SELF:lAutoOpen


/// <include file="Rdd.xml" path="doc/OrderSpec.AutoOrder/*" />
ACCESS AutoOrder  AS LONG
	RETURN SELF:nAutoOrder


/// <include file="Rdd.xml" path="doc/OrderSpec.AutoShare/*" />
ACCESS AutoShare  AS LONG
	RETURN SELF:nAutoShare


/// <include file="Rdd.xml" path="doc/OrderSpec.Current/*" />
ACCESS Current   AS LOGIC
	RETURN SELF:lCurrent


/// <include file="Rdd.xml" path="doc/OrderSpec.Current/*" />
ASSIGN Current( lLogic  AS LOGIC)
	// conditional index
	SELF:lCurrent := lLogic
	SELF:lIsCond := TRUE


	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.Custom/*" />
ACCESS Custom   AS LOGIC
	RETURN SELF:lCustom


/// <include file="Rdd.xml" path="doc/OrderSpec.Custom/*" />
ASSIGN Custom( lLogic  AS LOGIC)
	// conditional index
	SELF:lCustom := lLogic
	SELF:lIsCond := TRUE


	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.DBF/*" />
ACCESS DBF AS DbFileSpec
	//
	// Returns the DBFileSpec object that this OrderSpec object
	// belongs to.
	//
	RETURN SELF:oDBF


/// <include file="Rdd.xml" path="doc/OrderSpec.Descend/*" />
ACCESS Descend AS LOGIC
	RETURN SELF:lDescend


/// <include file="Rdd.xml" path="doc/OrderSpec.Descend/*" />
ASSIGN Descend( lLogic  AS LOGIC)
	// conditional index
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lDescend := FALSE


	ELSE
		SELF:lDescend := lLogic
		SELF:lIsCond := TRUE


	ENDIF


	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.EvalBlock/*" />
ACCESS EvalBlock  AS USUAL
	RETURN SELF:uEvalBlock


/// <include file="Rdd.xml" path="doc/OrderSpec.EvalBlock/*" />
ASSIGN EvalBlock( cbCodeBlock AS USUAL)
	// conditional index
	IF Empty( cbCodeBlock ) .OR. !__CanEval( cbCodeBlock )
		SELF:uEvalBlock := NIL
	ELSE
		SELF:uEvalBlock := cbCodeBlock
		SELF:lIsCond := TRUE
	ENDIF


	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.FileName/*" />
ACCESS FileName AS STRING
	RETURN SELF:cFileName


/// <include file="Rdd.xml" path="doc/OrderSpec.FileName/*" />
ASSIGN FileName( cName AS STRING)
	LOCAL aFullPath AS ARRAY
	LOCAL cFileName AS STRING
	LOCAL oDBFS     AS DbFileSpec


	IF Empty( cName )
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


/// <include file="Rdd.xml" path="doc/OrderSpec.ForBlock/*" />
ACCESS ForBlock AS USUAL
	RETURN SELF:uForBlock


/// <include file="Rdd.xml" path="doc/OrderSpec.ForBlock/*" />
ASSIGN ForBlock( cbCodeBlock AS USUAL)
	// conditional index
	IF Empty( cbCodeBlock ) .OR. !__CanEval( cbCodeBlock )
		SELF:uForBlock := NIL


	ELSE
		SELF:uForBlock := cbCodeBlock
		SELF:lIsCond := TRUE


	ENDIF


	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.ForCond/*" />
ACCESS ForCond AS USUAL
	RETURN SELF:uForCond


/// <include file="Rdd.xml" path="doc/OrderSpec.ForCond/*" />
ASSIGN ForCond( cForCondition AS USUAL)
	// conditional index
	IF Empty( cForCondition ) .OR. !IsString( cForCondition )
		SELF:uForCond := NULL_STRING
	ELSE
		SELF:uForCond := cForCondition
		SELF:lIsCond := TRUE
	ENDIF
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.HPLock/*" />
ACCESS HPLock AS LOGIC
	RETURN SELF:lHPLock


/// <include file="Rdd.xml" path="doc/OrderSpec.HPLock/*" />
ASSIGN HPLock( lLogic AS LOGIC)
	SELF:lHPLock := lLogic
	RETURN




/// <include file="Rdd.xml" path="doc/OrderSpec.ctor/*" />
CONSTRUCTOR( )
   SELF(NULL_OBJECT)


/// <include file="Rdd.xml" path="doc/OrderSpec.ctor/*" />
CONSTRUCTOR( oDBFS AS DbFileSpec)
      SELF:oDBF := oDBFS
      IF SELF:oDBF != NULL_OBJECT
         IF SELF:DBF:Orders == NULL_ARRAY
            SELF:DBF:Orders := {}
         ENDIF
         IF SELF:DBF:IndexNames == NULL_ARRAY
            SELF:DBF:IndexNames := {}
         ENDIF
      ENDIF
      // init the USUALs to NILs
      SELF:cbOrdExpr  	:= NIL
      SELF:uForBlock   := NIL
      SELF:uWhileBlock := NIL
      SELF:uEvalBlock  := NIL
      SELF:aKeyInfo   	:= {}
   RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.Interval/*" />
ACCESS Interval AS DWORD
	RETURN SELF:nStep


/// <include file="Rdd.xml" path="doc/OrderSpec.Interval/*" />
ASSIGN Interval( nDWord AS DWORD)
	// conditional index
	SELF:nStep := nDWord
	SELF:lIsCond := TRUE
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.IsCond/*" />
ACCESS IsCond AS LOGIC
	RETURN SELF:lIsCond


/// <include file="Rdd.xml" path="doc/OrderSpec.IsCond/*" />
ASSIGN IsCond( lLogic AS LOGIC)
	// conditional index
	SELF:lIsCond := lLogic
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.KeyInfo/*" />
ACCESS KeyInfo AS ARRAY
	RETURN SELF:aKeyInfo


/// <include file="Rdd.xml" path="doc/OrderSpec.KeyInfo/*" />
ASSIGN KeyInfo( aKeyInfo AS ARRAY)
	SELF:aKeyInfo := aKeyInfo
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.LockOffSet/*" />
ACCESS LockOffSet  AS LONG
	RETURN SELF:nLockOffSet


/// <include file="Rdd.xml" path="doc/OrderSpec.NoOptimize/*" />
ACCESS NoOptimize   AS LOGIC
	RETURN SELF:lNoOpt


/// <include file="Rdd.xml" path="doc/OrderSpec.NoOptimize/*" />
ASSIGN NoOptimize( lLogic AS LOGIC)
	// conditional index
	IF Empty( lLogic ) .OR. !IsLogic( lLogic )
		SELF:lNoOpt := FALSE
	ELSE
		SELF:lNoOpt := lLogic
		SELF:lIsCond := TRUE
	ENDIF
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.OrderAdd/*" />
METHOD OrderAdd( oFS, uOrder ) AS LOGIC   CLIPPER
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


/// <include file="Rdd.xml" path="doc/OrderSpec.OrderBlock/*" />
ACCESS OrderBlock    AS USUAL
	RETURN SELF:cbOrdExpr


/// <include file="Rdd.xml" path="doc/OrderSpec.OrderBlock/*" />
ASSIGN OrderBlock( cbCodeBlock AS USUAL)


	IF Empty( cbCodeBlock ) .OR. !__CanEval( cbCodeBlock )
		SELF:cbOrdExpr := NIL
	ELSE
		SELF:cbOrdExpr := cbCodeBlock
	ENDIF
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.OrderCreate/*" />
METHOD OrderCreate( oFS, cOrder, cKeyValue, cbKeyValue, lUnique ) AS LOGIC CLIPPER
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


/// <include file="Rdd.xml" path="doc/OrderSpec.OrderDelete/*" />
METHOD OrderDelete( uOrder )  AS LOGIC CLIPPER
	LOCAL lRetCode  AS LOGIC
	LOCAL cFullPath AS STRING
	LOCAL cRDD      AS STRING
	LOCAL cAlias	AS STRING
	LOCAL i         AS DWORD
	LOCAL nOrders, nFiles   AS DWORD
	LOCAL nOrdCount, nHandle AS DWORD


	cRDD := SELF:oDBF:RDD_Name


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
				nOrders := ALen( SELF:DBF:Orders )
				FOR i := 1 UPTO nOrders
                    VAR os := (OrderSpec) SELF:DBF:Orders[ i ]
					IF os:FileName == SELF:cFileName
						ADel( SELF:DBF:Orders, i )
						ASize( SELF:DBF:Orders, nOrders - 1 )
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
                VAR os := (OrderSpec) SELF:DBF:Orders[ i ]
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




/// <include file="Rdd.xml" path="doc/OrderSpec.OrderExpr/*" />
ACCESS OrderExpr AS STRING
	RETURN SELF:cOrdExpr


/// <include file="Rdd.xml" path="doc/OrderSpec.OrderExpr/*" />
ASSIGN OrderExpr( cExpression AS STRING)
	SELF:cOrdExpr := cExpression
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.OrderName/*" />
ACCESS OrderName  AS STRING
	RETURN SELF:cOrdName


/// <include file="Rdd.xml" path="doc/OrderSpec.OrderName/*" />
ASSIGN OrderName( cOrderName AS STRING)
	SELF:cOrdName := cOrderName
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.Recno/*" />
ACCESS RecNo  AS DWORD
	RETURN SELF:nRecno


/// <include file="Rdd.xml" path="doc/OrderSpec.Recno/*" />
ASSIGN RecNo ( nDWord AS DWORD)
	// conditional index
	IF Empty( nDWord )
		SELF:nRecno := 0
	ELSE
		SELF:nRecno := nDWord
		SELF:lIsCond := TRUE
	ENDIF
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.Records/*" />
ACCESS Records  AS DWORD
	RETURN SELF:nNext


/// <include file="Rdd.xml" path="doc/OrderSpec.Records/*" />
ASSIGN Records( nDWord AS DWORD)
	// conditional index
	SELF:nNext := nDWord
	SELF:lIsCond := TRUE
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.Rest/*" />
ACCESS Rest  AS LOGIC
	RETURN SELF:lRest


/// <include file="Rdd.xml" path="doc/OrderSpec.Rest/*" />
ASSIGN Rest( lLogic AS LOGIC)
	// conditional index
	SELF:lRest := lLogic
	SELF:lIsCond := TRUE
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.Start/*" />
ACCESS Start  AS DWORD
	RETURN SELF:nStart


/// <include file="Rdd.xml" path="doc/OrderSpec.Start/*" />
ASSIGN Start( nDWord AS DWORD)
	// conditional index
	SELF:nStart := nDWord
	SELF:lIsCond := TRUE
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.StrictRead/*" />
ACCESS StrictRead  AS LOGIC
	RETURN SELF:lStrictRead


/// <include file="Rdd.xml" path="doc/OrderSpec.Unique/*" />
ACCESS Unique  AS LOGIC
	RETURN SELF:lUnique


/// <include file="Rdd.xml" path="doc/OrderSpec.Unique/*" />
ASSIGN Unique( lLogic  AS LOGIC)
	SELF:lUnique := lLogic
	RETURN


/// <include file="Rdd.xml" path="doc/OrderSpec.WhileBlock/*" />
ACCESS WhileBlock  AS USUAL
	RETURN SELF:uWhileBlock


/// <include file="Rdd.xml" path="doc/OrderSpec.WhileBlock/*" />
ASSIGN WhileBlock( cbCodeBlock AS USUAL)
	// conditional index
	IF Empty( cbCodeBlock ) .OR. !__CanEval( cbCodeBlock )
		SELF:uWhileBlock := NIL
	ELSE
		SELF:uWhileBlock := cbCodeBlock
		SELF:lIsCond := TRUE
	ENDIF
	RETURN
END CLASS


