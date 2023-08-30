//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


#pragma warnings(165, off)
/// <include file="Rdd.xml" path="doc/DbFileSpec/*" />
[XSharp.Internal.TypesChanged];
CLASS DbFileSpec INHERIT FileSpec
	PROTECT DBF AS ARRAY
	PROTECT cMemFileName AS STRING
	PROTECT cMemFileExt AS STRING
	PROTECT Memo AS ARRAY
	PROTECT cRDD_Name AS STRING
	PROTECT uRDD_Version AS USUAL
	PROTECT aRDDs AS ARRAY
	PROTECT lAnsi AS LOGIC
	PROTECT nFCount AS INT
	PROTECT nHeaderSize AS INT
	PROTECT dLastUpDate AS DATE
	PROTECT aDbStruct AS ARRAY
	PROTECT nRecCount AS LONGINT
	PROTECT nRecSize AS INT
	PROTECT nRLockCount AS INT
	PROTECT nMemBlockSize AS INT
	PROTECT bForBlock AS USUAL
	PROTECT bWhileBlock AS USUAL
	PROTECT aFields AS ARRAY
	PROTECT nNext AS  LONG
	PROTECT nRecord AS  LONG
	PROTECT lRest AS LOGIC
	PROTECT cDelim AS STRING
	PROTECT lSDF AS LOGIC
	PROTECT aHidRDDs AS ARRAY
	PROTECT aOrders AS ARRAY
	PROTECT aIndexNames AS ARRAY


/// <include file="Rdd.xml" path="doc/DbFileSpec.ctor/*" />
CONSTRUCTOR( oFS AS FileSpec, cDriver := "" AS STRING, _aRDDs := NULL_ARRAY AS ARRAY)
    SELF(oFS:FullPath, cDriver, _aRDDs)


/// <include file="Rdd.xml" path="doc/DbFileSpec.ctor/*" />
CONSTRUCTOR( cFullPath := "" AS STRING, cDriver := "" AS STRING, _aRDDs := NULL_ARRAY AS ARRAY)
	SELF:aOrders := { }
	SELF:aIndexNames := { }




	IF Empty( cDriver )
		SELF:cRDD_Name := RddInfo( _SET_DEFAULTRDD )
	ELSE
		SELF:cRDD_Name := cDriver
	ENDIF


	SELF:aRDDs := _aRDDs


	IF ! String.IsNullOrEmpty( cFullPath )


		SUPER( cFullPath )


		IF SELF:cFSExtension == NULL_STRING
			SELF:cFSExtension := ".DBF"
		ENDIF


		IF File( SELF:FullPath )
			SELF:DBFSGetInfo()
		ENDIF
	ENDIF


	RETURN




 /// <exclude />
METHOD __MemFullPath() AS STRING STRICT
	LOCAL cPath AS STRING


	IF SELF:MemFileName == NULL_STRING .AND. SELF:MemFileExt == NULL_STRING
		RETURN NULL_STRING
	ENDIF


	cPath := SELF:cFSPath


	IF cPath == NULL_STRING .OR. SubStr2( cPath, SLen( cPath ) ) == "\"
		RETURN SELF:cFSDrive + cPath + SELF:MemFileName + SELF:MemFileExt
	ENDIF
	RETURN SELF:cFSDrive + cPath + "\" + SELF:MemFileName + SELF:MemFileExt


/// <include file="Rdd.xml" path="doc/DbFileSpec.Copy/*" />
METHOD Copy( oDBFSTarget AS USUAL, lIDX := NIL AS USUAL, lName := NIL AS USUAL) AS LOGIC STRICT
	LOCAL cMEMTarget AS STRING
	LOCAL cTargetPath AS STRING
	LOCAL cFileName AS STRING
	LOCAL cDrive AS STRING
	LOCAL cPath AS STRING
	LOCAL cNewName AS STRING
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL cbOldErr AS USUAL
	LOCAL aFullPath AS ARRAY
	LOCAL oSelf AS DbFileSpec
	LOCAL i AS DWORD
	LOCAL nFiles AS DWORD


	IF Empty( lName ) .OR. ! IsLogic( lName )
		lName := FALSE
	ENDIF

    IF oDBFSTarget IS FileSpec var fs
	    lRetCode := SUPER:Copy( fs, lName )
    ELSE
	    lRetCode := SUPER:Copy( (STRING) oDBFSTarget, lName )
    ENDIF


	IF lRetCode
		cFileName := SELF:MemFileName
		cbOldErr := ErrorBlock( { | oErr | _Break( oErr ) } )


		BEGIN SEQUENCE
			IF cFileName != NULL_STRING
				IF IsObject(oDBFSTarget) .and. __Usual.ToObject(oDBFSTarget) IS FileSpec VAR oFS
					cDrive      := oFS:Drive
					cPath       := oFS:Path
					cFileName   := oFS:FileName
					IF cFileName == NULL_STRING
						cFileName := SELF:MemFileName
					ENDIF
				ELSE
					IF ! IsString( oDBFSTarget )
						RETURN FALSE
					ENDIF
					oDBFSTarget := Upper( oDBFSTarget )
					oSelf := SELF
					aFullPath := ArrayNew( 4 )
					__SplitPath( oSelf, oDBFSTarget, aFullPath )
					cDrive := aFullPath[1]
					cPath := aFullPath[2]
					cFileName := aFullPath[3]
					IF cFileName == NULL_STRING
						cFileName := SELF:MemFileName
					ENDIF
				ENDIF


				IF SubStr2( cPath, SLen( cPath ) ) == "\"
					cTargetPath := cDrive + cPath
				ELSE
					cTargetPath := cDrive + cPath + "\"
				ENDIF


				IF lName
					IF ( cNewName := __NewName( cTargetPath + cFileName + SELF:MemFileExt ) ) != NULL_STRING
						cFileName := cNewName
					ENDIF
				ENDIF


				cMEMTarget := cTargetPath + cFileName + SELF:MemFileExt
				lRetCode := FCopy( SELF:MemFullPath, cMEMTarget )


			ENDIF


			IF Empty( lIDX ) .OR. ! IsLogic( lIDX )
				lIDX := TRUE
			ENDIF


			IF lRetCode .AND. lIDX .AND. ! Empty( SELF:aIndexNames )
				nFiles := ALen( SELF:aIndexNames )
				FOR i := 1 UPTO nFiles
					cPath := cTargetPath
					cFileName := SELF:aIndexNames[i]
					cPath := cTargetPath + SubStr2( cFileName, RAt2( "\", cFileName ) + 1 )
					lRetCode := FCopy( cFileName, cPath )
				NEXT
			ENDIF


		RECOVER USING oError
			ErrorBlock( cbOldErr )
            LOCAL oErr := oError as Error
			IF oErr:OSCode != 0
				oErr:Description := VO_Sprintf( __CAVOSTR_SYSLIB_DOS_ERROR, NTrim( oErr:OSCode ) + ;
					" ( " + DosErrString( oErr:OSCode ) + " )" )
			ENDIF


			SELF:oErrorInfo := oErr
			lRetCode := FALSE


		END SEQUENCE


		ErrorBlock( cbOldErr )


	ENDIF


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbFileSpec.CopyTo/*" />
METHOD CopyTo( oFS, cDriver, lWantAnsi ) AS USUAL CLIPPER
	LOCAL cTarget AS STRING
	LOCAL cAlias AS STRING
	LOCAL aFullPath AS ARRAY
	LOCAL lRetCode AS LOGIC
	LOCAL oSelf AS DbFileSpec
	LOCAL lOldAnsi AS LOGIC
	LOCAL oError AS USUAL
	LOCAL cbOldErr AS USUAL
	LOCAL nNext AS USUAL
	LOCAL nRec AS USUAL


	IF IsObject(oFS) .AND. __Usual.ToObject(oFS) IS FileSpec VAR oFsParam
		oFS := oFsParam:FullPath
	ENDIF


	IF Empty( oFS ) .OR. ! IsString( oFS )
		RETURN FALSE


	ELSE
		aFullPath := ArrayNew( 4 )
		cTarget := Upper( oFS )
		oSelf := SELF
		__SplitPath( oSelf, cTarget, aFullPath )


		IF SELF:cDelim == NULL_STRING .AND. ! SELF:lSDF .AND. aFullPath[4] == NULL_STRING
			aFullPath[4] := SELF:cFSExtension
		ENDIF


		IF SubStr2( aFullPath[2], SLen( aFullPath[2] ) ) == "\"
			cTarget := aFullPath[1] + aFullPath[2] + aFullPath[3] + aFullPath[4]
		ELSE
			cTarget := aFullPath[1] + aFullPath[2] + "\" + aFullPath[3] + aFullPath[4]
		ENDIF


	ENDIF


	cbOldErr := ErrorBlock( { | oErr | _Break( oErr ) } )


	BEGIN SEQUENCE
		IF Empty( lWantAnsi ) .OR. ! IsLogic( lWantAnsi )
			lOldAnsi := SetAnsi( SELF:lAnsi )
		ELSE
			lOldAnsi := SetAnsi( lWantAnsi )
		ENDIF


		IF SELF:nNext = 0
			nNext := NIL
		ELSE
			nNext := SELF:nNext
		ENDIF


		IF SELF:nRecord = 0
			nRec := NIL
		ELSE
			nRec := SELF:nRecord
		ENDIF


		cAlias := Symbol2String( __ConstructUniqueAlias( SELF:cFSFileName ) )


		IF ( lRetCode := DbUseArea( TRUE, SELF:aRDDs, SELF:FullPath, cAlias, TRUE, TRUE, NIL, NIL ) )
			IF SELF:cDelim == NULL_STRING .AND. ! SELF:lSDF
				lRetCode := ( cAlias ) -> DbCopy( cTarget,  ;
					SELF:aFields,  ;
					SELF:ForBlock,  ;
					SELF:WhileBlock,  ;
					nNext,  ;
					nRec,  ;
					SELF:lRest,  ;
					cDriver,  ;
					SELF:aHidRDDs )


			ELSEIF SELF:cDelim != NULL_STRING .AND. ! SELF:lSDF
				lRetCode := ( cAlias ) -> DbCopyDelim( cTarget,  ;
					SELF:cDelim,  ;
					SELF:aFields,  ;
					SELF:ForBlock,  ;
					SELF:WhileBlock,  ;
					nNext,  ;
					nRec,  ;
					SELF:lRest )


			ELSEIF SELF:cDelim == NULL_STRING .AND. SELF:lSDF
				lRetCode := ( cAlias ) -> DbCopySDF( cTarget,  ;
					SELF:aFields,  ;
					SELF:ForBlock,  ;
					SELF:WhileBlock,  ;
					nNext,  ;
					nRec,  ;
					SELF:lRest )


			ELSE
				lRetCode := FALSE


			ENDIF


			( cAlias ) -> ( DbCloseArea() )


		ENDIF


		SetAnsi( lOldAnsi )


	RECOVER USING oError
		ErrorBlock( cbOldErr )
        LOCAL oErr := oError as Error
		IF oErr:OSCode != 0
			oErr:Description := VO_Sprintf( __CAVOSTR_SYSLIB_DOS_ERROR, NTrim( oErr:OSCode ) + ;
				" ( " + DosErrString( oErr:OSCode ) + " )" )
		ENDIF


		SELF:oErrorInfo := oErr
		lRetCode := FALSE


	END SEQUENCE


	ErrorBlock( cbOldErr )


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbFileSpec.Create/*" />
METHOD Create( cFullPath, aDbStruct, cDriver, lWantAnsi, aRdds ) AS USUAL CLIPPER
	LOCAL cAlias AS STRING
	LOCAL lRetCode AS LOGIC
	LOCAL rddList AS _RddList
	LOCAL cPath AS STRING
	LOCAL cFileName AS STRING
	LOCAL cNewName AS STRING
	LOCAL aFullPath AS ARRAY
	LOCAL oSelf AS DbFileSpec
	LOCAL lOldAnsi AS LOGIC
	LOCAL cbOldErr AS USUAL
	LOCAL oError AS USUAL


	IF IsObject(cFullPath) .and. __Usual.ToObject(cFullPath) IS FileSpec  VAR oFS
		cFullPath := oFS:FullPath
	ENDIF


	cbOldErr := ErrorBlock( { | oErr | _Break( oErr ) } )


	BEGIN SEQUENCE
		IF IsArray( cFullPath )
			IF IsString( aDbStruct )
				cDriver := aDbStruct
			ENDIF
			aDbStruct := cFullPath
		ENDIF


		IF Empty( cFullPath ) .OR. ! IsString( cFullPath )
			IF SLen( SELF:cFSPath ) = RAt2( "\", SELF:cFSPath )
				cFullPath := SELF:cFSDrive + SELF:cFSPath + SELF:cFSFileName + SELF:cFSExtension
			ELSE
				cFullPath := SELF:cFSDrive + SELF:cFSPath + "\" + SELF:cFSFileName + SELF:cFSExtension
			ENDIF
		ENDIF


		IF Empty( aDbStruct ) .OR. ! IsArray( aDbStruct )
			cAlias := Symbol2String( __ConstructUniqueAlias( SELF:cFSFileName ) )
			IF DbUseArea( TRUE, SELF:cRDD_Name, SELF:__FullPathAcc(), cAlias, TRUE, TRUE, , , SELF:aRDDs )
				aDbStruct := DbStruct()
				( cAlias ) -> ( DbCloseArea() )
			ELSE
				RETURN FALSE
			ENDIF
		ENDIF


		aFullPath := ArrayNew( 4 )
		oSelf := SELF


		cFullPath := Upper( cFullPath )
		__SplitPath( oSelf, cFullPath, aFullPath )


		IF aFullPath[3] == NULL_STRING
			aFullPath[3] := SELF:cFSFileName
		ENDIF


		IF aFullPath[4] == NULL_STRING
			aFullPath[4] := SELF:cFSExtension
		ENDIF


		cPath := aFullPath[1] + aFullPath[2]
		cFileName := aFullPath[3] + aFullPath[4]


		IF aFullPath[3] == SELF:cFSFileName
			cAlias := Symbol2String( __ConstructUniqueAlias( aFullPath[3] ) )
		ENDIF


		IF SubStr2( cPath, SLen( cPath ) ) == "\"
			IF ( cNewName := __NewName( cPath + cFileName ) ) != NULL_STRING
				cFileName := cPath + cNewName + aFullPath[4]
			ELSE
				cFileName := cPath + aFullPath[3] + aFullPath[4]
			ENDIF
		ELSE
			IF ( cNewName := __NewName( cPath + "\" + cFileName ) ) != NULL_STRING
				cFileName := cPath + "\" + cNewName + aFullPath[4]
			ELSE
				cFileName := cPath + "\" + aFullPath[3] + aFullPath[4]
			ENDIF
		ENDIF




		aRdds := __RDDList( cDriver, aRdds )
		rddList := __AllocRddList( aRdds )


		IF Empty( lWantAnsi ) .OR. ! IsLogic( lWantAnsi )
			lOldAnsi := SetAnsi( SELF:lAnsi )
		ELSE
			lOldAnsi := SetAnsi( lWantAnsi )
		ENDIF


		lRetCode := VoDbCreate( cFileName, aDbStruct, rddList, TRUE, cAlias, "", FALSE, FALSE )


		IF lRetCode
			IF SELF:HeaderSize == 0
				SELF:FullPath := cFileName
				lRetCode := SELF:DBFSGetInfo( aRDDs )
			ENDIF
		ENDIF


		SetAnsi( lOldAnsi )


	RECOVER USING oError
		ErrorBlock( cbOldErr )
        LOCAL oErr := oError as Error
		IF oErr:OSCode != 0
			oErr:Description := VO_Sprintf( __CAVOSTR_SYSLIB_DOS_ERROR, NTrim( oErr:OSCode ) + ;
				" ( " + DosErrString( oErr:OSCode ) + " )" )
		ENDIF


		SELF:oErrorInfo := oErr
		lRetCode := FALSE


	END SEQUENCE


	ErrorBlock( cbOldErr )


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbFileSpec.DBFAttr/*" />
ACCESS DBFAttr AS STRING
	IF ALen( SELF:DBF ) > 0
		RETURN SELF:DBF[1, F_ATTR]
	ENDIF
	RETURN NULL_STRING


/// <include file="Rdd.xml" path="doc/DbFileSpec.DBFDateChanged/*" />
ACCESS DBFDateChanged AS DATE


	IF ALen( SELF:DBF ) > 0
		RETURN SELF:DBF[1, F_DATE]
	ENDIF
	RETURN NULL_DATE


/// <include file="Rdd.xml" path="doc/DbFileSpec.DBFName/*" />
ACCESS DBFName AS STRING


	IF ALen( SELF:DBF ) > 0
		RETURN SELF:DBF[1, F_NAME]
	ENDIF
	RETURN NULL_STRING


/// <include file="Rdd.xml" path="doc/DbFileSpec.DBFSGetInfo/*" />
METHOD DBFSGetInfo( xRdds, aHidden ) AS USUAL CLIPPER
	LOCAL cAlias AS STRING
	LOCAL cFile AS STRING
	LOCAL aDirArray AS ARRAY
	LOCAL lRetCode AS LOGIC
	LOCAL oError AS USUAL
	LOCAL cbOldErr AS USUAL
	LOCAL oMemoHandle AS OBJECT


	cbOldErr := ErrorBlock( { | oErr | _Break( oErr ) } )


	BEGIN SEQUENCE
		cFile := SELF:__FullPathAcc()
		cAlias := Symbol2String( __ConstructUniqueAlias( SELF:cFSFileName ) )


		IF ! IsArray( xRdds )
			xRdds := SELF:aRDDs
		ENDIF


		IF ALen( xRdds ) < 2
			IF ! IsString( xRdds )
				xRdds := SELF:cRDD_Name
			ENDIF
			xRdds := __RDDList( xRdds, aHidden )
		ENDIF


		lRetCode := DbUseArea( TRUE, xRdds, cFile , cAlias, TRUE, TRUE, NIL, NIL, aHidden )


		IF lRetCode
			SELF:cRDD_Name := RddName()
			SELF:uRDD_Version := ( cAlias ) -> ( DbInfo( DBI_RDD_VERSION ) )
			SELF:aRDDs := xRdds


			aDirArray := Directory( cFile )
			IF ALen( aDirArray ) > 0
				SELF:DBF := aDirArray
			ENDIF


			IF DbInfo( DBI_ISDBF )
				SELF:nFCount        := ( cAlias ) -> ( DbInfo( DBI_FCOUNT ) )
				SELF:nRecSize       := ( cAlias ) -> (  DbInfo( DBI_GETRECSIZE ) )
				SELF:nHeaderSize    := ( cAlias ) -> ( DbInfo( DBI_GETHEADERSIZE ) )
				SELF:lAnsi          := ( cAlias ) -> ( DbInfo( DBI_ISANSI ) )
				SELF:dLastUpDate    := ( cAlias ) -> ( DbInfo( DBI_LASTUPDATE ) )
				SELF:nRecCount      := ( cAlias ) -> ( VoDbLastRec() )
				SELF:nRLockCount    := ( cAlias ) -> ( DbInfo( DBI_LOCKCOUNT ) )
				SELF:aDbStruct      := ( cAlias ) -> ( DbStruct() )
			ENDIF


            // pMemoHandle := ( cAlias ) -> ( DbINFO( DBI_MEMOHANDLE ) )
			// Workaround for bug #965, DbInfo( DBI_MEMOHANDLE ) returns FileStream or IntPtr
			oMemoHandle := ( cAlias ) -> ( DbInfo( DBI_MEMOHANDLE ) )
			LOCAL lMemoHandle AS LOGIC
			DO CASE
			CASE oMemoHandle == NULL
				lMemoHandle := FALSE
			CASE oMemoHandle:GetType() == TypeOf(IntPtr)
				lMemoHandle := (IntPtr)oMemoHandle != IntPtr.Zero
			CASE oMemoHandle:GetType() == TypeOf(PTR)
				lMemoHandle := (IntPtr) oMemoHandle != IntPtr.Zero
			OTHERWISE
				lMemoHandle := FALSE
			END CASE


			IF lMemoHandle
				SELF:cMemFileName := SELF:cFSFileName
				SELF:cMemFileExt := ( cAlias ) -> ( DbInfo(  DBI_MEMOEXT ) )
				SELF:nMemBlockSize := ( cAlias ) -> ( DbInfo( DBI_MEMOBLOCKSIZE ) )


				aDirArray := Directory( SELF:MemFullPath )
				IF ALen( aDirArray ) > 0
					SELF:Memo := aDirArray
				ENDIF
			ENDIF
			( cAlias ) -> ( DbCloseArea() )
		ENDIF


	RECOVER USING oError
		ErrorBlock( cbOldErr )
        LOCAL oErr := oError as Error
		IF oErr:OSCode != 0
			oErr:Description := VO_Sprintf( __CAVOSTR_SYSLIB_DOS_ERROR, NTrim( oErr:OSCode ) +  ;
				" ( " + DosErrString( oErr:OSCode ) + " )" )
		ENDIF


		IF lRetCode
			( cAlias ) -> ( DbCloseArea() )
		ENDIF


		lRetCode := FALSE


		oErr:FuncSym := String2Symbol( "DBFileSpec:DBFSGetInfo" )
		oErr:CanDefault := FALSE
		oErr:CanRetry   := FALSE


		SELF:oErrorInfo := oErr


		Eval( cbOldErr, oErr )


	END SEQUENCE


	ErrorBlock( cbOldErr )


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbFileSpec.DBFSize/*" />
ACCESS DBFSize AS LONG
	LOCAL DW := 0 AS LONG
	IF ALen( SELF:DBF ) > 0
		DW := SELF:DBF[1, F_SIZE]
	ENDIF


	RETURN DW


/// <include file="Rdd.xml" path="doc/DbFileSpec.DBFTime/*" />
ACCESS DBFTime AS STRING


	IF ALen( SELF:DBF ) > 0
		RETURN SELF:DBF[1, F_TIME]
	ENDIF
	RETURN NULL_STRING


/// <include file="Rdd.xml" path="doc/DbFileSpec.DbStruct/*" />
ACCESS DbStruct AS ARRAY


	RETURN SELF:aDbStruct


/// <include file="Rdd.xml" path="doc/DbFileSpec.Delete/*" />
METHOD Delete()  AS LOGIC STRICT
	LOCAL cFileFullPath AS STRING
	LOCAL cMemFullPath AS STRING
	LOCAL lRetCode AS LOGIC


	cFileFullPath := SELF:__FullPathAcc()


	IF cFileFullPath != NULL_STRING
		lRetCode := FErase( cFileFullPath )
		IF ( ( cMemFullPath := SELF:MemFullPath ) != NULL_STRING ) .AND. lRetCode
			lRetCode := FErase( cMemFullPath )
			IF lRetCode
				SELF:MemFullPath := NULL_STRING
			ENDIF
		ENDIF
		IF lRetCode
			SELF:FullPath := NULL_STRING
			SELF:DBF := NULL_ARRAY
			SELF:Memo := NULL_ARRAY
			SELF:cRDD_Name := NULL_STRING
			SELF:uRDD_Version := NULL_STRING
			SELF:aRDDs := NULL_ARRAY
			SELF:lAnsi := FALSE
			SELF:nFCount := 0
			SELF:nHeaderSize := 0
			SELF:dLastUpDate := NULL_DATE
			SELF:aDbStruct := NULL_ARRAY
			SELF:nRecCount := 0
			SELF:nRecSize := 0
			SELF:nRLockCount := 0
			SELF:nMemBlockSize := 0
		ENDIF


		IF lRetCode .AND. ! Empty( SELF:aIndexNames )


			FOREACH cFame as STRING in SELF:aIndexNames
				lRetCode := FErase( cFame )
			NEXT


			FOREACH os as OrderSpec in aOrders
				IF lRetCode
					os:FileName := NULL_STRING
					os:OrderName := NULL_STRING
					os:OrderExpr := NULL_STRING
					os:OrderBlock := NIL
					os:Unique := FALSE
					os:KeyInfo := NULL_ARRAY
					os:IsCond := FALSE
					os:ForCond := NULL_STRING
					os:ForBlock := NIL
					os:WhileBlock := NIL
					os:EvalBlock := NIL
					os:Interval := 0
					os:Start := 0
					os:Records := 0
					os:RecNo := 0
					os:Rest := FALSE
					os:Descend := FALSE
					os:All := FALSE
					os:Add := FALSE
					os:Custom := FALSE
					os:Current := FALSE
					os:NoOptimize := FALSE
				ENDIF
			NEXT


			IF lRetCode
				SELF:aOrders := NULL_ARRAY
				SELF:aIndexNames := NULL_ARRAY
			ENDIF
		ENDIF
	ENDIF


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbFileSpec.Delim/*" />
ACCESS Delim AS STRING
	RETURN SELF:cDelim


/// <include file="Rdd.xml" path="doc/DbFileSpec.Delim/*" />
ASSIGN Delim( cDelimiter AS STRING)
	SELF:cDelim := cDelimiter
	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.FCount/*" />
ACCESS FCount AS INT
	RETURN SELF:nFCount


/// <include file="Rdd.xml" path="doc/DbFileSpec.Fields/*" />
ACCESS Fields AS ARRAY
	RETURN SELF:aFields


/// <include file="Rdd.xml" path="doc/DbFileSpec.Fields/*" />
ASSIGN Fields( aFLDs AS ARRAY)
	SELF:aFields := aFLDs
	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.FileName/*" />
ASSIGN FileName( cFileName AS STRING)
	LOCAL aFullPath AS ARRAY
	LOCAL cGetDefault AS STRING


	IF ! Empty( cFileName ) .AND. IsString( cFileName )
		IF SubStr2( cFileName, SLen( cFileName ) ) == "\"
			cFileName := SubStr3( cFileName, 1, SLen( cFileName ) - 1 )
			cFileName += ".DBF"
		ELSEIF RAt2( ".", cFileName ) = 0
			cFileName += ".DBF"
		ENDIF


		cFileName := Upper( cFileName )
		aFullPath := ArrayNew( 4 )
		__SplitPath( NIL, cFileName, aFullPath )
		cGetDefault := GetDefault()


		IF At2( ":", cFileName ) != 0 .OR.  ;
				( cGetDefault != NULL_STRING .AND. SELF:cFSDrive == NULL_STRING )
			SELF:cFSDrive := aFullPath[1]
		ENDIF


		IF At2( "\", cFileName ) != 0 .OR.  ;
				( cGetDefault != NULL_STRING .AND. SELF:cFSPath == NULL_STRING )
			SELF:cFSPath := aFullPath[2]
		ENDIF


		SELF:cFSFileName := aFullPath[3]


		IF RAt2( ".", cFileName ) > 0
			SELF:cFSExtension := aFullPath[ 4 ]
		ENDIF


	ELSE
		SELF:cFSFileName := NULL_STRING
	ENDIF


	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.Find/*" />
METHOD Find() AS LOGIC STRICT
	LOCAL lRetCode AS LOGIC


	lRetCode := SUPER:Find()


	IF lRetCode
		lRetCode := SELF:DBFSGetInfo( SELF:aRDDs )
	ENDIF


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbFileSpec.ForBlock/*" />
ACCESS ForBlock AS USUAL


	RETURN SELF:bForBlock


/// <include file="Rdd.xml" path="doc/DbFileSpec.ForBlock/*" />
ASSIGN ForBlock( cbCodeBlock AS USUAL )
	IF Empty( cbCodeBlock ) .OR. ! __CanEval( cbCodeBlock )
		SELF:bForBlock := NIL
	ELSE
		SELF:bForBlock := cbCodeBlock
	ENDIF


	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.FullPath/*" />
ASSIGN FullPath( cFullPath AS STRING)
	LOCAL aFullPath AS ARRAY


	IF ! Empty( cFullPath ) .AND. IsString( cFullPath )
		IF SubStr2( cFullPath, SLen( cFullPath ) ) == "\"
			cFullPath := SubStr3( cFullPath, 1, SLen( cFullPath ) - 1 )
			cFullPath += ".DBF"


		ELSEIF  RAt2( ".", cFullPath ) = 0
			cFullPath += ".DBF"
		ENDIF


		cFullPath := Upper( cFullPath )
		aFullPath := ArrayNew( 4 )
		__SplitPath( NIL, cFullPath, aFullPath )


		SELF:cFSDrive := aFullPath[1]
		SELF:cFSPath := aFullPath[2]
		SELF:cFSFileName := aFullPath[3]
		SELF:cFSExtension := aFullPath[4]


	ELSE
		SELF:cFSDrive := NULL_STRING
		SELF:cFSPath := NULL_STRING
		SELF:cFSFileName := NULL_STRING
		SELF:cFSExtension := NULL_STRING


    ENDIF
    RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.HeaderSize/*" />
ACCESS HeaderSize AS LONG
	RETURN SELF:nHeaderSize


/// <include file="Rdd.xml" path="doc/DbFileSpec.HidRDDs/*" />
ACCESS HidRDDs AS ARRAY
	RETURN SELF:aHidRDDs


/// <include file="Rdd.xml" path="doc/DbFileSpec.HidRDDs/*" />
ASSIGN HidRDDs( aHiddenRDD AS ARRAY)
	SELF:aHidRDDs := aHiddenRDD
	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.IndexNames/*" />
ACCESS IndexNames AS ARRAY
	RETURN SELF:aIndexNames


/// <include file="Rdd.xml" path="doc/DbFileSpec.IndexNames/*" />
ASSIGN IndexNames( cIndexName AS ARRAY)
	SELF:aIndexNames := cIndexName
	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.IsAnsi/*" />
ACCESS IsAnsi AS LOGIC


	RETURN SELF:lAnsi


/// <include file="Rdd.xml" path="doc/DbFileSpec.LastUpDate/*" />
ACCESS LastUpDate AS DATE


	RETURN SELF:dLastUpDate


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemAttr/*" />
ACCESS MemAttr AS STRING


	IF ALen( SELF:Memo ) > 0
		RETURN SELF:Memo[1, F_ATTR]
	ENDIF
	RETURN NULL_STRING


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemBlockSize/*" />
ACCESS MemBlockSize AS INT


	RETURN SELF:nMemBlockSize


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemDateChanged/*" />
ACCESS MemDateChanged AS DATE


	IF ALen( SELF:Memo ) > 0
		RETURN SELF:Memo[1, F_DATE]
	ENDIF
	RETURN NULL_DATE


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemFileExt/*" />
ACCESS MemFileExt AS STRING


	RETURN SELF:cMemFileExt


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemFileName/*" />
ACCESS MemFileName AS STRING


	RETURN SELF:cMemFileName


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemFullPath/*" />
ACCESS MemFullPath  AS STRING


	RETURN SELF:__MemFullPath()


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemFullPath/*" />
ASSIGN MemFullPath( cFullPath AS STRING)


	IF Empty( cFullPath )
		SELF:cMemFileName := NULL_STRING
		SELF:cMemFileExt := NULL_STRING
		RETURN
	ENDIF


	IF At2( ":", cFullPath ) != 0
		cFullPath := SubStr2( cFullPath, At2( ":", cFullPath ) + 1 )
	ENDIF


	IF RAt2( "\", cFullPath ) != 0
		cFullPath := SubStr2( cFullPath, RAt2(  "\", cFullPath ) + 1 )
	ENDIF


	IF RAt2( ".", cFullPath ) != 0
		SELF:cMemFileName := SubStr3( cFullPath, 1, RAt2( ".", cFullPath ) - 1 )
		SELF:cMemFileExt := SubStr2( cFullPath, RAt2( ".", cFullPath ) )
	ELSE
		SELF:cMemFileName := cFullPath
    ENDIF
	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemName/*" />
ACCESS MemName AS STRING


	IF ALen( SELF:Memo ) > 0
		RETURN SELF:Memo[1, F_NAME]
	ENDIF
	RETURN NULL_STRING


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemSize/*" />
ACCESS MemSize  AS LONG
	LOCAL DW AS LONG


	IF ALen( SELF:Memo ) > 0
		DW := SELF:Memo[1, F_SIZE]
	ENDIF


	RETURN DW


/// <include file="Rdd.xml" path="doc/DbFileSpec.MemTime/*" />
ACCESS MemTime AS STRING


	IF ALen( SELF:Memo ) > 0
		RETURN SELF:Memo[1, F_TIME]
	ENDIF
	RETURN NULL_STRING


/// <include file="Rdd.xml" path="doc/DbFileSpec.Move/*" />
METHOD Move( oDBFSTarget := NIL AS USUAL, lIDX := NIL AS USUAL, lName := NIL  AS USUAL) AS LOGIC STRICT
	LOCAL cTargetPath AS STRING
	LOCAL cMEMTarget AS STRING
	LOCAL cFileName AS STRING
	LOCAL cProdIndex AS STRING
	LOCAL aDirArray AS ARRAY
	LOCAL cSourcePath AS STRING
	LOCAL cMemSource AS STRING
	LOCAL oError AS USUAL
	LOCAL cbOldErr AS USUAL
	LOCAL lRetCode AS LOGIC
	LOCAL cPath AS STRING
	LOCAL i AS DWORD
	LOCAL nFiles AS DWORD


	IF Empty( lName ) .OR. ! IsLogic( lName )
		lName := FALSE
	ENDIF


	IF SubStr2( SELF:cFSPath, SLen( SELF:cFSPath ) ) == "\"
		cSourcePath := SELF:cFSDrive + SELF:cFSPath
	ELSE
		cSourcePath := SELF:cFSDrive + SELF:cFSPath + "\"
	ENDIF

    IF oDBFSTarget is FileSpec var fs
    	lRetCode := SUPER:Move( fs, lName )
    ELSE
        lRetCode := SUPER:Move( (string) oDBFSTarget, lName )
    ENDIF


	IF lRetCode
		IF Empty( lIDX ) .OR. ! IsLogic( lIDX )
			lIDX := TRUE
		ENDIF


		IF SubStr2( SELF:cFSPath, SLen( SELF:cFSPath ) ) == "\"
			cTargetPath := SELF:cFSDrive + SELF:cFSPath
		ELSE
			cTargetPath := SELF:cFSDrive + SELF:cFSPath + "\"
		ENDIF


		aDirArray := Directory( cTargetPath + SELF:cFSFileName + SELF:cFSExtension )
		IF ALen( aDirArray ) > 0
			SELF:DBF := aDirArray
		ENDIF


		cbOldErr := ErrorBlock( { | oErr | _Break( oErr ) } )
		BEGIN SEQUENCE
			cFileName := SELF:MemFileName
			IF cFileName != NULL_STRING
				IF cFileName != SELF:cFSFileName
					cFileName := SELF:cFSFileName
				ENDIF


				cMEMTarget := cTargetPath + cFileName + SELF:MemFileExt
				cMemSource := cSourcePath + SELF:MemFileName + SELF:MemFileExt


				IF SubStr3( cMEMTarget, 1, At2( ":", cMEMTarget ) ) == SubStr3( cMemSource, 1, At2( ":", cMemSource ) )
					IF !lName .AND. File( cMEMTarget )
						IF ( lRetCode := FErase( cMEMTarget  ) )
							lRetCode := FRename( cMemSource , cMEMTarget  )
						ENDIF
					ELSE
						lRetCode := FRename( cMemSource , cMEMTarget  )
					ENDIF


					IF lRetCode
						SELF:MemFullPath := cMEMTarget
						aDirArray := Directory( cTargetPath + cFileName + SELF:MemFileExt )
						IF ALen( aDirArray ) > 0
							SELF:Memo := aDirArray
						ENDIF
					ENDIF


				ELSE
					IF lRetCode := FCopy( cMemSource, cMEMTarget )
						lRetCode := FErase( cMemSource  )
						IF lRetCode
							SELF:MemFullPath := cMEMTarget
							aDirArray := Directory( cTargetPath + cFileName + SELF:MemFileExt )
							IF ALen( aDirArray ) > 0
								SELF:Memo := aDirArray
							ENDIF
						ENDIF
					ENDIF
				ENDIF
			ENDIF


			IF lRetCode .AND. lIDX .AND. ! Empty( SELF:aIndexNames )
				IF SubStr3( cTargetPath, 1, At2( ":", cTargetPath ) ) == SubStr3( cSourcePath, 1, At2( ":", cSourcePath ) )
					cPath := Upper( cTargetPath )
					nFiles := ALen( SELF:aIndexNames )
					FOR i := 1 UPTO nFiles
						cFileName := SELF:aIndexNames[i]
						IF nFiles = 1 .AND. SELF:cRDD_Name != "DBFNTX"
							cProdIndex := SubStr2( cFileName, RAt2( "\", cFileName ) + 1 )
							cProdIndex := SubStr3( cProdIndex, 1, RAt2( ".", cProdIndex ) - 1 )
							IF SELF:cFSFileName != cProdIndex
								cProdIndex := SELF:cFSFileName + SubStr2( cFileName, RAt2( ".", cFileName ) )
								cTargetPath := cPath + cProdIndex
							ELSE
								cTargetPath := cPath + SubStr2( cFileName, RAt2( "\", cFileName ) + 1 )
							ENDIF
						ELSE
							cTargetPath := cPath + SubStr2( cFileName, RAt2( "\", cFileName ) + 1 )
						ENDIF


						IF ! lName .AND. File( cTargetPath )
							IF ( lRetCode := FErase( cTargetPath  ) )
								lRetCode := FRename( cFileName , cTargetPath  )
							ENDIF
						ELSE
							lRetCode := FRename( cFileName , cTargetPath  )
						ENDIF
						IF lRetCode
							FOREACH os as OrderSpec in SELF:aOrders
								IF os:FileName == cFileName
									os:FileName := cTargetPath
								ENDIF
							NEXT
							SELF:aIndexNames[i] := cTargetPath
						ENDIF
					NEXT


				ELSE
					cPath := Upper( cTargetPath )
					nFiles := ALen( SELF:aIndexNames )
					FOR i := 1 UPTO nFiles
						cFileName := SELF:aIndexNames[i]
						IF nFiles = 1 .AND. SELF:cRDD_Name != "DBFNTX"
							cProdIndex := SubStr2( cFileName, RAt2( "\", cFileName ) + 1 )
							cProdIndex := SubStr3( cProdIndex, 1, RAt2( ".", cProdIndex ) - 1 )
							IF SELF:cFSFileName != cProdIndex
								cProdIndex := SELF:cFSFileName + SubStr2( cFileName, RAt2( ".", cFileName ) )
								cTargetPath := cPath + cProdIndex
							ELSE
								cTargetPath := cPath + SubStr2( cFileName, RAt2( "\", cFileName ) + 1 )
							ENDIF
						ELSE
							cTargetPath := cPath + SubStr2( cFileName, RAt2( "\", cFileName ) + 1 )
						ENDIF


						lRetCode := FCopy( cFileName, cTargetPath )
						IF lRetCode
							lRetCode := FErase( cFileName  )
							IF lRetCode
								FOREACH os as OrderSpec in SELF:aOrders
									IF os:FileName == cFileName
										os:FileName := cTargetPath
									ENDIF
								NEXT
								SELF:aIndexNames[i] := cTargetPath
							ENDIF
						ENDIF
					NEXT
				ENDIF
			ENDIF


		RECOVER USING oError
			ErrorBlock( cbOldErr )
            LOCAL oErr := oError as Error


			IF oErr:OSCode != 0
				oErr:Description := VO_Sprintf( __CAVOSTR_SYSLIB_DOS_ERROR, NTrim( oErr:OSCode ) + ;
					" ( " + DosErrString( oErr:OSCode ) + " )" )
			ENDIF


			SELF:oErrorInfo := oErr
			lRetCode := FALSE


		END SEQUENCE


		ErrorBlock( cbOldErr )


	ENDIF


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbFileSpec.Orders/*" />
ACCESS Orders AS USUAL


	RETURN SELF:aOrders


/// <include file="Rdd.xml" path="doc/DbFileSpec.Orders/*" />
ASSIGN Orders( oOrderSpec AS USUAL)


	IF IsObject(oOrderSpec) .AND. __Usual.ToObject(oOrderSpec) IS OrderSpec
		AAdd( SELF:aOrders, oOrderSpec )
	ENDIF
	IF IsArray( oOrderSpec )
		SELF:aOrders := oOrderSpec
	ENDIF


	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.RDD_Name/*" />
ACCESS RDD_Name AS STRING


	RETURN SELF:cRDD_Name


/// <include file="Rdd.xml" path="doc/DbFileSpec.RDD_Version/*" />
ACCESS RDD_Version AS USUAL


	RETURN SELF:uRDD_Version


/// <include file="Rdd.xml" path="doc/DbFileSpec.RDDs/*" />
ACCESS RDDs AS ARRAY


	RETURN SELF:aRDDs


/// <include file="Rdd.xml" path="doc/DbFileSpec.RecCount/*" />
ACCESS RecCount AS LONG


	RETURN SELF:nRecCount


/// <include file="Rdd.xml" path="doc/DbFileSpec.Recno/*" />
ACCESS RecNo AS LONG


	RETURN SELF:nRecord


/// <include file="Rdd.xml" path="doc/DbFileSpec.Recno/*" />
ASSIGN RecNo( nDWord   AS LONG)


	SELF:nRecord := nDWord
	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.Records/*" />
ACCESS Records  AS LONG


	RETURN SELF:nNext


/// <include file="Rdd.xml" path="doc/DbFileSpec.Records/*" />
ASSIGN Records( nDWord  AS LONG)
	SELF:nNext := nDWord
	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.RecSize/*" />
ACCESS RecSize AS LONG


	RETURN SELF:nRecSize


/// <include file="Rdd.xml" path="doc/DbFileSpec.Rename/*" />
METHOD Rename( oDBFSNewName:= NIL  AS USUAL, lName := NIL  AS USUAL) AS LOGIC
	LOCAL cTargetPath AS STRING
	LOCAL cSourcePath AS STRING
	LOCAL aDirArray AS ARRAY
	LOCAL cProdIndex AS STRING
	LOCAL cFileName AS STRING
	LOCAL cPath AS STRING
	LOCAL cbOldErr AS USUAL
	LOCAL oError AS USUAL
	LOCAL lRetCode AS LOGIC


	IF IsString( oDBFSNewName )
		oDBFSNewName := Upper( oDBFSNewName )
		IF At2( ":", oDBFSNewName ) != 0 .OR. At2( "\", oDBFSNewName ) != 0
			RETURN FALSE
		ENDIF


		IF RAt2( ".", oDBFSNewName ) = 0
			oDBFSNewName += ".DBF"
		ENDIF
	ENDIF


	IF SubStr2( SELF:cFSPath, SLen( SELF:cFSPath ) ) == "\"
		cSourcePath := SELF:cFSDrive + SELF:cFSPath
	ELSE
		cSourcePath := SELF:cFSDrive + SELF:cFSPath + "\"
	ENDIF


	IF Empty( lName ) .OR. ! IsLogic( lName )
		lName := FALSE
	ENDIF

    IF oDBFSNewName IS FileSpec var fs
	    lRetCode := SUPER:Rename( fs, lName )
    ELSE
        lRetCode := SUPER:Rename( (STRING)oDBFSNewName , lName )
    ENDIF


	cbOldErr := ErrorBlock( { | oErr | _Break( oErr ) } )


	BEGIN SEQUENCE
		IF lRetCode
			aDirArray := Directory( SELF:FullPath )
			IF ALen( aDirArray ) > 0
				SELF:DBF := aDirArray
			ENDIF
		ENDIF


		IF lRetCode .AND. SELF:MemFileName != NULL_STRING
			IF SubStr2( SELF:cFSPath, SLen( SELF:cFSPath ) ) == "\"
				cTargetPath := SELF:cFSDrive + SELF:cFSPath
				cPath := SELF:cFSDrive + SELF:cFSPath
			ELSE
				cTargetPath := SELF:cFSDrive + SELF:cFSPath + "\"
				cPath := SELF:cFSDrive + SELF:cFSPath + "\"
			ENDIF


			cSourcePath += SELF:MemFileName + SELF:MemFileExt
			cTargetPath += SELF:cFSFileName + SELF:MemFileExt


			IF lRetCode := FRename( cSourcePath , cTargetPath  )
				SELF:MemFullPath := cTargetPath
				aDirArray := Directory( SELF:MemFullPath )
				IF ALen( aDirArray ) > 0
					SELF:Memo := aDirArray
				ENDIF
			ENDIF
		ENDIF


		IF lRetCode .AND. ALen( SELF:aIndexNames ) = 1 .AND. SELF:cRDD_Name != "DBFNTX"
			cFileName := SELF:aIndexNames[1]
			cProdIndex := SubStr2( cFileName, RAt2( "\", cFileName ) + 1 )
			cProdIndex := SubStr3( cProdIndex, 1, RAt2( ".", cProdIndex ) - 1 )
			cProdIndex := SELF:cFSFileName + SubStr2( cFileName, RAt2( ".", cFileName ) )
			cTargetPath := cPath + cProdIndex


			lRetCode := FRename( cFileName , cTargetPath  )


			IF lRetCode
				FOREACH os as OrderSpec IN SELF:aOrders
					IF os:FileName == cFileName
						os:FileName := cTargetPath
					ENDIF
				NEXT


				SELF:aIndexNames[1] := cTargetPath
			ENDIF
		ENDIF


	RECOVER USING oError
		ErrorBlock( cbOldErr )
        LOCAL oErr := oError as Error


		IF oErr:OSCode != 0
			oErr:Description := VO_Sprintf( __CAVOSTR_SYSLIB_DOS_ERROR, NTrim( oErr:OSCode ) +  ;
				" ( " + DosErrString( oErr:OSCode ) + " )" )
		ENDIF


		SELF:oErrorInfo := oErr
		lRetCode := FALSE


	END SEQUENCE


	ErrorBlock( cbOldErr )


	RETURN lRetCode


/// <include file="Rdd.xml" path="doc/DbFileSpec.Rest/*" />
ACCESS Rest AS LOGIC


	RETURN SELF:lRest


/// <include file="Rdd.xml" path="doc/DbFileSpec.Rest/*" />
ASSIGN Rest( lLogic  AS LOGIC)


	IF Empty( lLogic ) .OR. ! IsLogic( lLogic )
		SELF:lRest := FALSE
	ELSE
		SELF:lRest := lLogic
	ENDIF


	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.RLockCount/*" />
ACCESS RLockCount AS LONG


	RETURN SELF:nRLockCount


/// <include file="Rdd.xml" path="doc/DbFileSpec.SDF/*" />
ACCESS SDF  AS LOGIC
	RETURN SELF:lSDF


/// <include file="Rdd.xml" path="doc/DbFileSpec.SDF/*" />
ASSIGN SDF( lLogic  AS LOGIC)
	SELF:lSDF := lLogic
	RETURN


/// <include file="Rdd.xml" path="doc/DbFileSpec.WhileBlock/*" />
ACCESS WhileBlock AS USUAL


	RETURN SELF:bWhileBlock


/// <include file="Rdd.xml" path="doc/DbFileSpec.WhileBlock/*" />
ASSIGN WhileBlock( cbCodeBlock  AS USUAL)


	IF Empty( cbCodeBlock ) .OR. ! __CanEval( cbCodeBlock )
		SELF:bWhileBlock := NIL
	ELSE
		SELF:bWhileBlock := cbCodeBlock
	ENDIF


	RETURN
END CLASS


