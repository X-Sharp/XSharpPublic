//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// DbBulk.prg: Bulk operations on Workareas
#ifdef COMPILEIT

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
/*
FUNCTION __allocNames    (aStru AS ARRAY)                AS _FIELDNAMES  PASCAL
	
	LOCAL n,i       AS DWORD
	LOCAL cName		AS STRING
	LOCAL pszName 	AS PSZ
	LOCAL nSize		AS DWORD
	LOCAL fldNames  AS _FIELDNAMES
	~"RANGECHECK-"
	n := ALen(aStru)
	
	fldNames := MemAlloc( (_SIZEOF(DWORD)) + (n * _SIZEOF(PSZ)) )
	
	fldNames.uiFieldCount := n
	
	FOR i := 1 TO n
		//	UH 11/12/1997
		//	fldNames.lpbName[i] := PTR(_CAST, Upper(aStru[i]))
		cName := Upper(aStru[i])
		nSize := SLen(cName) + 1
		pszName := MemAlloc(nSize)
		IF pszName != NULL_PSZ
			MemCopy(pszName, PTR(_CAST, cName), nSize)
		ENDIF
		fldNames.lpbName[i] := pszName
	NEXT
	
	RETURN fldNames
*/
//
// __DBAvg()
//
//  Star issue 1241359 is a DBEVAL() problem
//  but this function replaces the need to have
//  a PUBLIC variable __Avg for computing the
//  sum value.
//

FUNCTION __DBAvg        (siValue AS SHORT)          AS SHORT        PASCAL
	
	LOCAL  siRet    AS SHORT
	STATIC siSum    AS SHORT
	
	IF siValue == 2
		RETURN siSum
	ENDIF
	
	siRet := siSum
	
	IF siValue == 0
		siSum := 0
	ELSE
		siSum := siSum + siValue
	ENDIF
	
	RETURN siRet

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
FUNCTION __DBFLEDIT      (aStruct AS ARRAY, aNames AS ARRAY, aMatch AS ARRAY) AS ARRAY EXPORT LOCAL
	
	LOCAL aNew      AS ARRAY
	LOCAL cobScan   AS CODEBLOCK
	LOCAL cName     AS STRING
	LOCAL n, i, j   AS DWORD
	LOCAL lMatch	AS LOGIC
	
	
	IF Empty(aNames)
		RETURN (aStruct)
	ENDIF
	
	//	UH 11/30/1998
	IF Empty(aMatch)
		lMatch := .F.
	ELSE
		lMatch := .T.
	ENDIF
	
	aNew:= {}
	n   := Len(aNames)
	
	FOR i := 1 TO n
		AAdd(aNew, WithoutAlias(AllTrim(aNames[i])))
	NEXT
	
	aNames := aNew
	
	aNew    := {}
	cobScan   := {|aFld| aFld[DBS_NAME] == cName}
	
	FOR i := 1 TO n
		cName := aNames[i]
		j := AScan(aStruct, cobScan)
		
		IF j > 0
			IF lMatch
				IF aMatch[i, DBS_TYPE] == aStruct[j, DBS_TYPE]
					AAdd(aNew, aStruct[j])
				ENDIF
			ELSE
				AAdd(aNew, aStruct[j])
			ENDIF
		ENDIF
	NEXT
	
	RETURN aNew

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
FUNCTION __UniqueAlias   (cDbfName AS STRING)            AS STRING       PASCAL
	
	LOCAL cAlias    AS STRING
	LOCAL n         AS DWORD
	LOCAL nSelect   AS DWORD
	
	//  UH 11/09/1999
	//  n := At(".", cDbfName )
	n := RAt(".", cDbfName )
	
	IF n > 0
		cDbfName := SubStr(cDbfName, 1, n - 1)
	ENDIF
	
	n := RAt("\", cDbfName)
	
	IF n > 0
		cDbfName := SubStr(cDbfName, n+1)
	ENDIF
	
	n := 1
	
	cAlias := Upper(cDbfName)
	
	nSelect := SELECT(cAlias)
	
	DO WHILE nSelect > 0
		IF Len(cAlias) < 9
			cAlias := cAlias + AllTrim(Str(n))
		ELSE
			cAlias := SubStr(cAlias, 1, 8) + AllTrim(Str(n))
		ENDIF
		
		n++
		nSelect := SELECT(cAlias)
	ENDDO
	
	RETURN cAlias





FUNCTION DbApp(cFile, aFields, uCobFor, uCobWhile,nNext, nRec, lRest,cDriver, aHidden)     AS LOGIC CLIPPER
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL n, i          AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL aMatch		  AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL lAnsi         AS LOGIC
	
	lAnsi  := SetAnsi()
	TRY	
		siTo := VODBGetSelect()
		IF !Used()
			BREAK DBCMDError()
		ENDIF
		
		IF Empty( aStruct := __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY) )
			BREAK ParamError(ARRAY, 2)
		ENDIF
		
		DBUSEAREA(TRUE, cDriver, cFile, __UniqueAlias(cFile), TRUE, TRUE,/*aStru*/,/*cDelim*/, aHidden)
		siFrom := VODBGetSelect()
		
		aFields := {}
		
		n := FCount()
		aMatch := DbStruct()
		
		FOR i := 1 TO n
			AAdd(aFields, FieldName(i))
		NEXT
		
		IF ( !lAnsi ) .AND. ( DbInfo(DBI_ISANSI) )
			SetAnsi(.T.)
		ENDIF
		
		IF !Empty(aStruct := __DBFLEDIT(aStruct, aFields, aMatch))
			lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		ENDIF
		
		IF (siFrom > 0)
			DBCLOSEAREA()
		ENDIF
		
		VODBSetSelect(INT(siTo))
		
		
	CATCH e AS Exception
		IF  siFrom > 0
			VODBSetSelect(INT(siFrom))
			DBCLOSEAREA()
		ENDIF
		
		oError:FuncSym := #DBAPP
		THROW Error{e}
	END TRY
	
	
	SetAnsi( lAnsi )
	
	RETURN (lRetCode)


FUNCTION DbAppDelim(cFile, cDelim, aFields, uCobFor, uCobWhile, nNext,nRec, lRest)AS LOGIC 
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL siPos         AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	
	
	lAnsi  := SetAnsi()
	
	
	TRY
		
		siTo := VODBGetSelect()
		
		IF (Empty( aStruct := __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY) ))
			BREAK ParamError(ARRAY, 3)
		ENDIF
		
		IF Empty(cFile)
			BREAK ParamError(STRING, 1)
		ELSE
			IF Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			ENDIF
		ENDIF
		
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DBCREATE(cFile, aStruct, "DELIM", .T., __UniqueAlias(cFile), cDelim, .T.)
		
		siFrom := VODBGetSelect()
		
		
		IF ( !lAnsi .AND. lDbfAnsi)
			SetAnsi(.T.)
		ENDIF
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		DBCLOSEAREA()
		
		VODBSetSelect(INT(siTo))
		
		
	CATCH e AS Exception
		oError:FuncSym := #DBAPPDELIM
		THROW Error{e}
	END TRY
	
	
	
	SetAnsi( lAnsi )
	
	RETURN (lRetCode)




FUNCTION DbAppSdf(cFile, aFields, uCobFor,;
	uCobWhile, nNext, nRec, ;
	lRest                   )      AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL siPos         AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL oError        AS USUAL
	LOCAL uErrBlock     AS USUAL
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	
	
	lAnsi  := SetAnsi()
	
	TRY		
		siTo := VODBGetSelect()
		
		IF (Empty( aStruct := __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY) ))
			THROW ParamError(ARRAY, 2)
		ENDIF
		
		IF Empty(cFile)
			THROW ParamError(STRING, 1)
		ELSE
			IF Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			ENDIF
		ENDIF
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DBCREATE(cFile, aStruct, "SDF", .T., __UniqueAlias(cFile), ,.T.)
		
		siFrom := VODBGetSelect()
		
		IF ( !lAnsi .AND. lDbfAnsi )
			SetAnsi(.T.)
		ENDIF
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		DBCLOSEAREA()
		VODBSetSelect(INT(siTo))
		
	CATCH e AS Exception
		oError:FuncSym := #DBAPPSDF
		THROW Error{e}
	END TRY
	
	
	SetAnsi( lAnsi )
	
	RETURN (lRetCode)



FUNCTION DbCOpy(cFile, aFields, uCobFor,;
	uCobWhile, nNext, nRec, ;
	lRest, cDriver, aHidden    )     AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL oError        AS USUAL
	LOCAL uErrBlock     AS USUAL
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	
	lAnsi    := SetAnsi()
	
	siFrom   := VODBGetSelect()
	lRetCode := .F.
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		
		IF !Used()
			BREAK DBCMDError()
		ENDIF
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		IF  Empty(AFields)                      .AND. ;
			IsNil(uCobFor)                      .AND. ;
			IsNil(uCobWhile)                    .AND. ;
			IsNil(nNext)                        .AND. ;
			IsNil(nRec)                         .AND. ;
			Empty(lRest)                        .AND. ;
			IsNil(cDriver)                      .AND. ;
			IsNil(aHidden)                      .AND. ;
			( lDbfAnsi == lAnsi )               .AND. ;
			( DbInfo(DBI_MEMOHANDLE) == 0 )     .AND. ;
			(DbOrderInfo(DBOI_ORDERCOUNT) = 0)
			
			lRetCode := DBFileCopy( DbInfo(DBI_FILEHANDLE), cFile, DbInfo(DBI_FULLPATH) )
		ELSE
			IF ( Empty(aStruct := __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY)) )
				BREAK ParamError(ARRAY, 2)
			ENDIF
			
			DBCREATE( cFile, aStruct, cDriver,, __UniqueAlias(cFile),,,aHidden)
			
			IF ( !lAnsi ) .AND. ( DbInfo(DBI_ISANSI) )
				SetAnsi(.T.)
			ENDIF
			
			DBUSEAREA(.T., cDriver, cFile, __UniqueAlias(cFile),,,,,aHidden)
			
			VODBSelect(siFrom, @siTo)
			
			lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
			
			IF (siTo > 0)
				VODBSetSelect(INT(siTo))
				VODBCloseArea()
			ENDIF
			
			VODBSetSelect(INT(siFrom))
		ENDIF
		
		RECOVER USING oError
		SetAnsi(lAnsi)
		oError:FuncSym := #DBCOPY
		Eval( uErrBlock, oError)
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	SetAnsi( lAnsi )
	
	RETURN (lRetCode)



DEFINE BUFF_SIZE := 0x00008000
DEFINE FO_CREATE := 0x00001000

FUNCTION DBFileCopy( hfFrom, cFile, cFullPath )
	
	LOCAL lRetCode  AS LOGIC
	LOCAL ptrBuff   AS PTR
	LOCAL hfTo      AS PTR
	LOCAL oError    AS Error
	LOCAL n         AS DWORD
	LOCAL dwPos     AS LONG
	
	
	IF At(".", cFile) == 0
		cFile := cFile + Right(cFullPath, 4)
	ENDIF
	
	hfTo := FxOpen( cFile, FO_CREATE, "")
	
	IF hfTo == F_ERROR
		
		oError := Error{1}
		oError:SubSystem                := "DBCMD"
		oError:GenCode                  := EG_OPEN
		oError:OsCode                   := DosError()
		oError:FuncSym                  := #DBCOPY
		oError:FileName                 := FPathName()
		
		oError:@@Throw()
		
		lRetCode := .F.
		
	ELSE
		
		dwPos := FSeek3(hfFrom, 0, FS_RELATIVE )
		
		
		FSeek3(hfFrom, 0, FS_SET )
		
		n       := BUFF_SIZE
		ptrBuff := MemAlloc(n)
		
		DO WHILE n == BUFF_SIZE
			n := FRead3(hfFrom, ptrBuff, BUFF_SIZE)
			
			FWrite3(hfTo, ptrBuff, n)
		ENDDO
		
		MemFree(ptrBuff)
		
		FClose(hfTo)
		
		lRetCode := .T.
		
		
		FSeek3(hfFrom, dwPos, FS_SET )
		
	ENDIF
	
	RETURN lRetCode





FUNCTION DBCOPYDELIM     (cFile, cDelim, aFields,   ;
	uCobFor, uCobWhile, nNext,;
	nRec, lRest                )   AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL siPos         AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL oError        AS USUAL
	LOCAL uErrBlock     AS USUAL
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	
	lAnsi  := SetAnsi()
	
	siFrom := VODBGetSelect()
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		
		IF !Used()
			BREAK DBCMDError()
		ENDIF
		
		IF Empty(aStruct := __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY))
			BREAK ParamError(ARRAY, 3)
		ENDIF
		
		IF Empty(cFile)
			BREAK ParamError(STRING, 1)
		ELSE
			IF Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			ENDIF
		ENDIF
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DBCREATE(cFile, aStruct, "DELIM", .T., __UniqueAlias(cFile), cDelim)
		
		IF ( !lAnsi .AND. lDbfAnsi)
			SetAnsi(.T.)
		ENDIF
		
		VODBSelect(siFrom, @siTo)
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		VODBSetSelect(INT(siTo))
		DBCLOSEAREA()
		VODBSetSelect(INT(siFrom))
		
		RECOVER USING oError
		oError:FuncSym := #DBCOPYDELIM
		Eval( uErrBlock, oError)
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	
	SetAnsi( lAnsi )
	
	RETURN (lRetCode)



FUNCTION DbCopySDF(cFile, aFields, uCobFor,;
	uCobWhile, nNext, nRec, ;
	lRest                      )   AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL siPos         AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL oError        AS USUAL
	LOCAL uErrBlock     AS USUAL
	LOCAL cAlias        AS STRING
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	
	
	lAnsi  := SetAnsi()
	
	siFrom := VODBGetSelect()
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		
		IF !Used()
			BREAK DBCMDError()
		ENDIF
		
		IF Empty(aStruct := __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY))
			BREAK ParamError(ARRAY, 2)
		ENDIF
		
		IF Empty(cFile)
			BREAK ParamError(STRING, 1)
		ELSE
			IF Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			ENDIF
		ENDIF
		
		cAlias := __UniqueAlias(cFile)
		
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DBCREATE(cFile, aStruct, "SDF", .T., cAlias)
		
		
		IF ( !lAnsi .AND. lDbfAnsi)
			SetAnsi(.T.)
		ENDIF
		
		VODBSelect(siFrom, @siTo)
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		VODBSetSelect(INT(siTo))
		DBCLOSEAREA()
		VODBSetSelect(INT(siFrom))
		
		RECOVER USING oError
		oError:FuncSym := #DBCOPYSDF
		Eval( uErrBlock, oError)
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	
	SetAnsi( lAnsi )
	
	RETURN (lRetCode)




FUNCTION DbJoin(cAlias, cFile, aFields, uCobFor) AS LOGIC CLIPPER
	
	LOCAL siFrom1       AS DWORD
	LOCAL siFrom2       AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL oError        AS USUAL
	LOCAL lRetCode      AS LOGIC
	LOCAL uErrBlock     AS USUAL
	
	//  UH 02/03/1997
	LOCAL pJoinList     AS _JOINLIST
	
	
	IF IsNil(uCobFor)
		uCobFor := {|| .T.}
	ENDIF
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		
		siFrom1 := VODBGetSelect()
		
		siFrom2 := SELECT(cAlias)
		
		
		IF siFrom2 = 0
			BREAK ParamError(1, STRING)
		ENDIF
		
		VODBSetSelect(INT(siFrom1))
		
		
		IF Empty( aStruct := __TargetFields(cAlias, aFields, @pJoinList) )
			ClearstrucErrInfo()
			
			strucErrInfo.pszSubSystem   := String2Psz("DBCMD")
			strucErrInfo.dwGenCode      := EG_ARG
			strucErrInfo.dwSubCode      := EDB_NOFIELDS
			strucErrInfo.dwSeverity     := ES_ERROR
			strucErrInfo.lCanDefault    := .F.
			strucErrInfo.lCanRetry      := .F.
			strucErrInfo.lCanSubstitute := .F.
			
			oError := DefErrorGen(_VODBErrInfoPtr())
			
			BREAK oError
		ENDIF
		
		DBCREATE( cFile, aStruct,"" , .T., "" )
		
		
		VODBSelect(siFrom1, @siTo)
		
		pJoinList.uiDestSel := siTo
		
		lRetCode := DbGotop()
		
		DO WHILE !EOF()
			
			VODBSetSelect(INT(siFrom2))
			
			lRetCode := DbGotop()
			
			DO WHILE ! EOF()
				
				VODBSetSelect(INT(siFrom1))
				
				IF ( Eval(uCobFor) )
					DbJoinAppend(siTo, pJoinList)
				ENDIF
				
				VODBSetSelect(INT(siFrom2))
				DBSKIP(1)
			ENDDO
			
			VODBSetSelect(INT(siFrom1))
			
			DBSKIP(1)
			
		ENDDO
		
		RECOVER USING oError
		oError:FuncSym := #DBJOIN
		Eval( uErrBlock, oError)
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	MemFree(pJoinList)
	
	IF siTo > 0
		VODBSetSelect(INT(siTo))
		DBCLOSEAREA()
	ENDIF
	
	VODBSetSelect(INT(siFrom1))
	
	RETURN (lRetCode)

/// <summary>
/// </summary>
/// <returns>
/// </returns>
/*
FUNCTION DbJoinAppend(nSelect    AS DWORD,    ;
	struList   AS _JOINLIST)   AS LOGIC        PASCAL
	
	LOCAL lRetCode AS LOGIC
	
	lRetCode := VODBJoinAppend(nSelect, struList)
	
	IF !lRetCode
		lRetCode := DoError(#DbJoinAppend)
	ENDIF
	
	RETURN lRetCode
*/







FUNCTION DbSort(	cFile, aFields, uCobFor,;
	uCobWhile, nNext, nRec, ;
	lRest                   )   AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL oError        AS USUAL
	LOCAL lRetCode      AS LOGIC
	LOCAL fnFieldNames  AS _FIELDNAMES
	LOCAL fnSortNames   AS _FIELDNAMES
	LOCAL uErrBlock     AS USUAL
	//	UH 09/23/1997
	LOCAL cRdd 			AS STRING
	
	fnFieldNames := NULL_PTR
	fnSortNames  := NULL_PTR
	
	siFrom := VODBGetSelect()
	
	DEFAULT(@lRest, .F.)
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		
		IF !Used()
			BREAK DBCMDError()
		ENDIF
		
		aStruct := DbStruct()
		
		//	UH 09/23/1997
		cRdd := RDDNAME()
		
		fnFieldNames := _allocFieldNames(aStruct)
		
		IF Empty(AFields)
			BREAK ParamError(ARRAY, 2)
		ENDIF
		
		fnSortNames := __allocNames(AFields)
		
		//	UH 09/23/1997
		//	DBCREATE(cFile, aStruct, "", .T.)
		DBCREATE(cFile, aStruct, cRdd, .T.)
		
		VODBSelect(siFrom, @siTo)
		
		lRetCode := VODBSort(siTo, fnFieldNames, uCobFor, uCobWhile, nNext, nRec, lRest, fnSortNames)
		
		IF !lRetCode
			ptrErrInfo := _VODBErrInfoPtr()
			BREAK DefErrorGen(ptrErrInfo)
		ENDIF
		
		_freeFieldNames(fnFieldNames)
		
		_freeFieldNames(fnSortNames)
		
		IF (siTo > 0)
			VODBSetSelect(INT(siTo))
			VODBCloseArea()
		ENDIF
		
		VODBSetSelect(INT(siFrom))
		
		
		RECOVER USING oError
		
		IF fnFieldNames != NULL_PTR
			_freeFieldNames(fnFieldNames)
		ENDIF
		
		IF fnSortNames != NULL_PTR
			_freeFieldNames(fnSortNames)
		ENDIF
		
		oError:FuncSym := #DBSORT
		Eval( uErrBlock, oError)
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	RETURN lRetCode

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbTrans(nTo, aStru, uCobFor, uCobWhile, nNext, nRecno, lRest) AS LOGIC CLIPPER
	
	LOCAL fldNames  AS _FIELDNAMES
	LOCAL lRetCode  AS LOGIC
	
	
	IF !IsNil(uCobWhile)
		lRest := .T.
	ENDIF
	
	IF IsNil(lRest)
		lRest := .F.
	ENDIF
	
	fldNames := _allocFieldNames(aStru)
	
	
	lRetCode := VODBTrans(nTo, fldNames, uCobFor, uCobWhile, nNext, nRecno, lRest)
	
	_freeFieldNames(fldNames)
	
	IF !lRetCode
		lRetCode := DoError(#DbTrans)
	ENDIF
	
	RETURN lRetCode



/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
FUNCTION DbTotal(cFile, bKey, aFields,     ;
	uCobFor, uCobWhile, nNext,;
	nRec, lRest, xDriver ) 	AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL i, n          AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL aFldNum       AS ARRAY
	LOCAL aNum          AS ARRAY
	LOCAL lSomething    AS LOGIC
	LOCAL kEval
	LOCAL oError        AS USUAL
	LOCAL lRetCode      AS LOGIC
	LOCAL fldNames      AS _FIELDNAMES
	LOCAL uErrBlock     AS USUAL
	
	
	IF IsNil(uCobWhile)
		uCobWhile := {|| .T.}
	ELSE
		lRest := .T.
	ENDIF
	
	IF IsNil(uCobFor)
		uCobFor := {|| .T.}
	ENDIF
	
	IF IsNil(lRest)
		lRest := .F.
	ENDIF
	
	
	IF !IsNil(nRec)
		DbGoto(nRec)
		nNext := 1
	ELSE
		
		IF IsNil(nNext)
			nNext := -1
		ELSE
			lRest := .T.
		ENDIF
		
		IF !lRest
			DbGotop()
		ENDIF
		
	ENDIF
	
	aFldNum := {}
	
	n := Len(AFields)
	
	FOR i := 1 TO n
		AAdd(aFldNum, FieldPos( AllTrim(AFields[i]) ) )
	NEXT
	
	aNum  := ArrayNew(n)
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		
		IF !Used()
			BREAK DBCMDError()
		ENDIF
		
		aStruct := DbStruct()
		
		siFrom := VODBGetSelect()
		
		
		
		aStruct := {}
		
		n := FCount()
		
		FOR i := 1 TO n
			IF DbFieldInfo(DBS_TYPE, i) != "M"
				
				AAdd(aStruct, { FieldName(i)   , ;
				DbFieldInfo(DBS_TYPE, i), ;
				DbFieldInfo(DBS_LEN, i),  ;
				DbFieldInfo(DBS_DEC, i)   ;
				}                           ;
				)
				
			ENDIF
		NEXT
		
		
		IF ( Empty(aStruct) )
			BREAK ParamError(ARRAY, 3)
		ENDIF
		
		fldNames := _allocFieldNames(aStruct)
		
		//	DBCREATE( cFile, aStruct, "", .T.)
		IF IsNil(xDriver)
			xDriver := RddSetDefault()
		ENDIF
		DBCREATE( cFile, aStruct, xDriver, .T.)
		
		VODBSelect(siFrom, @siTo)
		
		n := Len(aFldNum)
		
		DO WHILE ( (!EOF()) .AND. nNext != 0 .AND. Eval(uCobWhile) )
			
			lSomething := .F.
			
			AFill(aNum, 0)
			
			kEval := Eval(bKey)
			
			DO WHILE ( nNext-- != 0 .AND. Eval(uCobWhile) .AND. kEval = Eval(bKey) )
				IF ( Eval(uCobFor) )
					IF ( !lSomething )
						//	CollectForced()
						lRetCode := VODBTransRec(siTo, fldNames)
						lSomething := .T.
					ENDIF
					
					FOR i := 1 TO n
						aNum[i] := aNum[i] + FIELDGET(aFldNum[i])
					NEXT
					
				ENDIF
				
				DBSKIP(1, .F.)
				
			ENDDO
			
			IF ( lSomething )
				VODBSetSelect(INT(siTo))
				
				FOR i := 1 TO n
					FIELDPUT(aFldNum[i], aNum[i])
				NEXT
				
				VODBSetSelect(INT(siFrom))
			ENDIF
			
		ENDDO
		
		_freeFieldNames(fldNames)
		
		
		IF (siTo > 0)
			VODBSetSelect(INT(siTo))
			VODBCloseArea()
		ENDIF
		
		VODBSetSelect(INT(siFrom))
		
		RECOVER USING oError
		IF fldNames != NULL_PTR
			_freeFieldNames(fldNames)
		ENDIF
		
		oError:FuncSym := #DBTOTAL
		Eval( uErrBlock, oError)
		lRetCode := .F.
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	RETURN (lRetCode)

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
FUNCTION DbUpdate(cAlias, uCobKey, lRand, bReplace) AS LOGIC CLIPPER
	
	LOCAL siTo, siFrom  AS DWORD
	LOCAL kEval         AS USUAL
	LOCAL oError        AS USUAL
	LOCAL uErrBlock     AS USUAL
	LOCAL lRetCode      AS LOGIC
	
	
	IF (lRand == NIL)
		lRand := .F.
	ENDIF
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	lRetCode := .T.
	
	BEGIN SEQUENCE
		
		
		IF !Used()
			BREAK DBCMDError()
		ENDIF
		
		
		DbGotop()
		
		siTo := VODBGetSelect()
		
		
		siFrom := SELECT(cAlias)
		DbGotop()
		
		DO WHILE !EOF()
			
			kEval := Eval(uCobKey)
			
			VODBSetSelect(INT(siTo))
			
			IF lRand
				
				DbSeek(kEval)
				
				IF FOUND()
					Eval(bReplace)
				ENDIF
				
			ELSE
				
				DO WHILE ( Eval(uCobKey) < kEval .AND. !EOF() )
					DBSKIP(1)
				ENDDO
				
				IF ( Eval(uCobKey) == kEval .AND. !EOF() )
					Eval(bReplace)
				ENDIF
				
			ENDIF
			
			VODBSetSelect(INT(siFrom))
			
			DBSKIP(1)
			
		ENDDO
		
		RECOVER USING oError
		oError:FuncSym := #DBUPDATE
		Eval( uErrBlock, oError)
		lRetCode := .F.
	END SEQUENCE
	
	
	VODBSetSelect(INT(siTo))
	
	ErrorBlock(uErrBlock)
	
	
	RETURN (lRetCode)



/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
FUNCTION __TargetFields  (cAlias AS STRING, aNames AS ARRAY, ppJoinList AS PTR) AS ARRAY PASCAL EXPORT LOCAL
	
	LOCAL aNew      AS ARRAY
	LOCAL cName     AS STRING
	LOCAL aStruct   AS ARRAY
	LOCAL adbStruct AS ARRAY
	LOCAL n, i      AS DWORD
	LOCAL siPos     AS DWORD
	LOCAL siSelect  AS DWORD
	LOCAL nBytes    AS DWORD
	LOCAL aFldList  AS ARRAY
	
	//  UH 02/03/1997
	LOCAL pJoinList AS _JOINLIST
	
	adbStruct := DbStruct()
	aStruct   := {}
	aFldList := {}
	
	IF ( Empty(aNames) )
		
		aNames    := {}
		n         := FCount()
		siSelect   := VODBGetSelect()
		
		FOR i := 1 TO n
			cName := adbStruct[i, DBS_NAME]
			
			AAdd(aFldList, {siSelect, FieldPos(cName)})
			AAdd(aStruct, aDbStruct[i])
			AAdd(aNames, cName)
		NEXT
		
	ELSE
		
		n := Len(aNames)
		
		aNew := {}
		
		FOR i := 1 TO n
			AAdd(aNew, AllTrim(Upper(aNames[i])))
		NEXT
		
		aNames := aNew
		
		n := FCount()
		
		siSelect := VODBGetSelect()
		
		FOR i := 1 TO n
			
			cName := adbStruct[i, DBS_NAME]
			
			IF AScan(aNames, {|c| c == cName}) > 0
				
				AAdd(aFldList, {siSelect, FieldPos(cName)})
				AAdd(aStruct, aDbStruct[i])
				
			ENDIF
			
		NEXT
		
	ENDIF
	
	siSelect := SELECT(cAlias)
	
	aDbStruct := DbStruct()
	
	n := Len(aNames)
	
	
	/* Herntz re-wrote the below code. Join seems to work just fine now. */
	FOR i := 1 TO n
		IF "->" $ aNames[i]
			cName := SubStr(aNames[i], At(">", aNames[i]) + 1)
		ELSE
			cName :=  aNames[i]
		ENDIF
		
		siPos := AScan(aDbStruct, {|a| a[DBS_NAME] == cName})
		IF siPos > 0 .AND. (AScan( aStruct, {|c|c[DBS_NAME]== cName }) == 0)
			AAdd(aFldList, {siSelect, FieldPos(cName)})
			AAdd(aStruct, aDbStruct[siPos])
		ENDIF
	NEXT
	
	n := ALen(aStruct)
	
	nBytes      := n * _SIZEOF(_JOINFIELD) + _SIZEOF(_JOINLIST)		// This allocates 1 element too much, but that does not hurt...
	pJoinList   := MemAlloc(nBytes)
	
	MemClear(pJoinList, nBytes)
	
	pJoinList.uiCount    := n
	
	FOR i := 1 TO n
		pJoinList.jfldInfo[i].uiSrcSel := aFldList[i,1]
		pJoinList.jfldInfo[i].uiSrcPos := aFldList[i,2] - 1
	NEXT
	
	PTR(ppJoinList) := pJoinList
	
	RETURN aStruct



/*
FUNCTION _allocFieldNames(aStru AS ARRAY)           AS XSharp.RDD._FIELDNAMES  PASCAL
	
	LOCAL n,i       AS DWORD
	LOCAL fldNames  AS _FIELDNAMES
	LOCAL pszName   AS PSZ
	//  UH 11/12/1997
	LOCAL cName     AS STRING
	LOCAL nSize     AS DWORD
	n := ALen(aStru)
	
	fldNames := MemAlloc( (_SIZEOF(DWORD)) + (n * _SIZEOF(PSZ)) )
	
	fldNames.uiFieldCount := n
	
	~"RANGECHECK-"
	FOR i := 1 TO n
		//  UH 11/12/1997
		//  pszName := AsPsz(Upper(aStru[i,1]))
		//  fldNames.lpbName[i] := PTR(_CAST, pszName)
		cName := Upper(aStru[i, 1])
		nSize := SLen(cName) + 1
		pszName := MemAlloc(nSize)
		IF pszName != NULL_PSZ
			MemCopy(pszName, PTR(_CAST, cName), nSize)
		ENDIF
		fldNames.lpbName[i] := pszName
	NEXT
	
	RETURN fldNames



FUNCTION _freeFieldNames(fldNames  AS XSharp.RDD._FIELDNAMES) AS VOID PASCAL
	//
	//  UH 11/12/1997
	//
	LOCAL n,i       AS DWORD
	~"RANGECHECK-"
	n := fldNames.uiFieldCount
	
	FOR i := 1 TO n
		IF fldNames.lpbName[i] != NULL_PTR
			MemFree(fldNames.lpbName[i])
		ENDIF
	NEXT
	
	MemFree(fldNames)
	
	RETURN

*/

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
STATIC FUNCTION WithoutAlias(cName)                         AS STRING PASCAL
	
	cName   := SubStr(cName, At(">", cName) + 1 )
	cName   := Trim(Upper(cName))
	
	RETURN cName


#endif