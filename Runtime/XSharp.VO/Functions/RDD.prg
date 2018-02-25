//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp
USING XSharp.RDD




FUNCTION DoError        (nSymFunc AS __Symbol, nTries := 1 as LONG) AS __Usual
	//LOCAL oError    AS Error
	//
	//oError := ErrorBuild(_VODBErrInfoPtr())
	//oError:Stack := ErrorStack(1) 
	//
	//oError:FuncSym := nSymFunc
	//
	//IF !IsNil(nTries)
		//oError:Tries   := nTries
	//ENDIF
	//RETURN Eval(ErrorBlock(), oError)
	RETURN __Usual._NIL

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION BOF            () AS LOGIC
	RETURN VODBBof()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION ALIAS          (nSelect AS __Usual) AS STRING
	IF IsNil(nSelect)
		RETURN Alias0()
	ENDIF
	RETURN VODBAlias(nSelect)

/// <summary>
/// Return the alias of the current work area as a string.
/// </summary>
/// <returns>
/// </returns>
FUNCTION Alias0() AS STRING
	THROW NotImplementedException{}

/// <summary>
/// Return the alias of the current work area as a __Symbol.
/// </summary>
/// <returns>
/// </returns>
FUNCTION Alias0Sym() AS __Symbol
	THROW NotImplementedException{}


	
FUNCTION DBCLEARINDEX   (uOrder AS __Usual, cOrdBag AS __Usual) AS LOGIC
	RETURN ORDLISTCLEAR(cOrdBag, uOrder)
	

FUNCTION ORDLISTCLEAR   (cOrdBag AS __Usual) AS LOGIC
	RETURN OrdListClear(cOrdBag, "")
	
FUNCTION ORDLISTCLEAR   (cOrdBag AS __Usual, uOrder AS __Usual) AS LOGIC
	LOCAL lRetCode  AS LOGIC
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	lRetCode := VODBOrdListClear(cOrdBag, uOrder)
	IF !lRetCode
		lRetCode := DoError(#ORDLISTCLEAR)
	ENDIF
	RETURN lRetCode
	
	
FUNCTION DBCLEARRELATION() AS LOGIC
	RETURN VODBClearRelation() 






/*
DEFINE SUCCESS             := 0
DEFINE FAILURE             := -1
	

	
	
STATIC GLOBAL ptrErrInfo       AS ERRINFO
	
	
	*----------------------------------------------------------------------------
	//
	// Some unducumented stuff
	//
	
FUNCTION _allocFieldNames(aStru AS ARRAY)           AS _FIELDNAMES  PASCAL
	
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
	
	
	
FUNCTION _freeFieldNames(fldNames  AS _FIELDNAMES) AS VOID PASCAL
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
	
	
	
*----------------------------------------------------------------------------
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
	
	
	
	
	
*----------------------------------------------------------------------------
//
// ALIAS([nSelect]) -> cAlias
//
	
FUNCTION ALIAS          (nSelect)
	
	IF IsNil(nSelect)
		RETURN Alias0()
	ENDIF
	
	RETURN VODBAlias(nSelect)
	
	
	
FUNCTION BOF            ()
	
	RETURN VODBBof()
	
*----------------------------------------------------------------------------
//
// DBAPPEND([lReleaseLocks])
//

	
	
FUNCTION DBAPPEND       (lReleaseLocks)
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(lReleaseLocks)
		lReleaseLocks := .T.
	ENDIF
	
	lRetCode := VODBAppend(lReleaseLocks)
	
	IF !lRetCode
		//    UH 06/26/1998
		//    lRetCode := DoError(#DBAPPEND)
		NetErr(.T.)
	ENDIF
	
	RETURN lRetCode
	
	
*----------------------------------------------------------------------------
//
// DBBLOBINFO()
//

FUNCTION DBBLOBINFO    (nOrdinal, nPos, xNewVal)
	
	IF !VODBBlobInfo(nOrdinal, nPos, @xNewVal)
		DoError(#DBBLOBINFO)
	ENDIF
	
	RETURN xNewVal
	
	
	
*----------------------------------------------------------------------------
//
// DBBUFFREFRESH()
//
// Note: Undocumented
//
	
FUNCTION DBBUFFREFRESH  ()
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBBuffRefresh()
	
	IF !lRetCode
		lRetCode := DoError(#DBBUFFREFRESH)
	ENDIF
	
	RETURN lRetCode
	
	
	
*----------------------------------------------------------------------------
//
// DBCLEARFILTER()
//
	
FUNCTION DBCLEARFILTER  ()
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBClearFilter()
	
	IF !lRetCode
		lRetCode := DoError(#DBCLEARFILTER)
	ENDIF
	
	RETURN lRetCode
	
	
	
*----------------------------------------------------------------------------
//
// DBCLEARINDEX()
//
//
	
FUNCTION DBCLEARINDEX   (uOrder, cOrdBag)
	
	RETURN ORDLISTCLEAR(cOrdBag, uOrder)
	
	
	
	
FUNCTION DBCLEARRELATION()
	
	RETURN VODBClearRelation()
	
	
	*----------------------------------------------------------------------------
	//
	// DBCLOSEALL()
	//
	
FUNCTION DBCLOSEALL     ()
	
	RETURN VODBCloseAll()
	
	
FUNCTION DBCLOSEAREA    ()
	
	RETURN VODBCloseArea()
	
	
	
FUNCTION DBCOMMIT       ()
	
	RETURN VODBCommit()
	
	
	
FUNCTION DBCOMMITALL    ()
	
	RETURN VODBCommitAll()
	
	
	
	
	*----------------------------------------------------------------------------
	*
	*   DBCREATE( cName,    ;   // target file (DBF)
	*           aStru       ;   // structure array like return value of DBSTRUCT()
	*           [,lNew]     ;   // select new workarea ? (default .F.)
	*           [,cAlias]   ;   // alias name for workarae
	*           [,cDriver]  ;   // name of RDD
	*           [,lOpen]    ;   // JUST OPEN the file (we need the structure
	*                           // parameter aStru to be able to open SDF and
	*                           // DELIMited datafiles. If this parameter is
	*                           // given, DBCREATE() works like DBUSEAREA()
	*           [,cDelim]   ;   // delimiter for DELIM RDD
	*           [,lJustOpen];   // just open this file (simulate USE)
	*           [,aRdds]    ;   // Array with names of RDDs we want inherit
	*                           // some methods from
	*           )
	*
	*   Returns:    LOGICAL ->  .F. ... Failure
	*                           .T. ... Success
	*
	*   Example:    IF DBCREATE("NEWDB.DBF", DBSTRUCT(), .T.,,,.T.)
	*                   ...
	*
	*               ENDIF
	*
	*----------------------------------------------------------------------------
	
FUNCTION DBCREATE (   cName,      ;
		aStru,      ;
		xDriver,    ;
		lNew,       ;
		cAlias,     ;
		cDelim,     ;
		lJustOpen,  ;
		aHidden     ;
		)
	
	LOCAL           rddList         AS _RDDLIST
	LOCAL           lKeep           AS LOGIC
	LOCAL           lRetCode        AS LOGIC
	LOCAL           aRdds           AS ARRAY
	
	IF aStru = NIL
		aStru := {}
	ENDIF
	
	IF !IsArray( aStru )
		RETURN .F.
	ENDIF
	
	
	//
	// Get "lNew" and create "lKeep" for VODBUseArea()
	// by Clipper's logic:
	//
	//      .T. -   to open in first available workarea
	//      .F. -   to open in current workarea
	//      NIL -   to close file after creating
	//
	
	IF lNew = NIL
		lNew    := .T.
		lKeep   := .F.
	ELSE
		lKeep   := .T.
	ENDIF
	
	IF lJustOpen == NIL
		lJustOpen := .F.
	ENDIF
	
	aRdds   := __RddList(xDriver, aHidden)
	rddList := __AllocRddList(aRdds)
	
	IF IsNil(cAlias)
		cAlias := ""
	ENDIF
	
	IF IsNil(cDelim)
		cDelim := ""
	ENDIF
	
	lRetCode := VODBCreate(cName, aStru, rddList, lNew, cAlias, cDelim, lKeep, lJustOpen)
	
	IF rddList != NULL_PTR
		MemFree(RDDLIST)
	ENDIF
	
	IF !lRetCode
		lRetCode := DoError(#DBCREATE)
	ENDIF
	
	RETURN lRetCode
	
	
	*----------------------------------------------------------------------------
	//
	// DBCREATEINDEX(cName, cExpr, [uCobExpr] [,lUnique])
	
FUNCTION DBCREATEINDEX(cName, cExpr, cobExpr, lUnique)
	
	RETURN OrdCreate(cName, NIL, cExpr, cobExpr, lUnique)
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBCREATEORDER(uOrder, cName, cExpr, [uCobExpr] [,lUnique])
	
FUNCTION DBCREATEORDER  (uOrder, cName, cExpr, cobExpr, lUnique)
	
	RETURN OrdCreate(cName, uOrder, cExpr, cobExpr, lUnique)
	
	
	*----------------------------------------------------------------------------
	//
	// DBDELETE()
	//
	
FUNCTION DBDELETE ()
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBDelete ()
	
	IF !lRetCode
		lRetCode := DoError(#DBDELETE)
	ENDIF
	
	RETURN lRetCode
	
	
	*----------------------------------------------------------------------------
	//
	// DBDELETEORDER(uOrder, cOrdBag)
	//
	
FUNCTION DbDeleteOrder(uOrder, cOrdBag)
	LOCAL cOrder AS USUAL
	LOCAL lRet   AS LOGIC
	
	lRet := TRUE
	
	IF IsNumeric(uOrder)
		cOrder := NIL
		lRet := VODBOrderInfo(DBOI_NAME,"",uOrder, @cOrder)
		uOrder := cOrder
	ENDIF
	
	RETURN ORDDESTROY(uOrder, cOrdBag)
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBDRIVER() -> cDriver
	//
	
FUNCTION DBDRIVER       ()
	
	RETURN RDDNAME()
	
	
	*----------------------------------------------------------------------------
	//
	// DBEVAL(cobAction [,uCobFor] [,uCobWhile] [, nNext] [,nRecno] [,lRest])
	
FUNCTION DbEval(uBlock, uCobFor, uCobWhile, nNext, nRecno, lRest)
	
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(lRest)
		lRest := .F.
	ENDIF
	
	lRetCode := VODBEval(uBlock, uCobFor, uCobWhile, nNext, nRecno, lRest)
	
	IF !lRetCode
		lRetCode := DoError(#DbEval)
	ENDIF
	
	RETURN lRetCode
	
	
	*----------------------------------------------------------------------------
	//
	// DBF() -> cAlias
	//
	
	//FUNCTION DBF            ()
	
	//  RETURN ALIAS0()
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBFIELDINFO()
	//
	
FUNCTION DbFieldInfo(nOrdinal, nPos, xNewVal)
	
	IF !VODBFieldInfo(nOrdinal, nPos, @xNewVal)
		DoError(#DbFieldInfo)
	ENDIF
	
	RETURN xNewVal
	
	
	
FUNCTION DbFIlter()
	
	RETURN VODBFilter()
	
	
	
FUNCTION DbGetSelect()
	
	RETURN VODBGetSelect()
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBGOBOTTOM
	//
	
FUNCTION DbGoBottom()
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBGoBottom()
	
	IF !lRetCode
		lRetCode := DoError(#DbGoBottom)
	ENDIF
	
	RETURN lRetCode
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBGOTO
	//
	
FUNCTION DbGoto(uRecId)
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBGoto(uRecId)
	
	IF !lRetCode
		lRetCode := DoError(#DbGoto)
	ENDIF
	
	RETURN lRetCode
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBGOTOP
	//
	
FUNCTION DbGotop()
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBGoTop()
	
	IF !lRetCode
		lRetCode := DoError(#DbGotop)
	ENDIF
	
	RETURN lRetCode
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBINFO()
	//
	//
	
FUNCTION DbInfo(nOrdinal, xNewVal)
	
	IF !VODBInfo(nOrdinal, @xNewVal)
		DoError(#DbInfo)
	ENDIF
	
	RETURN xNewVal
	
	
	*----------------------------------------------------------------------------
	//
	// DBJOINAPPEND()
	//
	// Note: Undocumented
	//
	
FUNCTION DbJoinAppend(nSelect    AS DWORD,    ;
		struList   AS _JOINLIST)   AS LOGIC        PASCAL
	
	LOCAL lRetCode AS LOGIC
	
	lRetCode := VODBJoinAppend(nSelect, struList)
	
	IF !lRetCode
		lRetCode := DoError(#DbJoinAppend)
	ENDIF
	
	RETURN lRetCode
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBCONTINUE()
	//
	
FUNCTION DbContinue()
	
	LOCAL lRetCode AS LOGIC
	
	lRetCode := VODBContinue()
	
	IF !lRetCode
		lRetCode := DoError(#DbContinue)
	ENDIF
	
	RETURN lRetCode
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBLOCATE() -> NIL
	//
	
FUNCTION DbLocate(uCobFor, uCobWhile, nNext, uRecId, lRest )
	
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(lRest)
		lRest := .F.
	ENDIF
	
	IF IsNil(uCobWhile)
		uCobWhile := {|| .T. }
	ELSE
		lRest := .T.
	ENDIF
	
	IF IsNil(nNext)
		nNext := 0
	ENDIF
	
	lRetCode := VODBLocate(uCobFor, uCobWhile, nNext, uRecId, lRest)
	
	IF !lRetCode
		lRetCode := DoError(#DbLocate)
	ELSE
		lRetCode := VODBFound()
	ENDIF
	
	RETURN lRetCode
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBORERDINFO()
	//
	
FUNCTION DbOrderInfo(nOrdinal,cBagName, uOrder, xNewVal)
	LOCAL lKeyVal   AS LOGIC
	
	IF !IsString(cBagName)
		cBagName := ""
	ENDIF
	
	IF IsString(uOrder)
		IF Len(uOrder) == 0
			uOrder := NIL
		ENDIF
	ENDIF
	
	IF nOrdinal == DBOI_KEYVAL
		lKeyVal  := .T.
		nOrdinal := DBOI_EXPRESSION
	ENDIF
	
	VODBOrderInfo(nOrdinal, cBagName, uOrder, @xNewVal)
	
	IF lKeyVal
		IF IsString(xNewVal)
			IF Len(xNewVal) == 0
				xNewVal := NIL
			ELSE
				xNewVal := &(xNewVal)
			ENDIF
		ENDIF
	ENDIF
	
	RETURN xNewVal
	
	
	*----------------------------------------------------------------------------
	//
	// DBPACK()
	//
	
FUNCTION DbPack()
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBPack()
	
	IF !lRetCode
		lRetCode := DoError(#DbPack)
	ENDIF
	
	RETURN lRetCode
	
	
	
	
FUNCTION DbRecordInfo(nOrdinal, uRecId, xNewVal)
	
	VODBRecordInfo(nOrdinal, uRecId, @xNewVal)
	
	RETURN xNewVal
	
	
	*----------------------------------------------------------------------------
	//
	// DBRECALL()
	//
	
FUNCTION DbRecall()
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBRecall()
	
	IF !lRetCode
		lRetCode := DoError(#DbRecall)
	ENDIF
	
	RETURN lRetCode
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBREINDEX()
	//
	
FUNCTION DbReindex()
	
	RETURN ORDLISTREBUILD()
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBRLOCK()
	//
	
FUNCTION DbRLock(uRecord)
	
	RETURN VODBRlock(uRecord)
	
	
	*----------------------------------------------------------------------------
	//
	// DBRLOCKLIST() -> ARRAY
	//
	
FUNCTION DbRLockList()
	
	LOCAL lockList          AS _LOCKLIST
	LOCAL aLockList := {}   AS ARRAY
	LOCAL i                 AS DWORD
	LOCAL nRecords          AS USUAL
	
	nRecords := 0
	
	IF !VODBInfo(DBI_LOCKCOUNT, @nRecords)
		DoError(#DbRLockList)
	ELSE
		lockList := DbInfo(DBI_GETLOCKARRAY)
		
		FOR i := 1 TO nRecords
			AAdd(aLockList, lockList.lRecno[i])
		NEXT
	ENDIF
	
	RETURN aLockList
	
	
	
FUNCTION DbRSelect(nPos)
	
	Default(@nPos, 0)
	
	RETURN VODBRSelect(nPos)
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBRUNLOCK([<uRecID>])
	//
	
FUNCTION DbRUnLock(uRecId)
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBUnlock(uRecId)
	
	IF !lRetCode
		lRetCode := DoError(#DbRUnLock)
	ENDIF
	
	RETURN lRetCode
	
	
	*----------------------------------------------------------------------------
	//
	// DBSEEK(xValue [,lSoft])
	//
	
FUNCTION DbSeek(xValue, lSoft, lLast)
	LOCAL dbsci     IS _DBSCOPEINFO
	LOCAL lRet      AS LOGIC
	
	Default(@lSoft, SetSoftSeek())
	
	MemClear(@dbsci, _SIZEOF(_DBSCOPEINFO))
	
	IF !IsNil(lLast)
		dbsci.fLast := lLast
	ENDIF
	
	VODBSetScope(@dbsci)
	
	Default(@xValue, "")
	
	IF !VODBSeek(xValue, lSoft)
		RETURN DoError(#DbSeek)
	ENDIF
	
	lRet := VODBFound()
	
	//  UH 02/16/1999
	MemClear(@dbsci, _SIZEOF(_DBSCOPEINFO))
	VODBSetScope(@dbsci)
	
	RETURN lRet
	
	
	
FUNCTION DbSelect(nNew)
	
	LOCAL nOld  AS DWORD
	
	Default(@nNew, 0)
	
	VODBSelect(nNew, @nOld)
	
	RETURN nOld
	
	
	
FUNCTION DbSelectArea(xValue)
	
	LOCAL sSelect   AS SHORT
	
	sSelect := _SELECT(xValue)
	
	IF sSelect = 0
		ptrErrInfo := _VODBErrInfoPtr()
		ptrErrInfo.pszArg     := AsPsz(xValue)
		ptrErrInfo.dwArgType  := UsualType(xValue)
		DoError(#DbSelectArea)
	ENDIF
	
	RETURN (sSelect > 0)
	
	
	
FUNCTION DbSetFound(lFnd)
	
	
	RETURN VODBSetFound(lFnd)
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBSETCONDITION()
	//
	
FUNCTION DBSETORDERCONDITION(  cFor,       ;
		uCobFor,    ;
		lAll,       ;
		uCobWhile,  ;
		uCobEval,   ;
		nStep,      ;
		nStart,     ;
		nNext,      ;
		nRecno,     ;
		lRest,      ;
		lDescending,;
		lAdditive,  ;
		lCurrent,   ;
		lCustom,    ;
		lNoOptimize     )
	
	
	RETURN OrdCondSet( cFor, uCobFor, lAll, uCobWhile, uCobEval, ;
		nStep, nStart, nNext, nRecno, lRest,      ;
		lDescending, lAdditive, lCurrent, lCustom, lNoOptimize)
	
	
	
FUNCTION DbSetSelect(nSelect)
	
	Default(@nSelect, 0)
	
	RETURN VODBSetSelect(nSelect)
	
	
	
	
FUNCTION DbSymSelect(sAlias)
	
	Default(@sAlias, Alias0Sym())
	
	RETURN VODBSymSelect(sAlias)
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBCLEARORDERCONDITION()
	//
	
FUNCTION DBCLEARORDERCONDITION ()
	
	RETURN OrdCondSet("", NIL, .F., NIL, NIL, 0, 0, 0, 0, .F., .F.)
	
	
	*----------------------------------------------------------------------------
	//
	// DBRELATION(wPos)
	//
	
FUNCTION DBRELATION     (wPos)
	
	LOCAL pszRelText        AS PSZ
	LOCAL cRelation         AS STRING
	
	Default(@wPos, 1)
	
	IF !VODBRelation(wPos, @pszRelText)
		DoError(#DBRELATION)
	ELSE
		cRelation := Psz2String(pszRelText)
	ENDIF
	
	RETURN cRelation
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBSETDRIVER([cDriver]) -> cDriver
	//
	
FUNCTION DbSetDriver(cDriver)
	
	RETURN RddSetDefault(cDriver)
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBSETFILTER(cbFilter [,cFilter])
	//
	
FUNCTION DbSetFilter(cbFilter, cFilter)
	
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(cFilter)
		//	UH 08/25/1999
		//	cFilter := ""
		cFilter := "UNKNOWN"
	ENDIF
	
	lRetCode := VODBSetFilter(cbFilter, cFilter)
	
	IF !lRetCode
		lRetCode := DoError(#DbSetFilter)
	ENDIF
	
	RETURN lRetCode
	
	
	*----------------------------------------------------------------------------
	//
	// DBSETINDEX(cIndex [,uOrder])
	//
	
FUNCTION DbSetIndex(cIndex, uOrder)
	
	IF IsNil(cIndex)
		RETURN ORDLISTCLEAR()
	ENDIF
	RETURN ORDLISTADD(cIndex, uOrder)
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBSETORDER(uOrder)
	//
	
FUNCTION DbSetOrder (uOrder, cBagName)
	
	LOCAL pszOrder          AS PSZ
	
	Default(@cBagName, "")
	
	RETURN VODBOrdSetFocus(cBagName, uOrder, @pszOrder)
	
	
	*----------------------------------------------------------------------------
	//
	// DBSETRELATION(cAlias|nSelect, uCobKey [,cKey])
	//
	
FUNCTION DbSetRelation  (xAlias, uCobKey, cKey)
	
	LOCAL nSelect   AS DWORD
	LOCAL cAlias    AS STRING
	LOCAL xType     AS DWORD
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(cKey)
		cKey := ""
	ENDIF
	
	xType := UsualType(xAlias)
	
	IF xType = STRING
		nSelect := Val(xAlias)
		
		IF nSelect = 0
			cAlias := xAlias
		ELSE
			cAlias := ALIAS(nSelect)
		ENDIF
		
	ELSE
		cAlias := ALIAS(xAlias)
	ENDIF
	
	lRetCode := VODBSetRelation(cAlias, uCobKey, cKey)
	
	IF !lRetCode
		lRetCode := DoError(#DbSetRelation)
	ENDIF
	
	RETURN lRetCode
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBSKIP([nRecords])
	//
	
FUNCTION DBSKIP         (nRecords)
	
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(nRecords)
		nRecords := 1
	ENDIF
	
	lRetCode := VODBSkip(nRecords)
	
	IF !lRetCode
		lRetCode := DoError(#DBSKIP)
	ENDIF
	
	RETURN lRetCode
	
	*----------------------------------------------------------------------------
	//
	// DBTRANS()
	//
	
FUNCTION DbTrans(nTo, aStru, uCobFor, uCobWhile, nNext, nRecno, lRest) AS LOGIC _NC CLIPPER
	
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
	
	
	*----------------------------------------------------------------------------
	//
	// DBUNLOCK()
	//
	
FUNCTION DbUnLock()
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBUnlock(NIL)
	
	IF !lRetCode
		lRetCode := DoError(#DbUnLock)
	ENDIF
	
	RETURN lRetCode
	
	
	
	
FUNCTION DbUnlockAll()
	
	RETURN VODBUnlockAll()
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBUSEAREA()
	//
	
FUNCTION DBUSEAREA (lNew,      ;   // Select the next free Area
	xDriver,    ;   // RDD name
	cName,      ;   // File name
	cAlias,     ;   // Alias qualifier
	lShare,     ;   // Shared mode on/off
	lReadOnly,  ;   // Read only mode on/off
	aStru,      ;   // Structure as two dimensional array
	cDelim,     ;   // Character for delimitted files
	aHidden     ;   // List of hidden RDDs as array
	;
		)                   AS LOGIC    _NC CLIPPER
	
	
	LOCAL           lRetCode        AS LOGIC
	LOCAL           rddList         AS _RDDLIST
	LOCAL           nTries          AS DWORD
	LOCAL           aRdds           AS ARRAY
	
	
	Default(@lNew, .F.)
	
	Default(@cAlias, "")
	
	Default( @lShare, !SetExclusive())
	
	Default(@lReadOnly, .F.)
	
	Default(@cDelim, "")
	
	nTries := 1
	
	aRdds := __RddList(xDriver, aHidden)
	
	rddList := __AllocRddList(aRdds)
	
	DO WHILE .T.
		
		IF !Empty(aStru)
			
			lRetCode := DBCREATE   ( cName, aStru, aRdds, lNew,;
				cAlias, cDelim, .T.)
		ELSE
			
			lRetCode := VODBUseArea(lNew, rddList, cName, cAlias,;
				lShare, lReadOnly)
			
		ENDIF
		
		IF lRetCode
			EXIT
		ELSE
			IF ( DoError(#DBUSEAREA, nTries) != E_RETRY )
				EXIT
			ENDIF
			nTries := nTries + 1
		ENDIF
		
	ENDDO
	
	IF rddList != NULL_PTR
		MemFree(RDDLIST)
	ENDIF
	
	RETURN lRetCode
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBZAP()
	//
	
FUNCTION DbZap()
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBZap()
	
	IF !lRetCode
		lRetCode := DoError(#DbZap)
	ENDIF
	
	RETURN lRetCode
	
	
	*----------------------------------------------------------------------------
	//
	// DELETED()
	//
	
FUNCTION DELETED        ()
	
	RETURN VODBDeleted()
	
FUNCTION EOF            ()
	
	RETURN VODBEof()
	
	*----------------------------------------------------------------------------
	//
	// FIELDPUT()
	//
	
FUNCTION FIELDPUT (wPos, xValue)
	
	LOCAL xRetVal AS USUAL
	
	IF VODBFieldPut(wPos, xValue)
		xRetVal := xValue
	ELSE
		DoError(#FIELDPUT)
	ENDIF
	
	RETURN xRetVal
	
	*----------------------------------------------------------------------------
	//
	// FIELDGET()
	//
	
FUNCTION FIELDGET (wPos)
	
	LOCAL xRetVal AS USUAL
	
	Default(@wPos, 1)
	
	IF !VODBFieldGet(wPos, @xRetVal)
		DoError(#FIELDGET)
	ENDIF
	
	RETURN xRetVal
	
FUNCTION FLOCK          ()
	
	RETURN VODBFlock()
	
FUNCTION FOUND          ()
	
	RETURN VODBFound()
	
	//--------------------------------------------------------------------
	//
	// HEADER()
	//
	
FUNCTION HEADER ()
	
	RETURN DbInfo(DBI_GETHEADERSIZE)
	
	
	
	
FUNCTION INDEXEXT       ()
	
	RETURN VODBOrdBagExt()
	
	
	
	
	//--------------------------------------------------------------------
	//
	// INDEXKEY([nOrder])
	//
	
FUNCTION INDEXKEY       (uOrder)
	
	LOCAL uRetVal   AS USUAL
	
	IF IsNil(uOrder)
		uOrder := 0
	ENDIF
	
	uRetVal := DbOrderInfo(DBOI_EXPRESSION, "", uOrder)
	
	IF IsNil(uRetVal)
		uRetVal := ""
	ENDIF
	
	RETURN uRetVal
	
	
	
	//--------------------------------------------------------------------
	//
	// INDEXORD()
	//
	
FUNCTION INDEXORD       ()
	
	LOCAL uRetVal := NIL
	
	uRetVal := DbOrderInfo(DBOI_NUMBER, "", NIL)
	
	Default(@uRetVal, 0)
	
	RETURN uRetVal
	
	
	
FUNCTION LASTREC        ()
	
	RETURN VODBLastRec()
	
	
	
	//--------------------------------------------------------------------
	//
	// LUPDATE()
	//
	
FUNCTION LUPDATE        ()
	
	RETURN DbInfo(DBI_LASTUPDATE)
	
	
	
FUNCTION ORDBAGEXT      ()
	
	RETURN VODBOrdBagExt()
	
	
	
	
	//--------------------------------------------------------------------
	//
	// ORDBAGNAME(uOrder)
	//
	// Note: Undocumented anymore
	//
	
FUNCTION ORDBAGNAME     (uOrder)
	
	RETURN DbOrderInfo(DBOI_BAGNAME, "", uOrder)
	
	
	
	*----------------------------------------------------------------------------
	//
	// DBORDCONDSET() -> LOGIC
	//
	// Note: Undocumented anymore
	//
	
FUNCTION OrdCondSet(   cFor,       ;
		uCobFor,    ;
		lAll,       ;
		uCobWhile,  ;
		uCobEval,   ;
		nStep,      ;
		nStart,     ;
		nNext,      ;
		nRecno,     ;
		lRest,      ;
		lDescending,;
		lAdditive,  ;
		lCurrent,   ;
		lCustom,    ;
		lNoOptimize     )
	
	
	LOCAL lRetCode          AS LOGIC
	LOCAL dbOrdCondInfo     IS _DBORDERCONDINFO
	
	MemClear( @dbOrdCondInfo, _SIZEOF(_DBORDERCONDINFO) )
	
	IF !IsNil(cFor)
		MemCopy( @dbOrdCondInfo.abFor[1], String2Psz( cFor), SLen(cFor) )
	ENDIF
	
	
	IF !IsNil(uCobFor)
		//RvdH 041007 Changed USUALS to Items
		MemCopy(@dbOrdCondInfo.itmCobFor, @uCobFor, _SIZEOF(_ITEM))
	ENDIF
	
	IF !IsNil(uCobWhile)
		//RvdH 041007 Changed USUALS to Items
		MemCopy(@dbOrdCondInfo.itmCobWhile,  @uCobWhile, _SIZEOF(_ITEM))
	ENDIF
	
	IF !IsNil(uCobEval)
		//RvdH 041007 Changed USUALS to Items
		MemCopy(@dbOrdCondInfo.itmCobEval , @uCobEval, _SIZEOF(_ITEM))
	ENDIF
	
	IF !IsNil(nStep)
		dbOrdCondInfo.lStep := nStep
	ENDIF
	
	IF !IsNil(nStart)
		dbOrdCondInfo.lStartRecno := nStart
	ENDIF
	
	IF !IsNil(nNext)
		dbOrdCondInfo.lNextCount := nNext
	ENDIF
	
	IF !IsNil(nRecno)
		dbOrdCondInfo.lRecno := nRecno
	ENDIF
	
	IF !IsNil(lRest)
		dbOrdCondInfo.fRest := lRest
	ENDIF
	
	IF !IsNil(lDescending)
		dbOrdCondInfo.fDescending := lDescending
	ENDIF
	
	
	IF !IsNil(lAll)
		dbOrdCondInfo.fAll := lAll
	ENDIF
	
	
	IF !IsNil(lAdditive)
		dbOrdCondInfo.fAdditive := lAdditive
	ENDIF
	
	IF !IsNil(lCustom)
		dbOrdCondInfo.fCustom := lCustom
	ENDIF
	
	IF !IsNil(lCurrent)
		dbOrdCondInfo.fUseCurrent := lCurrent
	ENDIF
	
	
	IF !IsNil(lNoOptimize)
		dbOrdCondInfo.fNoOptimize := lNoOptimize
	ENDIF
	
	lRetCode := VODBOrdCondSet( @dbOrdCondInfo )
	
	IF !lRetCode
		lRetCode := DoError(#OrdCondSet)
	ENDIF
	
	RETURN lRetCode
	
	
	
	*----------------------------------------------------------------------------
	//
	//  ORDCREATE(cName, [cOrder], cExpr, [uCobExpr] [,lUnique])
	//
	//  Note:   In Clipper are both parameters cName, cOrder
	//          optional, but one of them should be specified
	//
	//  Note: Undocumented anymore
	//
	
FUNCTION OrdCreate(cName, cOrder, cExpr, cobExpr, lUnique)
	
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(lUnique)
		lUnique := SetUnique()
	ENDIF
	
	IF IsNil(cName)
		IF IsNil(cOrder)
			ptrErrInfo := _VODBErrInfoPtr()
			ptrErrInfo.dwGenCode  := EG_ARG
			ptrErrInfo.dwSubCode  := EDB_CREATEINDEX
			ptrErrInfo.dwSeverity := ES_ERROR
			ptrErrInfo.pszArg     := AsPsz(cName)
			ptrErrInfo.lCanDefault:= TRUE
			DoError(#OrdCreate)
		ELSE
			cName := ""
		ENDIF
	ENDIF
	
	IF IsNil(cExpr)
		cExpr := ""
		IF IsNil(cobExpr)
			ptrErrInfo := _VODBErrInfoPtr()
			ptrErrInfo.dwGenCode  := EG_ARG
			ptrErrInfo.dwSubCode  := EDB_EXPRESSION
			ptrErrInfo.dwSeverity := ES_ERROR
			ptrErrInfo.lCanDefault:= TRUE
			DoError(#OrdCreate)
		ENDIF
	ELSE
		IF IsNil(cobExpr)
			cobExpr := &( "{||" + cExpr + "}" )
		ENDIF
	ENDIF
	
	lRetCode := VODBOrdCreate (cName, cOrder, cExpr, cobExpr, lUnique, NULL)
	
	IF !lRetCode
		lRetCode := DoError(#OrdCreate)
	ENDIF
	
	RETURN lRetCode
	
	
	*----------------------------------------------------------------------------
	//
	// ORDDESTROY(cOrderBag, [uOrder])
	//
	// Note: Undocumented anymore
	//
	
FUNCTION ORDDESTROY     (uOrder, cOrdBag)
	
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	IF IsString(uOrder)
		lRetCode := VODBOrdDestroy (cOrdBag, uOrder)
	ELSE
		ptrErrInfo := _VODBErrInfoPtr()
		
		ptrErrInfo.pszSubSystem     := String2Psz("DBCMD")
		ptrErrInfo.dwGenCode        := EG_ARG
		ptrErrInfo.dwSeverity       := ES_ERROR
		ptrErrInfo.lCanDefault      := .F.
		ptrErrInfo.lCanRetry        := .T.
		ptrErrInfo.lCanSubstitute   := .F.
		ptrErrInfo.pszArg           := AsPsz(uOrder)
		ptrErrInfo.dwArgType        := UsualType(uOrder)
		lRetCode := .F.
	ENDIF
	
	IF !lRetCode
		lRetCode := DoError(#ORDDESTROY)
	ENDIF
	
	RETURN lRetCode
	
	
	//--------------------------------------------------------------------
	//
	// ORDFOR(uOrder [,cOrdBag])
	//
	// Note: Undocumented anymore
	//
	
FUNCTION ORDFOR         (uOrder, cOrdBag, cFor)
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	IF !IsString(cFor)
		cFor := NIL
	ENDIF
	
	RETURN DbOrderInfo(DBOI_CONDITION, cOrdBag, uOrder, cFor)
	
	
	
	//--------------------------------------------------------------------
	//
	// ORDKEY(uOrder [,cOrdBag])
	//
	// Note: Undocumented anymore
	//
	
FUNCTION ORDKEY         (uOrder, cOrdBag)
	
	LOCAL xKey      AS USUAL
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	IF !VODBOrderInfo(DBOI_EXPRESSION, cOrdBag, uOrder, @xKey)
		xKey := ""
	ENDIF
	
	RETURN xKey
	
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// ORDLISTADD(cOrdBag , uOrder)
	//
	// Note: Undocumented anymore
	//
	
FUNCTION ORDLISTADD     (cOrdBag, uOrder)
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBOrdListAdd(cOrdBag, uOrder)
	
	IF !lRetCode
		lRetCode := DoError(#ORDLISTADD)
	ENDIF
	
	RETURN lRetCode
	*----------------------------------------------------------------------------
	//
	// ORDLISTCLEAR()
	//
	// Note: Undocumented anymore
	//
	
FUNCTION ORDLISTCLEAR   (cOrdBag, uOrder)
	
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	lRetCode := VODBOrdListClear(cOrdBag, uOrder)
	
	IF !lRetCode
		lRetCode := DoError(#ORDLISTCLEAR)
	ENDIF
	
	RETURN lRetCode
	
	
FUNCTION __OrdListClear()
	LOCAL lRet      AS LOGIC
	LOCAL lOpen     AS LOGIC
	LOCAL cDBF      AS STRING
	LOCAL cAlias    AS STRING
	LOCAL lShare    AS LOGIC
	LOCAL aRDD      AS ARRAY
	LOCAL rdds      AS _RDDLIST
	LOCAL i         AS DWORD
	
	IF Used()
		cDBF   := DbInfo(DBI_FULLPATH)
		rdds   := DbInfo(DBI_RDD_LIST)
		
		aRdd := {}
		FOR i := 1 TO rdds.uiRddCount
			AAdd(aRdd, Symbol2String(rdds.atomRddName[i]) )
		NEXT
		
		lShare := DbInfo(DBI_SHARED)
		cAlias := ALIAS()
		lOpen := RDDINFO(_SET_AUTOOPEN)
		RDDINFO(_SET_AUTOOPEN, .F.)
		DBCLOSEAREA()
		lRet := DBUSEAREA(.F., aRdd, cDBF, cAlias, lShare)
		RDDINFO(_SET_AUTOOPEN, lOpen)
	ELSE
		lRet := .F.
	ENDIF
	
	RETURN lRet
	
	*----------------------------------------------------------------------------
	//
	// ORDLISTREBUILD()
	//
	//
	// Note: Undocumented anymore
	
FUNCTION ORDLISTREBUILD ()
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBOrdListRebuild()
	
	IF !lRetCode
		lRetCode := DoError(#ORDLISTREBUILD)
	ENDIF
	
	RETURN lRetCode
	
	
	//--------------------------------------------------------------------
	//
	// ORDNAME(nOrder [,cOrdBag])
	//
	// Note: Undocumented anymore
	//
	
FUNCTION OrdName        (uOrder, cOrdBag)
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	RETURN DbOrderInfo(DBOI_NAME, cOrdBag, uOrder)
	
	
	
	//--------------------------------------------------------------------
	//
	// ORDNUMBER(uOrder [,cOrdBag])
	//
	// Note: Undocumented anymore
	//
	
FUNCTION ORDNUMBER      (uOrder, cOrdBag)
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	RETURN DbOrderInfo(DBOI_NUMBER, cOrdBag, uOrder)
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// ORDSETFOCUS(uOrder, cOrdBag)
	//
	// Note: Undocumented anymore
	//
	
FUNCTION ORDSETFOCUS    (uOrder, cOrdBag)
	//    UH 09/27/1999
	//    LOCAL cOrder            AS STRING
	LOCAL pszOrder    AS   PSZ
	
	Default(@cOrdBag, "")
	// UH 09/27/1999
	// VODBOrdSetFocus(cOrdBag, uOrder, @cOrder)
	// RETURN cOrder
	VODBOrdSetFocus(cOrdBag, uOrder, @pszOrder)
	RETURN Psz2String(pszOrder)
	
	
	
	*----------------------------------------------------------------------------
	//
	// RDDCOUNT([nType])
	//
	
FUNCTION RDDCOUNT       (nType)
	
	IF IsNil(nType)
		nType := RDT_FULL + RDT_TRANSFER
	ENDIF
	
	RETURN VODBRddCount(nType)
	
	
	*----------------------------------------------------------------------------
	//
	// RDDINFO()
	//
	//
	
FUNCTION RDDINFO        (nOrdinal, xNewVal)
	
	IF !VODBRDDInfo(nOrdinal, @xNewVal)
		//    DoError(#RDDINFO)
	ENDIF
	
	RETURN xNewVal
	
	
	
	*----------------------------------------------------------------------------
	//
	// RDDLIST([nType]) -> ARRAY
	//
	
FUNCTION RDDLIST        (nType)
	
	LOCAL rddList           AS _RDDLIST
	LOCAL aRddList := {}    AS ARRAY
	LOCAL i                 AS DWORD
	LOCAL nCount            AS DWORD
	
	IF IsNil(nType)
		nType := RDT_FULL + RDT_TRANSFER
	ENDIF
	
	nCount := VODBRddCount(nType)
	
	~"RANGECHECK-"
	IF nCount > 0
		
		rddList := MemAlloc( (_SIZEOF(DWORD)) + (nCount * _SIZEOF(SYMBOL)) )
		rddList.uiRddCount := nCount
		
		IF VODBRddList(rddList, nType)
			
			FOR i := 1 TO rddList.uiRddCount
				AAdd( aRddList, Symbol2String(rddList.atomRddName[i]) )
			NEXT
			
		ENDIF
		
		MemFree(RDDLIST)
		
	ENDIF
	RETURN aRddList
	
	
	
	*----------------------------------------------------------------------------
	//
	// RDDNAME()
	//
	// 431@001 UH 02/05/1995:   Call VODBRddName() if database is in USE
	//                          otherwise call RddSetDefault()
	//
	
FUNCTION RDDNAME        ()
	
	
	LOCAL cRet      AS STRING
	
	IF Used()
		cRet := VODBRddName()
	ELSE
		cRet := RddSetDefault(NIL)
	ENDIF
	
	RETURN cRet
	
	
	*----------------------------------------------------------------------------
	//
	// RDDSETDEFAULT([cDriver]) -> cDriver
	//
	
FUNCTION RddSetDefault  (cDriver)
	
	IF !IsString(cDriver)
		cDriver := NIL
	ELSE
		IF (cDriver == NULL_STRING)  .OR. ( Empty(Trim(cDriver)) )
			cDriver := NIL
		ENDIF
	ENDIF
	
	RETURN RDDINFO(_SET_DEFAULTRDD, cDriver)
	
	//--------------------------------------------------------------------
	//
	// RECSIZE()
	//
	
FUNCTION RECSIZE        ()
	
	RETURN DbInfo(DBI_GETRECSIZE)
	
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// RLOCK()
	//
	
FUNCTION RLOCK          ()
	RETURN (VODBRlock(NIL))
	
	
	
FUNCTION RECCOUNT       ()
	
	RETURN VODBLastRec()
	
	
	
	
	*----------------------------------------------------------------------------
	//
	// RECNO()
	//
	
FUNCTION RECNO          ()
	
	RETURN VODBRecno()
	
	
	
	*----------------------------------------------------------------------------
	//
	// SELECT([cAlias|nSelect|sAlias]) -> nSelect
	//
	// _SELECT([cAlias|nSelect|sAlias]) -> nSelect
	//
	//
	
DEFINE NEW_AREA      := 0
	
	
FUNCTION _SELECT        (xValue)
	LOCAL nSelect           AS DWORD
	LOCAL sAlias            AS SYMBOL
	LOCAL xType             AS DWORD
	LOCAL nAsc              AS DWORD
	
	
	IF IsNil(xValue)
		RETURN( VODBGetSelect() )
	ENDIF
	
	xType := UsualType(xValue)
	
	IF xType =SYMBOL
		nSelect := VODBSymSelect(xValue)
		
	ELSEIF xType =STRING
		nSelect := 0
		
		IF SLen(xValue) = 1
			nSelect := Val(xValue)
			nAsc := Asc( Upper(xValue) )
			IF nAsc > 64 .AND. nAsc < 75
				nSelect := nAsc - 64
			ENDIF
		ENDIF
		
		//  UH 07/10/2000
		//  IF (nSelect = 0) .AND. (xValue != "0")
		//    sAlias  := SysAddAtom( Upper( AllTrim(xValue) ) )
		//    nSelect := VODBSymSelect(sAlias)
		//  ELSE
		//    nSelect := VODBSetSelect(nSelect)
		//  ENDIF
		IF (nSelect > 0) .OR. ("0" == xValue)
			nSelect := VODBSetSelect(INT(nSelect))
		ELSE
			sAlias  := SysAddAtom( String2Psz( Upper( AllTrim(xValue) ) ) )
			nSelect := VODBSymSelect(sAlias)
		ENDIF
		
	ELSE
		nSelect := VODBSetSelect(xValue)
	ENDIF
	
	RETURN nSelect
	
	
	
FUNCTION SELECT         (xValue)
	
	LOCAL sSelect   AS DWORD
	LOCAL sCurrent  AS DWORD
	
	sCurrent := VODBGetSelect()
	
	sSelect := _SELECT(xValue)
	
	VODBSetSelect(INT(sCurrent))
	
	RETURN sSelect
	
	
	
	
	//---------------------------------------------------------------------------
	//
	//  COMIX extensions to the language
	//
	
FUNCTION OrdDescend     (xOrder, cOrdBag, lDescend)
	
	IF !IsLogic( lDescend )
		lDescend := NIL
	ENDIF
	
	RETURN DbOrderInfo(DBOI_ISDESC, cOrdBag, xOrder, lDescend)
	
	
	
FUNCTION OrdKeyAdd      (xOrder, cOrdBag, xVal)
	
	RETURN DbOrderInfo(DBOI_KEYADD, cOrdBag, xOrder, xVal)
	
	
	
FUNCTION OrdKeyCount    (xOrder, cOrdBag)
	
	RETURN DbOrderInfo(DBOI_KEYCOUNT, cOrdBag, xOrder, NIL)
	
	
	
FUNCTION OrdKeyDel      (xOrder, cOrdBag, xVal)
	
	RETURN DbOrderInfo( DBOI_KEYDELETE, cOrdBag, xOrder, NIL)
	
	
FUNCTION OrdKeyGoto     (nKeyNo)
	LOCAL lRetCode  AS LOGIC
	
	IF IsNumeric(nKeyNo)
		//  UH 04/28/1999
		//	nDiff := nKeyNo - ORDKeyNo()
		//	DBSKIP(nDiff)
		DbGotop()
		DBSKIP(nKeyno - 1)
		lRetCode := TRUE
	ENDIF
	
	RETURN lRetCode
	
	
	//  FUNCTION OrdKeyGoto     (nPos)
	//
	//      LOCAL lRetCode AS LOGIC
	//
	//      lRetCode := VODBOrderInfo ( DBOI_KEYGOTO, "", NIL, @nPos )
	//
	//      IF !lRetCode
	//          lRetCode := DoError(#ORDKEYGOTO)
	//      ENDIF
	//
	//      RETURN lRetCode
	//
	//      RETURN VODBOrderINfo ( DBOI_KEYGOTO, "", NIL, @nPos )
	
	
FUNCTION ORDKeyNo       (xOrder, cOrdBag)
	
	RETURN DbOrderInfo( DBOI_POSITION, cOrdBag, xOrder, NIL)
	
	
	
FUNCTION ORDKeyVal      ()
	
	RETURN DbOrderInfo( DBOI_KEYVAL, NIL ,NIL, NIL)
	
	
	
FUNCTION DBMemoExt      (cDriver)
	
	IF IsNil(cDriver)
		cDriver := ""
	ENDIF
	
	RETURN VODBMemoExt(cDriver)
	
	
	
STATIC FUNCTION OrdScopeNum(nScope)                 AS SHORT        _NC CLIPPER
	
	IF !IsNumeric( nScope )
		nScope := 0
	ENDIF
	
	nScope := INT(nScope)
	
	IF nScope < 0
		nScope := 0
	ENDIF
	
	IF nScope > 1
		nScope := 1
	ENDIF
	
	RETURN nScope
	
	
	
FUNCTION OrdSetRelation (cAlias, bKey, cKey)
	
	DbSetRelation(cAlias, bKey, cKey)
	
	(cAlias)->(OrdScope(0, bKey))
	
	(cAlias)->(OrdScope(1, bKey))
	
	RETURN NIL
	
	
	
	//  FUNCTION OrdSetScope    (nScope, xVal)
	
FUNCTION OrdScope       (nScope, xVal)
	LOCAL n     AS DWORD
	
	nScope := OrdScopeNum(nScope)
	
	n := DBOI_SCOPETOP
	
	IF PCount() > 1 .AND. IsNil(xVal)
		n := DBOI_SCOPETOPCLEAR
	ENDIF
	
	RETURN DbOrderInfo(n + nScope,,,xVal)
	
	
	
	
FUNCTION OrdSkipUnique  (nCount)
	RETURN VODBOrderInfo ( DBOI_SKIPUNIQUE, "", NIL,@nCount )
	
	
	
	
FUNCTION OrdIsUnique    (xOrder, cOrderBag)
	
	RETURN DbOrderInfo(DBOI_UNIQUE, cOrderBag, xOrder)
	
	
	
FUNCTION RDDVersion     (nParm)
	
	IF !IsNumeric(nParm)
		nParm := 0
	ENDIF
	
	RETURN DbInfo(DBI_RDD_VERSION, nParm)
	
	
	//---------------------------------------------------------------------------
	//
	//  FLEXFILE extensions to the language
	//
	
FUNCTION BLOBDirectExport   (nPointer, cTargetFile, nMode)
	
	LOCAL       lRet AS LOGIC
	
	lRet := DbInfo( BLOB_DIRECT_EXPORT, { nPointer, cTargetFile, nMode } )
	
	IF !lRet
		DoError(#BLOBDirectExport)
	ENDIF
	
	RETURN lRet
	
	
	
FUNCTION BLOBDirectGet      (nPointer, nStart, nCount)
	
	RETURN DbInfo( BLOB_DIRECT_GET, {nPointer, nStart, nCount} )
	
	
	
FUNCTION BLOBDirectImport   (nOldPointer, cSourceFile)
	
	RETURN DbInfo( BLOB_DIRECT_IMPORT, {nOldPointer, cSourceFile} )
	
	
	
FUNCTION BLOBDirectPut      (nOldPointer, uBLOB)
	
	RETURN DbInfo( BLOB_DIRECT_PUT, {nOldPointer, uBlob} )
	
	
	
FUNCTION BLOBExport         (nFieldPos, cFileName, nMode)
	
	LOCAL lRet          AS LOGIC
	
	DbInfo( BLOB_NMODE, nMode )
	
	lRet := VODBFileGet( nfieldPos, cFileName )
	
	IF !lRet
		DoError(#BLOBExport)
	ENDIF
	
	RETURN lRet
	
	
FUNCTION BLOBImport         (nFieldPos, cFileName)
	
	LOCAL lRet      AS LOGIC
	
	lRet := VODBFilePut( nFieldPos, cFileName )
	
	IF !lRet
		DoError(#BLOBImport)
	ENDIF
	
	RETURN lRet
	
	
	
FUNCTION BLOBRootGet        ()
	
	RETURN DbInfo( BLOB_ROOT_GET )
	
	
FUNCTION BLOBRootLock       ()
	
	RETURN DbInfo( BLOB_ROOT_LOCK )
	
	
FUNCTION BLOBRootPut        (xblob)
	
	RETURN DbInfo( BLOB_ROOT_PUT, xBlob )
	
	
	
FUNCTION BLOBRootUnlock     ()
	
	RETURN DbInfo( BLOB_ROOT_UNLOCK )
	
	
	
FUNCTION BLOBGet            (nFieldNo, nStart, nLen)
	
	RETURN DbInfo( BLOB_GET, {nFieldNo, nStart, nLen} )
	
	
FUNCTION DBMemoField        (xField)
	LOCAL n,i        AS DWORD
	LOCAL xRet       AS USUAL
	LOCAL nFields    AS DWORD
	
	IF IsNumeric(xField)
		n := xField
	ELSEIF IsSymbol(xField)
		n := FieldPosSym(xField)
	ELSEIF IsString(xField)
		n := FieldPos(xField)
	ELSE
		nFields := FCount()
		FOR i := 1 TO nFields
			IF DbFieldInfo(i, DBS_TYPE) == "M"
				n := i
				EXIT
			ENDIF
		NEXT
	ENDIF
	
	xRet := DbInfo( DBI_MEMOFIELD, n )
	
	RETURN xRet
	
	
	//---------------------------------------------------------------------------
	//
	//  DoError()
	//
	//  Call default error handler
	//
	
FUNCTION DoError        (nSymFunc, nTries)
	//RvdH 030926 Changed from as USUAL to as Error
	LOCAL oError    AS Error
	
	oError := ErrorBuild(_VODBErrInfoPtr())
	oError:Stack := ErrorStack(1) 
	
	oError:FuncSym := nSymFunc
	
	IF !IsNil(nTries)
		oError:Tries   := nTries
	ENDIF
	RETURN Eval(ErrorBlock(), oError)
	
	
	
	*----------------------------------------------------------------------------
	*
	*   Some useful STATIC functions
	*
	*
	*----------------------------------------------------------------------------
	
	//---------------------------------------------------------------------------
	//
	//  __AllocRddList()
	//
	//  Transfers a VO-Array into a DIM Array structure
	//
	
	
FUNCTION __AllocRddList (aRdds AS ARRAY)    AS _RDDLIST     PASCAL
	LOCAL n,i           AS DWORD
	LOCAL rddList       AS _RDDLIST
	n       := ALen(aRdds)
	rddList := MemAlloc( (_SIZEOF(DWORD)) + (n * _SIZEOF(SYMBOL)) )
	
	rddList.uiRddCount := n
	~"RANGECHECK-"
	FOR i := 1 TO n
		rddList.atomRddName[i] := SysAddAtomUpperA(aRdds[i])
	NEXT
	RETURN rddList
	//
	
FUNCTION __RddList      (xDriver, aHidden)
	
	LOCAL   nType   AS DWORD
	LOCAL   aRdds   AS ARRAY
	LOCAL   n       AS DWORD
	LOCAL   i       AS DWORD
	LOCAL   lBlob   AS LOGIC
	LOCAL   lDbf    AS LOGIC
	
	//  UH 09/24/1997
	//  IF (!IsArray(xDriver) .OR. IsString(xDriver))
	IF IsArray(xDriver)
		nType := ARRAY
	ELSEIF IsString(xDriver)
		//  UH 11/13/1997
		IF SLen(xDriver) = 0
			xDriver := RddSetDefault()
		ENDIF
		nType := STRING
	ELSE
		xDriver := RddSetDefault()
		nType := STRING
	ENDIF
	
	//  UH 09/24/1997
	//  nType := UsualType(xDriver)
	
	IF nType == ARRAY
		aRdds := xDriver
		
	ELSEIF nType == STRING
		aRdds := {}
		
		UpperA(xDriver)
		
		DO CASE
		CASE xDriver = "DBFNTX"
			lDbf := .T.
		CASE xDriver = "_DBFCDX"
			lDbf := .T.
		CASE xDriver = "DBFCDX"
			lBlob := .T.
			lDbf  := .T.
			xDriver := "_DBFCDX"
		CASE xDriver = "DBFMDX"
			lDbf := .T.
		OTHERWISE
			lDbf := .F.
			lBlob := .F.
		ENDCASE
		
		IF lDbf
			AAdd(aRdds, "CAVODBF")
		ENDIF
		
		AAdd(aRdds, xDriver)
		
		// UH (for David, Don)
		IF lBlob
			AAdd(aRdds, "DBFCDX")
		ENDIF
		
	ENDIF
	
	IF UsualType(aHidden) == ARRAY
		n := ALen(aHidden)
		
		FOR i := 1 TO n
			AAdd(aRdds, aHidden[i])
		NEXT
	ENDIF
	
	RETURN aRdds
	
FUNCTION IndexCount()
	LOCAL nRet      AS DWORD
	IF Used()
		nRet := DbOrderInfo(DBOI_ORDERCOUNT)
	ENDIF
	
	RETURN nRet
	
	//TEXTBLOCK E:\Program Files\CAVO26\SOURCE\System Library\DBBULK.PRG
	
FUNCTION _DbCreate(cFile1, cFile2, cDriver,lNew, cAlias)      AS LOGIC _NC CLIPPER
	
	LOCAL aStruct       AS ARRAY
	LOCAL oError        AS USUAL
	LOCAL i,n           AS INT
	LOCAL uErrBlock     AS USUAL
	LOCAL nSelect       AS INT
	LOCAL aField        AS ARRAY
	
	FIELD field_name, field_type, field_len, field_dec
	
	nSelect := 0
	
	Default(@lNew, .F.)
	
	IF ( Used() .AND. !lNew )
		DBCLOSEAREA()
	ENDIF
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	
	BEGIN SEQUENCE
		
		IF ( Empty(cFile2) )
			
			DBCREATE(cFile1,                                    ;
				{ {"FIELD_NAME", "C", 10, 0},   ;
				{"FIELD_TYPE", "C", 1, 0},      ;
				{"FIELD_LEN", "N", 3, 0},       ;
				{"FIELD_DEC", "N", 3, 0} },     ;
				cDriver,                        ;
				.F.,                            ;
				cAlias)
			
			
		ELSE
			                                                   
			
			DBUSEAREA(lNew, cDriver, cFile2)
			
			aStruct := {}
			
			n := LASTREC()
			
			VODBGoTop()
			
			DO WHILE !EOF()
				aField := {}
				AAdd(aField, AllTrim(field_name))
				AAdd(aField, AllTrim(field_type))
				AAdd(aField, field_len)
				AAdd(aField, field_dec)
				
				AAdd( aStruct, aField )
				DBSKIP(1, .F.)
			ENDDO
			
			VODBCloseArea()
			
			IF lNew
				VODBSetSelect(nSelect)
			ENDIF
			
			
			
			
			FOR i := 1 TO n
				
				IF (aStruct[i, DBS_TYPE] == "C") .AND. (aStruct[i, DBS_DEC] != 0)
					aStruct[i,DBS_LEN] += aStruct[i,DBS_DEC] * 256
				ENDIF
				
			NEXT
			
			DBCREATE(cFile1, aStruct, cDriver, lNew, cAlias )
			
		ENDIF
		
		
	RECOVER USING oError
		VODBCloseArea()
		oError:FuncSym := #_DBCREATE
		Eval( uErrBlock, oError)
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	RETURN ( Used() )
	
	
	
	
	
FUNCTION AFields(aNames, aTypes, aLens, aDecs)  AS DWORD _NC CLIPPER
	
	LOCAL aStruct           AS ARRAY
	LOCAL siCount           AS DWORD
	LOCAL si                AS DWORD
	LOCAL lNamesOk  := .F.  AS LOGIC
	LOCAL lTypesOk  := .F.  AS LOGIC
	LOCAL lLensOk   := .F.  AS LOGIC
	LOCAL lDecsOk   := .F.  AS LOGIC
	
	IF (Empty(aStruct := DbStruct() ))
		RETURN (0)
	ENDIF
	
	siCount := ALen(aStruct)
	
	IF UsualType(aNames) == ARRAY
		siCount := Min(siCount, Len(aNames) )
		lNamesOk := .T.
	ENDIF
	
	
	IF UsualType(aTypes) == ARRAY
		siCount := Min( siCount, Len(aTypes) )
		lTypesOk := .T.
	ENDIF
	
	
	IF UsualType(aLens) == ARRAY
		siCount := Min( siCount, Len(aLens) )
		lLensOk := .T.
	ENDIF
	
	
	IF UsualType(aDecs) == ARRAY
		siCount := Min( siCount, Len(aDecs) )
		lDecsOk := .T.
	ENDIF
	
	
	FOR si := 1 TO siCount
		
		IF lNamesOk
			aNames[si] := aStruct[si, DBS_NAME]
		ENDIF
		
		IF lTypesOk
			aTypes[si] := aStruct[si, DBS_TYPE]
		ENDIF
		
		IF lLensOk
			aLens[si]  := aStruct[si, DBS_LEN]
		ENDIF
		
		IF lDecsOk
			aDecs[si]  := aStruct[si, DBS_DEC]
		ENDIF
		
	NEXT
	
	
	RETURN siCount
	
	
	
	
	
FUNCTION DbApp(cFile, aFields, uCobFor, uCobWhile,;
		nNext, nRec, lRest,cDriver, aHidden)     AS LOGIC _NC CLIPPER
	//RvdH 061218 Added aHidden
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL n, i          AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL aMatch		  AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL oError        AS USUAL
	LOCAL uErrBlock     AS USUAL
	LOCAL lAnsi         AS LOGIC
	
	lAnsi  := SetAnsi()
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		siTo := VODBGetSelect()
		IF !Used()
			BREAK DBCMDError()
		ENDIF
		
		IF Empty( aStruct := __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY) )
			BREAK ParamError(ARRAY, 2)
		ENDIF
		
		//RvdH 061218 Added aHidden
		//DBUSEAREA(TRUE, cDriver, cFile, __UniqueAlias(cFile), TRUE, TRUE,/*aStru,/*cDelim, aHidden)
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
		
		
	RECOVER USING oError
		//  UH 10/11/2001
		IF  siFrom > 0
			VODBSetSelect(INT(siFrom))
			DBCLOSEAREA()
		ENDIF
		
		oError:FuncSym := #DBAPP
		Eval( uErrBlock, oError)
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	SetAnsi( lAnsi )
	
	RETURN (lRetCode)
	
	
FUNCTION DbAppDelim(cFile, cDelim, aFields,   ;
		uCobFor, uCobWhile, nNext,;
		nRec, lRest               )    AS LOGIC _NC CLIPPER
	
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
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		
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
		
		
	RECOVER USING oError
		oError:FuncSym := #DBAPPDELIM
		Eval( uErrBlock, oError)
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	
	SetAnsi( lAnsi )
	
	RETURN (lRetCode)
	
	
	
	
FUNCTION DbAppSdf(cFile, aFields, uCobFor,;
		uCobWhile, nNext, nRec, ;
		lRest                   )      AS LOGIC _NC CLIPPER
	
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
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		
		siTo := VODBGetSelect()
		
		IF (Empty( aStruct := __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY) ))
			BREAK ParamError(ARRAY, 2)
		ENDIF
		
		IF Empty(cFile)
			BREAK ParamError(STRING, 1)
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
		
	RECOVER USING oError
		oError:FuncSym := #DBAPPSDF
		Eval( uErrBlock, oError)
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	
	SetAnsi( lAnsi )
	
	RETURN (lRetCode)
	
	
	
	
FUNCTION DbCOpy(cFile, aFields, uCobFor,;
		uCobWhile, nNext, nRec, ;
		lRest, cDriver, aHidden    )     AS LOGIC _NC CLIPPER
	
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
		nRec, lRest                )   AS LOGIC _NC CLIPPER
	
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
		lRest                      )   AS LOGIC _NC CLIPPER
	
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
	
	
	
	
FUNCTION DbCopyStruct(cFile AS STRING, aFields AS ARRAY) AS LOGIC PASCAL
	
	RETURN DBCREATE(cFile, __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY) )
	
	
	
FUNCTION DbCOpyXStruct(cFile) AS LOGIC PASCAL
	
	LOCAL siSaveSel,n,i AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL oError        AS USUAL
	LOCAL uErrBlock     AS USUAL
	
	FIELD field_name, field_type, field_len, field_dec
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	BEGIN SEQUENCE
		
		
		IF !Used()
			BREAK DBCMDError()
		ENDIF
		
		aStruct := DbStruct()
		
		n := Len(aStruct)
		
		VODBSelect(0, @siSaveSel)
		
		_DbCreate(cFile)
		
		
		
		FOR i := 1 TO n
			
			IF aStruct[i, DBS_TYPE] == "C" .AND. aStruct[i, DBS_LEN] > 255
				aStruct[i, DBS_DEC] := SHORT(aStruct[i, DBS_LEN] / 256)
				aStruct[i, DBS_LEN] := aStruct[i, DBS_LEN] % 256
			ENDIF
			
			lRetCode := DBAPPEND()
			
			field_name  := aStruct[i, DBS_NAME]
			field_type  := aStruct[i, DBS_TYPE]
			field_len   := aStruct[i, DBS_LEN]
			field_dec   := aStruct[i, DBS_DEC]
			
		NEXT
		
		IF (VODBGetSelect() <> siSaveSel)
			DBCLOSEAREA()
			VODBSetSelect(INT(siSaveSel))
		ENDIF
		
	RECOVER USING oError
		oError:FuncSym := #DBCOPYXSTRUCT
		Eval( uErrBlock, oError)
		lRetCode := .F.
	END SEQUENCE
	
	ErrorBlock(uErrBlock)
	
	RETURN (lRetCode)
	
	
	
	//  UH 02/03/1997
	//  GLOBAL struJoinList  AS _JOINLIST
	
	
	
FUNCTION DbJoin(cAlias, cFile, aFields, uCobFor) AS LOGIC _NC CLIPPER
	
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
	
	
	
	
	
	
	
	
FUNCTION DbSort(	cFile, aFields, uCobFor,;
		uCobWhile, nNext, nRec, ;
		lRest                   )   AS LOGIC _NC CLIPPER
	
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
	
	Default(@lRest, .F.)
	
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
	
FUNCTION DbTotal(cFile, bKey, aFields,     ;
		uCobFor, uCobWhile, nNext,;
		nRec, lRest, xDriver ) 	AS LOGIC _NC CLIPPER
	
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
	
FUNCTION DbUpdate(cAlias, uCobKey, lRand, bReplace) AS LOGIC _NC CLIPPER
	
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
	
FUNCTION DbStruct() AS ARRAY PASCAL
	
	LOCAL aStruct   AS ARRAY
	LOCAL nFCount   AS DWORD
	LOCAL nProps    AS DWORD
	LOCAL i,j       AS DWORD
	LOCAL aField    AS ARRAY
	LOCAL xNewVal   AS USUAL
	
	aStruct := {}
	nFCount := FCount()
	
	IF !Used()
		ptrErrInfo := _VODBErrInfoPtr()
		
		ptrErrInfo.dwGenCode      := EG_NOTABLE
		ptrErrInfo.dwSubCode      := EDB_NOTABLE
		ptrErrInfo.pszSubSystem   := String2Psz("DBCMD")
		ptrErrInfo.dwSeverity     := ES_ERROR
		ptrErrInfo.lCanDefault    := .T.
		ptrErrInfo.symFuncSym     := #DBSTRUCT
		
		DefErrorGen(ptrErrInfo)
	ELSE
		FOR i := 1 UPTO nFCount
			aField := {}
			
			
			IF !VODBFieldInfo(DBS_PROPERTIES, i, @xNewVal)
				DoError(#DbFieldInfo)
			ENDIF
			nProps:= xNewVal
			
			FOR j := 1 UPTO nProps
				IF !VODBFieldInfo(j, i, @xNewVal)
					DoError(#DbFieldInfo)
				ENDIF
				AAdd(aField, xNewVal)
			NEXT
			
			AAdd(aStruct, aField)
			
		NEXT
	ENDIF
	
	RETURN aStruct
	
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
	
	
STATIC FUNCTION WithoutAlias(cName)                         AS STRING PASCAL
	
	cName   := SubStr(cName, At(">", cName) + 1 )
	cName   := Trim(Upper(cName))
	
	RETURN cName
	
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
	
	
	// Herntz re-wrote the below code. Join seems to work just fine now. 
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
	
	//RvdH 050616 FdW reported that the line below used the wrong SIZEOF
	//nBytes      := n * (_sizeof(_JOINFIELD)) +  2 * (_sizeof(WORD))
	nBytes      := n * _SIZEOF(_JOINFIELD) + _SIZEOF(_JOINLIST)		// This allocates 1 element too much, but that does not hurt...
	pJoinList   := MemAlloc(nBytes)
	
	MemClear(pJoinList, nBytes)
	
	pJoinList.uiCount    := n
	~"RANGECHECK-"
	FOR i := 1 TO n
		pJoinList.jfldInfo[i].uiSrcSel := aFldList[i,1]
		pJoinList.jfldInfo[i].uiSrcPos := aFldList[i,2] - 1
	NEXT
	
	PTR(ppJoinList) := pJoinList
	
	RETURN aStruct
	
	// STATIC FUNCTION LocalErrFunc(oError AS USUAL)               AS USUAL PASCAL
	
	// 	BREAK oError
	
	// 	RETURN NIL
	
STATIC FUNCTION ParamError  (dwArgNum  AS DWORD  ,    ;
		dwArgType AS DWORD       )       AS USUAL PASCAL
	
	LOCAL oError    AS USUAL
	
	ptrErrInfo := _VODBErrInfoPtr()
	
	ptrErrInfo.pszSubSystem   := String2Psz("DBCMD")
	ptrErrInfo.dwGenCode      := EG_ARG
	ptrErrInfo.dwSeverity     := ES_ERROR
	ptrErrInfo.lCanDefault    := .F.
	ptrErrInfo.lCanRetry      := .T.
	ptrErrInfo.lCanSubstitute := .F.
	ptrErrInfo.dwArgType      := dwArgType
	ptrErrInfo.dwArgNum       := dwArgNum
	
	oError := DefErrorGen(ptrErrInfo)
	
	RETURN oError
	
STATIC FUNCTION DBCMDError  ()                              AS USUAL PASCAL
	
	LOCAL oError    AS USUAL
	
	ptrErrInfo := _VODBErrInfoPtr()
	
	ptrErrInfo.dwGenCode      := EG_NOTABLE
	ptrErrInfo.dwSubCode      := EDB_NOTABLE
	ptrErrInfo.pszSubSystem   := String2Psz("DBCMD")
	ptrErrInfo.dwSeverity     := ES_ERROR
	ptrErrInfo.lCanDefault    := .T.
	
	oError := DefErrorGen(ptrErrInfo)
	
	//	UH 05/03/1999
	RETURN oError
	
_DLL FUNCTION RddInherit( pThis, pSuper, pSelf AS PTR, dwFuncCount AS DWORD ) AS LONG PASCAL:VO28RUN.RddInherit
	
	
	// DBFCDX RecordList Pseudo Functions
	/*
	FUNCTION rlAnd (rl1 AS LONG, rl2 AS LONG) AS LONG PASCAL
	RETURN DBINFO(DBI_RL_AND, {rl1, rl2})
	
	FUNCTION rlClear(rl AS LONG, nRecno AS LONG) AS VOID PASCAL
	DBINFO(DBI_RL_CLEAR, {rl,nRecno})
	RETURN
	
	FUNCTION rlCount(rl AS LONG) AS LONG PASCAL
	RETURN DBINFO(DBI_RL_COUNT, rl)
	
	FUNCTION rlDestroy(rl AS LONG)  AS VOID PASCAL
	DBINFO(DBI_RL_DESTROY, rl)
	RETURN
	
	FUNCTION rlExFilter() AS LONG PASCAL
	RETURN DBINFO(DBI_RL_EXFILTER)
	
	FUNCTION rlGetFilter() AS LONG PASCAL
	RETURN DBINFO(DBI_RL_GETFILTER)
	
	FUNCTION rlHasMaybe(rl AS LONG ) AS LOGIC PASCAL
	RETURN DBINFO(DBI_RL_HASMAYBE,rl)
	
	FUNCTION rlLen(rl AS LONG ) AS LONG PASCAL
	RETURN DBINFO(DBI_RL_LEN, rl)
	
	FUNCTION rlMaybeEval(rl AS LONG , bBlock AS USUAL) AS VOID PASCAL
	DBINFO(DBI_RL_MAYBEEVAL, {rl, bBlock})
	RETURN 
	
	FUNCTION rlNew(nLen AS LONG )  AS LONG PASCAL
	RETURN DBINFO(DBI_RL_NEW,nLen)
	
	FUNCTION rlNewDup(rlSrc AS LONG ) AS LONG PASCAL
	RETURN DBINFO(DBI_RL_NEWDUP,rlSrc)
	
	FUNCTION rlNewQuery(cQuery AS STRING) AS LONG PASCAL
	RETURN DBINFO(DBI_RL_NEWQUERY,cQuery)
	
	FUNCTION rlNextRecNo(rl AS LONG , nRecno AS LONG ) AS LONG PASCAL
	RETURN DBINFO(DBI_RL_NEXTRECNO, {rl, nRecno})
	
	FUNCTION rlNot(rl AS LONG ) AS LONG PASCAL
	RETURN DBINFO(DBI_RL_NOT, rl)
	
	FUNCTION rlOr(rl1 AS LONG , rl2 AS LONG ) AS LONG PASCAL
	RETURN DBINFO(DBI_RL_OR, {rl1, rl2})
	
	FUNCTION rlPrevReNo(rl AS LONG , nRecno AS LONG ) AS LONG PASCAL
	RETURN DBINFO(DBI_RL_PREVRECNO, {rl, nRecno})
	
	FUNCTION rlSet(rl AS LONG , nRecno AS LONG ) AS VOID PASCAL
	DBINFO(DBI_RL_SET, {rl, nRecno})
	RETURN 
	
	FUNCTION rlSetFilter(rl AS LONG )  AS VOID PASCAL
	DBINFO(DBI_RL_SETFILTER, rl)
	RETURN 
	
	FUNCTION rlTest(rl AS LONG , nRecno AS LONG )  AS LOGIC PASCAL
	RETURN DBINFO(DBI_RL_TEST, {rl, nRecno})
	
	// DBFCDX RecordList CONSTANTS
	DEFINE DBI_RL_AND	     := DBI_USER +  1
	DEFINE DBI_RL_CLEAR         := DBI_USER +  2
	DEFINE DBI_RL_COUNT         := DBI_USER +  3
	DEFINE DBI_RL_DESTROY       := DBI_USER +  4
	DEFINE DBI_RL_EXFILTER      := DBI_USER +  5
	DEFINE DBI_RL_GETFILTER     := DBI_USER +  6
	DEFINE DBI_RL_HASMAYBE      := DBI_USER +  7
	DEFINE DBI_RL_LEN           := DBI_USER +  8
	DEFINE DBI_RL_MAYBEEVAL     := DBI_USER +  9
	DEFINE DBI_RL_NEW           := DBI_USER + 10
	DEFINE DBI_RL_NEWDUP        := DBI_USER + 11
	DEFINE DBI_RL_NEWQUERY      := DBI_USER + 12
	DEFINE DBI_RL_NEXTRECNO     := DBI_USER + 13
	DEFINE DBI_RL_NOT           := DBI_USER + 14
	DEFINE DBI_RL_OR            := DBI_USER + 15
	DEFINE DBI_RL_PREVRECNO     := DBI_USER + 16
	DEFINE DBI_RL_SET           := DBI_USER + 17
	DEFINE DBI_RL_SETFILTER     := DBI_USER + 18
	DEFINE DBI_RL_TEST          := DBI_USER + 19
	*/
	

*/



	/// <summary>
	/// Return and optionally change the setting that determines whether to use the High Performance (HP) locking schema for newly created .NTX files.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION IndexHPLock(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	return false   

	/// <summary>
	/// Return and optionally change the setting that determines whether to use the new locking offset of -1 (0xFFFFFFFF) for .NTX files.
	/// </summary>
	/// <param name="lSet"></param>
	/// <returns>
	/// </returns>
	FUNCTION NewIndexLock(lSet AS __Usual) AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   

	/// <summary>
	/// Return the setting that determines whether to use the new locking offset of -1 (0xFFFFFFFF) for .NTX files.
	/// </summary>
	/// <returns>
	/// </returns>
	FUNCTION NewLocks() AS LOGIC
		/// THROW NotImplementedException{}
	RETURN FALSE   