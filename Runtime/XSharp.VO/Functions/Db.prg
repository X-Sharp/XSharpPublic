//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>
/// Return a set-get code block for a field that is identified by a __Symbol.
/// </summary>
/// <param name="symVar"></param>
/// <returns>
/// </returns>
function FieldBlockSym(symVar as __Symbol) as object
	/// THROW NotImplementedException{}
	return null_object   

/// <summary>
/// Get the contents of a field that is identified by a work area alias and the field name.
/// </summary>
/// <param name="symAlias"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
function FieldGetAlias(symAlias as __Symbol,symField as __Symbol) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   



/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
function FieldGetSelect(uSelect as __Usual,symField as __Symbol) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   


/// <summary>
/// Retrieve the contents of a field that is identified by its __Symbolic name.
/// </summary>
/// <param name="symVar"></param>
/// <returns>
/// </returns>
function FieldGetSym(symVar as __Symbol) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   

/// <summary>
/// Return the position of a field that is identified by a __Symbol.
/// </summary>
/// <param name="sField"></param>
/// <returns>
/// </returns>
function FieldPosSym(sField as __Symbol) as dword
	/// THROW NotImplementedException{}
	return 0   



/// <summary>
/// Set the value of a field identified by its work area alias and field name.
/// </summary>
/// <param name="symAlias"></param>
/// <param name="symField"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
function FieldPutAlias(symAlias as __Symbol,symField as __Symbol,u as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   

/// <summary>
/// Set the value of a field that is identified by its __Symbolic name.
/// </summary>
/// <param name="symVar"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
function FieldPutSym(symVar as __Symbol,u as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <param name="u"></param>
/// <returns>
/// </returns>
function FieldPutSelect(uSelect as __Usual,symField as __Symbol,u as __Usual) as __Usual
	/// THROW NotImplementedException{}
	return __Usual._NIL   



/// <summary>
/// Return a set-get code block for a field, specified as a __Symbol, in a specified work area.
/// </summary>
/// <param name="symVar"></param>
/// <param name="nArea"></param>
/// <returns>
/// </returns>
function FieldWBlockSym(symVar as __Symbol,nArea as dword) as object
	/// THROW NotImplementedException{}
	return null_object   



/// <summary>
/// Determine whether a database file is open.
/// </summary>
/// <returns>
/// </returns>
function Used() as logic
	/// THROW NotImplementedException{}
	return false   





*----------------------------------------------------------------------------

/// <summary>
/// </summary>
/// <param name="nSelect"></param>
/// <returns>
/// </returns>
function ALIAS          (nSelect)
	
	if IsNil(nSelect)
		return Alias0()
	endif
	
	return VODBAlias(nSelect)



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function BOF() AS LOGIC
	return VODBBof()



/// <summary>
/// </summary>
/// <returns>
/// </returns>

function DBAPPEND       (lReleaseLocks)
	local lRetCode  as logic
	
	if IsNil(lReleaseLocks)
		lReleaseLocks := .t.
	endif
	
	lRetCode := VODBAppend(lReleaseLocks)
	
	if !lRetCode
		//    UH 06/26/1998
		//    lRetCode := DoError(#DBAPPEND)
		NetErr(.t.)
	endif
	
	return lRetCode





/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBBUFFREFRESH  ()
	
	local lRetCode  as logic
	
	lRetCode := VODBBuffRefresh()
	
	if !lRetCode
		lRetCode := DoError(#DBBUFFREFRESH)
	endif
	
	return lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCLEARFILTER  ()
	
	local lRetCode  as logic
	
	lRetCode := VODBClearFilter()
	
	if !lRetCode
		lRetCode := DoError(#DBCLEARFILTER)
	endif
	
	return lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCLEARINDEX   (uOrder, cOrdBag)
	
	return ORDLISTCLEAR(cOrdBag, uOrder)




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCLEARRELATION()
	return VODBClearRelation()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCLOSEALL     ()
	return VODBCloseAll()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCLOSEAREA    ()
	return VODBCloseArea()



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCOMMIT       ()
	return VODBCommit()



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCOMMITALL    ()
	return VODBCommitAll()




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

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCREATE (   cName,      ;
	aStru,      ;
	xDriver,    ;
	lNew,       ;
	cAlias,     ;
	cDelim,     ;
	lJustOpen,  ;
	aHidden     ;
	)
	
	local           rddList         as _RDDLIST
	local           lKeep           as logic
	local           lRetCode        as logic
	local           aRdds           as array
	
	if aStru = NIL
		aStru := {}
	endif
	
	if !IsArray( aStru )
		return .f.
	endif
	
	
	//
	// Get "lNew" and create "lKeep" for VODBUseArea()
	// by Clipper's logic:
	//
	//      .T. -   to open in first available workarea
	//      .F. -   to open in current workarea
	//      NIL -   to close file after creating
	//
	
	if lNew = NIL
		lNew    := .t.
		lKeep   := .f.
	else
		lKeep   := .t.
	endif
	
	if lJustOpen == NIL
		lJustOpen := .f.
	endif
	
	aRdds   := __RddList(xDriver, aHidden)
	rddList := __AllocRddList(aRdds)
	
	if IsNil(cAlias)
		cAlias := ""
	endif
	
	if IsNil(cDelim)
		cDelim := ""
	endif
	
	lRetCode := VODBCreate(cName, aStru, rddList, lNew, cAlias, cDelim, lKeep, lJustOpen)
	
	if rddList != null_ptr
		MemFree(RDDLIST)
	endif
	
	if !lRetCode
		lRetCode := DoError(#DBCREATE)
	endif
	
	return lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCREATEINDEX(cName, cExpr, cobExpr, lUnique)
	
	return OrdCreate(cName, NIL, cExpr, cobExpr, lUnique)



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBCREATEORDER  (uOrder, cName, cExpr, cobExpr, lUnique)
	
	return OrdCreate(cName, uOrder, cExpr, cobExpr, lUnique)


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBDELETE ()
	
	local lRetCode  as logic
	
	lRetCode := VODBDelete ()
	
	if !lRetCode
		lRetCode := DoError(#DBDELETE)
	endif
	
	return lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbDeleteOrder(uOrder, cOrdBag)
	local cOrder as usual
	local lRet   as logic
	
	lRet := true
	
	if IsNumeric(uOrder)
		cOrder := NIL
		lRet := VODBOrderInfo(DBOI_NAME,"",uOrder, @cOrder)
		uOrder := cOrder
	endif
	
	return ORDDESTROY(uOrder, cOrdBag)



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBDRIVER       ()
	return RDDNAME()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbEval(uBlock, uCobFor, uCobWhile, nNext, nRecno, lRest)
	
	local lRetCode  as logic
	
	if IsNil(lRest)
		lRest := .f.
	endif
	
	lRetCode := VODBEval(uBlock, uCobFor, uCobWhile, nNext, nRecno, lRest)
	
	if !lRetCode
		lRetCode := DoError(#DbEval)
	endif
	
	return lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbFieldInfo(nOrdinal, nPos, xNewVal)
	
	if !VODBFieldInfo(nOrdinal, nPos, @xNewVal)
		DoError(#DbFieldInfo)
	endif
	
	return xNewVal



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbFIlter()
	
	return VODBFilter()



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbGetSelect()
	
	return VODBGetSelect()




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbGoBottom()
	
	local lRetCode  as logic
	
	lRetCode := VODBGoBottom()
	
	if !lRetCode
		lRetCode := DoError(#DbGoBottom)
	endif
	
	return lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbGoto(uRecId)
	
	local lRetCode  as logic
	
	lRetCode := VODBGoto(uRecId)
	
	if !lRetCode
		lRetCode := DoError(#DbGoto)
	endif
	
	return lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbGotop()
	
	local lRetCode  as logic
	
	lRetCode := VODBGoTop()
	
	if !lRetCode
		lRetCode := DoError(#DbGotop)
	endif
	
	return lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbInfo(nOrdinal, xNewVal)
	
	if !VODBInfo(nOrdinal, @xNewVal)
		DoError(#DbInfo)
	endif
	
	return xNewVal




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbContinue()
	
	local lRetCode as logic
	
	lRetCode := VODBContinue()
	
	if !lRetCode
		lRetCode := DoError(#DbContinue)
	endif
	
	return lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbLocate(uCobFor, uCobWhile, nNext, uRecId, lRest )
	
	local lRetCode  as logic
	
	if IsNil(lRest)
		lRest := .f.
	endif
	
	if IsNil(uCobWhile)
		uCobWhile := {|| .t. }
	else
		lRest := .t.
	endif
	
	if IsNil(nNext)
		nNext := 0
	endif
	
	lRetCode := VODBLocate(uCobFor, uCobWhile, nNext, uRecId, lRest)
	
	if !lRetCode
		lRetCode := DoError(#DbLocate)
	else
		lRetCode := VODBFound()
	endif
	
	return lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbOrderInfo(nOrdinal,cBagName, uOrder, xNewVal)
	local lKeyVal   as logic
	
	if !IsString(cBagName)
		cBagName := ""
	endif
	
	if IsString(uOrder)
		if Len(uOrder) == 0
			uOrder := NIL
		endif
	endif
	
	if nOrdinal == DBOI_KEYVAL
		lKeyVal  := .t.
		nOrdinal := DBOI_EXPRESSION
	endif
	
	VODBOrderInfo(nOrdinal, cBagName, uOrder, @xNewVal)
	
	if lKeyVal
		if IsString(xNewVal)
			if Len(xNewVal) == 0
				xNewVal := NIL
			else
				xNewVal := &(xNewVal)
			endif
		endif
	endif
	
	return xNewVal


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbPack()
	
	local lRetCode  as logic
	
	lRetCode := VODBPack()
	
	if !lRetCode
		lRetCode := DoError(#DbPack)
	endif
	
	return lRetCode




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbRecordInfo(nOrdinal, uRecId, xNewVal)
	VODBRecordInfo(nOrdinal, uRecId, @xNewVal)
	return xNewVal


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbRecall()
	
	local lRetCode  as logic
	
	lRetCode := VODBRecall()
	
	if !lRetCode
		lRetCode := DoError(#DbRecall)
	endif
	
	return lRetCode





/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbRLock(uRecord)
	
	return VODBRlock(uRecord)


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbRLockList()
	
	local lockList          as _LOCKLIST
	local aLockList := {}   as array
	local i                 as dword
	local nRecords          as usual
	
	nRecords := 0
	
	if !VODBInfo(DBI_LOCKCOUNT, @nRecords)
		DoError(#DbRLockList)
	else
		lockList := DbInfo(DBI_GETLOCKARRAY)
		
		for i := 1 to nRecords
			AAdd(aLockList, lockList.lRecno[i])
		next
	endif
	
	return aLockList

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbRSelect(nPos)
	
	Default(@nPos, 0)
	
	return VODBRSelect(nPos)




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbRUnLock(uRecId)
	
	local lRetCode  as logic
	
	lRetCode := VODBUnlock(uRecId)
	
	if !lRetCode
		lRetCode := DoError(#DbRUnLock)
	endif
	
	return lRetCode




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbSelect(nNew)
	
	local nOld  as dword
	
	Default(@nNew, 0)
	
	VODBSelect(nNew, @nOld)
	
	return nOld



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbSelectArea(xValue)
	
	local sSelect   as short
	
	sSelect := _SELECT(xValue)
	
	if sSelect = 0
		ptrErrInfo := _VODBErrInfoPtr()
		ptrErrInfo.pszArg     := AsPsz(xValue)
		ptrErrInfo.dwArgType  := UsualType(xValue)
		DoError(#DbSelectArea)
	endif
	
	return (sSelect > 0)






/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbSetSelect(nSelect)
	
	Default(@nSelect, 0)
	
	return VODBSetSelect(nSelect)




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbSymSelect(sAlias)
	
	Default(@sAlias, Alias0Sym())
	
	return VODBSymSelect(sAlias)





/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBRELATION     (wPos)
	
	local pszRelText        as psz
	local cRelation         as string
	
	Default(@wPos, 1)
	
	if !VODBRelation(wPos, @pszRelText)
		DoError(#DBRELATION)
	else
		cRelation := Psz2String(pszRelText)
	endif
	
	return cRelation



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbSetDriver(cDriver)
	
	return RddSetDefault(cDriver)



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbSetFilter(cbFilter, cFilter)
	
	local lRetCode  as logic
	
	if IsNil(cFilter)
		cFilter := "UNKNOWN"
	endif
	
	lRetCode := VODBSetFilter(cbFilter, cFilter)
	
	if !lRetCode
		lRetCode := DoError(#DbSetFilter)
	endif
	
	return lRetCode



*----------------------------------------------------------------------------
/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbSetRelation  (xAlias, uCobKey, cKey)
	
	local nSelect   as dword
	local cAlias    as string
	local xType     as dword
	local lRetCode  as logic
	
	if IsNil(cKey)
		cKey := ""
	endif
	
	xType := UsualType(xAlias)
	
	if xType = string
		nSelect := Val(xAlias)
		
		if nSelect = 0
			cAlias := xAlias
		else
			cAlias := ALIAS(nSelect)
		endif
		
	else
		cAlias := ALIAS(xAlias)
	endif
	
	lRetCode := VODBSetRelation(cAlias, uCobKey, cKey)
	
	if !lRetCode
		lRetCode := DoError(#DbSetRelation)
	endif
	
	return lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBSKIP         (nRecords)
	
	local lRetCode  as logic
	
	if IsNil(nRecords)
		nRecords := 1
	endif
	
	lRetCode := VODBSkip(nRecords)
	
	if !lRetCode
		lRetCode := DoError(#DBSKIP)
	endif
	
	return lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbUnLock()
	local lRetCode  as logic
	
	lRetCode := VODBUnlock(NIL)
	
	if !lRetCode
		lRetCode := DoError(#DbUnLock)
	endif
	
	return lRetCode




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbUnlockAll()
	return VODBUnlockAll()




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DBUSEAREA (lNew,      ;   // Select the next free Area
	xDriver,    ;   // RDD name
	cName,      ;   // File name
	cAlias,     ;   // Alias qualifier
	lShare,     ;   // Shared mode on/off
	lReadOnly,  ;   // Read only mode on/off
	aStru,      ;   // Structure as two dimensional array
	cDelim,     ;   // Character for delimitted files
	aHidden     ;   // List of hidden RDDs as array
	;
	)                   as logic    clipper
	
	
	local           lRetCode        as logic
	local           rddList         as _RDDLIST
	local           nTries          as dword
	local           aRdds           as array
	
	
	Default(@lNew, .f.)
	
	Default(@cAlias, "")
	
	Default( @lShare, !SetExclusive())
	
	Default(@lReadOnly, .f.)
	
	Default(@cDelim, "")
	
	nTries := 1
	
	aRdds := __RddList(xDriver, aHidden)
	
	rddList := __AllocRddList(aRdds)
	
	do while .t.
		
		if !Empty(aStru)
			
			lRetCode := DBCREATE   ( cName, aStru, aRdds, lNew,;
			cAlias, cDelim, .t.)
		else
			
			lRetCode := VODBUseArea(lNew, rddList, cName, cAlias,;
			lShare, lReadOnly)
			
		endif
		
		if lRetCode
			exit
		else
			if ( DoError(#DBUSEAREA, nTries) != E_RETRY )
				exit
			endif
			nTries := nTries + 1
		endif
		
	enddo
	
	if rddList != null_ptr
		MemFree(RDDLIST)
	endif
	
	return lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbZap()
	
	local lRetCode  as logic
	
	lRetCode := VODBZap()
	
	if !lRetCode
		lRetCode := DoError(#DbZap)
	endif
	
	return lRetCode


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DELETED        ()
	
	return VODBDeleted()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function EOF() AS LOGIC
	return VODBEof()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function FIELDPUT (wPos, xValue)
	
	local xRetVal as usual
	
	if VODBFieldPut(wPos, xValue)
		xRetVal := xValue
	else
		DoError(#FIELDPUT)
	endif
	
	return xRetVal

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function FieldGet(wPos)
	
	local xRetVal as usual
	
	Default(@wPos, 1)
	
	if !VODBFieldGet(wPos, @xRetVal)
		DoError(#FIELDGET)
	endif
	
	return xRetVal

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function FLOCK          ()
	
	return VODBFlock()

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function Found()
	return VODBFound()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function Header()
	return DbInfo(DBI_GETHEADERSIZE)





/// <summary>
/// </summary>
/// <returns>
/// </returns>
function LastRec()
	
	return VODBLastRec()



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function LUpdate()
	return DbInfo(DBI_LASTUPDATE)




/// <summary>
/// </summary>
/// <returns>
/// </returns>

function RDDCount(nType)
	if IsNil(nType)
		nType := RDT_FULL + RDT_TRANSFER
	endif
	return VODBRddCount(nType)


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function RDDInfo        (nOrdinal, xNewVal)
	
	if !VODBRDDInfo(nOrdinal, @xNewVal)
	endif
	
	return xNewVal



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function RDDList        (nType)
	
	local rddList           as _RDDLIST
	local aRddList := {}    as array
	local i                 as dword
	local nCount            as dword
	
	if IsNil(nType)
		nType := RDT_FULL + RDT_TRANSFER
	endif
	
	nCount := VODBRddCount(nType)
	
	if nCount > 0
		
		rddList := MemAlloc( (_sizeof(dword)) + (nCount * _sizeof(symbol)) )
		rddList.uiRddCount := nCount
		
		if VODBRddList(rddList, nType)
			
			for i := 1 to rddList.uiRddCount
				AAdd( aRddList, Symbol2String(rddList.atomRddName[i]) )
			next
			
		endif
		
		MemFree(RDDLIST)
		
	endif
	return aRddList



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function RDDName        ()
	local cRet      as string
	
	if Used()
		cRet := VODBRddName()
	else
		cRet := RddSetDefault(NIL)
	endif
	
	return cRet


/// <summary>
/// </summary>
/// <returns>
/// </returns>

function RddSetDefault  (cDriver)
	
	if !IsString(cDriver)
		cDriver := NIL
	else
		if (cDriver == null_string)  .OR. ( Empty(Trim(cDriver)) )
			cDriver := NIL
		endif
	endif
	
	return RDDINFO(_SET_DEFAULTRDD, cDriver)

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function RecSize() as long
	return DbInfo(DBI_GETRECSIZE)





/// <summary>
/// </summary>
/// <returns>
/// </returns>
function RLock()
	return (VODBRlock(NIL))



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function RecCount()
	return VODBLastRec()




/// <summary>
/// </summary>
/// <returns>
/// </returns>
function RecNo()
	return VODBRecno()


/// <summary>
/// </summary>
/// <returns>
/// </returns>
function _Select(xValue)
	local nSelect           as dword
	local sAlias            as symbol
	local xType             as dword
	local nAsc              as dword
	
	
	if IsNil(xValue)
		return( VODBGetSelect() )
	endif
	
	xType := UsualType(xValue)
	
	if xType =symbol
		nSelect := VODBSymSelect(xValue)
		
	elseif xType =string
		nSelect := 0
		
		if SLen(xValue) = 1
			nSelect := Val(xValue)
			nAsc := Asc( Upper(xValue) )
			if nAsc > 64 .AND. nAsc < 75
				nSelect := nAsc - 64
			endif
		endif
		
		if (nSelect > 0) .OR. ("0" == xValue)
			nSelect := VODBSetSelect(int(nSelect))
		else
			sAlias  := SysAddAtom( String2Psz( Upper( AllTrim(xValue) ) ) )
			nSelect := VODBSymSelect(sAlias)
		endif
		
	else
		nSelect := VODBSetSelect(xValue)
	endif
	
	return nSelect



/// <summary>
/// </summary>
/// <returns>
/// </returns>
function Select(xValue)
	
	local sSelect   as dword
	local sCurrent  as dword
	
	sCurrent := VODBGetSelect()
	
	sSelect := _SELECT(xValue)
	
	VODBSetSelect(int(sCurrent))
	
	return sSelect



function DBMemoExt      (cDriver)
	
	if IsNil(cDriver)
		cDriver := ""
	endif
	
	return VODBMemoExt(cDriver)




function RDDVersion     (nParm)
	
	if !IsNumeric(nParm)
		nParm := 0
	endif
	
	return DbInfo(DBI_RDD_VERSION, nParm)








function DBMemoField        (xField)
	local n,i        as dword
	local xRet       as usual
	local nFields    as dword
	
	if IsNumeric(xField)
		n := xField
	elseif IsSymbol(xField)
		n := FieldPosSym(xField)
	elseif IsString(xField)
		n := FieldPos(xField)
		else
		nFields := FCount()
		for i := 1 to nFields
			if DbFieldInfo(i, DBS_TYPE) == "M"
				n := i
				exit
			endif
		next
	endif
	
	xRet := DbInfo( DBI_MEMOFIELD, n )
	
	return xRet


//---------------------------------------------------------------------------
//
//  DoError()
//
//  Call default error handler
//

function DoError        (nSymFunc, nTries)
	//RvdH 030926 Changed from as USUAL to as Error
	local oError    as Error
	
	oError := ErrorBuild(_VODBErrInfoPtr())
	oError:Stack := ErrorStack(1) 
	
	oError:FuncSym := nSymFunc
	
	if !IsNil(nTries)
		oError:Tries   := nTries
	endif
	return Eval(ErrorBlock(), oError)



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


function __AllocRddList (aRdds as array)    as _RDDLIST     pascal
	local n,i           as dword
	local rddList       as _RDDLIST
	n       := ALen(aRdds)
	rddList := MemAlloc( (_sizeof(dword)) + (n * _sizeof(symbol)) )
	
	rddList.uiRddCount := n
	~"RANGECHECK-"
	for i := 1 to n
		rddList.atomRddName[i] := SysAddAtomUpperA(aRdds[i])
	next
	return rddList
//

function __RddList      (xDriver, aHidden)
	
	local   nType   as dword
	local   aRdds   as array
	local   n       as dword
	local   i       as dword
	local   lBlob   as logic
	local   lDbf    as logic
	
	//  UH 09/24/1997
	//  IF (!IsArray(xDriver) .OR. IsString(xDriver))
	if IsArray(xDriver)
		nType := array
	elseif IsString(xDriver)
		//  UH 11/13/1997
		if SLen(xDriver) = 0
			xDriver := RddSetDefault()
		endif
		nType := string
	else
		xDriver := RddSetDefault()
		nType := string
	endif
	
	//  UH 09/24/1997
	//  nType := UsualType(xDriver)
	
	if nType == array
		aRdds := xDriver
		
	elseif nType == string
		aRdds := {}
		
		UpperA(xDriver)
		
		do case
			case xDriver = "DBFNTX"
		lDbf := .t.
			case xDriver = "_DBFCDX"
		lDbf := .t.
			case xDriver = "DBFCDX"
				lBlob := .t.
				lDbf  := .t.
		xDriver := "_DBFCDX"
			case xDriver = "DBFMDX"
		lDbf := .t.
			otherwise
				lDbf := .f.
		lBlob := .f.
		endcase
		
		if lDbf
			AAdd(aRdds, "CAVODBF")
		endif
		
		AAdd(aRdds, xDriver)
		
		// UH (for David, Don)
		if lBlob
			AAdd(aRdds, "DBFCDX")
		endif
		
	endif
	
	if UsualType(aHidden) == array
		n := ALen(aHidden)
		
		for i := 1 to n
			AAdd(aRdds, aHidden[i])
		next
	endif
	
	return aRdds


function _DbCreate(cFile1, cFile2, cDriver,lNew, cAlias)      as logic clipper
	
	local aStruct       as array
	local oError        as usual
	local i,n           as int
	local uErrBlock     as usual
	local nSelect       as int
	local aField        as array
	
	field field_name, field_type, field_len, field_dec
	
	nSelect := 0
	
	Default(@lNew, .f.)
	
	if ( Used() .AND. !lNew )
		DBCLOSEAREA()
	endif
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	
	begin sequence
		
		if ( Empty(cFile2) )
			
			DBCREATE(cFile1,                                    ;
			{ {"FIELD_NAME", "C", 10, 0},   ;
			{"FIELD_TYPE", "C", 1, 0},      ;
			{"FIELD_LEN", "N", 3, 0},       ;
			{"FIELD_DEC", "N", 3, 0} },     ;
			cDriver,                        ;
			.f.,                            ;
			cAlias)
			
			
		else
			
			
			DBUSEAREA(lNew, cDriver, cFile2)
			
			aStruct := {}
			
			n := LASTREC()
			
			VODBGoTop()
			
			do while !EOF()
				aField := {}
				AAdd(aField, AllTrim(field_name))
				AAdd(aField, AllTrim(field_type))
				AAdd(aField, field_len)
				AAdd(aField, field_dec)
				
				AAdd( aStruct, aField )
				DBSKIP(1, .f.)
			enddo
			
			VODBCloseArea()
			
			if lNew
				VODBSetSelect(nSelect)
			endif
			
			
			
			
			for i := 1 to n
				
				if (aStruct[i, DBS_TYPE] == "C") .AND. (aStruct[i, DBS_DEC] != 0)
					aStruct[i,DBS_LEN] += aStruct[i,DBS_DEC] * 256
				endif
				
			next
			
			DBCREATE(cFile1, aStruct, cDriver, lNew, cAlias )
			
		endif
		
		
		recover using oError
		VODBCloseArea()
		oError:FuncSym := #_DBCREATE
		Eval( uErrBlock, oError)
	end sequence
	
	ErrorBlock(uErrBlock)
	
	return ( Used() )





function AFields(aNames, aTypes, aLens, aDecs)  as dword clipper
	
	local aStruct           as array
	local siCount           as dword
	local si                as dword
	local lNamesOk  := .f.  as logic
	local lTypesOk  := .f.  as logic
	local lLensOk   := .f.  as logic
	local lDecsOk   := .f.  as logic
	
	if (Empty(aStruct := DbStruct() ))
		return (0)
	endif
	
	siCount := ALen(aStruct)
	
	if UsualType(aNames) == array
		siCount := Min(siCount, Len(aNames) )
		lNamesOk := .t.
	endif
	
	
	if UsualType(aTypes) == array
		siCount := Min( siCount, Len(aTypes) )
		lTypesOk := .t.
	endif
	
	
	if UsualType(aLens) == array
		siCount := Min( siCount, Len(aLens) )
		lLensOk := .t.
	endif
	
	
	if UsualType(aDecs) == array
		siCount := Min( siCount, Len(aDecs) )
		lDecsOk := .t.
	endif
	
	
	for si := 1 to siCount
		
		if lNamesOk
			aNames[si] := aStruct[si, DBS_NAME]
		endif
		
		if lTypesOk
			aTypes[si] := aStruct[si, DBS_TYPE]
		endif
		
		if lLensOk
			aLens[si]  := aStruct[si, DBS_LEN]
		endif
		
		if lDecsOk
			aDecs[si]  := aStruct[si, DBS_DEC]
		endif
		
	next
	
	
	return siCount





function DbCopyStruct(cFile as string, aFields as array) as logic pascal
	
	return DBCREATE(cFile, __DBFLEDIT(DbStruct(), aFields, null_array) )



function DbCOpyXStruct(cFile) as logic pascal
	
	local siSaveSel,n,i as dword
	local aStruct       as array
	local lRetCode      as logic
	local oError        as usual
	local uErrBlock     as usual
	
	field field_name, field_type, field_len, field_dec
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	begin sequence
		
		
		if !Used()
			break DBCMDError()
		endif
		
		aStruct := DbStruct()
		
		n := Len(aStruct)
		
		VODBSelect(0, @siSaveSel)
		
		_DbCreate(cFile)
		
		
		
		for i := 1 to n
			
			if aStruct[i, DBS_TYPE] == "C" .AND. aStruct[i, DBS_LEN] > 255
				aStruct[i, DBS_DEC] := short(aStruct[i, DBS_LEN] / 256)
				aStruct[i, DBS_LEN] := aStruct[i, DBS_LEN] % 256
			endif
			
			lRetCode := DBAPPEND()
			
			field_name  := aStruct[i, DBS_NAME]
			field_type  := aStruct[i, DBS_TYPE]
			field_len   := aStruct[i, DBS_LEN]
			field_dec   := aStruct[i, DBS_DEC]
			
		next
		
		if (VODBGetSelect() <> siSaveSel)
			DBCLOSEAREA()
			VODBSetSelect(int(siSaveSel))
		endif
		
		recover using oError
		oError:FuncSym := #DBCOPYXSTRUCT
		Eval( uErrBlock, oError)
		lRetCode := .f.
	end sequence
	
	ErrorBlock(uErrBlock)
	
	return (lRetCode)




/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
function DbStruct() as array pascal
	
	local aStruct   as array
	local nFCount   as dword
	local nProps    as dword
	local i,j       as dword
	local aField    as array
	local xNewVal   as usual
	
	aStruct := {}
	nFCount := FCount()
	
	if !Used()
		ptrErrInfo := _VODBErrInfoPtr()
		
		ptrErrInfo.dwGenCode      := EG_NOTABLE
		ptrErrInfo.dwSubCode      := EDB_NOTABLE
		ptrErrInfo.pszSubSystem   := String2Psz("DBCMD")
		ptrErrInfo.dwSeverity     := ES_ERROR
		ptrErrInfo.lCanDefault    := .t.
		ptrErrInfo.symFuncSym     := #DBSTRUCT
		
		DefErrorGen(ptrErrInfo)
	else
		for i := 1 upto nFCount
			aField := {}
			
			
			if !VODBFieldInfo(DBS_PROPERTIES, i, @xNewVal)
				DoError(#DbFieldInfo)
			endif
			nProps:= xNewVal
			
			for j := 1 upto nProps
				if !VODBFieldInfo(j, i, @xNewVal)
					DoError(#DbFieldInfo)
				endif
				AAdd(aField, xNewVal)
			next
			
			AAdd(aStruct, aField)
			
		next
	endif
	
	return aStruct







static function ParamError  (dwArgNum  as dword  ,    ;
	dwArgType as dword       )       as usual pascal
	
	local oError    as usual
	
	ptrErrInfo := _VODBErrInfoPtr()
	
	ptrErrInfo.pszSubSystem   := String2Psz("DBCMD")
	ptrErrInfo.dwGenCode      := EG_ARG
	ptrErrInfo.dwSeverity     := ES_ERROR
	ptrErrInfo.lCanDefault    := .f.
	ptrErrInfo.lCanRetry      := .t.
	ptrErrInfo.lCanSubstitute := .f.
	ptrErrInfo.dwArgType      := dwArgType
	ptrErrInfo.dwArgNum       := dwArgNum
	
	oError := DefErrorGen(ptrErrInfo)
	
	return oError

static function DBCMDError  ()                              as usual pascal
	
	local oError    as usual
	
	ptrErrInfo := _VODBErrInfoPtr()
	
	ptrErrInfo.dwGenCode      := EG_NOTABLE
	ptrErrInfo.dwSubCode      := EDB_NOTABLE
	ptrErrInfo.pszSubSystem   := String2Psz("DBCMD")
	ptrErrInfo.dwSeverity     := ES_ERROR
	ptrErrInfo.lCanDefault    := .t.
	
	oError := DefErrorGen(ptrErrInfo)
	
	//	UH 05/03/1999
	return oError





/// <summary>
/// Return and optionally change the setting that determines whether to use the High Performance (HP) locking schema for newly created .NTX files.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function IndexHPLock(lSet as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   

/// <summary>
/// Return and optionally change the setting that determines whether to use the new locking offset of -1 (0xFFFFFFFF) for .NTX files.
/// </summary>
/// <param name="lSet"></param>
/// <returns>
/// </returns>
function NewIndexLock(lSet as __Usual) as logic
	/// THROW NotImplementedException{}
	return false   



/// <summary>
/// Return the setting that determines whether to use the new locking offset of -1 (0xFFFFFFFF) for .NTX files.
/// </summary>
/// <returns>
/// </returns>
function NewLocks() as logic
	/// THROW NotImplementedException{}
	return false   