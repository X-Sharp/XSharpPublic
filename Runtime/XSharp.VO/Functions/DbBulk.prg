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
function __allocNames    (aStru as array)                as _FIELDNAMES  pascal
	
	local n,i       as dword
	local cName		as string
	local pszName 	as psz
	local nSize		as dword
	local fldNames  as _FIELDNAMES
	~"RANGECHECK-"
	n := ALen(aStru)
	
	fldNames := MemAlloc( (_sizeof(dword)) + (n * _sizeof(psz)) )
	
	fldNames.uiFieldCount := n
	
	for i := 1 to n
		//	UH 11/12/1997
		//	fldNames.lpbName[i] := PTR(_CAST, Upper(aStru[i]))
		cName := Upper(aStru[i])
		nSize := SLen(cName) + 1
		pszName := MemAlloc(nSize)
		if pszName != null_psz
			MemCopy(pszName, ptr(_cast, cName), nSize)
		endif
		fldNames.lpbName[i] := pszName
	next
	
	return fldNames
*/
//
// __DBAvg()
//
//  Star issue 1241359 is a DBEVAL() problem
//  but this function replaces the need to have
//  a PUBLIC variable __Avg for computing the
//  sum value.
//

function __DBAvg        (siValue as short)          as short        pascal
	
	local  siRet    as short
	static siSum    as short
	
	if siValue == 2
		return siSum
	endif
	
	siRet := siSum
	
	if siValue == 0
		siSum := 0
	else
		siSum := siSum + siValue
	endif
	
	return siRet

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
function __DBFLEDIT      (aStruct as array, aNames as array, aMatch as array) as array export local
	
	local aNew      as array
	local cobScan   as codeblock
	local cName     as string
	local n, i, j   as dword
	local lMatch	as logic
	
	
	if Empty(aNames)
		return (aStruct)
	endif
	
	//	UH 11/30/1998
	if Empty(aMatch)
		lMatch := .f.
	else
		lMatch := .t.
	endif
	
	aNew:= {}
	n   := Len(aNames)
	
	for i := 1 to n
		AAdd(aNew, WithoutAlias(AllTrim(aNames[i])))
	next
	
	aNames := aNew
	
	aNew    := {}
	cobScan   := {|aFld| aFld[DBS_NAME] == cName}
	
	for i := 1 to n
		cName := aNames[i]
		j := AScan(aStruct, cobScan)
		
		if j > 0
			if lMatch
				if aMatch[i, DBS_TYPE] == aStruct[j, DBS_TYPE]
					AAdd(aNew, aStruct[j])
				endif
			else
				AAdd(aNew, aStruct[j])
			endif
		endif
	next
	
	return aNew

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
function __UniqueAlias   (cDbfName as string)            as string       pascal
	
	local cAlias    as string
	local n         as dword
	local nSelect   as dword
	
	//  UH 11/09/1999
	//  n := At(".", cDbfName )
	n := RAt(".", cDbfName )
	
	if n > 0
		cDbfName := SubStr(cDbfName, 1, n - 1)
	endif
	
	n := RAt("\", cDbfName)
	
	if n > 0
		cDbfName := SubStr(cDbfName, n+1)
	endif
	
	n := 1
	
	cAlias := Upper(cDbfName)
	
	nSelect := SELECT(cAlias)
	
	do while nSelect > 0
		if Len(cAlias) < 9
			cAlias := cAlias + AllTrim(Str(n))
		else
			cAlias := SubStr(cAlias, 1, 8) + AllTrim(Str(n))
		endif
		
		n++
		nSelect := SELECT(cAlias)
	enddo
	
	return cAlias





function DbApp(cFile, aFields, uCobFor, uCobWhile,nNext, nRec, lRest,cDriver, aHidden)     as logic clipper
	local siFrom        as dword
	local siTo          as dword
	local n, i          as dword
	local aStruct       as array
	local aMatch		  as array
	local lRetCode      as logic
	local lAnsi         as logic
	
	lAnsi  := SetAnsi()
	try	
		siTo := VODBGetSelect()
		if !Used()
			break DBCMDError()
		endif
		
		if Empty( aStruct := __DBFLEDIT(DbStruct(), aFields, null_array) )
			break ParamError(array, 2)
		endif
		
		//RvdH 061218 Added aHidden
		DBUSEAREA(true, cDriver, cFile, __UniqueAlias(cFile), true, true,/*aStru*/,/*cDelim*/, aHidden)
		siFrom := VODBGetSelect()
		
		aFields := {}
		
		n := FCount()
		aMatch := DbStruct()
		
		for i := 1 to n
			AAdd(aFields, FieldName(i))
		next
		
		if ( !lAnsi ) .AND. ( DbInfo(DBI_ISANSI) )
			SetAnsi(.t.)
		endif
		
		if !Empty(aStruct := __DBFLEDIT(aStruct, aFields, aMatch))
			lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		endif
		
		if (siFrom > 0)
			DBCLOSEAREA()
		endif
		
		VODBSetSelect(int(siTo))
		
		
	catch e as Exception
		if  siFrom > 0
			VODBSetSelect(int(siFrom))
			DBCLOSEAREA()
		endif
		
		oError:FuncSym := #DBAPP
		throw Error{e}
	end try
	
	
	SetAnsi( lAnsi )
	
	return (lRetCode)


function DbAppDelim(cFile, cDelim, aFields, uCobFor, uCobWhile, nNext,nRec, lRest)as logic 
	
	local siFrom        as dword
	local siTo          as dword
	local siPos         as dword
	local aStruct       as array
	local lRetCode      as logic
	local lAnsi         as logic
	local lDbfAnsi      as logic
	
	
	lAnsi  := SetAnsi()
	
	
	try
		
		siTo := VODBGetSelect()
		
		if (Empty( aStruct := __DBFLEDIT(DbStruct(), aFields, null_array) ))
			break ParamError(array, 3)
		endif
		
		if Empty(cFile)
			break ParamError(string, 1)
		else
			if Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			endif
		endif
		
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DBCREATE(cFile, aStruct, "DELIM", .t., __UniqueAlias(cFile), cDelim, .t.)
		
		siFrom := VODBGetSelect()
		
		
		if ( !lAnsi .AND. lDbfAnsi)
			SetAnsi(.t.)
		endif
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		DBCLOSEAREA()
		
		VODBSetSelect(int(siTo))
		
		
	catch e as Exception
		oError:FuncSym := #DBAPPDELIM
		throw Error{e}
	end try
	
	
	
	SetAnsi( lAnsi )
	
	return (lRetCode)




function DbAppSdf(cFile, aFields, uCobFor,;
	uCobWhile, nNext, nRec, ;
	lRest                   )      as logic clipper
	
	local siFrom        as dword
	local siTo          as dword
	local siPos         as dword
	local aStruct       as array
	local lRetCode      as logic
	local oError        as usual
	local uErrBlock     as usual
	local lAnsi         as logic
	local lDbfAnsi      as logic
	
	
	lAnsi  := SetAnsi()
	
	try		
		siTo := VODBGetSelect()
		
		if (Empty( aStruct := __DBFLEDIT(DbStruct(), aFields, null_array) ))
			throw ParamError(array, 2)
		endif
		
		if Empty(cFile)
			throw ParamError(string, 1)
		else
			if Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			endif
		endif
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DBCREATE(cFile, aStruct, "SDF", .t., __UniqueAlias(cFile), ,.t.)
		
		siFrom := VODBGetSelect()
		
		if ( !lAnsi .AND. lDbfAnsi )
			SetAnsi(.t.)
		endif
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		DBCLOSEAREA()
		VODBSetSelect(int(siTo))
		
	catch e as Exception
		oError:FuncSym := #DBAPPSDF
		throw Error{e}
	end try
	
	
	SetAnsi( lAnsi )
	
	return (lRetCode)



function DbCOpy(cFile, aFields, uCobFor,;
	uCobWhile, nNext, nRec, ;
	lRest, cDriver, aHidden    )     as logic clipper
	
	local siFrom        as dword
	local siTo          as dword
	local aStruct       as array
	local lRetCode      as logic
	local oError        as usual
	local uErrBlock     as usual
	local lAnsi         as logic
	local lDbfAnsi      as logic
	
	lAnsi    := SetAnsi()
	
	siFrom   := VODBGetSelect()
	lRetCode := .f.
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	begin sequence
		
		if !Used()
			break DBCMDError()
		endif
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		if  Empty(AFields)                      .AND. ;
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
		else
			if ( Empty(aStruct := __DBFLEDIT(DbStruct(), aFields, null_array)) )
				break ParamError(array, 2)
			endif
			
			DBCREATE( cFile, aStruct, cDriver,, __UniqueAlias(cFile),,,aHidden)
			
			if ( !lAnsi ) .AND. ( DbInfo(DBI_ISANSI) )
				SetAnsi(.t.)
			endif
			
			DBUSEAREA(.t., cDriver, cFile, __UniqueAlias(cFile),,,,,aHidden)
			
			VODBSelect(siFrom, @siTo)
			
			lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
			
			if (siTo > 0)
				VODBSetSelect(int(siTo))
				VODBCloseArea()
			endif
			
			VODBSetSelect(int(siFrom))
		endif
		
		recover using oError
		SetAnsi(lAnsi)
		oError:FuncSym := #DBCOPY
		Eval( uErrBlock, oError)
	end sequence
	
	ErrorBlock(uErrBlock)
	
	SetAnsi( lAnsi )
	
	return (lRetCode)



define BUFF_SIZE := 0x00008000
define FO_CREATE := 0x00001000

function DBFileCopy( hfFrom, cFile, cFullPath )
	
	local lRetCode  as logic
	local ptrBuff   as ptr
	local hfTo      as ptr
	local oError    as Error
	local n         as dword
	local dwPos     as long
	
	
	if At(".", cFile) == 0
		cFile := cFile + Right(cFullPath, 4)
	endif
	
	hfTo := FxOpen( cFile, FO_CREATE, "")
	
	if hfTo == F_ERROR
		
		oError := Error{1}
		oError:SubSystem                := "DBCMD"
		oError:GenCode                  := EG_OPEN
		oError:OsCode                   := DosError()
		oError:FuncSym                  := #DBCOPY
		oError:FileName                 := FPathName()
		
		oError:@@Throw()
		
		lRetCode := .f.
		
	else
		
		dwPos := FSeek3(hfFrom, 0, FS_RELATIVE )
		
		
		FSeek3(hfFrom, 0, FS_SET )
		
		n       := BUFF_SIZE
		ptrBuff := MemAlloc(n)
		
		do while n == BUFF_SIZE
			n := FRead3(hfFrom, ptrBuff, BUFF_SIZE)
			
			FWrite3(hfTo, ptrBuff, n)
		enddo
		
		MemFree(ptrBuff)
		
		FClose(hfTo)
		
		lRetCode := .t.
		
		
		FSeek3(hfFrom, dwPos, FS_SET )
		
	endif
	
	return lRetCode





function DBCOPYDELIM     (cFile, cDelim, aFields,   ;
	uCobFor, uCobWhile, nNext,;
	nRec, lRest                )   as logic clipper
	
	local siFrom        as dword
	local siTo          as dword
	local siPos         as dword
	local aStruct       as array
	local lRetCode      as logic
	local oError        as usual
	local uErrBlock     as usual
	local lAnsi         as logic
	local lDbfAnsi      as logic
	
	lAnsi  := SetAnsi()
	
	siFrom := VODBGetSelect()
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	begin sequence
		
		if !Used()
			break DBCMDError()
		endif
		
		if Empty(aStruct := __DBFLEDIT(DbStruct(), aFields, null_array))
			break ParamError(array, 3)
		endif
		
		if Empty(cFile)
			break ParamError(string, 1)
		else
			if Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			endif
		endif
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DBCREATE(cFile, aStruct, "DELIM", .t., __UniqueAlias(cFile), cDelim)
		
		if ( !lAnsi .AND. lDbfAnsi)
			SetAnsi(.t.)
		endif
		
		VODBSelect(siFrom, @siTo)
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		VODBSetSelect(int(siTo))
		DBCLOSEAREA()
		VODBSetSelect(int(siFrom))
		
		recover using oError
		oError:FuncSym := #DBCOPYDELIM
		Eval( uErrBlock, oError)
	end sequence
	
	ErrorBlock(uErrBlock)
	
	
	SetAnsi( lAnsi )
	
	return (lRetCode)



function DbCopySDF(cFile, aFields, uCobFor,;
	uCobWhile, nNext, nRec, ;
	lRest                      )   as logic clipper
	
	local siFrom        as dword
	local siTo          as dword
	local siPos         as dword
	local aStruct       as array
	local lRetCode      as logic
	local oError        as usual
	local uErrBlock     as usual
	local cAlias        as string
	local lAnsi         as logic
	local lDbfAnsi      as logic
	
	
	lAnsi  := SetAnsi()
	
	siFrom := VODBGetSelect()
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	begin sequence
		
		if !Used()
			break DBCMDError()
		endif
		
		if Empty(aStruct := __DBFLEDIT(DbStruct(), aFields, null_array))
			break ParamError(array, 2)
		endif
		
		if Empty(cFile)
			break ParamError(string, 1)
		else
			if Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			endif
		endif
		
		cAlias := __UniqueAlias(cFile)
		
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DBCREATE(cFile, aStruct, "SDF", .t., cAlias)
		
		
		if ( !lAnsi .AND. lDbfAnsi)
			SetAnsi(.t.)
		endif
		
		VODBSelect(siFrom, @siTo)
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		VODBSetSelect(int(siTo))
		DBCLOSEAREA()
		VODBSetSelect(int(siFrom))
		
		recover using oError
		oError:FuncSym := #DBCOPYSDF
		Eval( uErrBlock, oError)
	end sequence
	
	ErrorBlock(uErrBlock)
	
	
	SetAnsi( lAnsi )
	
	return (lRetCode)




function DbJoin(cAlias, cFile, aFields, uCobFor) as logic clipper
	
	local siFrom1       as dword
	local siFrom2       as dword
	local siTo          as dword
	local aStruct       as array
	local oError        as usual
	local lRetCode      as logic
	local uErrBlock     as usual
	
	//  UH 02/03/1997
	local pJoinList     as _JOINLIST
	
	
	if IsNil(uCobFor)
		uCobFor := {|| .t.}
	endif
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	begin sequence
		
		siFrom1 := VODBGetSelect()
		
		siFrom2 := SELECT(cAlias)
		
		
		if siFrom2 = 0
			break ParamError(1, string)
		endif
		
		VODBSetSelect(int(siFrom1))
		
		
		if Empty( aStruct := __TargetFields(cAlias, aFields, @pJoinList) )
			ClearstrucErrInfo()
			
			strucErrInfo.pszSubSystem   := String2Psz("DBCMD")
			strucErrInfo.dwGenCode      := EG_ARG
			strucErrInfo.dwSubCode      := EDB_NOFIELDS
			strucErrInfo.dwSeverity     := ES_ERROR
			strucErrInfo.lCanDefault    := .f.
			strucErrInfo.lCanRetry      := .f.
			strucErrInfo.lCanSubstitute := .f.
			
			oError := DefErrorGen(_VODBErrInfoPtr())
			
			break oError
		endif
		
		DBCREATE( cFile, aStruct,"" , .t., "" )
		
		
		VODBSelect(siFrom1, @siTo)
		
		pJoinList.uiDestSel := siTo
		
		lRetCode := DbGotop()
		
		do while !EOF()
			
			VODBSetSelect(int(siFrom2))
			
			lRetCode := DbGotop()
			
			do while ! EOF()
				
				VODBSetSelect(int(siFrom1))
				
				if ( Eval(uCobFor) )
					DbJoinAppend(siTo, pJoinList)
				endif
				
				VODBSetSelect(int(siFrom2))
				DBSKIP(1)
			enddo
			
			VODBSetSelect(int(siFrom1))
			
			DBSKIP(1)
			
		enddo
		
		recover using oError
		oError:FuncSym := #DBJOIN
		Eval( uErrBlock, oError)
	end sequence
	
	ErrorBlock(uErrBlock)
	
	MemFree(pJoinList)
	
	if siTo > 0
		VODBSetSelect(int(siTo))
		DBCLOSEAREA()
	endif
	
	VODBSetSelect(int(siFrom1))
	
	return (lRetCode)

/// <summary>
/// </summary>
/// <returns>
/// </returns>
/*
function DbJoinAppend(nSelect    as dword,    ;
	struList   as _JOINLIST)   as logic        pascal
	
	local lRetCode as logic
	
	lRetCode := VODBJoinAppend(nSelect, struList)
	
	if !lRetCode
		lRetCode := DoError(#DbJoinAppend)
	endif
	
	return lRetCode
*/







function DbSort(	cFile, aFields, uCobFor,;
	uCobWhile, nNext, nRec, ;
	lRest                   )   as logic clipper
	
	local siFrom        as dword
	local siTo          as dword
	local aStruct       as array
	local oError        as usual
	local lRetCode      as logic
	local fnFieldNames  as _FIELDNAMES
	local fnSortNames   as _FIELDNAMES
	local uErrBlock     as usual
	//	UH 09/23/1997
	local cRdd 			as string
	
	fnFieldNames := null_ptr
	fnSortNames  := null_ptr
	
	siFrom := VODBGetSelect()
	
	Default(@lRest, .f.)
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	begin sequence
		
		if !Used()
			break DBCMDError()
		endif
		
		aStruct := DbStruct()
		
		//	UH 09/23/1997
		cRdd := RDDNAME()
		
		fnFieldNames := _allocFieldNames(aStruct)
		
		if Empty(AFields)
			break ParamError(array, 2)
		endif
		
		fnSortNames := __allocNames(AFields)
		
		//	UH 09/23/1997
		//	DBCREATE(cFile, aStruct, "", .T.)
		DBCREATE(cFile, aStruct, cRdd, .t.)
		
		VODBSelect(siFrom, @siTo)
		
		lRetCode := VODBSort(siTo, fnFieldNames, uCobFor, uCobWhile, nNext, nRec, lRest, fnSortNames)
		
		if !lRetCode
			ptrErrInfo := _VODBErrInfoPtr()
			break DefErrorGen(ptrErrInfo)
		endif
		
		_freeFieldNames(fnFieldNames)
		
		_freeFieldNames(fnSortNames)
		
		if (siTo > 0)
			VODBSetSelect(int(siTo))
			VODBCloseArea()
		endif
		
		VODBSetSelect(int(siFrom))
		
		
		recover using oError
		
		if fnFieldNames != null_ptr
			_freeFieldNames(fnFieldNames)
		endif
		
		if fnSortNames != null_ptr
			_freeFieldNames(fnSortNames)
		endif
		
		oError:FuncSym := #DBSORT
		Eval( uErrBlock, oError)
	end sequence
	
	ErrorBlock(uErrBlock)
	
	return lRetCode

/// <summary>
/// </summary>
/// <returns>
/// </returns>
function DbTrans(nTo, aStru, uCobFor, uCobWhile, nNext, nRecno, lRest) as logic clipper
	
	local fldNames  as _FIELDNAMES
	local lRetCode  as logic
	
	
	if !IsNil(uCobWhile)
		lRest := .t.
	endif
	
	if IsNil(lRest)
		lRest := .f.
	endif
	
	fldNames := _allocFieldNames(aStru)
	
	
	lRetCode := VODBTrans(nTo, fldNames, uCobFor, uCobWhile, nNext, nRecno, lRest)
	
	_freeFieldNames(fldNames)
	
	if !lRetCode
		lRetCode := DoError(#DbTrans)
	endif
	
	return lRetCode



/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
function DbTotal(cFile, bKey, aFields,     ;
	uCobFor, uCobWhile, nNext,;
	nRec, lRest, xDriver ) 	as logic clipper
	
	local siFrom        as dword
	local siTo          as dword
	local i, n          as dword
	local aStruct       as array
	local aFldNum       as array
	local aNum          as array
	local lSomething    as logic
	local kEval
	local oError        as usual
	local lRetCode      as logic
	local fldNames      as _FIELDNAMES
	local uErrBlock     as usual
	
	
	if IsNil(uCobWhile)
		uCobWhile := {|| .t.}
	else
		lRest := .t.
	endif
	
	if IsNil(uCobFor)
		uCobFor := {|| .t.}
	endif
	
	if IsNil(lRest)
		lRest := .f.
	endif
	
	
	if !IsNil(nRec)
		DbGoto(nRec)
		nNext := 1
	else
		
		if IsNil(nNext)
			nNext := -1
		else
			lRest := .t.
		endif
		
		if !lRest
			DbGotop()
		endif
		
	endif
	
	aFldNum := {}
	
	n := Len(AFields)
	
	for i := 1 to n
		AAdd(aFldNum, FieldPos( AllTrim(AFields[i]) ) )
	next
	
	aNum  := ArrayNew(n)
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	begin sequence
		
		if !Used()
			break DBCMDError()
		endif
		
		aStruct := DbStruct()
		
		siFrom := VODBGetSelect()
		
		
		
		aStruct := {}
		
		n := FCount()
		
		for i := 1 to n
			if DbFieldInfo(DBS_TYPE, i) != "M"
				
				AAdd(aStruct, { FieldName(i)   , ;
				DbFieldInfo(DBS_TYPE, i), ;
				DbFieldInfo(DBS_LEN, i),  ;
				DbFieldInfo(DBS_DEC, i)   ;
				}                           ;
				)
				
			endif
		next
		
		
		if ( Empty(aStruct) )
			break ParamError(array, 3)
		endif
		
		fldNames := _allocFieldNames(aStruct)
		
		//	DBCREATE( cFile, aStruct, "", .T.)
		if IsNil(xDriver)
			xDriver := RddSetDefault()
		endif
		DBCREATE( cFile, aStruct, xDriver, .t.)
		
		VODBSelect(siFrom, @siTo)
		
		n := Len(aFldNum)
		
		do while ( (!EOF()) .AND. nNext != 0 .AND. Eval(uCobWhile) )
			
			lSomething := .f.
			
			AFill(aNum, 0)
			
			kEval := Eval(bKey)
			
			do while ( nNext-- != 0 .AND. Eval(uCobWhile) .AND. kEval = Eval(bKey) )
				if ( Eval(uCobFor) )
					if ( !lSomething )
						//	CollectForced()
						lRetCode := VODBTransRec(siTo, fldNames)
						lSomething := .t.
					endif
					
					for i := 1 to n
						aNum[i] := aNum[i] + FIELDGET(aFldNum[i])
					next
					
				endif
				
				DBSKIP(1, .f.)
				
			enddo
			
			if ( lSomething )
				VODBSetSelect(int(siTo))
				
				for i := 1 to n
					FIELDPUT(aFldNum[i], aNum[i])
				next
				
				VODBSetSelect(int(siFrom))
			endif
			
		enddo
		
		_freeFieldNames(fldNames)
		
		
		if (siTo > 0)
			VODBSetSelect(int(siTo))
			VODBCloseArea()
		endif
		
		VODBSetSelect(int(siFrom))
		
		recover using oError
		if fldNames != null_ptr
			_freeFieldNames(fldNames)
		endif
		
		oError:FuncSym := #DBTOTAL
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
function DbUpdate(cAlias, uCobKey, lRand, bReplace) as logic clipper
	
	local siTo, siFrom  as dword
	local kEval         as usual
	local oError        as usual
	local uErrBlock     as usual
	local lRetCode      as logic
	
	
	if (lRand == NIL)
		lRand := .f.
	endif
	
	uErrBlock := ErrorBlock( {|o| _Break(o) } )
	
	lRetCode := .t.
	
	begin sequence
		
		
		if !Used()
			break DBCMDError()
		endif
		
		
		DbGotop()
		
		siTo := VODBGetSelect()
		
		
		siFrom := SELECT(cAlias)
		DbGotop()
		
		do while !EOF()
			
			kEval := Eval(uCobKey)
			
			VODBSetSelect(int(siTo))
			
			if lRand
				
				DbSeek(kEval)
				
				if FOUND()
					Eval(bReplace)
				endif
				
			else
				
				do while ( Eval(uCobKey) < kEval .AND. !EOF() )
					DBSKIP(1)
				enddo
				
				if ( Eval(uCobKey) == kEval .AND. !EOF() )
					Eval(bReplace)
				endif
				
			endif
			
			VODBSetSelect(int(siFrom))
			
			DBSKIP(1)
			
		enddo
		
		recover using oError
		oError:FuncSym := #DBUPDATE
		Eval( uErrBlock, oError)
		lRetCode := .f.
	end sequence
	
	
	VODBSetSelect(int(siTo))
	
	ErrorBlock(uErrBlock)
	
	
	return (lRetCode)



/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
function __TargetFields  (cAlias as string, aNames as array, ppJoinList as ptr) as array pascal export local
	
	local aNew      as array
	local cName     as string
	local aStruct   as array
	local adbStruct as array
	local n, i      as dword
	local siPos     as dword
	local siSelect  as dword
	local nBytes    as dword
	local aFldList  as array
	
	//  UH 02/03/1997
	local pJoinList as _JOINLIST
	
	adbStruct := DbStruct()
	aStruct   := {}
	aFldList := {}
	
	if ( Empty(aNames) )
		
		aNames    := {}
		n         := FCount()
		siSelect   := VODBGetSelect()
		
		for i := 1 to n
			cName := adbStruct[i, DBS_NAME]
			
			AAdd(aFldList, {siSelect, FieldPos(cName)})
			AAdd(aStruct, aDbStruct[i])
			AAdd(aNames, cName)
		next
		
	else
		
		n := Len(aNames)
		
		aNew := {}
		
		for i := 1 to n
			AAdd(aNew, AllTrim(Upper(aNames[i])))
		next
		
		aNames := aNew
		
		n := FCount()
		
		siSelect := VODBGetSelect()
		
		for i := 1 to n
			
			cName := adbStruct[i, DBS_NAME]
			
			if AScan(aNames, {|c| c == cName}) > 0
				
				AAdd(aFldList, {siSelect, FieldPos(cName)})
				AAdd(aStruct, aDbStruct[i])
				
			endif
			
		next
		
	endif
	
	siSelect := SELECT(cAlias)
	
	aDbStruct := DbStruct()
	
	n := Len(aNames)
	
	
	/* Herntz re-wrote the below code. Join seems to work just fine now. */
	for i := 1 to n
		if "->" $ aNames[i]
			cName := SubStr(aNames[i], At(">", aNames[i]) + 1)
		else
			cName :=  aNames[i]
		endif
		
		siPos := AScan(aDbStruct, {|a| a[DBS_NAME] == cName})
		if siPos > 0 .AND. (AScan( aStruct, {|c|c[DBS_NAME]== cName }) == 0)
			AAdd(aFldList, {siSelect, FieldPos(cName)})
			AAdd(aStruct, aDbStruct[siPos])
		endif
	next
	
	n := ALen(aStruct)
	
	//RvdH 050616 FdW reported that the line below used the wrong SIZEOF
	//nBytes      := n * (_sizeof(_JOINFIELD)) +  2 * (_sizeof(WORD))
	nBytes      := n * _sizeof(_JOINFIELD) + _sizeof(_JOINLIST)		// This allocates 1 element too much, but that does not hurt...
	pJoinList   := MemAlloc(nBytes)
	
	MemClear(pJoinList, nBytes)
	
	pJoinList.uiCount    := n
	
	for i := 1 to n
		pJoinList.jfldInfo[i].uiSrcSel := aFldList[i,1]
		pJoinList.jfldInfo[i].uiSrcPos := aFldList[i,2] - 1
	next
	
	ptr(ppJoinList) := pJoinList
	
	return aStruct



/*
function _allocFieldNames(aStru as Array)           as XSharp.RDD._FIELDNAMES  pascal
	
	local n,i       as dword
	local fldNames  as _FIELDNAMES
	local pszName   as PSZ
	//  UH 11/12/1997
	local cName     as string
	local nSize     as dword
	n := ALen(aStru)
	
	fldNames := MemAlloc( (_sizeof(dword)) + (n * _sizeof(PSZ)) )
	
	fldNames.uiFieldCount := n
	
	~"RANGECHECK-"
	for i := 1 to n
		//  UH 11/12/1997
		//  pszName := AsPsz(Upper(aStru[i,1]))
		//  fldNames.lpbName[i] := PTR(_CAST, pszName)
		cName := Upper(aStru[i, 1])
		nSize := SLen(cName) + 1
		pszName := MemAlloc(nSize)
		if pszName != null_psz
			MemCopy(pszName, ptr(_cast, cName), nSize)
		endif
		fldNames.lpbName[i] := pszName
	next
	
	return fldNames



function _freeFieldNames(fldNames  as XSharp.RDD._FIELDNAMES) as void pascal
	//
	//  UH 11/12/1997
	//
	local n,i       as dword
	~"RANGECHECK-"
	n := fldNames.uiFieldCount
	
	for i := 1 to n
		if fldNames.lpbName[i] != null_ptr
			MemFree(fldNames.lpbName[i])
		endif
	next
	
	MemFree(fldNames)
	
	return

*/

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
static function WithoutAlias(cName)                         as string pascal
	
	cName   := SubStr(cName, At(">", cName) + 1 )
	cName   := Trim(Upper(cName))
	
	return cName


#endif