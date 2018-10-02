//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

// DbBulk.prg: Bulk operations on Workareas

/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
USING XSharp.RDD.Support


/// <exclude/>
FUNCTION __DBAvg(siValue AS LONG) AS LONG
	
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
FUNCTION __UniqueAlias   (cDbfName AS STRING)            AS STRING       
	
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


FUNCTION __DBFLEDIT(aStruct AS ARRAY, aFields AS ARRAY, aList AS ARRAY ) AS ARRAY
    RETURN VoDb.FieldList(aStruct, aFields, aList)

/// <summary>Import records from a database file.</summary>
FUNCTION DbApp(cFile, aFields, uCobFor, uCobWhile,nNext, nRec, lRest,cDriver, aHidden, aStruct)     AS LOGIC CLIPPER
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL n, i          AS DWORD
	LOCAL aMatch		AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL lAnsi         AS LOGIC
	
	lAnsi  := SetAnsi()
    lRetCode := FALSE
    siFrom := 0
	TRY	
		siTo := VODBGetSelect()
		IF !Used()
			THROW VoDb.DBCMDError(__FUNCTION__)
		ENDIF
		IF IsNil(aStruct)
            aStruct := DbStruct()
        ENDIF
		IF Empty( aStruct := VoDb.FieldList(aStruct, aFields, NULL_ARRAY) )
			THROW VoDb.ParamError(__FUNCTION__, ARRAY, 2)
		ENDIF
		
		DbUseArea(TRUE, cDriver, cFile, __UniqueAlias(cFile), TRUE, TRUE,/*aStru*/,/*cDelim*/, aHidden)
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
		
		IF !Empty(aStruct := VoDb.FieldList(aStruct, aFields, aMatch))
			lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		ENDIF
		
		IF (siFrom > 0)
			VODBCloseArea()
		ENDIF
		
		VODBSetSelect(INT(siTo))
		
		
	CATCH e AS Error
		IF  siFrom > 0
			VODBSetSelect(INT(siFrom))
			VODBCloseArea()
		ENDIF
		
		e:FuncSym := __FUNCTION__
		THROW Error{e}
    FINALLY
        SetAnsi( lAnsi )
	END TRY
	
	RETURN (lRetCode)


/// <summary>Import records from a delimited text file.</summary>
FUNCTION DbAppDelim(cFile, cDelim, aFields, uCobFor, uCobWhile, nNext,nRec, lRest,aStruct)AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL siPos         AS DWORD
	LOCAL lRetCode      AS LOGIC
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	
	lAnsi  := SetAnsi()
	
	TRY
		
		siTo := VODBGetSelect()
        IF IsNil(aStruct)
            aStruct := DbStruct()
        ENDIF
		IF (Empty( aStruct := VoDb.FieldList(aStruct, aFields, NULL_ARRAY) ))
			THROW VoDb.ParamError(__FUNCTION__, ARRAY, 3)
		ENDIF
		
		IF Empty(cFile)
			THROW VoDb.ParamError(__FUNCTION__, STRING, 1)
		ELSE
			IF Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			ENDIF
		ENDIF
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DbCreate(cFile, aStruct, "DELIM", .T., __UniqueAlias(cFile), cDelim, .T.)
		
		siFrom := VODBGetSelect()
		
		IF ( !lAnsi .AND. lDbfAnsi)
			SetAnsi(.T.)
		ENDIF
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		VODBCloseArea()
		
		VODBSetSelect(INT(siTo))
		
	CATCH e AS Error
		e:FuncSym := __FUNCTION__
		THROW Error{e}
    FINALLY
        SetAnsi( lAnsi )    
	END TRY
	RETURN (lRetCode)



/// <summary>Import records from an SDF file.</summary>
FUNCTION DbAppSdf(cFile, aFields, uCobFor,;
	uCobWhile, nNext, nRec, ;
	lRest , aStruct                  )      AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL siPos         AS DWORD
	LOCAL lRetCode      AS LOGIC
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	
	lAnsi  := SetAnsi()
	
	TRY		
		siTo := VODBGetSelect()
        IF IsNil(aStruct)
            aStruct := DbStruct()
        ENDIF
		
		IF (Empty( aStruct := VoDb.FieldList(aStruct, aFields, NULL_ARRAY) ))
			THROW VoDb.ParamError(__FUNCTION__, ARRAY, 2)
		ENDIF
		
		IF Empty(cFile)
			THROW VoDb.ParamError(__FUNCTION__, STRING, 1)
		ELSE
			IF Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			ENDIF
		ENDIF
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DbCreate(cFile, aStruct, "SDF", .T., __UniqueAlias(cFile), ,.T.)
		
		siFrom := VODBGetSelect()
		
		IF ( !lAnsi .AND. lDbfAnsi )
			SetAnsi(.T.)
		ENDIF
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		VODBCloseArea()
		VODBSetSelect(INT(siTo))
		
	CATCH e AS Error
		e:FuncSym := __FUNCTION__
		THROW e
    FINALLY
        SetAnsi( lAnsi )    
	END TRY
	RETURN (lRetCode)


/// <summary>Export records to a new database file.</summary>
FUNCTION DbCopy(cFile, aFields, uCobFor, uCobWhile, nNext, nRec, lRest, cDriver, aHidden, aStruct    )     AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL lRetCode      AS LOGIC
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	
	lAnsi    := SetAnsi()
	
	siFrom   := VODBGetSelect()
    siTo    := 0    
	lRetCode := .F.
	
	TRY
		
		IF !Used()
			THROW VoDb.DBCMDError(__FUNCTION__)
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
            IF IsNil(aStruct)
                aStruct := DbStruct()
            ENDIF
            
			IF ( Empty(aStruct := VoDb.FieldList(aStruct, aFields, NULL_ARRAY)) )
				THROW VoDb.ParamError(__FUNCTION__, ARRAY, 2)
			ENDIF
			
			DbCreate( cFile, aStruct, cDriver,, __UniqueAlias(cFile),,,aHidden)
			
			IF ( !lAnsi ) .AND. ( DbInfo(DBI_ISANSI) )
				SetAnsi(.T.)
			ENDIF
			
			DBUSEAREA(.T., cDriver, cFile, __UniqueAlias(cFile),,,,,aHidden)
			
			VODBSelect(siFrom, REF siTo)
			
			lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
			
			IF (siTo > 0)
				VODBSetSelect(INT(siTo))
				VODBCloseArea()
			ENDIF
			
			VODBSetSelect(INT(siFrom))
		ENDIF
		
    CATCH e AS Error
		SetAnsi(lAnsi)
		e:FuncSym := __FUNCTION__
		THROW e
    FINALLY
        SetAnsi( lAnsi )    
	END TRY
	
	RETURN (lRetCode)


#define BUFF_SIZE 0x00008000
#define FO_CREATE 0x00001000

FUNCTION DBFileCopy( hfFrom AS IntPtr, cFile AS STRING, cFullPath AS STRING) AS LOGIC 
	
	LOCAL lRetCode  AS LOGIC
	LOCAL ptrBuff   AS BYTE[]
	LOCAL hfTo      AS PTR
	LOCAL oError    AS Error
	LOCAL n         AS DWORD
	LOCAL dwPos     AS LONG
	
	
	IF At(".", cFile) == 0
		cFile := cFile + Right(cFullPath, 4)
	ENDIF
	
	hfTo := FCreate( cFile)
	
	IF hfTo == (IntPtr) F_ERROR
		
		oError := Error{1}
		oError:SubSystem                := "DBCMD"
		oError:GenCode                  := EG_OPEN
		oError:OsCode                   := DosError()
		oError:FuncSym                  :=__FUNCTION__
		oError:FileName                 := FPathName()
		
		THROW oError
		
		lRetCode := .F.
		
	ELSE
		
		dwPos := FSeek3(hfFrom, 0, FS_RELATIVE )
		
		
		FSeek3(hfFrom, 0, FS_SET )
		n       := BUFF_SIZE
		ptrBuff := BYTE[]{n}
		DO WHILE n == BUFF_SIZE
			n := FRead3(hfFrom, ptrBuff, BUFF_SIZE)
			FWrite3(hfTo, ptrBuff, n)
		ENDDO
		
		FClose(hfTo)
		lRetCode := .T.
		FSeek3(hfFrom, dwPos, FS_SET )
	ENDIF
	
	RETURN lRetCode




/// <summary>Export records to a new delimited text file.</summary>

FUNCTION DbCopyDelim (cFile, cDelim, aFields, uCobFor, uCobWhile, nNext,nRec, lRest, aStruct)   AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL siPos         AS DWORD
	LOCAL lRetCode      AS LOGIC
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	lAnsi  := SetAnsi()
	
	siFrom := VODBGetSelect()
	siTo   := 0
	TRY
		
		IF !Used()
			THROW VoDb.DBCMDError(__FUNCTION__)
		ENDIF
        IF IsNil(aStruct)
            aStruct := DbStruct()
        ENDIF
  		
		IF Empty(aStruct := VoDb.FieldList(aStruct, aFields, NULL_ARRAY))
			THROW VoDb.ParamError(__FUNCTION__, ARRAY, 3)
		ENDIF
		
		IF Empty(cFile)
			THROW VoDb.ParamError(__FUNCTION__, STRING, 1)
		ELSE
			IF Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			ENDIF
		ENDIF
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DbCreate(cFile, aStruct, "DELIM", .T., __UniqueAlias(cFile), cDelim)
		
		IF ( !lAnsi .AND. lDbfAnsi)
			SetAnsi(.T.)
		ENDIF
		
		VODBSelect(siFrom, REF siTo)
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		VODBSetSelect(INT(siTo))
		VODBCloseArea()
		
	CATCH e AS Error
		e:FuncSym := __FUNCTION__
		THROW e
    FINALLY
        SetAnsi( lAnsi )
		VODBSetSelect(INT(siFrom))
	END TRY
	
	RETURN (lRetCode)


/// <summary>Export records to a new SDF file.</summary>
FUNCTION DbCopySDF(cFile, aFields, uCobFor, uCobWhile, nNext, nRec, lRest, aStruct )   AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL siPos         AS DWORD
	LOCAL lRetCode      AS LOGIC
	LOCAL cAlias        AS STRING
	LOCAL lAnsi         AS LOGIC
	LOCAL lDbfAnsi      AS LOGIC
	
	lAnsi  := SetAnsi()
	siFrom := VODBGetSelect()
    siTo   := 0
	TRY
		
		IF !Used()
			THROW VoDb.DBCMDError(__FUNCTION__)
		ENDIF
        IF IsNil(aStruct)
            aStruct := DbStruct()
        ENDIF
		
		IF Empty(aStruct := VoDb.FieldList(aStruct, aFields, NULL_ARRAY))
			THROW VoDb.ParamError(__FUNCTION__, ARRAY, 2)
		ENDIF
		
		IF Empty(cFile)
			THROW VoDb.ParamError(__FUNCTION__, STRING, 1)
		ELSE
			IF Empty(siPos := At(".", cFile ) )
				cFile := cFile + ".TXT"
			ENDIF
		ENDIF
		
		cAlias := __UniqueAlias(cFile)
		
		
		lDbfAnsi := DbInfo(DBI_ISANSI)
		
		DbCreate(cFile, aStruct, "SDF", .T., cAlias)
		
		
		IF ( !lAnsi .AND. lDbfAnsi)
			SetAnsi(.T.)
		ENDIF
		
		VODBSelect(siFrom, REF siTo)
		
		lRetCode := DbTrans(siTo, aStruct, uCobFor, uCobWhile, nNext, nRec, lRest)
		
		VODBSetSelect(INT(siTo))
		VODBCloseArea()
		
	CATCH e AS Error
		e:FuncSym := __FUNCTION__
		THROW e
    FINALLY
    	SetAnsi( lAnsi )
		VODBSetSelect(INT(siFrom))
    
	END TRY
	
	
	
	RETURN (lRetCode)



/// <summary>Create a new database file by merging records/fields from two work areas.</summary>
FUNCTION DbJoin(cAlias, cFile, aFields, uCobFor) AS LOGIC CLIPPER
	LOCAL siFrom1       AS DWORD
	LOCAL siFrom2       AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL lRetCode      AS LOGIC
	
	LOCAL pJoinList     AS _JOINLIST
	
	IF IsNil(uCobFor)
		uCobFor := {|| .T.}
	ENDIF
	siTo   := 0
    siFrom1 := 0
	TRY
		
		siFrom1 := VODBGetSelect()
		
		siFrom2 := SELECT(cAlias)
		
		
		IF siFrom2 = 0
			THROW VoDb.ParamError(__FUNCTION__, STRING, 1)
		ENDIF
		
		VODBSetSelect(INT(siFrom1))

        
		IF Empty( aStruct := VoDb.TargetFields(cAlias, aFields, OUT pJoinList) )
			VAR oError := VoDb.DbCmdError(__FUNCTION__)
			oError:SubCode      := EDB_NOFIELDS
			oError:CanDefault    := .F.
			oError:CanRetry      := .F.
			oError:CanSubstitute := .F.
			THROW oError
		ENDIF
		DbCreate( cFile, aStruct,"" , .T., "" )
		VODBSelect(siFrom1, REF siTo)
		
		pJoinList:uiDestSel := siTo
		
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
		
	CATCH e AS Error
		e:FuncSym := __FUNCTION__
		THROW e
    FINALLY
	    IF siTo > 0
		    VODBSetSelect(INT(siTo))
		    VODBCloseArea()
	    ENDIF
	
	    VODBSetSelect(INT(siFrom1))
        
	END TRY
	
	RETURN (lRetCode)

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbJoinAppend(nSelect AS DWORD, list AS _JoinList)   AS LOGIC        
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VODBJoinAppend(nSelect, list))

/// <summary>Copy records to a database file in sorted order.
/// </summary>
FUNCTION DbSort(	cFile, aFields, uCobFor, uCobWhile, nNext, nRec, lRest )   AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL lRetCode      AS LOGIC
	LOCAL fnFieldNames  AS _FieldNames
	LOCAL fnSortNames   AS _FieldNames
	LOCAL cRdd 			AS STRING
	
	
	siFrom := VODBGetSelect()
	siTo   := 0
	DEFAULT(REF lRest, .F.)
	
	TRY
		
		IF !Used()
			THROW VoDb.DBCMDError(__FUNCTION__)
		ENDIF
		
		aStruct := DbStruct()
		
		//	UH 09/23/1997
		cRdd := RDDNAME()
		
		fnFieldNames := VoDb.allocFieldNames(aStruct)
		
		IF Empty(AFields)
			THROW VoDb.ParamError(__FUNCTION__, ARRAY, 2)
		ENDIF
		
		fnSortNames := VoDb.AllocFieldNames(AFields)
		
		DbCreate(cFile, aStruct, cRdd, .T.)
		VODBSelect(siFrom, REF siTo)
		lRetCode := VODBSort(siTo, fnFieldNames, uCobFor, uCobWhile, nNext, nRec, lRest, fnSortNames)
		
		IF !lRetCode
			THROW Error{RuntimeState.LastRDDError}
		ENDIF
		
		IF (siTo > 0)
			VODBSetSelect(INT(siTo))
			VODBCloseArea()
		ENDIF
		
		VODBSetSelect(INT(siFrom))
		
		
	CATCH e AS Error
		e:FuncSym := __FUNCTION__
		THROW e
	END TRY
	
	RETURN lRetCode

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbTrans(nTo, aStru, uCobFor, uCobWhile, nNext, nRecno, lRest) AS LOGIC CLIPPER
	
	LOCAL fldNames  AS _FieldNames
	
	IF !IsNil(uCobWhile)
		lRest := .T.
	ENDIF
	
	IF IsNil(lRest)
		lRest := .F.
	ENDIF
	
	fldNames := VoDb.AllocFieldNames(aStru)
	
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VODBTrans(nTo, fldNames, uCobFor, uCobWhile, nNext, nRecno, lRest))



/// <summary>
/// </summary>
/// <param name="uSelect"></param>
/// <param name="symField"></param>
/// <returns>
/// </returns>
FUNCTION DbTotal(cFile, bKey, aFields,  uCobFor, uCobWhile, nNext, nRec, lRest, xDriver ) 	AS LOGIC CLIPPER
	
	LOCAL siFrom        AS DWORD
	LOCAL siTo          AS DWORD
	LOCAL i, n          AS DWORD
	LOCAL aStruct       AS ARRAY
	LOCAL aFldNum       AS ARRAY
	LOCAL aNum          AS ARRAY
	LOCAL lSomething    AS LOGIC
	LOCAL kEval         AS USUAL
	LOCAL lRetCode  := FALSE   AS LOGIC
	LOCAL fldNames      AS _FieldNames
	
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
	siTo   := 0
	
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
	siTo   := 0
	FOR i := 1 TO n
		AAdd(aFldNum, FieldPos( AllTrim(AFields[i]) ) )
	NEXT
	
	aNum  := ArrayNew(n)
	
	TRY
		
		IF !Used()
			THROW VoDb.DBCMDError(__FUNCTION__)
		ENDIF
		
		aStruct := DbStruct()
		
		siFrom := VODBGetSelect()
		
		aStruct := {}
		
		n := FCount()
		
		FOR i := 1 TO n
			IF DbFieldInfo(DBS_TYPE, i) != "M"
				AAdd(aStruct, { FieldName(i) , DbFieldInfo(DBS_TYPE, i), DbFieldInfo(DBS_LEN, i),  DbFieldInfo(DBS_DEC, i)  })
			ENDIF
		NEXT
		
		IF Empty(aStruct) 
			THROW VoDb.ParamError(__FUNCTION__, ARRAY, 3)
		ENDIF
		
		fldNames := VoDb.allocFieldNames(aStruct)
		
		//	DbCreate( cFile, aStruct, "", .T.)
		IF IsNil(xDriver)
			xDriver := RddSetDefault()
		ENDIF
		DbCreate( cFile, aStruct, xDriver, .T.)
		
		VODBSelect(siFrom, REF siTo)
		
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
				
				DBSKIP(1)
				
			ENDDO
			
			IF ( lSomething )
				VODBSetSelect(INT(siTo))
				
				FOR i := 1 TO n
					FieldPut(aFldNum[i], aNum[i])
				NEXT
				
				VODBSetSelect(INT(siFrom))
			ENDIF
			
		ENDDO
		
		
		IF (siTo > 0)
			VODBSetSelect(INT(siTo))
			VODBCloseArea()
		ENDIF
		
		VODBSetSelect(INT(siFrom))
		
    CATCH e AS Error
		
		e:FuncSym := __FUNCTION__
		THROW e
	END TRY
	
	
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
	LOCAL lRetCode      AS LOGIC
	
	siTo := VODBGetSelect()
	IF (lRand == NIL)
		lRand := .F.
	ENDIF
	
	lRetCode := .T.
	
	TRY
		
		
		IF !Used()
			THROW VoDb.DBCMDError(__FUNCTION__)
		ENDIF
		
		
		DbGotop()
		
		
		
		
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
		
	CATCH e AS Error
		e:FuncSym := __FUNCTION__
		THROW e
    FINALLY
        VODBSetSelect(INT(siTo))
	END TRY
	
	RETURN (lRetCode)











