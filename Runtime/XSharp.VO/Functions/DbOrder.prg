//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp.Rdd.Support
USING XSharp.Rdd
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DBClearOrderCondition()  AS LOGIC
	RETURN OrdCondSet("", NIL, .F., NIL, NIL, 0, 0, 0, 0, .F., .F.)
	


/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbReindex() AS LOGIC
	RETURN ORDLISTREBUILD()
	


/// <summary>Move to the record having the specified key value in the controlling order.
/// </summary>
FUNCTION DbSeek(xValue, lSoft, lLast) AS LOGIC CLIPPER
	LOCAL dbsci     AS DBSCOPEINFO
	LOCAL lRet      AS LOGIC
	
	DEFAULT(REF lSoft, SetSoftSeek())
	
	dbsci := DbScopeInfo{}
	IF !IsNil(lLast)
		dbsci:Last := lLast
	ENDIF
	VODBSetScope( dbsci)
	DEFAULT(REF xValue, "")
    IF lRet := _DbCallWithError("DbSeek", VODBSeek(xValue, lSoft))
	    lRet := VODBFound()
	    VODBSetScope(dbsci)
    ENDIF
	RETURN lRet
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbSetFound(lFnd AS LOGIC) AS LOGIC 
	RETURN VODBSetFound(lFnd)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbSetOrderCondition(  cFor,       ;
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
		lNoOptimize     ) AS LOGIC CLIPPER
	
	
	RETURN OrdCondSet( cFor, uCobFor, lAll, uCobWhile, uCobEval, ;
		nStep, nStart, nNext, nRecno, lRest,      ;
		lDescending, lAdditive, lCurrent, lCustom, lNoOptimize)

	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbSetIndex(cIndex, uOrder) AS LOGIC CLIPPER
	
	IF IsNil(cIndex)
		RETURN ORDLISTCLEAR()
	ENDIF
	RETURN ORDLISTADD(cIndex, uOrder)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbSetOrder(uOrder, cBagName) AS LOGIC CLIPPER
	LOCAL cOrder  := "" AS STRING
	DEFAULT( REF cBagName, "")
	RETURN VODBOrdSetFocus(cBagName, uOrder, REF cOrder)
	

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION IndexCount() AS DWORD 
	LOCAL nRet      AS DWORD
	IF Used()
		nRet := DbOrderInfo(DBOI_ORDERCOUNT)
    ELSE
        nRet := 0
	ENDIF
	
	RETURN nRet
	

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION IndexExt       () AS STRING STRICT
	RETURN VODBOrdBagExt()
	
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION IndexKey(uOrder) AS STRING CLIPPER
	
	LOCAL uRetVal   AS USUAL
	
	IF IsNil(uOrder)
		uOrder := 0
	ENDIF
	
	uRetVal := DbOrderInfo(DBOI_EXPRESSION, "", uOrder)
	
	IF IsNil(uRetVal)
		uRetVal := ""
	ENDIF
	
	RETURN uRetVal
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION IndexOrd() AS INT STRICT
	
	LOCAL uRetVal := NIL AS USUAL
	
	uRetVal := DbOrderInfo(DBOI_NUMBER, "", NIL)
	
	DEFAULT( REF uRetVal, 0)
	
	RETURN uRetVal
	
	
	
	
/// <summary>Relate a specified work area to the current work area.</summary>
FUNCTION OrdSetRelation(cAlias, bKey, cKey) AS USUAL CLIPPER
	
	DbSetRelation(cAlias, bKey, cKey)
	
	(cAlias)->(OrdScope(0, bKey))
	
	(cAlias)->(OrdScope(1, bKey))
	
	RETURN NIL
	
	
/// <summary>Set or clear the boundaries for scoping key values in the controlling order.</summary>
FUNCTION OrdScope(nScope, xVal) AS USUAL CLIPPER
	LOCAL n     AS DWORD
	
	nScope := Db.OrdScopeNum(nScope)
	
	n := DBOI_SCOPETOP
	
	IF PCount() > 1 .AND. IsNil(xVal)
		n := DBOI_SCOPETOPCLEAR
	ENDIF
	
	RETURN DbOrderInfo(n + nScope,,,xVal)
	
	
	
/// <summary>Move the record pointer to the next or previous unique key in the controlling order.</summary>	
FUNCTION OrdSkipUnique(uCount) AS USUAL CLIPPER
    LOCAL nCount := uCount AS OBJECT
	RETURN VODBOrderInfo ( DBOI_SKIPUNIQUE, "", NIL, REF nCount )
	
	
	
/// <summary>Return the status of the unique flag for a given order.</summary>	
FUNCTION OrdIsUnique   (xOrder, cOrderBag) AS USUAL CLIPPER
	RETURN DbOrderInfo(DBOI_UNIQUE, cOrderBag, xOrder)
	
	


// DbOrder.prg - Order related functions
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdBagExt() AS STRING STRICT
	RETURN VODBOrdBagExt()
	
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdBagName(uOrder) AS STRING CLIPPER
	RETURN DbOrderInfo(DBOI_BAGNAME, "", uOrder)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
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
		lNoOptimize     ) AS LOGIC CLIPPER
	
	
	LOCAL dbOrdCondInfo     AS DbOrderCondInfo
	
	dbOrdCondInfo := DbOrderCondInfo{}
	IF !IsNil(cFor)
        dbOrdCondInfo:ForExpression := cFor
	ENDIF
	
    dbOrdCondInfo:ForBlock := VoDb.ValidBlock(uCobFor)
    dbOrdCondInfo:WhileBlock := VoDb.ValidBlock(uCobWhile)
	dbOrdCondInfo:EvalBlock := VoDb.ValidBlock(uCobEval)
	
	IF IsNumeric(nStep)
		dbOrdCondInfo:StepSize := nStep
	ENDIF
	
	IF IsNumeric(nStart)
		dbOrdCondInfo:StartRecNo := nStart
	ENDIF
	
	IF IsNumeric(nNext)
		dbOrdCondInfo:NextCount := nNext
	ENDIF
	
	IF IsNumeric(nRecno)
		dbOrdCondInfo:RecNo := nRecno
	ENDIF
	
	IF IsLogic(lRest)
		dbOrdCondInfo:Rest := lRest
	ENDIF
	
	IF IsLogic(lDescending)
		dbOrdCondInfo:Descending := lDescending
	ENDIF
	
	
	IF IsLogic(lAll)
		dbOrdCondInfo:All := lAll
	ENDIF
	
	
	IF IsLogic(lAdditive)
		dbOrdCondInfo:Additive := lAdditive
	ENDIF
	
	IF IsLogic(lCustom)
		dbOrdCondInfo:Custom := lCustom
	ENDIF
	
	IF IsLogic(lCurrent)
		dbOrdCondInfo:UseCurrent := lCurrent
	ENDIF
	
	
	IF !IsNil(lNoOptimize)
		dbOrdCondInfo:NoOptimize := lNoOptimize
	ENDIF
	
	RETURN _DbCallWithError("OrdCondSet", VODBOrdCondSet( dbOrdCondInfo ))
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdCreate(cName, cOrder, cExpr, cobExpr, lUnique) AS LOGIC CLIPPER
	IF IsNil(lUnique)
		lUnique := SetUnique()
	ENDIF
	
	IF IsNil(cName)
		IF IsNil(cOrder)
            RddError.PostArgumentError("OrdCreate", EDB_CREATEINDEX, nameof(cName), 1, {cName})
			DoError("OrdCreate")
		ELSE
			cName := ""
		ENDIF
	ENDIF
	
	IF IsNil(cExpr)
		cExpr := ""
		IF IsNil(cobExpr)
            RddError.PostArgumentError("OrdCreate", EDB_EXPRESSION, nameof(cExpr), 3, {cExpr})
  			DoError("OrdCreate")
		ENDIF
	ELSE
		IF IsNil(cobExpr)
			cobExpr := &( "{||" + cExpr + "}" )
		ENDIF
	ENDIF
	
    RETURN _DbCallWithError("OrdCreate", VODBOrdCreate(cName, cOrder, cExpr, cobExpr, lUnique, NULL))
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdDescend    (xOrder, cOrdBag, lDescend) AS LOGIC CLIPPER
	
	IF !IsLogic( lDescend )
		lDescend := NIL
	ENDIF
	
	RETURN DbOrderInfo(DBOI_ISDESC, cOrdBag, xOrder, lDescend)
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdDestroy(uOrder, cOrdBag) AS LOGIC CLIPPER
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	IF !IsString(uOrder)
        RddError.PostArgumentError("OrdDestroy", EDB_ORDDESTROY, nameof(uOrder), 1, {uOrder})
        RETURN FALSE
    ENDIF
    RETURN _DbCallWithError("OrdDestroy", VODBOrdDestroy(cOrdBag, uOrder))
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdFor(uOrder, cOrdBag, cFor) AS LOGIC CLIPPER
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	IF !IsString(cFor)
		cFor := NIL
	ENDIF
	
	RETURN DbOrderInfo(DBOI_CONDITION, cOrdBag, uOrder, cFor)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdKey(uOrder, cOrdBag) AS USUAL CLIPPER
	LOCAL xKey  := NULL    AS OBJECT
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	IF !VODBOrderInfo(DBOI_EXPRESSION, cOrdBag, uOrder, REF xKey)
		xKey := ""
	ENDIF
	RETURN xKey
    
/// <summary>Add a key to a custom built order.</summary>	
FUNCTION OrdKeyAdd(xOrder, cOrdBag, xVal) AS USUAL CLIPPER
	RETURN DbOrderInfo(DBOI_KEYADD, cOrdBag, xOrder, xVal)
	

/// <summary>Delete a key from a custom built order.</summary>	
FUNCTION OrdKeyDel(xOrder, cOrdBag, xVal) AS USUAL CLIPPER
	RETURN DbOrderInfo( DBOI_KEYDELETE, cOrdBag, xOrder, NIL)
	
/// <summary>Move to a record specified by its logical record number in the controlling order.</summary>	
FUNCTION OrdKeyGoto    (nKeyNo) AS LOGIC CLIPPER
	LOCAL lRetCode  AS LOGIC
	IF IsNumeric(nKeyNo)
		DbGotop()
		DBSKIP(nKeyno - 1)
		lRetCode := TRUE
    ELSE
        lRetCode := FALSE
	ENDIF
	RETURN lRetCode
	
	
/// <summary>Return the number of keys in an order.</summary>	
FUNCTION OrdKeyCount   (xOrder, cOrdBag) AS USUAL CLIPPER
	RETURN DbOrderInfo(DBOI_KEYCOUNT, cOrdBag, xOrder, NIL)
	

/// <summary>Get the logical record number of the current record.</summary>
FUNCTION OrdKeyNo      (xOrder, cOrdBag) 	AS USUAL CLIPPER
	RETURN DbOrderInfo( DBOI_POSITION, cOrdBag, xOrder, NIL)
	
	
/// <summary>Get the key value of the current record from the controlling order.</summary>	
FUNCTION OrdKeyVal     () AS USUAL STRICT
	RETURN DbOrderInfo( DBOI_KEYVAL, NIL ,NIL, NIL)
	
	
	
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdListAdd(cOrdBag, uOrder) AS LOGIC CLIPPER
	RETURN _DbCallWithError("OrdListAdd", VODBOrdListAdd(cOrdBag, uOrder))



/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdListClear(cOrdBag, uOrder)  AS LOGIC CLIPPER
	IF IsNil(cOrdBag)
		cOrdBag := ""
    ENDIF
    RETURN _DbCallWithError("OrdListClear", VODBOrdListClear(cOrdBag, uOrder))
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION __OrdListClear()  AS LOGIC STRICT
	LOCAL lRet      AS LOGIC
	LOCAL lOpen     AS LOGIC
	LOCAL cDBF      AS STRING
	LOCAL cAlias    AS STRING
	LOCAL lShare    AS LOGIC
	LOCAL aRDD      AS ARRAY
	LOCAL rdds      AS XSharp.RDD.RddList
	LOCAL i         AS DWORD
	
	IF Used()
		cDBF   := DbInfo(DBI_FULLPATH)
		rdds   := DbInfo(DBI_RDD_LIST)
		
		aRdd := {}
		FOR i := 1 TO rdds:uiRDDCount
			AAdd(aRdd, rdds:atomRddName[i] )
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
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdListRebuild ()  AS LOGIC STRICT
    RETURN _DbCallWithError("OrdListRebuild", VODBOrdListRebuild())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdName(uOrder, cOrdBag) AS LOGIC CLIPPER
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	RETURN DbOrderInfo(DBOI_NAME, cOrdBag, uOrder)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdNumber(uOrder, cOrdBag) AS USUAL CLIPPER
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	RETURN DbOrderInfo(DBOI_NUMBER, cOrdBag, uOrder)
	
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdSetFocus(uOrder, cOrdBag) AS USUAL CLIPPER
	LOCAL cOrder := ""   AS   STRING
	DEFAULT( REF cOrdBag, "")
	VODBOrdSetFocus(cOrdBag, uOrder, REF cOrder)
	RETURN cOrder
	
