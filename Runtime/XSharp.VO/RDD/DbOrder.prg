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
    // Don't call CoreDb because of error handling
	RETURN OrdListRebuild()
	

/// <summary>Move to the record having the specified key value in the controlling order.
/// </summary>
FUNCTION DbSeek(xValue, lSoft, lLast) AS LOGIC CLIPPER
	LOCAL dbsci     AS DBSCOPEINFO
	LOCAL lRet      AS LOGIC
	
	DEFAULT(REF lSoft, SetSoftSeek())
	
	dbsci := DbScopeInfo{}
	IF IsNil(lLast)
		lLast := FALSE
	ENDIF
	VoDb.SetScope( dbsci)
	DEFAULT(REF xValue, "")
    IF lRet := _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Seek(xValue, lSoft, lLast))
	    lRet := VoDb.Found()
	    VoDb.SetScope(dbsci)
    ENDIF
	RETURN lRet
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbSetFound(lFnd AS LOGIC) AS LOGIC 
	RETURN VoDb.SetFound(lFnd)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION DbSetOrderCondition(  cFor, uCobFor, lAll, uCobWhile, uCobEval, nStep, nStart, nNext, nRecno, lRest, lDescending, lAdditive, lCurrent, lCustom, lNoOptimize) AS LOGIC CLIPPER
    // Don't call VoDb because of error handling
	RETURN OrdCondSet( cFor, uCobFor, lAll, uCobWhile, uCobEval, ;
		nStep, nStart, nNext, nRecno, lRest,      ;
		lDescending, lAdditive, lCurrent, lCustom, lNoOptimize)

	
/// <summary>Open an index file and add all its orders to the order list in a work area.
/// </summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION DbSetIndex(cIndex, uOrder) AS LOGIC CLIPPER
	// Don't call VoDb because of error handling
	IF IsNil(cIndex)
		RETURN OrdListClear()
	ENDIF
	RETURN OrdListAdd(cIndex, uOrder)
	
	
	
/// <summary>Set the controlling order for a work area.</summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION DbSetOrder(uOrder, cBagName) AS LOGIC CLIPPER
	LOCAL cOrder  := "" AS STRING
	DEFAULT( REF cBagName, "")
	RETURN VoDb.OrdSetFocus(cBagName, uOrder, REF cOrder)
	

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
	

/// <summary>Return the default index file extension for a work area as defined by its RDD.</summary>
/// <returns>A string indicating the default index file extension for a work area as defined by its RDD</returns>
FUNCTION IndexExt       () AS STRING STRICT
	RETURN VoDb.OrdBagExt()
	
/// <summary>Return the key expression of a specified order.</summary>
/// <returns>The key expression of the specified order.  If there is no corresponding order or if no database file is open, IndexKey() returns a NULL_STRING.</returns>
/// <param name='nPosition'>The position of the order in the order list of the work area.  A value of 0 specifies the controlling order, without regard to its actual position in the list.</param>
FUNCTION IndexKey(nPosition) AS STRING CLIPPER
	LOCAL uRetVal   AS USUAL
	IF IsNil(nPosition)
		nPosition := 0
	ENDIF
	uRetVal := DbOrderInfo(DBOI_EXPRESSION, "", nPosition)
	IF IsNil(uRetVal)
		uRetVal := ""
	ENDIF
	RETURN uRetVal
	
	
	
/// <summary>Return the position of the controlling order within the order list.</summary>
/// <returns>The position of the controlling order.  A value of 0 indicates either that no database file is open or that there is no
/// controlling order and records are being accessed in natural order.</returns>
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
	nScope := VoDb.OrdScopeNum(nScope)
	n := DBOI_SCOPETOP
	IF PCount() > 1 .AND. IsNil(xVal)
		n := DBOI_SCOPETOPCLEAR
	ENDIF
	RETURN DbOrderInfo(n + nScope,,,xVal)
	
/// <summary>Move the record pointer to the next or previous unique key in the controlling order.</summary>	
FUNCTION OrdSkipUnique(uCount) AS USUAL CLIPPER
	RETURN VoDb.OrderInfo ( DBOI_SKIPUNIQUE, "", NIL, uCount )
	
/// <summary>Return the status of the unique flag for a given order.</summary>	
FUNCTION OrdIsUnique   (xOrder, cOrderBag) AS USUAL CLIPPER
	RETURN VoDb.OrderInfo(DBOI_UNIQUE, cOrderBag, xOrder,NULL)
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdBagExt() AS STRING STRICT
	RETURN VoDb.OrdBagExt()
	
	

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdBagName(uOrder) AS STRING CLIPPER
	RETURN DbOrderInfo(DBOI_BAGNAME, "", uOrder)
	
	
	
/// <summary>Set the condition and scope for an order.</summary>
/// <remarks>OrdCondSet() is like VODBOrdCondSet() but untyped and the various parameters are passed individually.</remarks>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
/// <seealso cref='M:XSharp.CoreDb.OrdCondSet(XSharp.RDD.Support.DbOrderCondInfo)' />
FUNCTION OrdCondSet(cFor, uCobFor, lAll, uCobWhile, uCobEval, nStep, nStart,     ;
		nNext, nRecno,lRest,lDescending,lAdditive,lCurrent, lCustom, lNoOptimize     ) AS LOGIC CLIPPER
	
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
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdCondSet( dbOrdCondInfo ))
	
	
	
/// <summary>
/// </summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
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
	
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdCreate(cName, cOrder, cExpr, cobExpr, lUnique, NULL))
	
	
/// <summary>
/// </summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION OrdDescend(xOrder, cOrdBag, lDescend) AS LOGIC CLIPPER
	
	IF !IsLogic( lDescend )
		lDescend := NIL
	ENDIF
	
	RETURN VoDb.OrderInfo(DBOI_ISDESC, cOrdBag, xOrder, lDescend)
	
/// <summary>
/// </summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION OrdDestroy(uOrder, cOrdBag) AS LOGIC CLIPPER
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	IF !IsString(uOrder)
        RddError.PostArgumentError(__FUNCTION__, EDB_ORDDESTROY, nameof(uOrder), 1, {uOrder})
        RETURN FALSE
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdDestroy(cOrdBag, uOrder))
	
	
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
	RETURN VoDb.OrderInfo(DBOI_CONDITION, cOrdBag, uOrder, cFor)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdKey(uOrder, cOrdBag) AS USUAL CLIPPER
	LOCAL xKey  := NIL    AS USUAL
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	IF !VODBOrderInfo(DBOI_EXPRESSION, cOrdBag, uOrder, REF xKey)
		xKey := ""
	ENDIF
	RETURN xKey
    
/// <summary>Add a key to a custom built order.</summary>	
FUNCTION OrdKeyAdd(xOrder, cOrdBag, xVal) AS USUAL CLIPPER
	RETURN VoDb.OrderInfo(DBOI_KEYADD, cOrdBag, xOrder, xVal)
	

/// <summary>Delete a key from a custom built order.</summary>	
FUNCTION OrdKeyDel(xOrder, cOrdBag, xVal) AS USUAL CLIPPER
	RETURN VoDb.OrderInfo( DBOI_KEYDELETE, cOrdBag, xOrder, NIL)
	
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
FUNCTION OrdKeyCount(xOrder, cOrdBag) AS USUAL CLIPPER
	RETURN VoDb.OrderInfo(DBOI_KEYCOUNT, cOrdBag, xOrder, NIL)
	

/// <summary>Get the logical record number of the current record.</summary>
FUNCTION OrdKeyNo(xOrder, cOrdBag) 	AS USUAL CLIPPER
	RETURN VoDb.OrderInfo( DBOI_POSITION, cOrdBag, xOrder, NIL)
	
	
/// <summary>Get the key value of the current record from the controlling order.</summary>	
FUNCTION OrdKeyVal() AS USUAL STRICT
	RETURN VoDb.OrderInfo( DBOI_KEYVAL, NIL ,NIL, NIL)
	
	
	
/// <summary>
/// </summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION OrdListAdd(cOrdBag, uOrder) AS LOGIC CLIPPER
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdListAdd(cOrdBag, uOrder))



/// <summary>
/// </summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION OrdListClear(cOrdBag, uOrder)  AS LOGIC CLIPPER
	IF IsNil(cOrdBag)
		cOrdBag := ""
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdListClear(cOrdBag, uOrder))
	
	
/// <summary>
/// </summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION __OrdListClear()  AS LOGIC STRICT
	LOCAL lRet      AS LOGIC
	LOCAL lOpen     AS LOGIC
	LOCAL cDBF      AS STRING
	LOCAL cAlias    AS STRING
	LOCAL lShare    AS LOGIC
	LOCAL aRDD      AS ARRAY
	LOCAL rdds      AS _RddList
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
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION OrdListRebuild ()  AS LOGIC STRICT
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdListRebuild())

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdName(uOrder, cOrdBag) AS LOGIC CLIPPER
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	RETURN VoDb.OrderInfo(DBOI_NAME, cOrdBag, uOrder, NULL)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdNumber(uOrder, cOrdBag) AS USUAL CLIPPER
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	RETURN VoDb.OrderInfo(DBOI_NUMBER, cOrdBag, uOrder,NULL)
	
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdSetFocus(uOrder, cOrdBag) AS USUAL CLIPPER
	LOCAL cOrder := ""   AS   STRING
	DEFAULT( REF cOrdBag, "")
	VoDb.OrdSetFocus(cOrdBag, uOrder, REF cOrder)
	RETURN cOrder
	
