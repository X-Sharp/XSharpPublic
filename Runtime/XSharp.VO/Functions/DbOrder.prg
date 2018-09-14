//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp.Rdd.Support

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
	


FUNCTION DbSeek(xValue, lSoft, lLast) AS LOGIC CLIPPER
	LOCAL dbsci     AS DBSCOPEINFO
	LOCAL lRet      AS LOGIC
	
	DEFAULT(REF lSoft, SetSoftSeek())
	
	
	IF !IsNil(lLast)
		dbsci:Last := lLast
	ENDIF
	VODBSetScope( dbsci)
	DEFAULT(REF xValue, "")
	IF !VODBSeek(xValue, lSoft)
		RETURN (LOGIC) DoError(#DbSeek)
	ENDIF
	lRet := VODBFound()
	VODBSetScope(dbsci)
	
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
FUNCTION DbSetOrder (uOrder, cBagName) AS LOGIC CLIPPER
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
		nRet := Functions.DbOrderInfo(DBOI_ORDERCOUNT)
    ELSE
        nRet := 0
	ENDIF
	
	RETURN nRet
	

/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION INDEXEXT       () AS STRING STRICT
	RETURN VODBOrdBagExt()
	
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION INDEXKEY(uOrder) AS STRING CLIPPER
	
	LOCAL uRetVal   AS USUAL
	
	IF IsNil(uOrder)
		uOrder := 0
	ENDIF
	
	uRetVal := Functions.DbOrderInfo(DBOI_EXPRESSION, "", uOrder)
	
	IF IsNil(uRetVal)
		uRetVal := ""
	ENDIF
	
	RETURN uRetVal
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION INDEXORD       () AS INT STRICT
	
	LOCAL uRetVal := NIL AS USUAL
	
	uRetVal := Functions.DbOrderInfo(DBOI_NUMBER, "", NIL)
	
	DEFAULT( REF uRetVal, 0)
	
	RETURN uRetVal
	
	
STATIC FUNCTION OrdScopeNum(nScope)  AS SHORT CLIPPER
	
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
	
	
	
FUNCTION OrdSetRelation (cAlias, bKey, cKey) AS USUAL CLIPPER
	
	DbSetRelation(cAlias, bKey, cKey)
	
	(cAlias)->(OrdScope(0, bKey))
	
	(cAlias)->(OrdScope(1, bKey))
	
	RETURN NIL
	
	
	
FUNCTION OrdScope       (nScope, xVal) AS USUAL CLIPPER
	LOCAL n     AS DWORD
	
	nScope := OrdScopeNum(nScope)
	
	n := DBOI_SCOPETOP
	
	IF PCount() > 1 .AND. IsNil(xVal)
		n := DBOI_SCOPETOPCLEAR
	ENDIF
	
	RETURN Functions.DbOrderInfo(n + nScope,,,xVal)
	
	
	
	
FUNCTION OrdSkipUnique(uCount) AS USUAL CLIPPER
    LOCAL nCount := uCount AS OBJECT
	RETURN VODBOrderInfo ( DBOI_SKIPUNIQUE, "", NIL, REF nCount )
	
	
	
	
FUNCTION OrdIsUnique    (xOrder, cOrderBag) AS USUAL CLIPPER
	
	RETURN Functions.DbOrderInfo(DBOI_UNIQUE, cOrderBag, xOrder)
	
	


// DbOrder.prg - Order related functions
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION ORDBAGEXT () AS STRING STRICT
	RETURN VODBOrdBagExt()
	
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION ORDBAGNAME     (uOrder) AS STRING CLIPPER
	RETURN Functions.DbOrderInfo(DBOI_BAGNAME, "", uOrder)
	
	
	
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
	
	
	LOCAL lRetCode          AS LOGIC
	LOCAL dbOrdCondInfo     AS DbOrderCondInfo
	
	dbOrdCondInfo := DbOrderCondInfo{}
	IF !IsNil(cFor)
        dbOrdCondInfo:ForExpression := cFor
	ENDIF
	
	
	IF !IsNil(uCobFor)
        dbOrdCondInfo:ForBlock := uCobFor
	ENDIF
	
	IF !IsNil(uCobWhile)
		dbOrdCondInfo:WhileBlock := uCobWhile
	ENDIF
	
	IF !IsNil(uCobEval)
		dbOrdCondInfo:EvalBlock := uCobEval
	ENDIF
	
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
	
	lRetCode := VODBOrdCondSet( dbOrdCondInfo )
	
	IF !lRetCode
		lRetCode := (LOGIC) DoError(#OrdCondSet)
	ENDIF
	
	RETURN lRetCode
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdCreate(cName, cOrder, cExpr, cobExpr, lUnique) AS LOGIC CLIPPER
	
	LOCAL lRetCode  AS LOGIC
	
	IF IsNil(lUnique)
		lUnique := SetUnique()
	ENDIF
	
	IF IsNil(cName)
		IF IsNil(cOrder)
            PostArgumentError("OrdCreate", EDB_CREATEINDEX, nameof(cName), 1, {cName})
			DoError("OrdCreate")
		ELSE
			cName := ""
		ENDIF
	ENDIF
	
	IF IsNil(cExpr)
		cExpr := ""
		IF IsNil(cobExpr)
            PostArgumentError("OrdCreate", EDB_EXPRESSION, nameof(cExpr), 3, {cExpr})
  			DoError("OrdCreate")
		ENDIF
	ELSE
		IF IsNil(cobExpr)
			cobExpr := &( "{||" + cExpr + "}" )
		ENDIF
	ENDIF
	
	lRetCode := VODBOrdCreate (cName, cOrder, cExpr, cobExpr, lUnique, NULL)
	
	IF !lRetCode
		lRetCode := (LOGIC) DoError(#OrdCreate)
	ENDIF
	
	RETURN lRetCode
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdDescend     (xOrder, cOrdBag, lDescend) AS LOGIC CLIPPER
	
	IF !IsLogic( lDescend )
		lDescend := NIL
	ENDIF
	
	RETURN Functions.DbOrderInfo(DBOI_ISDESC, cOrdBag, xOrder, lDescend)
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdDestroy (uOrder, cOrdBag) AS LOGIC CLIPPER
	
	LOCAL lRetCode AS LOGIC
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	IF IsString(uOrder)
		lRetCode := VODBOrdDestroy (cOrdBag, uOrder)
	ELSE
		PostArgumentError("OrdDestroy", EDB_ORDDESTROY, nameof(uOrder), 1, {uOrder})
        lRetCode := FALSE
	ENDIF
	IF !lRetCode
		lRetCode := (LOGIC) DoError("OrdDestroy")
	ENDIF
	RETURN lRetCode
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION ORDFOR         (uOrder, cOrdBag, cFor) AS LOGIC CLIPPER
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	IF !IsString(cFor)
		cFor := NIL
	ENDIF
	
	RETURN Functions.DbOrderInfo(DBOI_CONDITION, cOrdBag, uOrder, cFor)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION ORDKEY(uOrder, cOrdBag) AS USUAL CLIPPER
	LOCAL xKey  := NULL    AS OBJECT
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	IF !VODBOrderInfo(DBOI_EXPRESSION, cOrdBag, uOrder, REF xKey)
		xKey := ""
	ENDIF
	RETURN xKey
    
FUNCTION OrdKeyAdd(xOrder, cOrdBag, xVal) AS USUAL CLIPPER
	
	RETURN Functions.DbOrderInfo(DBOI_KEYADD, cOrdBag, xOrder, xVal)
	

FUNCTION OrdKeyDel(xOrder, cOrdBag, xVal) AS USUAL CLIPPER
	
	RETURN Functions.DbOrderInfo( DBOI_KEYDELETE, cOrdBag, xOrder, NIL)
	
	
FUNCTION OrdKeyGoto     (nKeyNo) AS LOGIC CLIPPER
	LOCAL lRetCode  AS LOGIC
	IF IsNumeric(nKeyNo)
		DbGotop()
		DBSKIP(nKeyno - 1)
		lRetCode := TRUE
    ELSE
        lRetCode := FALSE
	ENDIF
	RETURN lRetCode
	
	
	
FUNCTION OrdKeyCount    (xOrder, cOrdBag) AS USUAL CLIPPER
	RETURN Functions.DbOrderInfo(DBOI_KEYCOUNT, cOrdBag, xOrder, NIL)
	
	
FUNCTION ORDKeyNo       (xOrder, cOrdBag) 	AS USUAL CLIPPER
	RETURN Functions.DbOrderInfo( DBOI_POSITION, cOrdBag, xOrder, NIL)
	
	
	
FUNCTION ORDKeyVal      () AS USUAL STRICT
	RETURN Functions.DbOrderInfo( DBOI_KEYVAL, NIL ,NIL, NIL)
	
	
	
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION ORDLISTADD     (cOrdBag, uOrder) AS LOGIC CLIPPER
	
	LOCAL lRetCode  AS LOGIC
	
	lRetCode := VODBOrdListAdd(cOrdBag, uOrder)
	
	IF !lRetCode
		lRetCode := (LOGIC) DoError(#ORDLISTADD)
	ENDIF
	
	RETURN lRetCode



/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdListClear   (cOrdBag, uOrder)  AS LOGIC CLIPPER
	LOCAL lRetCode  AS LOGIC
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	lRetCode := VODBOrdListClear(cOrdBag, uOrder)
	IF !lRetCode
		lRetCode := (LOGIC) DoError("OrdListClear")
	ENDIF
	
	RETURN lRetCode
	
	
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
	LOCAL lRetCode  AS LOGIC
	lRetCode := VODBOrdListRebuild()
	IF !lRetCode
		lRetCode := (LOGIC) DoError("OrdListRebuild")
	ENDIF
	RETURN lRetCode
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdName(uOrder, cOrdBag) AS LOGIC CLIPPER
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	RETURN Functions.DbOrderInfo(DBOI_NAME, cOrdBag, uOrder)
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION OrdNumber(uOrder, cOrdBag) AS USUAL CLIPPER
	
	IF IsNil(cOrdBag)
		cOrdBag := ""
	ENDIF
	
	RETURN Functions.DbOrderInfo(DBOI_NUMBER, cOrdBag, uOrder)
	
	
	
	
/// <summary>
/// </summary>
/// <returns>
/// </returns>
FUNCTION ORDSETFOCUS    (uOrder, cOrdBag) AS USUAL CLIPPER
	LOCAL cOrder := ""   AS   STRING
	DEFAULT( REF cOrdBag, "")
	VODBOrdSetFocus(cOrdBag, uOrder, REF cOrder)
	RETURN cOrder
	
