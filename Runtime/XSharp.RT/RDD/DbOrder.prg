//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING XSharp.Rdd.Support
USING XSharp.Rdd
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearordercondition/*" />
FUNCTION DBClearOrderCondition()  AS LOGIC
    
	RETURN OrdCondSet("", NIL, .F., NIL, NIL, 0, 0, 0, 0, .F., .F.)
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbreindex/*" />
FUNCTION DbReindex() AS LOGIC
    // Don't call CoreDb because of error handling
	RETURN OrdListRebuild()
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbseek/*" />
FUNCTION DbSeek(uKey, lSoftSeek, lLast) AS LOGIC CLIPPER
	LOCAL dbsci     AS DBSCOPEINFO
	LOCAL lRet      AS LOGIC
	
	DEFAULT(REF lSoftSeek, SetSoftSeek())
	
	dbsci := DbScopeInfo{}
	IF lLast:IsNil
		lLast := FALSE
	ENDIF
	VoDb.SetScope( dbsci)
	DEFAULT(REF uKey, "")
    IF lRet := _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Seek(uKey, lSoftSeek, lLast))
	    lRet := VoDb.Found()
	    VoDb.SetScope(dbsci)
    ENDIF
	RETURN lRet
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetfound/*" />
FUNCTION DbSetFound(lFnd AS LOGIC) AS LOGIC 
	RETURN VoDb.SetFound(lFnd)
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetordercondition/*" />
FUNCTION DbSetOrderCondition(  cForCondition, cbForCondition, lAll, cbWhileCondition, cbEval, nInterval, nStart, nNext, nRecord, lRest, lDescend, lAdditive, lCurrent, lCustom, lNoOptimize) AS LOGIC CLIPPER
    // Don't call VoDb because of error handling
	RETURN OrdCondSet( cForCondition, cbForCondition, lAll, cbWhileCondition, cbEval, ;
		nInterval, nStart, nNext, nRecord, lRest,      ;
		lDescend, lAdditive, lCurrent, lCustom, lNoOptimize)

	
/// <param name="uOrder">Order in the index file to activate. When not specified then the first order in the file becomes active.</param>
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetindex/*" />
FUNCTION DbSetIndex(cIndexFile, uOrder) AS LOGIC CLIPPER
	// Don't call VoDb because of error handling
	IF cIndexFile:IsNil
		RETURN OrdListClear()
	ENDIF
	RETURN OrdListAdd(cIndexFile, uOrder)
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetorder/*" />
FUNCTION DbSetOrder(uOrder, cIndexFile) AS LOGIC CLIPPER
	DEFAULT( REF cIndexFile, "")
	RETURN VoDb.OrdSetFocus(cIndexFile, uOrder)
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/indexcount/*" />
FUNCTION IndexCount() AS DWORD 
	LOCAL nRet      AS DWORD
	IF Used()
		nRet := DbOrderInfo(DBOI_ORDERCOUNT)
    ELSE
        nRet := 0
	ENDIF
	
	RETURN nRet
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/indexext/*" />
FUNCTION IndexExt       () AS STRING STRICT
	RETURN VoDb.OrdBagExt()
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/indexkey/*" />
FUNCTION IndexKey(nPosition) AS STRING CLIPPER
	LOCAL uRetVal   AS USUAL
	IF nPosition:IsNil
		nPosition := 0
	ENDIF
	uRetVal := DbOrderInfo(DBOI_EXPRESSION, "", nPosition)
	RETURN uRetVal
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/indexord/*" />
FUNCTION IndexOrd() AS INT STRICT
	LOCAL uRetVal := NIL AS USUAL
	uRetVal := DbOrderInfo(DBOI_NUMBER, "", NIL)
	DEFAULT( REF uRetVal, 0)
    RETURN uRetVal
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordsetrelation/*" />
FUNCTION OrdSetRelation(uArea, cbKey, cKey) AS USUAL CLIPPER
	DbSetRelation(uArea, cbKey, cKey)
	(uArea)->(OrdScope(0, cbKey))
	(uArea)->(OrdScope(1, cbKey))
	RETURN NIL
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordscope/*" />
FUNCTION OrdScope(kScope, uNewValue) AS USUAL CLIPPER
	LOCAL n     AS DWORD
	kScope := VoDb.OrdScopeNum(kScope)
	n := DBOI_SCOPETOP
	IF PCount() > 1 .AND. uNewValue:IsNil
		n := DBOI_SCOPETOPCLEAR
	ENDIF
	RETURN DbOrderInfo(n + kScope,,,uNewValue)
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordskipunique/*" />	
FUNCTION OrdSkipUnique(nDirection) AS USUAL CLIPPER
	RETURN VoDb.OrderInfo ( DBOI_SKIPUNIQUE, "", NIL, nDirection )
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordisunique/*" />		
FUNCTION OrdIsUnique   (uOrder, cIndexFile) AS USUAL CLIPPER
    LOCAL result := NIL AS USUAL
	VoDb.OrderInfo(DBOI_UNIQUE, cIndexFile, uOrder,REF result)
    RETURN result
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordbagext/*" />
FUNCTION OrdBagExt() AS STRING STRICT
	RETURN VoDb.OrdBagExt()
	
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordbagname/*" />
FUNCTION OrdBagName(uOrder) AS STRING CLIPPER
	RETURN DbOrderInfo(DBOI_BAGNAME, "", uOrder)
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordcondset/*" />
FUNCTION OrdCondSet(cForCondition, cbForCondition, lAll, cbWhileCondition, cbEval, nInterval, nStart,     ;
		nNext, nRecord,lRest,lDescend,lAdditive,lCurrent, lCustom, lNoOptimize     ) AS LOGIC CLIPPER
	
	LOCAL dbOrdCondInfo     AS DbOrderCondInfo
	
	dbOrdCondInfo := DbOrderCondInfo{}

	
    dbOrdCondInfo:ForBlock := VoDb.ValidBlock(cbForCondition)
    dbOrdCondInfo:WhileBlock := VoDb.ValidBlock(cbWhileCondition)
	dbOrdCondInfo:EvalBlock := VoDb.ValidBlock(cbEval)
	IF !cForCondition:IsNil
        dbOrdCondInfo:ForExpression := cForCondition
        IF dbOrdCondInfo:ForBlock == NULL
            dbOrdCondInfo:ForBlock := MCompile(dbOrdCondInfo:ForExpression)
        ENDIF
	ENDIF
	IF nInterval:IsNumeric
		dbOrdCondInfo:StepSize := nInterval
	ENDIF
	IF nStart:IsNumeric
		dbOrdCondInfo:StartRecNo := nStart
	ENDIF
	IF nNext:IsNumeric
		dbOrdCondInfo:NextCount := nNext
	ENDIF
	IF nRecord:IsNumeric
		dbOrdCondInfo:RecNo := nRecord
	ENDIF
	IF lRest:IsLogic
		dbOrdCondInfo:Rest := lRest
	ENDIF
	IF lDescend:IsLogic
		dbOrdCondInfo:Descending := lDescend
	ENDIF
	IF lAll:IsLogic
		dbOrdCondInfo:All := lAll
	ENDIF
	IF lAdditive:IsLogic
		dbOrdCondInfo:Additive := lAdditive
	ENDIF
	IF lCustom:IsLogic
		dbOrdCondInfo:Custom := lCustom
	ENDIF
	IF lCurrent:IsLogic
		dbOrdCondInfo:UseCurrent := lCurrent
	ENDIF
	IF !lNoOptimize:IsNil
		dbOrdCondInfo:NoOptimize := lNoOptimize
	ENDIF
	RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdCondSet( dbOrdCondInfo ))
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcreateorder/*" />
FUNCTION OrdCreate(cIndexFile, cOrder, cKeyValue, cbKeyValue, lUnique) AS LOGIC CLIPPER
	IF lUnique:IsNil
		lUnique := SetUnique()
	ENDIF
	
	IF cIndexFile:IsNil
		IF cOrder:IsNil
            RddError.PostArgumentError("OrdCreate", EDB_CREATEINDEX, nameof(cIndexFile), 1, {cIndexFile})
			DoError("OrdCreate")
		ELSE
			cIndexFile := ""
		ENDIF
	ENDIF
	
	IF cKeyValue:IsNil
		cKeyValue := ""
		IF cbKeyValue:IsNil
            RddError.PostArgumentError("OrdCreate", EDB_EXPRESSION, nameof(cKeyValue), 3, {cKeyValue})
  			DoError("OrdCreate")
		ENDIF
	ELSE
		IF cbKeyValue:IsNil
			cbKeyValue := &( "{||" + cKeyValue + "}" )
		ENDIF
	ENDIF
	
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdCreate(cIndexFile, cOrder, cKeyValue, cbKeyValue, lUnique, NULL))
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/orddescend/*" />
FUNCTION OrdDescend(uOrder, cIndexFile, lNewDescend) AS LOGIC CLIPPER
	LOCAL oResult AS OBJECT	
	IF ! lNewDescend:IsLogic
		lNewDescend := NIL
	ENDIF
    oResult := lNewDescend
    VoDb.OrderInfo(DBOI_ISDESC, cIndexFile, uOrder, REF oResult)
    RETURN (LOGIC) oResult
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/orddestroy/*" />
FUNCTION OrdDestroy(uOrder, cIndexFile) AS LOGIC CLIPPER
	IF !uOrder:IsString
        RddError.PostArgumentError(__FUNCTION__, EDB_ORDDESTROY, nameof(uOrder), 1, {uOrder})
        RETURN FALSE
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdDestroy(cIndexFile, uOrder))
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordfor/*" />
FUNCTION OrdFor(uOrder, cIndexFile, cFor) AS LOGIC CLIPPER
	IF !cFor:IsString
		cFor := NIL
	ENDIF
	RETURN VoDb.OrderInfo(DBOI_CONDITION, cIndexFile, uOrder, cFor)
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordkey/*" />
FUNCTION OrdKey(uOrder, cIndexFile) AS USUAL CLIPPER
	LOCAL xKey  := NIL    AS USUAL
	IF !VODBOrderInfo(DBOI_EXPRESSION, cIndexFile, uOrder, REF xKey)
		xKey := ""
	ENDIF
	RETURN xKey
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordkeyadd/*" />	
FUNCTION OrdKeyAdd(uOrder, cIndexFile, uKeyValue) AS USUAL CLIPPER
	RETURN VoDb.OrderInfo(DBOI_KEYADD, cIndexFile, uOrder, uKeyValue)
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordkeydel/*" />	
FUNCTION OrdKeyDel(uOrder, cIndexFile, xVal) AS USUAL CLIPPER
	RETURN VoDb.OrderInfo( DBOI_KEYDELETE, cIndexFile, uOrder, NIL)
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordkeygoto/*" />	
FUNCTION OrdKeyGoto    (nKeyNo) AS LOGIC CLIPPER
	LOCAL lRetCode  AS LOGIC
	IF nKeyNo:IsNumeric
		DbGotop()
		DBSKIP(nKeyno - 1)
		lRetCode := TRUE
    ELSE
        lRetCode := FALSE
	ENDIF
	RETURN lRetCode
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordkeycount/*" />		
FUNCTION OrdKeyCount(uOrder, cIndexFile) AS USUAL CLIPPER
    LOCAL result := NIL AS USUAL
    VoDb.OrderInfo(DBOI_KEYCOUNT, cIndexFile, uOrder, REF result)
    RETURN result
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordkeyno/*" />	
FUNCTION OrdKeyNo(uOrder, cIndexFile) 	AS USUAL CLIPPER
    LOCAL result := NIL AS USUAL
    VoDb.OrderInfo(DBOI_POSITION, cIndexFile, uOrder, REF result)
    RETURN result
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordkeyval/*" />	
FUNCTION OrdKeyVal(uOrder,cIndexFile) AS USUAL STRICT
    LOCAL result := NIL AS USUAL
    VoDb.OrderInfo(DBOI_KEYVAL, cIndexFile, uOrder, REF result)
    RETURN result
	
/// <summary>Determines the number of orders for the current work area.  </summary>
/// <returns>OrdCount() returns the number of open indexes as a numeric value.
/// When no index is open or when no file is open in the current workarea, the return value is 0. </returns>
FUNCTION OrdCount() AS DWORD
    RETURN IIF (Used(), DBOrderInfo(DBOI_ORDERCOUNT),0)	

/// <summary>Return a list of all tag names for the current work area. </summary>
/// <returns>OrdList() returns a one dimensional array holding strings with the tag names of all open indexes.
/// When no index is open, an empty array is returned. </returns>
FUNCTION OrdList() AS ARRAY STRICT 
	LOCAL aResult AS ARRAY        
	LOCAL nIndex, nCount AS DWORD
	aResult := {}                
	nCount := OrdCount()
	FOR nIndex := 1 UPTO nCount
		AAdd(aResult, DBOrderInfo(DBOI_NAME, ,nIndex) )
	NEXT
	RETURN aResult

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordlistadd/*" />		
FUNCTION OrdListAdd(cIndexFile, cOrder) AS LOGIC CLIPPER
	RETURN VoDb.OrdListAdd(cIndexFile, cOrder)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearindex/*" />		
FUNCTION OrdListClear(cIndexFile, uOrder)  AS LOGIC CLIPPER
	RETURN VoDb.OrdListClear(cIndexFile, uOrder)
	
	
/// <exclude />
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
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordlistrebuild/*" />	
FUNCTION OrdListRebuild ()  AS LOGIC STRICT
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.OrdListRebuild())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordname/*" />	
FUNCTION OrdName(uOrder, cIndexFile) AS USUAL CLIPPER
	LOCAL result := NIL AS USUAL
    VoDb.OrderInfo(DBOI_NAME, cIndexFile, uOrder, REF result)
	RETURN result
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ordnumber/*" />	
FUNCTION OrdNumber(uOrder, cIndexFile) AS USUAL CLIPPER
	LOCAL result := NIL AS USUAL
    VoDb.OrderInfo(DBOI_NUMBER, cIndexFile, uOrder,REF result)
	RETURN result
	
	
	
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetorder/*" />	
FUNCTION OrdSetFocus(uOrder, cIndexFile) AS USUAL CLIPPER
	LOCAL cOrder := ""   AS   STRING
	DEFAULT( REF cIndexFile, "")
	VoDb.OrdSetFocus(cIndexFile, uOrder, OUT cOrder)
	RETURN cOrder
	

   /// <summary>Checks if a scope is set in a work area.</summary>
    /// <param name="uScope">An optional constant that indicates which scope needs to be set.<br/>
    /// <include file="RTComments.xml" path="Comments/ScopeParams/*"  /> <br/></param>
    /// <returns><include file="RTComments.xml" path="Comments/ScopeReturn/*"  /></returns>
    /// <seealso cref='M:XSharp.RT.Functions.DbSetScope(System.Int32,XSharp.__Usual)' />
    /// <seealso cref='M:XSharp.RT.Functions.DbClearScope(XSharp.__Usual)' />

FUNCTION DbScope(uScope) AS USUAL CLIPPER
    IF IsNil(uScope)
        uScope := OrdScope(TOPSCOPE)
        IF uScope:IsNil
            uScope := OrdScope(BOTTOMSCOPE)
        ENDIF
        RETURN ! uScope:IsNil
    ENDIF
    IF !uScope:IsNumeric
        uScope := SCOPE_BOTH
    ENDIF
    SWITCH (LONG) uScope
    CASE SCOPE_TOP
        uScope := OrdScope(TOPSCOPE)
    CASE SCOPE_BOTTOM
        uScope := OrdScope(BOTTOMSCOPE)
    CASE SCOPE_BOTH
    OTHERWISE
        uScope := {OrdScope(TOPSCOPE),OrdScope(BOTTOMSCOPE)}
    END SWITCH
    RETURN uScope
    
    /// <summary>Sets scope values.</summary>
    /// <param name="nScope">A constant that indicates which scope needs to be set.<br/>
    /// <include file="RTComments.xml" path="Comments/ScopeParams/*"  /></param>
    /// <param name="uValue">The value that needs to be set.
    /// The type of the value must match the type of the index expression.</param>
    /// <returns>TRUE when the scope was set succesfully, otherwise FALSE.</returns>
    /// <seealso cref='M:XSharp.RT.Functions.DbClearScope(XSharp.__Usual)' />
    /// <seealso cref='M:XSharp.RT.Functions.DbScope(XSharp.__Usual)' />

FUNCTION DbSetScope(nScope AS LONG, uValue AS USUAL) AS LOGIC
    LOCAL lResult := TRUE AS LOGIC
    TRY
        SWITCH nScope
        CASE SCOPE_TOP
            OrdScope(TOPSCOPE,uValue)
            lResult := XSharp.RuntimeState:LastRDDError == NULL
                
        CASE SCOPE_BOTTOM
            OrdScope(BOTTOMSCOPE, uValue)
            lResult := XSharp.RuntimeState:LastRDDError == NULL
                
        CASE SCOPE_BOTH
        OTHERWISE
            OrdScope(TOPSCOPE,uValue)
            lResult := XSharp.RuntimeState:LastRDDError == NULL
            OrdScope(BOTTOMSCOPE, uValue)
            lResult := lResult .AND. (XSharp.RuntimeState:LastRDDError == NULL)
        END SWITCH
    CATCH AS Exception
        lResult := FALSE
    END TRY
    RETURN lResult

    /// <summary>Clears the top and/or bottom scope. </summary>
    /// <param name="nScope">An optional constant that indicates which scope needs to be set.<br/>
    /// <include file="RTComments.xml" path="Comments/ScopeParams/*"  /></param>
    /// <returns>TRUE when the scope was cleared succesfully, otherwise FALSE.</returns>
    /// <seealso cref='M:XSharp.RT.Functions.DbSetScope(System.Int32,XSharp.__Usual)' />
    /// <seealso cref='M:XSharp.RT.Functions.DbScope(XSharp.__Usual)' />

FUNCTION DbClearScope(uScope) AS LOGIC CLIPPER
    LOCAL lResult := TRUE AS LOGIC
    IF !uScope:IsNumeric
        uScope := SCOPE_BOTH
    ENDIF
    TRY
        SWITCH (LONG) uScope
        CASE SCOPE_TOP
            OrdScope(TOPSCOPE,NIL)
            lResult := XSharp.RuntimeState:LastRDDError == NULL
                
        CASE SCOPE_BOTTOM
            OrdScope(BOTTOMSCOPE, NIL)
            lResult := XSharp.RuntimeState:LastRDDError == NULL

        CASE SCOPE_BOTH        
        OTHERWISE   // SCOPE_BOTH is default in Xbase++
            OrdScope(TOPSCOPE,NIL)
            lResult := XSharp.RuntimeState:LastRDDError == NULL
            OrdScope(BOTTOMSCOPE, NIL)
            lResult := lResult .AND. (XSharp.RuntimeState:LastRDDError == NULL)
        END SWITCH
    CATCH AS Exception
        lResult := FALSE
    END TRY
    RETURN lResult
