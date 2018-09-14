//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
 
USING XSharp.RDD
USING XSharp.RDD.Support

/// <summary>
/// Return the alias of a specified work area as a symbol.
/// </summary>
/// <param name="nArea"></param>
/// <returns>
/// </returns>
FUNCTION VODBAliasSym(nArea AS DWORD) AS SYMBOL
    RETURN AsSymbol(VODbAlias(nArea))


/// <summary>
/// </summary>
/// <param name="nOrdinal"></param>
/// <param name="nPos"></param>
/// <param name="ptrRet"></param>
/// <returns>
/// </returns>
FUNCTION VODBBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
	LOCAL oRet := NULL AS OBJECT
    LOCAL result AS LOGIC
    result := VODBBlobInfo(nOrdinal, nPos, REF oRet)
    ptrRet := oRet
    RETURN result	  


/// <summary>
/// Evaluate a code block for each record that matches a specified scope and/or condition.
/// </summary>
/// <param name="uBlock"></param>
/// <param name="uCobFor"></param>
/// <param name="uCobWhile"></param>
/// <param name="nNext"></param>
/// <param name="nRecno"></param>
/// <param name="lRest"></param>
/// <returns>
/// </returns>
FUNCTION VODBEval(uBlock AS USUAL,uCobFor AS USUAL,uCobWhile AS USUAL,nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC) AS LOGIC
    LOCAL cbFor   := NULL AS ICodeBlock
    LOCAL cbWhile := NULL AS ICodeBlock
    LOCAL cbEval   := NULL AS ICodeBlock
    LOCAL oFor    := uCobFor   AS OBJECT
    LOCAL oWhile  := uCobWhile AS OBJECT
    LOCAL oEval   := uBlock AS OBJECT
    IF oFor IS ICodeBlock
        cbFor := (ICodeBlock) oFor
    ENDIF
    IF oWhile IS ICodeBlock
        cbWhile := (ICodeBlock) oWhile
    ENDIF
    IF oEval IS ICodeBlock
        cbEval := (ICodeBlock) oEval
    ENDIF
    RETURN VODbEval(cbEval, cbFor, cbWhile, (OBJECT) nNext, (OBJECT) nRecno, lRest)


/// <summary>
/// Retrieve field definition information about a field.
/// </summary>
/// <param name="nOrdinal"></param>
/// <param name="nPos"></param>
/// <param name="ptrRet"></param>
/// <returns>
/// </returns>
FUNCTION VODBFieldInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
	LOCAL oRet := NULL AS OBJECT
    LOCAL result AS LOGIC
    result := VODBFieldInfo(nOrdinal, nPos, REF oRet)
    ptrRet := oRet
    RETURN result	


/// <summary>
/// Set the value of a specified database field.
/// </summary>
/// <param name="nPos"></param>
/// <param name="xValue"></param>
/// <returns>
/// </returns>
FUNCTION VODBFieldPut(nPos AS DWORD,xValue AS USUAL) AS LOGIC
    RETURN VODBFieldPut(nPos, (OBJECT) xValue)



/// <summary>
/// Move to a record specified by record number.
/// </summary>
/// <param name="uRecId"></param>
/// <returns>
/// </returns>
FUNCTION VODBGoto(uRecId AS USUAL) AS LOGIC
    RETURN VODBGoto((OBJECT) uRecID)

/// <summary>
/// Retrieve information about a work area.
/// </summary>
/// <param name="nOrdinal"></param>
/// <param name="ptrRet"></param>
/// <returns>
/// </returns>
FUNCTION VODBInfo(nOrdinal AS DWORD,ptrRet REF USUAL) AS LOGIC
	LOCAL oRet := NULL AS OBJECT
    LOCAL result AS LOGIC
    result := VODbInfo(nOrdinal, REF oRet)
    ptrRet := oRet
    RETURN result


/// <summary>
/// Search for the first record that matches a specified condition and scope.
/// </summary>
/// <param name="uCobFor"></param>
/// <param name="uCobWhile"></param>
/// <param name="nNext"></param>
/// <param name="uRecId"></param>
/// <param name="lRest"></param>
/// <returns>
/// </returns>
FUNCTION VODBLocate(uCobFor AS USUAL,uCobWhile AS USUAL,nNext AS LONG,uRecId AS USUAL,lRest AS LOGIC) AS LOGIC
    LOCAL cbFor   := NULL AS ICodeBlock
    LOCAL cbWhile := NULL AS ICodeBlock
    LOCAL oFor    := uCobFor   AS OBJECT
    LOCAL oWhile  := uCobWhile AS OBJECT
    IF oFor IS ICodeBlock
        cbFor := (ICodeBlock) oFor
    ENDIF
    IF oWhile IS ICodeBlock
        cbWhile := (ICodeBlock) oWhile
    ENDIF
    RETURN VODbLocate(cbFor, cbWhile, nNext, (OBJECT) uRecID, lRest)



/// <summary>
/// Create or replace an order in an index file.
/// </summary>
/// <param name="cBagName"></param>
/// <param name="uOrder"></param>
/// <param name="cExpr"></param>
/// <param name="uCobExpr"></param>
/// <param name="lUnique"></param>
/// <param name="ptrCondInfo"></param>
/// <returns>
/// </returns>
FUNCTION VODBOrdCreate(cBagName AS STRING,uOrder AS USUAL,cExpr AS STRING,uCobExpr AS USUAL,lUnique AS LOGIC,ptrCondInfo AS DbOrderCondInfo) AS LOGIC
    LOCAL cbKey AS ICodeBlock
    LOCAL oKey AS OBJECT
    oKey := uCobExpr
    IF oKey IS ICodeBlock
        cbKey := (ICodeBlock) oKey
    ELSE
        cbKey := NULL
    ENDIF
RETURN VODbOrdCreate(cBagName, (OBJECT) uOrder, cExpr, cbKey, lUnique, ptrCondInfo)

/// <summary>
/// Remove an order from an open index file.
/// </summary>
/// <param name="cOrdBag"></param>
/// <param name="uOrder"></param>
/// <returns>
/// </returns>
FUNCTION VODBOrdDestroy(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VODbOrdDestroy(cOrdBag, (OBJECT) uOrder)

/// <summary>
/// Return information about index files and the orders in them.
/// </summary>
/// <param name="nOrdinal"></param>
/// <param name="cBagName"></param>
/// <param name="uOrder"></param>
/// <param name="uRet"></param>
/// <returns>
/// </returns>
FUNCTION VODBOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS USUAL,uRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT   
    LOCAL result AS LOGIC
    result := VODbOrderInfo(nOrdinal, cBagName, (OBJECT) uOrder, REF oRet)
    uRet := oRet
    RETURN result

/// <summary>
/// Open an index file and add specified orders to the order list in a work area.
/// </summary>
/// <param name="cOrdBag"></param>
/// <param name="uOrder"></param>
/// <returns>
/// </returns>
FUNCTION VODBOrdListAdd(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VODbOrdListAdd(cOrdBag, (OBJECT) uOrder)

/// <summary>
/// Remove orders from the order list in a work area and close associated index files.
/// </summary>
/// <param name="cOrdBag"></param>
/// <param name="uOrder"></param>
/// <returns>
/// </returns>
FUNCTION VoDbOrdListClear(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDbOrdListClear(cOrdBag, (OBJECT) uOrder)


/// <summary>
/// Set the controlling order for a work area.
/// </summary>
/// <param name="cOrdBag"></param>
/// <param name="uOrder"></param>
/// <param name="pszOrder"></param>
/// <returns>
/// </returns>
FUNCTION VODBOrdSetFocus(cOrdBag AS STRING,uOrder AS USUAL,cOrder OUT STRING) AS LOGIC
    RETURN VoDbOrdSetFocus(cOrdBag, (OBJECT) uOrder, OUT cOrder)


/// <summary>
/// Retrieve information about a record.
/// </summary>
/// <param name="nOrdinal"></param>
/// <param name="uRecId"></param>
/// <param name="ptrRet"></param>
/// <returns>
/// </returns>
FUNCTION VODBRecordInfo(nOrdinal AS DWORD,uRecId AS USUAL,uRet REF USUAL) AS LOGIC
    LOCAL oRet AS OBJECT
    LOCAL lResult AS LOGIC
    lResult := VODBRecordInfo(nOrdinal, (OBJECT) uRecID, REF oRet)
    uRet := oRet
    RETURN lResult


/// <summary>
/// Return the linking expression of a specified relation.
/// </summary>
/// <param name="nPos"></param>
/// <param name="pszRel"></param>
/// <returns>
/// </returns>
FUNCTION VODBRelation(nPos AS DWORD, uRel REF USUAL) AS LOGIC
    LOCAL cRel := "" AS STRING
    LOCAL lResult AS LOGIC
    lResult := VODBRelation(nPos, REF cRel)
    uRel := cRel
    RETURN lResult

/// <summary>
/// Lock the current record.
/// </summary>
/// <param name="uRecId"></param>
/// <returns>
/// </returns>
FUNCTION VODBRlock(uRecId AS USUAL) AS LOGIC
    RETURN VoDbRLock((OBJECT) uRecId)
    
/// <summary>
/// Move to the record having the specified key value.
/// </summary>
/// <param name="xValue"></param>
/// <param name="lSoft"></param>
/// <returns>
/// </returns>
FUNCTION VODBSeek(xValue AS USUAL,lSoft AS LOGIC) AS LOGIC
RETURN VODBSeek( (OBJECT) xValue, lSoft)

/// <summary>
/// Select a new work area and retrieve the current work area.
/// </summary>
/// <param name="nNew"></param>
/// <param name="riOld"></param>
/// <returns>
/// </returns>
FUNCTION VODBSelect(nNew AS DWORD,riOld REF USUAL) AS LOGIC
	LOCAL nOld := 0 AS DWORD
    LOCAL lResult AS LOGIC
    lResult := VODBSelect(nNew, REF nOld)
    riOld := nOld
    RETURN lResult

/// <summary>
/// Set a filter condition.
/// </summary>
/// <param name="uCobFilter"></param>
/// <param name="cFilter"></param>
/// <returns>
/// </returns>
FUNCTION VODBSetFilter(uCobFilter AS USUAL,cFilter AS STRING) AS LOGIC
    LOCAL cbFilter := NULL AS ICodeBlock
    LOCAL oFilter  :=  uCobFilter AS OBJECT
    IF oFilter IS ICodeBlock
        cbFilter := (ICodeBlock) oFilter
    ENDIF
    RETURN VODBSetFilter(cbFilter, cFilter)



/// <summary>
/// Specify the code block for a locate condition.
/// </summary>
/// <param name="uCobFor"></param>
/// <returns>
/// </returns>
FUNCTION VODBSetLocate(uCobFor AS USUAL) AS LOGIC
    LOCAL cbFor := NULL AS ICodeBlock
    LOCAL oFor := uCobFor AS OBJECT
    IF oFor IS ICodeBlock
        cbFor := (ICodeBlock) oFor
    ENDIF
    RETURN VODBSetLocate(cbFor)
  

/// <summary>
/// Relate a specified work area to the current work area.
/// </summary>
/// <param name="cAlias"></param>
/// <param name="uCobKey"></param>
/// <param name="cKey"></param>
/// <returns>
/// </returns>
FUNCTION VODBSetRelation(cAlias AS STRING,uCobKey AS USUAL,cKey AS STRING) AS LOGIC
    LOCAL cbKey := NULL AS ICodeBlock
    LOCAL oKey := uCobKey AS OBJECT
    IF oKey IS ICodeBlock
        cbKey := (ICodeBlock) oKey
    ENDIF
	RETURN VODbSetRelation(cAlias, cbKey, cKey)



/// <summary>
/// </summary>
/// <param name="nDest"></param>
/// <param name="fnNames"></param>
/// <param name="uCobFor"></param>
/// <param name="uCobWhile"></param>
/// <param name="nNext"></param>
/// <param name="nRecno"></param>
/// <param name="lRest"></param>
/// <param name="fnSortNames"></param>
/// <returns>
/// </returns>
FUNCTION VODBSort(nDest AS DWORD,fnNames AS DbFIELDNAMES,uCobFor AS USUAL,uCobWhile AS USUAL, nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC,fnSortNames AS DbFIELDNAMES) AS LOGIC
    LOCAL cbFor   := NULL AS ICodeBlock
    LOCAL cbWhile := NULL AS ICodeBlock
    LOCAL oFor    := uCobFor   AS OBJECT
    LOCAL oWhile  := uCobWhile AS OBJECT
    IF oFor IS ICodeBlock
        cbFor := (ICodeBlock) oFor
    ENDIF
    IF oWhile IS ICodeBlock
        cbWhile := (ICodeBlock) oWhile
    ENDIF
    RETURN VODbSort(nDest, fnNames, cbFor, cbWhile, (OBJECT) nNext, (OBJECT) nRecno, lRest, fnSortNames)

/// <summary>
/// Select a new work area by specifying its alias as a symbol and return the number of the current work area.
/// </summary>
/// <param name="symAlias"></param>
/// <returns>
/// </returns>
FUNCTION VODBSymSelect(symAlias AS SYMBOL) AS INT
   LOCAL ret AS DWORD
   IF symAlias == NULL_SYMBOL
      ret := RuntimeState.CurrentWorkarea
   ELSE
      ret := RuntimeState.Workareas:FindAlias( AsString(symAlias) )
      IF ret != 0
         VODBSetSelect( (INT) ret )
      ENDIF
   ENDIF
   RETURN (INT) ret

/// <summary>
/// </summary>
/// <param name="nDest"></param>
/// <param name="fldNames"></param>
/// <param name="uCobFor"></param>
/// <param name="uCobWhile"></param>
/// <param name="nNext"></param>
/// <param name="nRecno"></param>
/// <param name="lRest"></param>
/// <returns>
/// </returns>
FUNCTION VODBTrans(nDest AS DWORD,fldNames AS DbFieldNames,uCobFor AS USUAL,uCobWhile AS USUAL, nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC) AS LOGIC
    LOCAL cbFor   := NULL AS ICodeBlock
    LOCAL cbWhile := NULL AS ICodeBlock
    LOCAL oFor    := uCobFor   AS OBJECT
    LOCAL oWhile  := uCobWhile AS OBJECT
    IF oFor IS ICodeBlock
        cbFor := (ICodeBlock) oFor
    ENDIF
    IF oWhile IS ICodeBlock
        cbWhile := (ICodeBlock) oWhile
    ENDIF
    RETURN VODBTrans(nDest, fldNames, cbFor, cbWhile, (OBJECT) nNext, (OBJECT) nRecno, lRest)

/// <summary>
/// Release all locks for a work area.
/// </summary>
/// <param name="uRecno"></param>
/// <returns>
/// </returns>
FUNCTION VODBUnlock(uRecno AS USUAL) AS LOGIC
    RETURN VODbUnLock((OBJECT) uRecno)


