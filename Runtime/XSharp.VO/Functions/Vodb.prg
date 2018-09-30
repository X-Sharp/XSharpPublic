//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING XSharp.RDD
USING XSharp.RDD.Support
USING System.Collections.Generic
USING SYstem.Linq
INTERNAL STATIC CLASS VoDb
    STATIC METHOD ValidBlock(uBlock AS USUAL) AS ICodeBlock
        LOCAL oBlock    := uBlock   AS OBJECT
        IF oBlock IS ICodeBlock
            RETURN (ICodeBlock) oBlock
        ENDIF
        RETURN NULL

END CLASS    

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
    result := XSharp.Core.Functions.VODBBlobInfo(nOrdinal, nPos, REF oRet)
    ptrRet := oRet
    RETURN result	  

   /// <summary>
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="nPos"></param>
    /// <param name="uValue"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBBlobInfo(nOrdinal AS DWORD,nPos AS DWORD,uValue AS USUAL) AS LOGIC
    LOCAL result AS LOGIC
    result := XSharp.Core.Functions.VODBBlobInfo(nOrdinal, nPos, (OBJECT) uValue)
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
    RETURN VODbEval(VoDb.ValidBlock(uBlock), VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), (OBJECT) nNext, (OBJECT) nRecno, lRest)
    
    
    /// <summary>
    /// Retrieve field definition information about a field.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="nPos"></param>
    /// <param name="ptrRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBFieldInfo(nOrdinal AS DWORD,nPos AS DWORD,ptrRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT
    LOCAL result AS LOGIC
    result := XSharp.Core.Functions.VODBFieldInfo(nOrdinal, nPos, REF oRet)
    ptrRet := oRet
    RETURN result	

    /// <summary>
    /// Set field definition information about a field.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="nPos"></param>
    /// <param name="uValue"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBFieldInfo(nOrdinal AS DWORD,nPos AS DWORD,uValue AS USUAL) AS LOGIC
    LOCAL result AS LOGIC
    result := XSharp.Core.Functions.VODBFieldInfo(nOrdinal, nPos, (OBJECT) uValue)
    RETURN result	
    

/// <summary>
/// Retrieve the value of a specified database field.
/// </summary>
/// <param name="nPos"></param>
/// <param name="oRet"></param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBFieldGet(nPos AS DWORD,uRet REF USUAL) AS LOGIC
    LOCAL lResult AS LOGIC
    LOCAL oValue := uRet AS OBJECT
    lResult := XSharp.Core.Functions.VODBFieldGet(nPos, REF oValue)
    uRet := oValue
    RETURN lResult
    
    /// <summary>
    /// Set the value of a specified database field.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="xValue"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBFieldPut(nPos AS DWORD,xValue AS USUAL) AS LOGIC
    RETURN VODBFieldPut(nPos, (OBJECT) xValue)
    
    
    
    /// <summary>
    /// Move to a record specified by record number.
    /// </summary>
    /// <param name="uRecId"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBGoto(uRecId AS USUAL) AS LOGIC
    RETURN VODBGoto((OBJECT) uRecID)
    
    /// <summary>
    /// Retrieve information about a work area.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="ptrRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBInfo(nOrdinal AS DWORD,ptrRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT
    LOCAL result AS LOGIC
    result := XSharp.Core.Functions.VODbInfo(nOrdinal, REF oRet)
    ptrRet := oRet
    RETURN result
    
   /// <summary>
    /// Set information about a work area.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="uValue"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBInfo(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    LOCAL result AS LOGIC
    result := XSharp.Core.Functions.VODbInfo(nOrdinal, (OBJECT) uValue)
    RETURN result

    /// <summary>
    /// Search for the first record that matches a specified condition and scope.
    /// </summary>
    /// <param name="uCobFor"></param>
    /// <param name="uCobWhile"></param>
    /// <param name="nNext"></param>
    /// <param name="uRecId"></param>
    /// <param name="lRest"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBLocate(uCobFor AS USUAL,uCobWhile AS USUAL,nNext AS LONG,uRecId AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VODbLocate(VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), nNext, (OBJECT) uRecID, lRest)
    
    
    
    /// <summary>
    /// Create or replace an order in an index file.
    /// </summary>
    /// <param name="cBagName"></param>
    /// <param name="uOrder"></param>
    /// <param name="cExpr"></param>
    /// <param name="uCobExpr"></param>
    /// <param name="lUnique"></param>
    /// <param name="ptrCondInfo"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBOrdCreate(cBagName AS STRING,uOrder AS USUAL,cExpr AS STRING,uCobExpr AS USUAL,lUnique AS LOGIC,ptrCondInfo AS DbOrderCondInfo) AS LOGIC
    RETURN VODbOrdCreate(cBagName, (OBJECT) uOrder, cExpr, VoDb.ValidBlock(uCobExpr), lUnique, ptrCondInfo)
    
    /// <summary>
    /// Remove an order from an open index file.
    /// </summary>
    /// <param name="cOrdBag"></param>
    /// <param name="uOrder"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBOrdDestroy(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VODbOrdDestroy(cOrdBag, (OBJECT) uOrder)
    
    /// <summary>
    /// Return information about index files and the orders in them.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="cBagName"></param>
    /// <param name="uOrder"></param>
    /// <param name="uRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS OBJECT,uRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT   
    LOCAL result AS LOGIC
    result := XSharp.Core.Functions.VODbOrderInfo(nOrdinal, cBagName,  uOrder, REF oRet)
    uRet := oRet
    RETURN result

    /// <summary>
    /// Set information about index files and the orders in them.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="cBagName"></param>
    /// <param name="uOrder"></param>
    /// <param name="uValue"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBOrderInfo(nOrdinal AS DWORD,cBagName AS STRING,uOrder AS OBJECT,uValue AS USUAL) AS LOGIC
    LOCAL result AS LOGIC
    result := XSharp.Core.Functions.VODbOrderInfo(nOrdinal, cBagName,  uOrder, (OBJECT) uValue)
    RETURN result
    /// <summary>
    /// Open an index file and add specified orders to the order list in a work area.
    /// </summary>
    /// <param name="cOrdBag"></param>
    /// <param name="uOrder"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBOrdListAdd(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VODbOrdListAdd(cOrdBag, (OBJECT) uOrder)
    
    /// <summary>
    /// Remove orders from the order list in a work area and close associated index files.
    /// </summary>
    /// <param name="cOrdBag"></param>
    /// <param name="uOrder"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VoDbOrdListClear(cOrdBag AS STRING,uOrder AS USUAL) AS LOGIC
    RETURN VoDbOrdListClear(cOrdBag, (OBJECT) uOrder)
    
    
    /// <summary>
    /// Set the controlling order for a work area.
    /// </summary>
    /// <param name="cOrdBag"></param>
    /// <param name="uOrder"></param>
    /// <param name="pszOrder"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBOrdSetFocus(cOrdBag AS STRING,uOrder AS USUAL,cOrder OUT STRING) AS LOGIC
    RETURN VoDbOrdSetFocus(cOrdBag, (OBJECT) uOrder, OUT cOrder)

    /// <summary>
    /// Retrieve information about a RDD.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="uRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBRDDInfo(nOrdinal AS DWORD,uRet REF USUAL) AS LOGIC
    LOCAL oValue AS OBJECT
    oValue := uRet
    LOCAL result := XSharp.Core.Functions.VODBRddInfo(nOrdinal, REF oValue) AS LOGIC
    uRet := oValue
    RETURN result
    
     /// <summary>
    /// Set information about a RDD.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="uRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBRDDInfo(nOrdinal AS DWORD,uValue AS USUAL) AS LOGIC
    LOCAL result := XSharp.Core.Functions.VODBRddInfo(nOrdinal, (OBJECT) uValue) AS LOGIC
    RETURN result
    
    
    /// <summary>
    /// Retrieve information about a record.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="uRecId"></param>
    /// <param name="ptrRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBRecordInfo(nOrdinal AS DWORD,uRecId AS USUAL,uRet REF USUAL) AS LOGIC
    LOCAL oRet := NULL AS OBJECT
    LOCAL lResult AS LOGIC
    lResult := XSharp.Core.Functions.VODBRecordInfo(nOrdinal, uRecID, REF oRet)
    uRet := oRet
    RETURN lResult
    
    /// <summary>
    /// Set information about a record.
    /// </summary>
    /// <param name="nOrdinal"></param>
    /// <param name="uRecId"></param>
    /// <param name="ptrRet"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBRecordInfo(nOrdinal AS DWORD,uRecId AS USUAL,uRet AS USUAL) AS LOGIC
    LOCAL lResult AS LOGIC
    lResult := XSharp.Core.Functions.VODBRecordInfo(nOrdinal, uRecID,  (OBJECT) uRet)
    RETURN lResult
    

    /// <summary>
    /// Return the linking expression of a specified relation.
    /// </summary>
    /// <param name="nPos"></param>
    /// <param name="pszRel"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
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
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBRlock(uRecId AS USUAL) AS LOGIC
    RETURN VoDbRLock((OBJECT) uRecId)


    /// <summary>
    /// Move to the record having the specified key value.
    /// </summary>
    /// <param name="xValue"></param>
    /// <param name="lSoft"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBSeek(xValue AS USUAL,lSoft AS LOGIC, lLast AS LOGIC) AS LOGIC
    RETURN VODBSeek( (OBJECT) xValue, lSoft, lLast)

    /// <summary>
    /// Move to the record having the specified key value.
    /// </summary>
    /// <param name="xValue"></param>
    /// <param name="lSoft"></param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBSeek(xValue AS USUAL,lSoft AS LOGIC) AS LOGIC
    RETURN VODBSeek( (OBJECT) xValue, lSoft)
    
    /// <summary>
    /// Select a new work area and retrieve the current work area.
    /// </summary>
    /// <param name="nNew"></param>
    /// <param name="riOld"></param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
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
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBSetFilter(uCobFilter AS USUAL,cFilter AS STRING) AS LOGIC
    RETURN VODBSetFilter(VoDb.ValidBlock(uCobFilter), cFilter)
    
    
    
    /// <summary>
    /// Specify the code block for a locate condition.
    /// </summary>
    /// <param name="uCobFor"></param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBSetLocate(uCobFor AS USUAL) AS LOGIC
    RETURN VODBSetLocate(VoDb.ValidBlock(uCobFor))
    
    
    /// <summary>
    /// Relate a specified work area to the current work area.
    /// </summary>
    /// <param name="cAlias"></param>
    /// <param name="uCobKey"></param>
    /// <param name="cKey"></param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION VODBSetRelation(cAlias AS STRING,uCobKey AS USUAL,cKey AS STRING) AS LOGIC
    RETURN VODbSetRelation(cAlias, VoDb.ValidBlock(uCobKey), cKey)
    
    
    
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
FUNCTION VODBSort(nDest AS DWORD,fnNames AS _FieldNames,uCobFor AS USUAL,uCobWhile AS USUAL, nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC,fnSortNames AS _FieldNames) AS LOGIC
    RETURN VODbSort(nDest, fnNames, VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), (OBJECT) nNext, (OBJECT) nRecno, lRest, fnSortNames)
    
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
FUNCTION VODBTrans(nDest AS DWORD,fldNames AS _FieldNames,uCobFor AS USUAL,uCobWhile AS USUAL, nNext AS USUAL,nRecno AS USUAL,lRest AS LOGIC) AS LOGIC
    RETURN VODBTrans(nDest, fldNames, VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), (OBJECT) nNext, (OBJECT) nRecno, lRest)
    
    /// <summary>
    /// Release all locks for a work area.
    /// </summary>
    /// <param name="uRecno"></param>
    /// <returns>
    /// </returns>
FUNCTION VODBUnlock(uRecno AS USUAL) AS LOGIC
    RETURN VODbUnLock((OBJECT) uRecno)

    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBCreate(System.String,System.Collections.Generic.IList{XSharp.RDD.Support.RddFieldInfo},System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
    /// <param name="aStruct">Array with structure to use when creating the file.</param>
FUNCTION VODBCreate( cName AS STRING, aStruct AS ARRAY, cRddName AS STRING, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    LOCAL aFldInfo AS RddFieldInfo[]
    aFldInfo := Db.ArrayToFieldInfo(aStruct)
    RETURN VODBCreate(cName, aFldInfo:ToArray(), cRddName, lNew, cAlias, cDelim, lKeep, lJustOpen)  

    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBCreate(System.String,System.Collections.Generic.IList{XSharp.RDD.Support.RddFieldInfo},System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
    /// <param name="aStruct">Array with structure to use when creating the file.</param>
    /// <param name="oRddType">Type of the RDDs to use when creating the file</param>
FUNCTION VODBCreate( cName AS STRING, aStruct AS ARRAY, oRddType AS System.Type, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    LOCAL aFldInfo AS RddFieldInfo[]
    aFldInfo := Db.ArrayToFieldInfo(aStruct)
    RETURN VODBCreate(cName, aFldInfo, oRddType, lNew, cAlias, cDelim, lKeep, lJustOpen)  



    /// <inheritdoc cref="M:XSharp.Core.Functions.VODBCreate(System.String,System.Collections.Generic.IList{XSharp.RDD.Support.RddFieldInfo},System.String,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
    /// <param name="aStruct">Array with structure to use when creating the file.</param>
    /// <param name="aList">structure that describes the list of arrays to use</param>
FUNCTION VODBCreate( cName AS STRING, aStruct AS ARRAY, aList AS _RddList, lNew AS LOGIC, cAlias AS STRING, cDelim AS STRING, lKeep AS LOGIC, lJustOpen AS LOGIC ) AS LOGIC
    LOCAL oRdd AS RegisteredRDD
    FOREACH VAR name IN aList:atomRDDName
        oRDD := RegisteredRDD.Find(name)
        IF oRDD != NULL
            oRdd:Load()
        ELSE
            EXIT
        ENDIF
    NEXT
    IF oRDD == NULL
        RETURN FALSE
    ENDIF
    LOCAL aFldInfo AS RddFieldInfo[]
    aFldInfo := Db.ArrayToFieldInfo(aStruct)
    RETURN VODBCreate(cName, aFldInfo, oRDD:RddType, lNew, cAlias, cDelim, lKeep, lJustOpen)  




FUNCTION __AllocRddList(aRDDs AS ARRAY) AS _RddList
    LOCAL aNames AS List<STRING>
    aNames := List<STRING>{}
    FOREACH rdd AS USUAL IN aRDDs
        aNames:Add( (STRING) rdd)
    NEXT
    RETURN _RddList{aNames:ToArray()}
    
