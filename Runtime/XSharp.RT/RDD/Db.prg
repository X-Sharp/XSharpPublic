//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
// internal functions used by FieldBlock and FieldWBlock

USING XSharp.Rdd.Support
USING XSharp.Rdd
USING System.Linq
USING System.Collections.Generic

/// <summary>Determine the number of a work area.</summary>
FUNCTION Select(xValue AS USUAL) AS USUAL
    LOCAL dwSelect   AS DWORD
    LOCAL dwCurrent  AS DWORD
    dwCurrent := VoDb.GetSelect()
    dwSelect := _SELECT(xValue)
    VoDb.SetSelect((INT) dwCurrent)
    RETURN dwSelect
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION _Select(xValue) AS USUAL CLIPPER
    LOCAL nSelect           AS LONG
    LOCAL sAlias            AS SYMBOL
    LOCAL xType             AS DWORD
    LOCAL nAsc              AS DWORD
    
    IF IsNil(xValue)
        RETURN  (INT) VoDb.GetSelect() 
    ENDIF
    xType := UsualType(xValue)
    SWITCH xType
    CASE SYMBOL
        local symSelect := xValue as symbol
        nSelect := VoDb.SymSelect(symSelect)
    CASE STRING
        nSelect := 0
        LOCAL cValue := xValue AS STRING
        IF SLen(cValue) = 1
            nSelect := Val(cValue)
            nAsc := Asc( Upper(cValue) )
            IF nAsc > 64 .AND. nAsc < 75
                nSelect := (INT) nAsc - 64
            ENDIF
        ENDIF
        
        IF (nSelect > 0) .OR. ("0" == cValue)
            nSelect := (INT) VoDb.SetSelect(nSelect)
        ELSE
            sAlias  := AsSymbol( AllTrim(cValue) )
            nSelect := VoDb.SymSelect(sAlias)
        ENDIF
    CASE LONG
    CASE FLOAT
        nSelect := (INT) VoDb.SetSelect(xValue)
    OTHERWISE
        nSelect := 0
    END SWITCH
    RETURN nSelect
    
/// <exclude/>
FUNCTION __FieldGetNum( fieldpos AS DWORD ) AS USUAL
    LOCAL ret := NIL AS USUAL
    VoDb.FieldGet( fieldpos, REF ret )
    RETURN ret
    
/// <exclude/>
FUNCTION __FieldGetWaNum( workarea AS DWORD, fieldpos AS DWORD ) AS USUAL
    LOCAL ret := NIL AS USUAL
    LOCAL curArea AS DWORD
    curArea := RuntimeState.CurrentWorkarea
    TRY
        RuntimeState.CurrentWorkarea := workarea
        VoDb.FieldGet( fieldpos, REF ret )
    FINALLY
        RuntimeState.CurrentWorkarea := curArea
    END TRY   
    RETURN ret
    
    
/// <exclude/>
FUNCTION __FieldSetNum( fieldpos AS DWORD, uValue AS USUAL ) AS USUAL
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldPut( fieldpos, uValue ))
    
/// <exclude/>
FUNCTION __FieldSetWaNum( nArea AS DWORD, fieldpos AS DWORD, uValue AS USUAL ) AS USUAL
    LOCAL curArea AS DWORD
    curArea := RuntimeState.CurrentWorkarea
    TRY
        RuntimeState.CurrentWorkarea := nArea
        _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldPut( fieldpos, uValue ) )
    FINALLY
        RuntimeState.CurrentWorkarea := curArea
    END TRY   
    RETURN uValue
    
    
    
    
    /// <summary>
    /// Return a set-get code block for a field that is identified by its name.
    /// </summary>
    /// <param name="cFieldName"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldBlock(cFieldName AS STRING) AS CODEBLOCK
    LOCAL oCB  := NULL AS CODEBLOCK
    LOCAL nPos := 0    AS DWORD
    IF ! String.IsNullOrEmpty(cFieldName)
        nPos := FieldPos(cFieldName)
        IF nPos != 0
            //oCB := MCompile("{|x| iif( IsNil(x), __FieldGetNum( "+nPos:ToString()+"), __FieldSetNum( "+nPos:ToString()+" , x)")
            oCb := {|x| IIF(isNil(x), __FieldGetNum(nPos), __FieldSetNum(nPos, x))}
        ENDIF
    ENDIF
    RETURN oCB
    
    /// <summary>
    /// Return a set-get code block for a field that is identified by a Symbol.
    /// </summary>
    /// <param name="symFieldName"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldBlockSym(symFieldName AS SYMBOL) AS CODEBLOCK
    RETURN FieldBlock(symFieldName)   
    
    /// <summary>
    /// Return a set-get code block for a field, specified as a string, in a specified work area.
    /// </summary>
    /// <param name="cFieldName"></param>
    /// <param name="nArea"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldWBlock(cFieldName AS STRING,nArea AS DWORD) AS CODEBLOCK
    LOCAL oCB  := NULL AS CODEBLOCK
    LOCAL nPos := 0    AS DWORD
    IF ! String.IsNullOrEmpty(cFieldName)
        nPos := FieldPos(cFieldName, nArea)
        IF nPos != 0
            //VAR cPars := nArea:ToSTring()+","+nPos:ToString()
            // oCB := MCompile("{|x| iif( IsNil(x), __FieldGetWaNum("+cPars+"), __FieldSetWaNum("+cPars+", x)")
            oCB := {|x| IIF( IsNil(x), __FieldGetWaNum(nArea, nPos), __FieldSetWaNum(nArea, nPos, x)) }
        ENDIF
    ENDIF
    RETURN oCB
    
    /// <summary>
    /// Return a set-get code block for a field, specified as a Symbol, in a specified work area.
    /// </summary>
    /// <param name="symFieldname"></param>
    /// <param name="nArea"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldWBlockSym(symFieldname AS SYMBOL,nArea AS DWORD) AS CODEBLOCK
    RETURN FieldWBlock(symFieldname, nArea)
    
    
    
    
    /// <summary>
    /// Get the contents of a field that is identified by a work area alias and the field name.
    /// </summary>
    /// <param name="symAlias"></param>
    /// <param name="symField"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldGetAlias(symAlias AS SYMBOL,symField AS SYMBOL) AS USUAL
    RETURN fieldGetSelect(symAlias, symField)
    
    
    /// <summary>
    /// </summary>
    /// <param name="uSelect"></param>
    /// <param name="symField"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldGetSelect(uSelect AS USUAL,symField AS SYMBOL) AS USUAL
    LOCAL nArea := RuntimeState.CurrentWorkArea AS DWORD
    LOCAL uValue AS USUAL
    DbSelectArea(uSelect)
    uValue := FieldGetSym(symField)
    VoDb.SetSelect((INT) nArea)
    RETURN uValue
    
    /// <summary>
    /// Retrieve the contents of a field that is identified by its Symbolic name.
    /// </summary>
    /// <param name="symField"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldGetSym(symField AS SYMBOL) AS USUAL
    LOCAL dwPos AS DWORD
    dwPos := FieldPosSym(symField)
    IF dwPos == 0
        THROW Error.VODBError( EG_ARG, EDB_FIELDNAME,  symField  )
    ENDIF
    RETURN FieldGet( dwPos )  
    
    /// <summary>
    /// Return the position of a field that is identified by a Symbol.
    /// </summary>
    /// <param name="sField"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldPosSym(sField AS SYMBOL) AS DWORD
    RETURN FieldPos( (STRING) sField )
    
    
    /// <summary>
    /// Set the value of a field identified by its work area alias and field name.
    /// </summary>
    /// <param name="symAlias"></param>
    /// <param name="symField"></param>
    /// <param name="uValue"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldPutAlias(symAlias AS SYMBOL,symField AS SYMBOL,uValue AS USUAL) AS USUAL
    RETURN FieldPutSelect(symAlias, symField, uValue)
    
    /// <summary>
    /// Set the value of a field that is identified by its Symbolic name.
    /// </summary>
    /// <param name="symField"></param>
    /// <param name="uValue"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldPutSym(symField AS SYMBOL,uValue AS USUAL) AS USUAL
    LOCAL dwPos AS DWORD
    dwPos := FieldPosSym(symField)
    IF dwPos == 0
        THROW Error.VODBError( EG_ARG, EDB_FIELDNAME,  symField  )
    ENDIF
    RETURN FieldPut( dwPos ,uValue )
    
    /// <summary>
    /// </summary>
    /// <param name="uSelect"></param>
    /// <param name="symField"></param>
    /// <param name="uValue"></param>
    /// <returns>
    /// </returns>
FUNCTION FieldPutSelect(uSelect AS USUAL,symField AS SYMBOL,uValue AS USUAL) AS USUAL
    LOCAL origArea := (INT) RuntimeState.CurrentWorkArea  AS INT
    LOCAL ret AS USUAL
    LOCAL dwPos AS DWORD
    
    TRY
        dwPos := SELECT(uSelect )
        IF dwPos == 0
            THROW Error.VODBError( EG_ARG, EDB_BADALIAS,  symField  )
        ENDIF
        VoDb.SetSelect( (INT) dwPos )
        dwPos := FieldPosSym(symField)
        IF dwPos == 0
            VoDb.SetSelect( origArea )
            THROW Error.VODBError( EG_ARG, EDB_FIELDNAME,  symField  )
        ENDIF
        
        ret := FieldPut( dwPos, uValue )
    FINALLY
        VoDb.SetSelect( origArea )
    END TRY   
    
    RETURN ret
    
    
    
    
    
    *----------------------------------------------------------------------------
    
    /// <summary>
    /// </summary>
    /// <param name="nSelect"></param>
    /// <returns>
    /// </returns>
FUNCTION Alias(nSelect) AS STRING CLIPPER
    IF IsNil(nSelect)
        RETURN Alias0()
    ENDIF
    RETURN VoDb.Alias(nSelect)
    

/// <summary>RETURN the alias OF the current work area AS a symbol.</summary>
FUNCTION Alias0Sym() AS SYMBOL
    RETURN (SYMBOL) Alias0()
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbAppend(lReleaseLocks) AS LOGIC CLIPPER
    LOCAL lRetCode  AS LOGIC
    
    IF IsNil(lReleaseLocks)
        lReleaseLocks := .T.
    ENDIF
    
    lRetCode := VoDb.Append(lReleaseLocks)
    
    IF !lRetCode
        //    No Error but NetErr gets set for compatibility reasons
        //    lRetCode := DoError("DbAppend")
        NetErr(.T.)
    ENDIF
    
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbClearIndex(uOrder, cOrdBag) AS LOGIC CLIPPER
    RETURN OrdListClear(cOrdBag, uOrder)


/// <inheritdoc cref="M:XSharp.CoreDb.Create(System.String,XSharp.RDD.Support.RddFieldInfo[],System.Type,System.Boolean,System.String,System.String,System.Boolean,System.Boolean)" />
/// <param name="aStruct">Array with structure to use when creating the file.</param>
/// <param name="aHidden">A one-dimensional array with the names of RDDs from which the main RDD inherits special functionality.</param>
FUNCTION DbCreate (   cName,  aStruct, cRddName , lNew,  cAlias, cDelim, lJustOpen, aHidden ) AS LOGIC CLIPPER
    LOCAL lKeep           AS LOGIC
    LOCAL lRetCode        AS LOGIC
    
    IF aStruct == NIL
        aStruct := {}
    ENDIF
    
    IF !IsArray( aStruct )
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
    
    IF lNew == NIL
        lNew    := .T.
        lKeep   := .F.
    ELSE
        lKeep   := .T.
    ENDIF
    
    IF lJustOpen == NIL
        lJustOpen := .F.
    ENDIF
    
    IF IsNil(cAlias)
        cAlias := ""
    ENDIF
    
    IF IsNil(cDelim)
        cDelim := ""
    ENDIF
    LOCAL oDriver := cRddName AS OBJECT
    if oDriver == NULL_OBJECT
        oDriver := RuntimeState.DefaultRDD
    ENDIF
    IF oDriver IS STRING
        lRetCode := VoDbCreate(cName, aStruct, (STRING) oDriver, lNew, cAlias, cDelim, lKeep, lJustOpen)
    ELSEIF oDriver IS System.Type
        lRetCode := VoDbCreate(cName, aStruct, (Type) oDriver, lNew, cAlias, cDelim, lKeep, lJustOpen )
    ELSE
        THROW Error.DataTypeError( __FUNCTION__, nameof(cRddName ), 3, { cRddName  } )
    ENDIF
    RETURN lRetCode
    
    
    /// <summary>Create an index file and add an order to it.</summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION DbCreateIndex(cName, cExpr, cobExpr, lUnique) AS LOGIC CLIPPER
    RETURN OrdCreate(cName, NIL, cExpr, cobExpr, lUnique)
    
    
    
/// <summary>Create or replace an order in an index file.</summary>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION DbCreateOrder  (uOrder, cName, cExpr, cobExpr, lUnique) AS LOGIC CLIPPER
    RETURN OrdCreate(cName, uOrder, cExpr, cobExpr, lUnique)
    
    
    /// <summary>Mark the current record for deletion.</summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION DbDelete () AS LOGIC STRICT
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Delete() )
    
    
    /// <summary>Remove an order from an open index file.</summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION DbDeleteOrder(uOrder, cOrdBag) AS LOGIC CLIPPER
    LOCAL lRet   AS LOGIC
    
    lRet := TRUE
    
    IF IsNumeric(uOrder)
        lRet := VoDb.OrderInfo(DBOI_NAME,"",uOrder, REF uOrder)
    ENDIF
    
    RETURN OrdDestroy(uOrder, cOrdBag)
    
    
    
/// <inheritdoc cref="M:XSharp.CoreDb.Eval(XSharp.ICodeBlock,XSharp.ICodeBlock,XSharp.ICodeBlock,System.Object,System.Object,System.Boolean)"  />
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.Eval(XSharp.ICodeBlock,XSharp.ICodeBlock,XSharp.ICodeBlock,System.Object,System.Object,System.Boolean)"/>
/// <br/> <note type="tip">The difference between VoDbEval and CoreDb.Eval is that DbEval takes USUAL parameters and has optional parameters.</note></remarks>
/// <seealso cref="M:XSharp.CoreDb.Eval(XSharp.ICodeBlock,XSharp.ICodeBlock,XSharp.ICodeBlock,System.Object,System.Object,System.Boolean)"  />
FUNCTION DbEval(uBlock, uCobFor, uCobWhile, nNext, nRecno, lRest) AS LOGIC CLIPPER
    IF IsNil(lRest)
        lRest := .F.
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Eval(VoDb.ValidBlock(uBlock), VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), nNext, nRecno, lRest) )
  
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbFieldInfo(nOrdinal, nPos, xNewVal) AS USUAL CLIPPER
    _DbThrowErrorOnFailure(__FUNCTION__, !VoDb.FieldInfo(nOrdinal, nPos, REF xNewVal))
    RETURN xNewVal
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbGoto(uRecId) AS LOGIC CLIPPER
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Goto(uRecId) )
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbInfo(nOrdinal, xNewVal) AS USUAL CLIPPER
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info(nOrdinal, REF xNewVal))
    RETURN xNewVal
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbLocate(uCobFor, uCobWhile, nNext, uRecId, lRest ) AS LOGIC CLIPPER

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
    lRetCode := _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Locate(VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), nNext, uRecId, lRest))
    IF lRetCode
        lRetCode := VoDb.Found()
    ENDIF
    
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbOrderInfo(nOrdinal,cBagName, uOrder, xNewVal) AS USUAL CLIPPER
    LOCAL lKeyVal  := FALSE  AS LOGIC
    
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
    VoDb.OrderInfo(nOrdinal, cBagName, uOrder, REF xNewVal)
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
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRecordInfo(nOrdinal, uRecId, xNewVal) AS USUAL CLIPPER
    VoDb.RecordInfo(nOrdinal, uRecId, REF xNewVal)
    RETURN xNewVal
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRLock(uRecord) AS USUAL CLIPPER
    RETURN VoDb.Rlock(uRecord)
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRLockList() AS ARRAY STRICT

    LOCAL lockList          AS DWORD[]
    LOCAL aLockList := {}   AS ARRAY
    LOCAL i                 AS DWORD
    LOCAL uRecords  := NIL AS USUAL
    LOCAL nRecords          AS DWORD
    nRecords := 0
    
    IF _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info(DBI_LOCKCOUNT, REF uRecords))
        lockList := (DWORD[]) DbInfo(DBI_GETLOCKARRAY)
        nRecords := (DWORD) uRecords
        FOR i := 1 TO nRecords
            AAdd(aLockList, lockList[i])
        NEXT
    ENDIF
    
    RETURN aLockList
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRSelect(nPos) AS DWORD CLIPPER

    DEFAULT( REF nPos, 0)
    
    RETURN VoDb.RSelect(nPos)
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRUnLock(uRecId) AS LOGIC CLIPPER
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Unlock(uRecId) )
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSelect(nNew) AS DWORD CLIPPER

    LOCAL nOld := 0 AS DWORD
    
    DEFAULT( REF nNew, 0)
    
    VoDb.Select(nNew, REF nOld)
    
    RETURN nOld
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns> 
FUNCTION DbSelectArea(xValue) AS LOGIC CLIPPER
    LOCAL sSelect   AS SHORT
    sSelect := _SELECT(xValue)
    _DbThrowErrorOnFailure(__FUNCTION__, sSelect != 0)
    RETURN (sSelect > 0)
    
    
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSetSelect(nSelect) AS DWORD CLIPPER

    DEFAULT( REF  nSelect, 0)
    
    RETURN (DWORD) VoDb.SetSelect(nSelect)
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSymSelect(sAlias)  AS DWORD CLIPPER
    DEFAULT( REF  sAlias, Alias0Sym())
    RETURN (DWORD) VoDb.SymSelect(sAlias)
    
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRelation(wPos)  AS STRING CLIPPER
    LOCAL cRelText  := "" AS STRING
    DEFAULT(  REF wPos, 1)
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Relation(wPos, REF cRelText))
    RETURN cRelText
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSetDriver(cDriver) AS STRING CLIPPER
    IF IsString(cDriver)
        RETURN RddSetDefault(cDriver)
    ENDIF
    RETURN RddSetDefault()
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSetFilter(cbFilter, cFilter) AS LOGIC CLIPPER
    IF IsNil(cFilter)
        cFilter := "UNKNOWN"
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.SetFilter(cbFilter, cFilter) )
    
    
    
    *----------------------------------------------------------------------------
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSetRelation  (xAlias, uCobKey, cKey) AS LOGIC CLIPPER

    LOCAL nSelect   AS DWORD
    LOCAL cAlias    AS STRING
    LOCAL xType     AS DWORD
    
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

    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.SetRelation(cAlias, uCobKey, cKey) )
   

    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSkip (nRecords) AS LOGIC CLIPPER
    IF IsNil(nRecords)
        nRecords := 1
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Skip(nRecords) )
    
    
    
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbUseArea (lNew, xDriver, cName, cAlias, lShare, lReadOnly, aStru, cDelim,aHidden ) AS LOGIC CLIPPER
    LOCAL lRetCode        AS LOGIC
    LOCAL rddList         AS _RddList
    LOCAL nTries          AS LONG
    LOCAL aRdds           AS ARRAY
    
    DEFAULT( REF lNew, .F.)
    DEFAULT( REF cAlias, "")
    DEFAULT( REF lShare, !SetExclusive())
    DEFAULT( REF lReadOnly, .F.)
    DEFAULT( REF cDelim, "")
    nTries := 1
    aRdds   := VoDb.RDDList(xDriver, aHidden)
    rddList := VoDb.AllocRddList(aRdds)
    DO WHILE .T.
    
        IF !Empty(aStru)
            lRetCode := DbCreate ( cName, aStru, aRdds, lNew,cAlias, cDelim, .T.)
        ELSE
            lRetCode := VoDb.UseArea(lNew, rddList, cName, cAlias, lShare, lReadOnly)
        ENDIF
        IF lRetCode
            EXIT
        ELSE
            IF ( (INT) DoError(__FUNCTION__, nTries) != E_RETRY )
                EXIT
            ENDIF
            nTries := nTries + 1
        ENDIF
    ENDDO
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION FieldPut (wPos AS USUAL, xValue  AS USUAL) AS USUAL 

    LOCAL xRetVal := NIL AS USUAL

    IF _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldPut(wPos, xValue) )
        xRetVal := xValue
    ENDIF
    RETURN xRetVal
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION FieldGet(wPos) AS USUAL CLIPPER
    LOCAL xRetVal := NIL AS USUAL
    DEFAULT( REF wPos, 1)
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldGet(wPos, REF xRetVal))
    RETURN xRetVal
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION LUpdate()  AS DATE STRICT
    RETURN DbInfo(DBI_LASTUPDATE)
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
    
FUNCTION RddCount(nType) AS DWORD CLIPPER
    RETURN VoDb.RddCount()
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION RddInfo(nOrdinal, xNewVal) AS USUAL CLIPPER
    VoDb.RDDInfo(nOrdinal, REF xNewVal)
    RETURN xNewVal
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION RddList (nType) AS ARRAY CLIPPER
    LOCAL aRddList := {}    AS ARRAY
    IF VoDb.RddCount() > 0
        VAR aNames := VoDb.RddList()
        FOREACH name AS STRING IN aNames
            AAdd( aRddList, name)
        NEXT
    ENDIF
    RETURN aRddList
    
    
FUNCTION DbMemoExt      (cDriver) AS STRING CLIPPER
    IF IsNil(cDriver)
        cDriver := ""
    ENDIF
    
    RETURN VoDb.MemoExt(cDriver)
    
    
    
FUNCTION RddVersion     (nParm) AS USUAL CLIPPER
    IF !IsNumeric(nParm)
        nParm := 0
    ENDIF
    RETURN DbInfo(DBI_RDD_VERSION, nParm)
    
    
    
FUNCTION DbMemoField (xField AS USUAL)  AS USUAL
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
        n := 0
        FOR i := 1 TO nFields
            IF DbFieldInfo(i, DBS_TYPE) == "M"
                n := i
                EXIT
            ENDIF
        NEXT
    ENDIF
    
    xRet := DbInfo( DBI_MEMOFIELD, n )
    
    RETURN xRet
    
    
/// <exclude/>
FUNCTION _DbCreate(cFile1, cFile2, cDriver,lNew, cAlias)      AS LOGIC CLIPPER

    LOCAL aStruct       AS ARRAY
    LOCAL i,n           AS INT
    LOCAL nSelect       AS INT
    LOCAL aField        AS ARRAY
    
    FIELD field_name, field_type, field_len, field_dec
    
    nSelect := 0
    
    DEFAULT( REF lNew, .F.)
    
    IF ( Used() .AND. !lNew )
        VoDb.CloseArea()
    ENDIF
    
    TRY
        IF ( Empty(cFile2) )
            DbCreate(cFile1,                ;
            { {"FIELD_NAME", "C", 10, 0},   ;
            {"FIELD_TYPE", "C", 1, 0},      ;
            {"FIELD_LEN", "N", 3, 0},       ;
            {"FIELD_DEC", "N", 3, 0} },     ;
            cDriver,                        ;
            .F.,                            ;
            cAlias)
        ELSE
            DbUseArea(lNew, cDriver, cFile2)
            aStruct := {}
            n := VoDb.LastRec()
            VoDb.GoTop()
            DO WHILE !VoDb.EOF()
                aField := ArrayNew(4)
                aField[DBS_NAME] := AllTrim(fieldGet(1))
                aField[DBS_TYPE] := AllTrim(fieldGet(2))
                aField[DBS_LEN ] := fieldGet(3)
                aField[DBS_DEC ] := fieldGet(4)
                AAdd( aStruct, aField )
                VoDb.Skip(1)
            ENDDO
            
            VoDb.CloseArea()
            
            IF lNew
                VoDb.SetSelect(nSelect)
            ENDIF
            
            FOR i := 1 TO n
                aField := aStruct[i]
                IF aField[ DBS_TYPE] == "C" .AND. aField[DBS_DEC] != 0
                    aField[DBS_LEN] += aField[DBS_DEC] * 256
                ENDIF
            NEXT
            DbCreate(cFile1, aStruct, cDriver, lNew, cAlias )
        ENDIF
        
    CATCH e AS RddError
        VoDb.CloseArea()
        e:FuncSym := __FUNCTION__
        THROW e
    END TRY
    
    RETURN ( Used() )
    
    
    
    
/// <summary>Fill arrays with the structure of the current workarea.</summary>    
FUNCTION AFields(aNames, aTypes, aLens, aDecs)  AS DWORD CLIPPER
    LOCAL aStruct           AS ARRAY
    LOCAL siCount           AS DWORD
    LOCAL si                AS DWORD
    LOCAL lNamesOk  := .F.  AS LOGIC
    LOCAL lTypesOk  := .F.  AS LOGIC
    LOCAL lLensOk   := .F.  AS LOGIC
    LOCAL lDecsOk   := .F.  AS LOGIC
    aStruct := DbStruct() 
    IF (Empty(aStruct))
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
    
    
/// <exclude/>
FUNCTION __DbCopyStruct(cFile AS STRING, aFields AS ARRAY) AS LOGIC STRICT

    RETURN DBCREATE(cFile, VoDb.FieldList(DbStruct(), aFields, NULL_ARRAY) )
    

/// <summary>Copy the field definitions in a workarea to a structure-extended file as data..</summary>    
FUNCTION DbCopyXStruct(cFile AS STRING) AS LOGIC STRICT

    LOCAL siSaveSel,n,i AS DWORD
    LOCAL aStruct       AS ARRAY
    LOCAL lRetCode  := FALSE AS LOGIC
    
    FIELD field_name, field_type, field_len, field_dec
    
    TRY
        IF !Used()
            THROW VoDb.DBCMDError(__FUNCTION__)
        ENDIF
        
        aStruct := DbStruct()
        
        n := Len(aStruct)
        siSaveSel := 0
        VoDb.Select(0, REF siSaveSel)
        
        _DbCreate(cFile)
        
        FOR i := 1 TO n
        
            IF aStruct[i, DBS_TYPE] == "C" .AND. aStruct[i, DBS_LEN] > 255
                aStruct[i, DBS_DEC] := SHORT(aStruct[i, DBS_LEN] / 256)
                aStruct[i, DBS_LEN] := aStruct[i, DBS_LEN] % 256
            ENDIF
            
            lRetCode := DBAPPEND()
            
            FieldPutSym(#field_name ,aStruct[i, DBS_NAME])
            FieldPutSym(#field_type ,aStruct[i, DBS_TYPE])
            FieldPutSym(#field_len  ,aStruct[i, DBS_LEN])
            FieldPutSym(#field_dec  , aStruct[i, DBS_DEC])
            
        NEXT
        
        IF (VoDb.GetSelect() <> siSaveSel)
            DbCloseArea()
            VoDb.SetSelect(INT(siSaveSel))
        ENDIF
        
    CATCH e AS RddError
        e:FuncSym := __FUNCTION__
        THROW e
        lRetCode := .F.
    END TRY
    
    RETURN (lRetCode)
    
    /// <summary>
    /// </summary>
    /// <param name="uSelect"></param>
    /// <param name="symField"></param>
    /// <returns>
    /// </returns>
FUNCTION DbStruct() AS ARRAY PASCAL

    LOCAL aStruct   AS ARRAY
    LOCAL nFCount   AS DWORD
    LOCAL nProps    AS DWORD
    LOCAL i,j       AS DWORD
    LOCAL aField    AS ARRAY
    
    aStruct := {}
    nFCount := FCount()
    
    IF !Used()
        THROW VoDb.DbCmdError(__FUNCTION__)
    ELSE
        FOR i := 1 UPTO nFCount
            aField := {}
            
            LOCAL xNewVal := NIL AS USUAL
            _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldInfo(DBS_PROPERTIES, i, REF xNewVal))
            nProps:= (DWORD)  xNewVal
            FOR j := 1 UPTO nProps
                _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldInfo(j, i, REF xNewVal))
                AAdd(aField, xNewVal)
            NEXT
            
            AAdd(aStruct, aField)
            
        NEXT
    ENDIF
    
    RETURN aStruct
    
    
    /// <summary>
    /// Return and optionally change the setting that determines whether to use the High Performance (HP) locking schema for newly created .NTX files.
    /// </summary>
    /// <param name="lSet"></param>
    /// <returns>
    /// </returns>
FUNCTION IndexHPLock(lSet AS USUAL) AS LOGIC
    LOCAL lOld AS LOGIC
    lOld := RuntimeState.HPLocking
    IF IsLogic(lSet)
        RuntimeState.HPLocking := (LOGIC) lSet
    ENDIF
    RETURN lOld
    
    /// <summary>
    /// Return and optionally change the setting that determines whether to use the new locking offset of -1 (0xFFFFFFFF) for .NTX files.
    /// </summary>
    /// <param name="lSet"></param>
    /// <returns>
    /// </returns>
FUNCTION NewIndexLock(lSet AS USUAL) AS LOGIC
    LOCAL lOld AS LOGIC
    lOld := RuntimeState.NewIndexLock
    IF IsLogic(lSet)
        RuntimeState.NewIndexLock := (LOGIC) lSet
    ENDIF
    RETURN lOld
    
    
    
    /// <summary>
    /// Return the setting that determines whether to use the new locking offset of -1 (0xFFFFFFFF) for .NTX files.
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION NewLocks() AS LOGIC
    RETURN RuntimeState.NewIndexLock
    

/// <exclude />
FUNCTION __RDDList(cRddName AS STRING) AS ARRAY
    RETURN VoDb.RDDList(cRddName, {})
/// <exclude />
FUNCTION __RDDList(cRddName AS STRING, aHidden AS ARRAY) AS ARRAY
    RETURN VoDb.RDDList(cRddName, aHidden)

/// <exclude />
FUNCTION _AllocFieldNames(aStruct AS ARRAY) AS _FieldNames
    RETURN VoDb.AllocFieldNames(aStruct)

/// <exclude />    
FUNCTION _FreeFieldNames(aNames AS _FieldNames) AS VOID
    RETURN 

/// <exclude />    
FUNCTION __allocNames(aStruct AS ARRAY) AS _FieldNames
    RETURN VoDb.AllocFieldNames(aStruct)

/// <exclude />
FUNCTION __TargetFields(cAlias AS STRING, aFields AS ARRAY, list REF _JoinList ) AS ARRAY
    RETURN VoDb.TargetFields(cAlias, aFields, REF list)
