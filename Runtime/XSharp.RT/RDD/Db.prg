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
/// <param name="xValue">A value that identifies the work area.
/// This can be the number of the work area or its alias, specified either as a symbol or a string.
/// If xValue is not specified, the current work area number is returned.
/// Therefore, Select() is the same as DBGetSelect().</param>
/// <returns>A number from 0 to 4096.  0 is returned if xValue does not identify a valid work area or does not correspond to a valid alias.</returns>
FUNCTION Select(xValue) AS USUAL CLIPPER
    LOCAL sSelect   AS DWORD
	LOCAL sCurrent  AS DWORD
	sCurrent := VODBGetSelect()
	sSelect := _SELECT(xValue)
	VODBSetSelect(INT(sCurrent))
	RETURN sSelect



/// <summary>Determine the number of a work area.</summary>
/// <param name="cValue">The aliasof a workarea.  If cValue is empty then  the current work area number is returned.  </param>
/// <returns>A number from 0 to 4096.  0 is returned if cValue does not identify a valid work area or does not correspond to a valid alias.</returns>

FUNCTION _SelectString(cValue AS STRING) AS DWORD
    LOCAL nSelect := 0 AS DWORD
    cValue := AllTrim(cValue)
    IF SLen(cValue) = 1
        nSelect := Val(cValue)
		VAR nAsc := Asc( Upper(cValue) )
		IF nAsc > 64 .AND. nAsc < 75
			nSelect := nAsc - 64
		ENDIF
    ENDIF
        
    IF nSelect > 0 .OR. "0" == cValue
        nSelect := VoDb.SetSelect((INT) nSelect)
    ELSE
        nSelect := (DWORD) VoDb.SymSelect(cValue)
    ENDIF
    RETURN nSelect
    
/// <summary>Determine the number of a work area.</summary>
/// <param name="xValue">A value that identifies the work area.  This can be the number of the work area or its alias, specified either as a symbol or a string.  If xValue is not specified, the current work area number is returned.</param>
/// <returns>A number from 0 to 4096.  0 is returned if xValue does not identify a valid work area or does not correspond to a valid alias.</returns>
FUNCTION _Select(xValue) AS USUAL CLIPPER
    LOCAL nSelect           AS DWORD
    LOCAL xType             AS DWORD
    
    IF xValue:IsNil
        RETURN  (INT) VoDb.GetSelect() 
    ENDIF
    xType := UsualType(xValue)
    SWITCH xType
    CASE SYMBOL
        nSelect := (DWORD) VoDb.SymSelect((SYMBOL) xValue)
    CASE STRING
        nSelect := _SelectString(xValue)
    CASE LONG
    CASE FLOAT
        nSelect := VoDb.SetSelect(xValue)
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
            oCb := {|x| IIF(x:IsNil, __FieldGetNum(nPos), __FieldSetNum(nPos, x))}
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
    /// <param name="cFieldName">The name of the field.</param>
    /// <param name="uArea">The work area number where the field resides. This can be a string, number or symbol</param>
    /// <returns>
    /// </returns>
FUNCTION FieldWBlock(cFieldName AS STRING,uArea AS USUAL) AS CODEBLOCK
    RETURN {|x| IIF( x:IsNil, __FieldGetWa(uArea, cFieldName), __FieldSetWa(uArea, cFieldName, x)) }
    
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
    LOCAL nArea AS DWORD
    nArea := SELECT(uSelect )
    RETURN FieldPutArea(nArea, symField, uValue)
    
    
    *----------------------------------------------------------------------------
    
    /// <summary>
    /// </summary>
    /// <param name="nSelect"></param>
    /// <returns>
    /// </returns>
FUNCTION Alias(nSelect) AS STRING CLIPPER
    IF nSelect:IsNil
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
    
    IF lReleaseLocks:IsNil
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
    
    IF ! aStruct:IsArray
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
    
    LOCAL oDriver := cRddName AS OBJECT
    IF oDriver == NULL_OBJECT
        oDriver := RuntimeState.DefaultRDD
    ENDIF
    IF oDriver IS STRING
        lRetCode := VoDbCreate(cName, aStruct, (STRING) oDriver, lNew, cAlias, cDelim, lKeep, lJustOpen)
    ELSEIF oDriver IS System.Type
        lRetCode := VoDbCreate(cName, aStruct, (Type) oDriver, lNew, cAlias, cDelim, lKeep, lJustOpen )
    ELSE
        THROW Error.DataTypeError( __FUNCTION__, nameof(cRddName ), 3, { cRddName  } )
    ENDIF
    IF ! lRetCode .and. RuntimeState.LastRDDError != NULL
        THROW RuntimeState.LastRDDError
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
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbDelete" />
/// <remarks />
FUNCTION DbDelete () AS LOGIC STRICT
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Delete() )
    
    
    /// <summary>Remove an order from an open index file.</summary>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION DbDeleteOrder(uOrder, cOrdBag) AS LOGIC CLIPPER
    IF uOrder:IsNumeric
        VoDb.OrderInfo(DBOI_NAME,"",uOrder, REF uOrder)
    ENDIF
    
    RETURN OrdDestroy(uOrder, cOrdBag)
    
    
    
/// <inheritdoc cref="M:XSharp.CoreDb.Eval(XSharp.ICodeblock,XSharp.ICodeblock,XSharp.ICodeblock,System.Object,System.Object,System.Boolean)"  />
/// <remarks> <inheritdoc cref="M:XSharp.CoreDb.Eval(XSharp.ICodeblock,XSharp.ICodeblock,XSharp.ICodeblock,System.Object,System.Object,System.Boolean)"/>
/// <br/> <note type="tip">The difference between VoDbEval and CoreDb.Eval is that DbEval takes USUAL parameters and has optional parameters.</note></remarks>
/// <seealso cref="M:XSharp.CoreDb.Eval(XSharp.ICodeblock,XSharp.ICodeblock,XSharp.ICodeblock,System.Object,System.Object,System.Boolean)"  />
FUNCTION DbEval(uBlock, uCobFor, uCobWhile, nNext, nRecno, lRest) AS LOGIC CLIPPER
    IF lRest:IsNil
        lRest := .F.
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Eval(VoDb.ValidBlock(uBlock), VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), nNext, nRecno, lRest) )
  
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbFieldInfo(System.UInt32,System.UInt32,XSharp.__Usual@)" />
/// <remarks />
FUNCTION DbFieldInfo(nOrdinal, nFldPos, xNewVal) AS USUAL CLIPPER
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldInfo(nOrdinal, nFldPos, REF xNewVal))
    RETURN xNewVal
    
    
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbGoto(XSharp.__Usual)" />
/// <remarks />
FUNCTION DbGoto(uRecId) AS LOGIC CLIPPER
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Goto(uRecId) )
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbInfo(nOrdinal, xNewVal) AS USUAL CLIPPER
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info(nOrdinal, REF xNewVal))
    RETURN xNewVal
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbLocate(XSharp.__Usual,XSharp.__Usual,System.Int32,XSharp.__Usual,System.Boolean)" />
/// <remarks />
FUNCTION DbLocate(uCobFor, uCobWhile, nNext, uRecId, lRest ) AS LOGIC CLIPPER

    LOCAL lRetCode  AS LOGIC
    
    IF lRest:IsNil
        lRest := .F.
    ENDIF
    
    IF uCobWhile:IsNil
        uCobWhile := {|| .T. }
    ELSE
        lRest := .T.
    ENDIF
    
    IF nNext:IsNil
        nNext := 0
    ENDIF
    lRetCode := _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Locate(VoDb.ValidBlock(uCobFor), VoDb.ValidBlock(uCobWhile), nNext, uRecId, lRest))
    IF lRetCode
        lRetCode := VoDb.Found()
    ENDIF
    
    RETURN lRetCode
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbOrderInfo(System.UInt32,System.String,XSharp.__Usual,XSharp.__Usual)" />
/// <remarks />
FUNCTION DbOrderInfo(nOrdinal,cBagName, uOrder, xNewVal) AS USUAL CLIPPER
    LOCAL lKeyVal  := FALSE  AS LOGIC
    
    IF !cBagName:IsString
        cBagName := ""
    ENDIF
    
    IF uOrder:IsString
        IF SLen(uOrder) == 0
            uOrder := NIL
        ENDIF
    ENDIF
    
    IF nOrdinal == DBOI_KEYVAL
        lKeyVal  := .T.
        nOrdinal := DBOI_EXPRESSION
    ENDIF
    VoDb.OrderInfo(nOrdinal, cBagName, uOrder, REF xNewVal)
    IF lKeyVal
        IF xNewVal:IsString
            IF SLen(xNewVal) == 0
                xNewVal := NIL
            ELSE
                xNewVal := &(xNewVal)
            ENDIF
        ENDIF
    ENDIF
    
    RETURN xNewVal
    
    
    
    
    /// <summary>Retrieve record state information for the current record or a specified record</summary>
    /// <returns>State of the record</returns>
    /// <param name="nOrdinal">This must match one of the values from the DbRecordInfo Enum</param>
    /// <param name="uRecId">Some of the DbRecordInfo enum values require a record number</param>
    /// <param name="xNewVal">Some of the DbRecordInfo enum values require a new value</param>
 
    /// <seealso cref='T:XSharp.RDD.Enums.DbRecordInfo'>DbRecordInfo ENUM</seealso>
FUNCTION DbRecordInfo(nOrdinal, uRecId, xNewVal) AS USUAL CLIPPER
    VoDb.RecordInfo(nOrdinal, uRecId, REF xNewVal)
    RETURN xNewVal
    
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbRlock(XSharp.__Usual)" />
/// <remarks />
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
    
    
    
    
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbSetSelect(System.Int32)" />
/// <remarks />
FUNCTION DbSetSelect(nSelect) AS DWORD CLIPPER

    DEFAULT( REF  nSelect, 0)
    
    RETURN (DWORD) VoDb.SetSelect(nSelect)
    
    
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbSymSelect(XSharp.__Symbol)" />
/// <remarks />
FUNCTION DbSymSelect(sAlias)  AS DWORD CLIPPER
    IF sAlias:IsNil
        sAlias := Alias0Sym()
    ENDIF
    EnForceType(sAlias, SYMBOL)
    RETURN (DWORD) VoDb.SymSelect(sAlias)
    
    
    
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbRelation(System.UInt32,System.String@)" />
/// <remarks />
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
    IF cDriver:IsString
        RETURN RddSetDefault(cDriver)
    ENDIF
    RETURN RddSetDefault()
    
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbSetFilter(XSharp.__Usual,System.String)" />
/// <remarks />
FUNCTION DbSetFilter(cbFilter, cFilter) AS LOGIC CLIPPER
    IF cFilter:IsNil
        cFilter := "UNKNOWN"
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.SetFilter(cbFilter, cFilter) )
    
    
    
    *----------------------------------------------------------------------------
    /// <summary>Relate a specified work area to the current work area.</summary>
    /// <param name="xAlias">The name or workarea number of the child work area.</param>
    /// <param name="uCobKey">A code block that expresses the relational expression.</param>
    /// <param name="cKey">An optional character value that expresses the relational expression in textual form.</param>
    /// <param name="cName">An optional name for the relation. Defaults to ParentName + "_" + ChildName.</param>
    /// <returns>TRUE if successful; otherwise, FALSE.</returns>
FUNCTION DbSetRelation  (xAlias, uCobKey, cKey, cName) AS LOGIC CLIPPER

    LOCAL nSelect   AS DWORD
    LOCAL cAlias    AS STRING
    
    DEFAULT(REF cName, "")
    
    IF xAlias:IsString
        nSelect := Val(xAlias)
        
        IF nSelect = 0
            cAlias := xAlias
        ELSE
            cAlias := ALIAS(nSelect)
        ENDIF
        
    ELSE
        cAlias := ALIAS(xAlias)
    ENDIF

    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.SetRelation(cAlias, uCobKey, cKey, cName) )
   

    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbSkip(System.Int32)" />
/// <remarks />
FUNCTION DbSkip (nRecords) AS LOGIC CLIPPER
    IF nRecords:IsNil
        nRecords := 1
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Skip(nRecords) )
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbUseArea(System.Boolean,System.String,System.String,System.String,System.Boolean,System.Boolean)" />
/// <param name="aStru">(Optional) An array containing field descriptions in the format returned by DBStruct().<note type="tip">This argument does not apply to DBF files.</note></param>
/// <param name="cDelim">(Optional) The delimiter for fields within a delimited database file.<note type="tip">This argument does not apply to DBF files.</note></param>
/// <param name="aHidden">(Optional) A one-dimensional array with the names of RDDs from which the main RDD inherits special functionality.</param>
/// <remarks />
/// <seealso cref="O:XSharp.RT.Functions.VoDbUseArea" />
/// <seealso cref="O:XSharp.CoreDb.UseArea" />

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
    
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbFieldPut(System.UInt32,XSharp.__Usual)" />
/// <remarks />
FUNCTION FieldPut (nPos AS USUAL, xValue  AS USUAL) AS USUAL 

    LOCAL xRetVal := NIL AS USUAL
    IF _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldPut(nPos, xValue) )
        xRetVal := xValue
    ENDIF
    RETURN xRetVal
    
/// <inheritdoc cref="M:XSharp.RT.Functions.VoDbFieldGet(System.UInt32,XSharp.__Usual@)" />
/// <remarks />
FUNCTION FieldGet(nPos) AS USUAL CLIPPER
    LOCAL xRetVal := NIL AS USUAL
    DEFAULT( REF nPos, 1)
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldGet(nPos, REF xRetVal))
    RETURN xRetVal

/// <summary>Read an array of bytes direct from the workarea buffer.</summary>
/// <remarks>This will only work for DBF based workareas (not for Advantage workareas)</remarks>
FUNCTION FieldGetBytes(nPos ) AS BYTE[] CLIPPER
    LOCAL bRetVal := NULL AS BYTE[]
     DEFAULT( REF nPos, 1)
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldGetBytes(nPos, REF bRetVal))
    RETURN bRetVal


/// <summary>Write an array of bytes direct to the workarea buffer.</summary>
/// <remarks>This will only work for DBF based workareas (not for Advantage workareas)</remarks>
FUNCTION FieldPutBytes(nPos AS USUAL, aBytes as BYTE[]) AS LOGIC
     DEFAULT( REF nPos, 1)
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldPutBytes(nPos, aBytes))
    RETURN TRUE


    
FUNCTION FieldGetArea(workarea AS DWORD, symField AS SYMBOL) AS USUAL 
    LOCAL oldArea := VoDbGetSelect() AS DWORD
    LOCAL result := NIL  AS USUAL
    LOCAL dwPos AS DWORD
    TRY
        VoDbSetSelect( (INT) workarea)
        dwPos := FieldPosSym(symField)
        IF dwPos == 0
           VoDbSetSelect( (INT) oldArea)
           THROW Error.VODBError(EG_ARG, EDB_FIELDNAME, __FUNCTION__, {symField})
        ELSE
            VODBFieldGet( dwPos, REF result )
        ENDIF
    FINALLY
        VoDbSetSelect( (INT) oldArea)
    END TRY
    RETURN result


FUNCTION FieldPutArea(workarea AS DWORD, symField AS SYMBOL, uNewValue AS USUAL) AS USUAL 
    LOCAL oldArea := VoDbGetSelect() AS DWORD
    LOCAL dwPos AS DWORD
    TRY
        VoDbSetSelect( (INT) workarea)
        dwPos := FieldPosSym(symField)
        IF dwPos == 0
           VoDbSetSelect( (INT) oldArea)
           THROW Error.VODBError(EG_ARG, EDB_FIELDNAME, __FUNCTION__, {symField})
        ELSE
            VODBFieldPut( dwPos, uNewValue)
        ENDIF
    FINALLY
        VoDbSetSelect( (INT) oldArea)
    END TRY
    RETURN uNewValue
    
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
    
FUNCTION RddCount() AS DWORD CLIPPER
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
FUNCTION RddList () AS ARRAY CLIPPER
    LOCAL aRddList := {}    AS ARRAY
    IF VoDb.RddCount() > 0
        VAR aNames := VoDb.RddList()
        FOREACH name AS STRING IN aNames
            AAdd( aRddList, name)
        NEXT
    ENDIF
    RETURN aRddList
    
    
FUNCTION DbMemoExt      (cDriver) AS STRING CLIPPER
    RETURN VoDb.MemoExt(cDriver)
    
    
    
FUNCTION RddVersion     (nParm) AS USUAL CLIPPER
    IF !nParm:IsNumeric
        nParm := 0
    ENDIF
    RETURN DbInfo(DBI_RDD_VERSION, nParm)
    
    
    
FUNCTION DbMemoField (xField AS USUAL)  AS USUAL
    LOCAL n,i        AS DWORD
    LOCAL xRet       AS USUAL
    LOCAL nFields    AS DWORD
    
    IF xField:IsNumeric
        n := xField
    ELSEIF xField:IsSymbol
        n := FieldPosSym(xField)
    ELSEIF xField:IsString
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
    
    
/// <summary>Create an empty database file with field definitions from another database file.</summary>
/// <param name="cFile">The name of the target database file, including an optional drive, directory, and extension.</param>
/// <param name="acStruct">A one-dimensional array of field names to copy to the new database file.  The default is all fields.</param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
/// <remarks>
/// If <paramref name="cFile"/> does not exist, it is created.  If it exists, this function attempts to open the file in
/// exclusive mode and, if successful, the file is overwritten without warning or error.
/// If access is denied because, for example, another process is using the file, NetErr() is set to TRUE.
/// DbCopyStruct() creates the specified file in ANSI or OEM character set format, based ON the SetAnsi() setting.
/// (for more information, refer to the SetAnsi() function.)
/// </remarks>
/// <seealso cref='M:XSharp.Core.Functions.SetAnsi(System.Boolean)'>SetAnsi</seealso>
/// <seealso cref='M:XSharp.RT.Functions.DbCopyXStruct(System.String)'>DbCopyXStruct</seealso>
FUNCTION DbCopyStruct(cFile AS STRING, acStruct := NULL_ARRAY AS ARRAY) AS LOGIC STRICT
    RETURN DBCREATE(cFile, VoDb.FieldList(DbStruct(), acStruct, NULL_ARRAY) )



/// <summary>Copy the field definitions in a workarea to a structure-extended file as data..</summary>
/// <param name="cFile">The name of the target database file, including an optional drive, directory, and extension.</param>
/// <returns>TRUE if successful; otherwise, FALSE.</returns>
/// <remarks>
/// If <paramref name="cFile"/> does not exist, it is created.  If it exists, this function attempts to open the file in
/// exclusive mode and, if successful, the file is overwritten without warning or error.
/// If access is denied because, for example, another process is using the file, NetErr() is set to TRUE.
/// DbCopyXStruct() creates the specified file in ANSI or OEM character set format, based ON the SetAnsi() setting.
/// (for more information, refer to the SetAnsi() function.)
/// </remarks>
/// <seealso cref='M:XSharp.Core.Functions.SetAnsi(System.Boolean)'>SetAnsi</seealso>
/// <seealso cref='M:XSharp.RT.Functions.DbCopyStruct(System.String,XSharp.__Array)'>DbCopyStruct</seealso>
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
    END TRY
    
    RETURN (lRetCode)
    
    /// <summary>Create an array containing the structure of a database file.</summary>
    /// <returns>The structure of the database file in an array whose length is equal to the number of fields in the database file.  Each element of the array is a subarray containing information for one field.  The subarrays have the following format:
    ///      <para></para>
    ///        <list type="table">
    ///          <listheader>
    ///            <term>Constant</term>
    ///            <description>Attribute</description>
    ///          </listheader>
    ///          <item>
    ///            <term>DBS_NAME</term>
    ///            <description>cName</description>
    ///          </item>
    ///          <item>
    ///            <term>DBS_TYPE</term>
    ///            <description>cType</description>
    ///          </item>
    ///          <item>
    ///            <term>DBS_LEN</term>
    ///            <description>nLength</description>
    ///          </item>
    ///          <item>
    ///            <term>DBS_DEC</term>
    ///            <description>nDecimals</description>
    ///          </item>
    ///          <item>
    ///            <term>DBS_ALIAS</term>
    ///            <description>cAlias</description>
    ///          </item>
    ///        </list>
    ///        <para>If there is no database file in use in the work area, DBStruct() will generate a runtime error.</para>
    ///  </returns>
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
    IF lSet:IsLogic
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
    IF lSet:IsLogic
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
FUNCTION __TargetFields(cAlias AS STRING, aFields AS ARRAY, list OUT _JoinList ) AS ARRAY 
    RETURN VoDb.TargetFields(cAlias, aFields, OUT list)


/// <summary>Determine if the current record in the current workarea is empty.</summary>
/// <returns>TRUE when the record is empty. FALSE when there is no workarea open or when the record is not empty.</returns>
FUNCTION EmptyRecord() AS LOGIC
   LOCAL lRet    AS LOGIC
   LOCAL aRecord AS BYTE[]

   IF Used()
      aRecord := VODBRecordGet()
      lRet := TRUE
      FOREACH VAR b IN aRecord
         IF b != 32
            lRet := FALSE
            EXIT
         ENDIF
      NEXT
   ELSE
        lRet := FALSE
   ENDIF
   RETURN lRet

/// <summary>Determine if the specified field in the current workarea is empty.</summary>
/// <remarks>You can empty a string field by writing it with spaces and a date field by writing a NULL_DATE.
/// Numeric and Logical fields cannot be cleared once they contain data.</remarks>
/// <returns>TRUE when the field is empty. FALSE when there is no workarea open or when the field is not empty.</returns>
FUNCTION EmptyField( n AS DWORD ) AS LOGIC
   LOCAL aRecord AS BYTE[]
   LOCAL i       AS DWORD
   LOCAL lRet    AS LOGIC
   LOCAL nOffset AS DWORD
   LOCAL nEnd    AS DWORD
   LOCAL uLen AS USUAL
   IF Used() .AND. n > 0 .AND. n <= FCount()
        lRet := TRUE
        aRecord := VODBRecordGet()
        nOffset := 2        // skip the Deleted flag
        FOR i := 1 UPTO n - 1
            uLen := 0
            VoDbFieldInfo(DBS_LEN, i, REF uLen)
            nOffset += uLen
        NEXT
        uLen := 0
        VoDbFieldInfo(DBS_LEN, n, REF uLen)
        nEnd := uLen + nOffSet -1
      
        FOR i := nOffset UPTO nEnd
            IF aRecord[i] != 32
                lRet := FALSE
                EXIT
            ENDIF
        NEXT
    ELSE
        lRet := FALSE
    ENDIF
   RETURN lRet

