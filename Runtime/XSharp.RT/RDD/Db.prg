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



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/select/*" />
FUNCTION Select(uWorkArea) AS USUAL CLIPPER
    LOCAL sSelect   AS DWORD
	LOCAL sCurrent  AS DWORD
	sCurrent := VODBGetSelect()
	sSelect := _SELECT(uWorkArea)
	VODBSetSelect(INT(sCurrent))
	RETURN sSelect


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/select/*" />
FUNCTION _SelectString(uWorkArea AS STRING) AS DWORD
    LOCAL nSelect := 0 AS DWORD
    uWorkArea := AllTrim(uWorkArea)
    IF SLen(uWorkArea) = 1
        nSelect := Val(uWorkArea)
		VAR nAsc := Asc( Upper(uWorkArea) )
		IF nAsc > 64 .AND. nAsc < 75
			nSelect := nAsc - 64
		ENDIF
    ENDIF
        
    IF nSelect > 0 .OR. "0" == uWorkArea
        nSelect := VoDb.SetSelect((INT) nSelect)
    ELSE
        nSelect := (DWORD) VoDb.SymSelect(uWorkArea)
    ENDIF
    RETURN nSelect
    

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/select/*" />
FUNCTION _Select(uWorkArea) AS USUAL CLIPPER
    LOCAL nSelect           AS DWORD
    LOCAL xType             AS DWORD
     
    IF uWorkArea:IsNil 
        RETURN  (INT) VoDb.GetSelect() 
    ENDIF
    xType := UsualType(uWorkArea)
    SWITCH xType
    CASE SYMBOL
        nSelect := (DWORD) VoDb.SymSelect((SYMBOL) uWorkArea)
    CASE STRING
        nSelect := _SelectString(uWorkArea)
    CASE LONG
    CASE FLOAT
        nSelect := VoDb.SetSelect(uWorkArea)
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
    

    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldblock/*" />
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
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldblocksym/*" />
FUNCTION FieldBlockSym(symFieldName AS SYMBOL) AS CODEBLOCK
    RETURN FieldBlock(symFieldName)   

   
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldwblock/*" />
FUNCTION FieldWBlock(cFieldName AS STRING,dwWorkArea AS USUAL) AS CODEBLOCK
    RETURN {|x| IIF( x:IsNil, __FieldGetWa(dwWorkArea, cFieldName), __FieldSetWa(dwWorkArea, cFieldName, x)) }
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldwblocksym/*" />
FUNCTION FieldWBlockSym(symFieldname AS SYMBOL,dwWorkArea AS DWORD) AS CODEBLOCK
    RETURN FieldWBlock(symFieldname, dwWorkArea)
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldgetalias/*" />
FUNCTION FieldGetAlias(symAlias AS SYMBOL,symFieldName AS SYMBOL) AS USUAL
    RETURN fieldGetSelect(symAlias, symFieldName)
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldgetalias/*" />
FUNCTION FieldGetSelect(symAlias AS USUAL,symFieldName AS SYMBOL) AS USUAL
    LOCAL nArea := RuntimeState.CurrentWorkArea AS DWORD
    LOCAL uValue AS USUAL
    DbSelectArea(symAlias)
    uValue := FieldGetSym(symFieldName)
    VoDb.SetSelect((INT) nArea)
    RETURN uValue
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldgetsym/*" />
FUNCTION FieldGetSym(symField AS SYMBOL) AS USUAL
    LOCAL dwPos AS DWORD
    dwPos := FieldPosSym(symField)
    IF dwPos == 0
        THROW Error.VODBError( EG_ARG, EDB_FIELDNAME,  <OBJECT>{symField}  )
    ENDIF
    RETURN FieldGet( dwPos )  
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldpossym/*" />
FUNCTION FieldPosSym(sFieldName AS SYMBOL) AS DWORD
    RETURN FieldPos( (STRING) sFieldName )
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldputalias/*" />
FUNCTION FieldPutAlias(symAlias AS SYMBOL,symField AS SYMBOL,uNewValue AS USUAL) AS USUAL
    RETURN FieldPutSelect(symAlias, symField, uNewValue)
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldputsym/*" />
FUNCTION FieldPutSym(symField AS SYMBOL,uNewValue AS USUAL) AS USUAL
    LOCAL dwPos AS DWORD
    dwPos := FieldPosSym(symField)
    IF dwPos == 0
        THROW Error.VODBError( EG_ARG, EDB_FIELDNAME,  <OBJECT>{symField}  )
    ENDIF
    RETURN FieldPut( dwPos ,uNewValue )
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldputselect/*" />
FUNCTION FieldPutSelect(symAlias AS USUAL,symField AS SYMBOL,uNewValue AS USUAL) AS USUAL
    LOCAL nArea AS DWORD
    nArea := SELECT(symAlias )
    RETURN FieldPutArea(nArea, symField, uNewValue)
    
    
    *----------------------------------------------------------------------------
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/alias/*" />
FUNCTION Alias(uWorkArea) AS STRING CLIPPER
    IF uWorkArea:IsNil
        RETURN Alias0()
    ENDIF
    IF IsNumeric(uWorkArea)
        RETURN VoDb.Alias(uWorkArea)
    ENDIF
    RETURN (uWorkArea)->Alias()
    
    

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/alias0sym/*" />
FUNCTION Alias0Sym() AS SYMBOL
    RETURN (SYMBOL) Alias0()
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbappend/*" />
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
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbclearindex/*" />
FUNCTION DbClearIndex(uOrder, cIndexFile) AS LOGIC CLIPPER
    RETURN OrdListClear(cIndexFile, uOrder)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcreate/*" />
FUNCTION DbCreate (   cTargetFile,  aStruct, cDriver , lNew,  cAlias, cDelim, lOpen, acRDDs ) AS LOGIC CLIPPER
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
    
    IF lOpen == NIL
        lOpen := .F.
    ENDIF
    
    LOCAL oDriver := cDriver AS OBJECT
    IF oDriver == NULL_OBJECT
        oDriver := RuntimeState.DefaultRDD
    ENDIF
    IF oDriver IS STRING
        lRetCode := VoDbCreate(cTargetFile, aStruct, (STRING) oDriver, lNew, cAlias, cDelim, lKeep, lOpen)
    ELSEIF oDriver IS System.Type
        lRetCode := VoDbCreate(cTargetFile, aStruct, (Type) oDriver, lNew, cAlias, cDelim, lKeep, lOpen )
    ELSE
        THROW Error.DataTypeError( __FUNCTION__, nameof(cDriver ), 3, { cDriver  } )
    ENDIF
    IF ! lRetCode .AND. RuntimeState.LastRDDError != NULL
        THROW RuntimeState.LastRDDError
    ENDIF
    RETURN lRetCode
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcreateindex/*" />
FUNCTION DbCreateIndex(cIndexFile, cKeyValue, cbKeyValue, lUnique) AS LOGIC CLIPPER
    RETURN OrdCreate(cIndexFile, NIL, cKeyValue, cbKeyValue, lUnique)
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcreateorder/*" />
FUNCTION DbCreateOrder  (cOrder, cIndexFile, cKeyValue, cbKeyValue, lUnique) AS LOGIC CLIPPER
    RETURN OrdCreate(cIndexFile, cOrder, cKeyValue, cbKeyValue, lUnique)
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbdelete/*" />
FUNCTION DbDelete () AS LOGIC STRICT
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Delete() )
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbdeleteorder/*" />
FUNCTION DbDeleteOrder(uOrder, cIndexFile) AS LOGIC CLIPPER
    IF uOrder:IsNumeric
        VoDb.OrderInfo(DBOI_NAME,"",uOrder, REF uOrder)
    ENDIF
    
    RETURN OrdDestroy(uOrder, cIndexFile)
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbeval/*" />
FUNCTION DbEval(cbExecute, cbForCondition, cbWhileCondition, nNext, nRecord, lRest) AS LOGIC CLIPPER
    IF lRest:IsNil
        lRest := .F.
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Eval(VoDb.ValidBlock(cbExecute), VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest) )
  
     
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbfieldinfo/*" />
FUNCTION DbFieldInfo(kInfoType, nFieldPos, uNewSetting) AS USUAL CLIPPER
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldInfo(kInfoType, nFieldPos, REF uNewSetting))
    RETURN uNewSetting
    
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgoto/*" />
FUNCTION DbGoto(uRecID AS USUAL) AS LOGIC 
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Goto(uRecId) )
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgoto/*" />
FUNCTION DbGoto(uRecID AS USUAL, uArea AS USUAL) AS LOGIC 
    RETURN (uArea)->(DbGoto(uRecID))


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbinfo/*" />
FUNCTION DbInfo(kInfoType, uNewSetting) AS USUAL CLIPPER
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info(kInfoType, REF uNewSetting))
    IF kInfoType == DBI_GETLOCKARRAY .AND. ((OBJECT) uNewSetting) IS INT[]
        LOCAL aLocks AS INT[]
        LOCAL aResult AS ARRAY
        aLocks := (INT[]) uNewSetting
        aResult := ArrayNew()
        FOREACH VAR iLock IN aLocks
            aResult:Add(iLock)
        NEXT
        uNewSetting := aResult
    ENDIF
    RETURN uNewSetting
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dblocate/*" />
FUNCTION DbLocate(cbForCondition, cbWhileCondition, nNext, nRecord, lRest ) AS LOGIC CLIPPER

    LOCAL lRetCode  AS LOGIC
    
    IF lRest:IsNil
        lRest := .F.
    ENDIF
    
    IF cbWhileCondition:IsNil
        cbWhileCondition := {|| .T. }
    ELSE
        lRest := .T.
    ENDIF
    
    IF nNext:IsNil
        nNext := 0
    ENDIF
    lRetCode := _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Locate(VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest))
    IF lRetCode
        lRetCode := VoDb.Found()
    ENDIF
    
    RETURN lRetCode
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dborderinfo/*" />
/// <seealso cref='T:XSharp.RDD.Enums.DbOrder_Info'>DbOrder_Info ENUM</seealso>
FUNCTION DbOrderInfo(kInfoType,cIndexFile, uOrder, uNewSetting) AS USUAL CLIPPER
    LOCAL lKeyVal  := FALSE  AS LOGIC
    
    IF !cIndexFile:IsString
        cIndexFile := ""
    ENDIF
    
    IF uOrder:IsString
        IF SLen(uOrder) == 0
            uOrder := NIL
        ENDIF
    ENDIF
    
    IF kInfoType == DBOI_KEYVAL
        lKeyVal  := .T.
        kInfoType := DBOI_EXPRESSION
    ENDIF
    VoDb.OrderInfo(kInfoType, cIndexFile, uOrder, REF uNewSetting)
    IF lKeyVal
        IF uNewSetting:IsString
            IF SLen(uNewSetting) == 0
                uNewSetting := NIL
            ELSE
                uNewSetting := &(uNewSetting)
            ENDIF
        ENDIF
    ENDIF
    
    RETURN uNewSetting
    
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrecordinfo/*" />
/// <seealso cref='T:XSharp.RDD.Enums.DbRecordInfo'>DbRecordInfo ENUM</seealso>
FUNCTION DbRecordInfo(kInfoType, uRecId, uNewValue) AS USUAL CLIPPER
    VoDb.RecordInfo(kInfoType, uRecId, REF uNewValue)
    RETURN uNewValue
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrlock/*" />
FUNCTION DbRLock(uRecId) AS USUAL CLIPPER
    RETURN VoDb.Rlock(uRecId)
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrlocklist/*" />
FUNCTION DbRLockList() AS ARRAY STRICT

    LOCAL aLockList := {}   AS ARRAY
    LOCAL uRecords  := NIL AS USUAL
    
    IF _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info(DBI_LOCKCOUNT, REF uRecords))
        aLockList := DbInfo(DBI_GETLOCKARRAY)
    ENDIF
    
    RETURN aLockList
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrselect/*" />
FUNCTION DbRSelect(nRelation) AS DWORD CLIPPER

    DEFAULT( REF nRelation, 0)
    
    RETURN VoDb.RSelect(nRelation)
    
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrunlock/*" />
FUNCTION DbRUnLock(uRecID) AS LOGIC CLIPPER
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Unlock(uRecId) )
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbselect/*" />
FUNCTION DbSelect(nNew) AS DWORD CLIPPER

    LOCAL nOld := 0 AS DWORD
    
    DEFAULT( REF nNew, 0)
    
    VoDb.Select(nNew, REF nOld)
    
    RETURN nOld
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbselectarea/*" /> 
FUNCTION DbSelectArea(uArea) AS LOGIC CLIPPER
    LOCAL sSelect   AS SHORT
    sSelect := _SELECT(uArea)
    _DbThrowErrorOnFailure(__FUNCTION__, sSelect != 0)
    RETURN (sSelect > 0)
    
    
    
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetselect/*" /> 
FUNCTION DbSetSelect(nNewArea) AS DWORD CLIPPER

    DEFAULT( REF  nNewArea, 0)
    
    RETURN (DWORD) VoDb.SetSelect(nNewArea)
    
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsymselect/*" /> 
FUNCTION DbSymSelect(symAlias)  AS DWORD CLIPPER
    IF symAlias:IsNil
        symAlias := Alias0Sym()
    ENDIF
    EnForceType(symAlias, SYMBOL)
    RETURN (DWORD) VoDb.SymSelect(symAlias)
    
    
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrelation/*" /> 
FUNCTION DbRelation(nRelation)  AS STRING CLIPPER
    LOCAL cRelText  := "" AS STRING
    DEFAULT(  REF nRelation, 1)
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Relation(nRelation, REF cRelText))
    RETURN cRelText
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetdriver/*" /> 
FUNCTION DbSetDriver(cNewSetting) AS STRING CLIPPER
    IF cNewSetting:IsString
        RETURN RddSetDefault(cNewSetting)
    ENDIF
    RETURN RddSetDefault()
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetfilter/*" /> 
FUNCTION DbSetFilter(cbCondition, cCondition) AS LOGIC CLIPPER
    local sCondition as string
    if PCount() == 0 .or. (cbCondition:IsNil .and. cCondition:IsNil)
        return DbClearFilter()
    endif
    // Only one parameter passed and it is a string
    // Move it to the second parameter and clear the first parameter
    IF cbCondition:IsString .and. PCount() == 1
        cCondition  := cbCondition
        cbCondition := NIL
    endif
    // Create Compiled codeblock from String
    IF cbCondition:IsNil
        EnForceType(cCondition, STRING)
        sCondition := cCondition
        sCondition := sCondition:Trim()
        if !sCondition:StartsWith("{||")
            cbCondition := &("{||"+scondition+"}")
        else
            cbCondition := &(cCondition)
        endif
    endif
    // Todo : Extract the string from compiled codeblocks
    if cCondition:IsNil
        local oBlock as Object
        EnForceType(cbCondition, CODEBLOCK)
        oBlock := cbCondition
        // When the codeblock is a macro compiled codeblock
        if oBlock IS XSharp._Codeblock VAR cbMacro
            sCondition := cbMacro:ToString():Trim()
            if sCondition:StartsWith("{||") .and. sCondition:EndsWith("}")
                sCondition := sCondition:Substring(3, sCondition:Length-4):Trim()
            endif
            cCondition := sCondition
        else
            cCondition := "UNKNOWN"
        endif
    endif
    return _DbThrowErrorOnFailure(__FUNCTION__, VoDb.SetFilter(cbCondition, cCondition) )
    
    
    
    *----------------------------------------------------------------------------
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetrelation/*" /> 
/// <param name="cName">An optional name for the relation. Defaults to ParentName + "_" + ChildName.</param>
FUNCTION DbSetRelation  (xAlias, cbKey, cKey, cName) AS LOGIC CLIPPER

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

    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.SetRelation(cAlias, cbKey, cKey, cName) )
   

    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbskip/*" /> 
FUNCTION DbSkip (nRecords) AS LOGIC CLIPPER
    IF nRecords:IsNil
        nRecords := 1
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Skip(nRecords) )
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbusearea/*" /> 
FUNCTION DbUseArea (lNewArea, cDriver, cDataFile, cAlias, lShared, lReadOnly, aStruct, cDelim,acRDDs ) AS LOGIC CLIPPER
    LOCAL lRetCode        AS LOGIC
    LOCAL rddList         AS _RddList
    LOCAL nTries          AS LONG
    LOCAL aRdds           AS ARRAY
    
    DEFAULT( REF lNewArea, .F.)
    DEFAULT( REF cAlias, "")
    DEFAULT( REF lShared, !SetExclusive())
    DEFAULT( REF lReadOnly, .F.)
    DEFAULT( REF cDelim, "")
    nTries := 1
    aRdds   := VoDb.RDDList(cDriver, acRDDs)
    rddList := VoDb.AllocRddList(aRdds)
    DO WHILE .T.
    
        IF !Empty(aStruct)
            lRetCode := DbCreate ( cDataFile, aStruct, aRdds, lNewArea,cAlias, cDelim, .T.)
        ELSE
            lRetCode := VoDb.UseArea(lNewArea, rddList, cDataFile, cAlias, lShared, lReadOnly)
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
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldput/*" /> 
FUNCTION FieldPut (nFieldPos AS USUAL, uNewValue  AS USUAL) AS USUAL 
    LOCAL lResult AS LOGIC
    IF ! IsNumeric(nFieldPos)
        THROW Error.ArgumentError( __FUNCTION__, nameof(nFieldPos), __CavoStr(VoErrors.ARGNOTNUMERIC), 1 ,<OBJECT>{nFieldPos,uNewValue})
    ENDIF
    lResult  := VoDb.FieldPut(nFieldPos, uNewValue)
    IF ! lResult
        IF RuntimeState.Dialect == XSharpDialect.XPP
            uNewValue := NIL
        ELSE
            DoError(__FUNCTION__)
        ENDIF
    ENDIF
    RETURN uNewValue
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldget/*" /> 
FUNCTION FieldGet(nFieldPos) AS USUAL CLIPPER
    LOCAL xRetVal := NIL AS USUAL
    LOCAL lResult AS LOGIC
    IF ! IsNumeric(nFieldPos)
        THROW Error.ArgumentError(__FUNCTION__, nameof(nFieldPos), __CavoStr(VoErrors.ARGNOTNUMERIC), 1 ,<OBJECT> {nFieldPos})
    ENDIF
    lResult  := VoDb.FieldGet(nFieldPos, REF xRetVal)
    IF ! lResult
        IF RuntimeState.Dialect == XSharpDialect.XPP
            xRetVal := NIL
        ELSE
            DoError(__FUNCTION__)
        ENDIF
    ENDIF
    RETURN xRetVal

/// <summary>Read an array of bytes direct from the workarea buffer.</summary>
/// <remarks>This will only work for DBF based workareas (not for Advantage workareas)</remarks>
FUNCTION FieldGetBytes(nPos ) AS BYTE[] CLIPPER
    LOCAL bRetVal := NULL AS BYTE[]
    IF ! IsNumeric(nPos)
        THROW Error.ArgumentError(__FUNCTION__, nameof(nPos), __CavoStr(VoErrors.ARGNOTNUMERIC), 1 ,<OBJECT> {nPos})
    ENDIF
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldGetBytes(nPos, REF bRetVal))
    RETURN bRetVal


/// <summary>Write an array of bytes direct to the workarea buffer.</summary>
/// <remarks>This will only work for DBF based workareas (not for Advantage workareas)</remarks>
FUNCTION FieldPutBytes(nPos AS USUAL, aBytes AS BYTE[]) AS LOGIC
    IF ! IsNumeric(nPos)
        THROW Error.ArgumentError(__FUNCTION__, nameof(nPos), __CavoStr(VoErrors.ARGNOTNUMERIC), 1 ,<OBJECT> {nPos,aBytes})
    ENDIF
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldPutBytes(nPos, aBytes))
    RETURN TRUE


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldgetarea/*" />     
FUNCTION FieldGetArea(dwWorkArea AS DWORD, symFieldName AS SYMBOL) AS USUAL 
    LOCAL oldArea := VoDbGetSelect() AS DWORD
    LOCAL result := NIL  AS USUAL
    LOCAL dwPos AS DWORD
    TRY
        VoDbSetSelect( (INT) dwWorkArea)
        dwPos := FieldPosSym(symFieldName)
        IF dwPos == 0
           VoDbSetSelect( (INT) oldArea)
           THROW Error.VODBError(EG_ARG, EDB_FIELDNAME, __FUNCTION__, <OBJECT>{symFieldName})
        ELSE
            VODBFieldGet( dwPos, REF result )
        ENDIF
    FINALLY
        VoDbSetSelect( (INT) oldArea)
    END TRY
    RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldputarea/*" />  
FUNCTION FieldPutArea(dwWorkArea AS DWORD, symField AS SYMBOL, uNewValue AS USUAL) AS USUAL 
    LOCAL oldArea := VoDbGetSelect() AS DWORD
    LOCAL dwPos AS DWORD
    TRY
        VoDbSetSelect( (INT) dwWorkArea)
        dwPos := FieldPosSym(symField)
        IF dwPos == 0
           VoDbSetSelect( (INT) oldArea)
           THROW Error.VODBError(EG_ARG, EDB_FIELDNAME, __FUNCTION__, <OBJECT>{symField})
        ELSE
            VODBFieldPut( dwPos, uNewValue)
        ENDIF
    FINALLY
        VoDbSetSelect( (INT) oldArea)
    END TRY
    RETURN uNewValue
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lupdate/*" />  
FUNCTION LUpdate()  AS DATE STRICT
    RETURN DbInfo(DBI_LASTUPDATE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lupdate/*" />  
FUNCTION LUpdate(uArea AS USUAL)  AS DATE STRICT
    RETURN (uArea)->(LUpdate())
    

    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rddcount/*" />      
FUNCTION RddCount() AS DWORD CLIPPER
    RETURN VoDb.RddCount()
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rddinfo/*" />      
FUNCTION RddInfo(kInfoType, uNewSetting) AS USUAL CLIPPER
    VoDb.RDDInfo(kInfoType, REF uNewSetting)
    RETURN uNewSetting
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rddlist/*" />   
FUNCTION RddList () AS ARRAY CLIPPER
    LOCAL aRddList := {}    AS ARRAY
    IF VoDb.RddCount() > 0
        VAR aNames := VoDb.RddList()
        FOREACH name AS STRING IN aNames
            AAdd( aRddList, name)
        NEXT
    ENDIF
    RETURN aRddList
    
 /// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbmemoext/*" />      
FUNCTION DbMemoExt      (cDriver) AS STRING CLIPPER
    RETURN VoDb.MemoExt(cDriver)
    
    
FUNCTION RddVersion     (nParm) AS USUAL CLIPPER
    IF !nParm:IsNumeric
        nParm := 0
    ENDIF
    RETURN DbInfo(DBI_RDD_VERSION, nParm)
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbmemofield/*" />      
FUNCTION DbMemoField (uField AS USUAL)  AS USUAL
    LOCAL n,i        AS DWORD
    LOCAL xRet       AS USUAL
    LOCAL nFields    AS DWORD
    
    IF uField:IsNumeric
        n := uField
    ELSEIF uField:IsSymbol
        n := FieldPosSym(uField)
    ELSEIF uField:IsString
        n := FieldPos(uField)
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
    
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/afields/*" />   
FUNCTION AFields(acFieldNames, acTypes, anWidths, anDecimals)  AS DWORD CLIPPER
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
    
    IF UsualType(acFieldNames) == ARRAY
        siCount := Min(siCount, Len(acFieldNames) )
        lNamesOk := .T.
    ENDIF
    
    
    IF UsualType(acTypes) == ARRAY
        siCount := Min( siCount, Len(acTypes) )
        lTypesOk := .T.
    ENDIF
    
    
    IF UsualType(anWidths) == ARRAY
        siCount := Min( siCount, Len(anWidths) )
        lLensOk := .T.
    ENDIF
    
    
    IF UsualType(anDecimals) == ARRAY
        siCount := Min( siCount, Len(anDecimals) )
        lDecsOk := .T.
    ENDIF
    
    
    FOR si := 1 TO siCount
    
        IF lNamesOk
            acFieldNames[si] := aStruct[si, DBS_NAME]
        ENDIF
        
        IF lTypesOk
            acTypes[si] := aStruct[si, DBS_TYPE]
        ENDIF
        
        IF lLensOk
            anWidths[si]  := aStruct[si, DBS_LEN]
        ENDIF
        
        IF lDecsOk
            anDecimals[si]  := aStruct[si, DBS_DEC]
        ENDIF
        
    NEXT
    
    
    RETURN siCount
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcopystruct/*" />   
FUNCTION DbCopyStruct(cTargetFile AS STRING, acStruct := NULL_ARRAY AS ARRAY) AS LOGIC STRICT
    RETURN DBCREATE(cTargetFile, VoDb.FieldList(DbStruct(), acStruct, NULL_ARRAY) )



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcopyxstruct/*" /> 
FUNCTION DbCopyXStruct(cTargetFile AS STRING) AS LOGIC STRICT
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
        
        _DbCreate(cTargetFile)
        
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
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbstruct/*" />
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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbstruct/*" />
FUNCTION DbStruct(uArea AS USUAL) AS ARRAY PASCAL
    RETURN (uArea)->(DbStruct())

    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/indexhplock/*" />
FUNCTION IndexHPLock(lNewSetting AS USUAL) AS LOGIC
    LOCAL lOld AS LOGIC
    lOld := RuntimeState.HPLocking
    IF lNewSetting:IsLogic
        RuntimeState.HPLocking := (LOGIC) lNewSetting
    ENDIF
    RETURN lOld
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/newindexlock/*" />
FUNCTION NewIndexLock(lNewSetting AS USUAL) AS LOGIC
    LOCAL lOld AS LOGIC
    lOld := RuntimeState.NewIndexLock
    IF lNewSetting:IsLogic
        RuntimeState.NewIndexLock := (LOGIC) lNewSetting
    ENDIF
    RETURN lOld
    
    
    
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/newlocks/*" />
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


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptyrecord/*" />
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/emptyfield/*" />
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

