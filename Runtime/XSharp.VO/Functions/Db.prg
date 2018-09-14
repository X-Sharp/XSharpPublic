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
/// <summary>
/// </summary>
/// <returns>
/// </returns>


FUNCTION SELECT(xValue AS USUAL) AS USUAL 
    LOCAL dwSelect   AS DWORD
    LOCAL dwCurrent  AS DWORD
    dwCurrent := VODBGetSelect()
    dwSelect := _SELECT(xValue)
    VODBSetSelect(INT(dwCurrent))
    RETURN dwSelect
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION _Select(xValue) AS USUAL CLIPPER
    LOCAL nSelect           AS DWORD
    LOCAL sAlias            AS SYMBOL
    LOCAL xType             AS DWORD
    LOCAL nAsc              AS DWORD
    
    IF IsNil(xValue)
        RETURN( VODBGetSelect() )
    ENDIF
    xType := UsualType(xValue)
    
    IF xType = SYMBOL
        nSelect := (DWORD) VODBSymSelect(xValue)
        
    ELSEIF xType =STRING
        nSelect := 0
        IF SLen(xValue) = 1
            nSelect := Val(xValue)
            nAsc := Asc( Upper(xValue) )
            IF nAsc > 64 .AND. nAsc < 75
                nSelect := nAsc - 64
            ENDIF
        ENDIF
        
        IF (nSelect > 0) .OR. ("0" == xValue)
            nSelect := VODBSetSelect(INT(nSelect))
        ELSE
            sAlias  := SysAddAtom( String2Psz( Upper( AllTrim(xValue) ) ) )
            nSelect := (DWORD) VODBSymSelect(sAlias)
        ENDIF
        
    ELSE
        nSelect := VODBSetSelect(xValue)
    ENDIF
    
    RETURN nSelect
    
FUNCTION __FieldGetNum( fieldpos AS DWORD ) AS USUAL
    LOCAL ret := NULL AS OBJECT
    VODBFieldGet( fieldpos, REF ret )
    RETURN ret
    
FUNCTION __FieldGetWaNum( workarea AS DWORD, fieldpos AS DWORD ) AS USUAL
    LOCAL ret := NULL AS OBJECT
    LOCAL curArea AS DWORD
    curArea := RuntimeState.CurrentWorkarea
    TRY
        RuntimeState.CurrentWorkarea := workarea
        VODBFieldGet( fieldpos, REF ret )
    FINALLY
        RuntimeState.CurrentWorkarea := curArea
    END TRY   
    RETURN ret
    
    
    
    
    
FUNCTION __FieldSetNum( fieldpos AS DWORD, uValue AS USUAL ) AS USUAL
    IF ! VODBFieldPut( fieldpos, uValue )
        DoError( #__FieldSet )
    ENDIF
    // return original value to allow chained expressions
    RETURN uValue
    
    
FUNCTION __FieldSetWaNum( nArea AS DWORD, fieldpos AS DWORD, uValue AS USUAL ) AS USUAL
    LOCAL curArea AS DWORD
    curArea := RuntimeState.CurrentWorkarea
    TRY
        RuntimeState.CurrentWorkarea := nArea
        IF ! VODBFieldPut( fieldpos, uValue )        
            DoError( #__FieldSet )
        ENDIF 
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
            oCB := MCompile("{|x| iif( IsNil(x), __FieldGetNum( "+nPos:ToString()+"), __FieldSetNum( "+nPos:ToString()+" , x)")
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
            VAR cPars := nArea:ToSTring()+","+nPos:ToString()
            oCB := MCompile("{|x| iif( IsNil(x), __FieldGetWaNum("+cPars+"), __FieldSetWaNum("+cPars+", x)")
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
    VODBSetSelect((INT) nArea)
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
        VODBSetSelect( (INT) dwPos )
        dwPos := FieldPosSym(symField)
        IF dwPos == 0
            VODBSetSelect( origArea )
            THROW Error.VODBError( EG_ARG, EDB_FIELDNAME,  symField  )
        ENDIF
        
        ret := FieldPut( dwPos, uValue )
    FINALLY
        VODBSetSelect( origArea )
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
    RETURN VODBAlias(nSelect)
    
    
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
    
    lRetCode := VODBAppend(lReleaseLocks)
    
    IF !lRetCode
        //    UH 06/26/1998
        //    lRetCode := DoError(#DBAPPEND)
        NetErr(.T.)
    ENDIF
    
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbClearIndex(uOrder, cOrdBag) AS LOGIC CLIPPER
    RETURN OrdListClear(cOrdBag, uOrder)
    *----------------------------------------------------------------------------
        *
        *   DBCREATE( cName,    ;   // target file (DBF)
        *           aStru       ;   // structure array like return value of DBSTRUCT()
        *           [,lNew]     ;   // select new workarea ? (default .F.)
        *           [,cAlias]   ;   // alias name for workarae
        *           [,cDriver]  ;   // name of RDD
        *           [,lOpen]    ;   // JUST OPEN the file (we need the structure
        *                           // parameter aStru to be able to open SDF and
        *                           // DELIMited datafiles. If this parameter is
        *                           // given, DBCREATE() works like DBUSEAREA()
        *           [,cDelim]   ;   // delimiter for DELIM RDD
        *           [,lJustOpen];   // just open this file (simulate USE)
        *           [,aRdds]    ;   // Array with names of RDDs we want inherit
        *                           // some methods from
        *           )
        *
        *   Returns:    LOGICAL ->  .F. ... Failure
        *                           .T. ... Success
        *
        *   Example:    IF DBCREATE("NEWDB.DBF", DBSTRUCT(), .T.,,,.T.)
        *                   ...
        *
        *               ENDIF
        *
    *----------------------------------------------------------------------------
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbCreate (   cName,  aStru, cDriver, lNew,  cAlias, cDelim, lJustOpen, aHidden ) AS LOGIC CLIPPER
    LOCAL rddList         AS RddList
    LOCAL lKeep           AS LOGIC
    LOCAL lRetCode        AS LOGIC
    LOCAL aRdds           AS ARRAY
    
    IF aStru == NIL
        aStru := {}
    ENDIF
    
    IF !IsArray( aStru )
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
    LOCAL oDriver := cDriver AS OBJECT
    IF oDriver IS STRING
        lRetCode := VODBCreate(cName, aStru, (STRING) oDriver, lNew, cAlias, cDelim, lKeep, lJustOpen)
    ELSEIF oDriver IS System.Type
        lRetCode := VODBCreate( cName, aStru, (Type) oDriver, lNew, cAlias, cDelim, lKeep, lJustOpen )
    ELSE
        THROW Error.DataTypeError( "DbCreate", nameof(cDriver), 3, { cDriver } )
    ENDIF
    IF !lRetCode
        lRetCode := (LOGIC) DoError("DbCreate")
    ENDIF
    
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbCreateIndex(cName, cExpr, cobExpr, lUnique) AS LOGIC CLIPPER
    RETURN OrdCreate(cName, NIL, cExpr, cobExpr, lUnique)
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbCreateOrder  (uOrder, cName, cExpr, cobExpr, lUnique) AS LOGIC CLIPPER
    RETURN OrdCreate(cName, uOrder, cExpr, cobExpr, lUnique)
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbDelete () AS LOGIC STRICT

    LOCAL lRetCode  AS LOGIC
    
    lRetCode := VODBDelete ()
    
    IF !lRetCode
        lRetCode := (LOGIC) DoError("DBDELETE")
    ENDIF
    
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbDeleteOrder(uOrder, cOrdBag) AS LOGIC CLIPPER
    LOCAL lRet   AS LOGIC
    
    lRet := TRUE
    
    IF IsNumeric(uOrder)
        LOCAL oOrder AS OBJECT
        lRet := VODBOrderInfo(DBOI_NAME,"",uOrder, REF oOrder)
        uOrder := oOrder
    ENDIF
    
    RETURN ORDDESTROY(uOrder, cOrdBag)
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbEval(uBlock, uCobFor, uCobWhile, nNext, nRecno, lRest) AS LOGIC CLIPPER

    LOCAL lRetCode  AS LOGIC
    
    IF IsNil(lRest)
        lRest := .F.
    ENDIF
    
    lRetCode := VODBEval(uBlock, uCobFor, uCobWhile, nNext, nRecno, lRest)
    
    IF !lRetCode
        lRetCode := (LOGIC) DoError(#DbEval)
    ENDIF
    
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbFieldInfo(nOrdinal, nPos, xNewVal) AS USUAL CLIPPER
    LOCAL oNewVal := xNewVal AS OBJECT
    
    IF !VODBFieldInfo(nOrdinal, nPos, REF oNewVal)
        DoError(#DbFieldInfo)
    ENDIF
    
    RETURN oNewVal
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbGoto(uRecId) AS LOGIC CLIPPER

    LOCAL lRetCode  AS LOGIC
    
    lRetCode := VODBGoto(uRecId)
    
    IF !lRetCode
        lRetCode := (LOGIC) DoError(#DbGoto)
    ENDIF
    
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbInfo(nOrdinal, xNewVal) AS USUAL CLIPPER
    LOCAL oNewVal := xNewVal AS OBJECT
    IF !VODBInfo(nOrdinal, REF oNewVal)
        DoError(#DbInfo)
    ENDIF
    
    RETURN oNewVal
    
    
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
    
    lRetCode := VODBLocate(uCobFor, uCobWhile, nNext, uRecId, lRest)
    
    IF !lRetCode
        lRetCode := (LOGIC) DoError(#DbLocate)
    ELSE
        lRetCode := VODBFound()
    ENDIF
    
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbOrderInfo(nOrdinal,cBagName, uOrder, xNewVal) AS USUAL CLIPPER
    LOCAL lKeyVal   AS LOGIC
    
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
    LOCAL oNewVal := xNewVal AS OBJECT	
    VODBOrderInfo(nOrdinal, cBagName, uOrder, REF oNewVal)
    xNewVal := oNewVal
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
    LOCAL oNewVal := xNewVal AS OBJECT
    VODBRecordInfo(nOrdinal, uRecId, REF oNewVal)
    RETURN oNewVal
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRLock(uRecord) AS USUAL CLIPPER
    RETURN VODBRlock(uRecord)
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRLockList() AS ARRAY STRICT

    LOCAL lockList          AS DWORD[]
    LOCAL aLockList := {}   AS ARRAY
    LOCAL i                 AS DWORD
    LOCAL oRecords          AS OBJECT
    LOCAL nRecords          AS DWORD
    nRecords := 0
    
    IF !VODBInfo(DBI_LOCKCOUNT, REF oRecords)
        DoError(#DbRLockList)
    ELSE
        lockList := (DWORD[]) DbInfo(DBI_GETLOCKARRAY)
        nRecords := COnvert.ToUInt32(oRecords)
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
    
    RETURN VODBRSelect(nPos)
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRUnLock(uRecId) AS LOGIC CLIPPER

    LOCAL lRetCode  AS LOGIC
    
    lRetCode := VODBUnlock(uRecId)
    
    IF !lRetCode
        lRetCode := (LOGIC) DoError(#DbRUnLock)
    ENDIF
    
    RETURN lRetCode
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSelect(nNew) AS DWORD CLIPPER

    LOCAL nOld  AS DWORD
    
    DEFAULT( REF nNew, 0)
    
    VODBSelect(nNew, REF nOld)
    
    RETURN nOld
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns> 
FUNCTION DbSelectArea(xValue) AS LOGIC CLIPPER

    LOCAL sSelect   AS SHORT
    
    sSelect := _SELECT(xValue)
    
    IF sSelect = 0
        //		ptrErrInfo := _VODBErrInfoPtr()
        //		ptrErrInfo.pszArg     := xValue)
        //		ptrErrInfo.dwArgType  := UsualType(xValue)
        DoError(#DbSelectArea)
    ENDIF
    
    RETURN (sSelect > 0)
    
    
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSetSelect(nSelect) AS DWORD CLIPPER

    DEFAULT( REF  nSelect, 0)
    
    RETURN (DWORD) VODBSetSelect(nSelect)
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSymSelect(sAlias)  AS DWORD CLIPPER
    DEFAULT( REF  sAlias, Alias0Sym())
    RETURN (DWORD) VODBSymSelect(sAlias)
    
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbRelation(wPos)  AS STRING CLIPPER
    LOCAL cRelText  := "" AS STRING
    DEFAULT(  REF wPos, 1)
    IF !VODBRelation(wPos, REF cRelText)
        DoError("DbRelation")
    ENDIF
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

    LOCAL lRetCode  AS LOGIC
    
    IF IsNil(cFilter)
        cFilter := "UNKNOWN"
    ENDIF
    
    lRetCode := VODBSetFilter(cbFilter, cFilter)
    
    IF !lRetCode
        lRetCode := (LOGIC) DoError(#DbSetFilter)
    ENDIF
    
    RETURN lRetCode
    
    
    
    *----------------------------------------------------------------------------
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbSetRelation  (xAlias, uCobKey, cKey) AS LOGIC CLIPPER

    LOCAL nSelect   AS DWORD
    LOCAL cAlias    AS STRING
    LOCAL xType     AS DWORD
    LOCAL lRetCode  AS LOGIC
    
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
    
    lRetCode := VODBSetRelation(cAlias, uCobKey, cKey)
    
    IF !lRetCode
        lRetCode := (LOGIC) DoError(#DbSetRelation)
    ENDIF
    
    RETURN lRetCode
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DBSKIP (nRecords) AS LOGIC CLIPPER

    LOCAL lRetCode  AS LOGIC
    
    IF IsNil(nRecords)
        nRecords := 1
    ENDIF
    
    lRetCode := VODBSkip(nRecords)
    
    IF !lRetCode
        lRetCode := (LOGIC) DoError(#DBSKIP)
    ENDIF
    
    RETURN lRetCode
    
    
    
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION DbUseArea (lNew, xDriver, cName, cAlias, lShare, lReadOnly, aStru, cDelim,aHidden ) AS LOGIC CLIPPER
    LOCAL lRetCode        AS LOGIC
    LOCAL rddList         AS RddList
    LOCAL nTries          AS LONG
    LOCAL aRdds           AS ARRAY
    
    DEFAULT( REF lNew, .F.)
    DEFAULT( REF cAlias, "")
    DEFAULT( REF lShare, !SetExclusive())
    DEFAULT( REF lReadOnly, .F.)
    DEFAULT( REF cDelim, "")
    nTries := 1
    aRdds   := Db.RDDList(xDriver, aHidden)
    rddList := Db.AllocRddList(aRdds)
    DO WHILE .T.
    
        IF !Empty(aStru)
            lRetCode := DbCreate ( cName, aStru, aRdds, lNew,cAlias, cDelim, .T.)
        ELSE
            lRetCode := VODBUseArea(lNew, rddList, cName, cAlias, lShare, lReadOnly)
        ENDIF
        IF lRetCode
            EXIT
        ELSE
            IF ( (INT) DoError("DBUSEAREA", nTries) != E_RETRY )
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
FUNCTION FIELDPUT (wPos AS USUAL, xValue  AS USUAL) AS USUAL 

    LOCAL xRetVal AS USUAL
    
    IF VODBFieldPut(wPos, xValue)
        xRetVal := xValue
    ELSE
        DoError(#FIELDPUT)
    ENDIF
    
    RETURN xRetVal
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION FieldGet(wPos) AS USUAL CLIPPER

    LOCAL xRetVal AS OBJECT
    
    DEFAULT( REF wPos, 1)
    
    IF !VODBFieldGet(wPos, REF xRetVal)
        DoError(#FIELDGET)
    ENDIF
    
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
    
FUNCTION RDDCount(nType) AS DWORD CLIPPER
    RETURN VODBRddCount()
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION RDDInfo(nOrdinal, xNewVal) AS USUAL CLIPPER
    LOCAL oNewVal := xNewVal AS OBJECT
    IF !VODBRDDInfo(nOrdinal, REF oNewVal)
    ENDIF
    
    RETURN oNewVal
    
    
    
    /// <summary>
    /// </summary>
    /// <returns>
    /// </returns>
FUNCTION RDDList (nType) AS ARRAY CLIPPER
    LOCAL aRddList := {}    AS ARRAY
    IF VODBRddCount() > 0
        VAR aNames := VoDbRddList()
        FOREACH name AS STRING IN aNames
            AAdd( aRddList, name)
        NEXT
    ENDIF
    RETURN aRddList
    
    
    
    
    
    
    
    
    
    
    
FUNCTION DBMemoExt      (cDriver) AS STRING CLIPPER

    IF IsNil(cDriver)
        cDriver := ""
    ENDIF
    
    RETURN VODBMemoExt(cDriver)
    
    
    
    
FUNCTION RDDVersion     (nParm) AS USUAL CLIPPER

    IF !IsNumeric(nParm)
        nParm := 0
    ENDIF
    
    RETURN DbInfo(DBI_RDD_VERSION, nParm)
    
    
    
    
    
    
    
    
FUNCTION DBMemoField (xField AS USUAL)  AS USUAL
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
        FOR i := 1 TO nFields
            IF DbFieldInfo(i, DBS_TYPE) == "M"
                n := i
                EXIT
            ENDIF
        NEXT
    ENDIF
    
    xRet := DbInfo( DBI_MEMOFIELD, n )
    
    RETURN xRet
    
    
    //---------------------------------------------------------------------------
    //
    //  DoError()
    //
    //  Call default error handler
    //
    
    
    
    
    
    *----------------------------------------------------------------------------
        *
        *   Some useful STATIC functions
        *
        *
    *----------------------------------------------------------------------------
    
    //---------------------------------------------------------------------------
    //
    //  __AllocRddList()
    //
    //  Transfers a VO-Array into a DIM Array structure
    //
    
    /*
    FUNCTION __AllocRddList (aRdds AS ARRAY)    AS _RDDLIST     PASCAL
    LOCAL n,i           AS DWORD
    LOCAL rddList       AS _RDDLIST
    n       := ALen(aRdds)
    rddList := MemAlloc( (_SIZEOF(DWORD)) + (n * _SIZEOF(SYMBOL)) )
    
    rddList.uiRddCount := n
    ~"RANGECHECK-"
    FOR i := 1 TO n
    rddList.atomRddName[i] := SysAddAtomUpperA(aRdds[i])
    NEXT
    RETURN rddList
    //
    */
FUNCTION __RddList      (xDriver, aHidden) AS ARRAY CLIPPER

    LOCAL   nType   AS DWORD
    LOCAL   aRdds   AS ARRAY
    LOCAL   n       AS DWORD
    LOCAL   i       AS DWORD
    LOCAL   lBlob   AS LOGIC
    LOCAL   lDbf    AS LOGIC
    
    //  UH 09/24/1997
    //  IF (!IsArray(xDriver) .OR. IsString(xDriver))
    IF IsArray(xDriver)
        nType := ARRAY
    ELSEIF IsString(xDriver)
        //  UH 11/13/1997
        IF SLen(xDriver) = 0
            xDriver := RddSetDefault()
        ENDIF
        nType := STRING
    ELSE
        xDriver := RddSetDefault()
        nType := STRING
    ENDIF
    
    //  UH 09/24/1997
    //  nType := UsualType(xDriver)
    
    IF nType == ARRAY
        aRdds := xDriver
        
    ELSEIF nType == STRING
        aRdds := {}
        
        xDriver := Upper(xDriver)
        
        DO CASE
        CASE xDriver = "DBFNTX"
            lDbf := .T.
        CASE xDriver = "_DBFCDX"
            lDbf := .T.
        CASE xDriver = "DBFCDX"
                lBlob := .T.
                lDbf  := .T.
            xDriver := "_DBFCDX"
        CASE xDriver = "DBFMDX"
            lDbf := .T.
        OTHERWISE
                lDbf := .F.
            lBlob := .F.
        ENDCASE
        
        IF lDbf
            AAdd(aRdds, "CAVODBF")
        ENDIF
        
        AAdd(aRdds, xDriver)
        
        // UH (for David, Don)
        IF lBlob
            AAdd(aRdds, "DBFCDX")
        ENDIF
        
    ENDIF
    
    IF UsualType(aHidden) == ARRAY
        n := ALen(aHidden)
        
        FOR i := 1 TO n
            AAdd(aRdds, aHidden[i])
        NEXT
    ENDIF
    
    RETURN aRdds
    
    
FUNCTION _DbCreate(cFile1, cFile2, cDriver,lNew, cAlias)      AS LOGIC CLIPPER

    LOCAL aStruct       AS ARRAY
    LOCAL oError        AS USUAL
    LOCAL i,n           AS INT
    LOCAL uErrBlock     AS USUAL
    LOCAL nSelect       AS INT
    LOCAL aField        AS ARRAY
    
    FIELD field_name, field_type, field_len, field_dec
    
    nSelect := 0
    
    DEFAULT( REF lNew, .F.)
    
    IF ( Used() .AND. !lNew )
        DBCLOSEAREA()
    ENDIF
    
    uErrBlock := ErrorBlock( {|o| _Break(o) } )
    
    
    BEGIN SEQUENCE
    
        IF ( Empty(cFile2) )
        
            DBCREATE(cFile1,                                    ;
            { {"FIELD_NAME", "C", 10, 0},   ;
            {"FIELD_TYPE", "C", 1, 0},      ;
            {"FIELD_LEN", "N", 3, 0},       ;
            {"FIELD_DEC", "N", 3, 0} },     ;
            cDriver,                        ;
            .F.,                            ;
            cAlias)
            
            
        ELSE
        
        
            DBUSEAREA(lNew, cDriver, cFile2)
            
            aStruct := {}
            
            n := (INT) LASTREC()
            
            VODBGoTop()
            
            DO WHILE !EOF()
                aField := {}
                AAdd(aField, AllTrim(field_name))
                AAdd(aField, AllTrim(field_type))
                AAdd(aField, field_len)
                AAdd(aField, field_dec)
                
                AAdd( aStruct, aField )
                DBSKIP(1, .F.)
            ENDDO
            
            VODBCloseArea()
            
            IF lNew
                VODBSetSelect(nSelect)
            ENDIF
            
            
            
            
            FOR i := 1 TO n
            
                IF (aStruct[i, DBS_TYPE] == "C") .AND. (aStruct[i, DBS_DEC] != 0)
                    aStruct[i,DBS_LEN] += aStruct[i,DBS_DEC] * 256
                ENDIF
                
            NEXT
            
            DBCREATE(cFile1, aStruct, cDriver, lNew, cAlias )
            
        ENDIF
        
        
    RECOVER USING oError
        VODBCloseArea()
        //oError:FuncSym := #_DBCREATE
        //Eval( uErrBlock, oError)
    END SEQUENCE
    
    ErrorBlock(uErrBlock)
    
    RETURN ( Used() )
    
    
    
    
    
FUNCTION AFields(aNames, aTypes, aLens, aDecs)  AS DWORD CLIPPER

    LOCAL aStruct           AS ARRAY
    LOCAL siCount           AS DWORD
    LOCAL si                AS DWORD
    LOCAL lNamesOk  := .F.  AS LOGIC
    LOCAL lTypesOk  := .F.  AS LOGIC
    LOCAL lLensOk   := .F.  AS LOGIC
    LOCAL lDecsOk   := .F.  AS LOGIC
    
    IF (Empty(aStruct := DbStruct() ))
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
    
    
    
    
    
FUNCTION DbCopyStruct(cFile AS STRING, aFields AS ARRAY) AS LOGIC STRICT

    RETURN DBCREATE(cFile, __DBFLEDIT(DbStruct(), aFields, NULL_ARRAY) )
    
    
    
FUNCTION DbCOpyXStruct(cFile AS STRING) AS LOGIC STRICT

    LOCAL siSaveSel,n,i AS DWORD
    LOCAL aStruct       AS ARRAY
    LOCAL lRetCode      AS LOGIC
    LOCAL oError        AS USUAL
    LOCAL uErrBlock     AS USUAL
    
    FIELD field_name, field_type, field_len, field_dec
    
    uErrBlock := ErrorBlock( {|o| _Break(o) } )
    
    BEGIN SEQUENCE
    
    
        IF !Used()
            THROW Db.DBCMDError()
        ENDIF
        
        aStruct := DbStruct()
        
        n := Len(aStruct)
        
        VODBSelect(0, REF siSaveSel)
        
        _DbCreate(cFile)
        
        
        
        FOR i := 1 TO n
        
            IF aStruct[i, DBS_TYPE] == "C" .AND. aStruct[i, DBS_LEN] > 255
                aStruct[i, DBS_DEC] := SHORT(aStruct[i, DBS_LEN] / 256)
                aStruct[i, DBS_LEN] := aStruct[i, DBS_LEN] % 256
            ENDIF
            
            lRetCode := DBAPPEND()
            
            field_name  := aStruct[i, DBS_NAME]
            field_type  := aStruct[i, DBS_TYPE]
            field_len   := aStruct[i, DBS_LEN]
            field_dec   := aStruct[i, DBS_DEC]
            
        NEXT
        
        IF (VODBGetSelect() <> siSaveSel)
            DBCLOSEAREA()
            VODBSetSelect(INT(siSaveSel))
        ENDIF
        
    RECOVER USING oError
        //oError:FuncSym := #DBCOPYXSTRUCT
        //Eval( uErrBlock, oError)
        lRetCode := .F.
    END SEQUENCE
    
    ErrorBlock(uErrBlock)
    
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
    LOCAL xNewVal   AS USUAL
    
    aStruct := {}
    nFCount := FCount()
    
    IF !Used()
        VAR oError := Db.DbCmdError()
        oError:FuncSym     := "DbStruct"
        THROW oError
    ELSE
        FOR i := 1 UPTO nFCount
            aField := {}
            
            LOCAL oNewVal := xNewVal AS OBJECT
            IF !VODBFieldInfo(DBS_PROPERTIES, i, REF oNewVal)
                DoError("DbFieldInfo")
            ENDIF
            nProps:= Convert.ToUint32(oNewVal)
            FOR j := 1 UPTO nProps
                IF !VODBFieldInfo(j, i, REF oNewVal)
                    DoError("DbFieldInfo")
                ENDIF
                AAdd(aField, oNewVal)
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
    
    
    
INTERNAL STATIC CLASS Db
    INTERNAL STATIC METHOD ParamError(dwArgNum  AS DWORD ,   dwArgType AS DWORD) AS Error 
    
        LOCAL oError    AS Error
        oError := Error{RuntimeState.LastRDDError}
        oError:SubSystem    := "DBCMD"
        oError:GenCode      := EG_ARG
        oError:Severity     := ES_ERROR
        oError:CanDefault   := .F.
        oError:CanRetry     := .T.
        oError:CanSubstitute := .F.
        oError:ArgType      := dwArgType
        oError:ArgNum       := dwArgNum
        RETURN oError
        
    INTERNAL STATIC METHOD DBCMDError()  AS Error 
        LOCAL oError    AS Error
        oError := Error{RuntimeState.LastRDDError}	
        oError:GenCode      := EG_NOTABLE
        oError:SubCode      := EDB_NOTABLE
        oError:SubSystem    := "DBCMD"
        oError:Severity     := ES_ERROR
        oError:CanDefault   := .T.
        RETURN oError
        
    INTERNAL STATIC METHOD AllocFieldNames(aStru AS ARRAY) AS DbFieldNames
        VAR aNames := List<STRING>{}
        FOREACH aField AS ARRAY IN aStru
            aNames:Add(Upper(aField[DBS_NAME]))
        NEXT
        RETURN DbFieldNames{aNames}
        
    INTERNAL STATIC METHOD TargetFields  (cAlias AS STRING, aNames AS ARRAY, oJoinList OUT DbJoinList) AS ARRAY 
    
        LOCAL aNew      AS ARRAY
        LOCAL cName     AS STRING
        LOCAL aStruct   AS ARRAY
        LOCAL adbStruct AS ARRAY
        LOCAL nFields, i AS INT
        LOCAL siPos     AS DWORD
        LOCAL siSelect  AS DWORD
        LOCAL nBytes    AS DWORD
        LOCAL aFldList  AS ARRAY
        
        adbStruct := DbStruct()
        aStruct   := {}
        aFldList := {}
        
        IF ( Empty(aNames) )
        
            aNames     := {}
            nFields    := (INT) FCount()
            siSelect   := VODBGetSelect()
            FOR i := 1 TO nFields
                cName := adbStruct[i, DBS_NAME]
                AAdd(aFldList, {siSelect, FieldPos(cName)})
                AAdd(aStruct, aDbStruct[i])
                AAdd(aNames, cName)
            NEXT
        ELSE
            nFields := (INT)Len(aNames)
            aNew := {}
            FOR i := 1 TO nFields
                AAdd(aNew, AllTrim(Upper(aNames[i])))
            NEXT
            aNames := aNew
            nFields := (INT)FCount()
            siSelect := VODBGetSelect()
            FOR i := 1 TO nFields
                cName := adbStruct[i, DBS_NAME]
                IF AScan(aNames, {|c| c == cName}) > 0
                    AAdd(aFldList, {siSelect, FieldPos(cName)})
                    AAdd(aStruct, aDbStruct[i])
                ENDIF
            NEXT
        ENDIF
        siSelect := SELECT(cAlias)
        aDbStruct := DbStruct()
        nFields := (INT)Len(aNames)
        
        FOR i := 1 TO nFields
            IF "->" $ aNames[i]
                cName := SubStr(aNames[i], At(">", aNames[i]) + 1)
            ELSE
                cName :=  aNames[i]
            ENDIF
            
            siPos := AScan(aDbStruct, {|a| a[DBS_NAME] == cName})
            IF siPos > 0 .AND. (AScan( aStruct, {|c|c[DBS_NAME]== cName }) == 0)
                AAdd(aFldList, {siSelect, FieldPos(cName)})
                AAdd(aStruct, aDbStruct[siPos])
            ENDIF
        NEXT
        nFields := (INT)ALen(aStruct)
        oJoinList := DbJoinList{nFields}
        FOR i := 1 TO nFields
            oJoinList:Fields[i]:Area := aFldList[i,1]
            oJoinList:Fields[i]:Pos  := aFldList[i,2] - 1
        NEXT
        RETURN aStruct
        
    INTERNAL STATIC METHOD  RddList( xDriver AS USUAL, aHidden AS USUAL ) AS ARRAY
    
        LOCAL   nType   AS DWORD
        LOCAL   aRdds  := NULL_ARRAY AS ARRAY
        LOCAL   n       AS DWORD
        LOCAL   i       AS DWORD
        LOCAL   lBlob  := FALSE AS LOGIC
        LOCAL   lDbf    AS LOGIC
        
        IF IsArray(xDriver)
            nType := ARRAY
        ELSEIF IsString(xDriver)
            IF SLen(xDriver) = 0
                xDriver := RDDSetDefault()
            ENDIF
            nType := STRING
        ELSE
            xDriver := RDDSetDefault()
            nType := STRING
        ENDIF
        
        IF nType == ARRAY
            aRdds := xDriver
        ELSEIF nType == STRING
            aRdds := {}
            xDriver := upper(xDriver)
            DO CASE
            CASE xDriver = "DBFNTX"
                lDbf := .T.
            CASE xDriver = "_DBFCDX"
                lDbf := .T.
            CASE xDriver = "DBFCDX"
                    lBlob := .T.
                    lDbf  := .T.
                xDriver := "_DBFCDX"
            CASE xDriver = "DBFMDX"
                lDbf := .T.
            OTHERWISE
                    lDbf := .F.
                lBlob := .F.
            ENDCASE
            
            IF lDbf
                AAdd(aRdds, "DBF")  
            ENDIF
            
            AAdd(aRdds, xDriver)
            
            IF lBlob
                AAdd(aRdds, "DBFCDX")
            ENDIF
            
        ENDIF
        
        IF UsualType(aHidden) == ARRAY
            n := ALen(aHidden)
            FOR i := 1 TO n
                AAdd(aRdds, aHidden[i])
            NEXT
        ENDIF
        RETURN aRdds
        
    INTERNAL STATIC METHOD AllocRddList(aNames AS ARRAY) AS XSharp.RDD.RddList
        VAR aList := List<STRING>{}
        FOREACH cName AS STRING IN aNames
            aList:Add(cName)
        NEXT
        RETURN XSharp.RDD.RddList{aList:ToArray()}
        
END CLASS
