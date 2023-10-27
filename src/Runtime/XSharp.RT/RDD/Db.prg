//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// internal functions used by FieldBlock and FieldWBlock

USING XSharp.RDD.Support
USING XSharp.RDD
USING System.Linq
USING System.Collections.Generic


INTERNAL FUNCTION _SelectFoxPro(uWorkArea AS USUAL) AS USUAL
    LOCAL sSelect   AS DWORD
	LOCAL sCurrent  AS DWORD
    // handles an alias string or a uWorkArea number > 1
	IF IsString ( uWorkArea  ) .OR. ( IsNumeric ( uWorkArea )  .AND. uWorkArea > 1 )

		// Throws a exception if the uWorkArea number is > 4096 !
		IF IsNumeric ( uWorkArea ) .AND. uWorkArea > RuntimeState.Workareas:FindEmptyArea( FALSE )
			THROW ArgumentException{}
		ENDIF


		// note: No exception is thrown if a alias name doesn´t exist !

		sCurrent := VoDbGetSelect()  // save the current workarea

		sSelect := _Select(uWorkArea)  // activates temporary the area

		// IF ! Used()
		IF ! (sSelect) -> Used()
			sSelect := 0
		ENDIF

		VoDbSetSelect(INT(sCurrent))  // restore the workarea

	ELSE

		VAR lGetCurrentAreaNumber := IsNumeric( uWorkArea ) .AND. uWorkArea == 0
		VAR lGetHighestUnusedAreaNumber := IsNumeric( uWorkArea ) .AND. uWorkArea == 1

		// handles Select(), Select(0), Select(1) and takes care of
		// the - not yet implemented - SET COMPATIBLE ON/OFF setting.

		IF uWorkArea:IsNil .OR. lGetHighestUnusedAreaNumber .OR. lGetCurrentAreaNumber

			IF lGetHighestUnusedAreaNumber .OR. ( uWorkArea:IsNil .AND. RuntimeState.Compatible )
				// get the number of the highest unused work area
				sSelect := RuntimeState.Workareas:FindEmptyArea( FALSE )

			ELSE // Pcount()== 0 or lGetCurrentAreaNumber
				sSelect := VoDbGetSelect()

			ENDIF

		ELSE
			// Throw a exception if uWorkArea is something else
			THROW ArgumentException{}
		ENDIF

	ENDIF
    RETURN sSelect


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/select/*" />
FUNCTION Select(uWorkArea) AS USUAL CLIPPER
    LOCAL sSelect   AS DWORD
    IF RuntimeState.Dialect == XSharpDialect.FoxPro  // KHR
        sSelect := _SelectFoxPro(uWorkArea)
    ELSE
    	LOCAL sCurrent  AS DWORD
	    sCurrent := VoDbGetSelect()
	    sSelect := _Select(uWorkArea)
	    VoDbSetSelect(INT(sCurrent))
    ENDIF
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
    ELSE
    	//  ------ added KHR : FoxPro allowes SELECT 10 which becomes DbSelectArea("10")
       // see https://github.com/X-Sharp/XSharpPublic/issues/236
        LOCAL nTemp := Val( uWorkArea ) AS LONG
   	    IF nTemp:ToString() == uWorkArea
   	        nSelect := (DWORD) nTemp
    	ENDIF

        // -------------------
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
    VAR curArea := RuntimeState.CurrentWorkarea
    TRY
        RuntimeState.CurrentWorkarea := workarea
        RETURN __FieldGetNum(fieldpos)
    FINALLY
        RuntimeState.CurrentWorkarea := curArea
    END TRY



/// <exclude/>
FUNCTION __FieldSetNum( fieldpos AS DWORD, uValue AS USUAL ) AS USUAL
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldPut( fieldpos, uValue ))

/// <exclude/>
FUNCTION __FieldSetWaNum( nArea AS DWORD, fieldpos AS DWORD, uValue AS USUAL ) AS USUAL
    VAR curArea := RuntimeState.CurrentWorkarea
    TRY
        RuntimeState.CurrentWorkarea := nArea
        _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldPut( fieldpos, uValue ) )
        RETURN uValue
    FINALLY
        RuntimeState.CurrentWorkarea := curArea
    END TRY




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldblock/*" />
FUNCTION FieldBlock(cFieldName AS STRING) AS CODEBLOCK
    LOCAL oCB  := NULL AS CODEBLOCK
    LOCAL nPos := 0    AS DWORD
    IF ! String.IsNullOrEmpty(cFieldName)
        nPos := FieldPos(cFieldName)
        IF nPos != 0
            oCB := {|x| IIF(x:IsNil, __FieldGetNum(nPos), __FieldSetNum(nPos, x))}
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
    RETURN FieldGetArea(@@Select(symAlias ), symFieldName)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldgetalias/*" />
FUNCTION FieldGetSelect(symAlias AS USUAL,symFieldName AS SYMBOL) AS USUAL
    RETURN FieldGetArea(@@Select(symAlias ), symFieldName)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldgetsym/*" />
FUNCTION FieldGetSym(symField AS SYMBOL) AS USUAL
    LOCAL dwPos AS DWORD
    dwPos := FieldPosSym(symField)
    IF dwPos == 0
        THROW Error.VoDbError( EG_ARG, EDB_FIELDNAME,  <OBJECT>{symField}  )
    ENDIF
    RETURN FieldGet( dwPos )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldpossym/*" />
FUNCTION FieldPosSym(sFieldName AS SYMBOL) AS DWORD
    RETURN FieldPos( (STRING) sFieldName )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldputalias/*" />
FUNCTION FieldPutAlias(symAlias AS SYMBOL,symField AS SYMBOL,uNewValue AS USUAL) AS USUAL
    RETURN FieldPutArea(@@Select(symAlias ), symField, uNewValue)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldputsym/*" />
FUNCTION FieldPutSym(symField AS SYMBOL,uNewValue AS USUAL) AS USUAL
    LOCAL dwPos AS DWORD
    dwPos := FieldPosSym(symField)
    IF dwPos == 0
        THROW Error.VoDbError( EG_ARG, EDB_FIELDNAME,  <OBJECT>{symField}  )
    ENDIF
    RETURN FieldPut( dwPos ,uNewValue )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldputselect/*" />
FUNCTION FieldPutSelect(symAlias AS USUAL,symField AS SYMBOL,uNewValue AS USUAL) AS USUAL
    RETURN FieldPutArea(@@Select(symAlias ), symField, uNewValue)


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
FUNCTION DbAppend(lReleaseLocks, uArea) AS LOGIC CLIPPER
    LOCAL lRetCode  AS LOGIC

    IF lReleaseLocks:IsNil
        lReleaseLocks := .T.
    ENDIF
    IF uArea:IsNil
        lRetCode := VoDb.Append(lReleaseLocks)
    ELSE
        lRetCode := (uArea)->(VoDb.Append(lReleaseLocks))
    ENDIF

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

    IF aStruct:IsNil
        aStruct := {}
    ENDIF

    IF ! aStruct:IsArray
        RETURN .F.
    ENDIF

    //
    // Get "lNew" and create "lKeep" for VoDbUseArea()
    // by Clipper's logic:
    //
    //      .T. -   to open in first available workarea
    //      .F. -   to open in current workarea
    //      NIL -   to close file after creating
    //

    IF lNew:IsNil
        lNew    := .T.
        lKeep   := .F.
    ELSE
        lKeep   := .T.
    ENDIF

    IF lOpen:IsNil
        lOpen := .F.
    ENDIF

    LOCAL oDriver := cDriver AS OBJECT
    IF cDriver:IsNil
        oDriver := RuntimeState.DefaultRDD
    ENDIF
    IF oDriver IS STRING
        lRetCode := VoDbCreate(cTargetFile, aStruct, (STRING) oDriver, lNew, cAlias, cDelim, lKeep, lOpen)
    ELSEIF oDriver IS System.Type
        lRetCode := VoDbCreate(cTargetFile, aStruct, (Type) oDriver, lNew, cAlias, cDelim, lKeep, lOpen )
    ELSE
        THROW Error.DataTypeError( __FUNCTION__, nameof(cDriver ), 3, { cDriver  } )
    ENDIF
    IF ! lRetCode .AND. RuntimeState.LastRddError != NULL
        THROW RuntimeState.LastRddError
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbdelete/*" />
FUNCTION DbDelete (uArea AS USUAL) AS LOGIC STRICT
    RETURN (uArea)->DbDelete()

// Delete with AutoLock for FoxPro support
FUNCTION __DbDelete () AS LOGIC STRICT
    TRY
        DbAutoLock()
        RETURN DbDelete()
    FINALLY
        DbAutoUnLock()
    END TRY

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrecall/*" />
FUNCTION DbRecall() AS LOGIC STRICT
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Recall())

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrecall/*" />
FUNCTION DbRecall(uArea AS USUAL) AS LOGIC STRICT
    RETURN (uArea)->DbRecall()


// DbRecall with automatic locking for FoxPro support
FUNCTION __DbRecall() AS LOGIC STRICT
TRY
    DbAutoLock()
    RETURN DbRecall()
FINALLY
    DbAutoUnLock()
END TRY


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbdeleteorder/*" />
FUNCTION DbDeleteOrder(uOrder, cIndexFile) AS LOGIC CLIPPER
    IF uOrder:IsNumeric
        VoDb.OrderInfo(DBOI_NAME,"",uOrder, REF uOrder)
    ENDIF

    RETURN OrdDestroy(uOrder, cIndexFile)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbeval/*" />
FUNCTION DbEval(cbExecute, cbForCondition, cbWhileCondition, nNext, nRecord, lRest, lNoOpt) AS LOGIC CLIPPER
    cbExecute           := VoDb.ValidBlock(cbExecute,{||NIL})
    cbForCondition      := VoDb.ValidBlock(cbForCondition)
    cbWhileCondition    := VoDb.ValidBlock(cbWhileCondition)
    nNext               := IIF(nNext:IsNumeric, nNext, 0)
    nRecord             := IIF(nRecord:IsNumeric, nRecord, NULL_OBJECT)
    lRest               := IIF(lRest:IsNil, .F., lRest)
    VAR lOldOpt := __DbPushOptimize(lNoOpt)
    TRY
        // Do not throw error when VoDb.Eval was aborted because of a FALSE condition returned
        // by the cbExecute block
        VAR lOk := VoDb.Eval(cbExecute, cbForCondition, cbWhileCondition, nNext, nRecord, lRest)
        IF ! lOk .and. RuntimeState.LastRddError != NULL
            DoError(__FUNCTION__)
        ENDIF
        RETURN lOk
    FINALLY
        __DbPopOptimize(lNoOpt, lOldOpt)
    END TRY



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbfieldinfo/*" />
FUNCTION DbFieldInfo(kInfoType, nFieldPos, uNewSetting) AS USUAL CLIPPER
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldInfo(kInfoType, nFieldPos, REF uNewSetting))
    RETURN uNewSetting




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgoto/*" />
FUNCTION DbGoto(uRecID AS USUAL) AS LOGIC
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Goto(uRecID) )

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbgoto/*" />
FUNCTION DbGoto(uRecID AS USUAL, uArea AS USUAL) AS LOGIC
    RETURN (uArea)->(DbGoto(uRecID))


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbinfo/*" />
FUNCTION DbInfo(kInfoType, uNewSetting) AS USUAL CLIPPER
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Info(kInfoType, REF uNewSetting))
    IF kInfoType == DBI_GETLOCKARRAY
        LOCAL oResult := uNewSetting AS OBJECT
        IF oResult IS System.Array VAR aLocks
            VAR aResult := ArrayNew()
            FOREACH VAR iLock IN aLocks
                aResult:Add(iLock)
            NEXT
            uNewSetting := aResult
        ENDIF
    ENDIF
    RETURN uNewSetting


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dblocate/*" />
FUNCTION DbLocate(cbForCondition, cbWhileCondition, nNext, nRecord, lRest, lNoOpt ) AS LOGIC CLIPPER

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
    VAR lOldOpt := __DbPushOptimize(lNoOpt)
    TRY
        lRetCode := _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Locate(VoDb.ValidBlock(cbForCondition), VoDb.ValidBlock(cbWhileCondition), nNext, nRecord, lRest))
    FINALLY
        __DbPopOptimize(lNoOpt, lOldOpt)
    END TRY
    IF lRetCode
        lRetCode := VoDb.Found()
    ENDIF

    RETURN lRetCode


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dborderinfo/*" />
/// <seealso cref='XSharp.RDD.Enums.DbOrder_Info'>DbOrder_Info ENUM</seealso>
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
/// <seealso cref='XSharp.RDD.Enums.DbRecordInfo'>DbRecordInfo ENUM</seealso>
FUNCTION DbRecordInfo(kInfoType, uRecId, uNewValue) AS USUAL CLIPPER
    VoDb.RecordInfo(kInfoType, uRecId, REF uNewValue)
    RETURN uNewValue



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrlock/*" />
FUNCTION DbRLock(uRecId) AS USUAL CLIPPER
    RETURN VoDb.RLock(uRecId)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrlocklist/*" />
FUNCTION DbRLockList() AS ARRAY STRICT
    RETURN DbInfo(DBI_GETLOCKARRAY)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrselect/*" />
FUNCTION DbRSelect(nRelation) AS DWORD CLIPPER

    @@Default( REF nRelation, 0)

    RETURN VoDb.RSelect(nRelation)




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrunlock/*" />
FUNCTION DbRUnLock(uRecID) AS LOGIC CLIPPER
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Unlock(uRecID) )


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbselect/*" />
FUNCTION DbSelect(nNew) AS DWORD CLIPPER

    @@Default( REF nNew, 0)

    VoDb.Select(nNew, OUT VAR nOld)

    RETURN nOld



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbselectarea/*" />
FUNCTION DbSelectArea(uArea) AS LOGIC CLIPPER
    LOCAL sSelect   AS SHORT
    sSelect := _Select(uArea)
    _DbThrowErrorOnFailure(__FUNCTION__, sSelect != 0)
    RETURN (sSelect > 0)






/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetselect/*" />
FUNCTION DbSetSelect(nNewArea) AS DWORD CLIPPER

    @@Default( REF  nNewArea, 0)

    RETURN (DWORD) VoDb.SetSelect(nNewArea)




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsymselect/*" />
FUNCTION DbSymSelect(symAlias)  AS DWORD CLIPPER
    IF symAlias:IsNil
        symAlias := Alias0Sym()
    ENDIF
    EnforceType(symAlias, SYMBOL)
    RETURN (DWORD) VoDb.SymSelect(symAlias)





/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbrelation/*" />
FUNCTION DbRelation(nRelation)  AS STRING CLIPPER
    LOCAL cRelText  := "" AS STRING
    @@Default(  REF nRelation, 1)
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
    LOCAL sCondition AS STRING
    LOCAL cbCond AS CODEBLOCK
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
        EnforceType(cCondition, STRING)
        sCondition := cCondition
        sCondition := sCondition:Trim()
        if !sCondition:StartsWith("{||")
            cbCondition := &("{||"+sCondition+"}")
        else
            cbCondition := &(cCondition)
        endif
    ENDIF
    if cCondition:IsNil
        local oBlock as Object
        EnforceType(cbCondition, CODEBLOCK)
        oBlock := cbCondition
        // When the codeblock is a macro compiled codeblock
        IF oBlock IS XSharp.Codeblock VAR cbMacro
            sCondition := cbMacro:ToString():Trim()
            if sCondition:StartsWith("{||") .and. sCondition:EndsWith("}")
                sCondition := sCondition:Substring(3, sCondition:Length-4):Trim()
            ENDIF
            cCondition := sCondition
        ENDIF
    ENDIF
    IF cbCondition:IsCodeblock
        cbCond := cbCondition
    ELSE
        cbCond := NULL
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.SetFilter(cbCond, cCondition) )



    *----------------------------------------------------------------------------
/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsetrelation/*" />
/// <param name="cName">An optional name for the relation. Defaults to ParentName + "_" + ChildName.</param>
FUNCTION DbSetRelation  (xAlias, cbKey, cKey, cName) AS LOGIC CLIPPER

    LOCAL nSelect   AS DWORD
    LOCAL cAlias    AS STRING
    LOCAL cbRelation AS CODEBLOCK
    @@Default(REF cName, "")

    IF xAlias:IsString
        nSelect := Val(xAlias)

        IF nSelect = 0
            cAlias := xAlias
        ELSE
            cAlias := Alias(nSelect)
        ENDIF

    ELSE
        cAlias := Alias(xAlias)
    ENDIF
    IF cbKey:IsCodeblock
        cbRelation := cbKey
    ELSE
        cbRelation := NULL
    ENDIF
    IF !IsString(cKey)
        IF cbRelation != NULL
            VAR sKey := cbRelation:ToString():Trim()
            IF sKey:StartsWith("{||") .AND. sKey:EndsWith("}")
                sKey := sKey:Substring(3, sKey:Length-4):Trim()
            ENDIF
            cKey := sKey
        ENDIF
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.SetRelation(cAlias, cbRelation, cKey, cName) )




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbskip/*" />
FUNCTION DbSkip (nRecords, uArea) AS LOGIC CLIPPER
    IF nRecords:IsNil
        nRecords := 1
    ENDIF
    IF uArea:IsNil
        RETURN _DbThrowErrorOnFailure(__FUNCTION__, VoDb.Skip(nRecords) )
    ENDIF
    RETURN _DbThrowErrorOnFailure(__FUNCTION__, (uArea)->(VoDb.Skip(nRecords) ))



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbusearea/*" />
FUNCTION DbUseArea (lNewArea, cDriver, cDataFile, cAlias, lShared, lReadOnly, aStruct, cDelim,acRDDs ) AS LOGIC CLIPPER
    LOCAL lRetCode        AS LOGIC
    LOCAL rddList         AS _RddList
    LOCAL nTries          AS LONG
    LOCAL aRdds           AS ARRAY
    IF PCount() == 0
        RETURN DbCloseArea()
    ENDIF
    @@Default( REF lNewArea, .F.)
    @@Default( REF cAlias, "")
    @@Default( REF lShared, !SetExclusive())
    @@Default( REF lReadOnly, .F.)
    @@Default( REF cDelim, "")
    nTries := 1
    aRdds   := VoDb.RddList(cDriver, acRDDs)
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
        THROW Error.ArgumentError( __FUNCTION__, nameof(nFieldPos), __CavoStr(VOErrors.ARGNOTNUMERIC), 1 ,<OBJECT>{nFieldPos,uNewValue})
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
        THROW Error.ArgumentError(__FUNCTION__, nameof(nFieldPos), __CavoStr(VOErrors.ARGNOTNUMERIC), 1 ,<OBJECT> {nFieldPos})
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

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldget/*" />
/// <summary>Read an array of bytes direct from the workarea buffer.</summary>
/// <remarks>This will only work for DBF based workareas (not for Advantage workareas)</remarks>
///<returns>
/// The value of the field.`
/// IF <paramref name="nFieldPos"/> does not correspond to the position of any field in the database file, FieldGetBytes() will generate an error.
/// </returns>

FUNCTION FieldGetBytes(nFieldPos ) AS BYTE[] CLIPPER
    LOCAL bRetVal := NULL AS BYTE[]
    IF ! IsNumeric(nFieldPos)
        THROW Error.ArgumentError(__FUNCTION__, nameof(nFieldPos), __CavoStr(VOErrors.ARGNOTNUMERIC), 1 ,<OBJECT> {nFieldPos})
    ENDIF
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldGetBytes(nFieldPos, REF bRetVal))
    RETURN bRetVal


/// <remarks>This will only work for DBF based workareas (not for Advantage workareas)</remarks>
/// <summary>Write an array of bytes direct to the workarea buffer.</summary>
/// <param name="nFieldPos">The position OF the FIELD IN the database file structure.</param>
/// <param name="aBytes">The value to write to the field</param>
///<returns>
/// The value assigned TO the field.
/// IF <paramref name="nFieldPos"/> does not correspond to the position of any field in the database file, FieldPutBytes() will generate an error.
/// </returns>
FUNCTION FieldPutBytes(nFieldPos AS USUAL, aBytes AS BYTE[]) AS USUAL
    IF ! IsNumeric(nFieldPos)
        THROW Error.ArgumentError(__FUNCTION__, nameof(nFieldPos), __CavoStr(VOErrors.ARGNOTNUMERIC), 1 ,<OBJECT> {nFieldPos,aBytes})
    ENDIF
    _DbThrowErrorOnFailure(__FUNCTION__, VoDb.FieldPutBytes(nFieldPos, aBytes))
    RETURN aBytes


FUNCTION __FieldHelper(dwWorkArea AS DWORD, symFieldName AS SYMBOL, cFunction AS STRING, oldArea OUT DWORD) AS DWORD
    oldArea := VoDbGetSelect()
    LOCAL dwPos AS DWORD
    VoDbSetSelect( (INT) dwWorkArea)
    dwPos := FieldPosSym(symFieldName)
    IF dwPos == 0
        VoDbSetSelect( (INT) oldArea)
        THROW Error.VoDbError(EG_ARG, EDB_FIELDNAME, cFunction, <OBJECT>{symFieldName})
    ENDIF
    RETURN dwPos

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldgetarea/*" />
FUNCTION FieldGetArea(dwWorkArea AS DWORD, symFieldName AS SYMBOL) AS USUAL
    LOCAL dwPos AS DWORD
    LOCAL result := NIL AS USUAL
    dwPos :=__FieldHelper(dwWorkArea, symFieldName, __FUNCTION__, OUT VAR oldArea)
    VoDbFieldGet( dwPos, REF result )
    VoDbSetSelect( (INT) oldArea)
    RETURN result

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/fieldputarea/*" />
FUNCTION FieldPutArea(dwWorkArea AS DWORD, symField AS SYMBOL, uNewValue AS USUAL) AS USUAL
    LOCAL dwPos AS DWORD
    dwPos :=__FieldHelper(dwWorkArea, symField, __FUNCTION__, OUT VAR oldArea)
    VoDbFieldPut( dwPos, uNewValue)
    VoDbSetSelect( (INT) oldArea)
    RETURN uNewValue

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lupdate/*" />
FUNCTION LUpdate()  AS DATE STRICT
    IF Used()
        RETURN DbInfo(DBI_LASTUPDATE)
    ENDIF
    RETURN NULL_DATE

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/lupdate/*" />
FUNCTION LUpdate(uArea AS USUAL)  AS DATE STRICT
    RETURN (uArea)->(LUpdate())



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rddcount/*" />
FUNCTION RddCount() AS DWORD CLIPPER
    RETURN VoDb.RddCount()


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/rddinfo/*" />
FUNCTION RddInfo(kInfoType, uNewSetting) AS USUAL CLIPPER
    VoDb.RddInfo(kInfoType, REF uNewSetting)
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

    @@Default( REF lNew, .F.)

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
            DO WHILE !VoDb.Eof()
                aField := ArrayNew(4)
                aField[DBS_NAME] := AllTrim(FieldGet(1))
                aField[DBS_TYPE] := AllTrim(FieldGet(2))
                aField[DBS_LEN ] := FieldGet(3)
                aField[DBS_DEC ] := FieldGet(4)
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
                    aField[DBS_DEC] := 0
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
    RETURN DbCreate(cTargetFile, VoDb.FieldList(DbStruct(), acStruct, NULL_ARRAY) )



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbcopyxstruct/*" />
FUNCTION DbCopyXStruct(cTargetFile AS STRING) AS LOGIC STRICT
    LOCAL siSaveSel,n,i AS DWORD
    LOCAL aStruct       AS ARRAY
    LOCAL lRetCode  := FALSE AS LOGIC

    FIELD field_name, field_type, field_len, field_dec

    TRY
        IF !Used()
            THROW VoDb.DbCmdError(__FUNCTION__)
        ENDIF

        aStruct := DbStruct()

        n := Len(aStruct)
        VoDb.Select(0, OUT siSaveSel)

        _DbCreate(cTargetFile)

        FOR i := 1 TO n

            IF aStruct[i, DBS_TYPE] == "C" .AND. aStruct[i, DBS_LEN] > 255
                aStruct[i, DBS_DEC] := SHORT(aStruct[i, DBS_LEN] / 256)
                aStruct[i, DBS_LEN] := aStruct[i, DBS_LEN] % 256
            ENDIF

            lRetCode := DbAppend()

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
    RETURN VoDb.RddList(cRddName, {})
/// <exclude />
FUNCTION __RDDList(cRddName AS STRING, aHidden AS ARRAY) AS ARRAY
    RETURN VoDb.RddList(cRddName, aHidden)

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
      aRecord := VoDbRecordGet()
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
        aRecord := VoDbRecordGet()
        nOffset := 2        // skip the Deleted flag
        FOR i := 1 UPTO n - 1
            uLen := 0
            VoDbFieldInfo(DBS_LEN, i, REF uLen)
            nOffset += uLen
        NEXT
        uLen := 0
        VoDbFieldInfo(DBS_LEN, n, REF uLen)
        nEnd := uLen + nOffset -1

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


/// <summary>Automatically lock a record in the FoxPro dialect </summary>
FUNCTION DbAutoLock() AS VOID
    IF XSharp.RuntimeState.AutoLock != NULL
        XSharp.RuntimeState.AutoLock()
    ENDIF
    RETURN

/// <summary>Automatically unlock a record in the FoxPro dialect </summary>
FUNCTION DbAutoUnLock() AS VOID
    IF XSharp.RuntimeState.AutoUnLock != NULL
        XSharp.RuntimeState.AutoUnLock()
    ENDIF
    RETURN
/// <summary>Automatically lock a record in the FoxPro dialect </summary>

FUNCTION DbAutoLockArea(area AS STRING) AS USUAL STRICT
    (area)->(DbAutoLock())
    RETURN NIL

/// <summary>Automatically unlock a record in the FoxPro dialect </summary>
FUNCTION DbAutoUnLockArea(area AS STRING) AS USUAL STRICT
    (area)->(DbAutoUnLock())
    RETURN NIL
