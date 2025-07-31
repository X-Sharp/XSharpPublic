﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
// functions used by the compiler.
// These functions allow the assignment to an array to be interpreted as an array fill.
// This also contains the push / pop code to allow access to WITH variables outside of the
// function where they are declared.


USING XSharp.RDD
USING System.Collections.Generic
USING XSharp.RDD.Support

/// <include file="VFPDocs.xml" path="Runtimefunctions/dbgetprop/*" />
/// <seealso cref="DbSetProp" />
/// <seealso cref="DbcDatabase" />
/// <seealso cref="DbcTable" />
/// <seealso cref="DbcView" />
/// <seealso cref="DbcConnection" />
/// <seealso cref="DbcField" />
FUNCTION DbGetProp( cName AS STRING, cType AS STRING, cProperty AS STRING)  AS USUAL
    IF ! Dbc.IsValidObjectType(cType)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cType), __VfpStr(VFPErrors.VFP_INVALID_DB_OBJECT, cType))
    ENDIF
    IF ! Dbc.IsValidPropertyName(cProperty)

        THROW Error.ArgumentError(__FUNCTION__, nameof(cProperty), __VfpStr(VFPErrors.VFP_INVALID_DB_PROPERTY_NAME, cProperty))
    ENDIF
    VAR oDb := Dbc.GetCurrent()
    IF oDb == NULL_OBJECT
        THROW Error.VoDbError(EG_DB, EDB_NODB,__FUNCTION__)
    ENDIF

    RETURN oDb:GetProp(cName, cType, cProperty)


/// <include file="VFPDocs.xml" path="Runtimefunctions/dbsetprop/*" />
/// <seealso cref="DbGetProp" />
/// <seealso cref="DbcDatabase" />
/// <seealso cref="DbcTable" />
/// <seealso cref="DbcView" />
/// <seealso cref="DbcConnection" />
/// <seealso cref="DbcField" />
FUNCTION DbSetProp(cName AS STRING, cType AS STRING, cProperty AS STRING, ePropertyValue AS USUAL) AS USUAL
    IF ! Dbc.IsValidObjectType(cType)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cType), __VfpStr(VFPErrors.VFP_INVALID_DB_OBJECT, cType))
    ENDIF
    IF ! Dbc.IsValidPropertyName(cProperty)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cProperty),  __VfpStr(VFPErrors.VFP_INVALID_DB_PROPERTY_NAME, cProperty))
    ENDIF
    VAR oDb := Dbc.GetCurrent()
    IF oDb == NULL_OBJECT
        THROW Error.VoDbError(EG_NOTABLE, EDB_NOTABLE,__FUNCTION__)
    ENDIF
    RETURN oDb:SetProp(cName, cType, cProperty, ePropertyValue)


/// <include file="VFPDocs.xml" path="Runtimefunctions/dbc/*" />
/// <seealso cref="DbAlias" />
/// <seealso cref="DbUsed" />

FUNCTION Dbc() AS STRING
    LOCAL oDb := Dbc.GetCurrent() as DbcDatabase
    IF oDb != NULL
        RETURN oDb:FileName
    ENDIF
    RETURN String.Empty

/// <include file="VFPDocs.xml" path="Runtimefunctions/dbused/*" />
/// <seealso cref="DbAlias" />
/// <seealso cref="Dbc" />
FUNCTION DbUsed( cDatabaseName AS STRING) AS LOGIC
    RETURN Dbc.IsUsed(cDatabaseName)

/// <include file="VFPDocs.xml" path="Runtimefunctions/dbalias/*" />
/// <seealso cref="DbUsed" />
/// <seealso cref="Dbc" />
FUNCTION DbAlias () AS STRING
    LOCAL oDb := Dbc.GetCurrent() as DbcDatabase
    IF oDb != NULL
        RETURN oDb:Name
    ENDIF
    RETURN String.Empty


#pragma options("az", ON)
/// <include file="VFPDocs.xml" path="Runtimefunctions/adatabases/*" />
FUNCTION ADatabases( ArrayName AS ARRAY) AS DWORD
    local result := (DWORD) DbcManager.Databases:Count AS DWORD
    IF result > 0
        ArrayName := __FoxRedim(ArrayName, result, 2 )
        LOCAL nDb := 0 as DWORD
        FOREACH var db in DbcManager.Databases
            ArrayName[nDb,0]   := db:Name
            ArrayName[nDb,1]   := db:FileName
            nDb += 1
        NEXT
    ENDIF
    RETURN result
#pragma options("az", default)

/// <include file="VFPDocs.xml" path="Runtimefunctions/lock/*" />
FUNCTION Lock( cRecordNumberList, uArea) AS LOGIC CLIPPER
    RETURN RLock(cRecordNumberList, uArea)


FUNCTION __DbFieldList(aFields AS ARRAY, lIncludeMemo AS LOGIC) AS ARRAY
    var fields := __DbFieldListHelper(aFields, "", "", lIncludeMemo)
    LOCAL acFields := {} AS ARRAY
    foreach var cField in fields
        AAdd(acFields, cField)
    NEXT
    RETURN acFields

FUNCTION __DbFieldWild(includedFields, excludedFields, lIncludeMemo) AS ARRAY CLIPPER
    var fields := __DbFieldListHelper({}, includedFields, excludedFields, lIncludeMemo)
    LOCAL acFields := {} AS ARRAY
    foreach var cField in fields
        AAdd(acFields, cField)
    NEXT
    RETURN acFields

INTERNAL FUNCTION __DbFieldListHelper(aFieldList AS ARRAY, cIncludedFields AS STRING, cExcludedFields AS STRING, lIncludeMemo AS LOGIC) AS IList<String>
    var aFields   := List<String>{}	// Contains aFieldList in UPPER case
    VAR allfields := List<string>{}	// Contains fields from DBF in UPPER case
    VAR selected := List<string>{}
    LOCAL lAll as LOGIC
    if ALen(aFieldList) > 0
        IF !String.IsNullOrEmpty(cIncludedFields) .or. !String.IsNullOrEmpty(cExcludedFields)
            Throw Error.ArgumentError(__FUNCTION__, "FIELDNAMES", __VfpStr(VFPErrors.VFP_INVALID_FIELD_SPEC))
        ENDIF
        foreach cField as STRING in aFieldList
            aFields:Add(cField:ToUpperInvariant())
        next
    ENDIF
    lAll := ALen(aFieldList) == 0 .and. String.IsNullOrEmpty(cIncludedFields)
    LOCAL fCount as DWORD
    fCount := FCount()
    FOR VAR nFld := 1u to fCount
        LOCAL lInclude AS LOGIC
        LOCAL oVar := NULL AS OBJECT
        VoDb.FieldInfo( DBS_STRUCT, nFld, REF oVar)
        VAR oFld := (RddFieldInfo) oVar
        local fldName as STRING
        IF oFld:Alias != NULL
            fldName := oFld:Alias:ToUpperInvariant()
        ELSE
            fldName := oFld:Name:ToUpperInvariant()
        ENDIF
        SWITCH (STRING) oFld:FieldTypeStr
        CASE "M"
        CASE "G"
            lInclude := lIncludeMemo .or. aFields:IndexOf(fldName) > -1
        OTHERWISE
            lInclude := TRUE
        END SWITCH
        IF lInclude
            allfields:Add(fldName)
        ENDIF
    NEXT
    IF lAll
        selected:AddRange(allfields)
    ELSEIF ALen(aFieldList) > 0
        FOREACH cName AS STRING in aFieldList
            var cField := cName:ToUpper()
            IF allfields:IndexOf(cField) == -1
                Throw Error.ArgumentError(__FUNCTION__, "FIELDNAME", __VfpStr(VFPErrors.VFP_INVALID_FIELDNAME, cField))
            ENDIF
            selected:Add(cField)
        NEXT
    ENDIF
    IF ! String.IsNullOrEmpty(cIncludedFields)
        VAR aElements := __GetAllElements(cIncludedFields)
        FOREACH VAR cElement IN aElements
            FOREACH VAR cField in allfields
                IF selected:IndexOf(cField) == -1
                    IF Like(cElement, cField)
                        selected:Add(cField)
                    ENDIF
                ENDIF
            NEXT
        NEXT
    ENDIF
    IF ! String.IsNullOrEmpty(cExcludedFields)
        allfields:Clear()
        // now we only process the names that were selected before
        allfields:AddRange(selected)
        selected:Clear()
        VAR aElements := __GetAllElements(cExcludedFields)
        FOREACH VAR cElement IN aElements
            FOREACH VAR cField in allfields
                IF !Like(cElement, cField)
                    selected:Add(cField)
                ENDIF
            NEXT
        NEXT
    ENDIF
    return selected

FUNCTION DbCopyFox(cTargetFile, cType, aFields, cbForCondition, ;
        cbWhileCondition, nNext,nRecord, lRest, nCodePage, cDbName, cLongTableName, lCdx, lNoOptimize)   AS LOGIC CLIPPER
    local cOutPutType as STRING
    LOCAL result as LOGIC
    EnforceType(REF lCdx, __UsualType.Logic)
    EnforceType(REF lRest, __UsualType.Logic)
    EnforceType(REF lNoOptimize, __UsualType.Logic)
    VAR aFieldNames := __BuildFieldList(aFields, TRUE)
    LOCAL acFields := {} AS ARRAY
    FOREACH var cField in aFieldNames
        acFields:Add(cField)
    NEXT
    cOutPutType := cType
    var cDelim := RuntimeState.DelimRDD
    VAR lOldOpt := __DbPushOptimize(lNoOptimize)
    TRY
        SWITCH cOutPutType:ToLower()
        CASE "csv"
            RuntimeState.DelimRDD := "CSV"
            if String.IsNullOrEmpty(System.IO.Path.GetExtension(cTargetFile))
                cTargetFile += ".csv"
            endif
            result := DbCopyDelim (cTargetFile, ",", acFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest)
        CASE "sdf"
            if String.IsNullOrEmpty(System.IO.Path.GetExtension(cTargetFile))
                cTargetFile += ".txt"
            endif
            result := DbCopySDF(cTargetFile, acFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest)
        CASE "dbf"
            result := DbCopy(cTargetFile, acFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest, RddSetDefault(),,,lCdx)
        CASE "foxplus"
        CASE "fox2x"
            result := DbCopy(cTargetFile, acFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest,"DBFCDX",,, lCdx)

        OTHERWISE
            // Other Formats
            // DIF,MOD,SYLK,WK1,WKS,WR1,WRK,XLS,XL5
            Throw NotSupportedException{__VfpStr(VFPErrors.VFP_INVALID_FORMAT, "output", cOutPutType)}
        END SWITCH
    FINALLY
        RuntimeState.DelimRDD   := cDelim
        __DbPopOptimize(lNoOptimize, lOldOpt)
    END TRY
    return result



FUNCTION DbCopyDelimFox (cTargetFile, cDelim, cChar, aFields,  ;
        cbForCondition, cbWhileCondition, nNext,nRecord, lRest, nCodePage, lNoOptimize) ;
        AS LOGIC CLIPPER
    var acFields := {}
    foreach var cField in aFields
        AAdd(acFields, cField)
    NEXT
    IF IsString(cDelim)
        VAR sDelim := Upper(cDelim)
        if sDelim == "\TAB"
            cDelim := e"\t"
        elseif sDelim == "\BLANK"
            cDelim := " "
        endif
    ENDIF
    if !IsString(cChar)
        cChar := e"\""
    ENDIF
    RuntimeState.StringDelimiter := cChar
    VAR lOldOpt := __DbPushOptimize(lNoOptimize)
    TRY
        RETURN DbCopyDelim(cTargetFile, cDelim, acFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest)
    FINALLY
        __DbPopOptimize(lNoOptimize, lOldOpt)
    END TRY




FUNCTION DbCopyToArray(uSource, aFieldList, cbForCondition, cbWhileCondition, nNext,nRecord, lRest, lNoOptimize) AS ARRAY CLIPPER
    // COPY TO ARRAY doed not have a MEMO keyword, so automatically include memo fields
    VAR aFields := __BuildFieldList(aFieldList, TRUE)
    LOCAL aResult := {} AS ARRAY
    LOCAL lMulti   AS LOGIC
    LOCAL nRows    AS DWORD
    LOCAL nColumns AS DWORD
    LOCAL aFox   := NULL  AS __FoxArray
    LOCAL aSource := NULL_ARRAY AS ARRAY
    IF IsArray(uSource) .AND. ALen(uSource) > 0
        // We support both FoxPro arrays and VO arrays
        aSource   := uSource
        IF aSource IS __FoxArray VAR aFox1
            aFox := aFox1
            aFox:__Fill(NIL)
            lMulti := aFox:MultiDimensional
            IF lMulti
                nRows    := (DWORD) aFox:Rows
                nColumns := (DWORD) aFox:Columns
            ELSE
                nRows    := 1
                nColumns := (DWORD) aFox:Length
            ENDIF
        ELSE
            lMulti := IsArray(aSource[1])
            IF lMulti
                nRows    := ALen(aSource)
                nColumns := ALen(aSource[1])
            ELSE
                nRows    := 1
                nColumns := ALen(aSource)
            ENDIF
        ENDIF
    ELSE
        lMulti    := TRUE
        nColumns  := FCount()
        nRows     := (DWORD) RecCount()
    ENDIF
    LOCAL cbAction AS CODEBLOCK
    DO WHILE aFields:Count > nColumns
        aFields:RemoveAt(aFields:Count-1)
    ENDDO
    cbAction :=  {|| AAdd(aResult, DbCopyToArraySingleRecord(aFields)), ALen(aResult) < nRows }
    DbEval( cbAction, cbForCondition, cbWhileCondition, nNext,nRecord, lRest, lNoOptimize )
    IF aSource != NULL_ARRAY
        nColumns  := Math.Min(nColumns, FCount())
        IF aFox != NULL
            __FoxFillArray(aFox, NIL)
        ELSE
            AFill(aSource, NIL)
        ENDIF
        IF lMulti
            nRows := Math.Min(ALen(aResult), nRows)
            FOR VAR nRow := 1 TO nRows
                FOR VAR nCol := 1 TO nColumns
                    aSource[nRow, nCol] := aResult[nRow, nCol]
                NEXT
            NEXT
        ELSE
            aResult := aResult[1]
            FOR VAR nCol := 1 TO nColumns
                aSource[nCol] := aResult[nCol]
            NEXT
        ENDIF
        aResult := aSource
    ELSE
        NOP
    ENDIF
    RETURN aResult

INTERNAL FUNCTION DbCopyToArraySingleRecord(aFields as IList<string> ) AS ARRAY
    LOCAL result AS ARRAY
    result := ArrayNew(aFields:Count)
    FOR VAR i := 1 to aFields:Count
        result[i] := __FieldGet(aFields[i-1])
    NEXT
    RETURN result


FUNCTION DbAppendFromArray(aValues, aFieldList, cbForCondition) AS LOGIC CLIPPER
    IF ! IsArray(aValues)
        THROW Error.ArgumentError(__FUNCTION__ , nameof(aValues), __VfpStr(VFPErrors.VFP_MULTI_DIM_EXPECTED,nameof(aValues))  , 1, {aValues})
    ENDIF
    VAR aFields := __BuildFieldList(aFieldList, FALSE)
    LOCAL oForCondition   := NULL   AS ICodeblock
    IF cbForCondition IS ICodeblock
        oForCondition := (ICodeblock) cbForCondition
    ENDIF
    // Check to see if the array is multi dimensional and if the # of fields matches.
    IF ALen(aValues) > 0
        IF !IsArray(aValues[1])
            var aNewArray := {aValues}
            aValues := aNewArray
        ENDIF
        FOREACH var u in (ARRAY) aValues
            IF !IsArray(u)
                THROW Error.ArgumentError(__FUNCTION__ , nameof(aValues), __VfpStr(VFPErrors.VFP_MULTI_DIM_EXPECTED,nameof(aValues)), 1, {aValues})
            ENDIF
            local aElement := u as ARRAY
            IF aElement:Length < aFields:Count
                THROW Error.ArgumentError(__FUNCTION__ , nameof(aValues), __VfpStr(VFPErrors.VFP_SUBARRAY_TOO_SMALL ) , 1, {u})
            ENDIF
            DbAppend()
            // Todo Evaluate FOR clause
            FOR VAR i := 1 to aFields:Count
                __FieldSet(aFields[i-1], aElement[i])
            NEXT
            IF oForCondition != NULL
                LOCAL lResult as LOGIC
                lResult := (LOGIC) oForCondition:EvalBlock()
                IF ! lResult
                    DbBuffRefresh()
                ENDIF
            ENDIF
        NEXT
    ENDIF
    RETURN TRUE



FUNCTION DbAppFox(cSourceFile, cType, aFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest, cSheet, nCodePage, lNoOptimize) AS USUAL CLIPPER
    local cInPutType as STRING
    LOCAL result as LOGIC
    cInPutType := cType
    var cDelim := RuntimeState.DelimRDD
    TRY
        SWITCH cInPutType:ToLower()
        CASE "dbf"
            result := DbApp(cSourceFile, aFields, cbForCondition, cbWhileCondition, nNext, nRecord,lRest)
        CASE "csv"
            RuntimeState.DelimRDD := "CSV"
            if String.IsNullOrEmpty(System.IO.Path.GetExtension(cSourceFile))
                cSourceFile += ".csv"
            endif
            result := DbAppDelim(cSourceFile, ",", aFields, cbForCondition, cbWhileCondition, nNext, nRecord, lRest)
        CASE "sdf"
            if String.IsNullOrEmpty(System.IO.Path.GetExtension(cSourceFile))
                cSourceFile += ".txt"
            endif
            result := DbAppSdf(cSourceFile, aFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest)
        CASE "foxplus"
        CASE "fox2x"
            result := DbApp(cSourceFile, aFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest,"DBFCDX")
        OTHERWISE
            // Other Formats
            // DIF,MOD,SYLK,WK1,WKS,WR1,WRK,XLS,XL5, XL8
            Throw NotSupportedException{__VfpStr(VFPErrors.VFP_INVALID_FORMAT, "input", cInPutType)}
        END SWITCH
    FINALLY
        RuntimeState.DelimRDD   := cDelim
    END TRY
    return result


FUNCTION DbAppDelimFox (cTargetFile, cDelim, cChar, aFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest, nCodePage, lNoOptimize)   AS LOGIC CLIPPER
    IF IsString(cDelim)
        VAR sDelim := Upper(cDelim)
        if sDelim == "\TAB"
            cDelim := e"\t"
        elseif sDelim == "\BLANK"
            cDelim := " "
        endif
    ENDIF
    if !IsString(cChar)
        cChar := e"\""
    ENDIF
    RuntimeState.StringDelimiter := cChar
    RETURN DbAppDelim(cTargetFile, cDelim, aFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/dbsort/*" />
FUNCTION DbSortFox(cTargetFile, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, ;
        lRest, lNoOpt, lDesc, acOutPutFields)   AS LOGIC CLIPPER
    // acFields = the list of fields to sort on
    // acOutPutFields = the list of fields to write
    acOutPutFields := __BuildFieldList(acOutPutFields, TRUE)
    RETURN DbSort(cTargetFile, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, ;
        lRest, lNoOpt, lDesc, acOutPutFields)


    // function that handles extra options for the FoxPro variant of the USE command
    // At this moment the only extra thing this function does is to handle the uArea parameter
    // Several options are relevant for remote views in the Database only:
    // - Online
    // - Admin
    // - NoRequery
    // - NoData
    // - ConnString

/// <include file="VFPRUntimeDocs.xml" path="Runtimefunctions/dbuseareafox/*" />
FUNCTION DbUseAreaFox(uArea, cDataFile, cAlias, lShared, lReadOnly, ;
        lOnline, lAdmin, lAgain, lNoData, lNoRequery, nDataSession, uConnection) AS LOGIC CLIPPER

    LOCAL cDriver := "DBFVFP" AS STRING
    IF !IsNil(uArea)
        DbSelectArea(uArea)
    ENDIF
    IF !IsNil(lAgain) .and. lAgain
        // select other area where cDataFile is open and copy lShared, lReadonly
        var ext := System.IO.Path.GetExtension(cDataFile)
        if String.IsNullOrEmpty(ext)
            cDataFile := System.IO.Path.ChangeExtension(cDataFile, ".dbf")
        endif
        if File(cDataFile)
            cDataFile := FPathName()
        endif
        // locate the area with the same filename
        for var i := 1u to Workareas.MaxWorkareas
            var area := RuntimeState.DataSession.GetRDD(i)
            if area is Workarea var wa .and. String.Compare(wa:FileName, cDataFile, TRUE) == 0
                lShared := wa:Shared
                lReadOnly := wa:ReadOnly
                EXIT
            endif
        next
    ENDIF
    if IsNil(cAlias)
        cAlias := System.IO.Path.GetFileNameWithoutExtension( cDataFile ):ToUpper()
        if VoDbGetSelect(cAlias) > 0
            cAlias := Chr(64+RuntimeState.DataSession.CurrentWorkareaNO)
            DO WHILE VoDbGetSelect(cAlias) > 0
                cAlias := Chr(Asc(cAlias)+1)
            ENDDO
        ENDIF

    ENDIF
    RETURN DbUseArea(FALSE, cDriver, cDataFile, cAlias, lShared, lReadOnly)


FUNCTION DbSeekFox(uExpr, uOrder, cBagName, lDescend) AS LOGIC CLIPPER
    local currentOrder := NIL AS USUAL
    local bagName      := "" AS STRING
    local changeOrder  := FALSE AS LOGIC
    if ! IsNil(uOrder) .or. ! IsNil(cBagName)
        currentOrder := OrdName()
        bagName      := OrdBagName()
        OrdSetFocus(uOrder, cBagName)
        changeOrder := TRUE
    endif
    local result := DbSeek(uExpr,,lDescend) AS LOGIC
    if changeOrder
        OrdSetFocus(currentOrder, bagName)
    endif
    RETURN result


/// <include file="VFPDocs.xml" path="Runtimefunctions/seek/*" />
FUNCTION Seek(uExpression, uWorkarea, uOrder) AS LOGIC CLIPPER
    LOCAL dwCurrentWorkarea := 0 AS DWORD
    IF ! IsNil( uWorkarea )
		dwCurrentWorkarea := VoDbGetSelect()
        IF ! DbSelectArea( uWorkarea )
        	RETURN FALSE
        ENDIF
    ENDIF

    LOCAL lResult := DbSeekFox(uExpression, uOrder) AS LOGIC

    IF dwCurrentWorkarea != 0
    	DbSelectArea( dwCurrentWorkarea )
    END IF
    RETURN lResult


FUNCTION DbCopyStructFox(cTargetFile, aFields, lCdx) AS LOGIC CLIPPER
    local acStruct as ARRAY
    IF IsArray(aFields)
        acStruct := aFields
    ELSE
        acStruct := NULL_ARRAY
    ENDIF
    VAR siFrom   := VoDbGetSelect()
    var result := DbCreate(cTargetFile, VoDb.FieldList(DbStruct(), acStruct, NULL_ARRAY) )
    IF IsLogic(lCdx) .and. lCdx
        local aOrders := {} AS ARRAY
        aOrders := __DbGetTags()
        result  := DbUseArea(TRUE, ,cTargetFile,__UniqueAlias(cTargetFile),FALSE,FALSE)
        IF result
            result := __DbCreateTags(aOrders)
            VoDbCloseArea()
        ENDIF
    ENDIF
    VoDbSetSelect((INT) siFrom)
    return result


FUNCTION DbCopyXStructFox(cTargetFile AS STRING) AS LOGIC
    return DbCopyXStruct(cTargetFile)




