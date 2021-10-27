//
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
        THROW Error.ArgumentError(__FUNCTION__, nameof(cType), __VfpStr(VFPErrors.INVALID_DB_OBJECT, cType))
    ENDIF
    IF ! Dbc.IsValidPropertyName(cProperty)

        THROW Error.ArgumentError(__FUNCTION__, nameof(cProperty), __VfpStr(VFPErrors.INVALID_DB_PROPERTY_NAME, cProperty))
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
FUNCTION DbSetProp(cName AS STRING, cType AS STRING, cProperty AS STRING, ePropertyValue AS USUAL)
    IF ! Dbc.IsValidObjectType(cType)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cType), __VfpStr(VFPErrors.INVALID_DB_OBJECT, cType))
    ENDIF
    IF ! Dbc.IsValidPropertyName(cProperty)
        THROW Error.ArgumentError(__FUNCTION__, nameof(cProperty),  __VfpStr(VFPErrors.INVALID_DB_PROPERTY_NAME, cProperty))
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


#ifdef NOTDEFINED
/// <include file="VFPDocs.xml" path="Runtimefunctions/adatabases/*" />
FUNCTION ADatabases(ArrayName REF __FoxArray)
    local result := DbcManager.Databases:Count AS LONG
    IF result > 0
        if ! IsArray(ArrayName) .or. ArrayName == NULL_OBJECT
            ArrayName := __FoxArray{}
        ENDIF
        ArrayName:ReDim(result,2)
        FOR VAR nDb := 1 to result
            var db := DbcManager.Databases[nDb]
            ArrayName[nDb,1]   := db:Name
            ArrayName[nDb,2]   := db:FileName
        NEXT
    ENDIF
    RETURN result
#endif

/// <include file="VFPDocs.xml" path="Runtimefunctions/lock/*" />
FUNCTION Lock( cRecordNumberList, uArea) AS LOGIC
    RETURN RLock(cRecordNumberList, uArea)


FUNCTION __DbFieldList(aFields AS ARRAY, lIncludeMemo AS LOGIC) AS ARRAY
    var fields := __DbFieldListHelper(aFields, "", "", lIncludeMemo)
    LOCAL acFields := {} AS ARRAY
    foreach var cField in fields
        AAdd(acFields, cField)
    NEXT
    RETURN acFields

FUNCTION __DbFieldWild(includedFields, excludedFields, lIncludeMemo) AS ARRAY
    var fields := __DbFieldListHelper({}, includedFields, excludedFields, lIncludeMemo)
    LOCAL acFields := {} AS ARRAY
    foreach var cField in fields
        AAdd(acFields, cField)
    NEXT
    RETURN acFields

INTERNAL FUNCTION __DbFieldListHelper(aFieldList AS ARRAY, cIncludedFields AS STRING, cExcludedFields AS STRING, lIncludeMemo AS LOGIC) AS IList<String>
    VAR allfields := List<string>{}	// Contains all fields in UPPER case
    VAR selected := List<string>{}
    LOCAL lAll as LOGIC
    if ALen(aFieldList) > 0
        IF !String.IsNullOrEmpty(cIncludedFields) .or. !String.IsNullOrEmpty(cExcludedFields)
            Throw Error.ArgumentError(__FUNCTION__, "FIELDNAMES", __VfpStr(VFPErrors.INVALID_FIELD_SPEC))
        ENDIF
    ENDIF
    lAll := ALen(aFieldList) == 0 .and. String.IsNullOrEmpty(cIncludedFields)
    LOCAL fCount as DWORD
    fCount := FCount()
    FOR VAR nFld := 1u to fCount
        LOCAL lInclude AS LOGIC
        LOCAL cType := NIL as USUAL
        VoDb.FieldInfo(DBS_TYPE, nFld,@cType)
        SWITCH (STRING) cType
            CASE "M"
                lInclude := lIncludeMemo
            CASE "G"
                lInclude := FALSE
            OTHERWISE
                lInclude := TRUE
        END SWITCH
        IF lInclude
            LOCAL oVar := NULL AS OBJECT
            VoDb.FieldInfo( DBS_STRUCT, nFld, REF oVar)
            VAR oFld := (RddFieldInfo) oVar
            IF oFld:Alias != NULL
                allfields:Add(oFld:Alias:ToUpperInvariant())
            ELSE
                allfields:Add(oFld:Name:ToUpperInvariant())
            ENDIF
        ENDIF
    NEXT
    IF lAll
        selected:AddRange(allfields)
    ELSEIF ALen(aFieldList) > 0
        FOREACH cName AS STRING in aFieldList
            var cField := cName:ToUpper()
            IF allfields:IndexOf(cField) == -1
                Throw Error.ArgumentError(__FUNCTION__, "FIELDNAME", __VfpStr(VFPErrors.INVALID_FIELDNAME, cField))
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
            result := DbCopy(cTargetFile, acFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest)
        CASE "foxplus"
        CASE "fox2x"
            result := DbCopy(cTargetFile, acFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest,"DBFCDX")

        OTHERWISE
            // Other Formats
            // DIF,MOD,SYLK,WK1,WKS,WR1,WRK,XLS,XL5
            Throw NotSupportedException{__VfpStr(VFPErrors.INVALID_FORMAT, "output", cOutPutType)}
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




FUNCTION DbCopyToArray(aFieldList, cbForCondition, cbWhileCondition, nNext,nRecord, lRest, lNoOptimize) AS ARRAY CLIPPER
    VAR aFields := __BuildFieldList(aFieldList, FALSE)
    LOCAL aResult := {} AS ARRAY

    DbEval( {|| AAdd(aResult, DbCopyToArraySingleRecord(aFields)) }, cbForCondition, cbWhileCondition, nNext,nRecord, lRest, lNoOptimize )

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
        THROW Error.ArgumentError(__FUNCTION__ , nameof(aValues), __VfpStr(VFPErrors.MULTI_DIM_EXPECTED,nameof(aValues))  , 1, {aValues})
    ENDIF
    VAR aFields := __BuildFieldList(aFieldList, FALSE)
    LOCAL oForCondition   := NULL   AS ICodeblock
    IF cbForCondition IS ICodeblock
        oForCondition := (ICodeblock) cbForCondition
    ENDIF
    // Check to see if the array is multi dimensional and if the # of fields matches.
    IF ALen(aValues) > 0
        FOREACH var u in (ARRAY) aValues
            IF !IsArray(u)
                THROW Error.ArgumentError(__FUNCTION__ , nameof(aValues), __VfpStr(VFPErrors.MULTI_DIM_EXPECTED,nameof(aValues)), 1, {aValues})
            ENDIF
            local aElement := u as ARRAY
            IF ALen(aElement) < aFields:Count
                THROW Error.ArgumentError(__FUNCTION__ , nameof(aValues), __VfpStr(VFPErrors.SUBARRAY_TOO_SMALL ) , 1, {u})
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
            Throw NotSupportedException{__VfpStr(VFPErrors.INVALID_FORMAT, "input", cInPutType)}
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


FUNCTION DbSort(cTargetFile, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, ;
    lRest, lNoOpt, lDesc, aFields)   AS LOGIC CLIPPER
    // Todo: Implement field list
    RETURN DbSort(cTargetFile, acFields, cbForCondition, cbWhileCondition, nNext, nRecord, ;
    lRest, lNoOpt, lDesc)


