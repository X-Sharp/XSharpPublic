//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// See FoxProCmd.xh for the UDCs that are used to call these functions

USING System.Collections.Generic
USING System.Diagnostics
USING System.Reflection
USING XSharp.RDD.Support
USING XSharp.Internal



INTERNAL FUNCTION __GetFieldValues(aFieldList IN USUAL, lIncludeMemo AS LOGIC, lBlank AS LOGIC) AS NameValuePair[]
    var fields :=  __BuildFieldList(aFieldList, lIncludeMemo)
    var values := List<NameValuePair>{}
    FOREACH var cName in fields
        VAR uValue := __FieldGet(cName)
        if lBlank
            uValue := EmptyUsual(UsualType(uValue))
        endif
        values:Add( NameValuePair{cName, uValue})
    NEXT
RETURN values:ToArray()

INTERNAL FUNCTION __GetAllElements(cSource as STRING) AS STRING[]
    cSource := cSource:ToUpperInvariant()
RETURN cSource:Split(<Char>{c' ',c',',c'\t'}, StringSplitOptions.RemoveEmptyEntries)

INTERNAL FUNCTION __BuildFieldList(aFieldList IN USUAL, lIncludeMemo as LOGIC) AS IList<String>
VAR selected := List<string>{}
IF IsArray(aFieldList) .and. ALen(aFieldList) > 0
    FOREACH cFld AS STRING in aFieldList
        selected:Add(cFld:ToString())
    NEXT
	RETURN selected
ENDIF
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
                selected:Add(oFld:Alias:ToUpperInvariant())
            ELSE
                selected:Add(oFld:Name:ToUpperInvariant())
            ENDIF
        ENDIF
    NEXT
return selected




FUNCTION __ScatterMemVar(aFieldList, lBlank) AS LOGIC CLIPPER
    VAR aFields := __GetFieldValues(aFieldList, FALSE, lBlank)
    FOREACH var oField in aFields
        __MemVarDecl(oField:Name, TRUE)
        __MemVarPut(oField:Name, oField:Value)
    NEXT
    RETURN TRUE

FUNCTION __GatherMemVar(aFieldList) AS LOGIC CLIPPER
    VAR aFields := __BuildFieldList(aFieldList, FALSE)
    FOREACH var cField in aFields
        __FieldSet(cField, MemVarGet(cField))
    NEXT
    RETURN TRUE
    #pragma options ("az", on)

FUNCTION __ScatterArray(uSource, aFieldList, lBlank) AS ARRAY CLIPPER
    VAR aFields := __GetFieldValues(aFieldList, FALSE, lBlank)
    VAR nLen    := (DWORD) aFields:Length

    LOCAL aResult AS ARRAY
    LOCAL aSource := NULL_ARRAY as ARRAY
    IF IsArray(uSource)
        aSource := uSource
        if aSource IS __FoxArray VAR aFox
            nLen := Min(ALen(aFox, 0), nLen)
        ELSE
            nLen := Min(ALen(aSource), nLen)
        ENDIF
        aResult := aSource
    ELSE
        aResult := __FoxArray{(DWORD) nLen}
    ENDIF

    FOR VAR nI := 0 to nLen-1
        VAR oField := aFields[nI]
        aResult[nI] := oField:Value
    NEXT
    RETURN aResult


FUNCTION __GatherArray(uSource, aFieldList) AS LOGIC CLIPPER
    VAR aFields := __BuildFieldList(aFieldList, FALSE)
    VAR current := 0
    IF ! IsArray(uSource)
        RETURN FALSE
    ENDIF
    LOCAL aSource := uSource as ARRAY
    var found    := FALSE
    FOREACH var cField in aFields
        if aSource:Length > current
            __FieldSet(cField, aSource[current++])
            found    := TRUE

        endif
    NEXT
    RETURN found

    #pragma options ("az", default)

[NeedsAccessToLocals(FALSE)];
FUNCTION __ScatterObject(aFieldList, lBlank, cObject, lAdditive) AS OBJECT CLIPPER
    LOCAL oResult := NULL_OBJECT as OBJECT
    VAR aFields := __GetFieldValues(aFieldList, FALSE, lBlank)
    IF IsArray(aFieldList)
        aFields := aFieldList
    ENDIF
    IF !IsLogic(lAdditive)
        lAdditive := FALSE
    ENDIF
    IF IsString(cObject) .and. lAdditive
        TRY
            LOCAL uObject := VarGet(cObject) AS USUAL
            IF IsObject(uObject)
                oResult := uObject
            ENDIF
        CATCH
            oResult := NULL_OBJECT
        END TRY
    ENDIF
    IF oResult == NULL_OBJECT
        oResult := XSharp.VFP.Empty{}
    ENDIF
    IF oResult IS XSharp.VFP.Empty VAR oEmpty
        FOREACH var oField in aFields
            oEmpty:_AddProperty(oField:Name, oField:Value)
        NEXT
    ELSE
        FOREACH var oField in aFields
            AddProperty(oResult, oField:Name, oField:Value)
        NEXT
    ENDIF
    RETURN oResult


FUNCTION __GatherObject(oObject, aFieldList ) AS LOGIC CLIPPER
    VAR aFields := __BuildFieldList(aFieldList, FALSE)
    LOCAL oLocal as OBJECT
    IF ! IsObject(oObject)
        RETURN FALSE
    ENDIF
    oLocal := oObject
    IF oLocal IS XSharp.IDynamicProperties VAR oDynamic
        var props := List<String>{}
        props:AddRange(oDynamic:GetPropertyNames())
        FOREACH var cField in aFields
            if props:IndexOf(cField:ToUpper()) >= 0
                var value := oDynamic:NoIvarGet(cField)
                __FieldSet(cField, value)
            endif
        NEXT
    ELSE
        FOREACH var cField in aFields
            var value := IVarGet(oLocal, cField)
            __FieldSet(cField, value)
        NEXT
    ENDIF
    RETURN TRUE



