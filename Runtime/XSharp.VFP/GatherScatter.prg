//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

// Support functions for the GATHER and SCATTER UDCs
//SCATTER [FIELDS] [FieldNameList | LIKE Skeleton
//   | EXCEPT Skeleton] [MEMO] [BLANK]
//   TO ArrayName | TO ArrayName | MEMVAR
//   | NAME ObjectName [ADDITIVE]

// GATHER FROM ArrayName | MEMVAR | NAME ObjectName
//     [FIELDS] FieldList | LIKE Skeleton | EXCEPT Skeleton]
//   [MEMO]

// APPEND FROM ARRAY ArrayName [FOR lExpression] 
// [FIELDS] FieldList | LIKE Skeleton | EXCEPT Skeleton

// THere are several variations of these commands:
// 1) Explicit fields list
// 2) Skeleton of fields to be INcluded
// 3) Skeleton of fields to be EXcluded
// 4) no fields
// 5) MEMO clause which includes MEMO fields in the gather/scatter 
// Note that LIKE and EXCEPT can be combined
// APPEND FROM ARRAY aMyArray FIELDS LIKE A*,P* EXCEPT PARTNO*
// SCATTER FIELDS LIKE A*,P* EXCEPT PARTNO* TO myArray

// so the recommented parameters are:
//  1) List of field names
//  2) Include Skeleton
//  3) Exclude Skeleton
//  4) IncludeMemo

// The targets/source  of the Scatter/Gather can be:
//  1) Array
//  2) MemVar
//  3) Object (Add Property). Creates a new object with Scatter. 
//  SCATTER NAME oTest BLANK gets translated to something like oTest := __ScatterObject(...)

USING System.Collections.Generic  
USING System.Diagnostics
USING System.Reflection
USING XSharp.RDD.Support

[DebuggerDisplay("{Name,nq}={Value}")];
INTERNAL STRUCTURE NameValuePair
    INTERNAL Name  as STRING
    INTERNAL @@Value AS USUAL
END STRUCTURE	

INTERNAL FUNCTION __GetFieldValues(aFieldList AS ARRAY, cIncludedFields AS STRING, ;
    cExcludedFields AS STRING, lIncludeMemo AS LOGIC, lBlank AS LOGIC) AS NameValuePair[]
    var fields :=  __BuildFieldList(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo)
    var values := List<NameValuePair>{}	
    FOREACH var cName in fields
        VAR uValue := __FieldGet(cName)
        if lBlank
            uValue := EmptyUsual(UsualType(uValue))
        endif
        values:Add( NameValuePair{} {Name := cName, Value := uValue})
    NEXT
RETURN values:ToArray()

INTERNAL FUNCTION __GetAllElements(cSource as STRING) AS STRING[]
    cSource := cSource:ToUpperInvariant()
RETURN cSource:Split(<Char>{c' ',c',',c'\t'}, StringSplitOptions.RemoveEmptyEntries)

INTERNAL FUNCTION __BuildFieldList(aFieldList AS ARRAY, cIncludedFields AS STRING, cExcludedFields AS STRING, lIncludeMemo AS LOGIC) AS IList<String>
    VAR allfields := List<string>{}	// Contains all fields in UPPER case
    VAR selected := List<string>{}
    LOCAL lAll as LOGIC
    if ALen(aFieldList) > 0
        IF !String.IsNullOrEmpty(cIncludedFields) .or. !String.IsNullOrEmpty(cExcludedFields)
            Throw Error.ArgumentError(__FUNCTION__, "FIELDNAMES", i"You cannot combine both a field list and an include mask")
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
                Throw Error.ArgumentError(__FUNCTION__, "FIELDNAME", i"Field '{cName}' does not exist")
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

FUNCTION __ScatterMemVar(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo, lBlank) AS LOGIC CLIPPER
    VAR aFields := __GetFieldValues(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo, lBlank)
    FOREACH var oField in aFields
        __MemVarDecl(oField:Name, TRUE)
        __MemVarPut(oField:Name, oField:Value)
    NEXT
    RETURN TRUE
    
    
    
FUNCTION __GatherMemVar(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo) AS LOGIC CLIPPER
    VAR aFields := __BuildFieldList(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo)
    FOREACH var cField in aFields
        __FieldSet(cField, MemVarGet(cField))
    NEXT
    RETURN TRUE
    #pragma options ("az", on)
    
FUNCTION __ScatterArray(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo, lBlank) AS ARRAY CLIPPER
    VAR aFields := __GetFieldValues(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo, lBlank)
    VAR aResult := {}
    FOREACH var oField in aFields
        AAdd(aResult, oField:Value)
    NEXT
    RETURN aResult
    
    
FUNCTION __GatherArray(uSource, aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo) AS LOGIC CLIPPER
    VAR aFields := __BuildFieldList(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo)
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
    
FUNCTION __ScatterObject(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo, lBlank, cObject, lAdditive) AS OBJECT CLIPPER
    VAR aFields := __GetFieldValues(aFieldList, cIncludedFields, cExcludedFields, lIncludeMemo, lBlank)    
    LOCAL oResult := NULL_OBJECT as OBJECT
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
            oEmpty:__AddProperty(oField:Name, oField:Value)
        NEXT
    ELSE
        FOREACH var oField in aFields
            AddProperty(oResult, oField:Name, oField:Value)
        NEXT
    ENDIF
    RETURN oResult
    
    
FUNCTION __GatherObject(oObject, cFieldList, cIncludedFields, cExcludedFields, lIncludeMemo ) AS LOGIC CLIPPER
    VAR aFields := __BuildFieldList(cFieldList, cIncludedFields, cExcludedFields, lIncludeMemo)    
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
    


