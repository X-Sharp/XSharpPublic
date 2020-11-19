//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <summary>Creates an object from a class definition or an Automation-enabled application.</summary>
/// <param name="cClassName">Specifies the class or OLE object from which the new object is created.</param>
/// <param name="_args">These optional parameters are used to pass values to the Init event procedure for the class.
/// The Init event is executed when you issue CREATEOBJECT( ) and allows you to initialize the object.</param>
/// <returns>The object that was created</returns>
/// <seealso cref='O:XSharp.RT.Functions.CreateInstance' >CreateInstance</seealso>
using System.Collections.Generic

FUNCTION CreateObject(cClassName, _args ) AS OBJECT CLIPPER
    // The pseudo function _ARGS() returns the Clipper arguments array
    RETURN CreateInstance(_ARGS())
    
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/createobjectex/*" />
FUNCTION CreateObjectEx(cClsIdOrcProgId, cComputerName , cIID ) AS OBJECT CLIPPER
    // The pseudo function _ARGS() returns the Clipper arguments array
    RETURN CreateInstance(_ARGS())
    
    
INTERNAL PROCEDURE RddInit() AS VOID _INIT3
    // Make sure that the VFP dialect has the DBFVFP driver as default RDD
    RddSetDefault("DBFVFP")
    RuntimeState.SetValue(Set.CollateFox,"MACHINE")
    RuntimeState.SetValue(Set.MemoWidth, 50)
    RuntimeState.SetValue(Set.Near, FALSE)
    RuntimeState.SetValue(Set.SqlAnsi, FALSE)
    RuntimeState.SetValue(Set.FoxLock, TRUE)
    RuntimeState.AutoOrder := 0
    RuntimeState.Eof :=  TRUE
    RuntimeState.MemoBlockSize := 64
RETURN 



Function SetFoxCollation(cCollation as STRING) AS STRING
    local cOld := RuntimeState.GetValue<STRING>(Set.CollateFox) AS STRING
    local aAllowed as STRING[]
    LOCAL lOk := FALSE as LOGIC
    LOCAL cValue := cCollation as STRING
    aAllowed := System.Enum.GetNames(typeof(XSharp.FoxCollations))
    cValue := cValue:Trim():ToUpper()
    FOREACH VAR cEnum in aAllowed
        IF String.Compare(cValue, cEnum, StringComparison.OrdinalIgnoreCase) == 0
            lOk := TRUE
            EXIT
        ENDIF
    NEXT
    IF lOk
        RuntimeState.SetValue(Set.CollateFox,cValue)
    ELSE
        local oError as Error
        oError := Error.ArgumentError(__FUNCTION__, nameof(cCollation), 1, {cCollation})
        oError:Description := "Collating sequence '"+cCollation+"' is not found"
        oError:Throw()
    ENDIF
    RETURN cOld
    
    
/// <include file="VfpRuntimeDocs.xml" path="Runtimefunctions/icase/*" />

function ICase(lCondition, eResult, lCondition2, eResult2, eOtherwiseResult) as usual CLIPPER
    LOCAL nCount := PCount() AS LONG
    // loop through the actual parameters. The odd parameters should be logic
    // the even parameters are return values for their siblings.
    for var nI := 1 to nCount-1 step 2
        local cond := _GetFParam(nI) as logic
        if cond
            return _GetFParam(nI+1) 
        endif
    next
    // no conditions are met, if the # of parameters is odd then return the last value
    if nCount % 2 == 1
        return _GetFParam(nCount)
    endif
    // the # of parameters is even. When >= 2 then get the type of parameter 2 and return an empty value
    if PCount() >= 2
        var type := UsualType(_GetFParam(2))
        return EmptyUsual(type)
    ENDIF
    // when they call this function with < 2 parameters then we have no idea what return type they expect...
    return NIL
    
    
FUNCTION __VfpStr( resid AS DWORD ) AS STRING
    // Strings are stored in a Managed resource with a name
    // the name matches the enum names
    // convert the id to the enum and get its name
    LOCAL strId  AS STRING
    LOCAL strMessage AS STRING
    strId := Enum.GetName( TYPEOF(VFPErrors) , resid)
    IF !String.IsNullOrEmpty(strId)
            strMessage := XSharp.Messages.GetString( strId )
            IF String.IsNullOrEmpty( strMessage )
                strMessage := ": canot load string resource '" + strId + "'"
        ENDIF
    ELSE
        strMessage := "Cannot find string for error number "+resid:ToString()
    ENDIF
    RETURN strMessage
    
    
    
    
/// <include file="VFPDocs.xml" path="Runtimefunctions/vartype/*" />
FUNCTION VarType( eExpression AS USUAL) AS STRING
    RETURN VarType(eExpression, FALSE)
    
    
/// <include file="VFPDocs.xml" path="Runtimefunctions/vartype/*" />
FUNCTION VarType( eExpression AS USUAL, lNullDataType AS LOGIC) AS STRING
    IF IsNil(eExpression) .AND. ! lNullDataType
        RETURN "X"
    ENDIF
    RETURN ValType(eExpression)
    
    
    
FUNCTION DbCopyFox(cTargetFile, cType, aFields, includedFields, excludedFields, cbForCondition, ;
    cbWhileCondition, nNext,nRecord, lRest, nCodePage, cDbName, cLongTableName, lCdx, lNoOptimize)   AS LOGIC CLIPPER
    local cOutPutType as STRING
    LOCAL result as LOGIC
    cOutPutType := cType       
    var fields := __BuildFieldList(aFields, includedFields, excludedFields, TRUE)
    var acFields := {}
    foreach var cField in fields
        AAdd(acFields, cField)
    NEXT
    var cDelim := RuntimeState.DelimRDD
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
            Throw NotSupportedException{"The output format "+cOutPutType+" is not available yet"}
        END SWITCH
    FINALLY
        RuntimeState.DelimRDD   := cDelim
    END TRY
    return result



FUNCTION DbCopyDelimFox (cTargetFile, cDelim, cChar, aFields, includedFields, excludedFields, ;
    cbForCondition, cbWhileCondition, nNext,nRecord, lRest, nCodePage, lNoOptimize)   AS LOGIC CLIPPER
    var fields := __BuildFieldList(aFields, includedFields, excludedFields, TRUE)
    var acFields := {}
    foreach var cField in fields
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
	RETURN DbCopyDelim(cTargetFile, cDelim, acFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest)
	



FUNCTION DbCopyToArray(aFieldList, cIncludedFields, cExcludedFields, cbForCondition, cbWhileCondition, nNext,nRecord, lRest, lNoOptimize) AS ARRAY CLIPPER
    VAR aFields := __BuildFieldList(aFieldList, cIncludedFields, cExcludedFields, FALSE)
    VAR aResult := {}

    DbEval( {|| AAdd(aResult, DbCopyToArraySingleRecord(aFields)) }, cbForCondition, cbWhileCondition, nNext,nRecord, lRest )

    RETURN aResult

INTERNAL FUNCTION DbCopyToArraySingleRecord(aFields as IList<string> ) AS ARRAY
    LOCAL result AS ARRAY
    result := ArrayNew(aFields:Count)
    FOR VAR i := 1 to aFields:Count
        result[i] := __FieldGet(aFields[i-1])
    NEXT
    RETURN result


FUNCTION DbAppendFromArray(aValues, aFieldList, cIncludedFields, cExcludedFields, cbForCondition) AS LOGIC CLIPPER
    IF ! IsArray(aValues)
        THROW Error.ArgumentError(__FUNCTION__ , nameof(aValues),"Argument should be a multidimensional array" , 1, {aValues})
    ENDIF
    VAR aFields := __BuildFieldList(aFieldList, cIncludedFields, cExcludedFields, FALSE)
    LOCAL oForCondition   := NULL   AS ICodeblock
    IF cbForCondition IS ICodeblock
        oForCondition := (ICodeblock) cbForCondition
    ENDIF
    // Check to see if the array is multi dimensional and if the # of fields matches.
    IF ALen(aValues) > 0
        FOREACH var u in (ARRAY) aValues
            IF !IsArray(u)
                THROW Error.ArgumentError(__FUNCTION__ , nameof(aValues), "Argument should be a multidimensional array", 1, {aValues})
            ENDIF
            local aElement := u as ARRAY
            IF ALen(aElement) < aFields:Count
                THROW Error.ArgumentError(__FUNCTION__ , nameof(aValues), "Not enough elements in sub array", 1, {u})
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
    local cOutPutType as STRING
    LOCAL result as LOGIC
    cOutPutType := cType       
    var cDelim := RuntimeState.DelimRDD
    TRY
        SWITCH cOutPutType:ToLower()
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
            Throw NotSupportedException{"The input format "+cOutPutType+" is not available yet"}         
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
