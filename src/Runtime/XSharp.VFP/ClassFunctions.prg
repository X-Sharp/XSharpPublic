//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection
USING System.Collections.Generic

/// <include file="VFPRuntimeDocs.xml" path="Runtimefunctions/addproperty/*" />
[FoxProFunction("ADDPROPERTY", FoxFunctionCategory.ClassAndObject, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION AddProperty (oObjectName AS OBJECT, cPropertyName AS STRING, eNewValue := NIL AS USUAL) AS LOGIC
    if (object) oObjectName is XSharp.IDynamicProperties2 var oDynamic
        oDynamic:_AddProperty(cPropertyName, eNewValue,1,"")
    else
        Send(oObjectName,#AddProperty,cPropertyName, eNewValue)
    endi
    return true


/// <include file="VFPRuntimeDocs.xml" path="Runtimefunctions/removeproperty/*" />
[FoxProFunction("REMOVEPROPERTY", FoxFunctionCategory.ClassAndObject, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
function RemoveProperty( oObjectName as object, cPropertyName as string) as logic
    if (object) oObjectName is XSharp.IDynamicProperties2 var oDynamic
        oDynamic:_RemoveProperty(cPropertyName)
    ELSE
        Send(oObjectName,#RemoveProperty,cPropertyName)
    ENDIF
    RETURN TRUE


/// <include file="VFPRuntimeDocs.xml" path="Runtimefunctions/getpem/*" />
[FoxProFunction("GETPEM", FoxFunctionCategory.ClassAndObject, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.High)];
FUNCTION GETPEM( uObject as USUAL, cProperty as STRING) as USUAL
    if uObject is string var strObject
        return NIL
    endif
    return IVarGet(uObject, cProperty)

/// <include file="VFPDocs.xml" path="Runtimefunctions/compobj/*" />
[FoxProFunction("COMPOBJ", FoxFunctionCategory.ClassAndObject, FoxEngine.LanguageCore, FoxFunctionStatus.Full, FoxCriticality.Medium)];
FUNCTION CompObj (oExpression1 AS OBJECT, oExpression2 AS OBJECT) AS LOGIC
    RETURN CompObjHelper(oExpression1, oExpression2, 0)

INTERNAL FUNCTION CompObjHelper(oExpression1 AS OBJECT, oExpression2 AS OBJECT, nDepth AS INT) AS LOGIC
    IF oExpression1 == NULL_OBJECT .AND. oExpression2 == NULL_OBJECT
        RETURN TRUE
    ENDIF

    IF oExpression1 == NULL_OBJECT .OR. oExpression2 == NULL_OBJECT
        RETURN FALSE
    ENDIF

    // Guard against cyclic object graphs causing infinite recursion.
    // Returning TRUE (assume equal) at the depth limit mirrors the VFP behaviour of
    // not recursing indefinitely; real cycles are extremely rare in practice.
    LOCAL CONST MAX_COMPOBJ_DEPTH := 50 AS INT
    IF nDepth > MAX_COMPOBJ_DEPTH
        RETURN TRUE
    ENDIF

    // Build a name→PropertyInfo map for oExpression2's comparable (readable, non-indexed) properties
    VAR dict2 := Dictionary<STRING, PropertyInfo>{}
    FOREACH VAR oP IN oExpression2:GetType():GetProperties(BindingFlags.Public | BindingFlags.Instance)
        IF oP:CanRead .AND. oP:GetIndexParameters():Length == 0
            dict2[oP:Name] := oP
        ENDIF
    NEXT

    // Iterate oExpression1's comparable properties and compare with O(1) lookup into dict2
    LOCAL nComparableCount := 0 AS INT
    FOREACH VAR oProp1 IN oExpression1:GetType():GetProperties(BindingFlags.Public | BindingFlags.Instance)
        IF !oProp1:CanRead .OR. oProp1:GetIndexParameters():Length > 0
            LOOP
        ENDIF

        nComparableCount++

        IF !dict2:ContainsKey(oProp1:Name)
            RETURN FALSE
        ENDIF

        VAR oProp2 := dict2[oProp1:Name]

        VAR uVal1 := oProp1:GetValue(oExpression1)
        VAR uVal2 := oProp2:GetValue(oExpression2)

        IF uVal1 == NULL .AND. uVal2 == NULL
            LOOP
        ENDIF

        IF uVal1 == NULL .OR. uVal2 == NULL
            RETURN FALSE
        ENDIF

        IF uVal1 IS OBJECT VAR o1 .AND. uVal2 IS OBJECT VAR o2
            VAR oType := o1:GetType()
            IF oType:IsValueType .OR. oType == typeof(STRING)
                IF !uVal1:Equals(uVal2)
                    RETURN FALSE
                ENDIF
            ELSE
                IF !CompObjHelper(o1, o2, nDepth + 1)
                    RETURN FALSE
                ENDIF
            ENDIF
        ELSEIF !uVal1:Equals(uVal2)
            RETURN FALSE
        ENDIF
    NEXT

    // If oExpression2 has more comparable properties than oExpression1, they differ
    IF nComparableCount != dict2:Count
        RETURN FALSE
    ENDIF

    RETURN TRUE

