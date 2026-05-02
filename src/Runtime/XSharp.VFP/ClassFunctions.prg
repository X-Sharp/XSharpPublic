//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection

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
    IF oExpression1 == NULL_OBJECT .AND. oExpression2 == NULL_OBJECT
        RETURN TRUE
    ENDIF

    IF oExpression1 == NULL_OBJECT .OR. oExpression2 == NULL_OBJECT
        RETURN FALSE
    ENDIF

    VAR aProps1 := oExpression1:GetType():GetProperties(BindingFlags.Public | BindingFlags.Instance)
    VAR aProps2 := oExpression2:GetType():GetProperties(BindingFlags.Public | BindingFlags.Instance)

    IF aProps1:Length != aProps2:Length
        RETURN FALSE
    ENDIF

    FOREACH VAR oProp1 IN aProps1
        IF !oProp1:CanRead
            LOOP
        ENDIF

        IF oProp1:GetIndexParameters():Length > 0
            LOOP
        ENDIF

        LOCAL oProp2 := NULL AS PropertyInfo
        FOREACH VAR oP IN aProps2
            IF oP:Name == oProp1:Name
                oProp2 := oP
                EXIT
            ENDIF
        NEXT

        IF oProp2 == NULL .OR. !oProp2:CanRead
            RETURN FALSE
        ENDIF

        IF oProp2:GetIndexParameters():Length > 0
            LOOP
        ENDIF

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
                IF !CompObj(o1, o2)
                    RETURN FALSE
                ENDIF
            ENDIF
        ELSEIF !uVal1:Equals(uVal2)
            RETURN FALSE
        ENDIF
    NEXT

    RETURN TRUE

