//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
#command VFPPROP <cName> <cType> => PROPERTY <cName> AS <cType> GET _GetProperty(<"cName">) SET _SetProperty(<"cName">, value)
#command VFPPROP <cName> <cType> <cLit> => PROPERTY <cName> AS <cType> GET _GetProperty(<(cLit)>) SET _SetProperty(<(cLit)>, value)

BEGIN NAMESPACE XSharp.VFP

	CLASS Abstract
        PROTECTED _aProperties AS Dictionary<String, VfpProperty>
 
    CONSTRUCTOR()
        _aProperties :=  Dictionary<String, VfpProperty>{StringComparer.OrdinalIgnoreCase}
        _InitCompileTimeProperties()
        SELF:Name := ""
        SELF:Class := ClassName(SELF)
        SELF:ParentClass := SELF:GetType():BaseType:Name
        SELF:ClassLibrary := SELF:GetType():Assembly:ToString()
        SELF:Comment := ""
        SELF:BaseClass := ""
        RETURN
    METHOD _InitCompileTimeProperties() as VOID
        VAR aProps := SELF:GetType():GetProperties()
        FOREACH VAR oProp IN aProps
            var met := oProp:GetGetMethod(TRUE)
            var nVis := IIF(met:IsPublic,1 ,iif(met:IsPrivate,3,2))
            
            VAR VfpProperty :=VfpProperty{oProp:Name, NIL, nVis}
            _aProperties:Add(vfpProperty:Name, vfpProperty)
        NEXT
        RETURN
    VFPPROP Name STRING 
    VFPPROP BaseClass STRING 
    VFPPROP @@Class STRING "Class"
    VFPPROP ClassLibrary STRING 
    VFPPROP Comment STRING 
    VFPPROP Parent OBJECT
    VFPPROP ParentClass STRING 
 
    #region Property related
    METHOD AddProperty(cPropertyName, uValue, nVisibility, cDescription) AS LOGIC
        local oProp as VfpProperty
        oProp := VfpProperty{cPropertyName, uValue}
        IF IsNumeric(nVisibility)
            oProp:Visibility := (PropertyVisibilty) nVisibility
        ENDIF
        IF IsString(cDescription)
            oProp:Description := cDescription
        ENDIF
        _aProperties[cPropertyName] := oProp
        RETURN FALSE

    [Obsolete("This method is not supported in X#")];
    METHOD ResetToDefault(cName) AS VOID
        RETURN
    
    #endregion

 
    #region Designer related

    [Obsolete("This method is not supported in X#")];
    VIRTUAL METHOD ReadExpression(cPropertyName) AS STRING
       RETURN ""

    [Obsolete("This method is not supported in X#")];
    VIRTUAL METHOD WriteExpression(cPropertyName, uValue ) AS LOGIC
       RETURN FALSE

    [Obsolete("This method is not supported in X#")];
    VIRTUAL METHOD ReadMethod() as STRING STRICT
        RETURN ""

    [Obsolete("This method is not supported in X#")];
    VIRTUAL METHOD SaveAsClass(cClassLib, cClass, cDescription) AS LOGIC
        RETURN FALSE

    [Obsolete("This method is not supported in X#")];
    VIRTUAL METHOD WriteMethod(cMethodName, cMethodText, lCreateMethod, nVisibility, cDescription) as STRING STRICT
        RETURN ""

    #endregion
        


    #region Implementations
    PROTECTED METHOD _GetProperty(cName AS STRING)
        IF _aProperties:ContainsKey(cName)
            RETURN _aProperties[cName]:Value
        ELSE
           THROW Exception{"Property "+cName+" not found"}
        ENDIF

    PROTECTED METHOD _SetProperty(cName AS STRING, uValue as USUAL) AS VOID
        IF _aProperties:ContainsKey(cName)
            _aProperties[cName]:Value := uValue
            
        ELSE
           THROW Exception{"Property "+cName+" not found"}
        ENDIF
    
    #endregion
        CLASS VfpProperty
             PROPERTY Name AS STRING AUTO
             PROPERTY Value as USUAL AUTO
             PROPERTY Visibility as PropertyVisibilty AUTO
             PROPERTY Description AS STRING AUTO
             CONSTRUCTOR(cName as STRING, uValue as USUAL, nVis as INT)
                    SELF:Name := cName
                    SELF:Value := IIF(IsNil(uValue), FALSE, uValue)
                    SELF:Visibility := (PropertyVisibilty) nVis
             CONSTRUCTOR(cName as STRING, uValue as USUAL)
                    SELF(cName, uValue, PropertyVisibilty.Public)
        END CLASS
    END CLASS
END NAMESPACE 
