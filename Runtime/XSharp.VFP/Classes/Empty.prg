//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.Diagnostics
USING XSharp.Internal
/// <summary>
/// The FoxPro Empty class.
/// </summary>
[AllowLateBinding];
[DebuggerTypeProxy(TYPEOF(EmptyDebugView))];
CLASS XSharp.VFP.Empty

    PROTECTED _Properties AS Dictionary<STRING, USUAL> 
    PROTECTED _Attributes AS Dictionary<STRING, Tuple<PropertyVisibility, STRING> >
    
    CONSTRUCTOR()
        _Properties := Dictionary<STRING, USUAL>{StringComparer.OrdinalIgnoreCase}
        _Attributes := Dictionary<STRING, Tuple<PropertyVisibility, STRING> >{StringComparer.OrdinalIgnoreCase}
        _InitCompileTimeProperties()
        RETURN
        
    METHOD _InitCompileTimeProperties() AS VOID
        VAR aProps := SELF:GetType():GetProperties()
        FOREACH VAR oProp IN aProps
            VAR met := oProp:GetGetMethod(TRUE)
            VAR nVis := IIF(met:IsPublic,1 ,IIF(met:IsPrivate,3,2))
            SELF:AddProperty(oProp:Name, NIL, nVis)
        NEXT
        RETURN
        
        
        #region Property related
    METHOD AddProperty(cPropertyName, uValue, nVisibility, cDescription) AS LOGIC
        local cName as STRING
        // Note that we need to handle the syntax AddProperty("PropertyName(3)") which adds an array property with 3 elements
        EnforceType(cPropertyName, STRING)
        cName := cPropertyName
        IF ! String.IsNullOrEmpty(cName)
            local cDims := String.Empty as STRING
            if cName:EndsWith(")")
                var nPos := cName:IndexOf("(")
                if nPos > 0 // we need at least 1 character
                    cDims := cName:Substring(nPos)
                    cDims := cDims:Substring(1, cDims:Length-2)
                    cName := cName:Substring(0, nPos)
                ENDIF
            ENDIF
            IF ! String.IsNullOrEmpty(cDims)
                VAR aDims := cDims:Split(c',')
                LOCAL dims as USUAL[]
                dims := USUAL[] {aDims:Length}
                FOR var i := 1 to aDims:Length
                    dims[i] := Int32.Parse(aDims[i])
                NEXT
                local aValue as Array
                aValue := ArrayNew(dims)
                AFill(aValue, uValue)
                uValue := aValue
            ENDIF
            _Properties[cName] := uValue
            IF IsNumeric(nVisibility) .or. IsString(cDescription)
                LOCAL nPropVis  := PropertyVisibility.Public as PropertyVisibility
                LOCAL cPropDesc := "" as STRING
                IF IsNumeric(nVisibility)
                    nPropVis := (PropertyVisibility) nVisibility
                ENDIF
                IF IsString(cDescription)
                    cPropDesc := cDescription
                ENDIF
                _Attributes[cName] := Tuple<PropertyVisibility, STRING> {nPropVis, cPropDesc}
            ENDIF
        ENDIF
        RETURN TRUE

    METHOD RemoveProperty(cPropertyName) AS LOGIC
        // FoxPro does not throw an error when non existing properties are removed
        // FoxPro does not require the dimensions when deleting an array property
        IF _Attributes:ContainsKey(cPropertyName)
            _Attributes:Remove(cPropertyName)
        ENDIF
        if _Properties:ContainsKey(cPropertyName)
            _Properties:Remove(cPropertyName)
            RETURN TRUE
        ENDIF
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
    VIRTUAL METHOD ReadMethod() AS STRING STRICT
        RETURN ""
        
    [Obsolete("This method is not supported in X#")];
    VIRTUAL METHOD SaveAsClass(cClassLib, cClass, cDescription) AS LOGIC
        RETURN FALSE
        
    [Obsolete("This method is not supported in X#")];
    VIRTUAL METHOD WriteMethod(cMethodName, cMethodText, lCreateMethod, nVisibility, cDescription) AS STRING STRICT
        RETURN ""
        
    #endregion
    
    VIRTUAL METHOD NoIvarPut(cName AS STRING, uValue AS USUAL) AS USUAL STRICT
        IF _Properties:ContainsKey( cName)
            _Properties[cName] := uValue
        ELSE
            THROW PropertyNotFoundException{cName}
        ENDIF
        RETURN uValue
        
    VIRTUAL METHOD NoIvarGet(cName AS STRING) AS USUAL STRICT
        IF _Properties:ContainsKey(cName)
            RETURN _Properties[cName]
        ELSE
            THROW PropertyNotFoundException{cName}
        ENDIF
        
        
    #region Implementations
    PROTECTED METHOD _GetProperty(cName AS STRING) AS USUAL
        IF _Properties:ContainsKey(cName)
            RETURN _Properties[cName]
        ELSE
            THROW PropertyNotFoundException{cName}
        ENDIF
        
    PROTECTED METHOD _SetProperty(cName AS STRING, uValue AS USUAL) AS VOID
        IF _Properties:ContainsKey(cName)
            _Properties[cName] := uValue
        ELSE
            THROW PropertyNotFoundException{cName}
        ENDIF
        
    #endregion

    INTERNAL CLASS EmptyDebugView
        PRIVATE _value AS Empty
        PRIVATE PROPERTY _count as INT GET _value:_Properties:Count
        INTERNAL CONSTRUCTOR (e AS Empty)
            _value := e

        [DebuggerBrowsable(DebuggerBrowsableState.Collapsed)] ;
        [DebuggerDisplay("Count = {_count}", Type:="Dynamic Properties")];
        PUBLIC PROPERTY Properties AS IList<Prop>
        GET
            VAR result := List<Prop>{}
            FOREACH var item in _value:_Properties
                result:Add( Prop{} {Name := item:Key, @@Value := item:Value})
            NEXT
            RETURN result
        END GET
        END PROPERTY

        [DebuggerDisplay("{Name,nq} = {Value}", Type:="Dynamic Property")];
        INTERNAL CLASS Prop
            INTERNAL PROPERTY Name     AS STRING AUTO
            INTERNAL PROPERTY @@Value  AS USUAL AUTO
        END CLASS

    END CLASS

END CLASS

PUBLIC CLASS XSharp.VFP.PropertyNotFoundException INHERIT Exception
    PUBLIC PROPERTY PropertyName AS STRING AUTO GET PRIVATE SET
    PUBLIC CONSTRUCTOR(name as STRING)
        SUPER("Property '"+name+"' not found")
        SELF:PropertyName := name
        RETURN
END CLASS
