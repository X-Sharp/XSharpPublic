//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Diagnostics
USING System.Linq

/// <summary>
/// The Xbase++ DataObject class.
/// </summary>
[DebuggerTypeProxy(TYPEOF(DataObjectDebugView))];
CLASS XSharp.XPP.DataObject INHERIT XSharp.XPP.Abstract IMPLEMENTS IDynamicProperties
    PRIVATE _fields  AS Dictionary<STRING, USUAL>
    PRIVATE _methods AS Dictionary<STRING, USUAL>

    CONSTRUCTOR()
        SUPER()
        _fields  := Dictionary<STRING, USUAL>{StringComparer.OrdinalIgnoreCase}
        _methods := Dictionary<STRING, USUAL>{StringComparer.OrdinalIgnoreCase}

    /// <summary>
    /// Checks if the object has a particular member variable
    /// </summary>
    /// <param name="cName">is a character string containing the symbolic identifier of a member variable.</param>
    /// <returns>The return value is .T. (true) if the object has the member variable &lt;cVarName&gt;, otherwise it returns .F. (false). </returns>
    PUBLIC METHOD IsMemberVar(cName AS STRING) AS LOGIC
        IF SELF:_fields:ContainsKey(cName)
            RETURN TRUE
        ENDIF
        RETURN FALSE
    /// <include file="XPPComments.xml" path="Comments/ClassDescribe/*" />
    OVERRIDE METHOD ClassDescribe(uInfo) AS ARRAY CLIPPER
        local aResult as ARRAY
        local nInfo as LONG
        IF ! IsNumeric(uInfo)
            nInfo := CLASS_DESCR_ALL
        ELSE
            nInfo := uInfo
        ENDIF
        aResult := Super:ClassDescribe(CLASS_DESCR_ALL)
        IF nInfo == CLASS_DESCR_ALL .or. nInfo == CLASS_DESCR_MEMBERS
            FOREACH var fld in _fields
                AAdd(aResult[3], {fld:Key, VAR_INSTANCE+CLASS_EXPORTED, typeof(USUAL)})
            NEXT
        ENDIF
        IF nInfo == CLASS_DESCR_ALL .or. nInfo == CLASS_DESCR_METHODS
            FOREACH var met in _methods
                AAdd(aResult[4], {met:Key, METHOD_INSTANCE+CLASS_EXPORTED, met:Value, NIL, typeof(USUAL)})
            NEXT
        ENDIF
        SWITCH nInfo
        CASE CLASS_DESCR_ALL
            RETURN aResult
        CASE CLASS_DESCR_CLASSNAME
            RETURN aResult[1]
        CASE CLASS_DESCR_SUPERCLASSES
            RETURN aResult[2]
        CASE CLASS_DESCR_MEMBERS
            RETURN aResult[3]
        CASE CLASS_DESCR_METHODS
            RETURN aResult[4]
        CASE CLASS_DESCR_SUPERDETAILS
            RETURN {}
        END SWITCH
        RETURN {}

    /// <include file="XPPComments.xml" path="Comments/NoIvarPut/*" />
    OVERRIDE METHOD NoIvarPut(cName AS STRING, uValue AS USUAL) AS VOID
        SELF:_fields[cName] := uValue
        RETURN

    /// <include file="XPPComments.xml" path="Comments/NoIvarGet/*" />
    OVERRIDE METHOD NoIvarGet(cName AS STRING) AS USUAL
        IF SELF:_fields:ContainsKey(cName)
            RETURN SELF:_fields[cName]
        ENDIF
        RETURN NIL

    VIRTUAL METHOD GetPropertyNames() AS STRING[]
        return _fields:Keys:ToArray()

    /// <summary>Creates a dependent shallow copy of this instance </summary>
    /// <returns>This method returns the new DataObject instance. </returns>
    VIRTUAL METHOD Copy() AS DataObject
        LOCAL oNew AS DataObject
        oNew := DataObject{}
        FOREACH VAR element IN _fields
            oNew:_fields:Add(element:Key, element:Value)
        NEXT
        RETURN oNew

    /// <summary>Merges exported member variables from another object into this instance.</summary>
    /// <param name="oNewObject">DataObject to copy the member variables from.</param>
    /// <param name="cMessagePrefix">Optional prefix that will be added to all fields in the DataObject.</param>
    /// <returns>This method returns the DataObject (self).</returns>

    VIRTUAL METHOD Merge(oNewObject, cMessagePrefix) AS DataObject CLIPPER
        LOCAL oObject := oNewObject AS OBJECT
        EnforceType(REF cMessagePrefix, STRING)
        IF oObject IS DataObject var oDataObject
            FOREACH VAR element IN oDataObject:_fields
                IF !SELF:_fields:ContainsKey(cMessagePrefix+element:Key)
                    SELF:_fields:Add(cMessagePrefix+element:Key, element:Value)
                ENDIF
            NEXT
        ENDIF
        RETURN SELF


    /// <summary>Defines a dynamic method. </summary>
    /// <param name="cName">The name of the method to be defined.</param>
    /// <param name="uAction">A Function name or codeblock that must be executed when the method is called.</param>
    /// <returns>This method returns the DataObject (self).</returns>
    /// <remarks>Dynamic methods in X# are called from the NoMethod method inside the DataObject class.</remarks>
    VIRTUAL METHOD DefineMethod(cName AS STRING, uAction AS USUAL) AS OBJECT
        SELF:_methods[cName] := uAction
        RETURN SELF

    /// <include file="XPPComments.xml" path="Comments/NoMethod/*" />
    OVERRIDE METHOD NoMethod(cName, uParams) AS USUAL CLIPPER
        LOCAL aParams AS USUAL[]
        LOCAL cMethod AS STRING
        cMethod := RuntimeState.NoMethod
        IF SELF:_methods:ContainsKey(cMethod)
            aParams := USUAL[]{ PCOunt() +1}
            // The pseudo function _ARGS() returns the Clipper arguments array
            System.Array.Copy(_ARGS(), 0, aParams, 1, PCount())
            aParams[1] := SELF
            LOCAL action AS USUAL
            action := SELF:_methods[cMethod]
            IF IsString(action)
                RETURN _CallClipFunc(action, aParams)
            ELSEIF IsCodeBlock(action)
                LOCAL oBlock := action AS CODEBLOCK
                RETURN oBlock:Eval(aParams)
            ENDIF
        ENDIF
        RETURN NIL

    INTERNAL CLASS DataObjectDebugView
        PRIVATE _value AS XSharp.XPP.DataObject
        PRIVATE PROPERTY _count as INT GET _value:_fields:Count
        INTERNAL CONSTRUCTOR (d AS XSharp.XPP.DataObject)
            _value := d

        [DebuggerBrowsable(DebuggerBrowsableState.Collapsed)] ;
        [DebuggerDisplay("Count = {_count}", Type:="Dynamic Properties")];
        PUBLIC PROPERTY Properties AS IList<Prop>
        GET
            VAR result := List<Prop>{}
            FOREACH var item in _value:_fields
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
