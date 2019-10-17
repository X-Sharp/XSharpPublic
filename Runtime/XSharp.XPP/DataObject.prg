USING System.Collections.Generic



CLASS XSharp.XPP.DataObject INHERIT XSharp.XPP.Abstract
    PRIVATE _fields  AS Dictionary<STRING, USUAL>
    PRIVATE _methods AS Dictionary<STRING, USUAL>

    CONSTRUCTOR()
        SUPER()
        _fields  := Dictionary<STRING, USUAL>{StringComparer.OrdinalIgnoreCase} 
        _methods := Dictionary<STRING, USUAL>{StringComparer.OrdinalIgnoreCase} 

    PUBLIC METHOD IsMemberVar(cName AS STRING) AS LOGIC
        IF SELF:_fields:ContainsKey(cName)
            RETURN TRUE
        ENDIF
        RETURN FALSE
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
                aadd(aResult[3], {fld:Key, VAR_INSTANCE+CLASS_EXPORTED, typeof(USUAL)})
            NEXT
        ENDIF
        IF nInfo == CLASS_DESCR_ALL .or. nInfo == CLASS_DESCR_METHODS
            FOREACH var met in _methods
                aadd(aResult[4], {met:Key, METHOD_INSTANCE+CLASS_EXPORTED, met:Value, NIL, typeof(USUAL)})
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
    
    /// <summary>Handles assignment operations to and adds undefined instance variables.</summary>
    /// <param name="cName">The field name that gets assigned.</param>
    /// <param name="uValue">The value of an assignment.</param>
    /// <remarks> 
    /// If a message is sent to a DataObject instance for which no corresponding instance variable exists,
    /// the method NoIvarPut() is executed. The method receives the name of the instance variable in the &lt;cMessage&gt; parameter,
    /// and the value to be assigned in the &lt;xValue&gt; parameter. NoIvarPut() first creates the instance variable and then assigns
    /// the value passed. Because it now exists, future access to the instance variable no longer causes NoIvarPut() to be executed.
    /// Instead, the operation is handled in the same way as for a static instance variable. The function IsMemberVar() can be used to
    /// test for the existence of an instance variable added via NoIvarPut(). In addition, the method :classDescribe() also reflects
    /// dynamic instance variables. 
    /// </remarks>
    OVERRIDE METHOD NoIvarPut(cName, uValue) AS USUAL CLIPPER
        SELF:_fields[cName] := uValue
        RETURN uValue

    /// <summary>Handles assignment operations to and adds undefined instance variables.</summary>
    /// <param name="cName">The field name that gets accessed.</param>
    /// <remarks>Returns NIL for an undefined instance variable.
    /// This method is executed whenever an undefined instance is accessed. By definition, DataObjects return NIL
    /// in such cases. This allows to check for the presence of a value in a dynamic instance variable simply by
    /// comparing against the value NIL. <br/>
    /// The function IsMemberVar() and the method classDescribe() can be used to determine if a specific member is
    /// defined in a DataObject. This works irrespective of whether the member was added dynamically, or was defined
    /// statically in a class derived from DataObject. 
    /// </remarks>
    OVERRIDE METHOD NoIvarGet(cName) AS USUAL CLIPPER
        IF SELF:_fields:ContainsKey(cName)
            RETURN SELF:_fields[cName]
        ENDIF
        RETURN NIL

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

   /// <summary>Defines a dynamic method.</summary>
   /// <returns>Execution of an undefined method always returns NIL. </returns>
   /// <remarks>The method noMethod() is executed whenever an undefined method is called.
   /// The implementation of noMethod() in the DataObject class always returns the value NIL.
   /// In order to dynamically add a method to a DataObject instance, the method defineMethod() must be used.
   /// Once a method is defined using defineMethod(), calls to the new method no longer cause noMethod() to be executed.
   /// Consequently, overriding noMethod() in a derived class is not required for adding methods to DataObject instances. 
   /// </remarks>
   OVERRIDE METHOD NoMethod(uParams) AS USUAL CLIPPER
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



END CLASS
