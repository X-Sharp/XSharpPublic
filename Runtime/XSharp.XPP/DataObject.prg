using System.Collections.Generic



CLASS XSharp.XPP.DataObject inherit XSharp.XPP.Abstract
    PRIVATE _fields  as Dictionary<String, Usual>
    PRIVATE _methods as Dictionary<String, Usual>

    CONSTRUCTOR()
        SUPER()
        _fields  := Dictionary<String, Usual>{StringComparer.OrdinalIgnoreCase} 
        _methods := Dictionary<String, Usual>{StringComparer.OrdinalIgnoreCase} 

    PUBLIC METHOD IsMemberVar(cName as STRING) AS LOGIC
        IF SELF:_fields:ContainsKey(cName)
            return TRUE
        ENDIF
        RETURN FALSE
        

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
            return SELF:_fields[cName]
        ENDIF
        RETURN NIL

    /// <summary>Creates a dependent shallow copy of this instance </summary>
    /// <returns>This method returns the new DataObject instance. </returns>
    VIRTUAL METHOD Copy() AS DataObject
        local oNew as DataObject
        oNew := DataObject{}
        FOREACH var element in _fields
            oNew:_fields:Add(element:Key, element:Value)
        NEXT
        RETURN oNew

    /// <summary>Merges exported member variables from another object into this instance.</summary>
    /// <param name="oNewObject">DataObject to copy the member variables from.</param>
    /// <param name="cMessagePrefix">Optional prefix that will be added to all fields in the DataObject.</param>
    /// <returns>This method returns the DataObject (self).</returns>

    VIRTUAL METHOD Merge(oNewObject, cMessagePrefix) AS DataObject CLIPPER
        LOCAL oObject := oNewObject as Object
        EnforceType(REF cMessagePrefix, STRING)
        IF oObject is DataObject var oDataObject
            FOREACH var element in oDataObject:_fields
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
    VIRTUAL METHOD DefineMethod(cName as STRING, uAction as USUAL) AS OBJECT
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
   OVERRIDE METHOD NoMethod(cName, uParams) AS USUAL CLIPPER
        LOCAL aParams as USUAL[]
        if self:_methods:ContainsKey(cName)
            aParams := USUAL[]{ PCOunt() }
            FOR VAR nX := 2 to PCount()
                aParams[nX] := _GetFParam(nX)
            NEXT
            aparams[1] := SELF
            local action as usual
            action := self:_methods[cName]
            if IsString(action)
                return _CallClipFunc(action, aParams)
            elseif IsCodeBlock(action)
                local oBlock := action as CodeBlock
                return oBlock:Eval(aParams)
            endif
        endif
        return NIL



END CLASS
