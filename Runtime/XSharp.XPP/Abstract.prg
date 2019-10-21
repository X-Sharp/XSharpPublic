//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Reflection
CLASS XSharp.XPP.Abstract 
    PRIVATE inSend := FALSE AS LOGIC
    
    /// <summary>Retrieves the name of the class an object belongs to.</summary>
    /// <returns>The method returns a character string representing the name of a class.</returns>
    METHOD ClassName() AS STRING
        RETURN SELF:GetType():Name
        
        /// <summary>Retrieves the class object (System.Type) of a class.</summary>
        /// <returns>The method returns the class object of a class.</returns>
        /// <remarks>The X# XPP implementation returns a System.Type object as class object.</remarks>
    METHOD ClassObject AS OBJECT
        RETURN SELF:GetType()
        
    METHOD Eval(uBlock) AS USUAL CLIPPER
        IF pCount() > 0
            LOCAL aParams AS USUAL[]
            LOCAL bBlock := uBlock AS CODEBLOCK
            aParams := USUAL[]{ PCOunt()-1 }
            // The pseudo function _ARGS() returns the Clipper arguments array
            System.Array.Copy(_ARGS(),1, aParams, 0, PCount()-1)
            RETURN XSharp.RT.Functions.Eval(bBlock, aParams)
        ELSE
            THROW ArgumentException{"Missing Codeblock parameter", nameof(uBlock)}
        ENDIF

    VIRTUAL METHOD HasIVar(cName AS STRING) AS LOGIC
        RETURN IvarGetInfo(SELF, cName) != 0

    VIRTUAL METHOD NoIvarGet(cName) AS USUAL CLIPPER
        IF XSharp.XPP.ClassObject.IsInstanceOfRuntimeClass(SELF)
            RETURN XSharp.XPP.ClassObject.CallIVarGet(SELF, cName)
        ENDIF
        RETURN NIL

    VIRTUAL METHOD NoIvarPut(cName, uValue) AS USUAL CLIPPER
        IF XSharp.XPP.ClassObject.IsInstanceOfRuntimeClass(SELF)
            RETURN XSharp.XPP.ClassObject.CallIVarPut(SELF, cName, uValue)
        ENDIF
        RETURN NIL

        /// <summary>Handles assign operations to undefined instance variables. </summary>
        /// <param name="cName">The fieldname to assign.</param>
        /// <param name="uValue">The value of an assignment. </param>
        /// <returns>The return value of the method is ignored.</returns>
    METHOD SetNoIVar(cName , uValue ) AS USUAL CLIPPER
        RETURN SELF:NoIVarPut(cName, uValue)
        
        /// <summary>Handles access operations to undefined instance variables. </summary>
        /// <param name="cName">The fieldname to access.</param>
        /// <returns></returns>
    METHOD GetNoIVar(cName ) AS USUAL CLIPPER
        RETURN SELF:NoIvarGet(cName)
        
        /// <summary>Handles calls to undefined methods.</summary>
        /// <param name="cName">The methodname to call.</param>
        /// <param name="uParams">The parameters to send to the method.</param>
        /// <returns>The return value will be interpreted as the return value of the called undefined method. </returns>
        /// <remarks>If an undefined method is called, a runtime error is raised.
        /// However, when :noMethod() is declared in the class, the runtime error will not occur.
        /// Instead, program execution is directed to this method. The parameter <paramref name="cName" /> contains the name of the undefined method,
        /// followed by the parameters the callee has passed to the method call.</remarks>
    VIRTUAL METHOD NoMethod(uParams) AS USUAL CLIPPER
        LOCAL cMethod AS STRING
        cMethod := RuntimeState.NoMethod
        IF ! SELF:inSend
            SELF:inSend := TRUE
            TRY
                IF XSharp.XPP.ClassObject.IsInstanceOfRuntimeClass(SELF)
                    RETURN XSharp.XPP.ClassObject.CallMethod(SELF, cMethod, _ARGS())
                ENDIF
                RETURN __InternalSend(SELF, cMethod,  _ARGS())
            FINALLY
                SELF:inSend := FALSE
            END TRY
        ELSE
            THROW Error.VOError( EG_NOMETHOD, __ENTITY__, cMethod, 1, <OBJECT>{cMethod} )
        ENDIF

    /// <summary>Receives notifications from DatabaseEngines </summary>
        /// <param name="nEvent">This parameter receives a numeric value that corresponds with a constant listed in APPEVENT.CH.</param>
        /// <param name="nNotification">The second parameter identifies the situation for which an object is notified. The file DMLB.CH lists define constants that can be used to test in a program which situation occurred. </param>
        /// <remarks>There is no need in X# to include the header files. The defines are included as part of the X# runtime.</remarks>
    METHOD Notify(nEvent, nNotification) AS USUAL CLIPPER
        RETURN SELF
        
        /// <summary>Checks if an object belongs to or is derived from a particular class.</summary>
        /// <param name="uParent">A character string containing the name of the class an object belongs to or is derived from. Alternatively, the class object (System.Type) can be passed instead of the class name.</param>
        /// <returns>The method returns .T. (true) if the object executing the method belongs to or is derived from the specified class. </returns>
        /// <remarks>This method is used to check if an unknown object has features of a known class. This is especially useful for event driven programming or when classes are inherited from other classes.</remarks>
    METHOD IsDerivedFrom(uParent) AS LOGIC CLIPPER
        LOCAL oType AS System.Type
        IF IsString(uParent)
            oType := XSharp.RT.Functions.FindClass(uParent)
            RETURN oType:IsAssignableFrom(SELF:GetType())
        ELSEIF IsObject(uParent)
            IF ((OBJECT) uParent) IS System.Type 
                otype := (System.Type) uParent
                RETURN oType:IsAssignableFrom(SELF:GetType())
            ENDIF
        ENDIF
        RETURN FALSE
        
        /// <summary>Reflects the class definition of a class.</summary>
        /// <returns>If <paramref name="nInfo" /> is omitted or set to CLASS_DESCR_ALL, an array with four elements is returned.
        /// They contain the information resulting from passing one of the following constants to this method.</returns>
        /// <param name="nInfo">A constant from must be used for this parameter.
        /// It defaults to CLASS_DESCR_ALL and defines the type of information included in the return value. </param>
        /// <remarks>The constants are included in XSharp.XPP.DLL and you do not need to include Class.CH anymore. <br/>
        /// Allowed constants are: <br/>
        /// - CLASS_DESCR_ALL<br/>
        /// - CLASS_DESCR_CLASSNAME<br/>
        /// - CLASS_DESCR_SUPERCLASSES<br/>
        /// - CLASS_DESCR_MEMBERS<br/>
        /// - CLASS_DESCR_METHODS<br/>
        /// - CLASS_DESCR_SUPERDETAILS : this is not supported in X#.<br/>
        /// </remarks>
    VIRTUAL METHOD ClassDescribe(nInfo) AS ARRAY CLIPPER
        LOCAL aResult AS ARRAY
        LOCAL otype   AS System.Type
        LOCAL aFields AS ARRAY
        LOCAL aMethods AS ARRAY
        IF ! IsNumeric(nInfo)
            nInfo := CLASS_DESCR_ALL
        ENDIF
        aResult    := ArrayNew(4)
        otype := SELF:GetType()
        aResult[1] := oType:Name
        IF oType:BaseType != NULL
            aResult[2] := {oType:BaseType:Name}
        ELSE
            aResult[2] := {}
        ENDIF
        VAR aFieldInfo := oType:GetFields()
        aResult[3] := aFields := arrayNew(aFieldInfo:Length)
        FOR VAR nFld := 1 TO aFieldInfo:Length
            LOCAL oFld AS FieldInfo
            oFld := aFieldInfo[nFld]
            aFields[nFld] := {oFld:Name, EnCodeFieldAttributes(oFld:Attributes), oFld:FieldType}
        NEXT
        VAR aMethodInfo := oType:GetMethods()
        aResult[4] := aMethods := arrayNew(aMethodInfo:Length)
        FOR VAR nMethod := 1 TO aFieldInfo:Length
            LOCAL oMeth AS MethodInfo
            oMeth := aMethodInfo[nMethod]
            aMethods[nMethod] := {oMeth:Name, EnCodeMethodAttributes(oMeth:Attributes),NIL ,NIL,oMeth:ReturnType}
        NEXT
        SWITCH (LONG) nInfo
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
        
        
    STATIC PRIVATE METHOD EncodeFieldAttributes(attr AS FieldAttributes) AS LONG
        LOCAL result := 0 AS LONG
        IF attr:HasFlag(FieldAttributes.Public)
            result += CLASS_EXPORTED
        ELSEIF attr:HasFlag(FieldAttributes.Family)
            result += CLASS_PROTECTED
        ELSEIF attr:HasFlag(FieldAttributes.Private)
            result += CLASS_HIDDEN
        ENDIF
        IF attr:HasFlag(FieldAttributes.Static)
            result +=VAR_CLASS
        ELSE
            result +=VAR_INSTANCE
        ENDIF
        RETURN result
        
    STATIC PRIVATE METHOD EnCodeMethodAttributes(attr AS MethodAttributes) AS LONG
        LOCAL result := 0 AS LONG
        IF attr:HasFlag(MethodAttributes.Public)
            result += CLASS_EXPORTED
        ELSEIF attr:HasFlag(MethodAttributes.Family)
            result += CLASS_PROTECTED
        ELSEIF attr:HasFlag(MethodAttributes.Private)
            result += CLASS_HIDDEN
        ENDIF
        IF attr:HasFlag(MethodAttributes.Static)
            result +=METHOD_CLASS
        ELSE
            result +=METHOD_INSTANCE
        ENDIF
        RETURN result
        
        
END CLASS
