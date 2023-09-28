//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection
USING System.Collections.Generic
USING XSharp.Internal

[AllowLateBinding];
abstract class XSharp.XPP.Abstract
    PRIVATE inSend := FALSE AS LOGIC
    private static classObjects as Dictionary<System.Type, ClassObject>

    internal static method GetClassObject(type as System.Type) as ClassObject
        IF classObjects:ContainsKey(type)
            RETURN classObjects[type]
        ENDIF
        var result := XSharp.XPP.StaticClassObject{type}
        classObjects:Add(type, result)
        RETURN result
    internal static method RemoveClassObject(type as System.Type) as logic
        if classObjects:ContainsKey(type)
            classObjects:Remove(type)
            return true
        endif
        return false


    STATIC CONSTRUCTOR
        classObjects := Dictionary<System.Type, ClassObject>{}

    /// <summary>Retrieves the name of the class an object belongs to.</summary>
    /// <returns>The method returns a character string representing the name of a class.</returns>
    METHOD ClassName() AS STRING
        RETURN SELF:GetType():Name

        /// <summary>Retrieves the class object (System.Type) of a class.</summary>
        /// <returns>The method returns the class object of a class.</returns>
        /// <remarks>The X# XPP implementation returns a System.Type object as class object.</remarks>
    METHOD ClassObject() AS OBJECT STRICT
        RETURN Abstract.GetClassObject(SELF:GetType())

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
        RETURN IVarGetInfo(SELF, cName) != 0

    VIRTUAL METHOD NoIvarGet(cName AS STRING) AS USUAL
        if ClassHelpers.IsInstanceofRuntimeClass(self)
            return ClassHelpers.CallIVarGet(self, cName)
        ENDIF
        RETURN NIL

    VIRTUAL METHOD NoIvarPut(cName AS STRING, uValue AS USUAL) AS VOID
        if ClassHelpers.IsInstanceofRuntimeClass(self)
            ClassHelpers.CallIVarPut(self, cName, uValue)
        ENDIF
        RETURN

        /// <summary>Handles assign operations to undefined instance variables. </summary>
        /// <param name="cName">The fieldname to assign.</param>
        /// <param name="uValue">The value of an assignment. </param>
        /// <returns>The return value of the method is ignored.</returns>
    METHOD SetNoIVar(cName AS USUAL , uValue  AS USUAL) AS VOID
        SELF:NoIvarPut(cName, uValue)
        RETURN

        /// <summary>Handles access operations to undefined instance variables. </summary>
        /// <param name="cName">The fieldname to access.</param>
        /// <returns></returns>
    METHOD GetNoIVar(cName AS USUAL ) AS USUAL STRICT
        RETURN SELF:NoIvarGet(cName)

    /// <summary>Handles calls to undefined methods.</summary>
    /// <param name="uParams">The parameters to send to the method.</param>
    /// <returns>The return value will be interpreted as the return value of the called undefined method. </returns>
    /// <remarks>If an undefined method is called, a runtime error is raised.</remarks>
    VIRTUAL METHOD NoMethod(cName, uParams) AS USUAL CLIPPER
        LOCAL cMethod AS STRING
        cMethod := RuntimeState.NoMethod
        IF ! SELF:inSend
            SELF:inSend := TRUE
            TRY
                if ClassHelpers.IsInstanceofRuntimeClass(self)
                    return ClassHelpers.CallMethod(self, cMethod, _ARGS())
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
                oType := (System.Type) uParent
                RETURN oType:IsAssignableFrom(SELF:GetType())
            ENDIF
        ENDIF
        RETURN FALSE

    /// <include file="XPPComments.xml" path="Comments/ClassDescribe/*" />
    VIRTUAL METHOD ClassDescribe(uInfo) AS ARRAY CLIPPER
        LOCAL aResult AS ARRAY
        LOCAL oType   AS System.Type
        LOCAL aFields AS ARRAY
        LOCAL aMethods AS ARRAY
        LOCAL nInfo as USUAL
        IF ! IsNumeric(uInfo)
            nInfo := CLASS_DESCR_ALL
        ELSE
            nInfo := uInfo
        ENDIF
        // we always build the whole array and return a sub array when not all the info is needed.
        aResult    := ArrayNew(4)
        oType := SELF:GetType()
        aResult[1] := oType:Name
        IF oType:BaseType != NULL
            aResult[2] := {oType:BaseType:Name}
        ELSE
            aResult[2] := {}
        ENDIF
        VAR aFieldInfo := oType:GetFields()
        aResult[3] := aFields := ArrayNew(aFieldInfo:Length)
        FOR VAR nFld := 1 TO aFieldInfo:Length
            LOCAL oFld AS FieldInfo
            oFld := aFieldInfo[nFld]
            aFields[nFld] := {oFld:Name, EncodeFieldAttributes(oFld:Attributes), oFld:FieldType}
        NEXT
        VAR aMethodInfo := oType:GetMethods()
        aResult[4] := aMethods := ArrayNew(aMethodInfo:Length)
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
