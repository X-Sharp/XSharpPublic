//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Reflection
USING System.Linq
USING System.Collections.Concurrent
USING XSharp.Internal
using XSharp.RT

[AllowLateBinding];
abstract class XSharp.XPP.Abstract IMPLEMENTS ILateBound
    PRIVATE inSend := FALSE AS LOGIC
    private static classObjects as ConcurrentDictionary<System.Type, ClassObject>

    internal static method GetClassObject(oType as System.Type) as ClassObject
        IF classObjects:TryGetValue(oType, out var result)
            RETURN result
        ENDIF
        result := XSharp.XPP.StaticClassObject{oType}
        classObjects:TryAdd(oType, result)
        RETURN result

    internal static method RemoveClassObject(oType as System.Type) as logic
        if classObjects:TryRemove(oType, out var _)
            return true
        endif
        return false


    STATIC CONSTRUCTOR
        classObjects := ConcurrentDictionary<System.Type, ClassObject>{}

    /// <include file="XSharp.XPP.Docs.xml" path="doc/Abstract.ClassName/*" />
    METHOD ClassName() AS STRING
        RETURN SELF:GetType():Name

    /// <include file="XSharp.XPP.Docs.xml" path="doc/Abstract.ClassObject/*" />
    METHOD ClassObject() AS OBJECT STRICT
        RETURN Abstract.GetClassObject(SELF:GetType())

    METHOD Eval(uBlock) AS USUAL CLIPPER
        IF pCount() > 0
            LOCAL aParams AS USUAL[]
            LOCAL bBlock := uBlock AS CODEBLOCK
            aParams := USUAL[]{ PCOunt()-1 }
            // The pseudo function _ARGS() returns the Clipper arguments array
            System.Array.Copy(_ARGS(),1, aParams, 0, PCOunt()-1)
            RETURN XSharp.RT.Functions.Eval(bBlock, aParams)
        ELSE
            THROW ArgumentException{"Missing Codeblock parameter", nameof(uBlock)}
        ENDIF

    ///  <inheritdoc />
    VIRTUAL METHOD HasIVar(cName AS STRING) AS LOGIC
        RETURN IVarGetInfo(SELF, cName) != 0


    STATIC METHOD IsMemberAccessible(mem as MemberInfo, st as System.Diagnostics.StackTrace) AS LOGIC
        var memType  := mem:DeclaringType
        for var i := 0 to st:FrameCount - 1
            var frame := st:GetFrame(i)
            var m := frame:GetMethod()
            if m:DeclaringType != typeof(Abstract) .and. m:DeclaringType:IsAssignableFrom(memType)
                return TRUE
            endif
        next
        RETURN FALSE

    ///  <inheritdoc />
    VIRTUAL METHOD NoIvarGetSelf(cName AS STRING) AS USUAL
        return SELF:_NoIvarGet(cName, TRUE)

    ///  <inheritdoc />
    VIRTUAL METHOD NoIvarGet(cName AS STRING) AS USUAL
        return SELF:_NoIvarGet(cName, FALSE)

    private METHOD _NoIvarGet(cName AS STRING, lSelf as LOGIC) AS USUAL
        if ClassHelpers.IsInstanceofRuntimeClass(self)
            return ClassHelpers.CallIVarGet(self, cName)
        ENDIF
        var mem := OOPHelpers.GetFieldOrProperty(self:GetType(), cName)
        // Note that XBase++ allows to call Static members with an instance syntax
        // XBase++ also allows to call private members on untype fields in a method of the same class
        // So we are not filtering here

        if mem is FieldInfo var fld
            if fld:IsPrivate .and. ! lSelf
                if !IsMemberAccessible(fld, System.Diagnostics.StackTrace{false})
                    fld := NULL
                endif
            endif
            if fld != null
                return fld:GetValue(self)
            endif
        elseif mem is PropertyInfo var  prop .and. prop:CanRead .and. (prop:GetMethod:IsPublic .or. lSelf)
            if prop:GetIndexParameters():Length == 0
                return prop:GetValue(self)
            endif
        endif

        var oError := Error.VOError( EG_NOVARMETHOD, __FUNCTION__, nameof(cName), 2, <object>{self, cName} )
        oError:Description := oError:Message+" '"+cName+"'"
        throw oError

    ///  <inheritdoc />
    VIRTUAL METHOD NoIvarPutSelf(cName AS STRING, uValue AS USUAL) AS VOID
        SELF:_NoIvarPut(cName, uValue, TRUE)

    ///  <inheritdoc />
    VIRTUAL METHOD NoIvarPut(cName AS STRING, uValue AS USUAL) AS VOID
        SELF:_NoIvarPut(cName, uValue, FALSE)

    PRIVATE METHOD _NoIvarPut(cName AS STRING, uValue AS USUAL, lSelf as LOGIC) AS VOID
        if ClassHelpers.IsInstanceofRuntimeClass(self)
            ClassHelpers.CallIVarPut(self, cName, uValue)
        ENDIF
        var mem := OOPHelpers.GetFieldOrProperty(self:GetType(), cName)
        // Note that XBase++ allows to call Static members with an instance syntax
        // So we are not filtering here
        if mem is FieldInfo var fld

            if fld:IsPrivate .and. ! lSelf
                if !IsMemberAccessible(fld, System.Diagnostics.StackTrace{false})
                    fld := null
                endif
            endif
            if fld != null
                var oValue := OOPHelpers.ValueConvert(uValue, fld:FieldType)
                fld:SetValue(self, oValue)
            endif
            RETURN
        elseif mem is PropertyInfo var  prop .and. prop:CanWrite .and. (prop:SetMethod:IsPublic .or. lSelf)
            if prop:GetIndexParameters():Length == 0
                var oValue := OOPHelpers.ValueConvert(uValue, prop:PropertyType)
                prop:SetValue(self, oValue)
                return
            endif
        endif
        var oError := Error.VOError( EG_NOVARMETHOD, __FUNCTION__, nameof(cName), 2, <object>{self, cName} )
        oError:Description := oError:Message+" '"+cName+"'"
        throw oError
    /// <include file="XSharp.XPP.Docs.xml" path="doc/Abstract.SetNoIVar/*" />
    METHOD SetNoIVar(cName AS USUAL , uValue  AS USUAL) AS VOID
        SELF:NoIvarPut(cName, uValue)
        RETURN

    /// <include file="XSharp.XPP.Docs.xml" path="doc/Abstract.GetNoIVar/*" />
    METHOD GetNoIVar(cName AS USUAL ) AS USUAL STRICT
        RETURN SELF:NoIvarGet(cName)

    /// <include file="XSharp.XPP.Docs.xml" path="doc/Abstract.NoMethod/*" />
#ifdef DOC
    VIRTUAL METHOD NoMethod() AS USUAL CLIPPER
#else
    VIRTUAL METHOD NoMethod(cName, uParams) AS USUAL CLIPPER
#endif
        LOCAL cMethod AS STRING
        cMethod := RuntimeState.NoMethod
        var uArgs := _ARGS()
        IF ! SELF:inSend
            SELF:inSend := TRUE
            TRY
                if ClassHelpers.IsInstanceofRuntimeClass(self)
                    return ClassHelpers.CallMethod(self, cMethod, uArgs)
                ENDIF
                var t := self:GetType()
                do while t != typeof(System.Object)
                    var overloads := OOPHelpers.FindOverloads(t, cMethod, false):ToArray()
                    var mi  := OOPHelpers.FindBestOverLoad<MethodInfo>(t, overloads, cMethod, uArgs)
                    if mi != null
                        if OOPHelpers.SendHelper(null, mi, uArgs, out var result)
                            return result
                        endif
                    endif
                    t := t:BaseType
                enddo
                RETURN __InternalSend(SELF, cMethod,  uArgs)
            FINALLY
                SELF:inSend := FALSE
            END TRY
        ELSE
            THROW Error.VOError( EG_NOMETHOD, __ENTITY__, cMethod, 1, <OBJECT>{cMethod} )
        ENDIF

    /// <include file="XSharp.XPP.Docs.xml" path="doc/Abstract.Notify/*" />
    METHOD Notify(nEvent, nNotification) AS USUAL CLIPPER
        RETURN SELF

    /// <include file="XSharp.XPP.Docs.xml" path="doc/Abstract.IsDerivedFrom/*" />
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
