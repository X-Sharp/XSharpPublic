//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Collections.Concurrent
using System.Text
using System.Reflection
using System.Reflection.Emit
using XSharp.XPP
using XSharp.RT
using System.Diagnostics


static class ClassHelpers


[DebuggerDisplay("FieldDescriptor {Name}")];
internal class FieldDescriptor
    internal property Name       as string auto
    internal property Type       as System.Type auto
    internal property Attributes as FieldAttributes auto
    internal property FieldInfo  as FieldInfo auto
end class

[DebuggerDisplay("MethodDescriptor {Name}")];
internal class MethodDescriptor
    internal property Name       as string auto
    internal property Type       as System.Type auto
    internal property Attributes as MethodAttributes auto
    internal property Block      as codeblock auto
    internal property Varname    as string auto
    internal property Getter     as logic auto
    internal property Setter     as logic auto
end class

[DebuggerDisplay("ClassDescriptor {Name}")];
internal class ClassDescriptor
    internal property Name       as string auto
    internal property SuperClass as string auto
    internal property Interfaces as string[] auto
    internal property Fields     as FieldDescriptor[] auto
    internal property Methods    as MethodDescriptor[] auto
    internal property Properties as MethodDescriptor[] auto
    internal property HasInit    as logic auto
    internal method GetUniqueHashCode as int
        local nCode as int
        nCode := self:Name:GetHashCode() + self:SuperClass:GetHashCode()
        begin unchecked
            foreach var fld in Fields
                nCode += fld:Name:GetHashCode() + fld:Attributes:GetHashCode()
            next
            foreach var m in Methods
                nCode += m:Name:GetHashCode() + m:Attributes:GetHashCode()
            next
            foreach var p in Properties
                nCode += p:Name:GetHashCode() + p:Attributes:GetHashCode()
            next
        end unchecked
        return nCode


    internal method GetFieldInfo(cName as string) as FieldInfo
        foreach oField as FieldDescriptor in self:Fields
            if String.Compare(oField:Name, cName, true) == 0
                return oField:FieldInfo
            endif
        next
        return null

    internal method GetPropertyGetBlock(cName as string) as codeblock
        foreach oMethod as MethodDescriptor in self:Properties
            if String.Compare(oMethod:Name, cName, true) == 0 .and. oMethod:Getter
                return oMethod:Block
            endif
        next
        return null_codeblock

    internal method GetPropertySetBlock(cName as string) as codeblock
        foreach oMethod as MethodDescriptor in self:Properties
            if String.Compare(oMethod:Name, cName, true) == 0 .and. oMethod:Setter
                return oMethod:Block
            endif
        next
        return null_codeblock


    internal method GetMethodBlock(cName as string) as codeblock
        foreach oMethod as MethodDescriptor in self:Methods
            if String.Compare(oMethod:Name, cName, true) == 0
                return oMethod:Block
            endif
        next
        return null_codeblock

    internal constructor (cName as string)
        Name := cName

end class


    private static Classes     as ConcurrentDictionary<string, DynamicClassObject>
    private static OldClasses  as ConcurrentDictionary<int, DynamicClassObject>

    static constructor
        Classes    := ConcurrentDictionary<string, DynamicClassObject>{StringComparer.OrdinalIgnoreCase}
        OldClasses := ConcurrentDictionary<int, DynamicClassObject>{}

    /// <summary>
    /// Find the ClassObject for a class
    /// </summary>
    /// <param name="cClassName"></param>
    /// <param name="lIncludeDeleted"></param>
    /// <returns></returns>
    static method FindClass(cClassName as string, lIncludeDeleted as logic) as ClassObject
        if !String.IsNullOrEmpty(cClassName) .and. Classes:TryGetValue(cClassName, out var result)
            return result
        endif
        return null

    internal static method DeleteClass(classObject as DynamicClassObject) as logic
        local lOk := false as logic
        if Classes:TryRemove(classObject:Name, out var _)
            lOk := true
            var nCode := classObject:Descriptor:GetUniqueHashCode()
            OldClasses:TryAdd(nCode, classObject)
        endif
        return lOk


    internal static method CreateType(oDesc as ClassDescriptor) as TypeBuilder
        local tb as TypeBuilder
        local ta as TypeAttributes
        local parent := null as System.Type
        local mb := null as ModuleBuilder
        mb := GetDynamicModule()
        ta  := TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.AutoClass | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit | TypeAttributes.AutoLayout
        if ! String.IsNullOrEmpty(oDesc:SuperClass)
            parent := XSharp.RT.Functions.FindClass(oDesc:SuperClass)
            if parent == null_object
                var oError := Error.VOError( EG_NOCLASS, __function__, "SuperClass", 1,  <object>{oDesc:SuperClass}  )
                oError:Description := oError:Message+" '"+oDesc:SuperClass+"'"
                throw oError
            else
                if !typeof(XSharp.XPP.Abstract):IsAssignableFrom(parent)
                    var oError := Error.VOError( EG_NOCLASS, __function__, "SuperClass", 1,  <object>{oDesc:SuperClass}  )
                    oError:Description :=" Class '"+oDesc:SuperClass+"' must inherit from XSharp.XPP.Abstract"
                    throw oError

                endif
            endif
        else
            parent := typeof(XSharp.XPP.Abstract)
        endif

        var ctor := typeof(DebuggerDisplayAttribute):GetConstructor(<Type> { typeof(string) })
        var arguments   := <object>{ oDesc:Name }
        var debuggerDisplay := CustomAttributeBuilder{ctor, arguments}

        local suffix := 0 as int
        do while true
            var ns := iif(suffix == 0, "XppDynamic", "XppDynamic"+ suffix:ToString())
            var name := ns+"."+oDesc:Name
            try
                var existing := mb:FindTypes(FullNameTypeFilter, name)
                if existing:Length > 0
                    suffix += 1
                    loop
                endif
                tb  := mb:DefineType(name,ta, parent)
                exit
            catch
                // this should not happen since we are checking already
                suffix += 1
            end try
        enddo
        tb:SetCustomAttribute(debuggerDisplay)

        return tb

    internal static an := null as AssemblyName
    internal static ab := null as AssemblyBuilder
    internal static mb := null as ModuleBuilder
    internal static method GetDynamicModule() as ModuleBuilder
        if an == null .or. ab == null .or. mb == null
            an  := AssemblyName{"XSharp.XPP.DynamicClasses"}
            ab  := AppDomain.CurrentDomain:DefineDynamicAssembly(an, AssemblyBuilderAccess.Run)
            mb  := ab:DefineDynamicModule("MainModule")
        endif
        return mb


    /// Delegate to filter types in the DynamicAssembly
    internal static method FullNameTypeFilter(t as Type, oParam as object) as logic
        return t:FullName == (string) oParam

    internal static method GetClassObject(oObject as object) as DynamicClassObject
        local oType as System.Type
        if oObject == null
            return null_object
        endif
        oType := oObject:GetType()
        foreach var element in Classes
            if element:Value:Type == oType
                return element:Value
            endif
        next
        return null_object
    internal static method  IsInstanceofRuntimeClass(oObject as object) as logic
        return GetClassObject(oObject) != null_object



    internal static method CallIVarGet(oObject as object, cName as string) as usual
        local oClass as DynamicClassObject
        oClass := GetClassObject(oObject)
        if oClass != null_object
            var oDesc := oClass:Descriptor
            local oInfo as FieldInfo
            oInfo := oDesc:GetFieldInfo(cName)
            if oInfo != null
                return oInfo:GetValue(oObject)
            endif
            local oBlock as codeblock

            oBlock := oDesc:GetPropertyGetBlock(cName)
            if oBlock != null_codeblock
                return EvalBlock(oBlock, oObject, null)
            endif
        endif
        var oError := Error.VOError( EG_NOVAR, __entity__, nameof(cName), 2, <object>{oObject, cName} )
        oError:Description  := oError:Message + " '"+cName+"'"
        throw oError



    internal static method CallIVarPut(oObject as object, cName as string, uValue as usual) as void
        local oClass as DynamicClassObject
        oClass := ClassHelpers.GetClassObject(oObject)
        if oClass != null_object
            var oDesc := oClass:Descriptor
            local oInfo as FieldInfo
            oInfo := oDesc:GetFieldInfo(cName)
            if oInfo != null
                oInfo:SetValue(oObject, __castclass(object, uValue))
                return
            endif
            local oBlock as codeblock
            oBlock := oDesc:GetPropertySetBlock(cName)
            if oBlock != null_codeblock
                ClassHelpers.EvalBlock(oBlock, oObject, uValue)
                return
            endif
        endif
        var oError := Error.VOError( EG_NOVAR, __entity__, nameof(cName), 2, <object>{oObject, cName, uValue} )
        oError:Description  := oError:Message + " '"+cName+"'"
        throw oError


    internal static method CallMethod(oObject as object, cName as string, uParams as usual[]) as usual
        local oClass as DynamicClassObject
        oClass := ClassHelpers.GetClassObject(oObject)
        if oClass != null_object
            var oDesc := oClass:Descriptor
            local oBlock as codeblock
            oBlock := oDesc:GetMethodBlock(cName)
            if oBlock != null_codeblock
                return EvalBlock(oBlock, oObject, uParams)
            endif
        endif
        var oError := Error.VOError( EG_NOMETHOD, __entity__, nameof(cName), 2, <object>{oObject, cName, uParams} )
        oError:Description  := oError:Message + " '"+cName+"'"
        throw oError


        // Use the class descriptor to create the class
    static internal method ImplementClass(oDesc as ClassDescriptor) as DynamicClassObject
        local cName as string
        local oResult as DynamicClassObject

        cName := oDesc:Name
        if Classes:TryGetValue(cName, out var _)
            throw Error{"A Class definition for "+cName+" already exists with another structure"}
        endif
        // fetch the type from the old classes list when it exists
        local nCode := oDesc:GetUniqueHashCode() as long
        if OldClasses:TryGetValue(nCode, out var oldClass)
            if oldClass:Descriptor:Name == oDesc:Name
                Classes:TryAdd(cName, oldClass)
                return oldClass
            endif
        endif


        local oTb as TypeBuilder
        oTb := CreateType(oDesc)
        foreach var oFld in oDesc:Fields
            oFld:FieldInfo := oTb:DefineField(oFld:Name, oFld:Type, oFld:Attributes)
        next
        var type := oTb:CreateType()
        foreach var oFld in oDesc:Fields
            oFld:FieldInfo := type:GetField(oFld:FieldInfo:Name)
        next

        oResult := DynamicClassObject{type, oDesc:Name, oDesc}
        Classes:TryAdd(oDesc:Name, oResult)
        return oResult

        // create the class by converting the parameters to a class descriptor
    internal static method CreateClassDescriptor(cClassName as string, aSuperClasses:= null_array as array, aMember:= null_array as array, aMethod:= null_array as array) as ClassDescriptor
        local aFields  := List<FieldDescriptor>{} as List<FieldDescriptor>
        local aMethods := List<MethodDescriptor>{} as List<MethodDescriptor>
        local aProperties := List<MethodDescriptor>{} as List<MethodDescriptor>
        local oSuper    as object
        local oSuperType as System.Type
        local aInterfaces := List<string>{} as List<string>
        local cSuperClass := "" as string
        if String.IsNullOrEmpty(cClassName)
            throw Error.ArgumentError("ClassCreate", nameof(cClassName),1, <object>{cClassName})
        endif
        if aSuperClasses != null_array
            for var nI := 1 to ALen(aSuperClasses)
                local uElement as usual
                uElement := aSuperClasses[nI]
                if IsString(uElement)
                    oSuper := ClassHelpers.FindClass((string) uElement,false)
                    if oSuper == null
                        oSuper := XSharp.RT.Functions.FindClass((string) uElement)
                    endif
                elseif IsSymbol(uElement)
                    oSuper := ClassHelpers.FindClass((symbol) uElement,false)
                elseif IsObject(uElement)
                    oSuper := uElement
                else
                    throw Error.ArgumentError("ClassCreate", nameof(aSuperClasses),2, <object>{aSuperClasses})
                endif
                if oSuper != null

                    if oSuper is XSharp.XPP.DynamicClassObject var rtClass
                        cSuperClass := rtClass:Name
                    else
                        if oSuper is System.Type var st
                            oSuperType := st
                        else
                            oSuperType := oSuper:GetType()
                        endif
                        if oSuperType:IsInterface
                            aInterfaces:Add(oSuperType:FullName)
                        else
                            cSuperClass := oSuperType:FullName
                        endif
                    endif
                endif
            next
        endif
        if aMember != null_array
            // check to see if each member is a 2 dimensional array, where the sub array has 2 or more
            for var nI := 1 to ALen(aMember)
                local uElement := aMember[nI] as usual
                local lError := false as logic
                if IsArray(uElement) .and. ALen(uElement) >= 2
                    local aElement := uElement as array
                    if IsString(aElement[1]) .and. IsNumeric(aElement[2])
                        local oField as FieldDescriptor
                        oField := FieldDescriptor{}
                        oField:Name  := aElement[1]
                        oField:Attributes := DecodeFieldAttributes(aElement[2])
                        oField:Type  := typeof(usual)
                        aFields:Add(oField)
                    else
                        lError := true
                    endif
                else
                    lError := true
                endif
                if lError
                    throw Error.ArgumentError("ClassCreate", nameof(aMember),3, <object>{aMember})
                endif
            next
        endif
        local hasInit := false as logic
        if aMethod != null_array
            // check to see if each method is a 2 dimensional array, where the sub array has 3 or 4 members
            for var nI := 1 to ALen(aMethod)
                local uElement := aMethod[nI] as usual
                local lError := false as logic
                if IsArray(uElement) .and. ALen(uElement) >= 3
                    local aElement := uElement as array
                    if IsString(aElement[1]) .and. IsNumeric(aElement[2]) .and. IsCodeBlock(aElement[3])
                        local oMethod as MethodDescriptor
                        oMethod := MethodDescriptor{}
                        oMethod:Name        := aElement[1]
                        oMethod:Attributes  := DecodeMethodAttributes(aElement[2])
                        oMethod:Type        := typeof(usual)
                        oMethod:Block       := aElement[3]
                        oMethod:Getter := _and(aElement[2], METHOD_ACCESS) == METHOD_ACCESS
                        oMethod:Setter := _and(aElement[2], METHOD_ASSIGN) == METHOD_ASSIGN
                        if ALen(aElement) > 3 .and. IsString(aElement[4])
                            oMethod:Varname := aElement[4]
                        endif
                        if oMethod:Getter .or. oMethod:Setter
                            aProperties:Add(oMethod)
                        else
                            aMethods:Add(oMethod)
                        endif
                        if String.Compare(oMethod:Name, "INIT",true) == 0
                            hasInit := true
                        endif
                    else
                        lError := true
                    endif
                else
                    lError := true
                endif
                if lError
                    throw Error.ArgumentError("ClassCreate", nameof(aMethod),3, <object>{aMethod})
                endif
            next
        endif
        var oClassDesc          := ClassDescriptor{cClassName}
        oClassDesc:Fields       := aFields:ToArray()
        oClassDesc:Methods      := aMethods:ToArray()
        oClassDesc:Properties   := aProperties:ToArray()
        oClassDesc:SuperClass   := cSuperClass
        oClassDesc:Interfaces   := aInterfaces:ToArray()
        oClassDesc:HasInit      := hasInit
        return oClassDesc


    internal static method EvalBlock(oBlock as codeblock, oObject as object, uParams params usual[]) as usual
        local uNewParams as usual[]
        if uParams != null
            uNewParams := usual[]{uParams:Length+1}
        else
            uNewParams := usual[]{1}
        endif
        uNewParams[1] := oObject
        if uParams != null .and. uParams:Length > 0
            System.Array.Copy( uParams, 0, uNewParams, 1, uParams:Length )
        endif
        return Eval(oBlock, uNewParams)

    internal static method DecodeFieldAttributes(nAttrib as long) as FieldAttributes
        local attribute as FieldAttributes
        if _and(nAttrib, CLASS_EXPORTED ) == CLASS_EXPORTED
            attribute := FieldAttributes.Public
        elseif _and(nAttrib, CLASS_PROTECTED) == CLASS_PROTECTED
            attribute := FieldAttributes.Family
        elseif _and(nAttrib, CLASS_HIDDEN) == CLASS_HIDDEN
            attribute := FieldAttributes.Private
        else
            attribute := FieldAttributes.Public
        endif
        if _and(nAttrib, VAR_CLASS) == VAR_CLASS
            attribute |= FieldAttributes.Static
        elseif _and(nAttrib, VAR_CLASS_SHARED) == VAR_CLASS_SHARED
            attribute |= FieldAttributes.Static
        elseif _and(nAttrib, VAR_INSTANCE) == VAR_INSTANCE
            // Instance attribute does not exist
            nop
        else
            nop
        endif
        /*
        if _and(nAttrib, VAR_ASSIGN_HIDDEN) == VAR_ASSIGN_HIDDEN
        nop
        elseif _and(nAttrib, VAR_ASSIGN_PROTECTED) == VAR_ASSIGN_PROTECTED
        nop
        elseif _and(nAttrib, VAR_ASSIGN_EXPORTED) == VAR_ASSIGN_EXPORTED
        nop
        else
        nop
        endif
        */
        return attribute

    internal static method DecodeMethodAttributes(nAttrib as long) as MethodAttributes
        local attribute as MethodAttributes
        if _and(nAttrib, CLASS_EXPORTED ) == CLASS_EXPORTED
            attribute := MethodAttributes.Public
        elseif _and(nAttrib, CLASS_PROTECTED) == CLASS_PROTECTED
            attribute := MethodAttributes.Family
        elseif _and(nAttrib, CLASS_HIDDEN) == CLASS_HIDDEN
            attribute := MethodAttributes.Private
        else
            attribute := MethodAttributes.Public
        endif
        if _and(nAttrib, METHOD_CLASS) == METHOD_CLASS
            attribute |= MethodAttributes.Static
        elseif _and(nAttrib, METHOD_INSTANCE) == METHOD_INSTANCE
            // Instance attribute does not exist
            nop
        else
            nop
        endif
        return attribute

end class


/// <summary>The worker class to help create classes at runtime.</summary>
[DebuggerDisplay("DynamicClassObject {Name}")];
class XSharp.XPP.DynamicClassObject inherit ClassObject

#region Fields and Properties
    private _name as string
    private _desc as ClassHelpers.ClassDescriptor
    property Name as string get _name
    internal property Descriptor as  ClassHelpers.ClassDescriptor get _desc
    property Type  as System.Type get _Type
#endregion
    internal constructor(oType as System.Type, cName as string, oDesc  as ClassHelpers.ClassDescriptor)
        super(oType)
        _name := cName
        _desc := oDesc

    override method New() as object clipper
        local oRes as object
        oRes := super:New()
        if self:Descriptor:HasInit
            // Call the Init method
            local oBlock := self:Descriptor:GetMethodBlock("INIT") as codeblock
            // The pseudo function _ARGS() returns the Clipper arguments array
            var old := OOPHelpers.EmulateSelf
            OOPHelpers.EmulateSelf := true
            ClassHelpers.EvalBlock(oBlock, oRes, _ARGS())
            OOPHelpers.EmulateSelf := old
        endif
        return oRes

#region Class Helper methods

    internal method HasIVar(cVar as string) as logic
        local oClass as DynamicClassObject
        oClass := ClassHelpers.GetClassObject(self)
        if oClass != null
            if oClass:Descriptor:GetPropertyGetBlock(cVar) != null .or. ;
                    oClass:Descriptor:GetPropertySetBlock(cVar) != null
                return true
            endif
        endif
        return IVarGetInfo(self, cVar) != 0

#endregion

end class
