//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Reflection
using System.Linq
using System.Diagnostics
using XSharp.RT
#pragma options("az", on)

/// <include file="XSharp.XPP.Docs.xml" path="doc/ClassObject/*" />
[DebuggerDisplay("ClassObject {Type.FullName}")];
abstract class XSharp.XPP.ClassObject implements ILateBound
    protected _Type as System.Type
    virtual property Type as System.Type => _Type

    constructor(t as System.Type)
        self:_Type := t

    /// <include file="XSharp.XPP.Docs.xml" path="doc/ClassObject.New/*" />
    virtual method New() as object clipper
        return _CreateInstance(self:_Type, _Args())

    /// <include file="XSharp.XPP.Docs.xml" path="doc/ClassObject.NoIvarGet/*" />
    virtual method NoIvarGet(cName as string) as usual
        var mem := OOPHelpers.GetFieldOrProperty(_Type, cName)
        if mem is FieldInfo var fld .and. fld:IsStatic
            return fld:GetValue(null)
        endif
        if mem is PropertyInfo var  prop .and. prop:CanRead .and. prop:GetMethod:IsStatic
            if prop:GetIndexParameters():Length == 0
                return prop:GetValue(null,null)
            endif
        endif
        var oError := Error.VOError( EG_NOVARMETHOD, _Type:Name, nameof(cName), 2, <object>{_Type:Name, cName} )
        oError:Description := oError:Message+" '"+cName+"'"
        throw oError

    /// <include file="XSharp.XPP.Docs.xml" path="doc/ClassObject.NoIvarPut/*" />
    virtual method NoIvarPut(cName as string, uValue as usual) as void
        local oValue as object
        // get member from cache
        var mem := OOPHelpers.GetFieldOrProperty(_Type, cName)
        if mem is FieldInfo var fld .and. fld:IsStatic
            oValue := OOPHelpers.ValueConvert(uValue, fld:FieldType)
            fld:SetValue(null, oValue)
            return
        endif
        if mem is PropertyInfo var  prop .and. prop:CanWrite .and. prop:SetMethod:IsStatic
            oValue := OOPHelpers.ValueConvert(uValue, prop:PropertyType)
            if prop:GetIndexParameters():Length == 0
                prop:SetValue(null, (object) uValue, null)
            endif
            return
        endif
        var oError := Error.VOError( EG_NOVARMETHOD, _Type:Name, nameof(cName), 2, <object>{_Type:Name, cName, uValue} )
        oError:Description := oError:Message+" '"+cName+"'"
        throw oError


    /// <include file="XSharp.XPP.Docs.xml" path="doc/ClassObject.NoMethod/*" />
    virtual method NoMethod() as usual clipper
        // Lookup class method and call it.
        // when method not found in the class, then walk the base class
        var cMethod := XSharp.RT.Functions.NoMethod()
        var uArgs := _Args()
        var t := self:_Type
        do while t != typeof(System.Object)
            var overloads := OOPHelpers.FindOverloads(t, cMethod, false):ToArray()
            var mi  := OOPHelpers.FindBestOverLoad<MethodInfo>(overloads, cMethod, uArgs)
            if mi != null
                if OOPHelpers.SendHelper(null, mi, uArgs, out var result)
                    return result
                endif
            endif
            t := t:BaseType
        enddo

        // maybe this is a class method?
        var oError := Error.VOError( EG_NOMETHOD, __function__, nameof(cMethod), 2, <object>{null, cMethod, uArgs} )
        throw oError

end class
