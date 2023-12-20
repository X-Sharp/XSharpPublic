//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
USING System.Collections
using System.Collections.Generic
using System.Collections.Concurrent
using System.Diagnostics
using System.Linq
using System.Reflection


[DebuggerDisplay("{Name,nq}={Value}")];
STRUCTURE XSharp.VFP.NameValuePair
    PUBLIC Name    as STRING
    PUBLIC @@Value as USUAL
    CONSTRUCTOR(cName as STRING, uValue as USUAL)
        SELF:Name  := cName
        SELF:Value := uValue

END STRUCTURE


class XSharp.VFP.PropertyContainer IMPLEMENTS IEnumerable<NameValuePair>

    static _PropertyCache as ConcurrentDictionary<System.Type, IList<PropertyDescriptor> >

    static constructor
        _PropertyCache :=  ConcurrentDictionary<System.Type, IList<PropertyDescriptor> > {}

    protected _Properties as ConcurrentDictionary<string, PropertyDescriptor>
    protected _Values     as ConcurrentDictionary<string, usual>
    protected _Owner      as object

    property Count as long get _Properties:Count
    constructor(oOwner as object)
        _Properties := ConcurrentDictionary<string, PropertyDescriptor>{StringComparer.OrdinalIgnoreCase}
        _Values     := ConcurrentDictionary<string, usual>{StringComparer.OrdinalIgnoreCase}
        _Owner := oOwner
        self:InitCompileTimeProperties()

    private method InitCompileTimeProperties() as void
        var oType := _Owner:GetType()
        local list as IList<PropertyDescriptor>
        if _PropertyCache:TryGetValue(oType, out list)
            foreach var item in list
                _Properties[item:Name] := item
            next
            return
        endif
        var aProps := _Owner:GetType():GetProperties()
        list := List<PropertyDescriptor>{}
        foreach var oProp in aProps
            var met := oProp:GetGetMethod(true)
            if met != null
                var nVis := iif(met:IsPublic,1 ,iif(met:IsPrivate,3,2))
                var desc := self:_Add(oProp:Name, nVis,nil, out var _)
                list:Add(desc)
                _Properties[oProp:Name]:PropInfo := oProp
            endif
        next
        var aFields := _Owner:GetType():GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance)
        foreach var oFld in aFields
            var nVis := iif(oFld:IsPublic,1 ,iif(oFld:IsPrivate,3,2))
            var desc := self:_Add(oFld:Name, nVis,nil , out var _)
            list:Add(desc)
            _Properties[oFld:Name]:PropInfo := oFld
        next
        _PropertyCache:TryAdd(oType, list)
        return

    #region Property related
    private method _Add(cName as String, nVisibility as usual, cDescription as usual, foxArray OUT __FoxArray ) as PropertyDescriptor
       // Note that we need to handle the syntax AddProperty("PropertyName(3)") which adds an array property with 3 elements
        foxArray := NULL
        if ! String.IsNullOrEmpty(cName)
            local cDims := String.Empty as string
            cName := cName:Trim()
            if cName:EndsWith(")")
                var nPos := cName:IndexOf("(")
                if nPos > 0 // we need at least 1 character
                    cDims := cName:Substring(nPos)
                    cDims := cDims:Substring(1, cDims:Length-2)
                    cName := cName:Substring(0, nPos)
                endif
            endif
            if ! String.IsNullOrEmpty(cDims)
                var aDims := cDims:Split(c',')
                local dims as usual[]
                dims := usual[] {aDims:Length}
                for var i := 1 to aDims:Length
                    dims[i] := Int32.Parse(aDims[i])
                next
                local aValue as __FoxArray
                aValue := __FoxArray{0}
                if (dims:Length == 1)
                    aValue:ReDim(dims[1])
                else
                    aValue:ReDim(dims[1], dims[2])
                endif
                foxArray := aValue
            endif
            local nPropVis  := PropertyVisibility.Public as PropertyVisibility
            local cPropDesc := "" as string
            if IsNumeric(nVisibility) .or. IsString(cDescription)
                if IsNumeric(nVisibility)
                    nPropVis := (PropertyVisibility) nVisibility
                endif
                if IsString(cDescription)
                    cPropDesc := cDescription
                endif
             endif
             var desc := PropertyDescriptor{cName, nPropVis, cPropDesc}
             _Properties[cName]:= desc
             return desc
        endif
        return null

    method Add(cName as string, uValue as usual, nVisibility as usual, cDescription as usual) as PropertyDescriptor
        var desc := self:_Add(cName, nVisibility, cDescription, out var foxArray)
        if desc != null
            if foxArray != NULL
                _Values[cName] := foxArray
            else
                _Values[cName] := uValue
            endif
            return desc
        endif
        return null

    method Remove(cPropertyName as string) as logic
        // FoxPro does not throw an error when non existing properties are removed
        // FoxPro does not require the dimensions when deleting an array property
        _Values:TryRemove(cPropertyName, out var _)
        if _Properties:TryGetValue(cPropertyName, out var desc)
            // you cannot remove builtin properties, only dynamic properties
            if desc:PropInfo == null
                _Properties:TryRemove(cPropertyName, out var _)
            endif
            return true
        endif
        return false

        #endregion

    #region IDynamicProperties
    virtual method NoIvarPut(cName as string, uValue as usual) as void
        if _Properties:TryGetValue( cName, out var desc)
            if desc:PropInfo is PropertyInfo var oProp
                oProp:SetValue(_Owner, uValue)
                return
            elseif desc:PropInfo is FieldInfo var oFld
                oFld:SetValue(_Owner, uValue)
                return
            elseif self:_Values:ContainsKey(cName)
                self:_Values[cName] := uValue
                return
            endif
        endif
        throw PropertyNotFoundException{cName}


    virtual method NoIvarGet(cName as string) as usual
        if _Properties:TryGetValue(cName, out var desc)
            if desc:PropInfo is PropertyInfo var oProp
                return oProp:GetValue(_Owner)
            elseif desc:PropInfo is FieldInfo var oFld
                return oFld:GetValue(_Owner)
            elseif self:_Values:TryGetValue(cName, out var result)
                return result
            endif
        endif
        throw PropertyNotFoundException{cName}

    virtual method GetPropertyNames() as string[]
        return _Properties:Keys:ToArray()

    #endregion


     method GetProperties() as IList<NameValuePair>
        var result := List<NameValuePair>{}
        foreach var item in _Properties
            local desc as PropertyDescriptor
            var uValue := self:NoIvarGet(item:Key)
            desc := item:Value
            if desc:PropInfo == null .or. (desc:PropInfo is PropertyInfo var info .and. info:PropertyType != typeof(PropertyContainer))

                result:Add( NameValuePair{item:Key, uValue})
            endif
        next
        return result

     method IEnumerable<NameValuePair>.GetEnumerator() AS IEnumerator<NameValuePair>
        var props := SELF:GetProperties()
        return props:GetEnumerator()

     method IEnumerable.GetEnumerator() AS IEnumerator
        var props := SELF:GetProperties()
        return props:GetEnumerator()
end class

public class XSharp.VFP.PropertyDescriptor
    public Name           as string
    public Visibility     as PropertyVisibility
    public Description    as string
    public PropInfo       as MemberInfo
    public constructor(cName as string, nVis as PropertyVisibility, cDesc as string)
        Name        := cName
        Visibility  := nVis
        Description := cDesc

end class


public class XSharp.VFP.PropertyNotFoundException inherit Exception
    public property PropertyName as string auto get private set
    public constructor(name as string)
        super(__VfpStr(VFPErrors.PROPERTY_NOT_FOUND, name))
        self:PropertyName := name
        return
end class
