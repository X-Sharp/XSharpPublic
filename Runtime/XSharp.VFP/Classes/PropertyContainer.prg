//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.Collections.Generic
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


class XSharp.VFP.PropertyContainer

    static _PropertyCache as Dictionary<System.Type, IList<PropertyDescriptor> >

    static constructor
        _PropertyCache :=  Dictionary<System.Type, IList<PropertyDescriptor> > {}

    protected _Properties as Dictionary<string, PropertyDescriptor>
    protected _Values     as Dictionary<string, usual>
    protected _Owner      as object

    property Count as long get _Properties:Count
    constructor(oOwner as object)
        _Properties := Dictionary<string, PropertyDescriptor>{StringComparer.OrdinalIgnoreCase}
        _Values     := Dictionary<string, usual>{StringComparer.OrdinalIgnoreCase}
        _Owner := oOwner
        self:InitCompileTimeProperties()

    private method InitCompileTimeProperties() as void
        var oType := _Owner:GetType()
        local list as IList<PropertyDescriptor>
        if _PropertyCache:ContainsKey(oType)
            list := _PropertyCache[oType]
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
                var desc := self:Add(oProp:Name, nil, nVis,nil)
                if _Values:ContainsKey(oProp:Name)
                    _Values:Remove(oProp:Name)
                endif
                list:Add(desc)
                _Properties[oProp:Name]:PropInfo := oProp
            endif
        next
        _PropertyCache[oType] := list
        return

       #region Property related
     method Add(cName as string, uValue as usual, nVisibility as usual, cDescription as usual) as PropertyDescriptor
        // Note that we need to handle the syntax AddProperty("PropertyName(3)") which adds an array property with 3 elements
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
                uValue := aValue
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
             _Values[cName] := uValue
             return desc
        endif
        return null

    method Remove(cPropertyName as string) as logic
        // FoxPro does not throw an error when non existing properties are removed
        // FoxPro does not require the dimensions when deleting an array property
        if _Values:ContainsKey(cPropertyName)
            _Values:Remove(cPropertyName)
        endif
        if _Properties:ContainsKey(cPropertyName)
            local desc := _Properties[cPropertyName] as PropertyDescriptor
            // you cannot remove builtin properties, only dynamic properties
            if desc:PropInfo == null
                _Properties:Remove(cPropertyName)
            endif
            return true
        endif
        return false

        #endregion

    #region IDynamicProperties
    virtual method NoIvarPut(cName as string, uValue as usual) as void
        if _Properties:ContainsKey( cName)
            var desc := _Properties[cName]
            if desc:PropInfo != null
                desc:PropInfo:SetValue(_Owner, uValue)
                return
            elseif self:_Values:ContainsKey(cName)
                self:_Values[cName] := uValue
                return
            endif
        endif
        throw PropertyNotFoundException{cName}


    virtual method NoIvarGet(cName as string) as usual
        if _Properties:ContainsKey(cName)
            var desc := _Properties[cName]
            if desc:PropInfo != null
                return desc:PropInfo:GetValue(_Owner)
            elseif self:_Values:ContainsKey(cName)
                return self:_Values[cName]
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
            if desc:PropInfo == null .or. desc:PropInfo:PropertyType != typeof(PropertyContainer)

                result:Add( NameValuePair{item:Key, uValue})
            endif
        next
        return result

end class

public class XSharp.VFP.PropertyDescriptor
    public Name           as string
    public Visibility     as PropertyVisibility
    public Description    as string
    public PropInfo       as PropertyInfo
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
