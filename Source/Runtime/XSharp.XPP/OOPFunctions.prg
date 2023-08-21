//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

function IsMemberVar(uObject , cName , nAttributes) as logic clipper
    local oObject as object
    EnforceType(uObject, object)
    EnforceType(cName, string)
    EnforceType(ref nAttributes, long)
    oObject := uObject
    if oObject is XSharp.XPP.DataObject
        local DObject := (DataObject) oObject as DataObject
        return DObject:IsMemberVar(cName)
    endif
    if oObject is XSharp.XPP.Abstract var AO
        return AO:HasIVar(cName)
    endif
    return IVarGetInfo(oObject, cName) != 0



FUNCTION __GetXppClassObject(type AS System.Type) AS ILateBound
    RETURN Abstract.GetClassObject(type)



