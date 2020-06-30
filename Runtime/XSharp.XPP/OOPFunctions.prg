//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

FUNCTION IsMemberVar(uObject , cName , nAttributes) as LOGIC CLIPPER
LOCAL oObject as Object
EnforceType(uObject, OBJECT)
EnforceType(cName, STRING)
EnforceType(REF nAttributes, LONG)
oObject := uObject
IF oObject IS XSharp.XPP.DataObject
    local DObject := (DataObject) oObject as DataObject
    return DObject:IsMemberVar(cName)
ENDIF
IF oObject IS XSharp.XPP.Abstract var AO
    return AO:HasIVar(cName)
ENDIF
return IVarGetInfo(oObject, cName) != 0

    
