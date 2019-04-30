FUNCTION IsMemberVar(oObject , cName , nAttributes) as LOGIC CLIPPER
EnforceType(oObject, OBJECT)
EnForceType(cname, STRING)
EnforceType(REF nAttributes, LONG)
IF oObject IS XSharp.XPP.DataObject
    local DObject := oObject as DataObject
    return DObject:IsMemberVar(cName)
ENDIF
IF oObject IS XSharp.XPP.Abstract var AO
    return AO:HasIVar(cName)
ENDIF
return IVarGetInfo(oObject, cName) != 0

    
