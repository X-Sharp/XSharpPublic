//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using XSharp.XPP
/// <include file="XSharp.XPP.Docs.xml" path="doc/ClassObject/*" />
function ClassObject(cClassName as string) as ClassObject
    local oResult as ClassObject
    oResult := ClassHelpers.FindClass(cClassName, false)
    return oResult


/// <include file="XSharp.XPP.Docs.xml" path="doc/ClassDestroy/*" />
function ClassDestroy(uObject) as logic clipper
    local oObject as object
    if IsObject(uObject)
        oObject := uObject
        if oObject is DynamicClassObject var rtClass
            return ClassHelpers.DeleteClass(rtClass)
        endif
    elseif IsString(uObject)
        oObject := ClassObject((string) uObject)
        if oObject != null
            return ClassDestroy(oObject)
        endif
    endif
    return false

/// <include file="XSharp.XPP.Docs.xml" path="doc/ClassCreate/*" />
function ClassCreate(cClassName , aSuperClasses , aMember , aMethod ) as usual clipper
    // in XPP the superclass can all be classes
    // we support one superclass and interfaces
    // the members array contains sub arrays with per field a name and attribute
    // The attribute contains a visibility aspect (CLASS_HIDDEN ,CLASS_PROTECTED, CLASS_EXPORTED )
    // as well as assignment rights( VAR_ASSIGN_HIDDEN, VAR_ASSIGN_PROTECTED,VAR_ASSIGN_EXPORTED)
    // and types of instance vars (VAR_INSTANCE, VAR_CLASS , VAR_CLASS_SHARED)
    // The methods array contains for each method a name, attribute and codeblock
    // The attribute contains a visibility aspect (CLASS_HIDDEN ,CLASS_PROTECTED, CLASS_EXPORTED )
    // and types of method (METHOD_INSTANCE, METHOD_CLASS )
    // and assignment property (METHOD_ACCESS, METHOD_ASSIGN)
    // INIT method should be mapped to the constructor, with clipper calling convention
    EnforceType(ref cClassName, string)
    EnforceType(ref aSuperClasses, array)
    EnforceType(ref aMember, array)
    EnforceType(ref aMethod, array)
    var descriptor := ClassHelpers.CreateClassDescriptor(cClassName, aSuperClasses, aMember, aMethod)
    return ClassHelpers.ImplementClass(descriptor)







