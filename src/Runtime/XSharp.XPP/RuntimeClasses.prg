//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <summary>Retrieves the class object of class. </summary>
/// <param name="cClassName">The name of the class whose class object should be returned. </param>
/// <returns>The function returns the class object of the class with the name &lt;cClassName&gt;.
/// The return value is NULL_OBJECT when the class does not exist. </returns>
function ClassObject(cClassName as string) as ClassObject
    local oResult as ClassObject
    oResult := ClassHelpers.FindClass(cClassName, false)
    return oResult


/// <summary>Remove the class object of class. </summary>
/// <param name="uObject">The name of the class whose class object should be deleted, or the class object.</param>
/// <returns>The return value is .T. (true) when the class object is removed from memory, otherwise it is .F. (false). </returns>
/// <remarks>The function ClassDestroy() removes the class object of a dynamically created class from main memory.
/// Dynamic classes are created during runtime by the ClassCreate() function. They are unknown at compile time.
/// Therefore, they do not have a class function and are represented at runtime of a program only by a class object. <br/>
/// When a program uses a dynamic class, the corresponding class object should be removed from main memory when the class
/// is no longer needed. Otherwise, the class object remains in memory and can be retrieved by the ClassObject() function at any time.<br/>
/// <b>Note</b>In X# classes are never really freed from memory. The .Net framework does not allow that. ClassDestroy() does remove the class
/// from the list of active classes. If you recreate the same class later with the same structure then the class definition from the previous
/// defintion is reused.</remarks>
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

/// <summary>Create a class dynamically.</summary>
/// <returns>The class object.</returns>
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







