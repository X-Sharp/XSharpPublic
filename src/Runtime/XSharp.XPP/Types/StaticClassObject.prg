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

/// <summary>
/// This class returns the ClasssObject for classes created at compile time (Static classes)
/// This object allows to access static members and methods late bound
/// Such as <code>Example():Fieldname</code>
/// </summary>
[DebuggerDisplay("ClassObject {type.FullName}")];
class XSharp.XPP.StaticClassObject inherit ClassObject

    constructor(t as System.Type)
        super(t)

end class
