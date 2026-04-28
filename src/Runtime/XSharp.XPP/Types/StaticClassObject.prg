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

/// <include file="XSharp.XPP.Docs.xml" path="doc/StaticClassObject/*" />
[DebuggerDisplay("ClassObject {Type.FullName}")];
class XSharp.XPP.StaticClassObject inherit ClassObject

    constructor(t as System.Type)
        super(t)

end class
