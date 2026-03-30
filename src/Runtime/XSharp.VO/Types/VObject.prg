//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

/// <include file="XSharp.VO.Docs.xml" path="doc/VObject/*" />
CLASS XSharp.VObject
    /// <include file="XSharp.VO.Docs.xml" path="doc/VObject.oCargo/*" />
    EXPORT oCargo AS OBJECT
    /// <include file="XSharp.VO.Docs.xml" path="doc/VObject.ctor/*" />
    CONSTRUCTOR()
        SUPER()
        RETURN

    DESTRUCTOR()
        SELF:Destroy()
        RETURN
    /// <include file="XSharp.VO.Docs.xml" path="doc/VObject.Destroy/*" />
    VIRTUAL METHOD Destroy() AS USUAL CLIPPER
        RETURN SELF


END CLASS
