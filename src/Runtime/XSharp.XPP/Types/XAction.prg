//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


begin namespace XSharp.XPP

/// <include file="XSharp.XPP.Docs.xml" path="doc/XAction/*" />
class XAction
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XAction.Name/*" />
    property Name as string auto
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XAction.Block/*" />
    property Block as codeblock auto
    constructor(cName as string, oBlock as codeblock)
        Name := cName
        Block := oBlock
        return
end class

end namespace
