//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


begin namespace XSharp.XPP

/// <summary>Helper class that stores actions (codeblocks) for XML nodes</summary>
class XAction
    /// <summary>XML node name for which the action runs.</summary>
    property Name as string auto
    /// <summary>Codeblock to run for the action</summary>
    property Block as codeblock auto
    constructor(cName as string, oBlock as codeblock)
        Name := cName
        Block := oBlock
        return
end class

end namespace
