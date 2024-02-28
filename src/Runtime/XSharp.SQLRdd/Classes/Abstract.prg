//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


using System
using System.Collections.Generic
using System.Text
using System.Diagnostics

begin namespace XSharp.RDD.SqlRDD

/// <summary>
/// The Abstract class.
/// </summary>
[DebuggerDisplay("{Name,nq}")];
class SqlDbObject
#ifdef DEBUG
    static private nId := 0 as long
    /// <exclude />
    property Id	  as long auto get private set
#endif
    /// <summary>
    /// The Name of the object.
    /// </summary>
    /// <value></value>
    property Name as string auto get private set
    
    constructor(cName as string)
#ifdef DEBUG
        self:Id := ++nId
#endif
        self:Name := cName
        return

    override method ToString() as string
        return self:Name

    internal method SetName(cName as string) as void
        self:Name := cName
        return

end class
end namespace // XSharp.RDD.SqlRDD
