//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.Collections.Generic
using System.Text
using System.Data.Common
using System.Data

begin namespace XSharp.RDD.SqlRDD
/// <summary>
/// The HandleObject class.
/// </summary>
class SqlDbHandleObject inherit SqlDbObject implements IDisposable
    /// <summary>
    /// Unique handle for the object
    /// </summary>
    property Handle             as IntPtr auto
    /// <summary>
    /// Create a new instance of the HandleObject class
    /// </summary>
    /// <param name="cName">Name to associate with the object</param>
    constructor(cName as string)
        super(cName)
        self:Handle := SqlDbHandles.GetHandle(self)
        return
    public virtual method Dispose() as void
        SqlDbHandles.Remove(self)
        return
end class
end namespace
