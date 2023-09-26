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
/// The Parameter class.
/// </summary>
class SqlDbParameter inherit SqlDbObject
    property Ordinal    as long auto
    property Value      as object auto
    property DbParameter as DbParameter auto
    property Command    as SqlDbCommand auto
    property Direction  as ParameterDirection auto

    constructor(nOrdinal as long, oValue as object)
        super(nOrdinal:ToString())
        self:Ordinal := nOrdinal
        self:Value   := oValue
        self:Direction := ParameterDirection.Input
        return
    constructor(cName as string, oValue as object)
        super(cName)
        self:Ordinal := -1
        self:Value   := oValue
        self:Direction := ParameterDirection.Input
        return

end class
end namespace // XSharp.RDD.SqlRDD.Classes
