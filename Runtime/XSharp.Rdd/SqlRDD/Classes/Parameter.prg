//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.Data.Common
USING System.Data

BEGIN NAMESPACE XSharp.RDD.SqlRDD

/// <summary>
/// The Parameter class.
/// </summary>
CLASS SqlDbParameter INHERIT SqlDbObject
    PROPERTY Ordinal    as LONG AUTO
    PROPERTY Value      AS OBJECT AUTO
    PROPERTY DbParameter as DbParameter AUTO
    PROPERTY Command    AS SqlDbCommand AUTO
    PROPERTY Direction  AS ParameterDirection AUTO

    CONSTRUCTOR(nOrdinal as LONG, oValue as OBJECT)
        SUPER(nOrdinal:ToString())
        SELF:Ordinal := nOrdinal
        SELF:Value   := oValue
        SELF:Direction := ParameterDirection.Input
        RETURN
    CONSTRUCTOR(cName as STRING, oValue as OBJECT)
        SUPER(cName)
        SELF:Ordinal := -1
        SELF:Value   := oValue
        SELF:Direction := ParameterDirection.Input
        RETURN

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD.Classes
