//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Data
USING XSharp.RDD

/// <include file="XSharp.Data.Docs.xml" path="doc/DbDataRow/*" />
CLASS XSharp.DbDataRow INHERIT DataRow IMPLEMENTS IDbRow
    /// <include file="XSharp.Data.Docs.xml" path="doc/DbDataRow.RecNo/*" />
    PROPERTY RecNo AS DWORD AUTO
    PUBLIC CONSTRUCTOR(builder AS DataRowBuilder )
        SUPER(builder)

    PROTECTED INTERNAL CONSTRUCTOR(builder AS DataRowBuilder , nRecord AS DWORD)
        SUPER(builder)
        if nRecord != 0
            SELF:RecNo := nRecord
        endif
END CLASS
