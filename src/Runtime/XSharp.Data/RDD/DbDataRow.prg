//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data
USING XSharp.RDD

/// <summary>This class represents a DBF Row in a DbDataTable class.</summary>
CLASS XSharp.DbDataRow INHERIT DataRow IMPLEMENTS IDbRow
    /// <summary>Record number in the workarea</summary>
    PROPERTY RecNo AS LONG AUTO
    PUBLIC CONSTRUCTOR(builder AS DataRowBuilder )
        SUPER(builder)
        
    PROTECTED INTERNAL CONSTRUCTOR(builder AS DataRowBuilder , nRecord AS LONG)
        SUPER(builder)
        if nRecord != 0
            SELF:RecNo := nRecord
        endif
END CLASS
