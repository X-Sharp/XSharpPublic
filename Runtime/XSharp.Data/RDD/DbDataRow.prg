//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data


/// <summary>This class represents a DBF Row in a DbDataTable class.</summary>
CLASS XSharp.DbDataRow INHERIT DataRow
    /// <summary>Record number in the workarea</summary>
    PROPERTY RecNo AS LONG AUTO 
    PROTECTED INTERNAL CONSTRUCTOR(builder AS DataRowBuilder , nRecord AS LONG)
        SUPER(builder)
    SELF:RecNo := nRecord
END CLASS
