//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Data
USING XSharp.RDD

/// <summary>This class represents a DBF Field in a DbDataTable class.</summary>
CLASS XSharp.DbDataColumn INHERIT DataColumn
    PROPERTY ColumnInfo AS DbColumnInfo AUTO
    
    CONSTRUCTOR(info AS DbColumnInfo)
    SUPER(info:ColumnName, info:DotNetType)
    SELF:ColumnInfo := info
    SELF:AllowDBNull := info:IsNullable
    IF info:IsAutoIncrement
        SELF:AutoIncrement := info:IsAutoIncrement
        SELF:AutoIncrementSeed := -1
        SELF:AutoIncrementStep := -1
        SELF:ReadOnly          := TRUE
    ENDIF
    
END CLASS

