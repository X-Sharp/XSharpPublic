//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING System.Data
USING System.Data.Common

BEGIN NAMESPACE XSharp.RDD.SqlRDD
ENUM TableMode
    MEMBER Table
    MEMBER Query
END ENUM


ENUM SqlRDDEventReason
    MEMBER ConnectionString
    MEMBER CommandText
    MEMBER MaxRecords
    MEMBER ColumnList
    MEMBER OrderByClause
    MEMBER WhereClause
    MEMBER RecnoColumn
    MEMBER DeletedColumn
    MEMBER LongFieldNames
END ENUM


END NAMESPACE
