//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System
using System.Collections.Generic
using System.Text
using System.Data
using System.Data.Common

begin namespace XSharp.RDD.SqlRDD
enum TableMode
    member Table
    member Query
end enum


enum SqlRDDEventReason
    member ConnectionString
    member CommandText
    member MaxRecords
    member ColumnList
    member OrderByClause
    member WhereClause
    member RecnoColumn
    member DeletedColumn
    member LongFieldNames
    member TrimTrailingSpaces
    member IndexTags
    member IndexInfo
end enum


end namespace
